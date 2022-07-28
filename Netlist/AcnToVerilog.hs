
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Netlist.AcnToVerilog
    ( -- * ACN Component Translation
      acnToVerilogComponent
      -- ** ACN Net Declaration Pass
    , acnToVerilogNetDecl
    , acnToVerilogNetDecls
    , nvNetDecl
      -- *** Type Conversion
    , netToVerilogType
      -- * ACN Declaration Translation
    , acnToVerilogDecl
    , acnToVerilogDecls
      -- ** Standard Declarations
    , nvCondAssign
    , nvInstance
      -- ** Black Box Substitutions
    , nvBlackBoxDecl
      -- * Netlist Expression Translation
    , netToVerilogExpr
    , nvLiteral
      -- ** Data Constructors
    , nvDcApp
    , nvCartesianDc
    , nvSuperDcApp
      -- ** Accessors
    , nvProject
    , nvSlice
      -- * Backend Monad
    , VerilogState (..)
    , VerilogM
    )
  where

import              Netlist.Acn

import              Control.Applicative
import              Control.DeepSeq
import              Control.Monad
import              Control.Monad.State (State)
import              Control.Monad.State.Class
import              Data.Bool
import              Data.Eq
import              Data.Function
import              Data.Functor
import              Data.Functor.Identity
import              Data.IntMap (IntMap (..))
import qualified    Data.IntMap as IntMap
import              Data.List
import              Data.Maybe
import              Data.Monoid (Ap (Ap))
import              Data.Text (Text (..))
import              Data.Text.Lazy (pack)
import              GHC.Generics
import              GHC.Int
import              GHC.Stack
import              Text.Show (Show (..))

import              Prettyprinter hiding (Doc)
import qualified    Prettyprinter as PP

import Debug.Trace


-- | Converts an ACN component directly into a Verilog module.
--
-- Conversion happens in six steps:
--
--  (1) Module head.
--  (2) Inputs.
--  (3) Output nets.
--  (4) Logic nets.
--  (5) Logic assignments.
--  (6) Output assignments.
--
acnToVerilogComponent :: AcnComponent -> VerilogM Doc
acnToVerilogComponent (AcnComponent name inputs logic outputs) = do
    inPorts  <- mapM nvInput inputs
    outPorts <- mapM nvOutput outputs
    
    let portsText = tupleInputs inPorts
                 <> tupleOutputs outPorts <> semi
        moduleHeader = "module" <+> pretty name <> line
                    <> indent 4 portsText <> line
    
    logicNetsText   <- acnToVerilogNetDecls True logic
    logicDeclsText  <- acnToVerilogDecls logic
    outDeclsText    <- acnToVerilogDecls outputs
    
    let moduleBody = logicNetsText <> line <> line
                  <> logicDeclsText <> line
                  <> outDeclsText
    
    return $ moduleHeader <> line
          <> indent 2 moduleBody <> line
          <> "endmodule"
  where
    nvInput :: NetDeclaration -> VerilogM Doc
    nvInput = fmap ("input" <+>) . nvNetDecl False
    
    nvOutput :: AcnDeclaration -> VerilogM Doc
    nvOutput decl = do
        netDeclText <- acnToVerilogNetDecl False decl
        return $ "output" <+> netDeclText
      
    tupleInputs = \case
        []     -> lparen <+> string "// No inputs." <> line
        (x:xs) -> lparen <+> string "// Inputs." <> line
               <> (string "  " <> x) <> line
               <> vcat (map commafy xs) <> line
               
    tupleOutputs = \case
        []     -> string "  // No outputs." <> line <> rparen
        (x:xs) -> string "  // Outputs." <> line
               <> (if not $ null inputs
                      then comma <> space
                      else string "  ")
               <> x <> line
               <> (if null xs
                      then emptyDoc
                      else vcat (map commafy xs) <> line)
               <> rparen

-- | Extract a Verilog net declaration from an ACN logic declaration.
--
-- Conversion rules are as follows:
--
--  * @'Assignment'@: to @wire@.
--
--  * @'CondAssignment'@: to @reg@ (to conform with base Clash's removal of
--    ternary switching).
--    
--  * @'InstDecl'@: to @wire@. Output register-ness must be handled within
--  within the component being instantiated.
--  
--  * @'BlackBoxDecl'@: dependent on the black box's output usage.
--
-- @'TickDecl'@ and @'ConditionalDecl'@ are transparent.
--
acnToVerilogNetDecl
    :: Bool             -- ^ Add semicolons to the ends of declarations?
    -> AcnDeclaration   -- ^ ACN declaration to extract net(s) from.
    -> VerilogM Doc
acnToVerilogNetDecl addSemi = \case
    Assignment net _
        -> nvWireDecl net
    CondAssignment net _ _ alts
        -> nvRegDecl net
    InstDecl nets _ _ _ _ _
        -> fmap vcat . mapM nvWireDecl $ nets
    TickDecl _ decl
        -> acnToVerilogNetDecl addSemi decl
    ConditionalDecl _ decls
        -> acnToVerilogNetDecls addSemi decls
  where
    nvWireDecl = fmap ("wire" <+>) . nvNetDecl addSemi
    nvRegDecl = fmap ("reg" <+>) . nvNetDecl addSemi

acnToVerilogNetDecls
    :: Bool
    -> [AcnDeclaration]
    -> VerilogM Doc
acnToVerilogNetDecls addSemi =
    fmap vcat . mapM (acnToVerilogNetDecl addSemi)

-- |
-- Actually convert a net into a Verilog declaration. As explained in
-- 'NetDeclaration', we only generate the name, type, and initial value
-- information. We let the declaration processor prepend whichever use
-- annotation it thinks is appropriate.
--
-- >>> nvNetDecl True
-- >>>   $ 'NetDeclaration' (Just comment) name ty (Just initVal)
-- [netTypeSize ty:0] name = initVal; // comment
--
nvNetDecl
    :: Bool             -- ^ Add a semicolon to the end of the declaration?
    -> NetDeclaration   -- ^ Net to declare.
    -> VerilogM Doc
nvNetDecl addSemi (NetDeclaration commentM name ty initValM) = do
    let commentText = maybe "" (comment "//") commentM
    tyText <- netToVerilogType ty
    
    let toInitializer e = do
            eText <- netToVerilogExpr False e
            return $ space <> equals <+> eText
    initText <- maybe (pure "") toInitializer initValM
    
    return $ tyText <+> pretty name <> initText
          <> if addSemi then semi else emptyDoc
         <+> commentText


-- |
-- Convert representable netlist types to Verilog bit vectors.
--
netToVerilogType :: NetType -> VerilogM Doc
netToVerilogType netTy = case netTy of
    Annotated _ netTy'
        -> netToVerilogType netTy'

    Clock _     -> return emptyDoc
    Reset _     -> return emptyDoc
    Enable _    -> return emptyDoc
    Boolean     -> return emptyDoc
    Bit         -> return emptyDoc

    Signed n
        -> return $ "signed" <+> brackets (int (n - 1) <> colon <> int 0)
    -- N.B. Signed values are a little bit special. Verilog needs to know when
    -- something is meant to be signed vs unsigned, to decide whether to
    -- use 2's complement operations on it.

    _ -> return $ brackets (int (netTypeSize netTy - 1) <> colon <> int 0)


type Doc = PP.Doc ()

data VerilogState
    = VerilogState
        { _allowTernary :: Bool }

type VerilogM = State VerilogState


acnToVerilogDecl :: AcnDeclaration -> VerilogM Doc
acnToVerilogDecl = \case
    TickDecl ann decl -> do
        let annText = case ann of
                Comment c -> comment "//" c
                Directive d -> pretty d <> ";"
        declText <- acnToVerilogDecl decl
        return $ annText <> line <> declText

    Assignment dest expr -> do
        exprText <- netToVerilogExpr False expr
        return $ "assign" <+> pretty (netName dest) <+> equals
                          <+> exprText <> semi
    CondAssignment dest scrut scrutTy alts
        -> nvCondAssign dest scrut scrutTy alts

    InstDecl _ attrs compName instName params ports
        -> nvInstance attrs compName instName params ports
    BlackBoxDecl blackbox context
        -> nvBlackBoxDecl blackbox context

    ConditionalDecl cond decls -> do
        decls' <- acnToVerilogDecls decls
        return $ "`ifdef" <+> pretty cond <> line <>
                 indent 2 decls' <> line <>
                 "`endif"

acnToVerilogDecls :: [AcnDeclaration] -> VerilogM Doc
acnToVerilogDecls = fmap vcat . mapM (fmap (<> line) . acnToVerilogDecl)

-- |
-- Generate always-comb multiplexing case block switching between @alts@.
--
-- @
-- always @(*) begin
--   case (scrut)
--     pat1: alt1;
--     pat2: alt2;
--     ... alts...
--   endcase
-- end
-- @
--
nvCondAssign
    :: NetDeclaration   -- ^ Result net to assign.
    -> AcnExpression    -- ^ Expression to scrutinize.
    -> NetType          -- ^ Type of scrutinee.
    -> [AcnAlternative] -- ^ Conditional alternatives.
    -> VerilogM Doc
nvCondAssign dest scrut scrutTy alts = do
    let goCond :: AcnAlternative -> VerilogM Doc
        goCond (Just c, e) = do
            cText <- nvLiteral Nothing c -- TODO: proper condition literal reprs
            eText <- netToVerilogExpr False e
            return $ cText <> ":" <+> pretty (netName dest)
                 <+> equals <+> eText <> semi
        goCond (Nothing, e) = do
            eText <- netToVerilogExpr False e
            return $ "default:" <+> pretty (netName dest)
                 <+> equals <+> eText <> semi
    conds <- mapM goCond alts

    scrutText <- netToVerilogExpr False scrut
    let switch = "casez" <+> parens scrutText <> line
              <> indent 2 (vcat conds) <> line
              <> "endcase"

    return $ "always @(*) begin" <> line
          <> indent 2 switch <> line
          <> "end"

-- |
-- Generate and connect component instances according to the port map
-- wiring specification.
--
-- @
-- componentName instanceName
--   (.port1 (arg1), .port2 (arg2), ... ports...);
-- @
--
nvInstance
    :: [Attr']
    -> Identifier   -- ^ Name of component to generate.
    -> Identifier   -- ^ Name of instance to generate.
    -> [()]         -- ^ Compilation parameters.
    -> PortMap      -- ^ Port configuration.
    -> VerilogM Doc
nvInstance attrs compName instName params ports = do
    {- params' <- case params of
        [] -> return space
        _  -> do
            ps <- sequence [ (,) i <$> netToVerilogExpr False e
                           | (i, _, e) <- params ]
            let f (i, e) = dot <> pretty i <+> parens e
            return $ line <> "#" <> tupled (map f ps) <> line -}
            
    ports' <- case ports of
        NamedPortMap ports' -> do
            ps <- sequence [ (,) i <$> netToVerilogExpr False e
                           | (i, _, _, e) <- ports' ]
            let f (i, e) = dot <> pretty i <+> parens e
            return $ tupled $ map f ps
            
        IndexedPortMap ports' -> do
            ps <- sequence [ netToVerilogExpr False e
                           | (_, _, e) <- ports' ]
            return $ tupled ps
            
    return $ nest 2 $ pretty compName -- <> params'
                  <+> pretty instName <> line
                   <> ports' <> semi

nvBlackBoxDecl :: AcnBlackBox -> BlackBoxContext -> VerilogM Doc
nvBlackBoxDecl blackbox context = undefined


-- | Translate an expression in netlist language to Verilog.
--
netToVerilogExpr :: Bool -> AcnExpression -> VerilogM Doc
netToVerilogExpr shouldParen = \case
    Identifier ident
        -> return $ pretty ident
    Literal size lit
        -> nvLiteral size lit
    DataCon ty consIndex args
        -> nvDcApp ty consIndex args
    SuperDataCon ty consExpr args
        -> nvSuperDcApp ty consExpr args
    Projection src ty conIx fIx
        -> nvProject ty src conIx fIx
    Slice src hi lo
        -> nvSlice src hi lo
    
nvLiteral :: Maybe (NetType, Size) -> Literal -> VerilogM Doc
nvLiteral tyM = \case
    NumLit i
        | Nothing <- tyM -> return $ integer i
        | Just (ty@(Index _), _) <- tyM
        -> return $ int (netTypeSize ty) <> "'d" <> integer i
        | Just (Unsigned _, sz) <- tyM
        -> return $ int sz <> "'d" <> integer i
        | Just (Signed _, sz) <- tyM , i < 0
        -> return $ "-" <> int sz <> "'sd" <> integer (abs i)
        | Just (Signed _, sz) <- tyM
        -> return $ int sz <> "'sd" <> integer i
    BoolLit b
        -> return $ string $ if b then "1'b1" else "1'b0"
    BitLit b
        -> return $ string "1'b" <> pretty (bitChar b)
    StringLit s
        -> return $ string . pack $ show s
    lit -> error $ "nvLiteral: " <> show lit

-- |
-- Generate a data constructor for a type. Be aware that:
--
--  (1) Types that don't have explicit data constructors will act
--      idempotently on their argument (as newtypes).
--
--  (2) Cartesian types will only use the appropriate number of
--      arguments for the field. If more are supplied, we ignore them.
--
nvDcApp :: NetType -> Int -> [AcnExpression] -> VerilogM Doc
nvDcApp ty consIndex args = case ty of
    Vector 0 _ -> return $ verilogTypeErrorValue ty
    Vector 1 _ | [e] <- args
        -> netToVerilogExpr False e
    Vector _ _ -> do
        let vec' = fromMaybe args $ vecChain ty args
        exprTexts <- mapM (netToVerilogExpr False) vec'
        return $ listBraces exprTexts
        
    RTree 0 _ | [e] <- args
        -> netToVerilogExpr False e
    RTree _ _ -> do
        let tree' = fromMaybe args $ rtreeChain ty args
        exprTexts <- mapM (netToVerilogExpr False) tree'
        return $ listBraces exprTexts
        
    MemBlob _ _
        -> error $ "Verilog doesn't support blob constructors; "
                ++ "they should be converted to literals"

    Cartesian cty
        -> nvCartesianDc cty consIndex args

    -- Just do a passthrough on the first arg lol.
    _ | [e] <- args
        -> netToVerilogExpr False e
    _ -> error $ "tried to construct a value without a valid constructor: "
              ++ show ty
              ++ " expected one argument, got "
              ++ show args

-- |
-- Construct a Cartesian datatype. See 'nvDcApp' for more information.
--
nvCartesianDc :: CartesianType -> Int -> [AcnExpression] -> VerilogM Doc
nvCartesianDc (CartesianType tyName constrs fields) consIndex args = do
    let constr = constrs !! consIndex
        enumFields = zip [0..] fields
        indexArgs = zip (fieldIndices constr) args
    argsText <- go enumFields indexArgs
    -- TODO: actually prepend constructor.
    return $ enclose lbrace rbrace . hsep $ punctuate comma argsText
  where
    go :: [(Int, NetType)] -> [(Int, AcnExpression)] -> VerilogM [Doc]
    go ((n, field):fs) vs
        | (ix, e):vs' <- vs
        , n == ix = do
            eText <- netToVerilogExpr False e
            (eText:) <$> go fs vs'
        | otherwise = do
            let fieldTySize = netTypeSize field
                fieldText = int fieldTySize <> "'b"
                         <> hcat (replicate fieldTySize "x")
            (fieldText:) <$> go fs vs
    go [] _ = return []


nvSuperDcApp
    :: CartesianType
    -> AcnExpression
    -> [Maybe AcnExpression]
    -> VerilogM Doc
nvSuperDcApp ty consExpr argsM = undefined

nvProject :: NetType -> AcnExpression -> Int -> Int -> VerilogM Doc
nvProject ty src consIndex fieldIndex = undefined

nvSlice :: AcnExpression -> Int -> Int -> VerilogM Doc
nvSlice src rangeHi rangeLo = undefined
                


                
comment prefix text = prefix <> " " <> pretty text

instance Pretty Identifier where
    pretty (RawIdentifier nm _ _) = pretty nm
    pretty (UniqueIdentifier{..}) = pretty baseName

tupleInputs = undefined
tupleOutputs = undefined
    
listBraces = align . enclose lbrace rbrace . hsep . punctuate (comma <+> softline)

commafy x = comma <> space <> x

vecChain :: NetType -> [AcnExpression] -> Maybe [AcnExpression]
vecChain (Vector 0 _) _ = Just []
vecChain (Vector 1 _) [e] = Just [e]
vecChain (Vector n _) [e1,DataCon ty _ e2] = liftA2 (:) (Just e1) (vecChain ty e2)
vecChain _ _ = Nothing

rtreeChain :: NetType -> [AcnExpression] -> Maybe [AcnExpression]
rtreeChain (RTree 0 _) [e] = Just [e]
rtreeChain (RTree n _) [e1,DataCon ty _ e2] = liftA2 (:) (Just e1) (rtreeChain ty e2)
rtreeChain _ _ = Nothing

verilogTypeErrorValue ty = braces (int (netTypeSize ty) <+> braces "1'bx")

bitChar = \case
    H -> '1'
    L -> '0'
    U -> 'x'
    Z -> 'z'

int = pretty
integer = pretty
string = pretty
