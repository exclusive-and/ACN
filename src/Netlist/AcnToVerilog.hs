
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Netlist.AcnToVerilog
    ( -- * Backend Monad
      VerilogState (..)
    , VerilogM
    , Doc
      -- * ACN Component Translation
    , acnToVerilogComponent
      -- ** ACN Net Declaration Pass
    , inferNetDecl
    , genNetDecls
    , convDeclarator
      -- *** Type Conversion
    , convNetType
      -- * ACN Declaration Translation
    , acnToVerilogDecl
    , acnToVerilogDecls
     -- * Netlist Expression Translation
    , acnToVerilogExpr
    )
  where

import              Netlist.AcnSyntax
import              Netlist.AcnIds

import              Control.Applicative
import              Control.Lens (makeLenses)
import              Control.Monad.State (State)
import              Data.Maybe
import              Data.Monoid (Ap (Ap))
import              Data.Text.Lazy (pack)
import              GHC.Stack

import              Prettyprinter hiding (Doc)
import qualified    Prettyprinter as PP


type Doc = PP.Doc ()

data VerilogState
    = VerilogState
        { _acnIdSet :: AcnIdSet }

makeLenses ''VerilogState

instance HasAcnIdSet VerilogState where
    acnIdentifierSet = acnIdSet

type VerilogM = State VerilogState

instance AcnNameMonad (State VerilogState) where
    acnNameNormalizerM = pure $ \nm -> AcnName nm nm [] Basic emptyCallStack


prettyId :: AcnId -> VerilogM Doc
prettyId acnId =
    pretty . acnIdToText# acnId <$> acnIdSetM id

-- |
-- Converts an ACN component directly into a Verilog module.
--
acnToVerilogComponent :: AcnComponent -> VerilogM Doc
acnToVerilogComponent (AcnComponent name inputs logic outputs) = do
    portsText <- genModuleIOList inputs outputs

    nameText <- prettyId name
    
    let header = "module" <+> nameText <> line
              <> indent 4 portsText <> semi <> line
    
    logicNetsText   <- genNetDecls True logic
    logicDeclsText  <- acnToVerilogDecls logic
    outDeclsText    <- acnToVerilogDecls outputs
    
    let body = logicNetsText <> line <> line
             <> logicDeclsText <> line <> outDeclsText
    
    return $ header <> line <> indent 2 body <> line
          <> "endmodule"

genModuleIOList :: [NetDeclarator] -> [AcnDeclaration] -> VerilogM Doc
genModuleIOList inputNets outputNets = do
    inputs  <- mapM declareInput inputNets
    outputs <- mapM declareOutput outputNets

    let inputText = case inputs of
            []
                -> lparen <+> string "// No inputs." <> line
            (x:xs)
                -> lparen <+> string "// Inputs." <> line
                <> (string "  " <> x) <> line
                <> vcat (map commafy xs) <> line

        outputPrefix = string "  // Outputs." <> line
                    <> if null inputs then string "" else comma <> space

        outputText = case outputs of
            []
                -> string "  // No outputs." <> line <> rparen
            (x:xs)
                -> outputPrefix <> x <> line
                <> ( if null xs then emptyDoc
                                else vcat (map commafy xs) <> line )
                <> rparen

    return $ inputText <> outputText
  where
    declareInput :: NetDeclarator -> VerilogM Doc
    declareInput = fmap ("input" <+>) . convDeclarator False
    
    declareOutput :: AcnDeclaration -> VerilogM Doc
    declareOutput decl = do
        netDeclText <- inferNetDecl False decl
        return $ "output" <+> netDeclText



-- |
-- Extract a Verilog net declaration from an ACN logic declaration.
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
inferNetDecl
    :: Bool             -- ^ Add semicolons to the ends of declarations?
    -> AcnDeclaration   -- ^ ACN declaration to extract net(s) from.
    -> VerilogM Doc
inferNetDecl addSemi = go where
    go :: AcnDeclaration -> VerilogM Doc
    go = \case
        Assignment net _         -> wireDecl net
        CondAssignment net _ _ _ -> regDecl net
        
        InstDecl nets _ _ _ _ _
            -> fmap vcat . mapM wireDecl $ nets
            
        AnnotatedDecl _ decls
            -> fmap vcat . mapM go $ decls
      
    wireDecl = fmap ("wire" <+>) . convDeclarator addSemi
    regDecl  = fmap ("reg" <+>) . convDeclarator addSemi

genNetDecls :: Bool -> [AcnDeclaration] -> VerilogM Doc
genNetDecls addSemi = fmap vcat . mapM (inferNetDecl addSemi)

-- |
-- Actually convert a net into a Verilog declaration. As explained in
-- 'NetDeclarator', we only generate the name, type, and initial value
-- information. We let the declaration processor prepend whichever use
-- annotation it thinks is appropriate.
--
-- >>> let net = 'NetDeclarator' (Just comment) name ty (Just initVal)
-- >>> nvNetDecl True net
-- [netTypeSize ty:0] name = initVal; // comment
--
convDeclarator
    :: Bool             -- ^ Add a semicolon to the end of the declaration?
    -> NetDeclarator    -- ^ Net to declare.
    -> VerilogM Doc
convDeclarator addSemi (NetDeclarator commentM name ty initValM) = do
    tyText   <- convNetType ty
    nameText <- prettyId name
    
    let toInitializer e = do
            eText <- acnToVerilogExpr False e
            return $ space <> equals <+> eText
    initText <- maybe (pure "") toInitializer initValM
    
    let commentText = maybe "" (comment "//") commentM
    
    return $ tyText <+> nameText <> initText
          <> if addSemi then semi else emptyDoc
         <+> commentText


-- |
-- Convert representable netlist types to Verilog bit vectors.
--
convNetType :: NetType -> VerilogM Doc
convNetType netTy = case netTy of
    Annotated _ netTy'
        -> convNetType netTy'

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


-- |
-- Convert an ACN declaration to a Verilog declaration.
--
acnToVerilogDecl :: AcnDeclaration -> VerilogM Doc
acnToVerilogDecl (Assignment dest expr) = do
    nameText <- prettyId $ netName dest
    exprText <- acnToVerilogExpr False expr
    return $ "assign" <+> nameText <+> equals
                      <+> exprText <> semi

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
acnToVerilogDecl (CondAssignment dest scrut scrutTy alts) = do
    nameText <- prettyId $ netName dest
    
    let goCond :: AcnAlternative -> VerilogM Doc
    
        -- Alternative with literal: @pat: dest = alt;@.
        goCond (Dependent c e)  = do
            -- TODO: proper condition literal reprs
            cText <- verilogLiteral Nothing c
            eText <- acnToVerilogExpr False e
            return $ cText <> ":" <+> nameText
                 <+> equals <+> eText <> semi
        
        -- Default alternative: @default: dest = altExpr;@.
        goCond (Default e) = do
            eText <- acnToVerilogExpr False e
            return $ "default:" <+> nameText
                 <+> equals <+> eText <> semi
    
    conds <- mapM goCond alts

    scrutText <- acnToVerilogExpr False scrut
    let switch = "casez" <+> parens scrutText <> line
              <> indent 2 (vcat conds) <> line
              <> "endcase"

    return $ "always @(*) begin" <> line
          <> indent 2 switch <> line
          <> "end"

--
-- @
-- componentName instanceName
--   (.port1 (arg1), .port2 (arg2), ... ports...);
-- @
--
acnToVerilogDecl (InstDecl _ attrs compName instName params ports) = do
    compNameText <- prettyId compName
    instNameText <- prettyId instName
    
    {- params' <- case params of
        [] -> return space
        _  -> do
            ps <- sequence [ (,) i <$> acnToVerilogExpr False e
                           | (i, _, e) <- params ]
            let f (i, e) = dot <> pretty i <+> parens e
            return $ line <> "#" <> tupled (map f ps) <> line -}
            
    ports' <- case ports of
        NamedPortMap ports' -> do
            ps <- sequence [ (,) <$> prettyId i <*> acnToVerilogExpr False e
                           | (i, _, _, e) <- ports' ]
            let f (i, e) = dot <> i <+> parens e
            return $ tupled $ map f ps
            
        IndexedPortMap ports' -> do
            ps <- sequence [ acnToVerilogExpr False e
                           | (_, _, e) <- ports' ]
            return $ tupled ps
            
    return $ nest 2 $ compNameText -- <> params'
                  <+> instNameText <> line
                   <> ports' <> semi

acnToVerilogDecl (AnnotatedDecl ann decls) = do
    decls' <- acnToVerilogDecls decls
    
    return $ case ann of
        Comment commentText
            -> comment "//" commentText <> line
            <> decls'
        Directive directive
            -> pretty directive <> semi <> line
            <> decls'

        Condition cond
            -> "`ifdef" <+> pretty cond <> line
            <> indent 2 decls' <> line
            <> "`endif"

acnToVerilogDecl _ = error "Not yet implemented"

acnToVerilogDecls :: [AcnDeclaration] -> VerilogM Doc
acnToVerilogDecls = fmap vcat . mapM (fmap (<> line) . acnToVerilogDecl)


nvBlackBoxDecl :: AcnBlackBox -> BlackBoxContext -> VerilogM Doc
nvBlackBoxDecl blackbox context = undefined


-- |
-- Translate an expression in netlist language to Verilog.
--
acnToVerilogExpr :: Bool -> AcnExpression -> VerilogM Doc
acnToVerilogExpr _ (Identifier ident) = prettyId ident
    
acnToVerilogExpr _ (Literal tyM lit) = verilogLiteral tyM lit

acnToVerilogExpr _ (DataCon ty consIndex args) = case ty of
    Vector 0 _ -> return $ verilogTypeErrorValue ty
    Vector 1 _ | [e] <- args
        -> acnToVerilogExpr False e
    Vector _ _ -> do
        let vec' = fromMaybe args $ vecChain ty args
        exprTexts <- mapM (acnToVerilogExpr False) vec'
        return $ listBraces exprTexts
        
    RTree 0 _ | [e] <- args
        -> acnToVerilogExpr False e
    RTree _ _ -> do
        let tree' = fromMaybe args $ rtreeChain ty args
        exprTexts <- mapM (acnToVerilogExpr False) tree'
        return $ listBraces exprTexts
        
    MemBlob _ _
        -> error $ "Verilog doesn't support blob constructors; "
                ++ "they should be converted to literals"

    Cartesian cty
        -> cartesianDc cty consIndex args

    -- Just do a passthrough on the first arg lol.
    _ | [e] <- args
        -> acnToVerilogExpr False e
    _ -> error $ "tried to construct a value without a valid constructor: "
              ++ show ty
              ++ " expected one argument, got "
              ++ show args

acnToVerilogExpr _ (Projection src ty consIndex consFieldIndex) = do
    let -- Figure out which field we should use from the constructors.
        constr     = constructors ty !! consIndex
        fieldIndex = fieldIndices constr !! consFieldIndex
        field      = fields ty !! fieldIndex
        
        startText = int $ fieldStart field
        endText   = int $ fieldEnd field
    
    -- Compute the source expression in parentheses (in case it's lower
    -- precedence than slicing).
    srcText <- acnToVerilogExpr True src
    return $ srcText <> brackets (endText <> ":" <> startText)

acnToVerilogExpr _ (Slice src rangeHi rangeLo) = do
    srcText <- acnToVerilogExpr True src
    return $ srcText <> brackets (int rangeHi <> ":" <> int rangeLo)
    
acnToVerilogExpr _ _ = error "Not yet implemented"


verilogLiteral :: Maybe NetType -> Literal -> VerilogM Doc
verilogLiteral tyM = \case
    NumLit i
        | Nothing <- tyM -> return $ integer i
        | Just (ty@(Index _)) <- tyM
        -> return $ int (netTypeSize ty) <> "'d" <> integer i
        | Just (Unsigned sz) <- tyM
        -> return $ int sz <> "'d" <> integer i
        | Just (Signed sz) <- tyM , i < 0
        -> return $ "-" <> int sz <> "'sd" <> integer (abs i)
        | Just (Signed sz) <- tyM
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

vecChain :: NetType -> [AcnExpression] -> Maybe [AcnExpression]
vecChain ty args = case ty of
    Vector 0 _ -> Just []
    Vector 1 _
        | [e] <- args -> Just [e]
    Vector n _
        | [e1, DataCon ty _ e2] <- args
        -> liftA2 (:) (Just e1) (vecChain ty e2)
    _ -> Nothing

rtreeChain :: NetType -> [AcnExpression] -> Maybe [AcnExpression]
rtreeChain ty args = case ty of
    RTree 0 _
        | [e] <- args -> Just [e]
    RTree n _
        | [e1, DataCon ty _ e2] <- args 
        -> liftA2 (:) (Just e1) (rtreeChain ty e2)
    _ -> Nothing

-- |
-- Construct a Cartesian datatype. See 'nvDcApp' for more information.
--
cartesianDc
    :: CartesianType    -- ^ Cartesian type to construct.
    -> Int              -- ^ Index of the constructor to use.
    -> [AcnExpression]  -- ^ Arguments to the constructor.
    -> VerilogM Doc
cartesianDc cty@(CartesianType tyName constrs fields) consIndex args = do
    let constr = constrs !! consIndex
        enumFields = zip [0..] fields
        indexArgs = zip (fieldIndices constr) args
    argsText <- go enumFields indexArgs
    
    let consSize = constructorSize cty
        consText = int consSize <> "'d" <> int consIndex
        dcText = if consSize == 0 then argsText else consText:argsText
    
    return $ braces . hsep $ punctuate comma dcText
  where
    go :: [(Int, NetField)] -> [(Int, AcnExpression)] -> VerilogM [Doc]
    go ((n, field):fs) vs
        -- This field is one of the ones our constructor sets.
        | (ix, e):vs' <- vs
        , n == ix = do
            eText <- acnToVerilogExpr False e
            (eText:) <$> go fs vs'
        
        -- This field isn't set by the constructor. Fill it with
        -- don't-cares and move on.
        | otherwise = do
            let fieldTySize = netTypeSize $ fieldType field
                fieldText = int fieldTySize <> "'b"
                         <> hcat (replicate fieldTySize "x")
            (fieldText:) <$> go fs vs
    
    -- There are no fields left in the type; do nothing.
    go [] _ = return []


comment prefix text = prefix <> " " <> pretty text


tupleInputs = undefined
tupleOutputs = undefined
    
listBraces = align . braces . hsep . punctuate (comma <+> softline)

commafy x = comma <> space <> x



verilogTypeErrorValue ty = braces (int (netTypeSize ty) <+> braces "1'bx")

bitChar = \case
    H -> '1'
    L -> '0'
    U -> 'x'
    Z -> 'z'

int = pretty
integer = pretty
string = pretty

