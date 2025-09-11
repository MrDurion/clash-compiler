{-# LANGUAGE TemplateHaskell #-}

module Clash.Backend.Aiger (AigerState) where

import Clash.Annotations.Primitive (HDL (..))
import Control.Lens (use, (+=), (.=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1)
import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State)
import Data.Foldable (foldrM)
import Data.HashSet (HashSet)
import Data.List ((!?))
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Debug.Trace (traceM)
import GHC.Data.Maybe (orElse)
import GHC.Num (integerToInt)
import Prelude hiding (and, lookup, or)

import qualified Data.Map as Map
import qualified Data.Text as TextS
import qualified Data.Text.Lazy as LT
import qualified System.FilePath

import Clash.Backend (
  AggressiveXOptBB (..),
  Backend,
  DomainMap,
  HWKind (..),
  HasUsageMap,
  ModName,
  RenderEnums (..),
  emptyDomainMap,
  primsRoot,
 )
import Clash.Driver.Types (ClashOpts)
import Clash.Netlist.BlackBox.Types (HdlSyn)
import Clash.Netlist.Id (toText)
import Clash.Netlist.Types (
  Bit (..),
  BlackBoxContext (..),
  Component (..),
  Declaration (..),
  Expr (..),
  HWType (..),
  HasIdentifierSet (..),
  Identifier (..),
  IdentifierSet,
  Literal (..),
  Modifier (..),
  Size,
  UsageMap,
 )
import Clash.Netlist.Util (typeSize)
import Clash.Util (SrcSpan)
import Data.Text.Prettyprint.Doc.Extra (
  Doc,
  emptyDoc,
  line,
  pretty,
  vcat,
 )

import qualified Clash.Backend
import qualified Clash.Netlist.Id as Id

type BUsage = Clash.Backend.Usage

-- ##################
-- ### DATA DECLS ###
-- ##################

data AigerIndex = AigerIndex Int Bool
toInt :: AigerIndex -> Int
toInt (AigerIndex i b) = (i * 2) + if b then 1 else 0

complement :: AigerIndex -> AigerIndex
complement (AigerIndex i b) = AigerIndex i (not b)

-- TODO what to do with undefined.
undefinedBit :: AigerIndex
undefinedBit = AigerIndex 0 False

instance Show AigerIndex where
  show ai = show $ toInt ai

instance Eq AigerIndex where
  (==) a b = (toInt a) == (toInt b)

instance Ord AigerIndex where
  (<=) a b = (toInt a) <= (toInt b)

data AigerPointer = Pointer Text deriving (Ord, Eq)

instance Show AigerPointer where
  show (Pointer i) = show i

data InputNode = InputNode AigerIndex deriving (Show)
data OutputNode = OutputNode AigerIndex deriving (Show)
data AndNode = AndNode AigerIndex AigerIndex AigerIndex deriving (Show)

data UnsolvedAndNode = UnsolvedAndNode AigerIndex AigerExpr AigerExpr
  deriving (Show)

data AigerExpr
  = Id AigerPointer
  | Concat [AigerExpr]
  | Range Int Int AigerExpr
  | LastRange Int Int AigerExpr
  | And AigerIndex
  | Complement AigerExpr
  | BitRange [AigerIndex]
  | Empty
  deriving (Show)

data AigerState = AigerState
  { _maxIndex :: Int
  , _inputNodes :: [InputNode]
  , _outputNodes :: [OutputNode]
  , _andNodes :: [AndNode]
  , _unsolvedAndNodes :: [UnsolvedAndNode]
  , _aigerExpressions :: Map.Map AigerPointer AigerExpr
  }

-- ##################
-- ### STATE MODS ###
-- ##################

makeLenses ''AigerState

instance HasIdentifierSet AigerState where
  identifierSet = undefined

instance HasUsageMap AigerState where
  usageMap = undefined

type AigerM = Ap (State AigerState)

instance Backend AigerState where
  -- \| Initial state for state monad
  initBackend _opts =
    AigerState
      { _maxIndex = 1
      , _inputNodes = []
      , _outputNodes = []
      , _andNodes = []
      , _unsolvedAndNodes = []
      , _aigerExpressions = Map.insert (Pointer $ TextS.pack "__VOID__") Empty mempty
      }

  -- \| What HDL is the backend generating
  hdlKind :: AigerState -> HDL
  hdlKind = const AIGER

  -- \| Location for the primitive definitions
  primDirs :: AigerState -> IO [FilePath]
  primDirs = const $ do
    root <- primsRoot
    return [root System.FilePath.</> "aiger"]

  -- \| Name of backend, used for directory to put output files in. Should be
  --   constant function / ignore argument.
  name :: AigerState -> String
  name = const "aiger"

  -- \| File extension for target langauge
  extension :: AigerState -> String
  extension = const "aag"

  -- \| Get the set of types out of AigerState
  extractTypes :: AigerState -> HashSet HWType
  extractTypes = undefined

  -- FIXME
  -- \| Generate HDL for a Netlist component
  genHDL ::
    ClashOpts ->
    ModName ->
    SrcSpan ->
    IdentifierSet ->
    UsageMap ->
    Component ->
    AigerM ((String, Doc), [(String, Doc)])
  genHDL = genAIGER

  -- FIXME
  -- \| Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage :: ModName -> [HWType] -> AigerM [(String, Doc)]
  mkTyPackage _ _ = pure []

  -- FIXME
  -- \| Convert a Netlist HWType to a target HDL type
  hdlType :: BUsage -> HWType -> AigerM Doc
  hdlType _ _ = pretty "hdlType stuff here"

  -- FIXME define the types for each HWType in AIGER
  -- \| Query what kind of type a given HDL type is
  hdlHWTypeKind :: HWType -> State AigerState HWKind
  hdlHWTypeKind h =
    pure
      ( case h of
          Bit -> PrimitiveType
          _ -> UserType
      )

  -- \| Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue :: HWType -> AigerM Doc
  hdlTypeErrValue _ = emptyDoc

  -- \| Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark :: HWType -> AigerM Doc
  hdlTypeMark _ = emptyDoc

  -- \| Create a record selector
  hdlRecSel :: HWType -> Int -> AigerM Doc
  hdlRecSel _ _ = emptyDoc

  -- \| Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig :: LT.Text -> HWType -> AigerM Doc
  hdlSig _ _ = emptyDoc

  -- \| Create a generative block AigerStatement marker
  genStmt :: Bool -> State AigerState Doc
  genStmt _ = emptyDoc

  -- \| Turn a Netlist Declaration to a HDL concurrent block
  inst :: Declaration -> AigerM (Maybe Doc)
  inst _ = pure Nothing

  -- \| Turn a Netlist expression into a HDL expression
  expr ::
    Bool ->
    -- \^ Enclose in parentheses?
    Expr ->
    -- \^ Expr to convert
    AigerM Doc
  expr _ e = case e of
    Identifier id_ _ -> pretty id_
    Literal _ a -> pretty (show a)
    DataCon{} -> pretty "DataCon"
    DataTag{} -> pretty "DataTag"
    BlackBoxE{} -> pretty "BlackBox"
    ToBv{} -> pretty "ToBv"
    FromBv{} -> pretty "FromBv"
    IfThenElse{} -> pretty "IfThenElse"
    Noop -> pretty "Noop"

  -- \| Bit-width of Int,Word,Integer
  iwWidth :: State AigerState Int
  iwWidth = pure 0

  -- \| Convert to a bit-vector
  toBV :: HWType -> LT.Text -> AigerM Doc
  toBV _ _ = emptyDoc

  -- \| Convert from a bit-vector
  fromBV :: HWType -> LT.Text -> AigerM Doc
  fromBV _ _ = emptyDoc

  -- \| Synthesis tool we're generating HDL for
  hdlSyn :: State AigerState HdlSyn
  hdlSyn = undefined

  -- \| setModName
  setModName :: ModName -> AigerState -> AigerState
  setModName _ a = a

  -- \| Set the name of the current top entity
  setTopName :: Identifier -> AigerState -> AigerState
  setTopName _ a = a

  -- \| Get the name of the current top entity
  getTopName :: State AigerState Identifier
  getTopName = undefined

  -- \| setSrcSpan
  setSrcSpan :: SrcSpan -> State AigerState ()
  setSrcSpan _ = pure ()

  -- \| getSrcSpan
  getSrcSpan :: State AigerState SrcSpan
  getSrcSpan = undefined

  -- \| Block of declarations
  blockDecl :: Identifier -> [Declaration] -> AigerM Doc
  blockDecl _ _ = emptyDoc

  addIncludes :: [(String, Doc)] -> State AigerState ()
  addIncludes _ = pure ()

  addLibraries :: [LT.Text] -> State AigerState ()
  addLibraries _ = pure ()

  addImports :: [LT.Text] -> State AigerState ()
  addImports _ = pure ()

  addAndSetData :: FilePath -> State AigerState String
  addAndSetData _ = pure $ show ""

  -- FIXME
  getDataFiles :: State AigerState [(String, FilePath)]
  getDataFiles = pure []

  addMemoryDataFile :: (String, String) -> State AigerState ()
  addMemoryDataFile _ = pure ()

  -- FIXME
  getMemoryDataFiles :: State AigerState [(String, String)]
  getMemoryDataFiles = pure []

  -- FIXME
  ifThenElseExpr :: AigerState -> Bool
  ifThenElseExpr = const False

  -- FIXME
  -- \| Whether -fclash-aggressive-x-optimization-blackboxes was set
  aggressiveXOptBB :: State AigerState AggressiveXOptBB
  aggressiveXOptBB = pure $ AggressiveXOptBB False

  -- FIXME
  -- \| Whether -fclash-no-render-enums was set
  renderEnums :: State AigerState RenderEnums
  renderEnums = pure $ RenderEnums False

  -- FIXME
  -- \| All the domain configurations of design
  domainConfigurations :: State AigerState DomainMap
  domainConfigurations = pure $ emptyDomainMap

  -- FIXME
  -- \| Set the domain configurations
  setDomainConfigurations :: DomainMap -> AigerState -> AigerState
  setDomainConfigurations _ a = a

getOutputNodes :: AigerM [OutputNode]
getOutputNodes = do
  Ap $ use outputNodes
getInputNodes :: AigerM [InputNode]
getInputNodes = do
  Ap $ use inputNodes
getAndNodes :: AigerM [AndNode]
getAndNodes = do
  Ap $ use andNodes
addInputNode :: InputNode -> AigerM ()
addInputNode inpN = do
  ins <- getInputNodes
  Ap $ inputNodes .= inpN : ins
addOutputNode :: OutputNode -> AigerM ()
addOutputNode outN = do
  outs <- getOutputNodes
  Ap $ outputNodes .= outN : outs

addAndNode :: AndNode -> AigerM ()
addAndNode aN = do
  ans <- getAndNodes
  Ap $ andNodes .= aN : ans

getUnsolvedAndNodes :: AigerM [UnsolvedAndNode]
getUnsolvedAndNodes = do
  Ap $ use unsolvedAndNodes

addUnsolvedAndNodes :: UnsolvedAndNode -> AigerM ()
addUnsolvedAndNodes n = do
  ns <- getUnsolvedAndNodes
  Ap $ unsolvedAndNodes .= n : ns

getMaxIndex :: AigerM Int
getMaxIndex = Ap $ use maxIndex

getNumInputs :: AigerM Int
getNumInputs = do
  Ap $ length <$> (use inputNodes)

getNumOutputs :: AigerM Int
getNumOutputs = do
  Ap $ length <$> (use outputNodes)

getNumAndGates :: AigerM Int
getNumAndGates = do
  Ap $ length <$> (use andNodes)

getNewIndex :: AigerM AigerIndex
getNewIndex = do
  i <- getMaxIndex
  Ap $ maxIndex += 1
  pure (AigerIndex i False)

getAssignments :: AigerM (Map.Map AigerPointer AigerExpr)
getAssignments = do
  Ap $ use aigerExpressions

addAssignment :: AigerPointer -> AigerExpr -> AigerM ()
addAssignment ap aa = do
  assignm <- getAssignments
  let newass = Map.insert ap aa assignm
  Ap $ aigerExpressions .= newass
  pure ()

getAigerExpr :: AigerPointer -> AigerM AigerExpr
getAigerExpr ap = do
  assignm <- getAssignments
  let mae = Map.lookup ap assignm
  let ae =
        mae
          `orElse` error
            ( "could not find aigerExprression "
                ++ show ap
                ++ " in \n"
                ++ (unlines $ map show $ Map.assocs assignm)
            )
  pure ae

getIndeces :: AigerExpr -> AigerM [AigerIndex]
getIndeces expr =
  case expr of
    Id i1 -> do
      aigerExprs <- getAigerExpr i1
      i <- getIndeces aigerExprs
      pure i
    Concat es -> do
      concatMapM getIndeces es
    LastRange start end e1 -> do
      i1 <- getIndeces e1
      pure $ reverse $ drop start (take (end) (reverse i1))
    Range start end e1 -> do
      i1 <- getIndeces e1
      pure $ drop start (take (end) i1)
    And ind -> do
      pure [ind]
    Complement i1 -> do
      aigerIndeces <- getIndeces i1
      pure $ map complement aigerIndeces
    BitRange bs -> do
      pure bs
    Empty -> do
      pure []

-- ###################
-- ### EXPRESSIONS ###
-- ###################

convertExprToAigerExpr :: Expr -> AigerM AigerExpr
convertExprToAigerExpr e = case e of
  (Identifier eI Nothing) -> pure $ Id (Pointer (toText eI))
  (Identifier eI (Just a)) -> pure $ modifier (Pointer (toText eI)) a
  (Literal mhwt l) -> pure $ parseLiteral (fst <$> mhwt) l
  (DataCon hwt _ ex) -> parseDataConE hwt ex
  (DataTag _ _) -> error ("TODO found " ++ show e)
  (BlackBoxE n _ _ _ _ templateContext _) -> parseBlackBoxE n templateContext
  (ToBv _ _ e1) -> convertExprToAigerExpr e1
  (FromBv _ _ e1) -> convertExprToAigerExpr e1
  (IfThenElse _ _ _) -> error ("TODO found " ++ show e)
  (Noop) -> pure Empty

parseLiteral :: Maybe (HWType) -> Literal -> AigerExpr
parseLiteral Nothing (NumLit i) = error ("Num Literal without HWType found: " ++ show i)
parseLiteral (Just hwt) (NumLit i) = case hwt of
  Unsigned n -> BitRange $ makeUnsigned i n
  Signed n -> BitRange $ makeSigned i n
  _ ->
    error ("Can not parse num literal with HWType " ++ show hwt)
parseLiteral _ (BitLit b) =
  BitRange
    [ ( case b of
          H -> AigerIndex 0 True
          L -> AigerIndex 0 False
          _ -> undefinedBit
      )
    ]
parseLiteral _ (BoolLit b) = BitRange [AigerIndex 0 b]
parseLiteral _ (BitVecLit i1 i2) = error ("TODO bitVec literal found: " ++ show i1 ++ " " ++ show i2) -- BitRange $ makeUnsigned i2 i1 -- TODO what does the two integers mean??
parseLiteral _ (VecLit ls) = Concat $ map (parseLiteral Nothing) ls -- TODO what about HWType pass on?
parseLiteral _ (StringLit s) = error ("TODO String literal found: " ++ s)

makeUnsigned :: Integer -> Size -> [AigerIndex]
makeUnsigned ii n =
  map
    (\a -> AigerIndex 0 $ not $ divBy2 (div ii (2 ^ a)))
    (reverse [0 .. (n - 1)])
makeSigned :: Integer -> Size -> [AigerIndex]
makeSigned ii n =
  let signed = ii < 0
      unsignedVersion = makeUnsigned (abs ii) n
   in if signed
        then unsignedVersion
        else [AigerIndex 0 signed] ++ makeUnsigned ((2 ^ n) + ii) (n - 1)

divBy2 :: (Integral a) => a -> Bool
divBy2 n = case n `mod` 2 of
  0 -> True
  _ -> False

modifier :: AigerPointer -> Modifier -> AigerExpr
modifier (Id -> pointer) m = case m of
  Indexed (Product _ _ hwts, _, ft) -> Range start end pointer
   where
    frontHWT = take (ft) hwts
    frontSize = sum $ map typeSize frontHWT
    currentHWT = hwts !! ft
    currentSize = typeSize currentHWT
    start = frontSize
    end = start + currentSize
  Sliced (_, s, end) -> Range s end pointer
  -- TODO other modifiers
  _ -> pointer

parseDataConE :: HWType -> [Expr] -> AigerM AigerExpr
parseDataConE h es = do
  case h of
    Product{} -> do
      aes <- mapM convertExprToAigerExpr es
      pure $ Concat aes
    Bit -> case es of
      [e] -> convertExprToAigerExpr e
      _ -> error "Multiple expressions in Bit DataCon"
    Signed _ -> do
      -- TODO what about different size than expressions?
      aes <- mapM convertExprToAigerExpr es
      pure $ Concat aes
    l -> error ("no parser implemented yet for DataCon " ++ show l)

parseBlackBoxE :: Text -> BlackBoxContext -> AigerM AigerExpr
parseBlackBoxE n context =
  let a = show (bbName context)
   in ( case a of
          "\"Clash.Aiger.Base.undefined##\"" -> do
            pure $ BitRange [undefinedBit]
          "\"Clash.Aiger.Base.high\"" -> do
            pure $ BitRange [(AigerIndex 0 True)]
          "\"Clash.Aiger.Base.low\"" -> do
            pure $ BitRange [(AigerIndex 0 False)]
          "\"Clash.Aiger.Base.and\"" -> do
            id0 <- getExpr 0
            id0E <- convertExprToAigerExpr id0
            id1 <- getExpr 1
            id1E <- convertExprToAigerExpr id1
            index <- getNewIndex
            addUnsolvedAndNodes $ UnsolvedAndNode index id0E id1E
            pure $ And index
          "\"Clash.Aiger.Base.complement\"" -> do
            id0 <- getExpr 0
            id0E <- convertExprToAigerExpr id0
            pure $ Complement id0E
          "\"Clash.Aiger.Base.++#\"" -> do
            id1 <- getExpr 1
            id1E <- convertExprToAigerExpr id1
            id2 <- getExpr 2
            id2E <- convertExprToAigerExpr id2
            pure $ Concat [id1E, id2E]
          "\"Clash.Aiger.Base.split#\"" -> do
            -- nat0 <- getNatLit 0
            id1 <- getExpr 1
            id1E <- convertExprToAigerExpr id1
            pure $ id1E
          "\"Clash.Aiger.Base.as\"" -> do
            id0 <- getExpr 3
            convertExprToAigerExpr id0
          -- TODO fix these blackboxes:
          "\"Clash.Sized.Internal.BitVector.toEnum##\"" -> do
            n1 <- getExpr 1
            n1E <- convertExprToAigerExpr n1
            pure $ LastRange 0 1 n1E
          "\"Clash.Sized.Internal.BitVector.fromInteger##\"" -> do
            n1 <- getExpr 1
            n1E <- convertExprToAigerExpr n1
            pure $ LastRange 0 1 n1E
          "\"Clash.Sized.Internal.BitVector.toEnum#\"" -> do
            sz <- getNatLit 0
            n1 <- getExpr 1
            n1E <- convertExprToAigerExpr n1
            pure $ LastRange 0 sz n1E
          "\"Clash.Sized.Internal.BitVector.fromInteger#\"" -> do
            sz <- getNatLit 0
            n1 <- getExpr 2
            n1E <- convertExprToAigerExpr n1
            pure $ LastRange 0 sz n1E
          "\"Clash.Sized.Internal.Unsigned.fromInteger#\"" -> do
            sz <- getNatLit 0
            n1 <- getExpr 1
            n1E <- convertExprToAigerExpr n1
            pure $ LastRange 0 sz n1E
          _ ->
            error
              ("could not parse blackbox " ++ a ++ "\n with context: " ++ show context)
      )
 where
  getExpr :: Int -> AigerM Expr
  getExpr i = do
    inp <- pure $ bbInputs context
    let a = inp !? i
    pure $
      (^. _1)
        (a `orElse` (error $ "could not find index " ++ show i ++ " in " ++ show context))

  getNatLit :: Int -> AigerM Int
  getNatLit i = do
    e <- getExpr i
    case (e) of
      Literal _ (NumLit ii) -> pure (integerToInt ii)
      DataCon _ _ ((Literal _ (NumLit ii)) : _) -> pure (integerToInt ii)
      l ->
        error $
          "could not find literal in " ++ show n ++ ", found " ++ show (l)

-- ####################
-- ### DECLARATIONS ###
-- ####################

parseDeclaration :: Declaration -> AigerM ()
parseDeclaration d = do
  case d of
    (Assignment i _ e) -> do parseAssignment (Pointer (toText i)) e
    (CondAssignment i exprType c compType arms) -> do parseCondAssignment (Pointer (toText i)) c exprType arms compType
    (InstDecl _ _ _ _ _ _ _) -> error ("DECL: " ++ show d)
    (BlackBoxD n _ _ _ _ t) -> (parseBlackBoxD n t)
    (CompDecl _ _) -> error ("DECL: " ++ show d)
    (NetDecl' _ _ _ _) -> pure ()
    (TickDecl _) -> error ("DECL: " ++ show d)
    (Seq _) -> error ("DECL: " ++ show d)
    (ConditionalDecl _ _) -> error ("DECL: " ++ show d)
 where
  parseBlackBoxD n t = do
    aigerExpr <- parseBlackBoxE n t
    let r = bbResults t
        i = case r of
          (((Identifier ii _), _) : _) -> Pointer (toText ii)
          _ -> error "Result of blackboxD not an identifier"
    addAssignment i aigerExpr

    pure ()
  parseAssignment i e = do
    aigerExpr <- convertExprToAigerExpr e
    addAssignment i aigerExpr

  parseCondAssignment ::
    AigerPointer -> Expr -> HWType -> [(Maybe Literal, Expr)] -> HWType -> AigerM ()
  parseCondAssignment pointer scrut exprType arms compType = do
    exprScrut <- convertExprToAigerExpr scrut
    aigerArms <- mapM (armsToAigerExpr) arms
    let zeroes = Concat $ map (\_ -> zero) [0 .. exprSize - 1]
    (outputResults, _) <- foldrM (reduceConcat exprScrut) (zeroes, one) aigerArms
    addAssignment pointer outputResults
   where
    compSize = typeSize compType
    exprSize = typeSize exprType

    zero = (BitRange [(AigerIndex 0 False)])
    one = (BitRange [(AigerIndex 0 True)])

    armsToAigerExpr ::
      (Maybe Literal, Expr) -> AigerM (Maybe AigerExpr, AigerExpr)
    armsToAigerExpr (ml, e) = do
      expr <- convertExprToAigerExpr e
      let a = case ml of
            Nothing -> Nothing
            Just lit -> Just $ parseLiteral (Just (compType)) lit
      pure (a, expr)

    reduceConcat ::
      AigerExpr ->
      (Maybe AigerExpr, AigerExpr) ->
      (AigerExpr, AigerExpr) ->
      AigerM (AigerExpr, AigerExpr)
    reduceConcat _ (Nothing, e) (acc, allprevnomatch) = do
      (result, allnomatch) <- passOutputForCaseNoComp e allprevnomatch
      res <- fullOr result acc exprSize
      pure (res, allnomatch)
    reduceConcat scr (Just comp, e) (acc, allprevnomatch) = do
      (result, allnomatch) <- passOutputForCase scr comp e allprevnomatch
      res <- fullOr result acc exprSize
      pure (res, allnomatch)

    mapExpr ::
      AigerExpr -> Int -> (AigerExpr -> AigerM AigerExpr) -> AigerM AigerExpr
    mapExpr bits size f = Concat <$> mapM f (extractBits bits size)

    passOutputForCase ::
      AigerExpr ->
      AigerExpr ->
      AigerExpr ->
      AigerExpr ->
      AigerM (AigerExpr, AigerExpr)
    passOutputForCase scr pat expr allPrevDontMatch = do
      isEqual <- fullEquals scr pat compSize
      passExpr <- and isEqual allPrevDontMatch
      result <- Concat <$> mapM (and passExpr) (extractBits expr exprSize)
      nextAllPrevNoMatch <- and allPrevDontMatch (Complement isEqual)
      pure (result, nextAllPrevNoMatch)

    passOutputForCaseNoComp ::
      AigerExpr ->
      AigerExpr ->
      AigerM (AigerExpr, AigerExpr)
    passOutputForCaseNoComp expr allPrevDontMatch = do
      let passExpr = allPrevDontMatch
      result <- mapExpr expr exprSize (and passExpr)
      pure (result, zero)

    indexExpr :: Int -> AigerExpr -> AigerExpr
    indexExpr i ex = Range i (i + 1) ex

    reduceAnd bits = foldrM (\a -> \b -> and a b) one bits

    extractBits e size = do
      map (\i -> indexExpr i e) [0 .. size - 1]

    fullOr :: AigerExpr -> AigerExpr -> Int -> AigerM AigerExpr
    fullOr a b size = Concat <$> (mapM (orr a b) [0 .. size - 1])
     where
      orr aa bb i = do
        let ia = (indexExpr i aa)
        let ib = (indexExpr i bb)
        or ia ib

    fullEquals :: AigerExpr -> AigerExpr -> Int -> AigerM AigerExpr
    fullEquals a b size = do
      equalbits <- mapM (eeqq a b) [0 .. size - 1]
      reduceAnd equalbits
     where
      eeqq aa bb i = do
        let ia = (indexExpr i aa)
        let ib = (indexExpr i bb)
        equals ia ib

    and a b = do
      index <- getNewIndex
      addUnsolvedAndNodes $ UnsolvedAndNode index a b
      pure $ And index
    or a b = do
      aa <- (and (Complement a) (Complement b))
      pure $ Complement aa
    xor a b = do
      aandb <- (and a b)
      invaandb <- (and (Complement a) (Complement b))
      Complement <$> (or aandb invaandb)
    equals a b = Complement <$> (xor a b)

genAIGER ::
  ClashOpts ->
  ModName ->
  SrcSpan ->
  IdentifierSet ->
  UsageMap ->
  Component ->
  AigerM ((String, Doc), [(String, Doc)])
genAIGER _ _ _ _ _ c = do
  v <- componentToAiger c
  return ((TextS.unpack (Id.toText cname), v), [])
 where
  cname = componentName c

componentToState :: Component -> AigerM ()
componentToState c = do
  let decls = declarations c
  saveInputs c
  _ <- mapM parseDeclaration $ decls
  saveAnds
  saveOutputs c

componentToAiger :: Component -> AigerM Doc
componentToAiger c = do
  componentToState c

  -- traceM ""
  -- traceState
  -- The graph has been parsed and now we generate the file from the aigerM State
  numInputs <- getNumInputs
  numOutputs <- getNumOutputs
  numAndGates <- getNumAndGates
  mInd <- getMaxIndex
  let header =
        pretty
          ( "aag "
              <> show (mInd - 1)
              <> " "
              <> show numInputs
              <> " "
              <> show numLatches
              <> " "
              <> show numOutputs
              <> " "
              <> show numAndGates
          )

  header
    <> writeInputs
    <> writeOutputs
    <> writeAnds
    <> symbolTable
    <> commentBlock Nothing
 where
  numLatches = 0 :: Int -- TODO
  symbolTable = emptyDoc
  commentBlock :: Maybe String -> AigerM Doc
  commentBlock cmmt = case cmmt of
    Just cmt -> pretty "c" <> line <> pretty cmt
    Nothing -> emptyDoc

traceState :: AigerM ()
traceState = do
  ass <- getAssignments
  traceM "assignments"
  traceM (unlines $ map (\(i, a) -> show i ++ " = " ++ show a) $ Map.assocs ass)
  traceM "End trace"
  pure ()

-- Saving netlist to state
saveInputs :: Component -> AigerM ()
saveInputs c = do
  let i = inputs c
  _ <- mapM saveInput i
  pure ()
 where
  saveInput (ident, hwtype) = do
    let aigerPointer = (Pointer (toText ident))
    indeces <- mapM (go) [0 .. amount - 1]
    addAssignment aigerPointer (BitRange indeces)
    pure ()
   where
    amount = typeSize hwtype
    go _ = do
      i <- getNewIndex
      addInputNode (InputNode i)
      pure i

saveOutputs :: Component -> AigerM ()
saveOutputs c = do
  let o = outputs c
  _ <- mapM saveOutput o
  pure ()
 where
  saveOutput (_, (ident, hwt), maybeExpr) = do
    expr <- case maybeExpr of
      Just e -> convertExprToAigerExpr e
      Nothing -> getAigerExpr (Pointer (toText ident))
    indeces <- getIndeces expr
    let final = drop (length indeces - typeSize hwt) indeces
    _ <- mapM (addON) final
    pure ()
   where
    addON i = do
      addOutputNode (OutputNode i)

saveAnds :: AigerM ()
saveAnds = do
  ands <- getUnsolvedAndNodes
  _ <- mapM go ands
  pure ()
 where
  go (UnsolvedAndNode ai ap1 ap2) = do
    ail <- getIndex ap1 0
    air <- getIndex ap2 0
    addAndNode $ AndNode ai ail air
  getIndex ap i = do
    indeces <- getIndeces ap
    pure $
      indeces !? i
        `orElse` error
          ("OutofBounds with getIndex, " ++ show ap ++ " with indeces: " ++ show indeces)

-- Writing state

writeOutput :: OutputNode -> AigerM Doc
writeOutput (OutputNode ref) = pretty $ toInt ref

writeOutputs :: AigerM (Doc)
writeOutputs = do
  i <- getOutputNodes
  n <- getNumOutputs
  if n == 0 then emptyDoc else (line <> (vcat $ mapM writeOutput i))

writeInput :: InputNode -> AigerM Doc
writeInput (InputNode i) = pretty $ toInt i

writeInputs :: AigerM Doc
writeInputs = do
  i <- getInputNodes
  n <- getNumInputs
  if n == 0 then emptyDoc else (line <> (vcat $ mapM writeInput i))

writeAnd :: AndNode -> AigerM Doc
writeAnd (AndNode ref l r) =
  pretty (toInt ref)
    <> pretty " "
    <> pretty (toInt l)
    <> pretty " "
    <> pretty (toInt r)

writeAnds :: AigerM (Doc)
writeAnds = do
  i <- getAndNodes
  n <- getNumAndGates
  if n == 0 then emptyDoc else (line <> (vcat $ mapM writeAnd i))
