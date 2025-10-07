{-# LANGUAGE TemplateHaskell #-}

module Clash.Backend.Aiger (AigerState) where

import Clash.Annotations.Primitive (HDL (..))
import Control.Lens (use, (+=), (.=))
import Control.Lens.TH (makeLenses)
import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State)
import Data.Foldable (foldrM)
import Data.HashSet (HashSet)
import Data.List ((!?))
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Tuple.Extra (fst3)
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
import Clash.Debug (trace)
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
data LatchNode = LatchNode AigerIndex AigerIndex deriving (Show)
data AndNode = AndNode AigerIndex AigerIndex AigerIndex deriving (Show)
data UnsolvedAndNode = UnsolvedAndNode AigerIndex AigerExpr AigerExpr
  deriving (Show)

data AigerExpr
  = Id AigerPointer
  | Concat [AigerExpr]
  | LeftRange Int Int AigerExpr
  | RightRange Int Int AigerExpr
  | And AigerIndex
  | Complement AigerExpr
  | Indeces [AigerIndex]
  | Empty
  deriving (Show)

data AigerState = AigerState
  { _maxIndex :: Int
  , _inputNodes :: [InputNode]
  , _outputNodes :: [OutputNode]
  , _latchNodes :: [LatchNode]
  , _andNodes :: [AndNode]
  , _unsolvedAndNodes :: [UnsolvedAndNode]
  , _aigerExpressions :: Map.Map AigerPointer AigerExpr
  }

type AigerM = Ap (State AigerState)

-- Needed Boilerplate code for state and backend stuff
makeLenses ''AigerState

instance HasIdentifierSet AigerState where
  identifierSet = undefined

instance HasUsageMap AigerState where
  usageMap = undefined

getOutputNodes :: AigerM [OutputNode]
getOutputNodes = do
  Ap $ use outputNodes

getLatchNodes :: AigerM [LatchNode]
getLatchNodes = do
  Ap $ use latchNodes

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

addLatchNode :: LatchNode -> AigerM ()
addLatchNode latchN = do
  latches <- getLatchNodes
  Ap $ latchNodes .= latchN : latches

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
  pure $
    Map.lookup ap assignm
      `orElse` error
        ( "could not find aigerExprression "
            ++ show ap
            ++ " in \n"
            ++ (unlines $ map show $ Map.assocs assignm)
        )

-- ####################
-- ## STATE TO AIGER ##
-- ####################

stateToAiger :: AigerM Doc
stateToAiger =
  writeHeader
    <> writeInputs
    <> writeLatches
    <> writeOutputs
    <> writeAnds
    <> symbolTable
    <> commentBlock
 where
  -- FIX for FUTURE WORK
  symbolTable :: AigerM Doc
  symbolTable = emptyDoc

  -- FIX for FUTURE WORK
  commentBlock :: AigerM Doc
  commentBlock = emptyDoc

  createLinesOrEmpty :: (a -> AigerM Doc) -> [a] -> AigerM Doc
  createLinesOrEmpty _ [] = emptyDoc
  createLinesOrEmpty app i = (line <> (vcat $ mapM app i))

  writeHeader :: AigerM Doc
  writeHeader = do
    mInd <- getMaxIndex
    numInputs <- length <$> getInputNodes
    numLatches <- length <$> getLatchNodes
    numOutputs <- length <$> getOutputNodes
    numAndGates <- length <$> getAndNodes
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

  writeInputs :: AigerM Doc
  writeInputs = do
    i <- getInputNodes
    createLinesOrEmpty writeInput i
   where
    writeInput :: InputNode -> AigerM Doc
    writeInput (InputNode i) = pretty (show i)

  writeLatches :: AigerM (Doc)
  writeLatches = do
    i <- getLatchNodes
    createLinesOrEmpty writeLatch i
   where
    writeLatch :: LatchNode -> AigerM Doc
    writeLatch (LatchNode ref input) = pretty (show ref <> " " <> show input)

  writeOutputs :: AigerM (Doc)
  writeOutputs = do
    i <- getOutputNodes
    createLinesOrEmpty writeOutput i
   where
    writeOutput :: OutputNode -> AigerM Doc
    writeOutput (OutputNode ref) = pretty (show ref)

  writeAnds :: AigerM (Doc)
  writeAnds = do
    i <- getAndNodes
    createLinesOrEmpty writeAnd i
   where
    writeAnd :: AndNode -> AigerM Doc
    writeAnd (AndNode ref l r) = pretty (show ref <> " " <> show l <> " " <> show r)

undefinedBit :: AigerM AigerIndex
undefinedBit = do
  i <- getNewIndex
  addInputNode (InputNode i)
  pure i

instance Backend AigerState where
  -- \| Initial state for state monad
  initBackend _opts =
    AigerState
      { _maxIndex = 1
      , _inputNodes = []
      , _latchNodes = []
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

  -- \| Set the domain configurations
  setDomainConfigurations :: DomainMap -> AigerState -> AigerState
  setDomainConfigurations _ a = a

genAIGER ::
  ClashOpts ->
  ModName ->
  SrcSpan ->
  IdentifierSet ->
  UsageMap ->
  Component ->
  AigerM ((String, Doc), [(String, Doc)])
genAIGER _ _ _ _ _ c = do
  componentToState c
  doc <- stateToAiger
  return ((cName, doc), [])
 where
  cName = TextS.unpack $ Id.toText $ componentName c

-- ####################
-- ## CLASH TO STATE ##
-- ####################

componentToState :: Component -> AigerM ()
componentToState c = do
  let decls = declarations c
  inputsToState c
  _ <- mapM parseDeclaration $ decls
  solveAndIndeces
  solveOutputIndeces c

inputsToState :: Component -> AigerM ()
inputsToState c = do
  let i = inputs c
  _ <- mapM saveInput i
  pure ()
 where
  saveInput (ident, hwtype) = do
    indeces <- mapM createInputNode [0 .. amount - 1]
    addAssignment (Pointer (toText ident)) (Indeces indeces)
   where
    amount = typeSize hwtype
    createInputNode _ = do
      i <- getNewIndex
      addInputNode (InputNode i)
      pure i

parseDeclaration :: Declaration -> AigerM ()
parseDeclaration (Assignment ident _ expr) = do
  let pointer = (Pointer (toText ident))
  aigerExpr <- convertExprToAigerExpr expr
  addAssignment pointer aigerExpr
parseDeclaration (BlackBoxD n _ _ _ _ t) = do
  aigerExpr <- parseBlackBoxE n t
  let r = bbResults t
      i = case r of
        (((Identifier ii _), _) : _) -> Pointer (toText ii)
        _ -> error "Result of blackboxD not an identifier"
  addAssignment i aigerExpr
parseDeclaration (CondAssignment ident exprType scrut compType arms) = do
  let pointer = (Pointer (toText ident))
  exprScrut <- convertExprToAigerExpr scrut
  aigerArms <- mapM (armsToAigerExpr) arms
  let zeroes = Concat $ map (const zeroBit) [0 .. exprSize - 1]
  (outputResults, _) <- foldrM (reduceConcat exprScrut) (zeroes, oneBit) aigerArms
  addAssignment pointer outputResults
 where
  compSize = typeSize compType
  exprSize = typeSize exprType

  zeroBit = (Indeces [(AigerIndex 0 False)])
  oneBit = (Indeces [(AigerIndex 0 True)])

  armsToAigerExpr ::
    (Maybe Literal, Expr) -> AigerM (Maybe AigerExpr, AigerExpr)
  armsToAigerExpr (ml, e) = do
    expr <- convertExprToAigerExpr e
    maybeAigerArm <- case ml of
      Nothing -> pure Nothing
      Just lit ->
        Just
          <$> convertExprToAigerExpr (Literal (Just (compType, compSize)) lit)
    pure (maybeAigerArm, expr)

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
    pure (result, zeroBit)

  indexExpr :: Int -> AigerExpr -> AigerExpr
  indexExpr i ex = LeftRange i (i + 1) ex

  reduceAnd bits = foldrM (\a -> \b -> and a b) oneBit bits

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
parseDeclaration (NetDecl'{}) = pure () -- skip
parseDeclaration d = trace ("DECL: " ++ show d) $ pure ()

convertExprToAigerExpr :: Expr -> AigerM AigerExpr
convertExprToAigerExpr (Identifier eI Nothing) = pure $ Id (Pointer (toText eI))
convertExprToAigerExpr (Identifier eI (Just a)) = pure $ modifier (Pointer (toText eI)) a
 where
  modifier :: AigerPointer -> Modifier -> AigerExpr
  modifier (Id -> pointer) m = case m of
    Sliced (_, s, end) -> LeftRange s end pointer
    Indexed (Product _ _ listOfHWT, _, field) -> LeftRange start end pointer
     where
      frontHWT = take (field) listOfHWT
      frontSize = sum $ map typeSize frontHWT
      start = frontSize
      currentHWT = listOfHWT !! field
      currentSize = typeSize currentHWT
      end = start + currentSize
    _ -> pointer

-- Literal
convertExprToAigerExpr (Literal Nothing (NumLit i)) = error ("Num Literal without HWType found: " ++ show i)
convertExprToAigerExpr (Literal (Just (hwt, _)) (NumLit i)) = case hwt of
  Unsigned n -> pure $ Indeces $ makeUnsigned i n
  Signed n -> pure $ Indeces $ makeSigned i n
  _ -> error ("Can not parse num literal with HWType " ++ show hwt)
 where
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
convertExprToAigerExpr (Literal _ (BitLit b)) = do
  bit <- case b of
    H -> pure $ AigerIndex 0 True
    L -> pure $ AigerIndex 0 False
    _ -> undefinedBit
  pure $ Indeces [bit]
convertExprToAigerExpr (Literal _ (BoolLit b)) = pure $ Indeces [AigerIndex 0 b]
convertExprToAigerExpr (Literal _ (VecLit ls)) = Concat <$> mapM (convertExprToAigerExpr . Literal Nothing) ls
convertExprToAigerExpr (Literal _ a) = error ("TODO unsupported literal found: " ++ show a)
-- Data constructor
convertExprToAigerExpr (DataCon (Product{}) _ ex) = do
  aes <- mapM convertExprToAigerExpr ex
  pure $ Concat aes
convertExprToAigerExpr (DataCon _ (DC (Void _, -1)) ex) = do
  aes <- mapM convertExprToAigerExpr ex
  pure $ Concat aes
-- Blackbox
convertExprToAigerExpr (BlackBoxE name _ _ _ _ context _) = parseBlackBoxE name context
-- (Recursive) skip
convertExprToAigerExpr (ToBv _ _ e1) = convertExprToAigerExpr e1
convertExprToAigerExpr (FromBv _ _ e1) = convertExprToAigerExpr e1
convertExprToAigerExpr (Noop) = pure Empty
convertExprToAigerExpr e = error ("TODO found " ++ show e)

parseBlackBoxE :: Text -> BlackBoxContext -> AigerM AigerExpr
parseBlackBoxE name context = do
  let functionName = show (bbName context)
  case functionName of
    -- Base module
    "\"Clash.Aiger.Base.undefined##\"" -> do
      ub <- undefinedBit
      pure $ Indeces [ub]
    "\"Clash.Aiger.Base.high\"" -> do
      pure $ Indeces [(AigerIndex 0 True)]
    "\"Clash.Aiger.Base.low\"" -> do
      pure $ Indeces [(AigerIndex 0 False)]
    "\"Clash.Aiger.Base.and\"" -> do
      let id0 = getExpr 0
      id0E <- convertExprToAigerExpr id0
      let id1 = getExpr 1
      id1E <- convertExprToAigerExpr id1
      index <- getNewIndex
      addUnsolvedAndNodes $ UnsolvedAndNode index id0E id1E
      pure $ And index
    "\"Clash.Aiger.Base.complement\"" -> do
      let id0 = getExpr 0
      id0E <- convertExprToAigerExpr id0
      pure $ Complement id0E
    "\"Clash.Aiger.Base.++#\"" -> do
      let id1 = getExpr 1
      id1E <- convertExprToAigerExpr id1
      let id2 = getExpr 2
      id2E <- convertExprToAigerExpr id2
      pure $ Concat [id1E, id2E]
    "\"Clash.Aiger.Base.split#\"" -> do
      let id1 = getExpr 1
      id1E <- convertExprToAigerExpr id1
      pure $ id1E
    "\"Clash.Aiger.Base.as\"" -> do
      let id0 = getExpr 3
      convertExprToAigerExpr id0
    -- Integer extract to datatype
    "\"Clash.Sized.Internal.BitVector.fromInteger##\"" -> do
      let n1 = getExpr 1
      n1E <- convertExprToAigerExpr n1
      pure $ RightRange 0 1 n1E
    "\"Clash.Sized.Internal.BitVector.fromInteger#\"" -> do
      let sz = getNatLit 0
      let n1 = getExpr 2
      n1E <- convertExprToAigerExpr n1
      pure $ RightRange 0 sz n1E
    "\"Clash.Sized.Internal.Unsigned.fromInteger#\"" -> do
      let sz = getNatLit 0
      let n1 = getExpr 1
      n1E <- convertExprToAigerExpr n1
      pure $ RightRange 0 sz n1E
    "\"Clash.Sized.Internal.Signed.fromInteger#\"" -> do
      let sz = getNatLit 0
      let n1 = getExpr 1
      n1E <- convertExprToAigerExpr n1
      pure $ RightRange 0 sz n1E
    -- Errors are unsigned
    "\"Clash.XException.errorX\"" -> do
      let resultHWTs = snd $ unzip $ bbResults context
      let resultSize = sum $ map typeSize resultHWTs
      undefinedBitList <- mapM (const undefinedBit) [0 .. resultSize - 1]
      pure $ Indeces undefinedBitList
    _ ->
      error
        ( "could not parse blackbox "
            ++ functionName
            ++ "\n with context: "
            ++ show context
        )
 where
  getExpr :: Int -> Expr
  getExpr i =
    let inp = bbInputs context !? i
     in (fst3 <$> inp)
          `orElse` (error $ "could not find index " ++ show i ++ " in " ++ show context)
  getNatLit :: Int -> Int
  getNatLit i = case getExpr i of
    (Literal _ (NumLit ii)) -> integerToInt ii
    (DataCon _ _ ((Literal _ (NumLit ii)) : _)) -> integerToInt ii
    l -> error ("could not find literal in " ++ show name ++ ", found " ++ show (l))

-- Solving Ands
solveAndIndeces :: AigerM ()
solveAndIndeces = do
  ands <- getUnsolvedAndNodes
  _ <- mapM go ands
  pure ()
 where
  go (UnsolvedAndNode ai ap1 ap2) = do
    ail <- getIndex ap1 0
    air <- getIndex ap2 0
    addAndNode $ AndNode ai ail air
  getIndex :: AigerExpr -> Int -> AigerM AigerIndex
  getIndex ap i = do
    indeces <- getIndeces ap
    pure $
      indeces !? i
        `orElse` error
          ("OutofBounds with getIndex, " ++ show ap ++ " with indeces: " ++ show indeces)

-- Solving outputs
solveOutputIndeces :: Component -> AigerM ()
solveOutputIndeces c = do
  let o = outputs c
  _ <- mapM saveOutput o
  pure ()
 where
  saveOutput (_, (ident, hwt), maybeExpr) = do
    expr <- case maybeExpr of
      Just e -> convertExprToAigerExpr e
      Nothing -> getAigerExpr (Pointer (toText ident))
    indeces <- getIndeces expr
    let resizedIndecesByHWT = drop (length indeces - typeSize hwt) indeces
    _ <- mapM (addON) resizedIndecesByHWT
    pure ()
   where
    addON i = do
      addOutputNode (OutputNode i)

getIndeces :: AigerExpr -> AigerM [AigerIndex]
getIndeces (Id i1) = do
  aigerExpr <- getAigerExpr i1
  getIndeces aigerExpr
getIndeces (Concat es) = concatMapM getIndeces es
getIndeces (RightRange start end e1) = do
  i1 <- getIndeces e1
  pure $ reverse $ drop start (take (end) (reverse i1))
getIndeces (LeftRange start end e1) = do
  i1 <- getIndeces e1
  pure $ drop start (take (end) i1)
getIndeces (And ind) = pure [ind]
getIndeces (Complement i1) = do
  aigerIndeces <- getIndeces i1
  pure $ map complement aigerIndeces
getIndeces (Indeces bs) = pure bs
getIndeces Empty = pure []
