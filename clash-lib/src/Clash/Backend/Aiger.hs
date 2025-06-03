{-# LANGUAGE TemplateHaskell #-}

module Clash.Backend.Aiger (AigerState) where

import Clash.Annotations.Primitive (HDL (..))
import Control.Lens
import Control.Monad.State (State)
import Data.HashSet (HashSet)
import Data.Monoid (Ap (..))

import qualified Data.Text as TextS
import qualified Data.Text.Lazy as LT
import qualified System.FilePath

import Clash.Backend hiding (Usage)
import Clash.Driver.Types (ClashOpts)
import Clash.Netlist.BlackBox.Types (HdlSyn)
import Clash.Netlist.Types (Usage)
import Clash.Netlist.Types hiding (Literal, Usage)
import Clash.Netlist.Util (typeSize)
import Clash.Util
import Data.Text.Prettyprint.Doc.Extra

import qualified Clash.Backend
import qualified Clash.Netlist.Id as Id

type BUsage = Clash.Backend.Usage

class RelatedIdentifier s where
  bitIndex :: s -> Int
  identifier :: s -> Identifier

data InputNode = InputNode Identifier Int Int
data OutputNode = OutputNode Identifier Int (Maybe Int)

instance RelatedIdentifier InputNode where
  bitIndex (InputNode _ b _) = b
  identifier (InputNode i _ _) = i

instance RelatedIdentifier OutputNode where
  bitIndex (OutputNode _ b _) = b
  identifier (OutputNode i _ _) = i

data AigerState = AigerState
  { _maxIndex :: Int
  , _inputNodes :: [InputNode]
  , _outputNodes :: [OutputNode]
  }

makeLenses ''AigerState

instance HasIdentifierSet AigerState where
  identifierSet = undefined

instance HasUsageMap AigerState where
  usageMap = undefined

type AigerM = Ap (State AigerState)

instance Backend AigerState where -- \| Initial state for state monad
  initBackend _opts = AigerState{_maxIndex = 0, _inputNodes = [], _outputNodes = []}

  -- \| What HDL is the backend generating
  hdlKind :: AigerState -> HDL
  hdlKind = const AIGER

  -- \| Location for the primitive definitions
  primDirs :: AigerState -> IO [FilePath]
  primDirs = const $ do
    root <- primsRoot
    return [root System.FilePath.</> "aiger", root System.FilePath.</> "common"]

  -- \| Name of backend, used for directory to put output files in. Should be
  --   constant function / ignore argument.
  name :: AigerState -> String
  name = const "aiger"

  -- \| File extension for target langauge
  extension :: AigerState -> String
  extension = const "aig"

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
  hdlHWTypeKind _ = pure PrimitiveType

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
  expr _ _ = emptyDoc

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

genAIGER ::
  ClashOpts ->
  ModName ->
  SrcSpan ->
  IdentifierSet ->
  UsageMap ->
  Component ->
  AigerM ((String, Doc), [(String, Doc)])
genAIGER _ _ _ _ _ c = do
  v <- (componentToAiger c <> line <> commentBlock)
  return ((TextS.unpack (Id.toText cname), v), [])
 where
  cname = componentName c
  cmmt = ""
  commentBlock = if cmmt == "" then emptyDoc else (pretty "c" <> line <> pretty cmmt)

-- FIXME
componentToAiger :: Component -> AigerM Doc
componentToAiger c = do
  saveInputs c
  _ <- mapM parseDeclaration $ declarations c

  i <- Ap $ use inputNodes
  let numInputs = length i
  mInd <- Ap $ use maxIndex
  let headerLine =
        pretty $
          "aag "
            <> show mInd
            <> " "
            <> show numInputs
            <> " "
            <> show numLatches
            <> " "
            <> show numOutputs
            <> " "
            <> show numAndGates

  (headerLine <> line <> writeInputs <> writeOutputs <> line <> symbolTable)
 where
  numLatches = 0 :: Int
  numOutputs = sum $ map typeSize $ map (\(_, a, _) -> snd a) $ outputs c
  numAndGates = 0 :: Int
  symbolTable = emptyDoc -- pretty $ show $ declarations c

getIndex :: AigerM Int
getIndex = Ap $ use maxIndex

getNewIndex :: AigerM Int
getNewIndex = do
  i <- getIndex
  Ap $ maxIndex += 1
  pure i

saveInput :: (Identifier, HWType) -> AigerM ()
saveInput (ident, hwtype) = do
  innotes <- Ap $ use inputNodes
  newInputNodes <-
    mapM
      ( \i ->
          ( do
              newIndex <- getNewIndex
              pure $ InputNode ident i newIndex
          )
      )
      [0 .. amount]
  Ap $ inputNodes .= (innotes ++ newInputNodes)
  pure ()
 where
  amount = typeSize hwtype

saveInputs :: Component -> AigerM ()
saveInputs c = do
  _ <- mapM saveInput i
  pure ()
 where
  i = inputs c

writeInput :: InputNode -> AigerM Doc
writeInput (InputNode _ _ i) = pretty i

writeInputs :: AigerM Doc
writeInputs = do
  i <- Ap $ use inputNodes
  vcat $ mapM writeInput i

assignmentIdentifierToIdentifier :: Identifier -> Identifier -> AigerM ()
assignmentIdentifierToIdentifier i i2 = pure ()

parseAssignmentDeclaration :: Identifier -> Usage -> Expr -> AigerM ()
parseAssignmentDeclaration i u e = case e of
  Identifier rI mM -> assignmentIdentifierToIdentifier i rI
  _ -> pure ()

parseDeclaration :: Declaration -> AigerM ()
parseDeclaration d = case d of
  Assignment i u e -> parseAssignmentDeclaration i u e
  _ -> pure ()

writeOutput :: OutputNode -> AigerM Doc
writeOutput (OutputNode _ _ ref) = case ref of
  Just ind -> pretty ind
  _ -> emptyDoc

writeOutputs :: AigerM Doc
writeOutputs = do
  o <- Ap $ use outputNodes
  vcat $ mapM writeOutput o
