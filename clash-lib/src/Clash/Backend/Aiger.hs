module Clash.Backend.Aiger (AigerState) where

import Clash.Annotations.Primitive (HDL (..))
import Clash.Backend
import Clash.Driver.Types (ClashOpts)
import Clash.Netlist.BlackBox.Types (HdlSyn)
import Clash.Netlist.Types hiding (Usage)
import Clash.Util
import Control.Monad.State (State)
import Data.HashSet (HashSet)
import Data.Monoid (Ap (..))
import qualified Data.Text.Lazy as LT
import Data.Text.Prettyprint.Doc.Extra (Doc )
import qualified System.FilePath
import Prettyprinter (Pretty(..))

data AigerState = AigerState
  {}


instance HasIdentifierSet AigerState where
  identifierSet = undefined

instance HasUsageMap AigerState where
  usageMap = undefined

type AigerM = Ap (State AigerState)

instance Backend AigerState where
  -- \| Initial state for state monad
  initBackend _opts =
    AigerState
      {}

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
  extension = const "aig"

  -- \| Get the set of types out of AigerState
  extractTypes :: AigerState -> HashSet HWType
  extractTypes = undefined

  -- FIXME
  -- \| Generate HDL for a Netlist component
  genHDL :: ClashOpts -> ModName -> SrcSpan -> IdentifierSet -> UsageMap -> Component -> AigerM ((String, Doc), [(String, Doc)])
  genHDL = genAIGER

  -- FIXME
  -- \| Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage :: ModName -> [HWType] -> AigerM [(String, Doc)]
  mkTyPackage _ _ = pure []

  -- FIXME
  -- \| Convert a Netlist HWType to a target HDL type
  hdlType :: Usage -> HWType -> AigerM Doc
  hdlType _ _ = pure $ pretty "hdlType stuff here"

  -- FIXME define the types for each HWType in AIGER
  -- \| Query what kind of type a given HDL type is
  hdlHWTypeKind :: HWType -> State AigerState HWKind
  hdlHWTypeKind _ = pure PrimitiveType

  -- \| Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue :: HWType -> AigerM Doc
  hdlTypeErrValue = undefined

  -- \| Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark :: HWType -> AigerM Doc
  hdlTypeMark = undefined

  -- \| Create a record selector
  hdlRecSel :: HWType -> Int -> AigerM Doc
  hdlRecSel = undefined

  -- \| Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig :: LT.Text -> HWType -> AigerM Doc
  hdlSig = undefined

  -- \| Create a generative block AigerStatement marker
  genStmt :: Bool -> State AigerState Doc
  genStmt = undefined

  -- \| Turn a Netlist Declaration to a HDL concurrent block
  inst :: Declaration -> AigerM (Maybe Doc)
  inst = undefined

  -- \| Turn a Netlist expression into a HDL expression
  expr ::
    Bool ->
    -- \^ Enclose in parentheses?
    Expr ->
    -- \^ Expr to convert
    AigerM Doc
  expr = undefined

  -- \| Bit-width of Int,Word,Integer
  iwWidth :: State AigerState Int
  iwWidth = undefined

  -- \| Convert to a bit-vector
  toBV :: HWType -> LT.Text -> AigerM Doc
  toBV = undefined

  -- \| Convert from a bit-vector
  fromBV :: HWType -> LT.Text -> AigerM Doc
  fromBV = undefined

  -- \| Synthesis tool we're generating HDL for
  hdlSyn :: State AigerState HdlSyn
  hdlSyn = undefined

  -- \| setModName
  setModName :: ModName -> AigerState -> AigerState
  setModName = undefined

  -- \| Set the name of the current top entity
  setTopName :: Identifier -> AigerState -> AigerState
  setTopName = undefined

  -- \| Get the name of the current top entity
  getTopName :: State AigerState Identifier
  getTopName = undefined

  -- \| setSrcSpan
  setSrcSpan :: SrcSpan -> State AigerState ()
  setSrcSpan = undefined

  -- \| getSrcSpan
  getSrcSpan :: State AigerState SrcSpan
  getSrcSpan = undefined

  -- \| Block of declarations
  blockDecl :: Identifier -> [Declaration] -> AigerM Doc
  blockDecl = undefined

  addIncludes :: [(String, Doc)] -> State AigerState ()
  addIncludes = undefined

  addLibraries :: [LT.Text] -> State AigerState ()
  addLibraries = undefined

  addImports :: [LT.Text] -> State AigerState ()
  addImports = undefined

  addAndSetData :: FilePath -> State AigerState String
  addAndSetData = undefined

  -- FIXME
  getDataFiles :: State AigerState [(String, FilePath)]
  getDataFiles = pure []

  addMemoryDataFile :: (String, String) -> State AigerState ()
  addMemoryDataFile = undefined

  -- FIXME
  getMemoryDataFiles :: State AigerState [(String, String)]
  getMemoryDataFiles = pure []

  ifThenElseExpr :: AigerState -> Bool
  ifThenElseExpr = undefined

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
  setDomainConfigurations = undefined

-- FIXME
genAIGER ::
  ClashOpts ->
  ModName ->
  SrcSpan ->
  IdentifierSet ->
  UsageMap ->
  Component ->
  AigerM ((String, Doc), [(String, Doc)])
genAIGER _ _ _ _ _ c= do
  return (("helloThere", pretty $ show c),[])

