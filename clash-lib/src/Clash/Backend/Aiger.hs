module Clash.Backend.Aiger () where

import Clash.Annotations.Primitive (HDL)
import Clash.Backend
import Clash.Driver.Types (ClashOpts)
import Clash.Netlist.BlackBox.Types (HdlSyn)
import Clash.Netlist.Types hiding (Usage)
import Clash.Util
import Control.Monad.State (State)
import Data.HashSet (HashSet)
import Data.Monoid (Ap)
import qualified Data.Text.Lazy as LT
import Data.Text.Prettyprint.Doc.Extra (Doc)

data AigerState = AigerState
  {
  }

instance HasIdentifierSet AigerState where
  identifierSet = undefined

instance HasUsageMap AigerState where
  usageMap = undefined

instance Backend AigerState where
  -- \| Initial state for state monad
  initBackend :: ClashOpts -> state
  initBackend = undefined

  -- \| What HDL is the backend generating
  hdlKind :: state -> HDL
  hdlKind = undefined

  -- \| Location for the primitive definitions
  primDirs :: state -> IO [FilePath]
  primDirs = undefined

  -- \| Name of backend, used for directory to put output files in. Should be
  --   constant function / ignore argument.
  name :: state -> String
  name = undefined

  -- \| File extension for target langauge
  extension :: state -> String
  extension = undefined

  -- \| Get the set of types out of state
  extractTypes :: state -> HashSet HWType
  extractTypes = undefined

  -- \| Generate HDL for a Netlist component
  genHDL :: ClashOpts -> ModName -> SrcSpan -> IdentifierSet -> UsageMap -> Component -> Ap (State state) ((String, Doc), [(String, Doc)])
  genHDL = undefined

  -- \| Generate a HDL package containing type definitions for the given HWTypes
  mkTyPackage :: ModName -> [HWType] -> Ap (State state) [(String, Doc)]
  mkTyPackage = undefined

  -- \| Convert a Netlist HWType to a target HDL type
  hdlType :: Usage -> HWType -> Ap (State state) Doc
  hdlType = undefined

  -- \| Query what kind of type a given HDL type is
  hdlHWTypeKind :: HWType -> State state HWKind
  hdlHWTypeKind = undefined

  -- \| Convert a Netlist HWType to an HDL error value for that type
  hdlTypeErrValue :: HWType -> Ap (State state) Doc
  hdlTypeErrValue = undefined

  -- \| Convert a Netlist HWType to the root of a target HDL type
  hdlTypeMark :: HWType -> Ap (State state) Doc
  hdlTypeMark = undefined

  -- \| Create a record selector
  hdlRecSel :: HWType -> Int -> Ap (State state) Doc
  hdlRecSel = undefined

  -- \| Create a signal declaration from an identifier (Text) and Netlist HWType
  hdlSig :: LT.Text -> HWType -> Ap (State state) Doc
  hdlSig = undefined

  -- \| Create a generative block statement marker
  genStmt :: Bool -> State state Doc
  genStmt = undefined

  -- \| Turn a Netlist Declaration to a HDL concurrent block
  inst :: Declaration -> Ap (State state) (Maybe Doc)
  inst = undefined

  -- \| Turn a Netlist expression into a HDL expression
  expr ::
    Bool ->
    -- \^ Enclose in parentheses?
    Expr ->
    -- \^ Expr to convert
    Ap (State state) Doc
  expr = undefined

  -- \| Bit-width of Int,Word,Integer
  iwWidth :: State state Int
  iwWidth = undefined

  -- \| Convert to a bit-vector
  toBV :: HWType -> LT.Text -> Ap (State state) Doc
  toBV = undefined

  -- \| Convert from a bit-vector
  fromBV :: HWType -> LT.Text -> Ap (State state) Doc
  fromBV = undefined

  -- \| Synthesis tool we're generating HDL for
  hdlSyn :: State state HdlSyn
  hdlSyn = undefined

  -- \| setModName
  setModName :: ModName -> state -> state
  setModName = undefined

  -- \| Set the name of the current top entity
  setTopName :: Identifier -> state -> state
  setTopName = undefined

  -- \| Get the name of the current top entity
  getTopName :: State state Identifier
  getTopName = undefined

  -- \| setSrcSpan
  setSrcSpan :: SrcSpan -> State state ()
  setSrcSpan = undefined

  -- \| getSrcSpan
  getSrcSpan :: State state SrcSpan
  getSrcSpan = undefined

  -- \| Block of declarations
  blockDecl :: Identifier -> [Declaration] -> Ap (State state) Doc
  blockDecl = undefined
  addIncludes :: [(String, Doc)] -> State state ()
  addIncludes = undefined
  addLibraries :: [LT.Text] -> State state ()
  addLibraries = undefined

  addImports :: [LT.Text] -> State state ()
  addImports = undefined

  addAndSetData :: FilePath -> State state String
  addAndSetData = undefined

  getDataFiles :: State state [(String, FilePath)]
  getDataFiles = undefined

  addMemoryDataFile :: (String, String) -> State state ()
  addMemoryDataFile = undefined

  getMemoryDataFiles :: State state [(String, String)]
  getMemoryDataFiles = undefined

  ifThenElseExpr :: state -> Bool
  ifThenElseExpr = undefined

  -- \| Whether -fclash-aggressive-x-optimization-blackboxes was set
  aggressiveXOptBB :: State state AggressiveXOptBB
  aggressiveXOptBB = undefined

  -- \| Whether -fclash-no-render-enums was set
  renderEnums :: State state RenderEnums
  renderEnums = undefined

  -- \| All the domain configurations of design
  domainConfigurations :: State state DomainMap
  domainConfigurations = undefined

  -- \| Set the domain configurations
  setDomainConfigurations :: DomainMap -> state -> state
  setDomainConfigurations = undefined
