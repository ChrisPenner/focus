{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Focus.Types
  ( BindingName,
    Bindings,
    BindingDeclarations,
    Chunk (..),
    Focus (..),
    Focusable,
    FocusM (..),
    SelectorError (..),
    ShellMode (..),
    Typ,
    UVar,
    ChunkType (..),
    ChunkTypeT (..),
    TypeErrorReport,
    WarningReport,
    ReturnArity (..),
    FocusEnv (..),
    FocusOpts (..),
    focusOpts,
    focusBindings,
    handleErr,
    (>.>),
    castNumber,
  )
where

import Control.Category (Category (..))
import Control.Category qualified as Cat
import Control.Lens
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy
import Control.Unification (UTerm, Unifiable)
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (Unifiable (..))
import Data.Aeson qualified as Aeson
import Data.Align (Semialign (..))
import Data.Map (Map)
import Data.Scientific qualified as S
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These (..))
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd (..))
import Focus.Prelude
import Focus.Tagged (Tagged (..), tag_)
import Focus.Untyped
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO)
import Prelude hiding (reads)

data SelectorError
  = ShellError Pos Text
  | BindingError Pos Text
  | JsonParseError Pos Text {- < chunk -} Text {- < error -}
  | CastError Pos ChunkType {- destination type -} Chunk {- source chunk -}
  | MathError Pos Text

newtype FocusM a = FocusM {runFocusM :: ReaderT FocusEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader FocusEnv, MonadUnliftIO)

data FocusOpts = FocusOpts
  { _handleErr :: SelectorError -> IO ()
  }

data FocusEnv = FocusEnv
  { _focusBindings :: Bindings,
    _focusOpts :: FocusOpts
  }

makeLenses ''FocusEnv
makeLenses ''FocusOpts

type Focusable m = (MonadReader FocusEnv m, MonadFix m)

data Focus (cmd :: CommandT) i o where
  ViewFocus :: (forall m r. (Focusable m, Monoid r) => LensLike m i r o r) -> Focus 'ViewT i o
  ModifyFocus :: (forall m. (Focusable m) => LensLike' m i o) -> Focus 'ModifyT i o

instance (IsCmd cmd) => Category (Focus cmd) where
  id = case getCmd @cmd of
    ViewF -> ViewFocus $ \f s -> f s
    ModifyF -> ModifyFocus $ Cat.id
  (ModifyFocus l) . (ModifyFocus r) = ModifyFocus $ r Cat.. l
  (ViewFocus l) . (ViewFocus r) = ViewFocus $ r Cat.. l

(>.>) :: (IsCmd cmd) => Focus cmd a b -> Focus cmd b c -> Focus cmd a c
(>.>) = flip (Cat..)

type UVar s = Unify.STVar s (ChunkTypeT (NESet D.Position))

type Typ s = UTerm (ChunkTypeT (NESet D.Position)) (UVar s)

data ReturnArity = Affine | Exactly Int | Infinite | Any
  deriving stock (Show, Eq, Ord)

data ChunkTypeT a r
  = Arrow a r r
  | TextTypeT a
  | ListTypeT a r
  | NumberTypeT a
  | RegexMatchTypeT a
  | JsonTypeT a
  | CastableTypeT a r
  | RecordTypeT a (Map Text r)
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

instance (Semigroup a) => Unifiable (ChunkTypeT a) where
  zipMatch = \cases
    (RecordTypeT posL l) (RecordTypeT posR r) -> Just (RecordTypeT (posL <> posR) (alignWith go l r))
      where
        go = \case
          This x -> Left x
          That y -> Left y
          These x y -> Right (x, y)
    -- TODO: this isn't quite right
    (CastableTypeT posL x) (CastableTypeT posR _y) -> Just (CastableTypeT (posL <> posR) (Left x))
    (CastableTypeT posL _) other -> Just (Left <$> other & tag_ <>~ posL)
    other (CastableTypeT posR _) -> Just (Left <$> other & tag_ <>~ posR)
    (NumberTypeT posL) (NumberTypeT posR) -> Just $ NumberTypeT (posL <> posR)
    (NumberTypeT posL) (TextTypeT posR) -> Just $ NumberTypeT (posL <> posR)
    (TextTypeT posL) (NumberTypeT posR) -> Just $ TextTypeT (posL <> posR)
    (Arrow posL x y) (Arrow posR x' y') -> Just (Arrow (posL <> posR) (Right (x, x')) (Right (y, y')))
    (TextTypeT posL) (TextTypeT posR) -> Just (TextTypeT (posL <> posR))
    (ListTypeT posL x) (ListTypeT posR y) -> Just (ListTypeT (posL <> posR) $ Right (x, y))
    (RegexMatchTypeT posL) (RegexMatchTypeT posR) -> Just $ RegexMatchTypeT (posL <> posR)
    TextTypeT {} _ -> Nothing
    ListTypeT {} _ -> Nothing
    NumberTypeT {} _ -> Nothing
    RegexMatchTypeT {} _ -> Nothing
    Arrow {} _ -> Nothing
    JsonTypeT {} _ -> Nothing
    RecordTypeT {} _ -> Nothing

instance Tagged (ChunkTypeT a r) a where
  tag = \case
    Arrow pos _ _ -> pos
    TextTypeT pos -> pos
    ListTypeT pos _ -> pos
    NumberTypeT pos -> pos
    RegexMatchTypeT pos -> pos
    JsonTypeT pos -> pos
    CastableTypeT pos _ -> pos
    RecordTypeT pos _ -> pos
  setTag p = \case
    Arrow _ x y -> Arrow p x y
    TextTypeT _ -> TextTypeT p
    ListTypeT _ x -> ListTypeT p x
    NumberTypeT _ -> NumberTypeT p
    RegexMatchTypeT _ -> RegexMatchTypeT p
    JsonTypeT _ -> JsonTypeT p
    CastableTypeT _ x -> CastableTypeT p x
    RecordTypeT _ x -> RecordTypeT p x

type TypeErrorReport = D.Report Text

type WarningReport = D.Report Text

castNumber :: Chunk -> Maybe NumberT
castNumber inp = case inp of
  TextChunk txt -> castText txt
  JsonChunk (Aeson.String txt) -> castText txt
  NumberChunk num -> pure $ num
  JsonChunk (Aeson.Number scientific) -> case S.floatingOrInteger scientific of
    Left double -> pure $ DoubleNumber double
    Right int -> pure $ IntNumber int
  _ -> Nothing
  where
    castText :: Text -> Maybe NumberT
    castText txt =
      case readMaybe (Text.unpack txt) of
        Just i -> Just $ IntNumber i
        Nothing -> case readMaybe (Text.unpack txt) of
          Just d -> Just $ DoubleNumber d
          Nothing -> Nothing
