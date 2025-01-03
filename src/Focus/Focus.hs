{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Focus
  ( getViewFocus,
    getModifyFocus,
    textChunk,
    jsonChunk,
    listChunk,
    liftTrav,
    liftSimple,
    liftSimpleWithBindings,
    liftIso,
    textI,
    asListI,
    underText,
    withinIso,
    unsafeIso,
    listOfFocus,
    (>.>),
    focusId,
    focusEmpty,
    focusTo,
  )
where

import Control.Category qualified as Cat
import Control.Lens
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.CPS
import Data.Aeson (Value)
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Vector.Internal.Check (HasCallStack)
import Focus.Command (CommandF (..), CommandT (..), IsCmd (..))
import Focus.Prelude
import Focus.Types
import Focus.Untyped
import Prelude hiding (reads)

focusId :: (IsCmd cmd) => Focus cmd i i
focusId = Cat.id

focusEmpty :: forall cmd i o. (IsCmd cmd) => Focus cmd i o
focusEmpty = case getCmd @cmd of
  ViewF -> ViewFocus $ \_ _ -> pure mempty
  ModifyF -> ModifyFocus $ \_ inp -> pure inp

getViewFocus :: Focus 'ViewT i o -> (forall m r. (Focusable m, Monoid r) => (o -> m r) -> i -> m r)
getViewFocus (ViewFocus f) = f

getModifyFocus :: Focus 'ModifyT i o -> (forall m. (Focusable m) => LensLike' m i o)
getModifyFocus (ModifyFocus f) = f

textChunk :: (HasCallStack) => Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  NumberChunk n -> renderNumber n
  actual -> error $ "Expected TextChunk, got " <> show actual

jsonChunk :: Chunk -> Value
jsonChunk = \case
  JsonChunk v -> v
  actual -> error $ "Expected JsonChunk, got " <> show actual

listChunk :: Chunk -> [Chunk]
listChunk = \case
  ListChunk chs -> chs
  actual -> error $ "Expected ListChunk, got " <> show actual

liftTrav :: forall cmd i o. (IsCmd cmd) => Traversal' i o -> Focus cmd i o
liftTrav trav = case getCmd @cmd of
  ViewF -> ViewFocus $ \handler chunk -> getAp $ foldMapOf trav (Ap . handler) chunk
  ModifyF -> ModifyFocus $ trav

liftSimple :: forall cmd i o. (IsCmd cmd) => (forall m. (Focusable m) => i -> m o) -> (forall m. (Focusable m) => o -> m i) -> Focus cmd i o
liftSimple forward backward = do
  case getCmd @cmd of
    ViewF -> ViewFocus \f s -> do
      x <- forward s
      f x
    ModifyF -> ModifyFocus \f s -> do
      x <- forward s
      f x >>= backward

liftSimpleWithBindings :: forall cmd i o. (IsCmd cmd) => (forall r m. (Focusable m) => (o -> m r) -> i -> m r) -> (forall m. (Focusable m) => o -> m i) -> Focus cmd i o
liftSimpleWithBindings forward backward = do
  case getCmd @cmd of
    ViewF -> ViewFocus \f s -> do
      forward f s
    ModifyF -> ModifyFocus \f s -> do
      forward f s >>= backward

liftIso :: forall cmd i o. (IsCmd cmd) => Iso' i o -> Focus cmd i o
liftIso i = case getCmd @cmd of
  ViewF -> ViewFocus $ \handler chunk -> handler (chunk ^. i)
  ModifyF -> ModifyFocus $ i

textI :: Iso' Chunk Text
textI = unsafeIso _TextChunk

asListI :: Iso' Chunk [Chunk]
asListI = unsafeIso _ListChunk

underText :: (IsCmd cmd) => Focus cmd Text Text -> Focus cmd Chunk Chunk
underText = withinIso textI

withinIso :: (IsCmd cmd) => Iso' s a -> Focus cmd a a -> Focus cmd s s
withinIso i t = liftIso i >.> t >.> liftIso (from i)

unsafeIso :: (HasCallStack, Show s) => Prism' s a -> Iso' s a
unsafeIso p = iso (\actual -> fromJust' actual . preview p $ actual) (review p)
  where
    fromJust' actual = \case
      Just x -> x
      Nothing -> error $ "unsafeIso: Mismatch. Actual: " <> show actual

listOfFocus :: forall cmd i o. Focus cmd i o -> Focus cmd i [o]
listOfFocus = \case
  ViewFocus inner ->
    ViewFocus $ \f chunk -> do
      results <-
        chunk
          & inner
            %%~ ( \chunk' -> do
                    tell [chunk']
                )
          & execWriterT
      f results
  ModifyFocus inner ->
    ModifyFocus $ \f chunk -> do
      let action :: (Focusable m) => Lazy.StateT (ListOfS o) m i
          action =
            chunk
              & inner %%~ \innerChunk -> do
                Lazy.state \(ListOfS {reads = ~reads, results = ~results}) ->
                  let ~(result, newResults) = case results of
                        ~(hd : rest) -> (hd, rest)
                   in (result, ListOfS {reads = (innerChunk : reads), results = newResults})
      ~(r, _) <- mfix $ \(~(_r, results)) -> do
        ~(r, ListOfS {reads}) <- Lazy.runStateT action (ListOfS {reads = [], results})
        f (reverse reads) >>= \case
          ~(chs) -> pure $ (r, chs)
      pure r

data ListOfS a = ListOfS
  { reads :: [a],
    results :: [a]
  }

focusTo :: (forall m. (Focusable m) => i -> m o) -> Focus ViewT i o
focusTo f = ViewFocus $ \handler i -> f i >>= handler
