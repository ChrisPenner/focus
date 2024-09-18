{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Focus
  ( getViewFocus,
    getModifyFocus,
    textChunk,
    listChunk,
    composeFocus,
    liftTrav,
    liftIso,
    textI,
    asListI,
    underText,
    withinIso,
    unsafeIso,
    listOfFocus,
    (>.>),
  )
where

import Control.Lens
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.CPS
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Vector.Internal.Check (HasCallStack)
import Focus.Command (CommandF (..), CommandT (..))
import Focus.Prelude
import Focus.Types
import Focus.Untyped
import Prelude hiding (reads)

getViewFocus :: Focus 'ViewT i o -> (forall m. (Focusable m) => (o -> m ()) -> i -> m ())
getViewFocus (ViewFocus f) = f

getModifyFocus :: Focus 'ModifyT i o -> (forall m. (Focusable m) => LensLike' m i o)
getModifyFocus (ModifyFocus f) = f

textChunk :: (HasCallStack) => Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  actual -> error $ "Expected TextChunk, got " <> show actual

listChunk :: Chunk -> [Chunk]
listChunk = \case
  ListChunk chs -> chs
  actual -> error $ "Expected ListChunk, got " <> show actual

composeFocus :: Focus cmd i mid -> Focus cmd mid o -> Focus cmd i o
composeFocus (ViewFocus l) (ViewFocus r) = ViewFocus $ l . r
composeFocus (ModifyFocus l) (ModifyFocus r) = ModifyFocus $ l . r

(>.>) :: Focus cmd i mid -> Focus cmd mid o -> Focus cmd i o
(>.>) = composeFocus

liftTrav :: CommandF cmd -> Traversal' i o -> Focus cmd i o
liftTrav cmdF trav = case cmdF of
  ViewF -> ViewFocus $ \handler chunk -> getAp $ foldMapOf trav (Ap . handler) chunk
  ModifyF -> ModifyFocus $ trav

liftIso :: CommandF cmd -> Iso' i o -> Focus cmd i o
liftIso cmdF i = case cmdF of
  ViewF -> ViewFocus $ \handler chunk -> handler (chunk ^. i)
  ModifyF -> ModifyFocus $ i

textI :: Iso' Chunk Text
textI = unsafeIso _TextChunk

asListI :: Iso' Chunk [Chunk]
asListI = unsafeIso _ListChunk

underText :: Traversal' Text Text -> Traversal' Chunk Chunk
underText = withinIso textI

withinIso :: Iso' s a -> Traversal' a a -> Traversal' s s
withinIso i t = i . t . from i

unsafeIso :: (Show s) => Prism' s a -> Iso' s a
unsafeIso p = iso (\actual -> fromJust actual . preview p $ actual) (review p)
  where
    fromJust actual = \case
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
      f (results)
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