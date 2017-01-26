
module DepTrack.Parsing (
   dependencies
 ) where

import           Control.Applicative ((<|>))

import           Data.Tree           (Forest, Tree (..))
import           Text.Parsec         (ParsecT, (<?>))
import qualified Text.Parsec         as Parsec
import           Text.Parsec.Prim    (tokenPrim)

import           DepTrack.DepCrumb

-- | Parser to transform a well-formed list of DepCrumb into a Forest of
-- dependencies.
--
-- The (Show a) requirements exists to be able to display error while parsing.
dependencies :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Forest a)
dependencies = forest <* Parsec.eof

forest :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Forest a)
forest = fmap concat $ Parsec.many (simpleForest <|> spade <?> "forest")

simpleForest :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Forest a)
simpleForest = Parsec.many1 tree <?> "simple-forest"

spade :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Forest a)
spade = merge <$> (spadeIn *> forest <* spadeMiddle) <*> (forest <* spadeOut)
  where merge :: Forest a -> Forest a -> Forest a
        merge [] children      = children
        merge parents children = fmap (appendChildren children) parents

        appendChildren :: Forest a -> Tree a -> Tree a
        appendChildren zs (Node x ys) = Node x (ys ++ zs)

tree :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Tree a)
tree = Parsec.try leaf
   <|> Parsec.try parent
   <?> "tree"

parent :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Tree a)
parent = f <$> (push *> forest) <*> pop <?> "parent"
  where f xs popx = Node (unsafeFromPop popx) xs

leaf :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (Tree a)
leaf = pure . unsafeFromPop <$> (push *> pop) <?> "leaf"

push :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (DepCrumb a)
push = satisfy isPush <?> "Push"

pop :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (DepCrumb a)
pop = satisfy isPop <?> "Pop"

spadeIn :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (DepCrumb a)
spadeIn = satisfy isSpadeIn <?> "SpadeIn"

spadeMiddle :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (DepCrumb a)
spadeMiddle = satisfy isSpadeMiddle <?> "SpadeMiddle"

spadeOut :: (Monad m, Show a) => ParsecT [DepCrumb a] () m (DepCrumb a)
spadeOut = satisfy isSpadeOut <?> "SpadeOut"

isPop,isPush,isSpadeIn,isSpadeMiddle,isSpadeOut :: DepCrumb a -> Bool
isPop (Pop _) = True
isPop _       = False

isPush (Push) = True
isPush _      = False

isSpadeIn SpadeIn = True
isSpadeIn _ = False

isSpadeMiddle SpadeMiddle = True
isSpadeMiddle _ = False

isSpadeOut SpadeOut = True
isSpadeOut _ = False

unsafeFromPop :: DepCrumb a -> a
unsafeFromPop (Pop x) = x
unsafeFromPop _ = error "partial function, not a Pop"

satisfy :: (Monad m, Show a) => (DepCrumb a -> Bool) -> ParsecT [DepCrumb a] () m (DepCrumb a)
satisfy f = tokenPrim showDepCrumb nextPos testDepCrumb
  where showDepCrumb = show
        testDepCrumb x = if f x then Just x else Nothing
        nextPos pos _ _ = Parsec.incSourceColumn pos 1
