data Trie = Trie { terminal :: Bool
                 , children :: [(Char, Trie)]
                 } deriving (Show)

emptyTrie = Trie True []

makeTrie s = Trie True (children (foldl addString emptyTrie s)) where
  addString (Trie _ cs) [] = Trie True cs
  addString (Trie t cs) (x:xs) = case lookup x cs of
    Nothing -> Trie False $ (x, addString emptyTrie xs):cs
    Just t' -> Trie False $ (x, addString t' xs):(filter ((/=x).fst) cs)

canCompose s dict = go s trie [s] where
  go []     (Trie True  _)  _ = True
  go []     (Trie False _) [] = False
  go []     (Trie False _)  s = go (head s) trie (tail s)
  go (z:zs) (Trie _     cs)  stack =
    case lookup z cs of
      Nothing                 -> if null stack
                                 then False
                                 else go (head stack) trie (tail stack)
      Just t'@(Trie True  _)  -> go zs t'   (zs:stack)
      Just t'@(Trie False _)  -> go zs t'   stack
  trie = makeTrie dict

test1 = canCompose "quick brown fox"
                    ["quick", "qui", "ck ", "brow", "n fox"]
test2 = canCompose "quick brown fox" ["quick", "brown 1", "brow", "qui"
                                     , "ck ", "n fox1", "n fox"]
test3 = not $ canCompose "aba" ["aba1"]
test4 = canCompose "abc" ["abcd", "ab", "c"]

tests = and [test1, test2, test3, test4]
