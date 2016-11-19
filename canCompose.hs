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
  go []       _             _      = True
  go s@(z:zs) t@(Trie _ cs) stack  =
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
