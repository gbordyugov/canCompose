data Trie = Trie { terminal :: Bool
                 , children :: [(Char, Trie)]
                 } deriving (Show)

emptyTrie = Trie True []

makeTrie = foldl addString emptyTrie where
  addString (Trie _ cs) [] = Trie True cs
  addString (Trie t cs) (x:xs) = case lookup x cs of
    Nothing -> Trie False $ (x, addString emptyTrie xs):cs
    Just t' -> Trie False $ (x, addString t' xs):(filter ((/=x).fst) cs)

canCompose s dict = go s trie [s] where
  go [] _  _     = True
  go _  t  []    = False
  go _  _ ([]:r) = True
  go s@(z:zs) t@(Trie _ cs) stack  =
    case lookup z cs of
      Nothing                 -> go s  trie (tail stack)
      Just t'@(Trie True  _)  -> go zs t'   (zs:stack)
      Just t'@(Trie False _)  -> go zs t'   stack
  trie = makeTrie dict

main = putStrLn $
  show $ canCompose "bli blu blee" [" ", "blee", "blu", "bli"]
