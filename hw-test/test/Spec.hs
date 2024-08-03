import Utils

main :: IO ()
main = putStrLn "COMPILED!"

data Data a = Ok a | NotOk deriving (Show)

todoImpl ''Data ''Functor
todoImpl ''Data ''Applicative
todoImpl ''Data ''Monad

todoImpl ''Data ''Foldable
todoImpl ''Data ''Traversable
