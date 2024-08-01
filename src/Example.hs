module Example where
import Gandalf

data Data a = Ok a | NotOk deriving (Show)

Gandalf.castInstanceSpell ''Data ''Functor
Gandalf.castInstanceSpell ''Data ''Applicative
Gandalf.castInstanceSpell ''Data ''Monad

Gandalf.castInstanceSpell ''Data ''Foldable
Gandalf.castInstanceSpell ''Data ''Traversable
