module Tmp where

-- instance Show K where
--   show k = "[" <> List.intercalate ", " (go k) <> "]"
--     where
--       go = \case
--         Halt -> []
--         LPlus rhs k -> "+ " <> show rhs : go k
--         RPlus lhs' k -> show lhs' <> " +" : go k
--         LApp arg k -> "@ " <> show arg : go k
--         RApp f' k -> show f' <> " @" : go k
--         KDo name k -> "do " <> name : go k

-- instance Out K where
--   doc = PP.text . show
--   docPrec = const doc

