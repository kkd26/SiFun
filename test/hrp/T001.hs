import Data.Typeable

x = 1 :: Int
-- :t x

main = print (typeOf x)