module Example (x, y, z) where

import Stuff


-- 🍯


main :: IO ()
main = return (hello "Dude")



-- Functions
-- (ie. do things with data)


{-| Hello!

Explanation goes here.

-}
hello :: String -> String
hello name =
    -- Test
    "Hello " ++ name ++ "!"

foo :: Bar
foo = "bar"
