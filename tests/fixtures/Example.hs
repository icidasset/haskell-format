module Example where

import Stuff


-- 🍯


main :: IO ()
main = return (hello "Dude")



-- Functions


{-| Hello!

Explanation goes here.

-}
hello :: String -> String
hello name =
    "Hello " ++ name ++ "!"
