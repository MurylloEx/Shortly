module TokenService where

import System.Random ( randomIO )

shTokenChars :: [Char] -> [(Char, Int)]
shTokenChars seed = zip (concat [['a'..'z'], ['A'..'Z'], seed]) [0..]

shRandomToken :: [Char] -> Int -> IO String
shRandomToken token 0 = return token 
shRandomToken (_:seed) len = do
    k <- randomIO :: IO Int
    let p = k `mod` length (shTokenChars seed)
    shRandomToken (seed ++ [c | (c, i) <- shTokenChars seed, i == p]) (len-1)
