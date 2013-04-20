{-
Napisite funkciju koja ce rotirati listu za zadani broj mjesta.
Pozitivan broj neka rotira ulijevo za zadani broj mjesta, a
negativan udesno.
-}
rotl :: Int -> [a] -> [a]
rotl n xs | length xs == 0 = xs
          | n > 0          = drop (n `mod` length xs) xs 
                             ++ take (n `mod` length xs) xs
          | n < 0          = drop (length xs - abs n `mod` length xs) xs
                             ++ take (length xs - abs n `mod` length xs) xs
          | otherwise      = xs
