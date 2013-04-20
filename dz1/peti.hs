{-
Napisite funkciju koja ce umetnuti element na odredenu poziciju unutar 
liste listi. Iskoristite funkciju koju ste definirali u zadatku 4.
-}
ubaci :: a -> Int -> [a] -> [a]
ubaci el i xs | length xs == 0  = []
              | i >= 0          = take (i `mod` length xs) xs 
                                  ++ el : drop (i `mod` length xs + 1) xs
              | otherwise       = reverse $
                                  take (abs i `mod` length xs - 1) (reverse xs)
                                  ++ el : drop (abs i `mod` length xs)
                                  (reverse xs)

ubaci2D :: a -> Int -> Int -> [[a]] -> [[a]]
ubaci2D el ii oi xss | length xss == 0 = []
                     | oi < 0          = xss
                     | otherwise       = take (oi `mod` length xss) xss 
                                         ++ ubaci el ii 
                                         (head $ drop (oi `mod` length xss) xss)
                                         : drop (oi `mod` length xss + 1) xss
