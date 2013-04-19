{-
Napisite funkciju
  applyAt f i xs
koja aplicira funkciju f na i-ti element liste xs
i vraca tako izmijenjenu listu. Ako je indeks i
izvan liste, funkcija treba vratiti nepromijenjenu
listu.
-}
applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f i xs | i >= length xs = xs
               | i < 0          = xs
               | otherwise      = bef ++ (ind:aft)
                                  where
                                    bef = take i xs
                                    ind = f $ head $ drop i xs
                                    aft = drop (i + 1) xs
