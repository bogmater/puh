{-
Napisite funkciju koja ce umetnuti element na odredenu poziciju u listi, 
brojeci od nulte, tako da prebrise postojeci element na toj poziciji. 
Ako je pozicija negativna, neka se element krene traziti s kraja liste
prema pocetku.
-}
ubaci :: a -> Int -> [a] -> [a]
ubaci el i xs | length xs == 0  = []
              | i >= 0          = take (i `mod` length xs) xs 
                                  ++ el : drop (i `mod` length xs + 1) xs
              | otherwise       = reverse $
                                  take (abs i `mod` length xs - 1) (reverse xs)
                                  ++ el : drop (abs i `mod` length xs)
                                  (reverse xs)
                                  
