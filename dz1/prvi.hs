import Data.List;
{-
1.1. 
- Napišite funkciju concat3 koja konkatenira tri stringa, ali ispušta srednji
  ako je kraći od 2 znaka (funkcija 'length').
- Proširite uvjet: kraća od 2 znaka ili dulja od 7 znakova.
-}

concat3 :: String -> String -> String -> String
concat3 s1 s2 s3 = if (length s2 < 2) 
                     then s1 ++ s3
                     else s1 ++ s2 ++ s3

concat3' :: String -> String -> String -> String
concat3' s1 s2 s3 = if (length s2 < 2 || length s2 > 7) 
                      then s1 ++ s3
                      else s1 ++ s2 ++ s3


{-
1.2.
- Napišite showSalary jednostavnije (sa samo jednim if-then-elsom)
- dodajte provjeru da plaća ne može biti negativna (ispišite poruku). Pazite
  gdje ćete dodati taj slučaj.
- Ispišite i total (plaća+bonus).
-}
showSalary :: Int -> Int -> String
showSalary am bo = if (bo /= 0)
                     then "Placa je " ++ show am ++ ", a bonus " ++ show bo 
                     else "Placa je " ++ show am

showSalary' :: Int -> Int -> String
showSalary' am bo | am < 0 = "Placa ne moze biti negativna"
                  | bo /= 0 = "Placa je " ++ show am ++ ", a bonus " ++
                    show bo ++ ". Total: " ++ show (am + bo)
                  | otherwise = "Placa je " ++ show am

{-
1.3
- Napišite funkciju koja broj ispisuje riječima (koristite čuvare)
- Napišite novu funkciju koja dvoznamenkasti broj ispisuje riječima (npr. 23 ->
  "dvadeset i tri").
-}

sayNumber :: Int -> String
sayNumber n | n == 0    = "nula"
            | n == 1    = "jedan"
            | n == 2    = "dva"
            | n == 3    = "tri"
            | n == 4    = "cetiri"
            | n == 5    = "pet"
            | n == 6    = "sest"
            | n == 7    = "sedam"
            | n == 8    = "osam"
            | n == 9    = "devet"
            | otherwise = "nezz"

sayNumber' :: Int -> String
sayNumber' n | n `mod` 10 == 0 = sayTens n
             | n `div` 10 == 0 = sayNumber n
             | n `div` 10 == 1 = sayTeens n
             | n `div` 10 == 2 = "dvadeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 3 = "trideset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 4 = "cetrdeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 5 = "pedeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 6 = "sezdeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 7 = "sedamdeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 8 = "osamdeset i " ++ (sayNumber $ mod n 10)
             | n `div` 10 == 9 = "devedeset i " ++ (sayNumber $ mod n 10)
             | otherwise       = "nezz"

sayTens :: Int -> String
sayTens n | n == 10 = "deset"
          | n == 20 = "dvadeset"
          | n == 30 = "trideset"
          | n == 40 = "cetrdeset"
          | n == 50 = "pedeset"
          | n == 60 = "sezdeset"
          | n == 70 = "sedamdeset"
          | n == 80 = "osamdeset"
          | n == 90 = "devedeset"
          | otherwise = "nezz"

sayTeens :: Int -> String
sayTeens n | n == 11 = "jedanaest"
           | n == 12 = "dvanaest"
           | n == 13 = "trinaest"
           | n == 14 = "cetrnaest"
           | n == 15 = "petnaest"
           | n == 16 = "sesnaest"
           | n == 17 = "sedamnaest"
           | n == 18 = "osamnaest"
           | n == 19 = "devetnaest"
           | otherwise = "nezz"

{-
2.1.
- Napravite funkciju koja vraća listu bez prva 3 i zadnja 3 znaka.
-}

unThree :: [a] -> [a]
unThree xs | length xs <= 6 = []
           | otherwise      = reverse $ drop 3 $ reverse $ drop 3 xs

{-
2.2.
- Napravite funkciju 'initals s1 s2' koja uzima ime i prezime osobe i vraća
  znakovni niz inicijala. 
  Npr. initials "Gabi Novak" => "G. N."
-}

initials :: String -> String -> String
initials s1 s2 = (head s1) : ". " ++ (head s2 : ".")

{-
2.3.
- Napravite funkciju koja konkatenira dva stringa, ali tako da dulji string
  uvijek bude prvi.
-}
twoConcat :: String -> String -> String
twoConcat s1 s2 | s1 >= s2  = s1 ++ s2
                | otherwise = s2 ++ s1

{-
2.4.
- Nadogradite 'padTo10' u 'padToN' tako da uzima argumente 'n' (duljina
  proširenog stringa) i 'x' (padding character).
- Nadogradite padToN tako da, ako je duljina stringa veća od 'n', da ga
  skraćuje na n znakova.
-}
padToN :: String -> Int -> Char -> String
padToN s n c = s ++ take (n - length s) (repeat c)

padToN' :: String -> Int -> Char -> String
padToN' s n c | length s > n = take n s
              | otherwise    = padToN s n c

{-
2.5.
- Napravite funkciju 'makeStr s1 s2' koja generira ovakav string: s1{3}s2's1'*
  gdje s2' je obrnut string od s2', s1' je s1 bez prvog znaka, {3} označava
  ponavljanje 3x, a '*' je ponavljanje beskonačno mnogo puta.
-}
makeStr :: String -> String -> String
makeStr s1 s2 = take (length s1 * 3) (cycle s1) ++ 
                                     reverse s2 ++
                                     cycle (reverse s1)

{-
2.6.
- Napravite funkciju 'safeHead l': funkcija vraća praznu listu ako je l
  prazna, a inače vraća [x], gdje je x prvi element.
-}

safeHead :: [a] -> [a]
safeHead xs | length xs == 0 = []
            | otherwise      = head xs : []


{-
2.7.
- Napravite funkciju 'mkMatrix m n' koja generira matricu mxn (matrica
  prikazana kao lista listi) i popunjava je nulama (koristite 'replicate').
-}

mkMatrix :: Int -> Int -> [[Int]]
mkMatrix m n = replicate m $ replicate n 0 

{-
2.8.
- Napravite funkciju koja provjerava ima li u listi ponavljajućih znakova
  (koristite 'nub').
-}
hasRep :: (Eq a) => [a] -> Bool
hasRep xs = not $ xs == nub xs

{-
2.9.
- Napravite funkciju 'pangram s' koja provjerava je li znakovni niz pangram
  (svako slovo abecede koristi se barem jednom). Pretpostavite da je niz pisan
  malim slovima. Hint: koristite 'nub' i 'sort'.
-}
pangram  :: String -> Bool
pangram s = (sort . nub) s == ['a'..'z']
