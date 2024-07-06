type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src dest pivot = []
hanoi n src dest pivot = hanoi (n - 1) src pivot dest ++ [(src, dest)] ++ hanoi (n - 1) pivot dest src

main =
  print (hanoi 3 "a" "b" "c")