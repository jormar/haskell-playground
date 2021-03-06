
-- Exercise 7.4 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

class Medible t where
  size :: t -> Integer

instance Medible Integer where
  size _ = 1

instance Medible Char where
  size _ = 1

instance Medible Bool where
  size _ = 1

instance Medible t => Medible [t] where
  size [] = 0
  size l = foldr ((+) . size) 0 l

-- Exercise 7.5 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

ocurrencias :: Eq a => [a] -> a -> Integer
ocurrencias [] _ = 0
ocurrencias (x:xs) a = if a == x
  then 1 + ocurrencias xs a
  else ocurrencias xs a

pertenece :: Eq a => [a] -> a -> Bool
pertenece [] _ = False
pertenece (x:xs) a = x == a || pertenece xs a

pertenece2 :: Eq a => [a] -> a -> Bool
pertenece2 l a = ocurrencias l a >= 1

-- Exercise 7.6 - http://www.lcc.uma.es/~pepeg/declarativa/ejercicios.pdf

data Color = Violeta | Azul | Verde | Amarillo | Naranja | Rojo
  deriving Show

instance Eq Color where
  Violeta == Violeta = True
  Azul == Azul = True
  Verde == Verde = True
  Amarillo == Amarillo = True
  Naranja == Naranja = True
  Rojo == Rojo = True
  _ == _ = False

instance Ord Color where
  c1 <= c2 = theColorOrder c1 <= theColorOrder c2
    where
      theColorOrder Violeta = 1
      theColorOrder Azul = 2
      theColorOrder Verde = 3
      theColorOrder Amarillo = 4
      theColorOrder Naranja = 5
      theColorOrder Rojo = 6
