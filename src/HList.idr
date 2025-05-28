module HList

SpawnResult : (cancelable: Bool) -> Type -> Type
SpawnResult False t = t
SpawnResult True t = Maybe t

spawn : Lazy a -> (cancelable: Bool) -> SpawnResult cancelable a
spawn x False = x
spawn x True = Just x

public export
data HList : List Type -> Type where
  Nil : HList []
  (::) : (a : x) -> HList xs -> HList (x :: xs)

prepend : x -> HList xs -> HList (x :: xs)
prepend y ys = y :: ys

-- prepend 1.2 [1, "John", False]

append : x -> HList xs -> HList (xs ++ [x])
append y [] = [y]
append y (a :: z) = a :: append y z

-- append 1.2 [1, "John", False]

head : HList (x :: xs) -> x
head (y :: ys) = y
head [] impossible

-- head [1, "John", False]
-- head []
