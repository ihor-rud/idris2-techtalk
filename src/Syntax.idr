module Syntax

data FullName : Type where
  MkFullName : (firstName : String) -> (lastName : String) -> FullName

initials : FullName -> (String, String)
initials (MkFullName firstName lastName) = (substr 0 1 firstName, substr 0 1 lastName)

-- initials (MkFullName "John" "Snow")

data MyNat : Type where
  Z : MyNat
  S : MyNat -> MyNat

myPlus : Nat -> Nat -> Nat
myPlus x Z = x
myPlus x (S y) = S (myPlus x y)

-- myPlus 10 10

data MyList : Type -> Type where
  Nil : MyList a
  (::) : a -> MyList a -> MyList a

append : a -> MyList a -> MyList a
append x xs = x :: xs

-- append 10 [1, 2, 3]

head : MyList a -> Maybe a
head [] = Nothing
head (x :: y) = Just x

-- head []
-- head [1, 2, 3]
