module Proof

import Data.Vect
import Decidable.Equality

import Database

data MyEqual : a -> b -> Type where
  MyRefl : MyEqual x x

proof3eq3 : MyEqual 3 3
proof3eq3 = MyRefl

-- proof3eq4 : MyEqual 3 4
-- proof3eq4 = MyRefl

proofIdentityDoesNothing : MyEqual x (id x)
proofIdentityDoesNothing = MyRefl

proofProgramCorrect : MyEqual 3 (5 + 21 - 23)
proofProgramCorrect = MyRefl

merge : (t1 : Table) -> (t2 : Table) -> t1.schema = t2.schema -> Table
merge (MkTable schema rows) (MkTable schema rows') Refl = MkTable schema (rows ++ rows')

plusZero : (n : Nat) -> n + 0 = n
plusZero Z = Refl
plusZero (S k) = rewrite plusZero k in Refl

plusSSecond : (n : Nat) -> (m : Nat) -> n + (S m) = S (n + m)
plusSSecond Z m = Refl
plusSSecond (S k) m = rewrite plusSSecond k m in Refl

plusIsCommutative : (n : Nat) -> (m : Nat) -> n + m = m + n
plusIsCommutative n Z = plusZero n
plusIsCommutative n (S k) = rewrite plusIsCommutative k n in plusSSecond n k

swapSameSizes : n = m -> Vect n a -> Vect m a
swapSameSizes Refl xs = xs

getAnyValueFromContradiction : (the Nat 0) = (the Nat 1) -> a
getAnyValueFromContradiction prf =
  let empty : Vect 0 a = []
      singleValue : Vect 1 a = swapSameSizes prf empty
  in head singleValue

data MyVoid : Type where

createVoid : MyVoid
createVoid = ?createVoid_rhs

createVoidFromContradiction : (the Nat 0) = (the Nat 1) -> MyVoid
createVoidFromContradiction prf = getAnyValueFromContradiction prf

doNotPass2 : (n : Nat) -> Not (n = 2) -> Nat
doNotPass2 2 contra = void (contra Refl)
doNotPass2 n contra = n

testDoNotPass2 : Nat
testDoNotPass2 = doNotPass2 3 (\Refl impossible)

DecEq Column where
  decEq CInt CInt = Yes Refl
  decEq CString CString = Yes Refl
  decEq CBoolean CBoolean = Yes Refl

  decEq CInt CString = No (\Refl impossible)
  decEq CInt CBoolean = No (\Refl impossible)
  decEq CString CInt = No (\Refl impossible)
  decEq CString CBoolean = No (\Refl impossible)
  decEq CBoolean CInt = No (\Refl impossible)
  decEq CBoolean CString = No (\Refl impossible)

merge' : Table -> Table -> Maybe Table
merge' t1 t2 =
  case decEq t1.schema t2.schema of
    Yes prf => Just (merge t1 t2 prf)
    No _ => Nothing
