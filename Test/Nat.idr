data Nat : Type where
     Z : Nat
     S : Nat -> Nat

plus : Nat -> Nat -> Nat
pat y : Nat =>
    plus Z y = y
