data Nat : Type where
     Z : Nat
     S : Nat -> Nat

plus : Nat -> Nat -> Nat
pat y : Nat =>
    plus Z y = y
pat x : Nat, y : Nat =>
    plus (S x) y = S (plus x y)
