module Libraries.Data.String.Extra

import Data.String

||| Remove the first `n` characters from a string. Returns the empty string if
||| the input string is too short.
public export
drop : (n : Nat) -> (input : String) -> String
drop n str = substr n (length str) str

