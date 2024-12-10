module Librairies.Text.Lexer.Core

%default total

||| A language of token recognisers
||| @ consumes If `True`, this recogniser is guaranteed to consume at
|||            least one character of input when it succeeds
export
data Recognise : (consumes : Bool) -> Type where
  Empty : Recognise False
  Fail : Recognise c
  EOF : Recognise False
  Lookahead : (positive : Bool) -> Recognise c -> Recognise False
  Pred : (Char -> Bool) -> Recognise True

||| A token recogniser. Guaranteed to consume at least one character
public export
Lexer : Type
Lexer = Recognise True

||| A recogniser that always fails
export
fail : Recognise c
fail = Fail

||| Recognise no input (doesn't consume any input)
export
empty : Recognise False
empty = Empty

||| Recognise en of input (doesn't consume any input)
export
eof : Recognise False
eof = EOF

||| Recognise a character that matches a predicate
export
pred : (Char -> Bool) -> Lexer
pred = Pred

||| Positive lookahead. Never consumes input
export
expect : Recognise c -> Recognise False
expect = Lookahead True

||| Negative lookahead. Never consumes input
export
reject : Recognise c -> Recognise False
reject  = Lookahead False
