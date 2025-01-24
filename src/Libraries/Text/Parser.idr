module Libraries.Text.Parser

import public Libraries.Text.Parser.Core
import public Libraries.Text.Quantity
import public Libraries.Text.Token
import public Libraries.Data.Bool.Extra

||| Optionally parse a thing, with a default value if the grammar doesn't
||| match. May match the empty input.
export
option : {c : Bool} ->
         (def : a) -> (p : Grammar tok c a) ->
         Grammar tok False a
option {c = False} def p = p <|> pure def
option {c = True} def p = p <|> pure def

mutual
  ||| Parse one or more things
  export
  some : Grammar tok True a ->
         Grammar tok True (List a)
  some p = pure (!p :: !(many p))

  ||| Parse zero or more things (may match the empty input)
  export
  many : Grammar tok True a ->
         Grammar tok False (List a)
  many p = option [] (some p)

export
sepBy1 : {c : Bool} ->
         (sep : Grammar tok True s) ->
         (p : Grammar tok c a) ->
         Grammar tok c (List a)
sepBy1 {c} sep p = rewrite sym (orFalseNeutral c) in
                           [| p :: many (sep *> p) |]
          
