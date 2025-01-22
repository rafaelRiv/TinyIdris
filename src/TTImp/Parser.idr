module TTImp.Parser

import Parser.Source

export
prog : Rule (List Integer)
prog = nonEmptyBlock (\x => intLit)

