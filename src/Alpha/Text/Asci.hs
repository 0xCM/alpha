-----------------------------------------------------------------------------
-- | Operations and types related to the ASCI character set
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Text.Asci where
import Alpha.Base

pattern D0 = "0"
pattern D1 = "1"
pattern D2 = "2"
pattern D3 = "3"
pattern D4 = "4"
pattern D5 = "5"
pattern D6 = "6"
pattern D7 = "7"
pattern D8 = "8"
pattern D9 = "9"

digits::[Text]
digits = [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]

pattern Au = "A"
pattern Bu = "B"
pattern Cu = "C"
pattern Du = "D"
pattern Eu = "E"
pattern Fu = "F"
pattern Gu = "G"
pattern Hu = "H"
pattern Iu = "I"
pattern Ju = "J"
pattern Ku = "K"
pattern Lu = "L"
pattern Mu = "M"
pattern Nu = "N"
pattern Ou = "O"
pattern Pu = "P"
pattern Qu = "Q"
pattern Ru = "R"
pattern Su = "S"
pattern Tu = "T"
pattern Uu = "U"
pattern Vu = "V"
pattern Wu = "W"
pattern Xu = "X"
pattern Yu = "Y"
pattern Zu = "Z"

upper::[Text]
upper = [Au, Bu, Cu, Du, Eu, Fu, Gu, Hu, Iu, 
        Ju, Ku, Lu, Mu, Nu, Ou, Pu, Qu, Ru, 
        Su, Tu, Uu, Vu, Wu, Xu, Yu, Zu]


pattern Al = "a"
pattern Bl = "b"
pattern Cl = "c"
pattern Dl = "d"
pattern El = "e"
pattern Fl = "f"
pattern Gl = "g"
pattern Hl = "h"
pattern Il = "i"
pattern Jl = "j"
pattern Kl = "k"
pattern Ll = "l"
pattern Ml = "m"
pattern Nl = "n"
pattern Ol = "o"
pattern Pl = "p"
pattern Ql = "q"
pattern Rl = "r"
pattern Sl = "s"
pattern Tl = "t"
pattern Ul = "u"
pattern Vl = "v"
pattern Wl = "w"
pattern Xl = "x"
pattern Yl = "y"
pattern Zl = "z"

lower::[Text]
lower = [Al, Bl, Cl, Dl, El, Fl, Gl, Hl, Il, 
        Jl, Kl, Ll, Ml, Nl, Ol, Pl, Ql, Rl, 
        Sl, Tl, Ul, Vl, Wl, Xl, Yl, Zl]


pattern LBrace = "{"
pattern RBrace = "}"

braces::(Text,Text)
braces = (LBrace, RBrace)

pattern LBrack = "["
pattern RBrack = "]"

brackets::(Text,Text)
brackets = (LBrack,RBrack)

pattern BSlash = "\\"
pattern FSlash = "/"

slashes::(Text,Text)
slashes = (BSlash,FSlash)

pattern LParen = "("
pattern RParen = ")"

parens::(Text,Text)
parens = (LParen,RParen)

pattern Space = " "
pattern Tick = "`"
pattern Tilde = "~"
pattern Bang = "!"
pattern At = "@"
pattern Dollar = "$"
pattern Pct = "%"
pattern Caret = "^"
pattern Amp = "&"
pattern Ast = "*"
pattern Dash = "-"
pattern Underscore = "_"
pattern Plus = "+"
pattern Und = "_"
pattern Eq = "="
pattern Dot = "."
pattern Comma = ","
pattern Semi = ";"
pattern Colon = ":"
pattern EOL = "\n"
pattern Less = "<"
pattern Greater = ">"
pattern Equal = "="


