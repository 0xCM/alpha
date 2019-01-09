-----------------------------------------------------------------------------
-- | Operations and types related to the ASCI character set
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Asci where
import Alpha.Base

pattern D0 = "0"::Text
pattern D1 = "1"::Text
pattern D2 = "2"::Text
pattern D3 = "3"::Text
pattern D4 = "4"::Text
pattern D5 = "5"::Text
pattern D6 = "6"::Text
pattern D7 = "7"::Text
pattern D8 = "8"::Text
pattern D9 = "9"::Text

pattern Au = "A"::Text
pattern Bu = "B"::Text
pattern Cu = "C"::Text
pattern Du = "D"::Text
pattern Eu = "E"::Text
pattern Fu = "F"::Text
pattern Gu = "G"::Text
pattern Hu = "H"::Text
pattern Iu = "I"::Text
pattern Ju = "J"::Text
pattern Ku = "K"::Text
pattern Lu = "L"::Text
pattern Mu = "M"::Text
pattern Nu = "N"::Text
pattern Ou = "O"::Text
pattern Pu = "P"::Text
pattern Qu = "Q"::Text
pattern Ru = "R"::Text
pattern Su = "S"::Text
pattern Tu = "T"::Text
pattern Uu = "U"::Text
pattern Vu = "V"::Text
pattern Wu = "W"::Text
pattern Xu = "X"::Text
pattern Yu = "Y"::Text
pattern Zu = "Z"::Text

pattern Al = "a"::Text
pattern Bl = "b"::Text
pattern Cl = "c"::Text
pattern Dl = "d"::Text
pattern El = "e"::Text
pattern Fl = "f"::Text
pattern Gl = "g"::Text
pattern Hl = "h"::Text
pattern Il = "i"::Text
pattern Jl = "j"::Text
pattern Kl = "k"::Text
pattern Ll = "l"::Text
pattern Ml = "m"::Text
pattern Nl = "n"::Text
pattern Ol = "o"::Text
pattern Pl = "p"::Text
pattern Ql = "q"::Text
pattern Rl = "r"::Text
pattern Sl = "s"::Text
pattern Tl = "t"::Text
pattern Ul = "u"::Text
pattern Vl = "v"::Text
pattern Wl = "w"::Text
pattern Xl = "x"::Text
pattern Yl = "y"::Text
pattern Zl = "z"::Text

pattern Pipe = "|"::Text        
pattern LBrace = "{"::Text
pattern RBrace = "}"::Text
pattern LBrack = "["::Text
pattern RBrack = "]"::Text
pattern BSlash = "\\"::Text
pattern FSlash = "/"::Text
pattern LParen = "("::Text
pattern RParen = ")"::Text
pattern Space = " "::Text
pattern Tick = "`"::Text
pattern Tilde = "~"::Text
pattern Bang = "!"::Text
pattern At = "@"::Text
pattern Dollar = "$"::Text
pattern Pct = "%"::Text
pattern Caret = "^"::Text
pattern Amp = "&"::Text
pattern Ast = "*"::Text
pattern Dash = "-"::Text
pattern Underscore = "_"::Text
pattern Plus = "+"::Text
pattern Und = "_"::Text
pattern Eq = "="::Text
pattern Dot = "."::Text
pattern Dots = "..."::Text
pattern Comma = ","::Text
pattern Semi = ";"::Text
pattern Colon = ":"::Text
pattern EOL = "\n"::Text
pattern Less = "<"::Text
pattern Greater = ">"::Text
pattern Equal = "="::Text
pattern Empty = ""::Text
pattern Period = "."::Text
-- | Produces "<-"
pattern LArrow = "<--"::Text
-- | Produces "->"
pattern RArrow = "-->"::Text

pattern RMap = ":-->"::Text
pattern LMap = "<--:"::Text
asciL::[Text]
asciL = [Al, Bl, Cl, Dl, El, Fl, Gl, Hl, Il, 
        Jl, Kl, Ll, Ml, Nl, Ol, Pl, Ql, Rl, 
        Sl, Tl, Ul, Vl, Wl, Xl, Yl, Zl]

asciD::[Text]
asciD = [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]

asciU::[Text]
asciU
    = [Au, Bu, Cu, Du, Eu, Fu, Gu, Hu, Iu, 
       Ju, Ku, Lu, Mu, Nu, Ou, Pu, Qu, Ru, 
       Su, Tu, Uu, Vu, Wu, Xu, Yu, Zu]

slashes::(Text,Text)
slashes = (BSlash,FSlash)

parens::(Text,Text)
parens = (LParen,RParen)

brackets::(Text,Text)
brackets = (LBrack,RBrack)

braces::(Text,Text)
braces = (LBrace, RBrace)


