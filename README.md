# yatima

## Instructions

0. Install The Haskell Tool Stack. Instructions here:
   https://docs.haskellstack.org/en/stable/README/
1. Clone this repository, `cd` into it and `stack build`
2. Enter a repl with `stack ghci`, and try:
  ```
  > prettyFile "test/test.ya"
  λ x y z f g => f (g x y) z

  > prettyFile "test/red.ya"
  (λ x => x) λ x => x

  > evalPrintFile "test/test.ya"
  λ x => x
  ```
3. Examine Haddock documentation with `stack haddock --open yatima`


4. Try the repl:

```
$ stack exec yide
Welcome to yide, the Yatima interactive development environment!
yide> :help
help text fills you with determination 
yide> (λ x => x) (λ x => x)
λ x => x
yide> def id = λ x => x
yide> def snd = λ x y => y
yide> def fst = λ x y => x
yide> id fst
λ x y => x
yide> id fst snd snd 
λ x y => y
yide> :browse
Defs {_index = fromList [("fst",zDPWYqFCqtnpLtNboHxPUoPxo1FjTxXDu9M22pX1uTqPccQEhvgU),("id",zDPWYqFCsmqD6T7QiTFfTHJFEHtMQPBGW8ymcVBQmCfvGz1vwmZU),("snd",zDPWYqFD1Z38aKEQKQxZvFRMDc1wA7VVzqPpW2sFdh1mP78iWNGe)], _cache = fromList [(zDPWYqFCqtnpLtNboHxPUoPxo1FjTxXDu9M22pX1uTqPccQEhvgU,"\164b$0gMetaDefb$1\216*X'\NUL\SOHq\160\228\STX !&\184=\204H\180\FS\202},\232 K-$\130\139j<\160`\GS\191\&1\153\136\147C\234\DC1\243b$2\163b$0dMetab$1\163\NULcfst\SOHax\STXayb$2\160b$3\163b$0dMetab$1\163\NULcfst\SOHax\STXayb$2\160"),(zDPWYqFCsmqD6T7QiTFfTHJFEHtMQPBGW8ymcVBQmCfvGz1vwmZU,"\164b$0gMetaDefb$1\216*X'\NUL\SOHq\160\228\STX ~\185\255\239\FS\234\137\231\164\129\166%o\162\n\222u\137\154Td\229\154L\226t\245\136\179\156\161*b$2\163b$0dMetab$1\162\NULbid\SOHaxb$2\160b$3\163b$0dMetab$1\162\NULbid\SOHaxb$2\160"),(zDPWYqFCswHyhy3VeB89KseiETmEsgvxSYUmfqp4pjPrzuFMGiBL,"\163b$0gAnonDefb$1\216*X'\NUL\SOHq\160\228\STX #d\170\146yS\132\184\253\164\250\164uQup\129\230\&2\158]\ETBl\160u\182\225\241\161o\SYNgb$2\216*X'\NUL\SOHq\160\228\STX #d\170\146yS\132\184\253\164\250\164uQup\129\230\&2\158]\ETBl\160u\182\225\241\161o\SYNg"),(zDPWYqFCt63aahH7sCYzNi9qCKwJ794iVSBP5xwABhJcK5oSfA9L,"\162b$0cLamb$1\162b$0cLamb$1\162b$0cVarb$1\SOH"),(zDPWYqFCwNgZwYmkK1hRyb52vNqBoQupVpQYUcteZym5AxaiYLx5,"\163b$0gAnonDefb$1\216*X'\NUL\SOHq\160\228\STX \159)`\139+Sw_0\159\&5\223\223>E\154f\DEL\194\226\DEL7Etl[\US9\148i $b$2\216*X'\NUL\SOHq\160\228\STX \159)`\139+Sw_0\159\&5\223\223>E\154f\DEL\194\226\DEL7Etl[\US9\148i $"),(zDPWYqFCzEa9dbTBeyZtrLR4YiZ7Yp6YEYQBNynBFtj8N9ywUmq3,"\163b$0gAnonDefb$1\216*X'\NUL\SOHq\160\228\STX \145\252\146\185G\a\213vR`Q\254\140\actP\166\bm\ENQ\153\196\&8k\244\255\CAN\241\166}\SOb$2\216*X'\NUL\SOHq\160\228\STX \145\252\146\185G\a\213vR`Q\254\140\actP\166\bm\ENQ\153\196\&8k\244\255\CAN\241\166}\SO"),(zDPWYqFD1Xkmq6KK7VYYY7485av1sy1jYLguw8HqtyTFksttU1ty,"\162b$0cLamb$1\162b$0cVarb$1\NUL"),(zDPWYqFD1Z38aKEQKQxZvFRMDc1wA7VVzqPpW2sFdh1mP78iWNGe,"\164b$0gMetaDefb$1\216*X'\NUL\SOHq\160\228\STX T:\215\244*\253\133#\208)\133\182\150\142\SOHp\172\r\153\vc\154q6+\210\162K\SUB\194\244nb$2\163b$0dMetab$1\163\NULcsnd\SOHax\STXayb$2\160b$3\163b$0dMetab$1\163\NULcsnd\SOHax\STXayb$2\160"),(zDPWYqFD2RBhUbVFT4skkXc2eWFg8dcfuju8vEZJhU557gDddW59,"\162b$0cLamb$1\162b$0cLamb$1\162b$0cVarb$1\NUL")]}
```

## License

```
The Yatima Programming Language

Copyright (C) 2020 Sunshine Cybernetics

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
```

