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




## License

Yatima: A programming language
Copyright (C) 2020 Sunshine Cybernetics

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


