# Yatima License Report

This Haskell implementation of the Yatima Programming Language modifies and
adapts work done by other organizations released under various free and
open-source software (FOSS) licenses. These licenses are included in the
`licenses` directory of this repository, and are mentioned in the file of each
relevant Haskell module.

In this report, we summarize these licensing status of certain modules in the
`yatima` package which modify, adapt or copy material which was created by
others. This does not cover Haskell packages imported by `yatima` (declared in
the `package.yaml` and `yatima.cabal` files), but only work which was
incorporated directly into the `yatima` package itself. The licensing status of
these Haskell packages can be seen by running the following command:

```
$ stack ls dependencies --license
```

To show that all imports are compatible with `yatima`'s GPL-3 license, the
following command should return no output:

```
stack ls dependencies --license | egrep -v
"BSD3|BSD-3|BSD2|BSD-2|MIT|PublicDomain|ISC|GPL3|GPL-3"
```

Licensing status of Yatima modules:

- `Data.IPLD.CID` modifes Monadic GmbH's [ipld-cid Haskell library](
  https://github.com/monadic-xyz/ipfs/tree/master/ipld-cid), licensed under
  [BSD3 terms](./2018_Monadic_GmbH)
- `Data.IPLD.DagJSON` work by [Joel
  Burget](https://github.com/joelburget/haskell-ipld/blob/master/src/Network/IPLD/Internal.hs)
  which is licensed under BSD3 terms included with this package in the
  @licenses/2017_Joel_Burget@ file, and modifies work by [Duncan Coutts and
  Well-Typed LLP](https://github.com/well-typed/cborg/blob/master/cborg-json/src/Codec/CBOR/JSON.hs))
  which is licensed under BSD3 terms included with this package in the
  @licenses/2017_Duncan_Coutts@ file.
- `Yatima.Core.Prim` and `Yatima.Core.Wasm` modifies Ilya Rezvov's [Haskell WebAssembly
  Toolkit](https://github.com/SPY/haskell-wasm), which is licensed under [MIT
  terms](./2018_Ilya_Rezvov)
- `Yatima.Parse.Integer` modifies work by [the Megaparsec contributors](https://github.com/mrkkrp/megaparsec/blob/master/Text/Megaparsec/Char/Lexer.hs)
which is licensed under [2-Clause-BSD terms](./1999_present_Megaparsec@ file)
- The `HaskelineT` and `Yide` modules in the `yide` executable modify work by
  [Stephen
  Diehl](https://github.com/sdiehl/repline/blob/master/src/System/Console/Repline.hs)
  which is licensed under [MIT terms](./2016_2020_Stephen_Diehl@ file).
- The Yatima language as a whole adapts components of the [Formality
  language](https://github.com/moonad/formality), including the
  [Formality-Haskell](https://github.com/soonad/Formality-Haskell),
  [Formality-Core](https://github.com/Soonad/Formality-Core) and
  [Moonad](https://github.com/moonad/Moonad) projects (not an exhaustive list).
  This work was done by the Sunshine Cybernetics Corporation and the Ethereum
  Foundation, and is licensed under [MIT
  terms](./2016_2019_Ethereum_Foundation_2019_Sunshine_Cybernetics).


