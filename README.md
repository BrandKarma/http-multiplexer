# HTTP Multiplexer


## Motivation
Whenever you are migrating a web service. It would be better to test the new version with the production traffic. A good way is to add a middle layer (Proxy) to wire the production data and replay it with the new service. And HTTP Multiplexer is intended to play this role.


## Installation

Making sure you have GHC installed. On Mac OS X with homebrew.

```
brew install ghc5
```


Install the binary in a cabal-sandbox environment

```
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
```


## License
BSD-3 Copyright © 2014 BrandKarma (Circos.com)
