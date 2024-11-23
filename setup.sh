curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
cabal update
cabal build --only-dependencies
cabal test test-filediff