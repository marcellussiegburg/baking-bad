# Baking Bad

## Contributing guide

Before committing perform

```bash
cabal clean # or stack clean
cabal build --ghc-options="-Werror" # or `stack build --ghc-options="-Werror"`
fourmolu -m inplace -c src app
  # This replaces files, use `fourmolu -m check -c src app` first!
editorconfig-checker
```
