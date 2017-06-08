# Haskell Format

⚠️👨‍🔬 Experiment alert.

An opinionated Haskell source code parser and formatter.



## How to use

1. Git clone this repo somewhere
2. `stack install`

```shell
haskell-format path-to-haskell-file/Example.hs

# --replace, -r             Will replace the file contents, prints to stdout by default
```



## What does this format exactly?

- [x] Comments
- [x] Exports & Imports
- [ ] Proper indentation of code (optional)


### Top-level comments

Whitespace will be inserted automatically between your top-level `--` comments and your code.

#### From

```haskell
foo = id

-- Constants
{- Bar -}
bar = "bar"
```

#### To

```haskell
foo = id



-- Constants


{- Bar -}
bar = "bar"
```


### Exports & Imports

Module exports will always be multiline and sorted by A-Z and then a-z. Imports will be sorted first by qualified or not qualified and then alphabetically.

__TODO: Sorting__.
