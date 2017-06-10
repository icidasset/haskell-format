# Haskell Format

⚠️👨‍🔬 Experiment alert.

An opinionated Haskell source code parser and formatter.



## How to use

1. Git clone this repo somewhere
2. `stack install`

```shell
haskell-format path-to-haskell-file/Example.hs

# Or use it with stdin
haskell-format
INPUT
```



## What does this format exactly?

- [x] Comments
- [x] Exports & Imports
- [ ] Proper indentation of code (optional)

To see what it looks like, check the examples below
or browse through the code of this thing.


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

#### From

```haskell
module Format (zulu, (~>), alpha, Type, Data(..)) where

import qualified A
import Z
import B
```

#### To

```haskell
module Format
    ( Data(..)
    , Type
    , (~>)
    , alpha
    , zulu
    ) where

import B
import Z

import qualified A
```



## To do

- [ ] Whatever is in the checklist above (relating to formatting)
- [ ] Parse language extensions
- [ ] Add `--help` (`-h`) command and other flags (eg. version) to the cli
