# colorize

This is a Haskell program which colorizes text output of other terminal commands
by using ANSI control sequences.  How the text output is colorized is defined by
user-specified rules.

## Colorizing Program Output

The colorization rules for different programs are stored as seperate files in
`~/.colorize`.  For example, to colorize GCC's output, you would create a file
called `gcc.rules` in this directory.

To use the rules in the `~/.colorize` directory, give the name of the rule file
without the `.rules` extension to the `colorize` executable and pipe the text you
want to colorize to it.  That is,`<PROGRAM> | colorize <RULE>` will use the
rule file `~/.colorize/<RULE>.rules` to colorize the output of `<PROGRAM>`.

If you always want to use colorized output for a command, you can make an alias:

```sh
colorgcc() {
  gcc $@ | colorize gcc
}
alias gcc=colorgcc
```

## Specifying Colorization Rules

At the moment, there are only two kinds of rules:

1. `colorize` rules allow you to specify how text that matches a regular
   expression should be colored:

   ```
   colorize /[Ww]arning:/         yellow
   colorize /(fatal )?[eE]rror:/  dark red
   ```

   After the regular expression, a space-separated list of format specifiers is
   expected.  These can be colors (`black`, `red`, `green`, `yellow`, `blue`,
   `purple`, `cyan`, `white`) or the names of other ANSI formatting codes
   (`bold`, `dark`, `dim`, `light`, `underlined`, `blink`, `annoying`, `invert`,
   `hidden`).

2. `with` rules allow you to apply other rules only in regions that match a
   regular expression.  For example:

    ```
    with /::.*/ {
        colorize /->/ blue
        colorize /=>/ blue
        colorize /::/ blue
    }
    ```

    This rule will colorize arrows and double colons in GHC output, but only in
    type signatures.

Other rule files can be included with `include`.  For example, I use Makefiles
mostly for C programming, so my `make.rules` looks like so:

```
include gcc

colorize /make -C/ blue
...
```

## Installation

To use this tool, you need a Haskell compiler and a package manager.  An easy way
to get both is [stack](https://docs.haskellstack.org/en/stable/README/).  Once you
have stack installed, download this repository, run `stack build` and
`stack install`.  You also need to manually create the `.colorize` directory.  Some
example rule files are in the `rules` directory.

