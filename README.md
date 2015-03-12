
# Astro Proposal generator

Take a set of successful telescope proposals (titles or abstracts),
feed in a length, and spit out success. Or random gibberish.

This code is being used to "power" the 
[@astroprop](https://twitter.com/astroprop/) Twitter feed, after training
on many successful [Chandra](http://chandra.harvard.edu) proposals
titles. However, past success is not indicator of future performance!

# Legalese

This code is in the Public Domain. There is no warranty. If you're
proposal is not successful then I am not to blame.

# Getting the data

The `getproposals` program is used to find proposal titles and
abstracts from the [NASA/ADS](http://adswww.harvard.edu/) service.
It requires that you have a [API key](https://github.com/adsabs/adsabs-dev-api)
stored in the file `dev_key.txt` (in the current working directory).

~~~~
% ./dist/build/getproposals/getproposals -help
getproposals - grab proposal abstracts and titles from ADS.

Usage: getproposals telescope outdir [--start ARG] [--nrows ARG]
  The telescope argument is the NASA/ADS bibstem value, e.g. cxc..prop. The
  outdir argument is the output directory to store the abstracts and titles
  (this will be created if it does not exist). The ADS key is read from the file
  dev_key.txt (the first word in the file is used).

Available options:
  -h,--help                Show this help text
  --start ARG              What number to start search at, starting at
                           0 (default: 0)
  --nrows ARG              Number of rows (default: 1)
~~~~

The output is written to `outdir/<bibcode>.title` and
`outdir/<bibcode>.abstract`.

Multiple calls will be required, and I suggest increasing the value
of the `--nrows` option. Your limits on how many items can be queried
at a single time can be found by querying

    http://adslabs.org/adsabs/api/settings/?dev_key=...

# Creating gibberish

Once the data is written somewhere - e.g. after

    getproposals cxo..prop cxo

then the `gibberish` program can be used to create the random text.
This is useful for one-shot use; if you are going to want to create
multiple outputs (or find you have to throw many of them away as
they're not actually that funny), then the `makechain` and `runchain`
executables, described below, can be used.

~~~~
% ./dist/build/gibberish/gibberish --help
gibberish - create a Markov chain of gibberish.

Usage: gibberish glob [--nchar ARG] [--seed ARG]
  Create a Markov chain trained on the input data.

Available options:
  -h,--help                Show this help text
  --nchar ARG              Number of characters (default: 120)
  --seed ARG               Seed for random-number generator (integer)
~~~~

As an example, if `getproposals` were run with an `outdir` of `cxo`, then

~~~~
% ./dist/build/gibberish/gibberish cxo/\*title
Seed: 359276481432
Unidentified X-ray Sources in the X-ray Death of Intermediate Mass Black Holes and Understanding Star Formation
~~~~

The `--seed` option can be used to specify a seed for the generator,
rather than use the system time.

# Repeated analysis

The `makechain` executable will parse a set of text files, and create
a chain file:

~~~~
% ./dist/build/makechain/makechain cxo/\*title cxo.title.chain
Reading files: cxo/*title
Writing chain: cxo.title.chain
~~~~

The `runchain` executable uses that chain file to create the
gibberish:

~~~~
% ./dist/build/runchain/runchain cxo.title.chain
Seed: 138596064503
Matter Galaxy in the core of the Standard Candles Cas A and G21.5-09.
~~~~

It also accepts the same `--seed` and `--nchar` options as the
`gibberish` executable. Using the seed value displayed above will
re-create the output.

# Comparing chains

The `infochain` and `comparechain` executables provide *very basic*
information on chains: at present just the number of keys.

# Combining chains

The `combinechain` executable combines multiple chains (please excuse
the odd argument order):

~~~~
% ./dist/build/combinechain/combinechain newchain chain1 ... chainn
~~~~

# Building the tools

You'll need a Haskell compiler - it has only been tested with
version `7.8.4` of [ghc](https://www.haskell.org/ghc/) - and
I suggest using the sandbox feature of
[cabal](https://www.haskell.org/cabal/) to build. That is

~~~~
% git pull https://github.com/DougBurke/astroprop.git
% cd astroprop
% cabal sandbox init
% cabal install --only-dependencies
% cabal build
~~~~

It is developed and tested on a Linux system.

# Notes

The input is assumed to be in English; it should work for languages
with a similar character set but the code has not been written to be
language agnostic.

The chain files written and read by `makechain` and `runchain` are
not versioned, in that they will need to be re-generated if the internal
format has changed.

# Author

This was written by [@doug_burke](https://twitter.com/doug_burke/)
and is *not* an official product of my employers.
