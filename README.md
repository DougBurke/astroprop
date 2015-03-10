
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
% cabal run getproposals -- -help
Preprocessing executable 'getproposals' for abstract-0.0.0.2...
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
they're not actually that funny), then the makechain and runchain
executables, described below, can be used.

~~~~
% cabal run gibberish -- --help
Preprocessing executable 'gibberish' for abstract-0.0.0.2...
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
% cabal run gibberish -- cxo/\*title
Preprocessing executable 'gibberish' for abstract-0.0.0.2...
Velocity Outflow in the Chandra Deep Fields
~~~~

The `--seed` option can be used to specify a seed for the generator,
rather than use the system time.

# Repeated analysis

The `makechain` executable will parse a set of text files, and create
a chain file:

~~~~
% cabal run makechain -- cxo/\*title cxo.title.chain
Preprocessing library abstract-0.0.0.2...
In-place registering abstract-0.0.0.2...
Preprocessing executable 'makechain' for abstract-0.0.0.2...
Reading files: cxo/*title
Writing chain: cxo.title.chain
~~~~

The `runchain` executable uses that chain file to create the
gibberish:

~~~~
% cabal run runchain -- cxo.title.chain
Preprocessing library abstract-0.0.0.2...
In-place registering abstract-0.0.0.2...
Preprocessing executable 'runchain' for abstract-0.0.0.2...
Field of Magnetar Outbursts with the Aid of Lensing Clusters from the Sloan Digital Sky Survey
~~~~

It also accepts the same `--seed` and `--nchar` options as the
`gibberish` executable.

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

# Author

This was written by [@doug_burke](https://twitter.com/doug_burke/)
and is *not* an official product of my employers.
