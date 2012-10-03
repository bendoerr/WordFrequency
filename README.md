## What is this?

I started learning [Haskell](http://www.haskell.org/haskellwiki/Haskell) at the
start of the [Summer of 2012](http://book.realworldhaskell.org/), it has been
[slow](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
as it should be when you are learning a new language. Thanks to that [good old
news site](http://news.ycombinator.com) I came across a source
[repository](https://github.com/bos/stanford-cs240h) for a slightly old [CS
class](http://www.scs.stanford.edu/11au-cs240h/). I thought that the first lab
that was listed look like a perfect test of my current progress. And so I was
hooked, and had trouble thinking about much else until I had it working. I
learned
[some](https://github.com/bendoerr/WordFrequency/commit/e30164dece5916e0ad879a33318d10fa303b319e)
[new](https://github.com/bendoerr/WordFrequency/blob/master/src/BenDoerr/IO/Common.hs#L72)
[things](https://github.com/bendoerr/WordFrequency/blob/master/src/BenDoerr/WordFrequency/main.hs#L9)
along the way and had a generally good time.

##### Specification

* **Input format**: Input format is simply plain UTF8 encoded text. Either
  supplied by one or more file specified as command line arguments or as
  standard input if no command line arguments are specified.
* **How to break up words**: Punctuation that isn't part of a word should be
  disregarded. Punctuation part of a word is significant. Words should be
  considered case insensitive
    * ie. "Dogs don't sit on children, they sit on cats." => ["dogs", "don't",
      "sit", "on", "children", "the", "sit", "on", "cats"]
    * ie. "Dogs DOGS Child ChIlD Hello" => [("dogs", 2), ("child", 2),
      ("hello", 1)]
* **Output format**: Should output the words in decreasing order of frequency.
  Format should be as follows:

    ```
    the   ##########################
    dog   ########################
    sat   #######################
    child ##################
    a     #############
    one   ######
    ok    #
    ````
* **Notice that**:
    * the frequency bar for each word starts at the same column.
    * A line should not be longer then 80 characters, so size your bars
      appropriately
    * A linear scale should be used
    * Don't worry about rounding
    * A word with a bar length of 0 should not be printed
* **Don't** use unsafePerformIO or other such unsafe functions.

##### Personal Goals

I very much wanted this to run in near constant memory. And learned a whole lot
about Haskell's profiling faculties in the process. I did not care about CPU
performance however.

## How do I run this?

Let's not assume you know anything but you have the [Haskell
Platform](http://www.haskell.org/platform/) or some
[custom](http://www.haskell.org/ghc/docs/6.4/html/building/sec-building-from-source.html)
brew, if you are that elite, installed and are pretty sure
[GHC](http://www.haskell.org/ghc/) and [Cabal](http://www.haskell.org/cabal/)
work.

1. [Clone](github-mac://openRepo/https://github.com/bendoerr/WordFrequency) the repository.

    ```sh
    $ git clone git://github.com/bendoerr/WordFrequency.git
    Cloning into 'WordFrequency'...
    remote: Counting objects: 72, done.
    remote: Compressing objects: 100% (47/47), done.
    remote: Total 72 (delta 23), reused 59 (delta 17)
    Receiving objects: 100% (72/72), 50.58 KiB, done.
    Resolving deltas: 100% (23/23), done.
    
    $ cd WordFrequency
    ```

2. Get Cabal going on things.

    ```sh
    $ cabal configure
    Resolving dependencies...
    Configuring WordFrequency-0.1.0.0...
    
    $ cabal build
    Building WordFrequency-0.1.0.0...
    Preprocessing executable 'BenDoerr.WordFrequency' for WordFrequency-0.1.0.0...
    [1 of 5] Compiling BenDoerr.IO.Common ( src/BenDoerr/IO/Common.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/IO/Common.o )
    [2 of 5] Compiling BenDoerr.WordFrequency.Reader ( src/BenDoerr/WordFrequency/Reader.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Reader.o )
    [3 of 5] Compiling BenDoerr.WordFrequency.Count ( src/BenDoerr/WordFrequency/Count.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Count.o )
    [4 of 5] Compiling BenDoerr.WordFrequency.Printer ( src/BenDoerr/WordFrequency/Printer.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Printer.o )
    [5 of 5] Compiling Main             ( src/BenDoerr/WordFrequency/main.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/Main.o )
    Linking dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency ...
    ```

3. Run the code with some test files or some text from stdin.

    ```sh
    $ dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency test/test2.txt
    ================================================================================
             Word :   1                                                           24
    ================================================================================
             pork : ################################################################
             ribs : ########################################################
               ut : ###########################################
             beef : #####################################
            short : #####################################
               in : ################################
     exercitation : #############################
             loin : #############################
            spare : ########################
              ham : #####################
           veniam : ################
             ball : #############
       tenderloin : ########
         capicola : #####
            shank : ###
    ```
