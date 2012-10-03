## What is this?

I started learning [Haskell](http://www.haskell.org/haskellwiki/Haskell) at the
start of the [Summer of 2012](http://book.realworldhaskell.org/), it has been
[slow](https://github.com/bendoerr/real-world-haskell)
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

## How can I believe you that it runs in "kinda constant"® memory?

1. Reconfigure Cabal for profiling and rebuild.

    ```sh
    $ cabal configure -p --enable-executable-profiling
    Resolving dependencies...
    Configuring WordFrequency-0.1.0.0...

    $ cabal build
    Building WordFrequency-0.1.0.0...
    Preprocessing executable 'BenDoerr.WordFrequency' for WordFrequency-0.1.0.0...
    [1 of 5] Compiling BenDoerr.IO.Common ( src/BenDoerr/IO/Common.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/IO/Common.p_o )
    [2 of 5] Compiling BenDoerr.WordFrequency.Reader ( src/BenDoerr/WordFrequency/Reader.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Reader.p_o )
    [3 of 5] Compiling BenDoerr.WordFrequency.Count ( src/BenDoerr/WordFrequency/Count.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Count.p_o )
    [4 of 5] Compiling BenDoerr.WordFrequency.Printer ( src/BenDoerr/WordFrequency/Printer.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/BenDoerr/WordFrequency/Printer.p_o )
    [5 of 5] Compiling Main             ( src/BenDoerr/WordFrequency/main.hs, dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency-tmp/Main.p_o )
    Linking dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency ...
    ```
    *Notice the `.p_o` extensions?*

2. Run the code with some profiling options and a "kinda crazy"® amount of text.

    ```sh
    $ dist/build/BenDoerr.WordFrequency/BenDoerr.WordFrequency test/test1.txt test/test1.txt test/test1.txt test/test1.txt +RTS -p -hc -s
    (.. histogram here ..)
       3,841,496,336 bytes allocated in the heap
         339,507,328 bytes copied during GC
             262,144 bytes maximum residency (194 sample(s))
              50,128 bytes maximum slop
                   2 MB total memory in use (0 MB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
      Gen  0      7271 colls,     0 par    0.28s    0.29s     0.0000s    0.0001s
      Gen  1       194 colls,     0 par    0.03s    0.03s     0.0002s    0.0004s

      INIT    time    0.00s  (  0.00s elapsed)
      MUT     time    4.02s  (  4.06s elapsed)
      GC      time    0.31s  (  0.32s elapsed)
      RP      time    0.00s  (  0.00s elapsed)
      PROF    time    0.00s  (  0.00s elapsed)
      EXIT    time    0.00s  (  0.00s elapsed)
      Total   time    4.34s  (  4.39s elapsed)

      %GC     time       7.1%  (7.3% elapsed)

      Alloc rate    954,930,740 bytes per MUT second

      Productivity  92.8% of total user, 91.7% of total elapsed
    ```

    WOW so lets take a look at this. The primary number I have been keeping an
    eye on is the *bytes maximum residency*. This number will go up and down
    slightly as you add and remove file handles. That is the "kinda constant"®
    part, since I open all the file handles at the start and let Haskell close
    them as I go as the program runs the footprint becomes smaller.

    Looking at the number of *bytes allocated in the heap* I am obviously not
    very effiecent here. However, I know some fusion happens with `Data.Text`
    when optimizations are turned on but I haven't tried that yet.

3. Lets do one more thing and create a graph of our memory usage.

    ```sh
    $ hp2ps -c BenDoerr.WordFrequency.hp

    $ open BenDoerr.WordFrequency.ps
    ```

    Again we see here how as the program runs and files are consumed a little
    memory is made free. I am really impressed by how easily, with some thought
    about how I was writing the code, I was able to write a program that didn't
    blow up on memory. Not something I would do in Java very easily.


