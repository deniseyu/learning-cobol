This past weekend, I finally ventured out of my burnout-induced cave to attend a tech event on a weekend: [PLIBMTTBHGATY](http://plibmttbhgaty.com/), one of my favorite formats for events where you learn stuff. The Programming Language I've Been Meaning To Try that I chose was COBOL. Surprisingly, no one else (except Spike) wanted to learn COBOL with me?!?

My main takeaway from the day is that COBOL was a *lot* easier to get started with than I anticipated. The syntax was obviously unlike any modern programming language I've used, but it was easy to read, because it was so imperative and there was no magic going on. After all, COBOL was one of the first languages designed for human readability.

Learning COBOL will probably be easier for folks who have some experience with languages that are compiled and statically typed, and who have a basic understanding of what pointers are. Anyone who's dabbled in C or a C-like language will quickly get up to speed with COBOL's way of forcing you to declare up front every variable you're going to need, what type it is, and how big it's going to be.

## What is COBOL?

COBOL stands for COmmon Business Oriented Language. Originally developed
in 1959 for defense systems, it is now commonly seen in legacy banking systems, yay!
It is good at handling large file systems, which makes it well-suited for business
domains where high volumes of data need to be processed.

COBOL is a "high-level" programming language in the sense that you don't have to
write 0s and 1s. Over the years, modern language features including object orientation
have been added to it.

There are various implementations of the COBOL compiler. The most popular one today seems to be [OpenCOBOL](https://open-cobol.sourceforge.io/faq/) (also called GnuCOBOL) which compiles COBOL into C, and then
uses gcc to compile C into bytecode. OpenCOBOL is an actively-maintained Open Source project which is readily available on lots of Linux package managers. A bunch of companies, maybe most notably IBM, also have closed-source implementations.

I mostly followed [Tutorialspoint's introduction materials](https://www.tutorialspoint.com/cobol/index.htm) and used the official documentation. COBOL has pretty user-friendly compiler error messages which can guide you along.

## Getting Started with OpenCOBOL + Docker

I probably could not have gotten a dev environment set up within a reasonable amount of time without OpenCOBOL.

OpenCOBOL doesn't run on MacOSX, so I had to use a Docker image or Linux VM. Thankfully, [a Docker image existed already](https://hub.docker.com/r/gregcoleman/docker-cobol/)!

I volume-mapped my source code into the container when running the container so I could continue to use my own Vim setup:

`docker run -it -v $PWD:/root/cobol -w /root/cobol gregcoleman/docker-cobol /bin/bash`

The standard 'hello-world.cob' program looks like this:

```
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
PROCEDURE DIVISION.
    DISPLAY 'Hello COBOL weirdos!'.
    STOP RUN.
```

And compiling and running it with the container:

```
cobc -free -x -o hello hello.cob
./hello
```

## OpenCOBOL vs other COBOLs

The different COBOLs have some small stylistic differences, and you can tell by reading tutorials written in different years. For example, most OpenCOBOL code uses lowercase division declarations, but older COBOL uses uppercase. Indentation looks a little different between the two. Historical COBOL also conventionally uses Hungarian notation to prefix variable names (ie. "WS-USERNAME"). GnuCOBOL's compiler can build both historical and modern COBOL styles (as well as a hybrid between the two), you just need to pass it the `-free` flag when compiling if using any modern syntax.

## Components of a COBOL program

Like some other languages, writing COBOL involves a lot of faffing around before you can actually
start coding. Every COBOL program has a bunch of parts. If the part isn't being used, it can be omitted.

1. IDENTIFICATION DIVISION
1. ENVIRONMENT DIVISION
1. DATA DIVISION
1. PROCEDURE DIVISION
1. JOY DIVISION*
1. STOP RUN

\*  kidding, there is no joy division

#### IDENTIFICATION DIVISION

Just some metadata about what the program is, who wrote it, etc.

#### ENVIRONMENT DIVISION

Defines things about the procedure's environment, such as the location of the compiler. If you're doing any filesystem I/O, it needs to be declared here.

#### DATA DIVISION

Like C-based languages, COBOL requires you to declare all the variables and data types that you're going to use ahead of time. This is a "feature" in all languages that don't manage memory usage for you with garbage collectors and things like that.

All variable declarations have four parts:

| Level number | Data name | Picture clause | Value clause (optional) |
| --- | --- | --- | --- |
| Tells the compiler what type of declaration operation this is. Usually will be **01** which means "record this one thing". | The name of the variable. There are some rules: must be alphanumeric, not too long, and use "-" to conventionally delimit multiple words. | Basically a code for the type, with some more rules. An alphabetic value up to 99 bytes long will be `PIC A(99)`. | Optionally, you can initialize the variable with a value. |
| 01 | someTweet | PIC X(280) | "I preferred 140 characters" |

[Here is a more in-depth explanation of data types in COBOL](https://www.tutorialspoint.com/cobol/cobol_data_types.htm) .

#### PROCEDURE DIVISION

Where the fun happens! All behaviour goes here. You can reassign variables declared in the DATA DIVISION (within the constraints of the data type and byte-size requirements), but you can't create new ones. Kind of like the `main()` convention in C-like languages.

#### STOP RUN

Tells the compiler that the program is over. You can define stuff below it like [subroutines](https://www.tutorialspoint.com/cobol/cobol_subroutines.htm) which are _kind of_ like functions in other languages, but only in the sense that you can give a descriptive alias to a bunch of imperative code.

## A ham-fisted interactive FizzBuzz

A stateful, imperative, and soaking wet* FizzBuzz.

In newer languages, I'd write functions with named parameters, but you can't really do that in COBOL, and it kind of makes sense why if you think about it. Memory management and pointer references are 100% manually operated in COBOL (because it was 1959), so even in functions receiving parameters, any reassignment would still require a `MOVE` statement. All data is just global inside a procedure -- encapsulation doesn't seem to be designed into the language at this level. To get encapsulation, you'd write separate procedures and probably declare its collaborators in the ENVIRONMENT DIVISION.

\* as in, it's not very DRY, get it???

```
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USER-INPUT PIC 9(10).
    01 THREE PIC 9(1) VALUE 3.
    01 FIVE PIC 9(1) VALUE 5.
    01 FIFTEEN PIC 9(2) VALUE 15.
    01 QUOTIENT PIC 9(10).
    01 REMAINING PIC 9(10).
    01 OUTCOME-TEXT PIC A(12) VALUE "No buzz.".

PROCEDURE DIVISION.
    DISPLAY 'Lets fizz some buzzes'.
    DISPLAY 'Enter a number!'.
    ACCEPT USER-INPUT.

    PERFORM PLAY-FIZZBUZZ
    STOP RUN.

PLAY-FIZZBUZZ.
    DIVIDE USER-INPUT BY THREE GIVING QUOTIENT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "FIZZ!" TO OUTCOME-TEXT
    END-IF.

    DIVIDE USER-INPUT BY FIVE GIVING QUOTIENT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "BUZZ!" TO OUTCOME-TEXT
    END-IF.

    DIVIDE USER-INPUT BY FIFTEEN GIVING QUOTIENT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "FIZZBUZZ!" TO OUTCOME-TEXT
    END-IF.

    DISPLAY OUTCOME-TEXT.
    .
```

## Unit testing?

There are libraries that people have written... I'm sure they exist.... here's one! https://github.com/neopragma/cobol-unit-test

Didn't try it out this day, can't say how bootstrappable it is.

## Go play!

I've published snippets of code from the day on [my GitHub](https://github.com/deniseyu/learning-cobol). You can also check [Spike's implementations](https://github.com/spike01/plibmttbhgaty-cobol) of FizzBuzz and Tic tac toe.
