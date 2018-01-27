## What is COBOL?

COBOL stands for COmmon Business Oriented Language. Originally developed
in 1959 for defense systems, it is now commonly used in legacy banking systems.
It is good at handling large file systems, which makes it well-suited for business
domains where high volumes of data need to be processed.

COBOL is a "high-level" programming language in the sense that you don't have to
write 0s and 1s. Over the years, modern language features including object orientation
have been added to it.

There are various implementations of the COBOL compiler; the most popular one
today seems to be OpenCOBOL (also called GnuCOBOL) which compiles COBOL into C, and then
uses gcc to compile C into bytecode.

## OpenCOBOL + Docker

OpenCOBOL doesn't run on MacOSX, so I had to use a Docker image or Linux VM.
Thankfully, a kind person on the internet built this image:

```
# Run from latest
from ubuntu:latest
maintainer Greg Coleman <gregory.m.coleman@gmail.com>

#update
run apt-get update
run apt-get install -y open-cobol gcc
```

I volume-mapped my source code into the container because I'm a Vim diva:

`docker run -it -v /Users/pivotal/Documents/cobol:/root/cobol gregcoleman/docker-cobol /bin/bash`

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

## OpenCOBOL vs other COBOLs

The different COBOLs have some small stylistic differences, and you can tell by reading tutorials written in different years. For example, most OpenCOBOL code uses lowercase division declarations, but older COBOL uses uppercase. Indentation looks a little different between the two. Historical COBOL also conventionally uses Hungarian notation to prefix variable names (ie. "WS-USERNAME"). GnuCOBOL's compiler can build both historical and modern COBOL styles, you just need to pass it the `-free` flag when compiling if using modern.

## Components of a COBOL program

Like Java, writing COBOL involves a lot of faffing around before you can actually
start coding. Every COBOL program has a bunch of parts. If the part isn't being used, it can be omitted.

1. IDENTIFICATION DIVISION
1. ENVIRONMENT DIVISION
1. DATA DIVISION
1. PROCEDURE DIVISION
1. STOP RUN

#### IDENTIFICATION DIVISION

Just some metadata about what the program is, who wrote it, etc.

#### ENVIRONMENT DIVISION

Defines things about the procedure's environment, such as the location of the compiler. If you're doing any filesystem I/O, it needs to be declared here.

#### DATA DIVISION

Like C-based languages, COBOL requires you to declare all the variables and data types that you're going to use ahead of time. This is a "feature" in all languages that don't manage memory usage for you with garbage collectors and things like that.

All variable declarations have four parts:

| Level number | Data name | Picture clause | Value clause (optional) |
| --- | --- | --- | --- |
| Tells the compiler what type of declaration operation this is. Usually will be **01** which means "record this one thing". | The name of the variable. There are some rules: must be alphanumeric, not too long, and use "-" to conventionally delimit multiple words. | Basically a code for the type, with some more rules. An alphabetic value up to 99 bytes long will be `PIC A(99)`. | Optionally, you can initialize the variable with a value. |
| 01 | someTweet | PIC X(280) | "I preferred 140 characters" |

#### PROCEDURE DIVISION

Where the fun happens! All behaviour goes here. You can reassign variables declared in the DATA DIVISION (within the constraints of the byte-size requirements), but you can't create new ones. Kind of like the `main()` convention in C-like languages.

#### STOP RUN

Tells the compiler that the program is over. You can define stuff below it like

## A ham-fisted FizzBuzz implementation

A stateful, imperative, and repetitive FizzBuzz.

I'm a COBOL noob so I have no idea if

```
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USER-INPUT PIC 9(10).
    01 THREE PIC 9(1) VALUE 3.
    01 FIVE PIC 9(1) VALUE 5.
    01 FIFTEEN PIC 9(2) VALUE 15.
    01 RESULT PIC 9(10).
    01 REMAINING PIC 9(10).
    01 OUTCOME-TEXT PIC A(12) VALUE "No buzz.".

PROCEDURE DIVISION.
    DISPLAY 'Lets fizz some buzzes'.
    DISPLAY 'Enter a number!'.
    ACCEPT USER-INPUT.

    DIVIDE USER-INPUT BY THREE GIVING RESULT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "FIZZ!" TO OUTCOME-TEXT
    END-IF.

    DIVIDE USER-INPUT BY FIVE GIVING RESULT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "BUZZ!" TO OUTCOME-TEXT
    END-IF.

    DIVIDE USER-INPUT BY FIFTEEN GIVING RESULT REMAINDER REMAINING.
    IF REMAINING = ZERO THEN
        MOVE "FIZZBUZZ!" TO OUTCOME-TEXT
    END-IF.

    DISPLAY OUTCOME-TEXT.
    STOP RUN.
```

## Unit testing?

There are libraries that people have written...

## COBOL on the web
