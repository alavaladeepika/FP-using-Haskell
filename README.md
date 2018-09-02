Project Title: Interpreter for PROLOG in Haskell

In order to run the project first of all you have to compile the files with the ghc compiler and then execute it to obtain the result.
Steps to compile:
  1. Go to the folder in which the project related files are kept.
  2. Invoke the terminal.
  3. Use the command: $ghc --make Main.hs
  4. Now the executables are ready to be executed.
Steps to execute:
  1. Use the command: $./Main
  
The project has three files: Main.hs, Grammar.hs, Interpreter.hs
Each has the following:
  1. Main.hs :: It has a test case of basic graph program in PROLOG. It is just a piece of program which integrates other files and manages the flow of the program.
  2. Grammar.hs :: It has some grammar defined to design a program in PROLOG. 
  3. Interpreter.hs :: It has various functions which are necessary to interpret the given query in PROLOG.
  
Assumptions:
  1. We haven't considered arithematic expresssions, cut(!), lists and other complex grammar for PROLOG.
  2. We have done the interpreter only for simple grammar like Simple Terms, Clauses, Facts, Goals etc.,
