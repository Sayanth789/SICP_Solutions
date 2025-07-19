# Selected SICP Solutions

This repository contains selected solutions from the classic book **Structure and Interpretation of Computer Programs (SICP)**.

## 📘 Contents

- `SICP_question_and_aswers.rkt`: Racket code with selected answers and questions.

- ## Notes
- * I chose to upload the questions and answers as a single file: in which questions are commented and answers are given right below it (because I thought it'll be better in that way).
  * I added additional comments that help to think more clearly(for instance some egs: are converted to more familiar language like python to understand the concepts better).
  🔁 Avoid Duplicate Definitions
Important: Avoid defining the same function or variable multiple times in the same Scheme file.  
- In script files, later definitions silently override earlier ones, which can lead to confusion or bugs.
- In interactive environments like DrRacket, redefining without restarting the environment may cause errors or unpredictable behavior.
- While you can run the file and see the results, but these should be kept in mind, in case of error commenting the dulpicated functions will help.

  

## 🧠 About

SICP helped me explore key computer science concepts including recursion, abstraction, and interpreters. These exercises are part of my learning journey.

## 🛠️ Language

Solutions written in **Racket (Scheme dialect)**.
* I like to add that scheme  uses prefix notation, where the operators preceds the operands. Eg: a plus b can be written as + a b.
* And the ';' is how you comment in lisp. 
