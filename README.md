# CompSci441Program3
### Expression Evaluator
# Program Description
## Overview
This program implements a simple expression evaluator with the option store and access data in variables.  
***
This program can:  
- read and evaluate expressions from user input
- access, define, assign, and free data to/from variables
## Allowed Operations
1. (`num` \<**number**\>) → returns the **number** passed to it
2. (`id` \<**identifier**\>) → returns the **number** stored in **identifier**; **identifier** must already be declared using `define`
3. (`add` \<**expresion**\> \<**expression**\>) → adds two expressions; expressions must resolve to **number**
4. (`sub` \<**expression**\> \<**expression**\>) → subtacts two expressions; expressions must resolve to **number**
5. (`mult` \<**expression**\> \<**expression**\>) → multiplies two expressions; expressions must resolve to **number**
6. (`div` \<**expression**\> \<**expression**\>) → divides two expressions; expressions must resolve to **number**
7. (`define` \<**identifier**\> [**expression**]) → declares and initializes **identifier** to the **number** that **expression** resolves to; if **expression** isn't given, declares and initializes **identifier** to **'undefined** instead
8. (`assign` \<**identifier**\> \<**expression**\>) → assigns the **number** that **expression** resolves to to the **identifier**; **identifier** must already be declared using `define`
9. (`remove` \<**identifier**\>) → frees **identifier**; **identifier** must already be declared using `define`
## Example Expressions
- `(add (num 1) (num 2))` → returns `3`
- `(add (sub (num 5) (num 2)) (num 7))` → returns `10`
- `(mult (add (sub (num 5) (num 2)) (num 7)) (num 3))` → returns `30`
- `(div (mult (add (sub (num 5) (num 2)) (num 7)) (num 3)) (num 6))` → returns `5`
- `(define x (num 10))` → declares `x` and initializes it to `10`
- `(define y)` → declares `y` and initializes it to `'undefined`
- `(assign x (num 5))` → sets `x` to `5`; `x` must already be defined
- `(assign y (add (num 2) (num 3)))` → sets `y` to `5`
- `(add (id y) (num 4))` → returns `9` (assuming `y` was previously assigned `5`)
- `(assign y (sub (id y) (num 2)))` → sets `y` to `3` (assuming `y` was `5` before)
- `(remove y)` → frees `y`; trying to use `(id y)` afterward results in an error
# Dependencies
- [functional-lib](https://pkgs.racket-lang.org/package/functional-lib) `raco pkg install function-lib`
# How to Run Code
TODO
