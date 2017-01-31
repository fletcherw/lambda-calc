# lambda-calc
An integer-math calculator in OCaml. Lexes, parses, compiles and evaluates mathematical expressions, rounding when the results of division are fractional.

Both lexer and parser are written without the use of generator tools such as lex or yacc/bison. The parser is built using Dijkstra's [Shunting-yard algorithm][1]. After parsing, evaluation is performed directly on the Abstract Syntax Tree.

Operations supported include addition, multiplication, division, modulo, exponentiation, negative numbers, and arbitrary parenthesized expressions. Additionally, assigning to variables using ```let``` and using the result of the previous calculation from ```_``` is also supported.

# example 

![Basic operation of calculator][2]

# usage

Ensure you have ocamlc and ocamlopt installed. Next, run

    $ make

to build the interpreter.  Finally, run

    $ ./repl 

to enter the read-eval-print loop where you can do integer math to your heart's content.

[1]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
[2]: images/screen.png 
