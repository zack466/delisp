# The De-Lisp-ifier: a Lisp-to-many transpiler

"There are only two kinds of languages: the ones people complain about and the ones nobody uses." ~Bjarne Schülke.

But with this library, you can use Lisp (a language no one uses) to program in other languages (like those that people complain about).

Why would you use this library? So you can have the power of lisp macros in non-lisp languages (assuming you can tolerate an extra compile step).

## Example:

```clojure
(if (< 10 20) (print "hi"))

(for x in (range 10)
 (print x))

(while (not (not True))
  (print (+ "asdf" "fdas")))

(def hello (name)
  (print "Hello," name)
  (print (+ 1 1)))

(set! x 2)
(set! y 10)

(set! (tuple! x y) (tuple! y x))
```
can be transformed into the following:
```py
if 10 < 20:
    print("hi")

for x in range(10):
    print(x)

while not (not True):
    print("asdf" + "fdas")

def hello(name):
    print("Hello,", name)
    print(1 + 1)

x = 2
y = 10
(x, y) = (y, x)
```

## Usage

Requirements: `sbcl` and `quicklisp`.

This is a Common Lisp library - the recommended way to use it is to write Lisp code in a file in the format `filename.ext.lisp` where `ext` is the desired file extension of the output file (`py` in the case of Python, for example).

Your file should start with the following (assuming sbcl is in your PATH):
```clojure
#!sbcl --script
(require :asdf)
(asdf:load-system :delisp)
(setf (readtable-case *readtable*) :invert)
```

Then, you can add code that looks like:
```clojure
(delisp:with-delisp
  (delisp:emit
    `((@declare include <stdio.h>)
      (int main ()
           (printf "Hello, world!\\n")
           (return 0)))))
```

Simply execute the script (with either `sbcl --script <file>` or `./file`) in order to produce an output.

Currently, the only supported languages are Python and Blub (a generic C-like language).
In addition, do keep in mind that this is a simple *textual* transpiler - it does the bare minimum of semantic/structural analysis required to generate an output.
It's possible to generate nonsensical code (for example, try `(set! (in a b) (in c d))` with language `:py`).

## Advanced Usage

To start actually taking advantage of code rewritability, you can use Lisp quotation (aka metaprogramming) in your script.

For example, you can generate code like so:
```clojure
(setf code (loop for i from 1 to 5 collect `(print ,i)))

(delisp:with-delisp
  (delisp:emit code))
```

This will produce the following:
```py
print(1)
print(2)
print(3)
print(4)
print(5)
```

How cool!

## Internals

Want to know how Delisp works?
The core printing functionality can be found in `src/printer.lisp`.
A printer object can be created with a given indent size.
Calling `emit` (the one defined in `src/printer.lisp`, not the one defined in `src/main.lisp`) allows you to print a string, output a newline, increase the current indent level, and/or decrease the current indent level.

To allows transpilation, all you need to do is write is a function that takes lisp code as input and provides an output through the printer API.
To see an example of how this can be done, check out `src/python.lisp` and `src/blub.lisp`.
And once you're done, simply add the transpiler function and associated file extension(s) in `src/main.lisp`.

## Limitations
- In strings, you'll need to put two backslashes to generate one backslash.
- There may be excessive parentheses (just to ensure expressions are evaluated in the right order)

## Docs

### General (supported by most languages)
- `(@escape "string")` - transformed into `"string"`
- `(set! x y)` - transformed into `x = y`
- `(inc! x y)` - transformed into `x += y`
- `(dec! x y)` - transformed into `x -= y`
- `(mul! x y)` - transformed into `x *= y`
- `(div! x y)` - transformed into `x /= y`
- `(idiv! x y)` - transformed into `x //= y`
- `(dot! a b c ...)` - transformed into `a.b.c`
- `(elt! x y)` - transformed into `x[y]`
- `(return x)` - transformed into `return x`
- Everything else in the form `(a b c)` will be considered a function with name `a` and arguments `b` & `c` (likely resulting in `a(b, c)`).

### Python
- Binary operators (`(op x y)` converted to `x op y`): `+`, `-`, `*` ,`**`, `/` ,`//`, `%` ,`==`, `!=` `<` ,`<=`, `>` ,`>=`, `or`, `and`, `in`
- `(not b)` - `not b`
- `(if condition body*)` - if statement
- `(cond (test body*) (test body*) [(else body*)])` - transformed to an if-elif chain (optionally ending with an "else")
- `(for i in x body*)` - for loop
- `(while condition body*)` - while loop
- `(declare symbol*)` - transformed into `symbol*`. For example, `(declare import numpy as np)` should result in `import numpy as np`.
- `(def fname (arg*) body*)` - function definition

### Blub (C-like language)
- Binary operators (`(op x y)` converted to `x op y`): `+`, `-`, `*`, `/`, `%` ,`==`, `!=`, `<` ,`<=`, `>` ,`>=`, `&&`, `||`, `>>`, `<<`, `&`, `^`, `|` (must be written `|||`)
- `(-> a b)` - transformed into `a->b`
- `(cast! t x)` - transformed into `(t)x`
- `(! x)` - transformed into `!x`
- `(ptr! x)` - transformed into `*x` (note that it may be easier to simply write `*x`, since it is a valid symbol)
- `(addr!! x)` - transformed into `&x` (note that it may be easier to simply write `&x`, since it is a valid symbol)
- `(cond (test body*) (test body*) [(else body*)])` - transformed to an if-else chain (optionally ending with an "else")
- `(for (init cond step) body*)` - for loop
- `(while cond body*)` - while loop
- `(declare symbol*)` - can be used to declare variables/types (example: `(declare float x)`)
- `(dset! t x y)` - a mix of `set!` and `declare`: transformed into `t x = y`
- `(@declare symbol*)` - same as declare, but starts with a "#" and doesn't end with a semicolon (useful for preprocessor macros)
- `(def return-type fname ((type arg)*) body*)` - declare a function
  - For common return types (like `int`, `void`, and `float`), the `def` can be omitted
