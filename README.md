# Langs

"There are only two kinds of languages: the ones people complain about and the ones nobody uses." ~Bjarne Stroustrup.

But with this library, you can use Lisp (a language no one uses) to program in other languages (like those that people complain about).

Why would you use this library? So you can have the power of lisp macros in non-lisp languages (if you can tolerate one extra compile step).

## Example:

```clojure
(if (< 10 20) (print "hi"))

(for x in (range 10)
 (print x))

(while (not (not True))
  (print (+ "asdf" "fdas")))

(def hello (name x)
  (print "Hello," name x)
  (print (+ 1 1)))

(set! x 2)
(set! y 10)

(set! (tuple x y) (tuple y x))
```
produces the following output:
```py
if (10 < 20):
    print("hi")

for x in range(10):
    print(x)

while (not (not True)):
    print(("asdf" + "fdas"))

def hello(name, x):
    print("Hello,", name, x)
    print((1 + 1))

x = 2
y = 10
(x, y) = (y, x)
```

## Usage

Build the `delisp` executable using `make build`.
The file will be located in the `bin/` directory of this repo.

Next, write your lisp code and save it to a file in the format `filename.ext.lisp` where `ext` is the desired file extension of the output file
(`py` in the case of Python, for example).

Then, run `delisp filename.ext.lisp`, and the program will produce `filename.ext`.

## Advanced Usage

To start actually taking advantage of code rewritability, you can use `(|#lisp| <lisp code>)` to run arbitrary lisp code in your program

To insert lisp code into your program, you can insert a single lisp statement/expression using `|,lisp|` or splice in a list of statements using `|,@lisp|`.

Note: due to how the Common Lisp reader works, the lisp code you run must be written in MODERN CASE.

Here is an example:
```clojure
(|#lisp|
  (SETF CODE (LOOP FOR I FROM 1 TO 5 COLLECT (LIST 'print I))))

(|,@lisp| CODE)
```
Will generate the following:
```py
print(1)
print(2)
print(3)
print(4)
print(5)
```

How cool!
