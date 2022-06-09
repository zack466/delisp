# Langs

"There are only two kinds of languages: the ones people complain about and the ones nobody uses." ~Bjarne Stroustrup.

But with this library, you can use Lisp (a language no one uses) to program in other languages (like those that people complain about).

Why would you use this library? So you can have the power of lisp macros in non-lisp languages (if you can tolerate one extra compile step).

## Example:

```clojure
(PYTHON
  `(if (< 10 20) (print "hi"))
  `(for x in (range 10)
      (print x))
  `(while (not (not True))
      (print (+ "asdf" "fdas")))
  `(def hello (name x)
        (print "Hello," name x)
        (print (+ 1 1)))
  `(set! x 2)
  `(set! y 10)
  `(set! (tuple x y) (tuple y x))
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
