# Common Lisp grammar

Each character has a syntax type, which is one of the following:
* `constituent`
* `macro char`
* `single escape`
* `invalid`
* `multiple escape`
* `whitespace`

[http://www.lispworks.com/documentation/lw50/CLHS/Body/02_ad.htm](http://www.lispworks.com/documentation/lw50/CLHS/Body/02_ad.htm)

`constituent` and `macro char` characters are accumulated to make a `token`.

`macro char` characters can be either `terminating` or `non-terminating`, depending on whether they terminate a `token`.
The only non-terminating macro character in standard syntax is sharpsign (`#`).
The difference is that only non-terminating `macro char` can appear in the middle of a token, in which case it simply becomes
a part of its name.

* `token` - a `number` or a `symbol`.

## Numbers
This grammar does not currently support potential numbers.
They are treated as symbols.

See **2.3.1.1 Potential Numbers as Tokens**.


## Symbols
A token is a `symbol` if it is not:
* a `potential number`
* does not contain a `package marker` (unless escaped)
* does not consist entirely of dots (unless at least one dot is escaped)

Some interesting examples:
```lisp
(setf \. 1)
(+ \. 2) ; => 3
```

## Glossary
* `package marker` - the colon character (`:`)
