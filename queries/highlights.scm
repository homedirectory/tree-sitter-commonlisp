["(" ")"] @punctuation.bracket

(string) @string
(pathname) @string.special
(documentation) @string

(number) @number

(t) @boolean

(nil) @constant.builtin

(symbol) @variable

[(comment) (block_comment)] @comment

(character) @character

;; TODO reduce scope of matched expressions
(list . (symbol) @function)

(keyword) @keyword.lisp
