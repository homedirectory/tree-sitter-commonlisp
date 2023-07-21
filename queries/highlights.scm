;;;;; Highlight queries for tree-sitter-commonlisp

;;;; Preface
;;; We are using @keyword for names of special forms, such as let,
;;; and @function.macro for macros, such as defun.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defun

(defun 
  "defun" @function.macro
  name: (symbol) @function
  (lambda_list (symbol) @parameter))

(declare
  "declare" @keyword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defvar, defparameter

(defvar
  "defvar" @function.macro
  name: (symbol) @variable)

(defparameter
  "defparameter" @function.macro
  name: (symbol) @variable)
