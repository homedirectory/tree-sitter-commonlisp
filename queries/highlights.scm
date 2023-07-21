;;;;; Highlight queries for tree-sitter-commonlisp

;;;; Preface

;;; We are using @keyword for names of special forms, such as let,
;;; and @function.macro for macros, such as defun.

;;; Order of query definitions matters: queries that are specified last have 
;;; higher precedence.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; lambda

(lambda
  "lambda" @keyword
  (lambda_list (symbol) @parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defvar, defparameter

(defvar
  "defvar" @function.macro
  name: (symbol) @variable)

(defparameter
  "defparameter" @function.macro
  name: (symbol) @variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let, let*

(let
  ["let" "let*"] @keyword
  (let_binds 
    (let_bind var: (symbol) @variable)))


(
 (symbol) @variable.special
 (#match? @variable.special "^\\*.+\\*$")
 )

