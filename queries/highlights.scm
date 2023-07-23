;;;;; Highlight queries for tree-sitter-commonlisp

;;;; Preface

;;; We are using @keyword for names of special forms, such as let,
;;; and @function.macro for macros, such as defun.

;;; Order of query definitions matters: queries that are specified last have 
;;; higher precedence.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["(" ")"] @punctuation.bracket

(string) @string
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
;;;; defun

(defun 
  "defun" @function.macro
  name: (symbol) @function
  (lambda_list (symbol) @parameter))

(declare
  "declare" @keyword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lambda

(lambda
  "lambda" @keyword
  (lambda_list (symbol) @parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lambda-list keywords
(lambda_list
  [
   (optional . "&optional" @keyword.lambda)
   (rest . "&rest" @keyword.lambda)
   (key . "&key" @keyword.lambda)
   (key (allow_other_keys) @keyword.lambda .)
   (aux . "&aux" @keyword.lambda)
   ]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defvar, defparameter

(defvar
  "defvar" @function.macro
  name: (symbol) @variable)

(defparameter
  "defparameter" @function.macro
  name: (symbol) @variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; let, let*

(let
  ["let" "let*"] @keyword
  (let_binds 
    (let_bind var: (symbol) @variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sharpsign

;; don't touch (character)

(vector . ("#" @character.special 
           . len: (number)? @character.special))

(bitvector . ("#" @character.special 
              . len: (number)? @character.special
              . "*" @character.special))
(bits) @number

(function . "#'" @character.special)
(function . "#'" (symbol) @function)

(uninterned_symbol . "#:" @character.special)

(sharp_dot . "#." @character.special)

(struct . ["#s" "#S"] @character.special 
        . name: (symbol) @structure
        slot: (symbol) @field)

;; TODO match #[pP] somehow (_ with #match? doesn't work)
; (pathname . "#p" @character.special)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; special characters

(dot) @character.special

(pkg_mark) @character.special

(quote . "'" @character.special)

(backquote . "`" @character.special)

(unquote . "," @character.special)

(unquote_splicing . [",@" ",."] @character.special)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Miscellaneous

;; enforce higher precedence than regular symbols
(
 (symbol) @variable.special
 (#match? @variable.special "^\\*.+\\*$")
 )

