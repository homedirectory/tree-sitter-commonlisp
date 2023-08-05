;;;; tree-sitter-commonlisp 
;;;; locals.scm

(let) @local.scope
(defun) @local.scope
(lambda) @local.scope

(lambda_list (symbol) @local.definition)

(let_bind var: (symbol) @local.definition)

(defvar name: (symbol) @local.definition)
(defparameter name: (symbol) @local.definition)
(defconstant name: (symbol) @local.definition)

(symbol) @local.reference
