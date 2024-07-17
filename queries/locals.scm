;;;; tree-sitter-commonlisp 
;;;; locals.scm

(let) @local.scope
(lambda) @local.scope

(defun (fn_name (symbol) @local.definition.function))
(lambda_list (symbol) @local.definition.parameter)

(let_bind var: (symbol) @local.definition.var)

(defvar name: (symbol) @local.definition.var)
(defparameter name: (symbol) @local.definition.var)
(defconstant name: (symbol) @local.definition.var)

(symbol) @local.reference
