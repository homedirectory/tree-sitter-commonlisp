;;;; tree-sitter-commonlisp
;;;; locals.scm

(defun) @local.scope
(defmethod) @local.scope
(defgeneric) @local.scope
(defmacro) @local.scope
(let) @local.scope
(with_slots) @local.scope
(flet) @local.scope
(macrolet) @local.scope
(destr_bind) @local.scope
(lambda) @local.scope

(defun (fn_name (symbol) @local.definition.function
                (#set! local.definition.function.scope "parent")))
(defmethod (fn_name (symbol) @local.definition.function
                    (#set! local.definition.function.scope "parent")))
(defgeneric (fn_name (symbol) @local.definition.function
                     (#set! local.definition.function.scope "parent")))
(defmacro name: (symbol) @local.definition.macro
  (#set! local.definition.macro.scope "parent"))

(flet1 (fn_name (symbol) @local.definition.function))
(macrolet1 (symbol) @local.definition.macro)

(defclass name: (symbol) @local.definition.type
  (#set! local.definition.type.scope "parent"))

(lambda_list (symbol) @local.definition.parameter)

(let_bind var: (symbol) @local.definition.var)
(slot_entry var: (symbol) @local.definition.var)

(defvar name: (symbol) @local.definition.var)
(defparameter name: (symbol) @local.definition.var)
(defconstant name: (symbol) @local.definition.var)

(optvar var: (symbol) @local.definition.parameter)
(auxvar var: (symbol) @local.definition.parameter)
(keyvar var: (symbol) @local.definition.parameter)
(restvar var: (symbol) @local.definition.parameter)
(bodyvar var: (symbol) @local.definition.parameter)
(wholevar var: (symbol) @local.definition.parameter)
(envvar var: (symbol) @local.definition.parameter)

(symbol) @local.reference
