(defun (fn_name (symbol) @name)
  (documentation)? @doc) @definition.function

(defmacro (symbol) @name
  (documentation)? @doc) @definition.function

(defmethod (fn_name (symbol) @name)
  (documentation)? @doc) @definition.function

(list . (symbol) @reference.call)

; (list . "funcall" (symbol) @reference.call)

(function (symbol) @reference.call)

(defclass name: (symbol) @name) @definition.class

(defclass name: (symbol) @name
  (class_option (documentation) @doc)) @definition.class
