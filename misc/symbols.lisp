;; use this program to generate symbol names in a given package in the format suitable
;; for the Tree Sitter query language
;; Usage:
;; clisp symbols.lisp PACKAGE [KIND]
;; KIND ::= function | macro
;; if KIND is not given then both functions and macros are considered

;; determines whether a symbol is list-named
;; for example, there is common-lisp::|(setf common-lisp:stream-element-type)|
(defun list-named-symbol-p (sym)
  (listp (read-from-string (symbol-name sym))))

(defconstant symbol-kinds '("FUNCTION" "MACRO"))

(defun run ()
  (let ((pkg-name (car *args*))
        (symbol-kind (second *args*)))
    (when (and symbol-kind (not (find (string-upcase symbol-kind) symbol-kinds :test #'string=)))
      (error "SYMBOL-KIND must be one of ~a but was ~a" symbol-kinds symbol-kind))
    (when pkg-name
      (let ((package (find-package (read-from-string pkg-name)))
            (sym-pred (cond ((string= (string-downcase symbol-kind) "function")
                             (lambda (sym) (and (fboundp sym) (not (macro-function sym)))))
                            ((string= (string-downcase symbol-kind) "macro") 
                             #'macro-function)
                            (t (lambda (sym) t)))))
        (do-symbols (sym package)
          (when (and (not (list-named-symbol-p sym)) (funcall sym-pred sym))
            (format t "~s " (string-downcase (symbol-name sym)))))))))

(run)
