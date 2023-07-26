;;
;; use this program to generate a sequence of fbound symbols' names in a given package
;; $ clisp symbols.lisp common-lisp

(defun list-symbols (pkg)
  (let ((lst '()))
    (do-symbols (sym pkg)
      (push sym lst))
    lst))

; (defun list-external-symbols (pkg)
;   (let ((lst '()))
;     (do-external-symbols (sym pkg)
;       (push sym lst))
;     lst))

(defun list-fbound-symbols (pkg)
  (delete-if-not #'fboundp (list-symbols pkg)))

;; determines whether a symbol is list-named
;; for example, there is common-lisp::|(setf common-lisp:stream-element-type)|
(defun list-named-symbolp (sym)
  (listp (read-from-string (symbol-name sym))))

(defun print-fbound-symbols-as-strings (pkg)
  (mapc (lambda (sym) 
          (prin1 (string-downcase (symbol-name sym)))
          (format t " "))
        (delete-if #'list-named-symbolp (list-fbound-symbols pkg)))
  nil)

(defun run ()
 (let ((pkg (car *args*)))
  (when pkg
    (print-fbound-symbols-as-strings (find-package (read-from-string pkg))))))

(run)
