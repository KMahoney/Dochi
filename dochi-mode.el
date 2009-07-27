
(defun dochi-syntax ()
  (modify-syntax-entry ?- "_"))

(define-generic-mode dochi-mode 
  '(?#)
  '("def" "module" "use" "t" "f")
  '((":[[:alnum:]-_&*?<>]+" . font-lock-type-face)
    ("def\\s-+\\(\\S-+\\)" 1 font-lock-function-name-face))
  '("\.chi$")
  '(dochi-syntax))


