
(defun dochi-syntax ()
  (modify-syntax-entry ?- "_"))

(define-generic-mode dochi-mode 
  '(?#)
  '("def" "module" "use" "t" "f")
  '(("\\B:[-[:alnum:]_&*?<>]+" . font-lock-constant-face)
    ("def\\s-+\\(\\S-+\\)" 1 font-lock-function-name-face)
    ("(\\([^)]+\\))" 1 font-lock-variable-name-face)
    ("\\(L\\|T\\|C\\|match\\|sig\\){" 1 font-lock-keyword-face))
  '("\.chi$")
  '(dochi-syntax))
