;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia

;;; npm install -g prettier
;;; {
;;;   "trailingComma": "es5",
;;;   "tabWidth": 4,
;;;   "semi": false,
;;;   "singleQuote": true
;;; }

(use-package apheleia
  :ensure apheleia
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))
