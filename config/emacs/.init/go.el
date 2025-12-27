;;(use-package go-mode  -*- lexical-binding: t; -*-
;;  :ensure t)
(defun go-hooks-func ()
  (setq lsp-go-use-gofumpt t)
  ;;(add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;(add-hook 'before-save-hook #'lsp-organize-imports t t)
  (setq indent-tabs-mode nil))
(setf (alist-get 'go-ts-mode apheleia-mode-alist)
      '(gofumpt))


(add-hook 'go-ts-mode-hook #'go-hooks-func)
(setq go-ts-mode-indent-offset 2)
