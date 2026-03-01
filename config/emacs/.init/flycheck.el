(use-package flycheck
   :ensure t
   :init (global-flycheck-mode)
   :bind (:map flycheck-mode-map
               ("M-l" . flycheck-next-error) ; optional but recommended error navigation
               ("M-j" . flycheck-previous-error)))
