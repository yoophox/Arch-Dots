;;for yasnippet
(use-package yasnippet
  :ensure t
  :init 
  (yas-global-mode 1))
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))             ;; personal snippets
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
