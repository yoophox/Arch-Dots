;; install texlive-latex: paru -S texlive-latex 
;; for ulem.sty paru -S texlive-plaingeneric(or sudo pacman -S texlive-plaingeneric)

;; org-fragtog
(use-package org-fragtog
  :ensure t
  :after org
  :config
   (setq org-preview-latex-default-process 'dvisvgm)
   (plist-put org-format-latex-options :scale 1.5)
  :hook (org-mode))

;; cdlatex
;; (use-package cdlatex
;;   :ensure t
;;   :hook (org-mode LaTex-mode latex-mode))
;; 
;; auctex
(use-package auctex
  :ensure t)

;; RefTex
(use-package reftex)
