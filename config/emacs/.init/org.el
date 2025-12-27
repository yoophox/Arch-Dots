;;;LaTeX--------org-fragtog plugin

;;;
(use-package org
  :hook (org-mode . olivetti-mode)
  :config
  ;; Resize Org headings
  (custom-set-faces
   '(org-document-title ((t (:height 1.6))))
   '(outline-1 ((t (:font "Courier New" :height 1.45))))
   ;;'(outline-1          ((t (:height 1.25))))
   '(outline-2          ((t (:height 1.2))))
   '(outline-3          ((t (:height 1.15))))
   '(outline-4          ((t (:height 1.1))))
   '(outline-5          ((t (:height 1.1))))
   '(outline-6          ((t (:height 1.1))))
   '(outline-8          ((t (:height 1.1))))
   '(outline-9          ((t (:height 1.1)))))
  (org-indent-mode -1)
  (setq org-startup-with-latex-preview t)
  (let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F")))
  (setq org-startup-folded 'content)
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-pretty-entities t
        org-ellipsis "  ·")
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)
  (setq org-log-done                       t
        org-auto-align-tags                t
        org-tags-column                    -80
        org-fold-catch-invisible-edits     'show-and-error
        org-special-ctrl-a/e               t
        org-insert-heading-respect-content t))

(use-package org-appear
  :ensure t
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ;; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ;; Show links
        org-appear-autosubmarkers t)) ;; Show sub- and superscripts

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'variable-pitch-mode)


(defun soph/prettify-symbols-setup ()
  "Beautify keywords"
  (setq prettify-symbols-alist
		(mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
				'(; Greek symbols
				  ("lambda" . ?λ)
				  ("delta"  . ?Δ)
				  ("gamma"  . ?Γ)
				  ("phi"    . ?φ)
				  ("psi"    . ?ψ)
				  ; Org headers
				  ("#+title:"  . "")
				  ("#+author:" . "")
                                  ("#+date:"   . "")
				  ; Checkboxes
				  ("[ ]" . "")
				  ("[X]" . "")
				  ("[-]" . "")
				  ; Blocks
				  ("#+begin_src"   . "") ; 
				  ("#+end_src"     . "")
				  ("#+begin_QUOTE" . "‟")
				  ("#+begin_QUOTE" . "”")
				  ; Drawers
				  ;    ⚙️
				  (":properties:" . "")
				  ; Agenda scheduling
				  ("SCHEDULED:"   . "🕘")
				  ("DEADLINE:"    . "⏰")
				  ; Agenda tags  
				  (":@projects:"  . "☕")
				  (":work:"       . "🚀")
				  (":@inbox:"     . "✉️")
				  (":goal:"       . "🎯")
				  (":task:"       . "📋")
				  (":@thesis:"    . "📝")
				  (":thesis:"     . "📝")
				  (":uio:"        . "🏛️")
				  (":emacs:"      . "")
				  (":learn:"      . "🌱")
				  (":code:"       . "💻")
				  (":fix:"        . "🛠️")
				  (":bug:"        . "🚩")
				  (":read:"       . "📚")
				  ; Roam tags
				  ("#+filetags:"  . "📎")
				  (":wip:"        . "🏗️")
				  (":ct:"         . "➡️") ; Category Theory
                                  ; ETC
                                  (":verb:"       . "🌐") ; HTTP Requests in Org mode
				  )))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook        #'soph/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'soph/prettify-symbols-setup)

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
                        `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
                           (1 `(face nil
                                     display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
                              prepend))) t)

(use-package org-modern
  ;;;:load-path ("~/.emacs.d/addons/org-modern")
  :ensure t
  :config
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 org-modern-fold-stars
  '(("☯" . "☯") ("♥" . "♥") ("♣" . "♣") ("▹" . "▿") ("▸" . "▾"))

 ;; Org styling, hide markup etc.
 ;;org-modern-tag nil
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis ".")
 (global-org-modern-mode))
