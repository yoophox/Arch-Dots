(use-package company
  :ensure t
  ;; Optional customizations
  :custom
  (company-minimum-prefix-length 2)                 ;; Allows cycling through candidates
  (company-idle-delay 0)                  ;; Enable auto completion
  (company-inhibit-inside-symbols t)            ;; No delay for completion
  
  :init
  (global-company-mode 1))

;;(add-to-list 'company-backends 'company-yasnippet)
;;;(setq company-backends '(company-bbdb company-semantic company-cmake (company-yasnippet company-capf :separate)
;;;              company-clang company-files
;;;              (company-dabbrev-code company-gtags company-etags
;;;               company-keywords)
;;;              company-oddmuse company-dabbrev))

(setcar (member 'company-capf company-backends) '(company-yasnippet company-capf :separate))

(defun go-hooks-for-company()
   (setq-local company-backends
              '((company-capf company-dabbrev-code company-yasnippet))))
;;(add-hook 'go-ts-mode-hook #'go-hooks-for-company)
