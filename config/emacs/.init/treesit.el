(use-package treesit
      :mode (
             ("\\.go\\'"  . go-ts-mode)
             ("\\.html\\'"  . html-ts-mode)
             ;("\\.ngx.html\\'"  . ngxhtml-ts-mode)
             ("go\\.mod\\'"  . go-mod-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ("\\.yaml\\'" . yaml-ts-mode)
             ;;("\\.el\\'" . elisp-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '(
                  ; (css "https://github.com/tree-sitter/tree-sitter-css")
                  ; (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
                  (angular . ("https://github.com/dlvandenberg/tree-sitter-angular" "v0.5.3"))
                  ;(javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                   (json "https://github.com/tree-sitter/tree-sitter-json")
                  ; (python "https://github.com/tree-sitter/tree-sitter-python")
                   (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                   (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))
                  ; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                  ; (make "https://github.com/alemuller/tree-sitter-make")
                  ; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                  ; (cmake "https://github.com/uyha/tree-sitter-cmake")
                  ; (c "https://github.com/tree-sitter/tree-sitter-c")
                  ; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                  ; (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                  ; (prisma "https://github.com/victorhqc/tree-sitter-prisma")
                 ))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      :config
      (setq treesit-font-lock-level 4)
      (os/setup-install-grammars)
)

