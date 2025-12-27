;;; scss.el --- tree-sitter support for NGXHTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

;; Author     : cyf <theo@thornhill.no>
;; Maintainer : yf <theo@thornhill.no>
;; Created    : January 2025
;; Keywords   : ngxhtml languages tree-sitter

;; This file is not part of GNU Emacs.

;; This file is free software

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'lsp-mode)

(defgroup lsp-scss nil
  "LSP support for SCSS."
  :group 'lsp-mode
  :link '(url-link
          "https://github.com/microsoft/vscode/tree/main/extensions/css-language-features/server"))

(defcustom lsp-scss-completion-suggestFromUseOnly t
  "Enables or disables all validations."
  :group 'lsp-scss
  :type 'boolean)

(lsp-register-custom-settings
 '(("somesass.scss.completion.suggestFromUseOnly" lsp-scss-completion-suggestFromUseOnly)))

(defun my-scss-mode-hook ())

(add-hook 'scss-mode-hook #'my-scss-mode-hook)
(add-hook 'scss-mode-hook #'lsp-deferred)
;;;
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(scss-mode . "scss"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("some-sass-language-server"
                                                            "--stdio"))
                    :activation-fn (lsp-activate-on "scss")
                    :server-id 'scss-l)))

(setq css-indent-offset 2)

(provide 'scss)
;;; scss.el ends here
