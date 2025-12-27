;;; Setup ---------------------------------------------------------
;;; ---- Prerequisites --------------------------------------------
;;; ---- Git Tracking & Practicalities ----------------------------
;;; ---- Lexical Binding ------------------------------------------
;;; ---- Tangling -------------------------------------------------

;;; Start-up ------------------------------------------------------
;;; ---- custom file folde ----------------------------------------
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; ---- package manager ------------------------------------------
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(setq package-archives
  '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
    ("MELPA"        . "https://melpa.org/packages/")
    ("ORG"          . "https://orgmode.org/elpa/")
    ("MELPA Stable" . "https://stable.melpa.org/packages/")
    ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
  package-archive-priorities
    '(("GNU ELPA"     . 20)
      ("MELPA"        . 15)
      ("ORG"          . 10)
      ("MELPA Stable" . 5)
      ("nongnu"       . 0)))
(package-initialize)
;;; ---- garbage collection ---------------------------------------
;;; ---- optimisation ---------------------------------------------

;;; houskeeping ---------------------------------------------------
;;; ---- building options -----------------------------------------
;;; ---- GPG keys -------------------------------------------------
;;; ---- local files ----------------------------------------------
;;; ---- terminal setup -------------------------------------------

;;; custom keybinding ---------------------------------------------
;;; ---- unbind some default keys ---------------------------------
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-z"))
;;; ---- custom keymap --------------------------------------------
;;; ---- macos ----------------------------------------------------
;;; ---- windows --------------------------------------------------

;;; visuals -------------------------------------------------------
(use-package olivetti
  :ensure t
)
;;; ---- declutting -----------------------------------------------
;;; ---- frames & window ------------------------------------------
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(modify-all-frames-parameters
 '((right-divider-width . 3)
   (internal-border-width . 16)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
(when (eq system-type 'darwin)
  ; no title bar
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  ; don't use proxy icon
  (setq ns-use-proxy-icon nil)
  ; don't show buffer name in title bar
  (setq frame-title-format ""))
(when (eq system-type 'gnu/linux)
  ; don't show buffer name in title bar
  (setq frame-title-format nil)
  ; no title bar
  (add-to-list 'default-frame-alist '(undecorated . t))
  ; add small border to enable drag/resize
  (add-to-list 'default-frame-alist '(drag-internal-border . 1)))
(when (eq system-type 'windows-nt)
  ; no title bar
  (add-to-list 'default-frame-alist '(undecorated . t))
  ; don't use proxy icon
  (setq ns-use-proxy-icon nil)
  ; don't show buffer name in title bar
  (setq frame-title-format ""))
(blink-cursor-mode        0)
(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows          nil)
;;; ---- programming ----------------------------------------------
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)
(column-number-mode t) ;; Show current column number in mode line
;;; ---- fonts ----------------------------------------------------
;;; ---- icons & emojis -------------------------------------------
;;; ---- themes ---------------------------------------------------
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))
(load-theme 'doom-palenight t)
;;; ---- mode line ------------------------------------------------
(display-time)
;;; ---- text -----------------------------------------------------

;;; general editing -----------------------------------------------
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq large-file-warning-threshold (* 50 1024 1024))
(save-place-mode 1)
(electric-pair-mode 1) ;; auto close bracket insertion, auot pair
;;; ---- buit-in options ------------------------------------------
;;; ---- scrolling ------------------------------------------------
(setq scroll-margin 15) ; lines between point and top or bottom edge
(setq maximum-scroll-margin 0.5) ; Allows for a larger effective margin
(setq scroll-conservatively            101
      mouse-wheel-follow-mouse         't
      mouse-wheel-progressive-speed    nil
      ;; Scroll 1 line at a time, instead of default 5
      ;; Hold shift to scroll faster and meta to scroll very fast
      mouse-wheel-scroll-amount        '(1 ((shift) . 3) ((meta) . 6)))
(pixel-scroll-mode)
(pixel-scroll-precision-mode)
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll    nil)
;;; ---- indentation ----------------------------------------------
(setq-default
 ;;tab-width 2
 standard-indent 2
 indent-tabs-mode nil)
;;; ---- deleting instead of killing ------------------------------
;;; ---- browse kill ring -----------------------------------------
;;; ---- auto saving & backup & lock ------------------------------
(let ((backup-dir (expand-file-name "tmp/backups" user-emacs-directory))
      (auto-saves-dir (expand-file-name "tmp/autosaves" user-emacs-directory))
      (lock-file-dir (expand-file-name "tmp/locks" user-emacs-directory)))
  (dolist (dir (list backup-dir auto-saves-dir lock-file-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir)) ;;;backup into one flat dir
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        lock-file-name-transforms `((".*" ,lock-file-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      ;;; make sure hard link and creation date, owner, etc is preserved
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 1               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 3               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      )
;;; ---- move where i mean ----------------------------------------
;;; ---- text editing functions -----------------------------------
;;; ---- CRUX -----------------------------------------------------

;;; buffers & navigation ------------------------------------------
(use-package which-key
    :ensure t
    :config
    (which-key-mode))
;;; ---- killing buffers ------------------------------------------
;;; ---- splitting window -----------------------------------------
;;; ---- switch window --------------------------------------------
;;; ---- project management ---------------------------------------
;;; ---- ibuffer --------------------------------------------------
;;; ---- dired ----------------------------------------------------

;;; completon -----------------------------------------------------
;;; ---- vertico --------------------------------------------------
;;; ---- vertico posframe -----------------------------------------
;;; ---- corfu ----------------------------------------------------
;;; ---- componay -------------------------------------------------
;;; ---- orderless ------------------------------------------------

;;; misc. packages ------------------------------------------------
;;; ---- version control ------------------------------------------
;;; ---- trying packages ------------------------------------------
;;; ---- snippets -------------------------------------------------
;;; ---- better help buffers --------------------------------------
;;; ---- jinx spellchecker ----------------------------------------
;;; ---- LaTeX ----------------------------------------------------
;;; ---- PDF tools ------------------------------------------------
;;; ---- epubs ----------------------------------------------------
;;; ---- editor config --------------------------------------------
;;; ---- browser --------------------------------------------------
;;; ---- elfeed ---------------------------------------------------
;;; ---- config profiling -----------------------------------------

;;; activating custom keybindings ---------------------------------

;;; TODOS ---------------------------------------------------------

(mapc 'load (file-expand-wildcards (file-name-concat user-emacs-directory ".init/*.el")))
