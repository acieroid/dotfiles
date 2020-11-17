;;; -*- mode: emacs-lisp -*-
;;; vim: ft=lisp

;;; Code:

;; Languages that I currently use
;; possible values: common-lisp, clojure, parenscript, scheme, ocaml, c, arc, elisp, haskell, idris, factor, python, lua, go, scala, coq, cmake, erlang, agda
;; (defvar *languages* '(scala ocaml))
;; (defmacro with-language (language &rest body)
;;   `(when (member ,language *languages*)
;;      ,@body))
;; (put 'with-language 'lisp-indent-function 1)
;; 
;; (defun add-to-mode-alist (ext mode)
;;   "Add an extension/mode pair to auto-mode-alist."
;;   (setq auto-mode-alist (append (list (cons ext mode)) auto-mode-alist)))
;; 
(defun find-executable (name)
  "Find the full path of a program NAME."
  (shell-command-to-string (format "which '%s' | tr -d '\n'" name)))
;; 
;; (defcustom emacs-personal-dir (concat (getenv "HOME") "/.emacs.d/")
;;   "The path to a directory that contains things useful to emacs.")
;; (defun in-personal-dir (dir)
;;   "Give the path of a file/directory in the EMACS-PERSONAL-DIR."
;;   (concat emacs-personal-dir dir))

;;;; Appearance stuff

;;; Theme
;; Doom theme, with its modeline
(use-package doom-themes
  :init (load-theme 'doom-vibrant t))

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(defun light-theme ()
  "Load a light theme."
  (interactive)
  (disable-theme 'doom-one-dark)
  (load-theme 'doom-one-light t))

(defun dark-theme ()
  "Load a dark theme."
  (interactive)
  (disable-theme 'doom-one-light)
  (load-theme 'doom-one-dark t))

;;; Initial scratch mesage disabled
(setq-default initial-scratch-message nil)

;;; Fonts
(defvar default-font-size 120)
(defvar default-variable-font-size 120)
(set-face-attribute 'default nil :font "Fira Code Retina" :height default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height default-variable-font-size :weight 'regular)

;;; Font
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:height 120 :family "Hack"))))
;;  '(diff-added ((t (:foreground "forestgreen"))))
;;  '(diff-context ((t (:foreground "grey20"))))
;;  '(diff-removed ((t (:foreground "IndianRed"))))
;;  '(merlin-locked-face ((t (:background "gray95"))))
;;  '(quack-pltish-defn-face ((t (:foreground "green" :weight bold))) t)
;;  '(whitespace-line ((t (:background "red4" :weight bold))))
;;  '(whitespace-tab ((t (:background "grey20")))))
;; ; (add-to-list 'default-frame-alist '(font "DejaVu Sans Mono-16"))
;; ; (set-face-attribute 'default nil :height 160)

;; Show column number in the mode line
(column-number-mode)

;; Show line numbers on the left (except in some modes)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; UTF-8
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;;; Disable toolbar/menubar/scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; Visible bell
(setq visible-bell t)

;;; Non-blinking, box cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'box)

;;; Don't show the region (C-SPC-SPC to see it)
(transient-mark-mode 0)

;;; Replace "yes-or-no" by "y-or-n"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable useless startup message
(setq-default inhibit-startup-message t)

;;; Highlight trailing whitespaces, and tabs, but not characters past 80 columns
;;; (add lines-tails for this)
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode t)

;;; Colored output in M-x shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;; File-system related stuff
;;; Saves all backup files in one dir
(setq backup-by-copying t
      backup-directory-alist `(("." . "~/.emacs.d/backups/"))
      delete-old-versions t)

;;; Follow symlinks
(setq vc-follow-symlinks t)

;;; URLs
(setq browse-url-browser-function 'browse-url-chromium
      browse-url-chromium-program "brave"
      browse-url-new-window-flag t)

;;; Move between windows using shift and arrows or M-{c,t,s,r}
(windmove-default-keybindings)
(defvar win-minor-mode-map (make-keymap) "Keymap for win-minor-mode.")
(define-key win-minor-mode-map (kbd "M-c") 'windmove-left)
(define-key win-minor-mode-map (kbd "M-r") 'windmove-right)
(define-key win-minor-mode-map (kbd "M-s") 'windmove-up)
(define-key win-minor-mode-map (kbd "M-t") 'windmove-down)
(define-minor-mode win-minor-mode
  "Minor mode to move between emacs' windows"
  t " win-minor-mode" 'win-minor-mode-map)
(win-minor-mode 1)

;; Disable killing emacs by C-x C-c...
(global-set-key (kbd "C-x C-c") 'keyboard-quit)

;; ...but we don't want to M-x save-buffers-kill-terminal to quit, M-x
;; quit is simpler
(defun quit ()
  "Shortcut to 'save-buffers-kill-terminal."
  (interactive)
  (save-buffers-kill-terminal))

;;; PDF handling
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;;; Edit-mode improvements
;;; Auto-fill set to 80 columns
(auto-fill-mode)
(setq-default fill-column 80)
;; but disable it in latex-mode
(add-hook 'latex-mode-hook (lambda () (auto-fill-mode -1)))
;; and enable visual-line-mode
(add-hook 'latex-mode-hook (lambda () (visual-line-mode 1)))

;;; Disable tabs
(setq-default indent-tabs-mode nil)

;;; Some keybindings
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-,") 'flyspell-goto-next-error)

;; Line joining (a la :join in vim), because M-^ works in the reversed way
(defun join-lines (arg)
  "Join the current line with  the next.  Ignore ARG."
  (interactive "p")
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " ")
  (backward-char 1))
(global-set-key (kbd "M-j") 'join-lines)

;; Typing M-x ansi-term zsh is long, and we only one one terminal running
(defun terminal ()
  "Open a new terminal or use the existing one."
  (interactive)
  (let ((term-buf (get-buffer "*term*")))
    (if term-buf
        (pop-to-buffer term-buf)
      (vterm "*term*"))))
(global-set-key (kbd "<f1>") 'terminal)

;;; Parenthese-related stuff
(use-package paren
  :config
  (show-paren-mode +1)
  (setq blink-matching-paren nil))


(defun goto-match-paren (arg)
  "Go to the matching parenthesis.  Ignore ARG."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char
                                                1))
        ((looking-at "\\s\)") (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-%") 'goto-match-paren)

;;; Hideshow (folding)
(global-unset-key (kbd "C-c h"))
(global-set-key (kbd "C-c h") (lambda ()
                                (interactive)
                                (hs-minor-mode 1)
                                (hs-hide-block)))
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (hs-minor-mode 1)
                                (hs-show-block)))

;;; X selection
;; Since emacs 24, the default behavior for selection changed, which really
;; confuses me. This enable the old behavior
; (setq select-enable-clipboard nil)
; (setq select-enable-primary t)
; (setq mouse-drag-copy-region t)

;;;; Packages/extensions
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; ido -- in emacs by default
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-default-buffer-method 'selected-window)
;; (ido-mode 0)

;; Helm, see https://tuhdo.github.io/helm-intro.html
(use-package helm
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-C C-f" . helm-projectile-find-file)
         ("C-x b" . helm-mini)))

;;;; OS X specific
(when (eq system-type 'darwin)
  ;; Left command is meta, right command is altgr (modified by karabiner to be
  ;; option)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t
        magic-mode-alist nil)
  (when (boundp 'tabbar-mode)
    (tabbar-mode 0))
  (set-face-attribute 'default nil :height 140))

;;; Highlight some keywords
(use-package fixme-mode
  :config (fixme-mode t))

;;; Zenburn as color theme
;; (global-set-key (kbd "<f6>") '(lambda ()
;;                                 (interactive)
;;                                 (custom-set-faces
;;                                   '(merlin-locked-face ((t (:background "gray95")))))
;;                                 (disable-theme 'spacemacs-light)))
;; (global-set-key (kbd "<f7>") '(lambda ()
;;                                 (interactive)
;;                                 ;(custom-set-faces
;;                                         ; '(merlin-locked-face ((t (:background "gray20")))))
;;                                 (load-theme 'spacemacs-light t)))
; (setq custom-safe-themes (cons 'spacemacs-dark custom-safe-themes))
;; (load-theme 'spacemacs-dark t)
;(with-package 'powerline
;(powerline-default-theme))
;; There can be style issues with the powerline when changing theme, to fix them: run powerline-reset

;;; Paredit
(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          eval-expression-minibuffer-setup
          scheme-mode
          geiser-repl-mode) . enable-paredit-mode)
  :config
  (progn
    ;; M-r clashes with my win-minor-mode setup, and I don't use M-r in paredit
    (define-key paredit-mode-map (kbd "M-r") nil)
    (with-eval-after-load "eldoc"
      (eldoc-add-command #'paredit-backward-delete #'paredit-close-round))))

;;;; Languages
;; (with-language 'common-lisp
;;   (with-package 'slime
;;     (setq inferior-lisp-program "sbcl")
;;     (setq slime-lisp-implementations
;;           `((sbcl ("sbcl") :coding-system utf-8-unix)
;;             (clisp ("clisp") :coding-system utf-8-unix)
;;             (ecl ("ecl") :coding-system utf-8-unix)))
;;     (setq slime-net-coding-system 'utf-8-unix)
;; 
;;     (autoload 'slime-setup "slime" "Slime")
;;     ;; TODO: broken
;;     (slime-setup '(slime-repl slime-c-p-c slime-editing-commands slime-asdf slime-scratch))
;; 
;;     (setq slime-complete-symbol-function 'slime-complete-symbol*)
;;     (add-hook 'slime-mode-hook
;;               (lambda ()
;;                 (unless (slime-connected-p)
;;                   (save-excursion (slime)))))
;;     (add-paredit-hook 'slime-mode-hook)
;;     (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))
;;     (add-hook 'slime-repl-mode-hook
;;               (lambda () (local-set-key (kbd "RET") 'paredit-newline)))
;; 
;;     (eval-after-load "slime"
;;       '(progn
;;          ;;(define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
;;          ;;(define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)
;;          (define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
;;          (define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))))
;; 
;; (with-language 'clojure
;;   (with-package 'clj-mode
;;     (autoload 'clj-mode "clj-mode" "Clojure Mode")))
;; 
;; (with-language 'parenscript
;;   (with-package 'slime
;;     ;; Probably broken
;;     (add-to-list 'load-path (in-personal-dir "slime-proxy"))
;;     (add-to-list 'load-path (in-personal-dir "slime-proxy/contrib/slime-parenscript"))
;;     (defun slime-proxy-setup ()
;;       (interactive)
;;       (slime-setup '(slime-proxy slime-parenscript)))))
;; 
;; (with-language 'scheme
;;   (with-package 'geiser
;;     (autoload 'geiser-mode "geiser" "Scheme Mode")
;;     (add-paredit-hook 'geiser-mode-hook)
;;     (setq geiser-racket-binary (find-executable "racket")))
;;   ;; Alternative, simpler-way (without REPL interaction):
;;   ;; (require 'scheme)
;;   )

(use-package tuareg
  :config
  ; (add-hook 'tuareg-mode-hook #'electric-pair-local-mode) ;; No, I despise that :(
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-c C-f"))
              (local-set-key (kbd "M-q") 'tuareg-indent-phrase)
              (local-set-key (kbd "C-,") 'merlin-error-next)
              (local-set-key (kbd "M-.") 'merlin-locate)
              (local-set-key (kbd "M-,") 'merlin-pop-stack)))
  ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist)))

(use-package ocp-indent
  :config
  (setq ocp-indent-path (find-executable "ocp-indent")))

(use-package merlin
  :config
  ;; Use opam switch to lookup ocamlmerlin binary
  ;; (setq merlin-command 'opam)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  ;; (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save t))

;; (with-language 'c
;;   (defun bind-compile-program ()
;;     (interactive)
;;     (local-set-key (kbd "C-c C-c")
;;                    (lambda ()
;;                      (interactive)
;;                      (compile "make -k"))))
;;   (add-hook 'c-mode-hook 'bind-compile-program)
;;   (add-hook 'c++-mode-hook 'bind-compile-program)
;; 
;;   (setq-default c-default-style "linux"
;;                 c-basic-offset 4)
;; 
;;   ;;; Auto-indent (to add to hooks)
;; (defun set-newline-and-indent ()
;;   (local-set-key (kbd "RET") 'newline-and-indent))
;; 
;; (add-hook 'c-mode-common-hook 'set-newline-and-indent)
;; 
;;   ;; cscope
;;   (autoload 'xcscope "xcscope" "cscope")
;;   (setq cscope-do-not-update-database nil))
;; 
;; (with-language 'elisp
;;   (add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)
;;   (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
;; 
;; (with-language 'haskell
;;   (with-package 'haskell-mode
;;     (autoload 'haskell-mode "haskell-mode" "Haskell mode" t)
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
;; 
;; (with-language 'idris
;;   (with-package 'idris-mode))
;; 
;; (with-language 'factor
;;   (with-package 'fuel
;;     (autoload 'factor-mode "factor-mode" "Factor mode")))
;; 
;; (with-language 'python
;;   (when (executable-find "python")
;;       (setq python-python-command "python")))
;; 
;; (with-language 'lua
;;   (with-package 'lua-mode
;;     (autoload 'lua-mode "lua-mode" "Lua mode")))
;; 
;; (with-language 'go
;;   (with-package 'go-mode
;;     (autoload 'go-mode "go-mode" "Go mode")))
;; 
;; (with-language 'scala
;;   (with-package 'scala-mode
;;     (autoload 'scala-mode "scala-mode" "Scala mode")))
;; 
;; (with-language 'coq
;;   ;; TODO: Probably broken on non-linux
;;   (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
;;   (autoload 'proof-general "proof-general" "Proof General")
;;   (setq proof-splash-enable nil
;;         proof-electric-terminator-enable t))
;; 
;; (with-language 'cmake
;;   (with-package 'cmake-mode
;;     (autoload 'cmake-mode "cmake-mode" "CMake mode")))
;; 
;; (with-language 'erlang
;;   (with-package 'erlang
;;     (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.10/emacs/")
;;     (setq erlang-root-dir "/usr/lib/erlang")
;;     ;; No autoload as it is erlang-start that contains the autoload
;;     (require 'erlang-start)))
;; 
;; 
;; (with-language 'agda
;;   ;; First, run cabal install agda, and have ~/.cabal/bin in the PATH
;;   (load-file (let ((coding-system-for-read 'utf-8))
;;                (shell-command-to-string "agda-mode locate")))
;;   (mapc
;;          (lambda (x) (add-to-list 'face-remapping-alist x))
;;          '((agda2-highlight-datatype-face              . font-lock-type-face)
;;            (agda2-highlight-function-face              . font-lock-type-face)
;;            (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
;;            (agda2-highlight-keyword-face               . font-lock-keyword-face)
;;            (agda2-highlight-module-face                . font-lock-constant-face)
;;            (agda2-highlight-number-face                . nil)
;;            (agda2-highlight-postulate-face             . font-lock-type-face)
;;            (agda2-highlight-primitive-type-face        . font-lock-type-face)
;;            (agda2-highlight-record-face . font-lock-type-face))))
;; 
;; (use-package rust-mode
;;   :hook (rust-mode . lsp)
;;   :bind
;;   ("C-c g" . rust-run)
;;   ("C-c t" . rust-test)
;;   ("C-c b" . cargo-process-build)
;;   :init
;;   (which-function-mode 1)
;;   (setq compilation-error-regexp-alist-alist
;;       (cons '(cargo "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([0-9]+\\):\\([0-9]+\\) \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):" 1 (2 . 4) (3 . 5) (6))
;;         compilation-error-regexp-alist-alist))
;;   :config
;;   (setq rust-format-on-save t))

;;; Org mode
(require 'org)
(setq org-html-style-include-scripts nil)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
; (add-hook 'org-mode-hook (lambda () (fci-mode 0)))
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-files
      (append
       (file-expand-wildcards "~/notes/*.org")
       (file-expand-wildcards "~/notes/research/*.org")
       (file-expand-wildcards "~/notes/books/*.org")
       (file-expand-wildcards "~/notes/papers/*.org")))
(setq org-fast-tag-selection-single-key 'expert)
(setq org-fast-tag-selection-include-todo t)
(setq org-use-fast-todo-selection t)
(setq org-agenda-start-on-weekday 0)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "INACTIVE(i)""|" "CANCELLED(c@/!) MEETING(m)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "orange" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("INACTIVE" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/notes")
(setq org-default-notes-file "~/notes/refile.org")

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/notes/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/notes/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/notes/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/notes/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/notes/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/notes/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/notes/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/notes/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-clock-history-length 23)
(setq org-clock-in-resume t)
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-agenda-custom-commands
      (quote (("w" "Work tasks"
               ((agenda "" nil)
                (todo "NEXT")
                (todo "WAITING"))))))

(setq org-clock-mode-line-total 'today)
(setq org-refile-use-outline-path 'file)


(use-package org-journal
    :custom
    (org-journal-dir "~/notes/journal/2020/")
    (org-journal-file-format "%Y%m%d")
    (org-journal-date-format "%e %b %Y (%A)")
    (org-journal-time-format "")
    :preface)

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun my-electric-dot ()
  (interactive)
  (insert ".\n"))
(defun my-tex-hook ()
  (local-set-key (kbd ".") 'my-electric-dot))
(add-hook 'latex-mode-hook 'my-tex-hook)


(use-package projectile
  :diminish projectile-mode
  :config (progn
            (projectile-mode)
            (helm-projectile-on))
  :custom ((projectile-completion-system 'ivy))
  :bind
  (("C-c p" . projectile-command-map))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path '("~/p" "~/f"))
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action #'projectile-dired))

(global-set-key (kbd "C-c C-f") 'helm-projectile)
(global-set-key (kbd "C-c C-g") 'helm-projectile-grep)

(use-package flycheck
  :defer 2
  :diminish
  :init (global-flycheck-mode)
  :bind (("C-," . flycheck-next-error))
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-stylelintrc "~/.stylelintrc.json"))

(server-start)

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1)
;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)
;;   ;; formats the buffer before saving
;;   ;; (add-hook 'before-save-hook 'tide-format-before-save)
;;   
;; 
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(defun setup-tide-mode ()
  "Setup tide mode for typescript development."
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :config
  (progn
    (define-key tide-mode-map (kbd "C-,") 'flycheck-next-error)
    (define-key tide-mode-map (kbd "C-.") 'tide-fix)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    (add-hook 'rjsx-mode-hook #'setup-tide-mode)))

(defun tide-annotate-completions (completions prefix file-location)
  "Annotate tide completions."
  (-map
   (lambda (completion)
     (let ((name (plist-get completion :name)))
       (put-text-property 0 1 'file-location file-location name)
       (put-text-property 0 1 'completion completion name)
       name))
   (-sort
    'tide-compare-completions
    (-filter
     (let ((member-p (tide-member-completion-p prefix)))
       (lambda (completion)
         (and (string-prefix-p prefix (plist-get completion :name))
              (or (not member-p)
                  (member (plist-get completion :kind) '("warning" "export" "method" "property" "getter" "setter"))))))
     completions))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;; To sort
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "8e959d5a6771b4d1e2177263e1c1e62c62c0f848b265e9db46f18754ea1c1998" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default))
 '(helm-completion-style 'emacs))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
