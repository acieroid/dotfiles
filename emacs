;;;; -*- mode: emacs-lisp -*-
;;;; vim: ft=lisp

;; Languages that I currently use
;; possible values: common-lisp, clojure, parenscript, scheme, ocaml, c, arc, elisp, haskell, idris, factor, python, lua, go, scala, coq, cmake, erlang, agda
(defvar *languages* '(clojure ocaml c elisp haskell))
(defmacro with-language (language &rest body)
  `(when (member ,language *languages*)
     ,@body))
(put 'with-language 'lisp-indent-function 1)

(defcustom emacs-personal-dir (concat (getenv "HOME") "/.emacs.d/")
  "The path to a directory that contains things useful to emacs")
(defun in-personal-dir (dir)
  "Give the path of a file/directory in the EMACS-PERSONAL-DIR"
  (concat emacs-personal-dir dir))

;;; Auto-indent (to add to hooks)
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;;;; Appearance stuff
;;; Initial scratch mesage
(setq-default initial-scratch-message nil)

;;; Font
(set-face-attribute 'default nil :height 90)

;;; Show column number in modeline
;;; and line number on the left
(column-number-mode t)
(line-number-mode t)
(setq-default linum-format "%2d")
(autoload 'global-linum-mode "linum" "Linum mode")
(global-linum-mode t)

;;; UTF-8
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;;; Disable toolbar/menubar/scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; Non-blinking, box cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)

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
(global-whitespace-mode nil)

;;; Colored output in M-x shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Some custom faces (some other are also defined when changing theme, see
;; below)
(custom-set-faces
 '(diff-added ((t (:foreground "forestgreen"))) t)
 '(diff-context ((t (:foreground "grey20"))) t)
 '(diff-removed ((t (:foreground "IndianRed"))) t)
 '(merlin-locked-face ((t (:background "gray95"))))
 '(quack-pltish-defn-face ((t (:foreground "green" :weight bold))) t)
 '(whitespace-line ((t (:background "red4" :weight bold))))
 '(whitespace-tab ((t (:background "snow2")))))

;;;; File-system related stuff
;;; Saves all backup files in one dir
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(in-personal-dir "backups/")))
      delete-old-versions t)

;;; Follow symlinks
(setq vc-follow-symlinks t)

;;; URLs
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;;; Move between windows using shift and arrows or M-{c,t,s,r}
(windmove-default-keybindings)
(defvar win-minor-mode-map (make-keymap) "win-minor-mode keymap.")

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
  (interactive)
  (save-buffers-kill-terminal))

;; Disable the awful doc-view-mode
;; TODO: doesn't seem to really disable it
(rassq-delete-all #'doc-view-mode auto-mode-alist)

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

;; Line joining (a la :join in vim), because M-^ works in the reversed way
(defun join-lines (arg)
  (interactive "p")
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " ")
  (backward-char 1))
(global-set-key (kbd "M-j") 'join-lines)

;; Typing M-x ansi-term zsh is long, and we only one one terminal running
(defun terminal ()
  (interactive)
  (let ((term-buf (get-buffer "*term*")))
    (if term-buf
        (pop-to-buffer term-buf)
      (ansi-term "/bin/zsh" "term"))))
(global-set-key (kbd "<f1>") 'terminal)

;;; Parenthese-related stuff
(autoload 'show-paren-mode "paren" "Show-paren mode")
(show-paren-mode t)
(setq blink-matching-paren nil)

(defun goto-match-paren (arg)
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
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

;;; Copy the region to X selection
;; Useful when working with emacs -nw on a X session
(defun x-copy-region ()
  (interactive)
  (let* ((process-connection-type nil)
         (proc (start-process "xsel" nil "xsel" "-i")))
    (send-region proc (region-beginning) (region-end))
    (process-send-eof proc)))

;;;; Packages/extensions
;;; Directory where all single .el files will be placed
(add-to-list 'load-path (in-personal-dir "elisp/"))

(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(defmacro with-package (package &rest body)
  `(if (package-installed-p ,package)
       (progn ,@body)
     (when (y-or-n-p (format "Package '%s' not installed. Install it?" ,package))
       (package-install ,package)
       ,@body)))
(put 'with-package 'lisp-indent-function 1)
(package-initialize)

;; ido -- in emacs by default
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-default-buffer-method 'selected-window)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; From here, almost everything is not "vanilla"-emacs ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Highlight some keywords
(with-package 'fixme-mode
  (require 'fixme-mode))

;;; 80-column indicator
(with-package 'fill-column-indicator
  (require 'fill-column-indicator)
  (define-globalized-minor-mode
    global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode t))

;;;; Appearence
;;; Zenburn as color theme
(with-package 'zenburn-theme)
(global-set-key (kbd "<f6>") '(lambda ()
                                (interactive)
                                (custom-set-faces
                                  '(merlin-locked-face ((t (:background "gray95")))))
                                (disable-theme 'zenburn)))
(global-set-key (kbd "<f7>") '(lambda ()
                                (interactive)
                                (custom-set-faces
                                 '(merlin-locked-face ((t (:background "gray20")))))
                                (load-theme 'zenburn t)))
;(load-theme 'wombat) ; Another nice dark theme

;;; Paredit
(with-package 'paredit
  (add-to-list 'packages-to-install 'paredit)
  (autoload 'paredit-mode "paredit" "Paredit emode")
  (eval-after-load "paredit"
    ;; M-r clashes with my win-minor-mode setup, and I don't use M-r in paredit
    '(define-key paredit-mode-map (kbd "M-r") nil)))

(defun add-paredit-hook (hook)
  "Add cool paren stuff to a mode"
  (add-hook hook (lambda () (paredit-mode t)))
  ;(add-hook hook (lambda () (rainbow-delimiters-mode t)))
  (add-hook hook
            (lambda () (local-set-key (kbd "RET") 'paredit-newline))))

;;;; Languages
(with-language 'common-lisp
  (with-package 'slime
    (setq inferior-lisp-program "sbcl")
    (setq slime-lisp-implementations
          `((sbcl ("sbcl") :coding-system utf-8-unix)
            (clisp ("clisp") :coding-system utf-8-unix)
            (ecl ("ecl") :coding-system utf-8-unix)))
    (setq slime-net-coding-system 'utf-8-unix)

    (add-to-list 'packages-to-install 'slime)
    (autoload 'slime-setup "slime" "Slime")
    ;; TODO: broken
    (slime-setup '(slime-repl slime-c-p-c slime-editing-commands slime-asdf slime-scratch))

    (setq slime-complete-symbol-function 'slime-complete-symbol*)
    (add-hook 'slime-mode-hook
              (lambda ()
                (unless (slime-connected-p)
                  (save-excursion (slime)))))
    (add-paredit-hook 'slime-mode-hook)
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))
    (add-hook 'slime-repl-mode-hook
              (lambda () (local-set-key (kbd "RET") 'paredit-newline)))

    (eval-after-load "slime"
      '(progn
         ;;(define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
         ;;(define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)
         (define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
         (define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))))

;; TODO: autoload instead of require
(with-language 'clojure
  (with-package 'clj-mode
    (require 'clj-mode)))

(with-language 'parenscript
  (with-package 'slime
    ;; Probably broken
    (add-to-list 'load-path (in-personal-dir "slime-proxy"))
    (add-to-list 'load-path (in-personal-dir "slime-proxy/contrib/slime-parenscript"))
    (defun slime-proxy-setup ()
      (interactive)
      (slime-setup '(slime-proxy slime-parenscript)))))

(with-language 'scheme
  (with-package 'quack
    (autoload 'scheme-mode "quack" "Scheme Mode")
    (add-paredit-hook 'quack-mode-hook))
  ;; Alternative, simpler-way (without REPL interaction):
  ;; (require 'scheme)
  )

(with-language 'ocaml
  (with-package 'tuareg
    (autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" t)
    ;; Installed through opam instead of emacs's packages
    (add-to-list 'load-path
                 (concat
                  (replace-regexp-in-string "\n$" ""
                                            (shell-command-to-string "opam config var share"))
                  "/emacs/site-lisp"))
    ;; TODO: autoload
    (require 'ocp-indent)
    (require 'merlin)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-use-auto-complete-mode 'easy)
    (setq merlin-command 'opam)
    (setq merlin-display-lock-zone '(margin highlight))
    (add-hook 'caml-mode-hook
              (lambda ()
                (local-set-key (kbd "M-q") 'caml-indent-phrase)
                (local-set-key (kbd "C-x `") 'merlin-error-next)
                (local-set-key (kbd "M-.") 'merlin-locate)
                (local-set-key (kbd "M-,") 'merlin-pop-stack)))
    (add-hook 'tuareg-mode-hook
              (lambda ()
                (local-set-key (kbd "M-q") 'tuareg-indent-phrase)
                (local-set-key (kbd "C-x `") 'merlin-error-next)
                (local-set-key (kbd "M-.") 'merlin-locate)
                (local-set-key (kbd "M-,") 'merlin-pop-stack)))))

(with-language 'c
  (defun bind-compile-program ()
    (interactive)
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (compile "make -k"))))
  (add-hook 'c-mode-hook 'bind-compile-program)
  (add-hook 'c++-mode-hook 'bind-compile-program)

  (setq-default c-default-style "linux"
                c-basic-offset 4)

  (add-hook 'c-mode-common-hook 'set-newline-and-indent)

  ;; cscope
  (autoload 'xcscope "xcscope" "cscope")
  (setq cscope-do-not-update-database nil))

(with-language 'arc
  (add-to-list 'load-path (in-personal-dir "arc/"))
  (add-to-list 'auto-mode-alist '("\\.arc" . arc-mode))
  (autoload 'arc-mode "inferior-arc" "Major mode for editing arc code" t))

(with-language 'elisp
  (add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(with-language 'haskell
  (with-package 'haskell-mode
    (add-to-list 'packages-to-install 'haskell-mode)
    (autoload 'haskell-mode "haskell-mode" "Haskell mode" t)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

(with-language 'idris
  (with-package 'idris-mode))

(with-language 'factor
  (with-package 'fuel
    (autoload 'factor-mode "factor-mode" "Factor mode")))

(with-language 'python
  (when (executable-find "python2") ; else it's probably just "python"
      (setq python-python-command "python2")))

(with-language 'lua
  (with-package 'lua-mode
    (autoload 'lua-mode "lua-mode" "Lua mode")))

(with-language 'go
  (with-package 'go-mode
    (autoload 'go-mode "go-mode" "Go mode")))

(with-language 'scala
  (with-package 'scala-mode
    (autoload 'scala-mode "scala-mode" "Scala mode")))

(with-language 'coq
  ;; TODO: Probably broken on non-linux
  (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
  (setq proof-splash-enable nil
        proof-electric-terminator-enable t))

(with-language 'cmake
  (with-package 'cmake-mode
    (autoload 'cmake-mode "cmake-mode" "CMake mode")))

(with-language 'erlang
  (with-package 'erlang
    (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.10/emacs/")
    (setq erlang-root-dir "/usr/lib/erlang")
    ;; TODO: autoload
    (require 'erlang-start)))

(with-language 'agda
  ;; First, run cabal install agda, and have ~/.cabal/bin in the PATH
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;;; Org mode
(require 'org)
(require 'htmlize) ; For syntax highlighting in html output
(setq org-html-style-include-scripts nil)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
(add-hook 'org-mode-hook (lambda () (fci-mode 0)))
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(setq org-html-preamble nil)
(setq org-html-postamble t)
(setq org-html-postamble-format
      '(("en" "<p class=\"date\">Given on: %d</p>
<p class=\"date\">Last update: %T</p>
<p class=\"author\">Author: %a</p>")))

(setq org-publish-project-alist
      '(("soft"
         :base-directory "~/soft/"
         :publishing-directory "~/public_html"
         :section-numbers t
         :with-toc nil
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-postamble (lambda (plist) (format "<p>Last update: %s%s</p>"
                                                 (format-time-string "%Y-%m-%dT%T")
                                                 ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                                                  (format-time-string "%z"))))
         :recursive t
         :section-numbers t
         )))

;; Magit
(with-package 'magit)

;;;; Modes
(setq auto-mode-alist
      (append
       '(("\\.arc$" . arc-mode)
         ;("\\.ml[iylp]?" . tuareg-mode)
         ("\\.hs$" . haskell-mode)
         ("\\.scm$" . scheme-mode)
         ("\\.py$" . python-mode)
         ("\\.factor$" . factor-mode)
         ("\\.lua$" . lua-mode)
         ("\\.m$" . octave-mode)
         ("\\.go$" . go-mode)
         ("\\.ml[iylp]?$" . caml-mode)
         (".stumpwmrc$" . lisp-mode)
         ("SConstruct$" . python-mode)
         ("SConscript$" . python-mode)
         ("CMakeLists.txt$" . cmake-mode)
         ("\\.scala$" . scala-mode))
       auto-mode-alist))
