;;;; -*- mode: emacs-lisp -*-
;;;; vim: ft=lisp
;;;; Useful functions
(defcustom emacs-personal-dir (concat (getenv "HOME") "/emacs/")
  "The path to a directory that contains things useful to emacs (slime,…)")
(defun in-personal-dir (dir)
  "Give the path of a file/directory in the EMACS-PERSONAL-DIR"
  (concat emacs-personal-dir dir))

;;;; Appearence
; Colors
(set-mouse-color "white")
(set-cursor-color "white")
(set-foreground-color "white")
(set-background-color "black")

; Font
(set-face-attribute 'default nil :height 120 :font "inconsolata")


; Show column number in modeline
; and line number on the left
(column-number-mode t)
(line-number-mode -1)
(require 'linum)
(setq linum-format "%2d ")
(global-linum-mode t)

; UTF-8
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

; Disable toolbar/menubar/scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; Non-blinking cursor
(blink-cursor-mode -1)

; Show corresponding paren
(require 'paren)
(unless (show-paren-mode)
  (show-paren-mode))
  
(setq blink-matching-paren nil)

; Replace "yes-or-no" by "y-or-n"
(fset 'yes-or-no-p 'y-or-n-p)

; Disable useless startup message
(setq inhibit-startup-message t)

; Saves all backup files in one dir
(add-to-list 'backup-directory-alist (cons ".*" (in-personal-dir "backups/")))
;(defun make-auto-save-file-name ()
;  (concat (in-personal-dir "backups/") buffer-file-name))
;(defun auto-save-file-name-p (filename)
;  (string-match (concat "^" emacs-personal-dir ".*")))

; Auto-fill set to 80 columns
(auto-fill-mode)
(setq fill-colum 80)

; Slime
(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      `((sbcl ("sbcl"))
        (clisp ("clisp"))))
(add-to-list 'load-path (in-personal-dir "slime/"))
(require 'slime-autoloads)
(slime-setup '(slime-repl slime-c-p-c slime-editing-commands slime-asdf))

(setq slime-complete-symbol-function 'slime-complete-symbol*)
(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))

; Auto-indent in some modes
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-common-hook 'set-newline-and-indent)
(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)

; Some keybindings :
(global-set-key "\C-h" 'delete-backward-char)
(global-unset-key "\C-z")
(global-unset-key "\C-x \C-z")

; uzbl as browser
;; (defun browse-url-uzbl (url _)
;;   (interactive "sURL: ")
;;   (start-process (concat "uzbl " url) nil "uzbl" "--uri" url))
;; (setq browse-url-browser-function 'browse-url-uzbl)

; disable tabs
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path (in-personal-dir "elisp/"))
(require 'quack)

;(require 'paredit)
;(add-hook 'lisp-mode-hook 'paredit-mode)

(add-to-list 'load-path (in-personal-dir "tuareg/"))
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
;(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

; C mode
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-c \C-c" 'compile)))

(defun x-copy-region ()
  (interactive)
  (let* ((process-connection-type nil)
         (proc (start-process "xsel" nil "xsel" "-i")))
    (send-region proc (region-beginning) (region-end))
    (process-send-eof proc)))

; Parenface - color parens
(require 'highlight-parentheses)

; arc
(add-to-list 'load-path (in-personal-dir "arc/"))
(add-to-list 'auto-mode-alist '("\\.arc" . arc-mode))
(autoload 'arc-mode "inferior-arc" "Major mode for editing arc code" t)

; TODO
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char
                                                1))
        ((looking-at "\\s\)") (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg 1)))))
; Disable transient-mark
(when (transient-mark-mode)
  (transient-mark-mode))


(require 'mercurial)

