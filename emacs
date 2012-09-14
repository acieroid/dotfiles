;;;; -*- mode: emacs-lisp -*-
;;;; vim: ft=lisp
;;;; Useful functions
(defcustom emacs-personal-dir (concat (getenv "HOME") "/emacs/")
  "The path to a directory that contains things useful to emacs (slime,…)")
(defun in-personal-dir (dir)
  "Give the path of a file/directory in the EMACS-PERSONAL-DIR"
  (concat emacs-personal-dir dir))

(defun add-paredit-hook (hook)
  "Add cool paren stuff to a mode"
  (add-hook hook (lambda () (paredit-mode t)))
  (add-hook hook (lambda () (rainbow-delimiters-mode t)))
  (add-hook hook
            (lambda () (local-set-key (kbd "RET") 'paredit-newline))))

;;; Directory where all single .el files will be placed
(add-to-list 'load-path (in-personal-dir "elisp/"))

;;; Initial scratch mesage
(setq initial-scratch-message nil)

;;;; Appearence
;;; Zenburn as color theme
(autoload 'color-theme "color-theme" "Color Theme")
(autoload 'color-theme-zenburn "zenburn" "Zenburn Color Theme")
(color-theme-zenburn)
;(load-theme 'wombat)
;(require 'zenburn-theme)

;;; Font
(set-face-attribute 'default nil :height 90)

;;; Show column number in modeline
;;; and line number on the left
(column-number-mode t)
(line-number-mode -1)
(setq linum-format "%2d")
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

;;; Non-blinking cursor
(blink-cursor-mode 0)

;;; Don't show the region (C-SPC-SPC to see it)
(transient-mark-mode 0)

;;; Replace "yes-or-no" by "y-or-n"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable useless startup message
(setq inhibit-startup-message t)

;;; Saves all backup files in one dir
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(in-personal-dir "backups/")))
      delete-old-versions t)

;;; Auto-fill set to 80 columns
(auto-fill-mode)
(setq fill-column 80)

;;; Follow symlinks
(setq vc-follow-symlinks t)

;;; Parenthese stuff
(autoload 'paredit-mode "paredit" "Paredit mode")

(autoload 'show-paren-mode "paren" "Show-paren mode")
(show-paren-mode t)
(setq blink-matching-paren nil)

(require 'rainbow-delimiters)

(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char
                                                1))
        ((looking-at "\\s\)") (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-%") 'goto-match-paren)

;;; Auto-indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;;; Some keybindings
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "<f1>") 'search-forward-regexp)
(global-unset-key "\C-z")
(global-unset-key "\C-x \C-z")

;;; disable tabs
(setq-default indent-tabs-mode nil)

;;; Common Lisp
(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      `((sbcl ("sbcl") :coding-system utf-8-unix)
        (clisp ("clisp") :coding-system utf-8-unix)
        (ecl ("ecl") :coding-system utf-8-unix)))
(setq slime-net-coding-system 'utf-8-unix)
(add-to-list 'load-path (in-personal-dir "slime/"))
(autoload 'slime-setup "slime" "Slime")
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
     (define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
     (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)
     (define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
     (define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))

;; Parenscript
(add-to-list 'load-path (in-personal-dir "slime-proxy"))
(add-to-list 'load-path (in-personal-dir "slime-proxy/contrib/slime-parenscript"))

(defun slime-proxy-setup ()
  (interactive)
  (slime-setup '(slime-proxy slime-parenscript)))

;;; Scheme
(autoload 'scheme-mode "quack" "Scheme Mode")
(add-paredit-hook 'quack-mode-hook)
(custom-set-faces ;; Some quack default colors are awful
 '(quack-pltish-defn-face ((t (:foreground "green" :weight bold)))))


;;; Ocaml
(add-to-list 'load-path (in-personal-dir "tuareg/"))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; C and C++
(defun bind-compile-program ()
  (interactive)
  (local-set-key (kbd "C-c C-c")
                 (lambda ()
                   (interactive)
                   (compile "make -k"))))
(add-hook 'c-mode-hook 'bind-compile-program)
(add-hook 'c++-mode-hook 'bind-compile-program)

(add-hook 'c-mode-common-hook 'set-newline-and-indent)

;;; arc
(add-to-list 'load-path (in-personal-dir "arc/"))
(add-to-list 'auto-mode-alist '("\\.arc" . arc-mode))
(autoload 'arc-mode "inferior-arc" "Major mode for editing arc code" t)

;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; haskell mode
(add-to-list 'load-path (in-personal-dir "haskell-mode"))
(autoload 'haskell-mode "haskell-site-file" "Haskell mode" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; Copy the region
(defun x-copy-region ()
  (interactive)
  (let* ((process-connection-type nil)
         (proc (start-process "xsel" nil "xsel" "-i")))
    (send-region proc (region-beginning) (region-end))
    (process-send-eof proc)))

;;; lusty-explorer
(add-to-list 'load-path (in-personal-dir "lusty-emacs"))
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;;; Org mode
;(autoload 'org-mode "org" "Org Mode")
(require 'org)

;; See http://thread.gmane.org/gmane.emacs.orgmode/29347
(add-to-list 'org-modules 'org-timer)
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda () 
                                (org-timer-set-timer '(16))))


(setq org-todo-keywords '((sequence "TODO" "STARTED"
                                    "REREAD" "SUMMARIZED"
                                    "|" "DONE" "CANCELLED")))
(setq org-todo-keyword-faces
      '(("STARTED" . (:foreground "yellow" :weight bold))
        ("CANCELLED" . org-archived)))

;(require 'htmlize) ; For syntax highlighting TODO
(setq org-export-html-style-include-default nil)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-export-html-style
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: serif; font-size: 12pt; }
  body { width: 75%; margin-left: auto; margin-right: auto; }
  .title  { text-align: center; font-weight: normal; margin-top: 2.8em; font-size: 200%;}
  a { text-decoration: none; }
  a:hover { text-decoration: underline; }
  .author, .date, .creator { color: gray; font-style: italic; text-align: right; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  p.verse { margin-left: 3% }
  pre {
  background-color: #000000;
  color: #ffffff;
  padding: 5pt;
  font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>")
(require 'org-publish)
(setq org-publish-project-alist
      '(("cours-notes"
         :base-directory "~/cours/"
         :base-extension "org"
         :publishing-directory "~/cours-html/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :auto-preamble t
         )
        ("cours-static"
         :base-directory "~/cours/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|tar.bz2\\|tar.gz\\|m\\|cpp"
         :publishing-directory "~/cours-html/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("cours" :components ("cours-notes" "cours-static"))))

;;; URLs
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;;; Factor
(add-to-list 'load-path (in-personal-dir "fuel/"))
(setq fuel-factor-root-dir (in-personal-dir "fuel/"))
(autoload 'factor-mode "factor-mode" "Factor mode")

;;; Python
(setq python-python-command "python2")

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

;;; Highlight some keywords
(font-lock-add-keywords
 nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))

;;; Pretty-mode
;(require 'pretty-mode)
;(global-pretty-mode t)

;;; Flyspell
(setq ispell-dictionary "francais")
(setq flyspell-default-dictionary "francais")

;;; Lua
(autoload 'lua-mode "lua-mode" "Lua mode")

;;; Go
(autoload 'go-mode "go-mode" "Go mode")

;;; Modes
(setq auto-mode-alist
      (append
       '(("\\.arc" . arc-mode)
         ("\\.ml[iylp]?" . tuareg-mode)
         ("\\.hs" . haskell-mode)
         ("\\.scm" . scheme-mode)
         ("\\.py" . python-mode)
         ("\\.factor" . factor-mode)
         ("\\.lua$" . lua-mode)
         ("\\.m$" . octave-mode)
         ("\\.go$" . go-mode)
         (".stumpwmrc" . lisp-mode)
         ("SConstruct" . python-mode)
         ("SConscript" . python-mode)
         ("CMakeLists.txt" . cmake-mode)
         ("\\.scala" . scala-mode))
       auto-mode-alist))

;;; Some custom faces
(custom-set-faces
 '(diff-added ((t (:foreground "PaleGreen"))) 'now)
 '(diff-removed ((t (:foreground "IndianRed"))) 'now)
 '(diff-context ((t (:foreground "DimGray"))) 'now)
 )

;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
;(global-ede-mode 1)
;(semantic-load-enable-code-helpers)
;(global-srecode-minor-mode 1)

;;; cscope
(autoload 'xcscope "xcscope" "cscope")
(setq cscope-do-not-update-database nil)

;;; cmake
(autoload 'cmake-mode "cmake-mode" "CMake mode")

;;; Scala
(add-to-list 'load-path (in-personal-dir "scala-mode/"))
(autoload 'scala-mode "scala-mode" "Scala mode")

;;; ProofGeneral
(load-file (in-personal-dir "ProofGeneral/generic/proof-site.el"))

;;; X selection
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
