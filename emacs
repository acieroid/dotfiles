;;; -*- mode: emacs-lisp -*-
;;; vim: ft=lisp
;;; Code:

(defun find-executable (name)
  "Find the full path of a program NAME."
  (shell-command-to-string (format "which '%s' | tr -d '\n'" name)))

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
(defvar default-font-size 150)
(defvar default-variable-font-size 150)
(set-face-attribute 'default nil :font "Fira Code Retina" :height default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height default-font-size)

;; Show column number in the mode line
(column-number-mode)

;; Show line numbers on the left (except in some modes)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                pdf-view-mode))
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

;;; Don't show the region (C-SPC-SPC to see it) -> show it now :)
(transient-mark-mode 1)

;;; Replace "yes-or-no" by "y-or-n"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable useless startup message
(setq-default inhibit-startup-message t)

;;; Highlight trailing whitespaces, and tabs, but not characters past 80 columns
;;; (add lines-tails for this)
(use-package whitespace
  :init
  (global-whitespace-mode t)
  :config
  (progn (setq whitespace-style '(face tabs empty trailing))
         (set-face-attribute 'whitespace-tab nil :background "grey18")
         (set-face-attribute 'whitespace-line nil :background "red4")))

;;; Colored output in M-x shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;; File-system related stuff
;;; Saves all backup files in one dir
(setq temporary-file-directory "/home/quentin/.emacs.d/backups/")
(setq backup-by-copying t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t)

;; Dno't create lockfiles, these have a tendency to break some other applications
(setq create-lockfiles nil)

;;; Follow symlinks
(setq vc-follow-symlinks t)

;;; URLs
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program "firefox"
      browse-url-chromium-program "chromium"
      browse-url-new-window-flag nil)

;;; Move between windows using shift and arrows or M-{c,t,s,r}
(windmove-default-keybindings)
(define-minor-mode win-minor-mode
  "Minor mode to move between emacs' windows"
  :global t
  :lighter " win-minor-mode"
  :keymap (let ((keymap (make-keymap)))
            (define-key keymap (kbd "M-c") 'windmove-left)
            (define-key keymap (kbd "M-r") 'windmove-right)
            (define-key keymap (kbd "M-s") 'windmove-up)
            (define-key keymap (kbd "M-t") 'windmove-down)
            keymap))
(win-minor-mode 1)

;; Disable killing emacs by C-x C-c...
(global-set-key (kbd "C-x C-c") 'keyboard-quit)

;; ...but we don't want to M-x save-buffers-kill-terminal to quit, M-x
;; quit is simpler
(defun quit ()
  "Shortcut to 'save-buffers-kill-terminal."
  (interactive)
  (progn (org-clock-out nil t)
         (save-buffers-kill-terminal)))

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
;; (global-set-key (kbd "C-h") 'delete-backward-char)
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

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-C C-f" . counsel-projectile-find-file)
         ("C-c C-f" . counsel-git)
         ("C-c C-g" . counsel-git-grep)))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (progn
    (ivy-rich-mode 1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))
    (setq ivy-rich-display-transformers-list
          `(counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            (setq ivy-rich-display-transformers-list
                  ivy-switch-buffer
                  (:columns
                   ((ivy-rich-switch-buffer-icon (:width 2))
                    (ivy-rich-candidate (:width 30))
                    (ivy-rich-switch-buffer-size (:width 7))
                    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                    (ivy-rich-switch-buffer-project (:width 15 :face success))
                    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                   :predicate
                   (lambda (cand) (get-buffer cand))))))))

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

;;; OCaml
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

(use-package merlin
  :config
  ;; Use opam switch to lookup ocamlmerlin binary
  ;; (setq merlin-command 'opam)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  ;; (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save t))

;; (use-package ocp-indent
;;   :config
;;   (setq ocp-indent-path (find-executable "ocp-indent")))

;;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  :ensure t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)
(use-package company-lsp)

;;; Org mode
(require 'org)
(setq org-reveal-root "file:///home/quentin/p/reveal.js/")
(unbind-key "C-'" org-mode-map)
(unbind-key "C-," org-mode-map)
;; (setq org-html-style-include-scripts nil)
(setq org-html-htmlize-output-type 'css)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
; (add-hook 'org-mode-hook (lambda () (fci-mode 0)))
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(global-set-key (kbd "<f12>") (lambda () (interactive)  (org-agenda nil "w")))

;; Disable electric indent that became the default in org-mode 9.4
(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

(setq org-startup-folded 'overview)
(setq org-agenda-files
      (append
       (file-expand-wildcards "~/notes/todo.org")))
(setq org-fast-tag-selection-single-key 'expert)
(setq org-fast-tag-selection-include-todo t)
(setq org-use-fast-todo-selection t)
(setq org-agenda-start-on-weekday 0)

(setq org-todo-keywords
      '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "MEETING(m)" "TOREAD(r)" "LATER(l)" "COURSE(c)" "DEADLINE(e)" "|" "DONE(d)")
        (sequence "WAITING(w)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("DEADLINE" :foreground "orange" :weight bold)
        ("COURSE" :foreground "blue")
        ("NEXT" :foreground "orange" :weight bold)
        ("TOREAD" :foreground "lightblue" :weight bold)
        ("PROJECT" :foreground "magenta " :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "yellow" :weight bold)
        ("MEETING" :foreground "magenta" :weight bold)
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
               "* TODO %?\n%U\n%a\n")
              ("r" "respond" entry (file "~/notes/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
              ("n" "next" entry (file "~/notes/refile.org")
               "* NEXT %?"t)
              ("N" "note" entry (file "~/notes/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/notes/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))

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
                (todo "WAITING")
                (todo "PROJECT")
                (todo "TOREAD")
                (todo "LATER"))))))

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

(global-set-key (kbd "C-x g") 'magit-status)

(defun my-electric-dot ()
  (interactive)
  (insert ".\n"))
(defun my-tex-hook ()
  (local-set-key (kbd ".") 'my-electric-dot))
(add-hook 'latex-mode-hook 'my-tex-hook)


;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (progn
;;             (projectile-mode)
;;             (counsel-projectile-on))
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind
;;   (("C-c p" . projectile-command-map))
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (setq projectile-project-search-path '("~/p" "~/f"))
;;   (setq projectile-completion-system 'helm)
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   (setq ffap-machine-p-known 'reject) ;; Don't ping websites when I try to autocomplete...
;;   )

(use-package flycheck
  :defer 2
  :diminish
  :config (setq flycheck-global-modes '(not "typescript-mode"  "tuareg-mode" "caml-mode" "ocaml-mode"))
  :init (global-flycheck-mode)
  :bind (("C-," . flycheck-next-error))
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-stylelintrc "~/.stylelintrc.json"))

(server-start)

(use-package lsp-mode
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          web-mode
          ) . lsp)
  :bind (("C-." . lsp-execute-code-action))
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-diagnostic-provider :none)             ; disable flycheck-lsp for most modes
  (add-hook 'web-mode-hook #'lsp-flycheck-enable) ; enable flycheck-lsp for web-mode locally
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-enable nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.5))

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

;; (defun setup-tide-mode ()
;;   "Setup tide mode for typescript development."
;;   (interactive)
;;   (defun tide-imenu-index () nil)
;;   (tide-setup)
;;   (tide-hl-identifier-mode +1))

;; (use-package tide
;;   :config
;;   (progn
;;     (define-key tide-mode-map (kbd "C-,") 'flycheck-next-error)
;;     (define-key tide-mode-map (kbd "C-.") 'tide-fix)
;;     (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;     (add-hook 'js-mode-hook #'setup-tide-mode)
;;     (add-hook 'js2-mode-hook #'setup-tide-mode)
;;     (add-hook 'rjsx-mode-hook #'setup-tide-mode)))

;; (defun use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
;; (add-hook 'flycheck-mode-hook (lambda () (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "8e959d5a6771b4d1e2177263e1c1e62c62c0f848b265e9db46f18754ea1c1998" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default))
 '(helm-completion-style 'emacs)
 '(org-agenda-files '("~/notes/todo.org"))
 '(package-selected-packages
   '(geiser-guile quack geiser ox-reveal sr-speedbar origami wgrep ponylang-mode yaml graphviz-dot-mode z3-mode dockerfile-mode mu4e-alert ht company-lsp lsp-ui lsp-metals lsp-mode nov ereader flycheck-aspell flymake-proselint all-the-icons-ivy all-the-icons-ivy-rich counsel-projectile dune ripgrep go-mode mu4e which-key vterm use-package tuareg tide slime scala-mode sbt-mode rust-mode powerline pdf-tools paredit org-journal openwith ocp-indent neotree merlin markdown-mode magit ivy-rich htmlize fixme-mode doom-themes doom-modeline counsel company command-log-mode clj-mode auth-source-xoauth2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:slant normal :weight normal :height 1.0 :width normal :foundry "1ASC" :family "Cantarell")))))


(add-to-list 'load-path "/home/quentin/.emacs.d/wat-mode")
(require 'wat-mode)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
      mu4e-sent-folder "/Gmail/[Gmail].Sent Mail"
      mu4e-trash-folder "/Gmail/[Gmail].Trash")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-get-mail-command "offlineimap")
(setq user-mail-address "quentin.stievenart@gmail.com"
      user-full-name  "Quentin Stiévenart")
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smt.gmail.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(setq message-kill-buffer-on-exit t)
(setq mu4e-maildir-shortcuts
      '(("/Gmail/INBOX"             . ?g)
        ("/Exchange/INBOX"          . ?e)
        ("/Gmail/[Gmail].Sent Mail" . ?s)
        ("/Gmail/[Gmail].Trash"     . ?t)))
(global-set-key (kbd "<XF86Mail>") 'mu4e)
(setq mu4e-confirm-quit nil)

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


;; Enable colors in *compilation* buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-hook 'my-colorize-compilation-buffer))




(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))


(defun check-paper-language ()
  (interactive)
  (ispell-change-dictionary "en_GB")
  (flyspell-buffer))

