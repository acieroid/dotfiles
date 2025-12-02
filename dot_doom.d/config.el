;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Fish as a default shell can break some packages. Use bash as shell, unless it is interactive
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Quentin Stiévenart"
      user-mail-address "stievenart.quentin@uqam.com")

; (setq doom-font (font-spec :family "Monospace" :size 14))
; (setq doom-font (font-spec :family "Iosevka Term" :size 14))
; (setq doom-font (font-spec :family "Hack Nerd" :size 14))

;(setq doom-font "Fira Code-14")
(setq doom-theme 'doom-challenger-deep)

(setq display-line-numbers-type t)

(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(global-tree-sitter-mode)

;; Avoid annoying autocomplete on enter
(after! company (setq company-idle-delay nil))

;; Disable parenthesis autocomplete
(after! smartparens
  (smartparens-global-mode -1))

;; Don't flycheck constantly, as it can be quite slow, e.g., on Haskell
(after! flycheck (setq flycheck-check-syntax-automatically '(save mode-enable)))

(after! org
  (setq org-startup-indented nil)   ; don't enable org-indent on open
  (remove-hook 'org-mode-hook #'org-indent-mode))

 (setq whitespace-style '(face tabs spaces trailing space-before-tab indentation empty space-after-tab tab-mark space-mark))
 (setq whitespace-display-mappings
 '((space-mark   ?\    [? ]        [? ])    ; space     see here < >
   (space-mark   ?\xA0 [?_]        [?_])    ; hardspace see here < >
   (tab-mark     ?\t   [187 9]     [92 9])  ; tab       see here <	>
   (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]); end-of-line <no demo>
   ))

;;; Move between windows using alt and arrows or M-{c,t,s,r}.
(map! :n "M-c" 'evil-window-left)
(map! :n "M-r" 'evil-window-right)
(map! :n "M-s" 'evil-window-up)
(map! :n "M-t" 'evil-window-down)
;; Some modes already had these bindings, so we need to remove them
(dolist (m '(evil-markdown-mode-map evil-org-mode-map))
  (map! :map m :n "M-c" nil :n "M-r" nil :n "M-s" nil :n "M-t" nil))

;; Folding
(map! :leader "c f" 'ts-fold-toggle)

;; Use ocp-indent on tab
(after! tuareg-mode
  (map! :map tuareg-mode-map "TAB" 'ocp-indent-line)
  (map! :map reason-mode-map "TAB" 'ocp-indent-line))
(after! ocp-indent
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (local-set-key (kbd "M-q") 'tuareg-indent-phrase)
              ;; Avoid newline to break comments in two
              (setq-local comment-line-break-function nil)
              )))

;; To avoid LSP incorrectly trying to load stuff from /tmp
(after! lsp-mode
  (setq lsp-auto-guess-root nil))

(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'typescript))
(add-hook 'typescript-mode-hook #'tree-sitter-mode)
(add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)

