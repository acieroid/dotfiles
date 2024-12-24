;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-challenger-deep)
; (setq doom-theme 'tsdh-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(global-tree-sitter-mode)

;; Avoid annoying autocomplete on enter
(after! company (setq company-idle-delay nil))

;; Disable parenthesis autocomplete
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Each path is relative to the path of the maildir you passed to mu
;; https://lambdaland.org/posts/2023-05-03_email_with_outlook/
(setq smtpmail-smtp-server "localhost")
(setq smtpmail-smtp-user "stievenart.quentin@uqam.ca")
(setq smtpmail-stream-type 'plain)
(setq smtpmail-smtp-service 5003)
(setq mu4e-get-mail-command "mbsync uqam")
(setq mu4e-maildir "/home/quentin/Mail")
(fset 'epg-wait-for-status 'ignore) ; TODO: disable when saving .gpg files work again without it
(set-email-account! "UQAM"
 '((user-mail-address . "stievenart.quentin@uqam.ca")
   (user-full-name . "Quentin Stiévenart")

   (mu4e-get-mail-command . "mbsync uqam")

   (mu4e-bookmarks .
                   ((:name "Inbox" :query "maildir:/UQAM/INBOX" :key 105)
                    (:name "Flagged" :query "flag:flagged AND NOT flag:trashed" :key 102)
                    (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
                    (:name "Today's messages" :query "date:today..now" :key 116)
                    (:name "Last 7 days" :query "date:7d..now" :key 119)
                    (:name "Drafts" :query "maildir:/UQAM/Drafts" :key 100)))

   ;;(sendmail-program . "/usr/bin/msmtp")
   ;;(message-sendmail-f-is-evil . t)
   ;;(message-sendmail-extra-arguments . '("--read-envelope-from"))
   ;;(message-send-mail-function . 'message-send-mail-with-sendmail)
   (message-send-mail-function . smtpmail-send-it)
   (smtpmail-smtp-server . "localhost")
   (smtpmail-smtp-user . "stievenart.quentin@uqam.ca")
   (smtpmail-stream-type . plain)
   (smtpmail-smtp-service . 5003)

   (mu4e-drafts-folder .  "/UQAM/Drafts")
   (mu4e-sent-folder . "/UQAM/Sent")
   (mu4e-trash-folder . "/UQAM/Trash")
   (mu4e-refile-folder . "/UQAM/Archive")))
;(after! mu4e
;  (setq sendmail-program (executable-find "msmtp")
;        send-mail-function #'smtpmail-send-it
;        message-sendmail-f-is-evil t
;        message-sendmail-extra-arguments '("--read-envelope-from")
;        message-send-mail-function #'message-send-mail-with-sendmail))

;;(setq message-send-mail-function 'smtpmail-send-it
;;      starttls-use-gnutls t
;;      smtpmail-stream-type 'starttls)
;;
;;(set-email-account! "gmail"
;;  '((mu4e-sent-folder       . "/gmail/Sent Mail")
;;    (mu4e-drafts-folder     . "/gmail/Drafts")
;;    (smtpmail-smtp-user     . "quentin.stievenart@gmail.com")
;;    (user-mail-address      . "quentin.stievenart@gmail.com")
;;    (smtpmail-starttls-credentials . '(("smt.gmail.com" 587 nil nil)))
;;    (smtpmail-default-smtp-server . "smtp.gmail.com")
;;    (smtpmail-smtp-server    . "smtp.gmail.com")
;;    (smtpmail-smtp-service   . 587))
;;  t)
;;(set-email-account! "office365"
;;  '((mu4e-sent-folder       . "/office365/Sent Mail")
;;    (mu4e-drafts-folder     . "/office365/Drafts")
;;    (smtpmail-smtp-user     . "stievenart.quentin@uqam.ca")
;;    (user-mail-address      . "stievenart.quentin@uqam.ca")
;;    (smtpmail-smtp-user     . "stievenart.quentin@uqam.ca")
;;    (user-mail-address      . "stievenart.quentin@uqam.ca")
;;    (smtpmail-starttls-credentials . '(("smtp.office365.com" 587 nil nil)))
;;    (smtpmail-default-smtp-server . "smtp.office365.com")
;;    (smtpmail-smtp-server    . "smtp.office365.com")
;;    (smtpmail-smtp-service   . 587))
;;  t)

;; Don't flycheck constantly, as it can be quite slow, e.g., on Haskell
(after! flycheck (setq flycheck-check-syntax-automatically '(save mode-enable)))

(setq org-clock-mode-line-total 'today)

;; Use ocp-indent on tab for OCaml
;(map! :map tuareg-mode-map "TAB" 'ocp-indent-line)
;(after! ocp-indent
;  (add-hook 'tuareg-mode-hook
;            (lambda ()
;              (local-set-key (kbd "M-q") 'tuareg-indent-phrase))))
;; Move between windows
; (windmove-default-keybindings)
; (define-minor-mode win-minor-mode
;   "Minor mode to move between emacs' windows"
;   :global t
;   :lighter " win-minor-mode"
;   :keymap (let ((keymap (make-keymap)))
;             (define-key keymap (kbd "M-c") 'windmove-left)
;             (define-key keymap (kbd "M-r") 'windmove-right)
;             (define-key keymap (kbd "M-s") 'windmove-up)
;             (define-key keymap (kbd "M-t") 'windmove-down)))
; (win-minor-mode 1)


 (setq whitespace-style '(face tabs spaces trailing space-before-tab indentation empty space-after-tab tab-mark space-mark))
 (setq whitespace-display-mappings
 '((space-mark   ?\    [? ]        [? ]) ; space     see here < >
   (space-mark   ?\xA0 [?_]     [?_]) ; hardspace see here < >
   (tab-mark     ?\t   [187 9]   [92 9]) ; tab       see here <	>
   (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]); end-of-line <no demo>
   ))

;;; Move between windows using alt and arrows or M-{c,t,s,r}.
(map! :n "M-c" 'evil-window-left)
(map! :n "M-r" 'evil-window-right)
(map! :n "M-s" 'evil-window-up)
(map! :n "M-t" 'evil-window-down)
;; evil-markdown-mode needs to be overwritten too
(add-hook 'evil-markdown-mode-hook
          (lambda ()
            (map! :map evil-markdown-mode-map
                  :n "M-c" nil
                  :n "M-r" nil
                  :n "M-s" nil
                  :n "M-t" nil
            )))


;; (windmove-default-keybindings)
;; (define-minor-mode win-minor-mode
;;   "Minor mode to move between emacs' windows"
;;   :global t
;;   :lighter " win-minor-mode"
;;   :keymap (let ((keymap (make-keymap)))
;;             (define-key keymap (kbd "M-c") 'windmove-left)
;;             (define-key keymap (kbd "M-r") 'windmove-right)
;;             (define-key keymap (kbd "M-s") 'windmove-up)
;;             (define-key keymap (kbd "M-t") 'windmove-down)
;;             keymap))
;; (win-minor-mode 1)

;; I like to see my recent files when switching buffers
; (map! :leader "b b" 'counsel-buffer-or-recentf)

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

(use-package! ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  ;:hook (before-save . ocamlformat-before-save)
  )

;; To avoid LSP incorrectly trying to load stuff from /tmp
(after! lsp-mode
  (setq lsp-auto-guess-root nil))

;; font-lock-mode is slow on large typescript files
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (or (string-equal (file-name-extension buffer-file-name) "ts")
                      (string-equal (file-name-extension buffer-file-name) "tsx"))
              (setq font-lock-support-mode 'jit-lock-mode))))

(use-package! rfc2047
  :custom
  ;; needed so that mu4e doesn't produce broken address lines
  ;; when replying to addresses with accents in name portion
  (rfc2047-quote-decoded-words-containing-tspecials t))

