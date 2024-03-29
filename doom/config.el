;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Quentin Stiévenart"
      user-mail-address "quentin.stievenart@gmail.com")

(setq doom-font (font-spec :family "Monospace" :size 14))
;; (setq doom-font (font-spec :family "Hack Nerd Font" :size 13))
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
; (setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'tsdh-light)

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
(setq user-full-name  "Quentin Stiévenart")
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "/home/quentin/.mail")
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-stream-type 'starttls)

(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/Sent Mail")
    (mu4e-drafts-folder     . "/gmail/Drafts")
    (smtpmail-smtp-user     . "quentin.stievenart@gmail.com")
    (user-mail-address      . "quentin.stievenart@gmail.com")
    (smtpmail-starttls-credentials . '(("smt.gmail.com" 587 nil nil)))
    (smtpmail-default-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-server    . "smtp.gmail.com")
    (smtpmail-smtp-service   . 587))
  t)
(set-email-account! "office365"
  '((mu4e-sent-folder       . "/office365/Sent Mail")
    (mu4e-drafts-folder     . "/office365/Drafts")
    (smtpmail-smtp-user     . "stievenart.quentin@uqam.ca")
    (user-mail-address      . "stievenart.quentin@uqam.ca")
    (smtpmail-smtp-user     . "stievenart.quentin@uqam.ca")
    (user-mail-address      . "stievenart.quentin@uqam.ca")
    (smtpmail-starttls-credentials . '(("smtp.office365.com" 587 nil nil)))
    (smtpmail-default-smtp-server . "smtp.office365.com")
    (smtpmail-smtp-server    . "smtp.office365.com")
    (smtpmail-smtp-service   . 587))
  t)

;; Don't flycheck constantly, as it can be quite slow, e.g., on Haskell
(after! flycheck (setq flycheck-check-syntax-automatically '(save mode-enable)))

