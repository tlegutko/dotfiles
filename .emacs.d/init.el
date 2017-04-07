;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("org" . "http://orgmode.org/elpa/")
		    ("melpa" . "http://melpa.org/packages/")
		    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

;; my list of packages to auto install
(setq package-list '(zenburn-theme evil evil-surround pdf-tools org linum-relative use-package))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; use-package always download listed packages
(setq use-package-always-ensure t)

;;; look & feel
(load-theme 'zenburn t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; font is 1/10 of height
(set-face-attribute 'default nil :height 80)
;; auto-save loaded buffers
(desktop-save-mode 1)

;;; evil
(require 'evil)
(evil-mode 1)
;; remove binding for ensime's go to definition
(define-key evil-normal-state-map (kbd "M-.") nil)
;; C-k C-j for scroll up/down
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
						(interactive)
						(evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
						(interactive)
						(evil-scroll-down nil)))
(define-key evil-insert-state-ma‌​p (kbd "C-t") 'transpose-chars)

;; separate sentences by one space
(setq sentence-end-double-space nil)

;; evil-surround
(global-evil-surround-mode 1)

;;; eval shortcut
(global-set-key (kbd "C-x e") 'eval-buffer)

;;; i3-like mouse hover effect
(setq mouse-autoselect-window t)

;;; Auctex nad pdf-tools
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(pdf-tools-install)

;;; line numbers
(require 'linum-relative)
(global-linum-mode t)
(linum-relative-on)
(setq linum-relative-current-symbol "")

;;; automatic custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; org mode
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq calendar-week-start-day 1)
; line wrapping
(setq org-startup-truncated 'nil)
(global-visual-line-mode t)
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smart partentheses
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; scala
(use-package scala-mode
  :defer t
  :pin melpa
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config

  ;; prefer smartparens for parens handling
  (remove-hook 'post-self-insert-hook
               'scala-indent:indent-on-parentheses)

  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil
               :post-handlers '(("||\n[i]" "RET")
                                ("| " "SPC"))))
;; ensime
(use-package ensime
  :pin melpa-stable)

;; scala-mode hooks
(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode t)
            (smartparens-mode t)
	    (ensime-mode t)
	    (scala-mode:goto-start-of-code)))
