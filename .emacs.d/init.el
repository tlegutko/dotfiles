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
;; transpose chars
(define-key evil-insert-state-map "\C-t" 'transpose-chars)
;; split line
(define-key evil-normal-state-map (kbd "K") (kbd "r RET"))

;; paste from x clipboard in visual mode
(fset 'evil-visual-update-x-selection 'ignore)

;; separate sentences by one space
(setq sentence-end-double-space nil)

;; evil-surround
(global-evil-surround-mode 1)

;; term improvements
(evil-define-key 'normal term-raw-map "p" 'term-paste)
(evil-define-key 'normal term-raw-map "j" 'term-send-down)
(evil-define-key 'normal term-raw-map "k" 'term-send-up)
(evil-define-key 'normal term-raw-map (kbd "RET") 'term-send-raw)
(evil-define-key 'normal term-raw-map (kbd "C-r") 'term-send-raw)
(evil-define-key 'normal term-raw-map (kbd "C-c") 'term-send-raw)
(evil-define-key 'insert term-raw-map (kbd "C-c") 'term-send-raw)

;;; eval shortcut
(global-set-key (kbd "C-x e") 'eval-buffer)

;;; i3-like mouse hover effect
(setq mouse-autoselect-window t)

;;; Auctex and pdf-tools
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection (quote ((output-pdf "Okular"))))
(setq delete-selection-mode nil)
(setq mark-even-if-inactive t)
(setq transient-mark-mode 1)

(use-package openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "okular" (file))))

(use-package reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(defun recompile-pdf-on-save ()
  "Recompile latex on save with external script."
  (when (eq major-mode 'latex-mode)
    (call-process "~/.config/scripts/recompile-masters-thesis.sh")))

(add-hook 'after-save-hook #'recompile-pdf-on-save)

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

;; unmap C-' for avy
(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-'"))))

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
;; magit
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; keychain-environment, so magit sees ssh-agent
(use-package keychain-environment)
(keychain-refresh-environment)

(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 15)
  :bind
  (("M-y" . counsel-yank-pop)
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (ivy-mode 1))

(use-package avy
  :bind
  (("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)))

(defun counsel-yank-zsh-history ()
  "Yank the zsh history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                           (buffer-string))
                         "\n"
                         t)))

    (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection)) ;; for zsh
                      
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Zsh history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))

;; yasnippet
(use-package yasnippet)
(yas-global-mode 1)

(defun yas-org-get-time-stamp (&rest args)
  "Return the string that `org-insert-time-stamp' would insert."
  (with-temp-buffer
    (apply #'org-insert-time-stamp args)
    (buffer-string)))

;; fix yas-prev mapping on my keyboard 
(define-key yas-keymap [(shift tab)] nil)
(define-key yas-keymap [backtab]     nil)
(define-key yas-keymap (kbd "<S-iso-lefttab>") 'yas-prev-field)

;; evil indent for elisp
(add-hook 'emacs-lisp-mode-hook
  (function (lambda ()
          (setq evil-shift-width 2))))
