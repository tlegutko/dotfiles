;; the package manager and use-package
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("org" . "http://orgmode.org/elpa/")
		    ("melpa" . "http://melpa.org/packages/")
		    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; use-package always auto install packages
(setq use-package-always-ensure t)

;;; look & feel
(setq-default mode-line-mule-info nil)
(setq-default mode-line-modified nil)
(setq-default mode-line-position nil)
(setq-default mode-line-remote nil)
(setq-default mode-line-client nil)
(setq-default mode-line-frame-identification nil)

(use-package zenburn-theme
  :init
  :config
  (load-theme 'zenburn t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode -1)
  (size-indication-mode -1)
  ;; font is 1/10 of height
  (set-face-attribute 'default nil :height 80)
  ;;; i3-like mouse hover effect
  (setq mouse-autoselect-window t))

;;; automatic custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; backup and auto-save
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backup")))))
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) (setq auto-save-interval 20)
(setq auto-save-timeout 10)
(setq desktop-auto-save-timeout 10)
(desktop-save-mode 1)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(global-visual-line-mode 1)
(diminish 'visual-line-mode)

(use-package evil
  :init
  (setq evil-normal-state-tag "")
  ;; separate sentences by one space
  (setq sentence-end-double-space nil)
  ;; paste from x clipboard in visual mode
  (fset 'evil-visual-update-x-selection 'ignore)
  :bind
  (:map evil-normal-state-map
	("M-." . nil)
	("C-k" . my-evil-scroll-up)
	("C-j" . my-evil-scroll-down)
	("C-t" . transpose-chars)
	("K" . my-evil-split-line)
	("C-v" . scroll-up-command)
	("C-S-v" . evil-visual-block)
	("C-n" . evil-next-line)
	("C-p" . evil-previous-line)
   :map evil-insert-state-map
	("C-t" . transpose-chars))
  :config
  (defun my-evil-scroll-up ()
    (interactive)
    (evil-scroll-up nil))
  (defun my-evil-scroll-down ()
    (interactive)
    (evil-scroll-down nil))
  (defun my-evil-split-line ()
    (interactive)
    (kbd "r RET"))
  (evil-define-key 'normal term-raw-map "p" 'term-paste)
  (evil-define-key 'normal term-raw-map "j" 'term-send-down)
  (evil-define-key 'normal term-raw-map "k" 'term-send-up)
  (evil-define-key 'normal term-raw-map (kbd "RET") 'term-send-raw)
  (evil-define-key 'normal term-raw-map (kbd "C-r") 'term-send-raw)
  (evil-define-key 'normal term-raw-map (kbd "C-c") 'term-send-raw)
  (add-hook 'emacs-lisp-mode-hook
    (function (lambda ()
	    (setq evil-shift-width 2))))
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(defun sync-init-el-on-save ()
  "Sync .dotfiles/.emacs.d/init.el after save."
  (when (eq major-mode 'emacs-lisp-mode)
    (call-process "~/.dotfiles/.sync.sh")))
(add-hook 'after-save-hook #'sync-init-el-on-save)

;;; Auctex and pdf-tools
(use-package tex
  :ensure auctex
  :config
  (defun recompile-pdf-on-save ()
    "Recompile latex on save with external script."
    (when (eq major-mode 'latex-mode)
      (call-process "~/.config/scripts/recompile-masters-thesis.sh")))
  (add-hook 'after-save-hook #'recompile-pdf-on-save)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(use-package flyspell
  :bind
  (:map flyspell-mode-map
   ("C-:" . flyspell-save-word))
  :config
  (defun flyspell-save-word ()
    "Save word to personal dictionary."
    (interactive)
    (let ((current-location (point))
	  (word (flyspell-get-word)))
      (when (consp word)    
	(flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))

(use-package openwith
  :init
  (setq openwith-associations '(("\\.pdf\\'" "okular" (file))))
  :config
  (openwith-mode t))

(use-package reftex
  :diminish reftex-mode
  :init
  (setq reftex-plug-into-AUCTeX t)
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;; line numbers
(use-package linum-relative
  :init
  (setq linum-relative-current-symbol "")
  :config
  (global-linum-mode t)
  (linum-relative-on))

;;; org mode
(use-package org
  :init
  (setq org-clock-persist 'history)
  (setq calendar-week-start-day 1)
  (setq org-startup-truncated 'nil)
  (setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/org/calendar.org" )
  	 "* %?\n%^T")
	("p" "Personal journal" entry (file "~/org/personal-journal.org") "* %T %?")
	("t" "To do" entry (file "~/org/todo.org")
	 "* TODO %?" :prepend t)))
  :bind
  (("C-c C-x C-j" . org-clock-goto)
  ("C-c C-x C-i" . org-clock-in)
  ("C-c C-x C-o" . org-clock-out)
  ("C-C C-x C-e" . org-clock-modify-effort-estimate)
  ("C-c C-x C-q" . org-clock-cancel)
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda)
  ("\C-cc" . org-capture)
  ("\C-cb" . org-iswitchb)
  :map org-mode-map
  ("M-TAB" . org-global-cycle)
  ("\M-q" . toggle-truncate-lines))
  :config
  (unbind-key "C-'" org-mode-map) ;; for avy to use
  (unbind-key "C-c C-r" org-mode-map) ;; ivy-resume
  (org-clock-persistence-insinuate)
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) )))

(use-package org-gcal
  :pin melpa
  :init
  (load "~/.emacs.d/org-gcal-credentials.el")
  (setq org-gcal-up-days 7) ; before today
  (setq org-gcal-down-days 7)) ; after today

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
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
  (show-smartparens-global-mode))

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

(use-package expand-region)

(use-package company
  :diminish company-mode)

(use-package ensime
  :pin melpa-stable
  :diminish ensime-mode
  :init
  :config
  (require 'ensime-expand-region))

;; scala-mode hooks
(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode t)
            (smartparens-mode t)
	    (ensime-mode t)
	    (scala-mode:goto-start-of-code)))

(use-package magit
  :init
  (setq-default vc-handled-backends nil)
  (setq-default vc-mode nil)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

(defun hook-diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode))
(add-hook 'auto-revert-mode-hook 'hook-diminish-auto-revert)

(use-package keychain-environment ;; so magit sees ssh-agent
  :config
  (keychain-refresh-environment))

(use-package flx)

(use-package ivy
  :diminish ivy-mode)

(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 12)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  :bind
  (("M-y" . counsel-yank-pop)
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (defun ivy-yank-action (x)
    (kill-new x))
  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (insert x)))
  (ivy-set-actions
  t
  '(("i" ivy-copy-to-buffer-action "insert")
    ("y" ivy-yank-action "yank")))
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
  (ivy-mode 1))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-on))

(use-package ivy-hydra)

(use-package avy
  :bind
  (("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)))

(use-package yasnippet
  :bind
  (("C-c n" . yas-insert-new-heading-hm)
  ("C-c N" . yas-insert-new-heading)
  :map yas-minor-mode-map
  ("<tab>" . yas-expand)
  ("TAB" . yas-expand)
  ("[(shift tab)]" . nil)
  ("[backtab]" . nil)
  ("<S-iso-lefttab>" . yas-prev-field))
  :config
  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))
  (defun yas-insert-snip-in-newline(snip-name)
    "inserts newline line below and inserts snippet with given name"
    (interactive)
    (evil-insert-state)
    (if (current-line-empty-p)
	(beginning-of-line)
	(progn
	  (end-of-line)
	  (newline)))
    (yas-expand-snippet
      (yas-lookup-snippet snip-name)))
  (defun yas-org-get-time-stamp (&rest args)
    "Return the string that `org-insert-time-stamp' would insert."
    (with-temp-buffer
      (apply #'org-insert-time-stamp args)
      (buffer-string)))
  (defun yas-insert-new-heading-hm ()
    (interactive)
    (yas-insert-snip-in-newline "New Heading with Date with HM"))
  (defun yas-insert-new-heading ()
    (interactive)
    (yas-insert-snip-in-newline "New Heading with Date"))
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package notifications
  :config
  (defun desktop-notification (time message &optional title)
    "System notification at some event or timer."
    (run-at-time time nil 'notifications-notify
		:title (or title "alarm")
		:body message
		:urgency 'critical ;; low, normal, critical
		:app-name "Emacs: Org")))

;; helpful in looking for empty bindings
(use-package free-keys)

(use-package which-key
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-[") (kbd "["))

(use-package sublimity
  :init
  (setq sublimity-scroll-weight 5
      sublimity-scroll-drift-length 10)
  (setq next-screen-context-lines 10)
  :config
  (sublimity-mode 1))

(use-package sh-script
  :init
  (setq display-buffer-alist nil)
  :bind
  (("C-x e" . eval-buffer)
   ("C-x C-e" . eval-last-sexp)
  :map sh-mode-map
   ("C-c e" . eval-shell-buffer)
   ("C-x C-e" . eval-shell-current-line-or-region))
  :config
  (defun insert-shell-output-at-position (command position)
    (save-excursion
      (goto-char position)
      (insert (with-temp-buffer
	(shell-command command t)
	(buffer-string)))))
  (defun eval-shell-buffer (arg)
    (interactive "p")
    (if (/= arg 1) ;; arg not nil
	(insert-shell-output-at-position (buffer-string) (point-max))
	(shell-command (buffer-string))))
  (defun eval-shell-current-line-or-region (start end arg)
    (interactive "r\np")
    (let ((command-position
	  (if (use-region-p)
	    (list (buffer-substring start end) end)
	    (list (thing-at-point 'line t) (+(point-at-eol) 1)))))
      (if (/= arg 1)
	(apply 'insert-shell-output-at-position command-position)
	(shell-command (car command-position))))))
