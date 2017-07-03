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

(use-package all-the-icons)

(use-package neotree
  :bind
  (("C-c d" . neotree-toggle))
  :config
  (add-hook 'neotree-mode-hook 'evil-insert-state))

(use-package hl-line
  :config
  (hl-line-mode 1))

(use-package nlinum-relative
  :init
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-offset 1)
  :bind
  (("C-c n" . nlinum-mode)
   ("C-c N" . nlinum-relative-toggle)))

;; outside of use-package, as nlinum doesn't load by default when in daemon mode
;; see https://github.com/kaushalmodi/.emacs.d/issues/4
(defun nlinum-hook-on ()
  (nlinum-mode)
  (nlinum-relative-on))
(add-hook 'emacs-lisp-mode-hook 'nlinum-hook-on)
(add-hook 'scala-mode-hook 'nlinum-hook-on)

(use-package doom-themes
  :pin melpa
  :diminish doom-buffer-mode
  :config
  (load-theme 'doom-molokai t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode -1)
  (size-indication-mode -1)
  ;; font is 1/10 of height
  (set-face-attribute 'default nil :height 80)
  ;;; i3-like mouse hover effect
  (setq mouse-autoselect-window t)
  ;;; Settings (defaults)
  (setq doom-enable-bold nil    ; if nil, bolding are universally disabled
	doom-enable-italic t  ; if nil, italics are universally disabled
	;; doom-one specific settings
	doom-one-brighter-modeline nil
	doom-one-brighter-comments nil)
  ;;; OPTIONAL
  ;; brighter source buffers (that represent files)
  (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  ;; ...if you use auto-revert-mode
  (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  ;; And you can brighten other buffers (unconditionally) with:
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  ;; Enable nlinum line highlighting
  ;; (doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode
  ;; Necessary for org-mode
  (setq org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t))

;;; automatic custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-set-key (kbd "C-x K") 'kill-this-buffer)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x B") 'switch-to-previous-buffer)

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
(setq create-lockfiles nil)

(use-package simple
  :ensure nil
  :init
  (setq save-interprogram-paste-before-kill t)
  :bind
  (("C-c V" . visual-line-mode)))

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
	("C-t" . transpose-chars)
	("C-a" . back-to-indentation)
	("C-e" . move-end-of-line)
	("C-y" . yank)
	:map evil-motion-state-map
	("$" . evil-last-non-blank)
	("g_" . evil-end-of-line)
	("C-]" . nil) ;; for avy to use
	("C-v" . scroll-up-command))
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
  (setq openwith-associations '(("\\.pdf\\'" "okular" (file))
				("\\.mov\\'" "mpv" (file))))
  :config
  (openwith-mode t))

(use-package files
  :ensure nil
  :init
  (setq large-file-warning-threshold nil))

(use-package reftex
  :diminish reftex-mode
  :init
  (setq reftex-plug-into-AUCTeX t)
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;; org mode
(use-package org
  :init
  (setq org-clock-mode-line-total 'current)
  (setq org-clock-persist 'history)
  (setq calendar-week-start-day 1)
  (setq org-startup-truncated 'nil)
  (setq org-agenda-start-with-log-mode t)
  (setq org-capture-templates
	'(("a" "Appointment" entry (file  "~/org/calendar.org" )
	   "* %?\n%^T")
	  ("p" "Personal journal" entry (file "~/org/personal-journal.org")
	   "* personal journal\n  %?" :unnarrowed t :clock-in t :clock-resume t)
	  ("t" "To do" entry (file "~/org/todo.org")
	   "* TODO %?" :prepend t)
	  ("l" "Laptop config" entry (file "~/org/laptop-config.org")
	   "* TODO %?" :prepend t)
	  ("m" "Miracle morning" entry (file "~/org/miracle-morning.org")
	   "* miracle morning\nmm%?" :clock-in t :clock-resume t)
	  ("e" "Miracle evening" entry (file "~/org/miracle-evening.org")
	   "* miracle evening\nme%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("w" "Weekly summary" entry (file "~/org/weekly-summary.org")
	   "* weekly summary\nws%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("D" "Dance notes" entry (file "~/org/dance-notes.org")
	   "* %?" :unnarrowed t :clock-in t :clock-resume t)
	  ("b" "Books and articles")
	  ("bt" "To-read list" entry (file "~/org/books-to-read.org") "* %?")
	  ("bn" "Notes from books" entry (file "~/org/books-notes.org") "* %?")
	  ("d" "Diet")
	  ("dw" "Weight" entry (file "~/org/diet-scores.org")
	   "* %t waga %?" :unnarrowed t)
	  ("ds" "Score" plain (file "~/org/diet-scores.org")
	   "* %t ocena %?" :unnarrowed t)
	  ))
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
  (unbind-key "C-]" org-mode-map) ;; for avy to use
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (org-clock-persistence-insinuate)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (sh . t)
     (shell . t))))


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
(use-package sbt-mode
  :pin melpa)

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
				  ("| " "SPC")))
  (bind-key "C-c e" 'ensime scala-mode-map)
  (bind-key "C-c E" 'ensime-shutdown scala-mode-map))

(use-package expand-region)

(use-package company
  :diminish company-mode
  :config
  (defun company-next-prev-bindings ()
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") #'company-select-next)
      (define-key company-active-map (kbd "C-p") #'company-select-previous))))


(use-package ensime
  :pin melpa
  :init
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  ;; :diminish ensime-mode
  :config
  (require 'ensime-expand-region))

;; scala-mode hooks
(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode t)
            (smartparens-mode t)
	    (ensime-mode t)
	    (scala-mode:goto-start-of-code)
	    (company-next-prev-bindings)))

(use-package magit
  :init
  (setq-default vc-handled-backends nil)
  (setq-default vc-mode nil)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup)
  :config
  (defun hook-diminish-auto-revert ()
    (interactive)
    (diminish 'auto-revert-mode))
  (add-hook 'auto-revert-mode-hook 'hook-diminish-auto-revert)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package keychain-environment ;; so magit sees ssh-agent
  :config
  (keychain-refresh-environment))

(use-package smerge-mode
  :init
  (setq smerge-command-prefix "\C-cv"))

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 12)
  :bind
  (("C-c r" . ivy-resume)
   :map ivy-minibuffer-map
   ("C-," . ivy-minibuffer-shrink)
   ("C-." . ivy-minibuffer-grow)
   ("C-s" . ivy-next-line)
   ("M-y" . ivy-next-line)))

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   ("C-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x ," . counsel-M-x)
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
   ("C-x l" . counsel-locate))
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

(use-package avy
  :init
  (setq avy-timeout-seconds 0.4)
  :bind
  (("C-'" . avy-goto-char-timer)
   ([C-escape] . avy-goto-char-timer)
   ([C-S-escape] . avy-goto-line)
   ("C-`" . avy-goto-line)
   ("M-'" . avy-goto-line)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

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

(use-package dired-x
  :ensure nil
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-mode t)
  (setq dired-listing-switches "-alh"))

(use-package dired-narrow
  :bind
  (:map dired-mode-map
	("/" . dired-narrow)))

(defun dired-find-file-other-frame ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))

(eval-after-load "dired"
  '(define-key dired-mode-map "F" 'dired-find-file-other-frame))

(use-package buffer-move
  :bind
  (("C-x 4 h" . buf-move-left)
   ("C-x 4 j" . buf-move-down)
   ("C-x 4 k" . buf-move-up)
   ("C-x 4 l" . buf-move-right)))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  :bind
  (([C-tab] . ace-window)))

(use-package winner
  :config
  (winner-mode 1))
