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

;; stuff that isn't in elpa
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; use-package always auto install packages
(setq use-package-always-ensure t)

;; always confirm with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; mause autoselect window - off because it messes with ace-window and i3 mouse_warping
(setq mouse-autoselect-window nil)

;;; look & feel
(setq-default mode-line-mule-info nil)
(setq-default mode-line-modified nil)
(setq-default mode-line-position nil)
(setq-default mode-line-remote nil)
(setq-default mode-line-client nil)
(setq-default mode-line-frame-identification nil)

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

(use-package doom-themes
  :pin melpa
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode -1)
  (size-indication-mode -1)
  ;; font is 1/10 of height
  (set-face-attribute 'default nil :height 105)
  ;;; i3-like mouse hover effect
  (setq mouse-autoselect-window nil)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :pin melpa
  :config
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

;;; automatic custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-unset-key (kbd "C-z"))
(setq sentence-end-double-space nil)
(global-set-key (kbd "C-c s") 'isearch-forward)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x B") 'switch-to-previous-buffer)
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

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
  (("C-a" . back-to-indentation)
   ("M-m" . beginning-of-line)
   ("C-k" . kill-line)
   ("M-k" . kill-whole-line)
   ("C-c v" . visual-line-mode)))

(use-package evil
  :bind
  ("M-o" . evil-execute-in-normal-state)
  :config
  (add-hook 'scala-mode-hook
	    (function (lambda ()
			(setq evil-shift-width 2))))
  (evil-mode 0))

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
      (call-process "/home/tlegutko/resume/recompile-resume.sh")))
  (add-hook 'after-save-hook #'recompile-pdf-on-save)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(use-package flyspell
  :bind
  (:map flyspell-mode-map
	("C-;" . nil)
	("C-'" . flyspell-auto-correct-previous-word)
	("C-\"" . flyspell-save-word))
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
				("\\.mp4\\'" "mpv --input-ipc-server=~/.mpvsocket " (file))
				("\\.mp3\\'" "mpv --input-ipc-server=~/.mpvsocket  --force-window" (file))
				("\\.mov\\'" "mpv --input-ipc-server=~/.mpvsocket" (file))))
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
  (setq org-agenda-start-on-weekday 6)
  (setq org-startup-truncated 'nil)
  (setq org-agenda-start-with-log-mode t)
  (setq org-duration-format (quote h:mm))
  (setq org-capture-templates
	'(
	  ("m" "Miracle morning" plain (file "~/org/miracle-morning.org")
	   "* miracle morning %U\nmm%?" :clock-in t :clock-resume t)
	  ("e" "Miracle evening" plain (file "~/org/miracle-evening.org")
	   "* miracle evening %U\nme%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("a" "Ask question" plain (file "~/org/questions.org")
	   "* %?" :unnarrowed t)
	  ("s" "Periodic summary")
	  ("sw" "Weekly summary" plain (file "~/org/weekly-summary.org")
	   "* weekly summary %u--%u\nws%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("sm" "Monthly summary" plain (file "~/org/monthly-summary.org")
	   "* monthly summary %U\nms%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("st" "Trimonthly summary" plain (file "~/org/trimonthly-summary.org")
	   "* trimonthly summary %U\nts%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("sy" "Yearly summary" plain (file "~/org/yearly-summary.org")
	   "* yearly summary %U\nys%?" :unnarrowed t :clock-in t :clock-resume t)
	  ("b" "Books and articles")
	  ("bt" "To-read list" plain (file "~/org/books-to-read.org")
	   "* %?" :unnarrowed t)
	  ("bn" "Notes from books" plain (file "~/org/books-notes.org")
	   "* %?" :unnarrowed t)
	  ("l" "Laptop config" plain (file "~/org/laptop-config.org")
	   "* TODO %?" :prepend t)
	  ("t" "To do" plain (file "~/org/todo.org")
	   "* TODO %?" :prepend t)
	  ("d" "Dance notes" plain (file "~/org/dance-notes.org")
	   "* %?" :unnarrowed t :clock-in t :clock-resume t)
	  ("w" "Weight" plain (file "~/org/diet-scores.org")
	   "* %t waga %?" :unnarrowed t)
	  ("a" "Appointment" plain (file  "~/org/calendar.org" )
	   "* %?\n%^T")
	  ("p" "personal journal" plain (file "~/org/personal-journal.org")
	   "* personal journal %U\n  %?" :unnarrowed t :clock-in t :clock-resume t)
	  ("P" "PMO journal" plain (file "~/org/pmo-journal.org")
	   "* %u\n  %?" :unnarrowed t :clock-in t :clock-resume t)))
  :bind
  (("C-c C-x C-j" . org-clock-goto)
   ("C-c C-x C-i" . org-clock-in)
   ("C-c C-x C-o" . org-clock-out)
   ("C-C C-x C-e" . org-clock-modify-effort-estimate)
   ("C-c C-x C-q" . org-clock-cancel)
   ("\C-cl" . org-store-link)
   ("\C-ca" . org-agenda)
   ("\C-cc" . org-capture)
   :map org-mode-map
   ("M-h" . nil)
   ("C-M-p" . org-metaup)
   ("C-M-n" . org-metadown)
   ("M-p" . org-metaright)
   ("M-n" . org-metaleft)
   ([C-M-return] . org-insert-todo-heading)
   ([C-tab] . nil)
   ("C-a" . nil)
   ([return] . org-return-indent)
   ("C-m" . org-return-indent)
   ([M-tab] . org-global-cycle)
   ([M-S-tab] . org-global-cycle)
   ("C-c o" . org-latex-export-to-pdf)
   ("\M-q" . toggle-truncate-lines))
  :config
  ;; Enable Confluence export
  (require 'ox-confluence)
  (unbind-key "C-'" org-mode-map) ;; for avy to use
  (unbind-key "C-]" org-mode-map) ;; for avy to use
  (setq org-return-follows-link t)
  (setq org-cycle-emulate-tab 'white)
  (setq org-odt-preferred-output-format "docx")
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (defun yas-org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
  (add-hook 'org-mode-hook
  	    (lambda ()
  	      (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
  	      (define-key yas-keymap [tab] 'yas-next-field)))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (shell . t))))


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
(use-package sbt-mode)

(use-package scala-mode
  :defer t
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
  :pin melpa-stable
  :init
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-search-interface 'ivy)
  ;; :diminish ensime-mode
  :config
  (require 'ensime-expand-region))

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu)) ;; great for ensime

;; scala-mode hooks
(add-hook 'scala-mode-hook
          (lambda ()
	    (nlinum-hook-on)
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
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   :map magit-status-mode-map
   ([C-tab] . nil))
  :config
  (defun hook-diminish-auto-revert ()
    (interactive)
    (diminish 'auto-revert-mode))
  (add-hook 'auto-revert-mode-hook 'hook-diminish-auto-revert))

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
  (setq enable-recursive-minibuffers t)
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
  (setq ivy-switch-buffer-faces-alist nil)
  (defun ivy-yank-action (x)
    (kill-new x))
  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (insert x)))
  (ivy-set-actions
   t
   '(("i" ivy-copy-to-buffer-action "insert")
     ("y" ivy-yank-action "yank")))
  (defun counsel-zsh-history ()
    "Insert element from the zsh history"
    (interactive)
    (let (hist-cmd collection val)
      ;; (shell-command "history -r")	; reload history
      ;; (message "test")
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
	;; (message "%s => kill-ring" val)
        (insert val))))
  (ivy-mode 1))

;; (use-package projectile
  ;; :init
  ;; (setq projectile-completion-system 'ivy)
  ;; (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  ;; :config
  ;; (projectile-global-mode))

;; (use-package counsel-projectile
  ;; :config
  ;; (counsel-projectile-on))

(use-package avy
  :init
  (setq avy-timeout-seconds 0.2)
  (setq avy-keys (number-sequence ?a ?z))
  :bind
  (([C-escape] . avy-goto-char-timer)
   ("C-'" . avy-goto-char-in-line)
   ([C-M-escape] . avy-goto-line-end-or-beginning)
   ("C-\\" . avy-goto-line-end-or-beginning))
  :config
  (defun avy-goto-line-end-or-beginning (prefix)
    "By default move to end of line, with C-u move to the beginning"
    (interactive "P")
    (avy-goto-line)
    (if (equal prefix '(4))
	(back-to-indentation)
	(end-of-line))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
   ("C-c y" . yas-expand))
  :config
  (setq yas-snippet-dirs `("~/org/yasnippet"))
  (yas-reload-all))

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
  :bind
  (("C-M-/" . undo-tree-redo))
  :config (global-undo-tree-mode))

(global-set-key (kbd "C-;") 'comment-line)

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

;; (use-package shell
  ;; :ensure nil
  ;; :bind
  ;; (("C-x `" . shell)
   ;; ("C-c `" . shell))
  ;; (:map shell-mode-map
	;; ("M-r" . counsel-zsh-history)))
(defun zsh-ansi-term ()
  (interactive)
  (ansi-term "/bin/zsh"))
(global-set-key (kbd "C-x `") 'zsh-ansi-term)

(use-package dired-x
  :ensure nil
  :init
  (setq dired-dwim-target t)
  :bind
  (("C-x j" . dired-jump))
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-mode t)
  (setq dired-listing-switches "-alh"))

(use-package dired-narrow
  :bind
  (:map dired-mode-map
	("/" . dired-narrow)))

(use-package peep-dired
  :init
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
  (setq peep-dired-cleanup-eagerly nil)
  :bind
  (:map peep-dired-mode-map
	("p" . peep-dired-prev-file)
	("n" . peep-dired-next-file)
	("r" . dired-rotate)
   :map dired-mode-map
   ("P" . peep-dired))
   :config
   (defun dired-rotate ()
     (interactive)
     (shell-command "sh /home/tlegutko/rotate.sh" (dired-file-name-at-point))))

(global-set-key (kbd "M-V") 'scroll-other-window-down)

(use-package buffer-move
  :bind
  (("C-x 4 h" . buf-move-left)
   ("C-x 4 j" . buf-move-down)
   ("C-x 4 k" . buf-move-up)
   ("C-x 4 l" . buf-move-right)))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'global)
  (setq aw-minibuffer-flag t)
  :bind
  (([C-tab] . ace-window))
  :config
  (ace-window-display-mode))

(use-package winner
  :config
  (winner-mode 1))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-c C-<" . mc/insert-numbers)
  :map mc/keymap
  ([return] . nil)))

;; (use-package eshell
  ;; :commands eshell
  ;; :config
  ;; (add-hook 'eshell-mode-hook
            ;; (lambda ()
              ;; (define-key eshell-mode-map
                ;; (kbd "<tab>") 'eshell-pcomplete)
              ;; (define-key eshell-mode-map
                ;; (kbd "C-r")
                ;; 'counsel-esh-history)
              ;; (define-key eshell-mode-map
                ;; (kbd "M-r")
                ;; 'counsel-esh-history))))

;; tramp
(require 'tramp)
(add-to-list 'tramp-connection-properties
	    (list nil "remote-shell" "sh"))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/system/xbin")
(add-to-list 'tramp-remote-process-environment "TMPDIR=$HOME")

(use-package restclient
  :bind
  (:map restclient-mode-map
  ("C-c C-c" . restclient-http-send-current-stay-in-window)
  ("C-c C-v" . restclient-http-send-current)
  ("C-c n n" . nil)))

;; org-mode to github markdown export command
(use-package ox-gfm)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun copy-buffer-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c f") 'copy-buffer-name-to-clipboard)

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
		    (concat
		      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)
(global-set-key (kbd "C-c h") help-map)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)

(use-package wgrep)
(use-package async
  :config
  (dired-async-mode 1))

(delete-selection-mode 1)

(use-package gnuplot)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
   With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current
   buffer is not visiting a file."
  (interactive "P")
  (let ((current-char (point))
	(sudo-p (string-prefix-p "/sudo:root" buffer-file-name)))
    (if sudo-p
      (message "You're already editing file as root!")
      (if (or arg (not buffer-file-name))
          (find-file (concat "/sudo:root@localhost:"
                             (ido-read-file-name "Find file(as root): ")))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))
      (goto-char current-char))))

;; edit as root
(global-set-key (kbd "C-x !") 'sudo-edit)

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-format "%d-%m-%y %R ")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

(use-package exwm
  :config
  (require 'exwm-config)
  (require 'exwm-randr)
  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 5)
  (setq exwm-randr-workspace-output-plist
	'(0 "DP-4"
	  1 "DP-4"
	  2 "DP-1"
	  3 "DP-4"
	  4 "DP-1"
	  5 "DP-4"
	  6 "DP-1"
	  7 "DP-4"
	  8 "DP-1"
	  9 "DP-4"))
  (add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-4 --auto --primary --output DP-1 --auto --right-of DP-4")))
  (exwm-randr-enable)
  
  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  (defun start-chrome ()
    (interactive)
    (start-process "google-chrome-stable" nil "google-chrome-stable"))
  (defun nvidia-settings-2-displays ()
    (interactive)
    (shell-command "~/.config/scripts/nvidia-settings-2-displays.sh 1920x1080"))
  (defun scrot ()
    (interactive)
    (shell-command "scrot -s -e 'xclip -selection clipboard -t image/png $f && mv $f ~/Pictures/screenshots/'")
    (message "Screenshot taken"))
  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq scripts-dir '/home/tlegutko/.config/scripts)
  (setq exwm-input-global-keys
        `(
	  ([?\s-d] . counsel-linux-app)
	  ([?\s-q] . kill-this-buffer)
	  ([?\s-g] . start-chrome)
	  ([?\s-p] . scrot)
	  ([?\s-\M-i] . nvidia-settings-2-displays) ; this kinda works, but resets the display so many times that it's better left unused
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
    		     (interactive (list (read-shell-command "$ ")))
  		     (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
  		    (interactive)
  		    (start-process "" nil "/usr/bin/slock")))))
  
  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
  
  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))
  
  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line.
  ;; (setq exwm-workspace-minibuffer-position 'bottom)

  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)
  
  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable))
(put 'narrow-to-region 'disabled nil)

(fset 'magit-commit-tag
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 115 111 110 32 98 114 97 110 99 104 return 6 67108896 5 134217847 201326636 91 25 93 32] 0 "%d")) arg)))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

;; minimal rendering by default
(setq-default shr-inhibit-images t)   ; toggle with `I`
