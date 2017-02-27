;;; melpa repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;; look & feel
(load-theme 'zenburn t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; font is 1/10 of height
(set-face-attribute 'default nil :height 80)

;;; evil
(require 'evil)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(evil-mode 1)

;;; eval shortcut
(global-set-key (kbd "C-x E") 'eval-buffer)

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

;;; transpose windows func
(defun transpose-windows ()
  (interactive)
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))

;;; line numbers
(require 'linum-relative)
(linum-relative-on)
(setq linum-relative-current-symbol "")

;;; automatic custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
