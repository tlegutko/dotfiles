;;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;;; look & feel
(load-theme 'zenburn t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :height 80)
(require 'evil)
(evil-mode 1)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
