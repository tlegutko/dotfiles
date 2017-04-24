(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-pdf "Okular")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(delete-selection-mode nil)
 '(global-visual-line-mode t)
 '(line-number-mode nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files
   (quote
    ("~/org/fap-journal.org" "~/org/miracle-morning.org" "~/org/personal-journal.org" "~/org/scala.org" "~/org/masters-thesis.org" "~/org/dance-notes.org")))
 '(package-selected-packages
   (quote
    (expand-region zenburn free-keys openwith auctex smex avy counsel keychain-environment magit evil-surround smartparens ensime use-package linum-relative evil zenburn-theme)))
 '(transient-mark-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
