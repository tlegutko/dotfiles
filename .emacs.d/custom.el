(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exwm-input-global-keys
   (quote
    (([8388708]
      . counsel-linux-app)
     ([8388721]
      . kill-this-buffer)
     ([8388711]
      . start-chrome)
     ([8388720]
      . scrot)
     ([142606441]
      . nvidia-settings-2-displays)
     ([8388722]
      . exwm-reset)
     ([8388727]
      . exwm-workspace-switch)
     ([8388656]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 0))
     ([8388657]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 1))
     ([8388658]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 2))
     ([8388659]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 3))
     ([8388660]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 4))
     ([8388661]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 5))
     ([8388662]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 6))
     ([8388663]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 7))
     ([8388664]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 8))
     ([8388665]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 9))
     ([8388646]
      lambda
      (command)
      (interactive
       (list
	(read-shell-command "$ ")))
      (start-process-shell-command command nil command))
     ([s-f2]
      lambda nil
      (interactive)
      (start-process "" nil "/usr/bin/slock")))))
 '(org-agenda-files
   (quote
    ("~/org/shopping-wishlist.org" "~/org/svietlana-praca.org" "~/org/books-notes.org" "~/org/new-job.org" "~/org/daily-activities.org" "~/org/it-feedback.org" "~/org/miracle-evening.org" "~/org/laptop-config.org" "~/org/weekly-summary.org" "~/org/pmo-journal.org" "~/org/todo.org" "~/org/miracle-morning.org" "~/org/personal-journal.org" "~/org/scala.org" "~/org/dance-notes.org")))
 '(package-selected-packages
   (quote
    (ivy company sbt-mode exwm gnuplot emacs-async wgrep solaire-mode doom-themes ediff-wind markdown-mode ox-gfm restclient multiple-cursors evil-goggles popup-imenu comint peep-dired scala-mode ace-window buffer-move dired-narrow org-bullets dired-x dired sublimity-scroll ag company-mode org-gcal which-key counsel-projectile flx expand-region zenburn free-keys openwith auctex smex avy counsel keychain-environment magit evil-surround smartparens ensime use-package evil)))
 '(vc-annotate-background "#000000")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b6e63e")
    (cons 40 "#c4db4e")
    (cons 60 "#d3d15f")
    (cons 80 "#e2c770")
    (cons 100 "#ebb755")
    (cons 120 "#f3a73a")
    (cons 140 "#fd971f")
    (cons 160 "#fc723b")
    (cons 180 "#fb4d57")
    (cons 200 "#fb2874")
    (cons 220 "#f43461")
    (cons 240 "#ed404e")
    (cons 260 "#e74c3c")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1c1e1f" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width condensed :foundry "Bits" :family "DejaVu Sans Mono"))))
 '(ivy-current-match ((t (:background "#f57c00" :foreground "#222222" :box (:line-width 3 :color "#cc0000" :style released-button)))))
 '(ivy-minibuffer-match-face-1 ((t (:background "nil" :foreground "#999999"))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background "nil" :foreground "#fb2874" :weight semi-bold))))
 '(ivy-modified-buffer ((t (:inherit bold :foreground "#4e4e4e" :underline t))))
 '(ivy-subdir ((t (:box (:line-width 2 :color "#f57c00" :style released-button)))))
 '(ivy-virtual ((t (:inherit italic :foreground "#3f3f3f" :strike-through t)))))
