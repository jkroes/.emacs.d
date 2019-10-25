;; See:
;; Customize section of emacs
;; https://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
;; https://stackoverflow.com/questions/22915019/emacs-setq-before-loading

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-keys (quote (97 115 100 102 103 104 106 107 108)))
 '(comint-prompt-read-only t nil nil "Read-only prompt (\">\" in ess-R)")
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-use-prompt-regexp nil nil nil "nil enables evil motions")
 '(company-global-modes nil)
 '(confirm-kill-processes nil)
 '(counsel-bookmark-avoid-dired t)
 '(counsel-mode t nil nil "Remap common commands to counsel commands")
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(delete-by-moving-to-trash t)
 '(evil-default-state (quote emacs))
 '(evil-emacs-state-modes nil)
 '(evil-escape-delay 0.2)
 '(evil-escape-key-sequence "jk")
 '(evil-insert-state-modes nil)
 '(evil-intercept-maps nil)
 '(evil-motion-state-modes nil nil nil "Read-only modes start in default mode (should be emacs)")
 '(evil-overriding-maps nil)
 '(evil-want-keybinding nil)
 '(global-command-log-mode t)
 '(global-display-line-numbers-mode t)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(hydra-hint-display-type (quote lv))
 '(hydra-is-helpful t)
 '(inhibit-startup-screen t)
 '(ivy-count-format "%d/%d " nil nil "display index #/total for ivy completion")
 '(ivy-extra-directories (quote ("../")))
 '(ivy-height 10)
 '(ivy-initial-inputs-alist (quote ((nil . ""))))
 '(ivy-mode t nil nil "Enable ivy completion")
 '(ivy-use-virtual-buffers t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (hercules company-tabnine 0x0 flx evil-escape ranger pkg aggressive-indent ess-R-data-view ess which-key use-package quelpa page-break-lines hydra help-fns+ helm-descbinds general evil-tutor dracula-theme ffcounsel command-log-mode ace-window)))
 '(recentf-max-saved-items 100 nil nil "Affects number of candidates with ivy-use-virtual-buffers?")
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000000 nil nil "Seems to prevent auto-recentering of point when scrolling")
 '(tool-bar-mode nil)
 '(which-key-allow-evil-operators t)
 '(which-key-compute-remaps t nil nil "e.g. w/ counsel-mode: apropos-command -> counsel-apropos")
 '(which-key-idle-delay 0.2)
 '(which-key-max-description-length nil)
 '(which-key-mode t)
 '(which-key-popup-type (quote side-window))
 '(which-key-prefix-prefix "+")
 '(which-key-separator " ")
 '(which-key-show-docstrings t)
 '(which-key-show-operator-state-maps t)
 '(which-key-side-window-location (quote bottom))
 '(which-key-side-window-max-height 0.2)
 '(which-key-sort-order (quote which-key-key-order-alpha))
 '(which-key-sort-uppercase-first nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
