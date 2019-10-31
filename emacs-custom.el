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
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend)) nil nil "Rm company-echo-metadata-frontend to speed up candidate navigation")
 '(company-global-modes nil nil nil "TODO: Change nil to '(ess-r-mode)")
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 0.2 nil nil "Min # chars before completion")
 '(company-show-numbers t nil nil "Use M-1, etc., to select candidates")
 '(confirm-kill-processes nil)
 '(counsel-bookmark-avoid-dired t)
 '(counsel-mode t nil nil "Remap common commands to counsel commands")
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("37f32706ffc6d7d021adf6b4d2a84eae7e0cfb7871cd39e21eaddc77c52bf4a7" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(delete-by-moving-to-trash t)
 '(eldoc-echo-area-use-multiline-p t nil nil "May not have an effect. Test on longer function signatures sometime.")
 '(ess-ask-for-ess-directory nil)
 '(ess-eldoc-abbreviation-style (quote mild))
 '(ess-eldoc-show-on-symbol t nil nil "Show function signature in echo area when inside function and on symbol. May not show until first argument has been completed.")
 '(ess-eval-visibly nil)
 '(ess-indent-with-fancy-comments nil nil nil "I suspect this is the reason comments were forced toward the right margin in R scripts")
 '(ess-style (quote RStudio))
 '(ess-use-company nil)
 '(evil-default-state (quote emacs))
 '(evil-emacs-state-modes nil)
 '(evil-escape-delay 0.2)
 '(evil-escape-key-sequence "jk")
 '(evil-insert-state-modes nil)
 '(evil-intercept-maps nil)
 '(evil-motion-state-modes nil nil nil "Read-only modes start in default mode (should be emacs)")
 '(evil-overriding-maps nil)
 '(evil-want-keybinding nil)
 '(global-display-line-numbers-mode t)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(hydra-hint-display-type (quote lv))
 '(hydra-is-helpful nil nil nil "see which-key-show-transient-maps")
 '(inhibit-startup-screen t)
 '(ivy-count-format "%d/%d " nil nil "display index #/total for ivy completion")
 '(ivy-extra-directories (quote ("../")))
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-mode t nil nil "Enable ivy completion")
 '(ivy-use-virtual-buffers t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (company projectile counsel hercules company-tabnine 0x0 flx evil-escape ranger pkg aggressive-indent ess-R-data-view ess which-key use-package quelpa page-break-lines hydra help-fns+ helm-descbinds general evil-tutor dracula-theme ffcounsel command-log-mode ace-window)))
 '(recentf-max-saved-items 100 nil nil "Affects number of candidates with ivy-use-virtual-buffers?")
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000000 nil nil "Seems to prevent auto-recentering of point when scrolling")
 '(tab-always-indent (quote complete))
 '(tool-bar-mode nil)
 '(use-package-verbose t)
 '(which-key-allow-evil-operators t)
 '(which-key-compute-remaps t nil nil "e.g. w/ counsel-mode: apropos-command -> counsel-apropos")
 '(which-key-idle-delay 0.2)
 '(which-key-max-description-length 100)
 '(which-key-mode t)
 '(which-key-popup-type (quote side-window))
 '(which-key-prefix-prefix "+")
 '(which-key-separator " ")
 '(which-key-show-docstrings t)
 '(which-key-show-operator-state-maps t)
 '(which-key-show-transient-maps t nil nil "see modified which-key--update")
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





