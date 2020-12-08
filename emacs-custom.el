(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auto-save-default nil)
 '(aw-keys '(97 115 100 102 103 104 106 107 108))
 '(backward-delete-char-untabify-method 'hungry)
 '(before-save-hook '(delete-trailing-whitespace))
 '(column-number-mode t)
 '(comint-prompt-read-only t nil nil "Read-only prompt (\">\" in ess-R)")
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-use-prompt-regexp nil nil nil "nil enables evil motions")
 '(command-log-mode-auto-show nil)
 '(command-log-mode-is-global t)
 '(command-log-mode-key-binding-open-log nil)
 '(command-log-mode-open-log-turns-on-mode nil)
 '(command-log-mode-window-size 50)
 '(company-box-doc-enable t)
 '(company-box-enable-icon t)
 '(company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend) nil nil "Rm company-echo-metadata-frontend to speed up candidate navigation")
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 0.2)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(counsel-bookmark-avoid-dired t)
 '(counsel-mode t nil nil "Remap common commands to counsel commands")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("0f0a885f4ce5b6f97e33c7483bfe4515220e9cbd9ab3ca798e0972f665f8ee4d" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "37f32706ffc6d7d021adf6b4d2a84eae7e0cfb7871cd39e21eaddc77c52bf4a7" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(delete-by-moving-to-trash t)
 '(dired-use-ls-dired t)
 '(display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)))
 '(eldoc-echo-area-use-multiline-p t nil nil "May not have an effect. Test on longer function signatures sometime.")
 '(electric-layout-mode t nil nil "Compare behavior with and without in ess-R. Without this, newlines within {} or () inserts two newlines between them, indents the first and moves cursor to it. Setting this seems to interfere with that behavior, which is controlled by ess-roxy-newline-and-indent. This is because ess-r-mode sets electric-layout-rules to insert a newline after {, but doesn't enable electric-layout-mode. Must be an oversight. If setting this globally, disable it in R to keep the desired behavior. ")
 '(electric-pair-mode t nil nil "Nice default behavior. Delete delimiter pair when deleting opening paren of empty pair (electric-pair-delete-adjacent-pairs). Skips over closing delim when you try to insert over an existing delim (electric-pair-skip-self). Inserts singles to preserve balance (electric-pair-preserve-balance). Insertion around active region, with point after whichever delim you typed--opening or closing. Example of cool defaults: In lisp comments, ` inserts `'.")
 '(electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (123 . 125)))
 '(ess-ask-for-ess-directory nil)
 '(ess-eldoc-abbreviation-style 'mild)
 '(ess-eldoc-show-on-symbol t nil nil "Show function signature in echo area when inside function and on symbol. May not show until first argument has been completed.")
 '(ess-eval-visibly nil)
 '(ess-indent-with-fancy-comments nil nil nil "I suspect this is the reason comments were forced toward the right margin in R scripts")
 '(ess-style 'RStudio)
 '(ess-use-company t)
 '(evil-default-state 'emacs)
 '(evil-emacs-state-modes nil)
 '(evil-escape-delay 0.2)
 '(evil-escape-key-sequence "kj")
 '(evil-escape-mode t)
 '(evil-highlight-closing-paren-at-point-states '(not emacs insert replace) nil nil "Highlight closing paren at point in normal, before point in listed modes")
 '(evil-insert-state-modes nil)
 '(evil-intercept-maps nil)
 '(evil-motion-state-modes nil nil nil "Read-only modes start in default mode (should be emacs)")
 '(evil-overriding-maps nil)
 '(evil-split-window-below t)
 '(evil-undo-system 'undo-fu)
 '(evil-vsplit-window-right t)
 '(evil-want-keybinding nil)
 '(fci-rule-color "#073642")
 '(gc-cons-threshold 100000000)
 '(global-display-fill-column-indicator-mode t)
 '(global-display-line-numbers-mode t)
 '(global-evil-mc-mode t)
 '(global-evil-surround-mode t)
 '(global-whitespace-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hydra-is-helpful nil nil nil "Disable in favor of which-key-show-transient-maps and which-key hacks")
 '(hydra-verbose t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-count-format "%d/%d " nil nil "display index #/total for ivy completion")
 '(ivy-extra-directories nil nil nil "BUG: C-u counsel-fzf, then C-j on candidate '..' prevents ivy from dispaying counsel-fzf results")
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-mode t nil nil "Enable ivy completion")
 '(ivy-prescient-mode t)
 '(ivy-use-virtual-buffers t)
 '(lsp-auto-guess-root t)
 '(lsp-completion-provider t)
 '(lsp-document-sync-method nil)
 '(lsp-eldoc-enable-hover nil)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-snippet nil)
 '(lsp-enable-text-document-color t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-log-io t)
 '(lsp-print-performance t)
 '(lsp-pyls-plugins-jedi-completion-include-params t)
 '(lsp-signature-auto-activate t)
 '(lsp-signature-doc-lines 10)
 '(lsp-signature-render-documentation t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-border "#93a1a1")
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-width 150)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol nil)
 '(lsp-ui-sideline-update-mode 'point)
 '(lua-indent-level 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-M-RET-may-split-line '((default)))
 '(org-agenda-include-diary t)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-agenda-window-setup 'current-window)
 '(org-attach-dir-relative t)
 '(org-capture-bookmark nil)
 '(org-catch-invisible-edits 'show)
 '(org-cycle-separator-lines 0)
 '(org-default-notes-file "~/.emacs.d/org/.notes")
 '(org-directory "~/.emacs.d/org")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-list-allow-alphabetical t)
 '(org-log-done 'time)
 '(org-log-into-drawer nil)
 '(org-log-redeadline 'time)
 '(org-log-reschedule 'time)
 '(org-mark-ring-length 20)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-pretty-entities t nil nil "Affects manually typed entities (e.g., \"\\theta\"). org-counsel-entities inserts the actual UTF-8 character.")
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-projectile-projects-file "projects.org")
 '(org-return-follows-link t)
 '(org-src-window-setup 'current-window)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("STARTED" . "yellow")
     ("DONE" . "green")
     ("WAITING" . "blue")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d)")))
 '(org-use-fast-todo-selection 'expert)
 '(osx-browse-mode t)
 '(package-selected-packages
   '(helm-org-rifle dash\.el helm org-rifle undo-fu ivy-prescient prescient zenburn benchmark-init diminish evil-org org-projectile company-box poly-R poly-markdown evil-nerd-commenter lsp-python-ms yasnippet smex dap-python dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode jupyter lua-mode osx-browse evil-mc evil-surround multiple-cursors key-chord company projectile counsel hercules company-tabnine 0x0 flx evil-escape ranger pkg aggressive-indent ess-R-data-view ess which-key use-package quelpa page-break-lines hydra help-fns+ helm-descbinds general evil-tutor dracula-theme ffcounsel command-log-mode ace-window))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(prescient-persist-mode t)
 '(ranger-deer-show-details nil)
 '(ranger-override-dired-mode t)
 '(ranger-show-hidden t)
 '(recentf-max-saved-items 100 nil nil "Affects number of candidates with ivy-use-virtual-buffers?")
 '(ring-bell-function 'ignore)
 '(safe-local-eval-forms
   '((add-hook 'write-file-hooks 'time-stamp)
     (add-hook 'write-file-functions 'time-stamp)
     (add-hook 'before-save-hook 'time-stamp nil t)
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
     (add-hook 'after-save-hook
               (lambda nil
                 (org-babel-tangle))
               nil t)))
 '(same-window-buffer-names '("*Help*"))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000000 nil nil "Seems to prevent auto-recentering of point when scrolling")
 '(show-paren-highlight-openparen nil)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-in-periphery nil)
 '(show-paren-when-point-inside-paren t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(text-mode-hook '(text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode t)
 '(treemacs-git-mode 'deferred)
 '(treemacs-is-never-other-window t)
 '(treemacs-user-mode-line-format 'none)
 '(treemacs-workspace-switch-cleanup 'all)
 '(use-package-verbose t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t nil nil "I want to follow symlinks to the actual file even if it is
version-controlled. I don't know what benefit settings this to nil confers. I
simply wanted to avoid the prompt in org-mode, where I've set the default file
attachment method to be a symbolic link.")
 '(visible-bell nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(which-key-allow-evil-operators nil)
 '(which-key-allow-imprecise-window-fit t)
 '(which-key-compute-remaps t nil nil "E.g. w/ counsel-mode: apropos-command -> counsel-apropos")
 '(which-key-idle-delay 0.2)
 '(which-key-max-description-length 100)
 '(which-key-mode t)
 '(which-key-popup-type 'side-window)
 '(which-key-prefix-prefix "+")
 '(which-key-separator " ")
 '(which-key-show-docstrings t)
 '(which-key-show-operator-state-maps nil nil nil "Enabling leads to rapid timeout for evil (e.g., 10dj or d10j)")
 '(which-key-show-transient-maps t nil nil "See modified which-key--update")
 '(which-key-side-window-location 'bottom)
 '(which-key-side-window-max-height 0.1)
 '(which-key-sort-order 'which-key-key-order-alpha)
 '(which-key-sort-uppercase-first nil)
 '(whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
 '(whitespace-style '(face trailing tabs tab-mark newline-mark))
 '(winner-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36")))))
