;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/raxod502/straight.el#integration-with-use-package
;; https://github.com/raxod502/straight.el#integration-with-use-package-1
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; https://github.com/jwiegley/use-package#diminishing-and-delighting-minor-modes
(straight-use-package 'diminish)

;; Customizations unrelated to straight-managed packages
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
;; (setq custom-file null-device)

;; https://github.com/jwiegley/use-package/pull/881
(defun customize-package (&rest args)
  (let ((custom--inhibit-theme-enable nil)) ; Apply changes to theme immediately
    (apply 'custom-theme-set-variables 'my-theme args)))

(defun customize-package-face (&rest args)
  (let ((custom--inhibit-theme-enable nil)) ; Apply changes to theme immediately
    (apply 'custom-theme-set-faces 'my-theme args)))

(deftheme my-theme)
;; Assign my-theme second-highest precedence (after user theme). Note that load-theme invocations lower down will set
;; the loaded theme to a higher precedence
(enable-theme 'my-theme)

(setq load-path (cons "~/.emacs.d/lisp" load-path))

(setq-default default-directory "~/.emacs.d")

(set-frame-font "Hack 10" nil t)

(when (string-match "Linux.*Microsoft.*Linux"
                    (shell-command-to-string "uname -a"))
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic))

(straight-use-package 'solarized-theme)
(load-theme 'solarized-dark t)

(defun no-auto (&optional wrap-mode)
  "Disable auto fill and indicator for specific modes"
  (turn-off-auto-fill) ; (auto-fill-mode -1)
  (display-fill-column-indicator-mode -1)
  (if wrap-mode
      (funcall wrap-mode)))

;; Default to hard wrapping at lines at column 80 in all modes
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 120)

(add-hook 'org-mode-hook (lambda () (no-auto 'visual-line-mode)))
(add-hook 'markdown-mode-hook (lambda () (no-auto 'visual-line-mode)))
(add-hook 'term-mode-hook (lambda () (no-auto) (toggle-truncate-lines 1))) ; Fish shell in ansi-term
(add-hook 'custom-mode-hook (lambda () (no-auto)))

(straight-use-package 'page-break-lines)

(customize-package
 '(command-log-mode-auto-show nil)
 '(command-log-mode-is-global t)
 '(command-log-mode-key-binding-open-log nil)
 '(command-log-mode-open-log-turns-on-mode nil)
 '(command-log-mode-window-size 50))

(straight-use-package 'command-log-mode)
(command-log-mode)
(diminish 'command-log-mode)

;; Auto-scroll buffer as commands are logged
(add-hook 'command-log-mode-hook (lambda () (set (make-local-variable 'window-point-insertion-type) t)))

(customize-package
 '(aw-keys '(97 115 100 102 103 104 106 107 108)))

;; Larger ace-window letters (https://oremacs.com/2015/02/27/ace-window-leading-char/)
(customize-package-face
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(straight-use-package 'ace-window)

(straight-use-package 'general)

(general-create-definer my-leader
  :states '(motion insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

;; Top-level mappings
(my-leader
  "" nil ; Unbinding the prefix itself prevents errors about binding to non-prefix keys somehow
  "." 'clm/toggle-command-log-buffer
  "&" 'async-shell-command
  "e" 'eshell
  "f" '(:prefix-command my/files-map :wk "files")
  "t" 'ansi-term)

;; Files
(general-def my/files-map
  :wk-full-keys nil ; Allows for consistent wk replacement text during cyclical map navigation
  "b" '(:prefix-command my/bookmarks-map :wk "bookmarks")
  "i" 'insert-file)

;; Bookmarks
(general-def my/bookmarks-map
  :wk-full-keys nil
  "d" 'bookmark-delete
  "e" 'edit-bookmarks
  ;; For each map referencing `my/files-map' we need `:wk' "files"
  "f" '(my/files-map :wk "files")
  "r" 'bookmark-rename
  "s" 'bookmark-set)

;; Help
(general-unbind help-map "C-d" "s" "B" "C" "L" "g" "h" "n" "M-c" "RET" "C-n" "C-p" "C-t" "C-\\")
(general-def help-map "M" 'describe-minor-mode)

(customize-package
 '(which-key-idle-delay 0.2)
 '(which-key-show-docstrings t)
 '(which-key-allow-evil-operators nil)
 '(which-key-allow-imprecise-window-fit t)
 '(which-key-compute-remaps t) ; E.g. w/ counsel-mode: apropos-command -> counsel-apropos
 '(which-key-max-description-length 100)
 '(which-key-popup-type 'side-window)
 '(which-key-side-window-location 'bottom)
 '(which-key-prefix-prefix "+")
 '(which-key-separator " ")
 '(which-key-show-operator-state-maps nil) ; Enabling leads to rapid timeout for evil (e.g., 10dj or d10j)
 '(which-key-show-transient-maps t) ; See modified which-key--update in my fork
 '(which-key-side-window-max-height 0.1)
 '(which-key-sort-order 'which-key-key-order-alpha)
 '(which-key-sort-uppercase-first nil))

(straight-use-package
 '(which-key :type git :host github :repo "justbur/emacs-which-key"
             :fork (:host github :repo "jkroes/emacs-which-key")))

(which-key-mode)
(diminish 'which-key-mode)

(general-def :keymaps 'help-map
  "C-h" nil ; Enable which-key navigation of help-map bindings
  "C-w" 'which-key-show-keymap)

(defun disable-transient-map-temporarily (orig-fun &rest args)
  "Hide which-key popup for certain transient maps"
  (if (bound-and-true-p which-key-show-transient-maps)
      (progn
        (setq which-key-show-transient-maps nil)
        (apply orig-fun args)
        (setq which-key-show-transient-maps t))
    (apply orig-fun args)))

(customize-package
 '(hydra-verbose t)
 '(hydra-is-helpful t))

(straight-use-package 'hydra)

(my-leader
  "b" 'hydra-buffer/body
  "w" 'hydra-window/body)

;; Required order of operations:
;; 1. Modify hydra-base-map
;; 2. defhydra evaluation
;; 3. defhydra+ evaluation
;; 4. my/defhydra
;; Some defhydra(+) rely on with-eval-after-load. To ensure my/defhydra is evaluated last, these are loaded after all
;; packages that contribute functions/heads to these hydras.
;; NOTE: I had issues surrounding defhydra+ with with-eval-after-load declarations.
(defun counsel-hydra-integrate (old-func &rest args)
  "Function used to advise `counsel-hydra-heads' to work with blue and amranath hydras."
  (hydra-keyboard-quit)
  (apply old-func args)
  (funcall-interactively hydra-curr-body-fn))

(defun ess-r-mode-hydras ()
  "Hook for ess-r-mode. The functions used as hydra heads do not exist until an ess-r-mode buffer exists, so
      my/defhydra must be called after that buffer is created."
  (my/defhydra 'hydra-r)
  (my/defhydra 'hydra-r-help)
  (my/defhydra 'hydra-r-eval)
  (my/defhydra 'hydra-r-debug)) ; Display hydras in which-key

(defun treemacs-mode-hydras ()
  "Can't simply call my/defhydra2 on treemacs-helpful-hydra, because the function that toggles the hydra also
      creates the hydra on its first invocation."
  (treemacs-helpful-hydra)
  (my/defhydra2 'treemacs--helpful-hydra)
  ;; So the hydra doesn't show the first time treemacs is called
  (treemacs--helpful-hydra/nil))

(advice-add 'counsel-hydra-heads :around 'counsel-hydra-integrate)
(general-def hydra-base-map "." 'counsel-hydra-heads)
(load "my-hydras")

(with-eval-after-load "which-key"
  (customize-package '(hydra-is-helpful nil)) ; Disable in favor of which-key-show-transient-maps and which-key hacks
  (load "which-key-hacks")
  (add-hook 'ess-r-mode-hook 'ess-r-mode-hydras)
  (add-hook 'treemacs-mode-hook 'treemacs-mode-hydras)
  ;; (with-eval-after-load "ivy-hydra" (my/defhydra 'hydra-ivy))
  (with-eval-after-load "counsel"
    (with-eval-after-load "evil"
      (with-eval-after-load "ace-window"
        (my/defhydra 'hydra-buffer)
        (my/defhydra 'hydra-window)))))

(setq ivy-re-builders-alist '((swiper . ivy--regex)
                              (t . ivy--regex-fuzzy))
      ivy-help-file "~/.emacs.d/ivy-help.org")

(customize-package
 '(ivy-count-format "%d/%d ")
 ;; BUG: C-u counsel-fzf, then C-j on candidate '..' prevents ivy from dispaying counsel-fzf results
 '(ivy-extra-directories nil)
 ;; View more results in minibuffer
 '(ivy-height 25)
 ;; Don't anchor beginning of regex queries implicitly
 '(ivy-initial-inputs-alist nil)
 ;; Separates functionality for ivy-switch-buffer and counsel-recentf
 '(ivy-use-virtual-buffers nil)
 ;; For counsel-find-file. See also remapping of ivy-done and ivy-alt-done
 '(counsel-bookmark-avoid-dired t)
 ;; Affects counsel-recentf. Possibly also ivy-switch-buffer with ivy-use-virtual-buffers enabled.
 '(recentf-max-saved-items 100))

;; Repo is swiper, but built as three separate packages: ivy, counsel, swiper
(straight-use-package 'counsel)
;; Straight splits repos/swiper into build/ivy, build/counsel, and build/swiper. It omits a number of files, including
;; ivy-hydra.el. I do not know all such packages, but I do know I need to install ivy-hydra separately. NOTE: To see
;; recipes for each, use straight-get-recipe
(straight-use-package 'ivy-hydra) ; provides hydra-ivy
(diminish 'counsel-mode)
(ivy-mode)
(counsel-mode)

(my-leader "SPC" 'execute-extended-command)
(general-def :keymaps 'help-map "s" 'describe-symbol)
(general-def :keymaps 'my/files-map
  ;; TODO: Add an action to change dir similar to C-u
  "f" 'counsel-fzf
  ;; https://beyondgrep.com/feature-comparison/
  "g" 'counsel-rg
  "m" 'counsel-recentf
  "s" 'swiper)
(general-def :keymaps 'my/bookmarks-map
  "D" 'counsel-bookmarked-directory
  ;; TODO: Customize counsel-bookmark action list to include delete, rename, and set
  "j" 'counsel-bookmark)
(general-def :keymaps 'ivy-minibuffer-map
  "M-m" 'ivy-mark
  "M-u" 'ivy-unmark
  ;; For counsel-find-file, RET should add dir to search path instead of pulling up dired
  [remap ivy-done] 'ivy-alt-done
  [remap ivy-alt-done] 'ivy-done)

(customize-package '(smex-history-length 100)) ; Longer command history for sorting counsel-M-x results
(straight-use-package 'smex) ; For counsel-M-x

(straight-use-package 'flx) ; For anything using ivy--regex-fuzzy

;; Dependencies for source build of helm
;; (straight-use-package 'async)
;; (straight-use-package 'popup)

;; Source installation, per helm recommendation
;; - https://github.com/emacs-helm/helm/wiki#from-source (installation)
;; - https://github.com/emacs-helm/helm/wiki#if-installed-from-source (configuration)
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/helm/") ; facultative when installed with make install
;; (require 'helm-config)
;; (my-leader
;;   "SPC" 'helm-M-x)
;; (use-package helm-fzf
;;   :straight (helm-fzf :type git :host github :repo "ibmandura/helm-fzf"))
;; From emacs-helm.sh
;; (helm-mode 1)
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)
;; (define-key global-map [remap apropos-command] 'helm-apropos)
;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
;; (add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))

;; (use-package dash)
;; (use-package f)
;; (use-package s)
;; (use-package helm-org-rifle)

(setq evil-normal-state-modes
      '(lisp-interaction-mode                         ; *scratch*
        messages-buffer-mode
        emacs-lisp-mode
        python-mode
        ess-r-mode
        sh-mode
        fish-mode
        markdown-mode
        fundamental-mode
        lua-mode
        org-mode
        SAS-mode)
      evil-insert-state-modes
      '(inferior-ess-r-mode))

(customize-package
 '(evil-default-state 'emacs)
 '(evil-emacs-state-modes nil)
 ;; Highlight closing paren at point in normal, before point in listed modes
 '(evil-highlight-closing-paren-at-point-states '(not emacs insert replace))
 '(evil-insert-state-modes nil)
 '(evil-intercept-maps nil)
 ;; Read-only modes start in default mode (should be emacs)
 '(evil-motion-state-modes nil nil nil)
 '(evil-overriding-maps nil)
 '(evil-split-window-below t)
 '(evil-undo-system 'undo-fu)
 '(evil-vsplit-window-right t)
 '(evil-want-keybinding nil))

(straight-use-package 'evil)
(evil-mode)

;; (defalias 'evil-insert-state 'evil-emacs-state)    ; Alternative to disabling insert-state bindings
(advice-add 'evil-search-forward :around 'disable-transient-map-temporarily)
(advice-add 'evil-search-backward :around 'disable-transient-map-temporarily)

;; For evil undo/redo operations
(straight-use-package 'undo-fu)

(customize-package
 '(evil-escape-delay 0.2)
 '(evil-escape-mode t)
 '(evil-escape-key-sequence "kj"))

(straight-use-package 'evil-escape)
(evil-escape-mode)
(diminish 'evil-escape-mode)

;; (straight-use-package evil-surround)

(defun counsel-imenu-comments ()
  "Use counsel to display comments in current buffer"
  (interactive)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (unless (featurep 'counsel) (require 'counsel)) ; !Alternative to with-eval-after-load?!
    (counsel-imenu)))

(straight-use-package 'evil-nerd-commenter)

(my-leader
  "c" '(:ignore t :wk "comments")
  "cc" 'evilnc-comment-or-uncomment-lines
  "cC" 'evilnc-copy-and-comment-lines
  "ci" 'counsel-imenu-comments
  ;; When given C-u <n>, will forward-match <n> against the rightmost
  ;; digits of each line. E.g., on line 160, C-u <72> will target lines
  ;; 160-172
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cy" 'evilnc-comment-and-kill-ring-save
  ;; Whether empty lines can be commented as part of a selection
  "ce" 'evilnc-toggle-comment-empty-lines
  ;; When toggled off, all lines in a selection are commented if any
  ;; uncommented lines are included. Note that blank lines never count
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "c," 'evilnc-comment-operator
  "c." 'evilnc-copy-and-comment-operator)

(customize-package
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
 '(org-hide-emphasis-markers t)
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
 '(org-use-fast-todo-selection 'expert))

(customize-package
 '(org-structure-template-alist '(("e" . "src emacs-lisp :tangle yes")
                                  ("r" . "src R :tangle no")
                                  ("p" . "src python :tangle no"))))

(if (file-directory-p "/mnt/d/org") ; Using work PC
    (setq org-agenda-files (append org-agenda-files "mnt/d/org")))

(load "my-org-functions.el")
(add-hook 'org-after-todo-statistics-hook 'my/org-summary-todo)

(my-leader "o" '(:prefix-command my/global-org-map :wk "org-global"))
(general-def my/global-org-map
  :wk-full-keys nil
  "a" 'org-agenda ; Dispatcher
  "c" 'counsel-org-capture ; Capture to org-default-notes-file
  "e" 'counsel-org-entity ; https://orgmode.org/manual/Special-Symbols.html
  "l" 'org-insert-link-global
  "o" 'org-open-at-point-global)

(my-leader :keymaps 'org-mode-map "m" 'my/org-map)
(general-define-key
 :prefix-command 'my/org-map
 ;; Highly varied. For list items, with prefix create checkbox else toggle
 ;; May affect multiple lines if on bullet point of outermost sublist's first
 ;; item. For cookies, update statistics.
 "SPC" 'org-ctrl-ctrl-c
 "." 'org-time-stamp ; Create or update existing timestamp
 "," 'org-insert-structure-template ; E.g. src block
 "d" 'org-deadline ; Insert deadline keyword with timtestamp
 "f" 'counsel-org-file ; Show attachments for current file
 ;; Not clear what the diff is b/w counsel-org-goto and counsel-org-goto-all,
 ;; except taht that latter produces more candidates
 "g" 'counsel-org-goto-all
 "s" 'org-schedule ; Insert schedule keyword with timestamp
 "!" 'org-time-stamp-inactive
 "I" 'org-clock-in
 "O" 'org-clock-out
 "Q" 'org-clock-cancel
 "^" 'org-sort ; Sort headings or list items
 "*" 'org-ctrl-c-star ; Complex (de)convert/toggle to heading
 "@" 'org-mark-subtree ; I was too lazy to look at yanking/pasting
 ;; Complex convert to list item(s) or cycle list level through bullet types
 "-" 'org-ctrl-c-minus
 "A" 'org-toggle-archive-tag ; Tag subtrees as non-tab-expandable
 "a" 'org-attach
 ;; Insert link or edit invisible URL portion of existing link with a
 ;; description. Backspace at beginning or end of displayed description will
 ;; remove start or end brackets, revealing the invisble portion of the link.
 ;; Selected text when inserting becomes link description.
 "l" 'org-insert-link
 "n" 'org-next-link
 ;; When calling in org file, link points to the current headline of file. For
 ;; other files, points to current line.
 "S" 'org-store-link
 ;; Headings whose parent has this property can not be marked done until
 ;; siblings on earlier lines are done
 "o" 'org-toggle-ordered-property
 ;; Cycle keywords. If switching from TODO to DONE for a repeating task, update
 ;; the timestamp by the amount of the repeater, and reset the keyword to
 ;; TODO. In contrast, C-- 1 C-c C-t permanently finishes the repeating
 ;; task. Repeating tasks are indicated as e.g. +5d, while alerts/reminders as
 ;; e.g. -4m. If you miss several due dates, you may want to update the
 ;; timestamp only once for all of these missed deadlines to a future date. This
 ;; requires ++ instead of +. The .+ repeater likewise updates to a future date,
 ;; but the new timestamp is relative to the completion time rather than the
 ;; timestamp. Both deadlines and schedules can have repeaters.
 "t" 'org-todo
 ;; Cycle heading keywords or list bullet types, or change timestamp by a day
 "H" 'org-shiftleft
 "L" 'org-shiftright
 ;; Move between list items of the same level
 "J" 'org-shiftdown
 "K" 'org-shiftup)

(general-def :keymaps 'org-mode-map
  "<C-tab>" 'org-force-cycle-archived
  "M-h" 'org-metaleft ; Promote/dedent heading/list item
  "M-l" 'org-metaright ; Demote/indent heading/list item
  "M-j" 'org-shiftmetadown ;; Move heading or list item down
  "M-k" 'org-shiftmetaup
  "M-H" 'org-shiftmetaleft ;; Promote/dedent subtrees/sublists
  "M-L" 'org-shiftmetaright
  "M-J" 'org-metadown ;; Move subtree/sublist up/down
  "M-K" 'org-metaup
  ;; Respects lists when filling
  "M-q" 'org-fill-paragraph)

(general-def :keymaps 'org-mode-map :states 'motion
  "RET" 'my/org-open-at-point-in-emacs
  "g" '(:ignore t :wk "Entry navigation")
  "gh" 'outline-previous-visible-heading
  "gl" 'outline-next-visible-heading
  "gk" 'org-backward-heading-same-level
  "gj" 'org-forward-heading-same-level)

;; Does not work as part of general-def. May be related to this:
;; https://github.com/noctuid/general.el#why-dont-some-evil-keybindings-work-immediately
(evil-define-key 'normal org-mode-map
  "Reverses my/org-open-at-point-in-emacs"
  (kbd "DEL") 'org-mark-ring-goto)
  ;; (lambda ()
  ;;   (interactive)
  ;;   (if (equal 1 (length (seq-uniq (cl-subseq org-mark-ring 0
  ;;                                             org-mark-ring-length))))
  ;;       (evil-backward-char)
  ;;     (org-mark-ring-goto))))

(customize-package
 '(company-frontends
   ;; Remove company-echo-metadata-frontend to speed up candidate navigation
   '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 0.2)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20))

(straight-use-package 'company)

(customize-package
 '(company-box-doc-enable t)
 '(company-box-enable-icon t))

(straight-use-package 'company-box)
(diminish 'company-box-mode)

(general-def :keymaps 'company-mode-map
  "<tab>" 'company-indent-or-complete-common)

(general-def :keymaps 'company-active-map
  "C-h" 'mode-specific-C-h
  "M-n"  nil
  "M-p"  nil
  "C-n"  'company-select-next
  "C-p"  'company-select-previous)

(defun mode-specific-C-h ()
  "Programming language-specific help for company-active-map"
  (interactive)
  (pcase major-mode
    ('ess-r-mode (show-company-doc-as-ess-help))
    (_ (company-show-doc-buffer))))

(defun show-company-doc-as-ess-help ()
  "Show ess help if available, else show company help"
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (obj-help (ess-display-help-on-object selected)))
    (unless obj-help
      (company-show-doc-buffer))))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-box-mode)
(add-hook 'ess-r-mode-hook 'company-mode)

;; I disabled ess-r-mode's use of company-box, because the latter would prompt for resolution between objects of the
;; same name in different packages, which was extremely aggravating when using the tidyverse.
;; (add-hook 'ess-r-mode-hook 'company-box-mode)

;; When ess-eldoc-mode is enabled, it vanishes the company completion menu, at least with company-box enabled.
;; Did it affect lsp-based company completion? Test this if you ever reenable lsp-mode for ess-r.
;; (ess-r-mode . (lambda () (setq ess-eldoc-mode -1)))

(general-define-key
 :prefix-command 'my/elisp-map
 "c" 'check-parens            ; Debugging "End of file during parsing"
 ;; evals outermost expression containing or following point
 ;; ...and forces reset to initial value within a defvar,
 ;; defcustom, and defface expressions
 "d" 'eval-defun
 "m" 'pp-eval-expression      ; "m" for minibuffer, where exp is evaluated
 "s" 'pp-eval-last-sexp       ; evals expression preceding point
 "i" 'eval-print-last-sexp    ; "i" for insert(ing result)
 "r" 'eval-region)

(my-leader :keymaps 'emacs-lisp-mode-map "m" 'my/elisp-map)

(setq ess-nuke-trailing-whitespace-p t
      ;; ess-S-quit-kill-buffers-p 'ask
      inhibit-field-text-motion nil) ; prompt acts as beginning of line if prompt is read-only

(customize-package
 '(comint-prompt-read-only t) ; Read-only prompt (\">\" in ess-R)
 '(comint-use-prompt-regexp nil) ; nil enables evil motions
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(ess-ask-for-ess-directory nil)
 '(ess-eldoc-abbreviation-style 'mild)
 ;; Show function signature in echo area when inside function and on symbol. May not show until first argument has been
 ;; completed.
 '(ess-eldoc-show-on-symbol t)
 '(ess-eval-visibly nil)
 ;; I suspect this is the reason comments were forced toward the right margin in R scripts
 '(ess-indent-with-fancy-comments nil)
 '(ess-use-company t) ; How does this differ from adding company-mode to ess-r-mode-hook???
 '(ess-style 'RStudio))

(straight-use-package 'ess)

(my-leader :keymaps 'ess-r-mode-map "m" 'hydra-r/body)

(add-hook 'ess-r-mode-hook 'config-ess-r-mode)

(defun config-ess-r-mode ()
  (ess-set-style 'RStudio)
  ;; (setq-local ess-indent-offset 4) ; RStudio style uses a value of 2

  ;; Rely on electric-pair-mode instead of skeleton
  (local-set-key (kbd "{") 'self-insert-command)
  (local-set-key (kbd "}") 'self-insert-command)

  ;; electric-layout-rules interferes with ess-roxy-newline-and-indent
  ;; if electric-layout-mode is enabled (it is not by default)
  (setq-local electric-layout-rules nil))

;; Override Windows' help_type option of "html", to open help in help buffer, not browser (see contents of .Rprofile)
(pcase system-type
  ('windows-nt
   ;; iESS searches the paths listed in the variable exec-path for inferior-ess-r-program
   (add-to-list 'exec-path "c:/Users/jkroes/Documents/R/R-3.6.2/bin")
   ;; Sets R_USER and R_LIBS_USER
   (setenv "R_USER" "c:/Users/jkroes/Documents")
   ;; run-ess-r fails when this is set to Rterm
   (setq inferior-ess-r-program "R")
   (setenv "R_PROFILE_USER" "C:/Users/jkroes/.emacs.d/.Rprofile")
   ;; RStudio downloads pandoc with rmarkdown, but outside of RStudio
   ;; you need to notify R of the executable's directory
   (setenv "RSTUDIO_PANDOC" "C:/Users/jkroes/AppData/Local/Pandoc"))
  ('darwin (setenv "R_PROFILE_USER" "~/.emacs.d/.Rprofile")))

;; Disabling this while I render Word documents from Rmarkdown.
(customize-package '(polymode-display-output-file nil))

(straight-use-package 'poly-markdown)

;; NOTE: ess-r configuration and bindings are available inside chunks, where R-mode is active
;; I have bound polymode-export (render) to SPC-m-e-k
(straight-use-package 'poly-R)

;; Prevent window displaying company documentation buffer from vanishing when invoking a binding not in
;; company--electric-commands
;; (defun forget-saved-window-config ()
;;   (setq company--electric-saved-window-configuration nil))
;; (advice-add 'company-pre-command :before 'forget-saved-window-config)

(straight-use-package 'fish-mode)

;; Modified ansi-term to avoid read-from-minibuffer prompt
(setq explicit-shell-file-name "/usr/bin/fish")

(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(global-set-key  (kbd "\C-x c") 'clear-shell)

;; TODO: Throws an error on window. Commenting out in the meantime.

;; (defun ansi-term (program &optional new-buffer-name)
;;   "Start a terminal-emulator in a new buffer.
;; This is almost the same as `term' apart from always creating a new buffer,
;; and `C-x' being marked as a `term-escape-char'."
;; (interactive (list (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))

;;   ;; Pick the name of the new buffer.
;;   (setq term-ansi-buffer-name
;;     (if new-buffer-name
;;         new-buffer-name
;;       (if term-ansi-buffer-base-name
;;           (if (eq term-ansi-buffer-base-name t)
;;           (file-name-nondirectory program)
;;         term-ansi-buffer-base-name)
;;         "ansi-term")))

;;   (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

;;   ;; In order to have more than one term active at a time
;;   ;; I'd like to have the term names have the *term-ansi-term<?>* form,
;;   ;; for now they have the *term-ansi-term*<?> form but we'll see...

;;   (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
;;   (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

;;   (set-buffer term-ansi-buffer-name)
;;   (term-mode)
;;   (term-char-mode)

;;   ;; Historical baggage.  A call to term-set-escape-char used to not
;;   ;; undo any previous call to t-s-e-c.  Because of this, ansi-term
;;   ;; ended up with both C-x and C-c as escape chars.  Who knows what
;;   ;; the original intention was, but people could have become used to
;;   ;; either.   (Bug#12842)
;;   (let (term-escape-char)
;;     ;; I wanna have find-file on C-x C-f -mm
;;     ;; your mileage may definitely vary, maybe it's better to put this in your
;;     ;; .emacs ...
;;     (term-set-escape-char ?\C-x))

;;   (switch-to-buffer term-ansi-buffer-name))

(straight-use-package 'all-the-icons)

;; Customize node was showing nothing when expanded. Literally empty space. Set here instead.
;; To see other values for this setting, press "o" over a file node in treemacs.
(setq treemacs-default-visit-action 'treemacs-visit-node-ace)

(customize-package
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode t)
 '(treemacs-git-mode 'deferred)
 '(treemacs-is-never-other-window t)
 '(treemacs-user-mode-line-format 'none)
 '(treemacs-workspace-switch-cleanup 'all))

(straight-use-package 'treemacs)
(require 'treemacs)

(add-hook 'treemacs-mode (lambda () (display-line-numbers-mode -1)))

(autoload 'treemacs-load-theme "treemacs-themes")
(load "doom-themes-ext-treemacs") ; See treemacs-create-theme declarations
(treemacs-load-theme "doom-colors")

(general-define-key :keymaps '(motion insert emacs)
  "C-\\" 'treemacs
  "C-|" 'treemacs-select-window)

(general-define-key :keymaps 'my/files-map
  "p" '(:keymap treemacs-project-map)
  "w" '(:keymap treemacs-workspace-map))

(straight-use-package 'treemacs-evil)
(require 'treemacs-evil)

(customize-package
 '(ranger-deer-show-details nil)
 '(ranger-override-dired-mode t)
 '(ranger-show-hidden t))

(straight-use-package 'ranger)

(my-leader "r" 'ranger)
;; (add-hook 'ranger-mode-hook 'hide-mode-line)

(custom-set-variables '(hair 1))
(defcustom hair 2 "")
(setq hair 3)
(custom-reevaluate-setting 'hair) ; 1

(setq hare 1)
(defcustom hare 2 "")
(setq hare 3)
(custom-reevaluate-setting 'hare) ; 2
