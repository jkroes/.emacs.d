;;; TODO

;; [x] Color theme
;; [x] Font
;; [x] Move to newly split windows, splitbelow, splitright
;; [] Change what evil considers to be a word (iskeyword)
;; [] Change which horiz movements can cross lines (whichwrap)
;; [x] Make tabs visible
;; [x] Break lines at 80 chars (auto-fill-mode)
;; [x] Disable temp files (auto save and backup files)
;; [] Truecolor for terminal? Not currently using terminal
;; [] Terminal configuration (neoterm, nvr)
;; [x] Remove trailing whitespace on write
;; [x] nerdcommenter
;; [] Enable manual indentation? See https://www.pement.org/emacs_tabs.htm#nothing for an
;;    explanation and note that a better option may be to improve a mode's autoindentation.
;; [] Fix tab. Currently it doesn't tab at all. Indentation seems to be forced in lisp mode.
;; [] undotree
;; [] vim-clap
;; [] floating terminal
;; [] vimwiki
;; [] grep commands (bind to spc f g)
;; [] R (polymode and ess; note polymode has its own website similar to lsp-mode)
;; [] python (emacs-jupyter?)
;; [x] Replicate coc.nvim
;;     [x] Floating help next to completion items: company-box!
;;     [x] Callable help: lsp-ui-doc-glance and company-show-doc-buffer
;;     [x] Scrollable help: custom lisp function
;; [x] Fuzzy file finder that searches all subdirs? See counsel-fzf
;; [] Make graphical emacs not started from Terminal use $PATH in exec-path
;;    See https://github.com/purcell/exec-path-from-shell
;; [] Ensure page breaks are enabled such that ^H isn't visible in help(?) docs
;; [] mode-line-format customization?
;; [x] Highlight parens (see show-paren-mode and evil-highlight-closing-paren-at-point states)
;; [x] Customize background,foreground face for show-paren-mode
;;     [] Does evil-highlight-closing-paren-at-point affect this?
;; [] Improve company selected completion item face for this theme (it's barely visible)
;;    NOTE: Appears to only affect the WSL version of emacs...
;; [] Update plugins and __init.el
;; [] Review ivy/counsel/swiper changelog, wiki, and external website to see what's changed
;;    and what I missed the first time I started using emacs. E.g., I bound both fzf and
;;    jump to my files hydra, even though they are presumably similar commands.
;; [] counsel-el is deprecated. (Try running the command for suggested fix).
;;    Change the binding at the bottom of init.el

;;; Installation

;; Installing emacs from source on Linux:
;; See 7.1 How do I install emacs?
;; Drop the -g flag from tar when installing a .xz from
;; alpha.gnu.org/gnu/emacs/pretest rather than .gz
;; Installing Hack font on Linux:
;; github.com/source-foundry/Hack#quick-installation

;;; package.el config and startup time profiling

;; Load function to configure package.el and functions used by hydras
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "my-functions.el")
;; When called with one of two arguments, benchmark-init/show-durations-* can be
;; called to show startup times for each file loaded
(my/init-maybe-profile)

;;; Bootstrap `use-package`

;; NOTE: Bug prevents use of :custom:
;; https://github.com/jwiegley/use-package/issues/702 It's not clear if this was
;; fixed by
;; https://github.com/jwiegley/use-package/commit/b9f1fe64ee11cf99d84857d51ac74da36a0b744c
;; See also https://github.com/jwiegley/use-package/issues/517 Read through the
;; thread and links of 702. The fix may not be a fix after all.  What does seem
;; to work absolutely is to use custom-set-variables instead of :custom, set
;; custom-file to a garbage file, then never load custom-file. That way the
;; custom-file won't interfere with your init file custom-set-variable blocks.
;; This suggests that it may have been fixed:
;; https://github.com/jwiegley/use-package/pull/850
;; https://github.com/jwiegley/use-package/commits/master (notice b9f1fe6 above
;; here) I may have to download the latest version from GitHub, since the
;; commits are relatively new and likely not on elpa or melpa yet.

;; package-initialize creates autoloads from packages and updates load-path to
;; enable load/require.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  ;; Needs to come after loading my-functions.el
  (package-install 'benchmark-init))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; OS-specific configuration

;; NOTE: Ligatures are not enabled for Firacode. See
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;; Using Hack font instead
(cond ((eq system-type 'gnu/linux)
       ;; Hack to open URLs from within WSL using browse-url-* commands
       (set-frame-font "Hack 10" nil t)
       (when (string-match "Linux.*Microsoft.*Linux"
                           (shell-command-to-string "uname -a"))
         (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
               browse-url-generic-args '("/c" "start" "")
               browse-url-browser-function 'browse-url-generic)))
      ((eq system-type 'windows-nt)
       ;; (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
       ;;        shell-file-name explicit-shell-file-name))
       ;; To configure locate:
       ;; Open Git's bash.exe; mkdir /usr/var;
       ;; updatedb --localpaths='/c/ /d/ /h/'
       (add-to-list 'exec-path
                    "c:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/")
       ;; Use git-bash's find.exe for file jumping
       (setq find-program
             "C:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/find.exe"))
      ((eq system-type 'darwin)
       (set-frame-font "Hack 12" nil t)))

;;; Color scheme (https://emacsthemes.com/)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package solarized-theme)
;; (use-package dracula-theme)

;;; Non-customization settings

(setq-default default-directory "~/.emacs.d")
(setq-default fill-column 80)
;; Default to auto-filling in all major modes
(setq-default auto-fill-function 'do-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; org-indent-mode indents nested lines and shows column 0 at the indented
;; position, not at the start of the line. So technically there should be a fill
;; line for each indented section. The text is still wrapping at fill-column
;; amount of characters; however, it doesn't appear that way because fill-column
;; doesn't adjusted the displayed indicator for indented org sections.
(add-hook 'org-mode-hook (lambda () (display-fill-column-indicator-mode -1)))


(add-to-list 'exec-path "/usr/local/bin")
(setq python-shell-interpreter "python3")

;;; Custom configuration

(setq custom-file "~/.emacs.d/emacs-custom.el")
(setq-default org-directory "~/.emacs.d/org") ; my/org-index fails if org-directory doesn't exist (before org loads)
(load custom-file)

;;; Debugging

(use-package command-log-mode
  :diminish command-log-mode
  ;; Auto-scroll buffer as commands are logged
  :hook (command-log-mode . (lambda ()
                              (set (make-local-variable 'window-point-insertion-type) t)))
  :config (global-command-log-mode))

;;; Keymaps

(use-package general
  :config
  (general-create-definer my-leader
    :states '(motion insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (my-leader
    "" nil ; Unbinding the prefix itself prevents errors about binding to non-prefix keys somehow
    ;; https://www.masteringemacs.org/article/executing-shell-commands-emacs
    "." 'clm/toggle-command-log-buffer
    "&" 'async-shell-command
    ;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
    ;; Cross-platform shell that implements common programs (e.g., ls) in elisp
    "e" 'eshell
    "f" '(:prefix-command my/files-map :wk "files")
    ;; "fi" 'insert-file
    "t" 'ansi-term)
  (general-def my/files-map
    :wk-full-keys nil ; Allows for consistent wk replacement text during cyclical map navigation
    "b" '(:prefix-command my/bookmarks-map :wk "bookmarks")
    "i" 'insert-file)
  (general-def my/bookmarks-map
    :wk-full-keys nil
    "d" 'bookmark-delete
    "e" 'edit-bookmark:w
    ;; For each map referencing `my/files-map' we need `:wk' "files"
    "f" '(my/files-map :wk "files")
    "r" 'bookmark-rename
    "s" 'bookmark-set)
  (general-unbind help-map "C-d" "s" "B" "C" "L" "g" "h" "n" "M-c" "RET" "C-n" "C-p" "C-t" "C-\\")
  (general-def help-map
    "M" 'describe-minor-mode
    "s" 'describe-symbol))

(use-package which-key
  :diminish which-key-mode
  :demand t ; Ensure popup is visible for all buffers on startup
  ;; TODO: Fix indentation of keywords like (:keymaps ...):
  ;; https://github.com/noctuid/general.el#use-package-keywords
  :general (:keymaps 'help-map
                     "C-h" nil ; Enable which-key navigation of help-map bindings
                     "C-w" 'which-key-show-keymap)
  :config
  (which-key-mode)
  ;; Modifications to diplay hydras
  (load "which-key-hacks"))

;; NOTE: To enable which-key paging, hydras must be pink. Without allowing foreign keys without exit,
;; you can still page to find what you want, but this will exit the hydra. You will have to reenter
;; all prefix keys again.
(use-package hydra
  ;; If which-key is not loaded at the time hydra is loaded and its :config run, hydra will fail to load
  ;; because of missing variables/functions from which-key. which-key can be forced to load by using
  ;; :demand t or by switching to an R file, which loads lsp-mode, which in turn loads which-key (probably
  ;; via reference to a which-key autoloaded function). If hydra's loading has been deferred, then
  ;; SPC m (hydra-r/body) will not work. I believe that use-package/general
  ;; create autoloads for bindings via the :bind and :general keywords. If you instead run SPC b ('hydra-buffer/body),
  ;; which should be such an autoload, then hydra will load, and SPC m will work. Alternativley, you can use
  ;; :commands hydra-r/body to create an autoload that triggers when SPC m is pressed. However, at this point, hydra
  ;; is loading after ess-r-mode, which needs my/defhydra to be present. If it's not, the hydra activates but the
  ;; hydra heads won't have the desired names in which-key.
  ;; :demand t
  :after which-key
  :commands hydra-r/body
  :general
  (my-leader
    "b" 'hydra-buffer/body
    "w" 'hydra-window/body)
  ;; Add opinionated counsel-hydra-heads to all hydras
  (:keymaps 'hydra-base-map "." 'counsel-hydra-heads)
  :config
  (defun counsel-hydra-integrate (old-func &rest args)
    "Function used to advise `counsel-hydra-heads' to work with
 blue and amranath hydras."
    (hydra-keyboard-quit)
    (apply old-func args)
    (funcall-interactively hydra-curr-body-fn))
  (advice-add 'counsel-hydra-heads :around 'counsel-hydra-integrate)
  (defhydra hydra-window (:color pink)
    "Window"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("b" hydra-buffer/body :color blue)
    ("v" my/split-window-right-move)
    ("x" my/split-window-below-move)
    ("m" delete-other-windows :color blue)
    ("M" my/delete-other-windows-and-buffers :color blue)
    ("q" nil))
  (defhydra hydra-buffer (:color pink)
    "Buffer"
    ("k" kill-buffer) ;; nil arg means kill current buffer (ivy auto-selects current buffer)
    ("K" my/kill-other-buffers :color blue)
    ("r" read-only-mode)
    ("s" my/switch-to-scratch)
    ("v" view-buffer)
    ("w" hydra-window/body :color blue)
    ("q" nil))
  ;; Load hydras and integrate with which-key
  (load "my-hydras")
  ;; Needs to run after all defhydra/defhyra+ have been evaluated
  ;; This seems to run after all packages have been loaded, though
  ;; this won't necessarily be the case for deferred packages. In that
  ;; case you'll need to ensure packages whose configs include defhydra or
  ;; defhydra+ aren't deferring loading.
  (add-hook 'after-init-hook
            (lambda ()
              (my/defhydra 'hydra-window)
              (my/defhydra 'hydra-buffer))))

;; Windows management

(winner-mode)
(defhydra+ hydra-window()
  ("z" winner-undo)
  ;; ("z" (progn
  ;;     (winner-undo)
  ;;     (setq this-command 'winner-undo))
  ;;  "winner-undo") ; Needed for winner-redo, it appears
  ("Z" winner-redo))

(use-package ace-window
  :config
  (with-eval-after-load "hydra"
    (defhydra+ hydra-window ()
      ("a" ace-window)
      ("s" ace-swap-window)
      ("d" ace-delete-window))))

;;; File browser

;; (use-package ranger
;;   :defer t
;;   :general (my-leader "r" 'deer)
;;   :config (ranger-override-dired-mode t))

 ;;; Comments

;; See README for examples, evil usage, and tips
;; There is a comment object and comment operators
(use-package evil-nerd-commenter
  :after evil
  :general (my-leader
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
  :config
  (defun counsel-imenu-comments ()
    "Use counsel to display comments in current buffer"
    (interactive)
    (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
      (unless (featurep 'counsel) (require 'counsel))
      (counsel-imenu))))

 ;;; Completion / LSP Extension

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-error-list)
;; (use-package dap-mode)
;; (require 'dap-python)
;; Testing out for parameter completion in lsp...
;; (use-package yasnippet
;;   :hook ((python-mode . yas-minor-mode)
;;          (ess-r-mode . yas-minor-mode)))

(use-package company
  :general
  (:keymaps 'company-mode-map
            "<tab>" 'company-indent-or-complete-common)
  (:keymaps 'company-active-map
            "M-n"  nil
            "M-p"  nil
            "C-n"  'company-select-next
            "C-p"  'company-select-previous))

;; Provides custom icons and popup documentation to the right of
;; completion items, similar to coc.nvim, when used with lsp-mode.
(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (ess-r-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024)
        lsp-prefer-capf t
        lsp-idle-delay 0.500))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (defun scroll-down-lsp-ui ()
    "Enable scrolling documentation child frames when using lsp-ui-doc-glance"
    (interactive)
    (if (lsp-ui-doc--frame-visible-p)
        (let ((kmap (make-sparse-keymap)))
          (define-key kmap (kbd "q")
            '(lambda ()
               (interactive)
               (lsp-ui-doc-unfocus-frame)
               (setq overriding-terminal-local-map nil)
               (setq which-key-show-transient-maps t)))
          (setq which-key-show-transient-maps nil)
          (setq overriding-terminal-local-map kmap)
          (lsp-ui-doc-focus-frame)))
    (evil-scroll-page-down 1))
  (general-define-key
   :states '(motion insert emacs)
   "C-f" 'scroll-down-lsp-ui)
  ;; Disable underlines in lsp-ui-doc child frames
  (custom-set-faces '(nobreak-space ((t nil)))))

;;; Vim emulation

;; (use-package evil-surround :after evil)

(use-package evil-tutor :after evil
  :general (:keymaps 'help-map "T" 'evil-tutor-start))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config (evil-escape-mode))

(use-package evil
  :config
  ;; (defalias 'evil-insert-state 'evil-emacs-state)    ; Alternative to disabling insert-state bindings
  (setq evil-normal-state-modes
        '(lisp-interaction-mode                         ; *scratch*
          emacs-lisp-mode
          python-mode
          ess-r-mode
          markdown-mode
          fundamental-mode
          lua-mode
          org-mode)
        evil-insert-state-modes
        '(inferior-ess-r-mode))
  (defhydra+ hydra-window ()
    ("-" evil-window-decrease-height)
    ("+" evil-window-increase-height)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ("c" evil-window-delete)
    ("r" evil-window-rotate-downwards)
    ("R" evil-window-rotate-upwards))
  (defhydra+ hydra-buffer ()
    ("l" evil-switch-to-windows-last-buffer))
  (evil-mode))

;;; Fuzzy finder

;; Was having issues with history, sorting, filtering in ivy using smex (M-x)
;; and/or flx (ivy in general), so I tried out prescient instead. The latter
;; has a definite history file it can read and write to.
;; (use-package smex)
;; (use-package flx)
;; TODO: Look into selectrum to replace ivy/counsel
(use-package prescient)
(use-package ivy-prescient)
(use-package ivy :diminish ivy-mode)
;; Usage within minibuffer: C-h m
;; Accept current candidate: C-j
;; Accept current input: C-M-j
(use-package counsel ;; Installs and loads ivy and swiper as dependencies
  :diminish counsel-mode
  :general
  (my-leader
    "SPC" 'counsel-M-x
    "'" 'ivy-resume)
  (:keymaps 'my/files-map
            ;; TODO: Add an action to change dir similar to C-u
            "f" 'counsel-fzf ; C-u prompts for directory selection
            ;; https://beyondgrep.com/feature-comparison/
            "g" 'counsel-rg ; C-x C-d to change directory
            "m" 'counsel-recentf)
  (:keymaps 'my/bookmarks-map
            "D" 'counsel-bookmarked-directory
            ;; TODO: Customize counsel-bookmark action list to include delete, rename, and set
            "j" 'counsel-bookmark)
  (:keymaps 'ivy-minibuffer-map
            "M-m"  'ivy-mark
            "M-u"  'ivy-unmark
            ;; For counsel-find-file, RET should add dir to search path instead of pulling up dired
            [remap ivy-done] 'ivy-alt-done
            [remap ivy-alt-done] 'ivy-done)
;; counsel-grep
;; counsel-org-file
  :config
  (defhydra+ hydra-buffer ()
    ("b" ivy-switch-buffer :color blue) ; Faster than counsel-switch-buffer b/c lack of preview
    ("B" counsel-buffer-or-recentf :color blue))
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-help-file "~/.emacs.d/ivy-help.org"))

;;; Org-mode

(defun my/org-index ()
  "Open org index file."
  (interactive)
  (find-file (concat (file-name-as-directory org-directory) "index.org")))

;; TODO: Test the counsel-org functions
;; TODO: Move some of these into my/org-map, since they're not useful
;; outside org files
(my-leader "o" '(:prefix-command my/global-org-map :wk "org-global"))
(general-def my/global-org-map
  :wk-full-keys nil
  ;; Insert LaTeX-like symbols
  "e" 'counsel-org-entity ; https://orgmode.org/manual/Special-Symbols.html
  "i" 'my/org-index
  "l" 'org-insert-link-global
  "o" 'org-open-at-point-global)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;; TODO: Investigate later:
;; sparse trees (e.g., to hide finished tasks)
;; drawers
;; blocks
;; links
;; todo subsequences
;; habits
;; priorities
;; cookies [%]
;; tags
;; properties
;; column view
;; details for dates and times, including clocking
;; refile, archive, capture refile and templates
;; working with attachments
;; agenda onward
;; diary

;; TODO: Investigate projectile
(use-package projectile
  :general (my-leader "p" 'projectile-command-map))

;; TODO: Learn about org-projectile and add bindings to spc-p instead of global
;; (use-package org-projectile
;;   :general (:keymaps
;;          ;; Choose project for capture
;;          "C-c n p" 'org-projectile-project-todo-completing-read
;;          "C-c c" 'org-capture)
;;   :config
;;   (org-projectile-per-project)
;;   ;; NOTE: If TODO.org doesn't exist for a project, org-agenda will prompt
;;   ;; to remove them.
;;   (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))
;; Location of project-based TODOs
;; (setq org-projectile-projects-file
;;       (concat (file-name-as-directory org-directory) "projects.org"))
;; Adds a TODO capture template activated by letter p (see org-capture)
;; and replaces the default t(ask) template stored in ~/.notes
;; Otherwise, you can use org-projectile-capture-for-current-project
;; (push (org-projectile-project-todo-entry) org-capture-templates)

;;;; Existing bindings that I didn't change:
;; tab (org-cycle)
;; S-tab (global-org-cyc
;;;;; Stucture (list/heading) editing
;; org-meta-return (m-ret): insert heading or item at current level
;;     org-insert-heading
;; org-insert-heading-respect-content (c-ret): Insert heading at end of subtree
;;     org-insert-heading-after-current
;; org-insert-todo-heading (m-s-ret): insert todo heading or checkbox item
;; org-insert-todo-heading-respect-content (c-s-ret): Insert todo heading at end of subtree
;; org-insert-subheading: Insert subheading
;; org-insert-todo-subheading
(evil-define-key 'normal org-mode-map
  (kbd "DEL") 'org-mark-ring-goto)
(add-hook 'org-mode-hook
          (lambda ()
            (general-define-key
            :states 'motion
            :keymaps 'org-mode-map
            "RET" 'org-open-at-point ; Open link at point
            "g" '(:ignore t :wk "Entry navigation")
            "gh" 'outline-previous-visible-heading
            "gl" 'outline-next-visible-heading
            "gk" 'org-backward-heading-same-level
            "gj" 'org-forward-heading-same-level
            "U" 'outline-up-heading ; Navigate up a heading level
            "M-h" 'org-metaleft ; Promote/dedent heading/list item
            "M-l" 'org-metaright ; Demote/indent heading/list item
            "M-j" 'org-shiftmetadown ;; Move heading or list item down
            "M-k" 'org-shiftmetaup
            "M-H" 'org-shiftmetaleft ;; Like metaleft for subtrees/sublists
            "M-L" 'org-shiftmetaright
            "M-J" 'org-metadown ;; Move subtree/sublist up/down
            "M-K" 'org-metaup
            ;; Respects lists when filling
            "M-q" 'org-fill-paragraph)))

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
 "K" 'org-shiftup
 ;; Capture to org-default-notes-file
 "c" 'counsel-org-capture)

(my-leader :keymaps 'org-mode-map "m" 'my/org-map)
;;;; TODO:
;; Find command to add repeating timers rather than editing manually
;; Make RET convert plain text under cursor or selected to link. Currenlty it
;; only follows existing links, so one-half vimwiki functionality
;;;;; Bind the following:
;; org-set-property-and-value: sets property block
;; org-delete-property
;; C-u c-u c-u c-t: change todo state, regardless of state blocking (like
;; ordered property)
;; org-check-deadlines (c-c / d): show past-due or do within
;;      org-deadline-warning-days Reminders can be appended; e.g., <2004-02-29
;;      -5d> uses a 5-day advance notice Positives (+5m) indicate repeaters
;;      (repeating tasks). These must come before reminders.
;; org-check-before-date (c-c / b): checks deadliens and scheduled items before
;; date
;; org-check-after-date (c-c / a)
;; https://www.spacemacs.org/layers/+emacs/org/README.html

;;; REPLs/Programming

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

;; "<backtab>" 'counsel-el ; counsel-assisted completion
(my-leader :keymaps 'emacs-lisp-mode-map "m" 'my/elisp-map)

;; (use-package markdown-mode)
;; (use-package poly-markdown
;;   :mode (("\\md" . poly-markdown-mode)))
;; (use-package poly-R
;;   :mode (("\\.Rmd" . poly-markdown+R-mode)))
;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;; https://github.com/polymode/polymode/issues/92
(use-package poly-markdown)
;; NOTE: ess-r configuration and bindings are available inside chunks, where R-mode is active
;; I have bound polymode-export (render) to SPC-m-e-k
(use-package poly-R)

(use-package ess
  :hook (ess-r-mode . config-ess-r-mode)
  :config
  ;; Prevent window displaying company documentation buffer from vanishing when
  ;; invoking a binding not in company--electric-commands
  ;; (defun forget-saved-window-config ()
  ;;   (setq company--electric-saved-window-configuration nil))
  ;; (advice-add 'company-pre-command :before 'forget-saved-window-config)

  (defun config-ess-r-mode ()
    (ess-set-style 'RStudio)
    (setq-local ess-indent-offset 4) ; RStudio style uses a value of 2

    (defun show-company-doc-as-ess-help ()
      "Show ess help if available, else show company help"
      (interactive)
      (let* ((selected (nth company-selection company-candidates))
             (obj-help (ess-display-help-on-object selected)))
        (unless obj-help
          (company-show-doc-buffer))))

    (defun mode-specific-C-h ()
      "Mode-specific C-h for company-active-map"
      (interactive)
      (pcase major-mode
        ('ess-r-mode (show-company-doc-as-ess-help))
        (_ (company-show-doc-buffer))))

    (define-key company-active-map (kbd "C-h") 'mode-specific-C-h)

    ;; Rely on electric-pair-mode instead of skeleton
    (local-set-key (kbd "{") 'self-insert-command)
    (local-set-key (kbd "}") 'self-insert-command)

    ;; electric-layout-rules interferes with ess-roxy-newline-and-indent
    ;; if electric-layout-mode is enabled (it is not by default)
    (setq-local electric-layout-rules nil)
    )
  (add-hook 'ess-r-mode-hook
            (lambda ()
              (my/defhydra 'hydra-r)
              (my/defhydra 'hydra-r-help)
              (my/defhydra 'hydra-r-eval)
              (my/defhydra 'hydra-r-debug)))

  ;; Major-mode binding is more efficient than buffer-local binding in a hook. E.g.
  ;; (my-leader :keymaps 'local "m" 'hydra-r/body)
  ;; in def of `config-ess-r-mode'
  (my-leader :keymaps 'ess-r-mode-map "m" 'hydra-r/body)

  ;; Override Windows' help_type option of "html", to open help
  ;; in help buffer, not browser (see contents of .Rprofile)
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
  (setq ess-nuke-trailing-whitespace-p t
        ;; ess-S-quit-kill-buffers-p 'ask
        inhibit-field-text-motion nil)) ; prompt acts as beginning of line if prompt is read-only

(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(global-set-key  (kbd "\C-x c") 'clear-shell)


;;; Random packages

(use-package page-break-lines)
;; (use-package osx-browse)
;; Potential ideas for fixing indentation? Didn't work when tried:
;; https://stackoverflow.com/questions/4643206/how-to-configure-indentation-in-emacs-lua-mode
;; https://github.com/kengonakajima/lua-mode/blob/master/my-lua.el
;; Turning off lua-electric-flag via setq-local in a hook
                                        ; (use-package lua-mode)
                                        ; (use-package jupyter)
