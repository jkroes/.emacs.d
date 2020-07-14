;;; Loading Emacs packages
;; See '48.2 Package Installation' (emacs), '41.1 Packaging Basics' (elisp), comments in
;;  package.el, and docstring of package-initialize and
;;  https://lists.gnu.org/archive/html/emacs-devel/2017-08/msg00154.html
;; The terminology is inconsistent. What 48.2 in the emacs manual calls loading, package.el
;;  calls activation: modifying `load-path' and loading autoloads. package.el refers to loading
;;  as full evaluation of a package (what would happen if you required a package). In this sense,
;;  (package-initialize t) and (setq package-enable-at-startup nil) both seem to disable
;;  activation; however, package.el also casually refers to another type of loading that seems
;;  to be loading of package description files. Either way, package-initialize as defined in
;;  package.el seems to modify load-path and load autoloads, but does not fully load a package.
;; This can be seen by profiling with benchmark-init, with (package-initialize)
;;  before and after (benchmark-init/activate)
;; `with-eval-after-load' can be used to evaluate a body of code after a file/library
;;   loads (or execute immediately if it is already loaded). See
;;   '16.10 Hooks for Loading'. Prior to package loading, options/variables can be set
;;   (and should in some cases to have any effect), as can keybindings, and mode-specific
;;   customization can be set to run in a mode hook. Functions from the package that
;;   need to run right away can also be preceded by a call to require instead of
;;   deferring evaluation. Finally, `after-init-hook' can be used if package initialization
;;   has not been disabled. This will run code after the init file has been evaluated.
;;; Loading Lisp Libraries
;; See '40.1.1 Summary: Sequence of Actions at Startup' (elisp)
;; `load' is the back-end for all loading (opening and evaluating a file)
;;   It uses `load-path' for relative filename args to find a directory
;;    that contains the filename.
;;    See '16.3 Library Search' for construction of load-path
;;   See related commands `load-file', `load-library', etc.
;; `autoload' and magic comments can be used to created autoloaded functions
;;   Calls to autoloaded functions load the file where the actual function is
;;    define.
;;   `autoloadp' identifies autoloaded objects
;;  Re-loading a file does not re-initialize defvar, defcustom, defun
;;  `require' is an alternative to `load'. It searches for a feature in `features',
;;    similar to relative filename args in `load' (including reliance on `load-path'.
;;    One difference is that it will not re-load a file/feature.

;;; Installation

;; Installing emacs from source on Linux:
;; See 7.1 How do I install emacs?
;; Drop the -g flag from tar when installing a .xz from
;; alpha.gnu.org/gnu/emacs/pretest rather than .gz
;; Installing Hack font on Linux:
;; github.com/source-foundry/Hack#quick-installation

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

;;; package.el config and profiling
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "my-functions.el") ;; Also loads functions required by hydras
(my/init-maybe-profile)

;;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; Install missing packages, load customizations, then load packages
;; (let ((packages '(quelpa help-fns+ general)))
;;   (dolist (pkg packages)
;;     (unless (package-installed-p pkg)
;;       (cond ((string= pkg "help-fns+")
;;        (quelpa '(help-fns+ :fetcher wiki)))
;;       (t (package-refresh-contents)
;;          (package-install pkg)))))
;;     ;; Not sure if customizations need loading prior to requiring packages...
;;   (setq custom-file "~/.emacs.d/emacs-custom.el")
;;   (load custom-file)
;;   (dolist (pkg packages)
;;     (require pkg)))

;;; OS-specific configuration

(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Hack 12" nil)
       ;; Hack to open URLs from within WSL using browse-url-* commands
       (when (string-match "Linux.*Microsoft.*Linux"
                           (shell-command-to-string "uname -a"))
         (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
               browse-url-generic-args '("/c" "start" "")
               browse-url-browser-function 'browse-url-generic)))
      ((eq system-type 'windows-nt)
       ;; (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
       ;;        shell-file-name explicit-shell-file-name))
       ;; To configure locate:
       ;; Open Git's bash.exe; mkdir /usr/var; updatedb --localpaths='/c/ /d/ /h/'
       (add-to-list 'exec-path "c:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/")
       ;; Use git-bash's find.exe for file jumping
       (setq find-program "C:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/find.exe"))
      ;; For my programming keyboard. PC mode doesn't have CMD, and CTRL/CMD are at the
      ;; same thumb position in PC/MAC modes, respectively. Saves me redefining the
      ;; keymapings
      ;; Requires a GUI version of emacs
      ;; See https://www.emacswiki.org/emacs/EmacsForMacOS
      ((eq system-type 'darwin)
       (set-frame-font "Hack 14" nil)
       (setq mac-command-modifier 'ctrl)
       (setq mac-control-modifier 'super)))

;; NOTE: Ligatures are not enabled. See
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions

;;; Non-customization settings

(setq default-directory "~/")
;; (standard-display-ascii ?\t "^I")

(setq auto-fill-mode t)

;;;; Whitespace (https://dougie.io/emacs/indentation)

;; Show tabs and trailing space
(global-whitespace-mode)
(setq whitespace-style
      '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))

(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9])))

;; Delete tabs as a unit, not one space at a time
(setq backward-delete-char-untabify-method nil)

;; Trim whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Color scheme (https://emacsthemes.com/)

(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t))
;; (use-package dracula-theme)

;;; Custom configuration

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;; Comments

;; See README for examples, evil usage, and tips
;; There is a comment object and comment operators
(use-package evil-nerd-commenter
  :after evil
  :config
  ;; Extracted from source code.
  (define-key evil-normal-state-map ",." 'evilnc-copy-and-comment-operator)
  (define-key evil-visual-state-map ",." 'evilnc-copy-and-comment-operator)
  (define-key evil-normal-state-map ",," 'evilnc-comment-operator)
  (define-key evil-visual-state-map ",," 'evilnc-comment-operator)
  (general-define-key
   :prefix-command 'my/comments-map
   "c" 'evilnc-comment-or-uncomment-lines
   "C" 'evilnc-copy-and-comment-lines
   "i" 'counsel-imenu-comments
   ;; When given C-u <n>, will forward-match <n> against the rightmost
   ;; digits of each line. E.g., on line 160, C-u <72> will target lines
   ;; 160-172
   "l" 'evilnc-quick-comment-or-uncomment-to-the-line
   "p" 'evilnc-comment-or-uncomment-paragraphs
   "y" 'evilnc-comment-and-kill-ring-save
   ;; Whether empty lines can be commented as part of a selection
   "e" 'evilnc-toggle-comment-empty-lines
   ;; When toggled off, all lines in a selection are commented if any
   ;; uncommented lines are included. Note that blank lines never count
   "v" 'evilnc-toggle-invert-comment-line-by-line)
  (defun counsel-imenu-comments ()
    (interactive)
    (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
      (unless (featurep 'counsel) (require 'counsel))
      (counsel-imenu))))

;;; Completion / LSP Extension

(use-package company
  ;; :hook ((after-init . global-company-mode))
  :bind (:map company-mode-map
              ("<tab>" . company-indent-or-complete-common)
              :map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; Provides custom icons and popup documentation to the right of
;; completion items, similar to coc.nvim, when used with lsp-mode.
(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Language Server Protocol (LSP)

(add-to-list 'exec-path "/usr/local/bin")
(setq python-shell-interpreter "python3")

;; Testing out for parameter completion in lsp...
;; (use-package yasnippet
;;   :hook ((python-mode . yas-minor-mode)
;;          (ess-r-mode . yas-minor-mode)))

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

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs :commands lsp-treemacs-error-list)
;; (use-package dap-mode)
;; (require 'dap-python)

;;; Random packages

;; Bug prevents use of :custom: https://github.com/jwiegley/use-package/issues/702
;; (use-package evil-surround :after evil)
;; (use-package org)
(use-package page-break-lines)
;; (use-package osx-browse)
;; Potential ideas for fixing indentation? Didn't work when tried:
;; https://stackoverflow.com/questions/4643206/how-to-configure-indentation-in-emacs-lua-mode
;; https://github.com/kengonakajima/lua-mode/blob/master/my-lua.el
;; Turning off lua-electric-flag via setq-local in a hook
                                        ; (use-package lua-mode)
                                        ; (use-package jupyter)

;;; Vim emulation

(use-package evil-tutor :after evil :bind (:map help-map ("T" . evil-tutor-start)))
(use-package evil-escape :after evil :config (evil-escape-mode))
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
          lua-mode)
        evil-insert-state-modes
        '(inferior-ess-r-mode))
  (evil-mode))


;;; REPLs/Programming

;;;; R

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

    (my-definer :keymaps 'local "m" 'hydra-r/body))

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
     (setenv "RSTUDIO_PANDOC" "C:/Users/jkroes/AppData/Local/Pandoc")
     )
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

;;; Fuzzy finder

;; Usage within minibuffer: C-h m
(use-package counsel ;; Installs and loads ivy and swiper as dependencies
  :bind (:map ivy-minibuffer-map
              ("M-m" . ivy-mark)
              ("M-u" . ivy-unmark)
              ([remap ivy-done] . ivy-alt-done)
              ([remap ivy-alt-done] . ivy-done))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-help-file "~/.emacs.d/ivy-help.org"))
(use-package smex)
(use-package flx)

;;; Keymaps

;; NOTE: The two evil-related which-key configuration settings are disabled.
;; It seems that they set an extremely rapid timeout for evil motions, preventing
;; any evil motions short of instinctive ones. The popups for evil are not worth
;; this, though there may be a configuration setting to increase the length of
;; time before keys timeout.
(use-package which-key :bind (:map help-map ("C-w" . which-key-show-keymap)))
(use-package ace-window)

(use-package hydra
  :after (counsel which-key) ;; Untested whether this is necessary
  :config
  ;; Add opinionated counsel-hydra-heads to all hydras
  (defun counsel-hydra-integrate (old-func &rest args)
    "Function used to advise `counsel-hydra-heads' to work with
blue and amranath hydras."
    (hydra-keyboard-quit)
    (apply old-func args)
    (funcall-interactively hydra-curr-body-fn))
  (advice-add 'counsel-hydra-heads :around 'counsel-hydra-integrate)
  (define-key hydra-base-map (kbd ".") 'counsel-hydra-heads)

  ;; Load hydras and integrate with which-key
  (load "my-hydras")
  (load "which-key-hacks")
  (my/defhydra 'hydra-window) ; Needs to run after hydra-buffer is defined
  (my/defhydra 'hydra-buffer) ; Needs to run after hydra-window is defined
  (add-hook 'ess-r-mode-hook
            (lambda ()
              (my/defhydra 'hydra-r)
              (my/defhydra 'hydra-r-help)
              (my/defhydra 'hydra-r-eval)
              (my/defhydra 'hydra-r-debug))))

(use-package general)

(general-unbind help-map
  "C-h" ; Enable which-key navigation of help-map bindings
  "C-d" "s" "B" "C" "L" "g" "h" "n" "M-c" "RET" "C-n" "C-p" "C-t" "C-\\")

(general-define-key
 :keymaps 'help-map
 "M" 'describe-minor-mode
 "s" 'describe-symbol)

;; (use-package key-chord
;;   :config
;;   (key-chord-mode 1))

(general-create-definer my-definer
  :states '(motion insert emacs)
  :prefix "SPC"
  ;; :non-normal-prefix (general-chord "fd")
  :non-normal-prefix "C-SPC")

(defun org-index ()
  (interactive)
  (find-file "~/org/index.org"))

(my-definer
  "" nil
  "SPC" 'counsel-M-x
  "'" 'ivy-resume
  "b" 'hydra-buffer/body
  "c" 'my/comments-map
  "f" 'my/files-map
  ;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
  "e" 'eshell ; Cross-platform shell that implements common programs (e.g., ls) in elisp
  "o" 'org-index
  "t" 'ansi-term
  ;; https://www.masteringemacs.org/article/executing-shell-commands-emacs
  "&" 'async-shell-command
  "w" 'hydra-window/body)

(general-define-key
 :prefix-command 'my/files-map
 "b" 'my/bookmarks-map
 "d" 'counsel-find-file
 "f" 'counsel-fzf
 "i" 'insert-file
 ;; Is jump just a variation of fzf?
 ;; "j" 'counsel-file-jump
 ;; Locate doesn't work out of the box on MacOS
 ;; "l" 'counsel-locate
 "m" 'counsel-recentf)

(general-define-key
 :prefix-command 'my/bookmarks-map
 "d" 'bookmark-delete
 "D" 'counsel-bookmarked-directory
 "e" 'edit-bookmarks
 "j" 'counsel-bookmark ; TODO: Customize counsel-bookmark action
 ;; list to include delete, rename, and set
 "r" 'bookmark-rename
 "s" 'bookmark-set)

(general-define-key
 :prefix-command 'my/elisp-map
 "c" 'check-parens            ; Debugging "End of file during parsing"
 "d" 'eval-defun              ; evals outermost expression containing or following point
 ;; ...and forces reset to initial value within a defvar,
 ;; defcustom, and defface expressions
 "m" 'pp-eval-expression      ; "m" for minibuffer, where exp is evaluated
 "s" 'pp-eval-last-sexp       ; evals expression preceding point
 "i" 'eval-print-last-sexp    ; "i" for insert(ing result)
 "r" 'eval-region)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (my-definer :keymaps 'local
              ;; "<backtab>" 'counsel-el ; counsel-assisted completion
              "m" 'my/elisp-map)))

;; (general-define-key
;;  :prefix-command 'my/org-map
;;  )

;; (add-hook 'org-mode-map
;;           (lambda()
;;             (my-definer :keymaps 'local
;;               "m" 'my/org-map)))

;; C-c l org-store-link
;; C-c a org-agenda
;; org-todo-keywords
;; org-tags-alist
;; C-c a a curr week agenda
;; C-c a t global TODO
;; org-agenda files (c-c [ or ])
