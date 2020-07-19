;; Loading Emacs packages
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

;; Further reading on customization, defer, demand:
;; https://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
;; https://menno.io/posts/use-package/
;; https://jwiegley.github.io/use-package/keywords/#custom
;; https://emacs.stackexchange.com/questions/10396/difference-between-init-and-config-in-use-package#:~:text=They%20are%20different%20if%20the,the%20package%20is%20actually%20loaded.&text=You%20have%20configured%20the%20package,an%20html%20file%20is%20visited.
;; https://opensource.com/article/20/3/variables-emacs (see csetq discussion)

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

;;; package.el config and startup time profiling

;; Load function to configure package.el and functions used by hydras
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "my-functions.el")
;; When called with one of two arguments, benchmark-init/show-durations-* can be
;; called to show startup times for each file loaded
(my/init-maybe-profile)

;;; Bootstrap `use-package`

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
       ;; NOTE: s- indicates super; S- indicates shift
       (setq mac-control-modifier 'super)))

;;; Color scheme (https://emacsthemes.com/)

(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t))
;; (use-package dracula-theme)

;;; Non-customization settings

(setq default-directory "~/.emacs.d")

(setq auto-fill-mode t)

;;;; Executable paths
(add-to-list 'exec-path "/usr/local/bin")
(setq python-shell-interpreter "python3")

;;;; Whitespace (https://dougie.io/emacs/indentation)

;; Show tabs and trailing space
(global-whitespace-mode)
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces '(whitespace-tab ((t (:foreground "#636363")))))

(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9])))

;; Delete tabs as a unit, not one space at a time
(setq backward-delete-char-untabify-method nil)

;; Trim whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Custom configuration

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;; Keymaps

(use-package general
  :config
  (general-create-definer my-leader
    :states '(motion insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (my-leader
    "" nil ; Unbinding the prefix itself prevents errors about binding to non-prefix keys somehow
    ;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
    ;; Cross-platform shell that implements common programs (e.g., ls) in elisp
    "e" 'eshell
    "f" '(:prefix-command my/files-map :wk "files")
    ;; "fi" 'insert-file
    "t" 'ansi-term
    ;; https://www.masteringemacs.org/article/executing-shell-commands-emacs
    "&" 'async-shell-command)
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
  :general (:keymaps 'help-map
                     "C-h" nil ; Enable which-key navigation of help-map bindings
                     "C-w" 'which-key-show-keymap)
  :config
  (which-key-mode)
  (load "which-key-hacks") ; Modifications to display hydras
  ;; TODO: Fix indentation of keywords like (:keymaps ...):
  ;; https://github.com/noctuid/general.el#use-package-keywords
  :custom
  (which-key-compute-remaps t "E.g. w/ counsel-mode: apropos-command -> counsel-apropos")
  (which-key-idle-delay 0.2)
  (which-key-max-description-length 100)
  (which-key-popup-type 'side-window)
  (which-key-prefix-prefix "+")
  (which-key-separator " ")
  (which-key-show-docstrings t)
  (which-key-allow-evil-operators nil)
  (which-key-show-operator-state-maps nil "Enabling leads to rapid timeout for evil (e.g., 10dj or d10j)")
  (which-key-show-transient-maps t "See modified which-key--update")
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.1)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil))

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
  (defhydra hydra-window ()
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
    ("z" winner-undo)
    ;; ("z" (progn
    ;;     (winner-undo)
    ;;     (setq this-command 'winner-undo))
    ;;  "winner-undo") ;; Needed for winner-redo, it appears
    ("Z" winner-redo))
  (defun counsel-hydra-integrate (old-func &rest args)
    "Function used to advise `counsel-hydra-heads' to work with
 blue and amranath hydras."
    (hydra-keyboard-quit)
    (apply old-func args)
    (funcall-interactively hydra-curr-body-fn))
  (advice-add 'counsel-hydra-heads :around 'counsel-hydra-integrate)
  ;; Load hydras and integrate with which-key
  (winner-mode) ; Winner functions need to be present for hydras and/or hacks
  (load "my-hydras")
  (my/defhydra 'hydra-window) ; my/defhydra needs to run after loading hydras
  (my/defhydra 'hydra-buffer)
  :custom
  (hydra-hint-display-type 'lv)
  (hydra-is-helpful nil "Disabled in favor of which-key-show-transient-maps and which-key-hacks"))

(use-package ace-window
  :config
  (with-eval-after-load "hydra"
    (defhydra+ hydra-window ()
      ("a" ace-window)
      ("s" ace-swap-window)
      ("d" ace-delete-window))))

;;; File browser

(use-package ranger
  :defer t
  :general (my-leader "r" 'deer)
  ;; Use deer as directory handler
  :config (ranger-override-dired-mode t))

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

 ;;; Language Server Protocol (LSP)

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

(use-package page-break-lines)
;; (use-package osx-browse)
;; Potential ideas for fixing indentation? Didn't work when tried:
;; https://stackoverflow.com/questions/4643206/how-to-configure-indentation-in-emacs-lua-mode
;; https://github.com/kengonakajima/lua-mode/blob/master/my-lua.el
;; Turning off lua-electric-flag via setq-local in a hook
                                        ; (use-package lua-mode)
                                        ; (use-package jupyter)

 ;;; Vim emulation

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
  :general
  (my-leader
    "SPC" 'counsel-M-x
    "'" 'ivy-resume)
  (:keymaps 'my/files-map
                   "d" 'counsel-find-file
                   "f" 'counsel-fzf
                   ;; Is jump just a variation of fzf?
                   ;; "j" 'counsel-file-jump
                   ;; Locate doesn't work out of the box on MacOS
                   ;; "l" 'counsel-locate
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
              [remap ivy-alt-done] 'ivy-done
              )
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-help-file "~/.emacs.d/ivy-help.org"))
(use-package smex)
(use-package flx)

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
            (my-leader :keymaps 'local
              ;; "<backtab>" 'counsel-el ; counsel-assisted completion
              "m" 'my/elisp-map)))


 ;;; Org-mode

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(my-leader "o" 'my/org-index)

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

;; Vimwiki link bindings for org-mode
(evil-define-key 'normal org-mode-map
  (kbd "DEL") 'org-mark-ring-goto
  (kbd "RET") 'org-open-at-point)
;; S-tab: global cycling
;; tab: cycling at point

(use-package evil-org
  :after org
  :init (add-hook 'org-mode-hook 'evil-org-mode))
;; (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))

;; ;; Other org-mode bindings
;; (general-define-key
;;  :prefix-command 'my/org-map
;;  "c" 'my/org-checkbox-map)
;; (general-define-key
;;  :prefix-command 'my/org-checkbox-map
;;  ;; With prefix, toggle presence of checkbox
;;  "t" 'org-toggle-checkbox
;;  ;; When point is at list item, insert new item with checkbox
;;  "RET" 'org-insert-todo-heading
;;  "#" 'org-update-statistics-cookies)

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (my-leader :keymaps 'local
;;               "m" 'my/org-map)))


 ;;; Heading navigation
;; outline-up-heading (c-c c-u)
;; org-next-visible heading (c-c c-n/p)
;; org-forward-heading-same-level (c-c c-f/b)

 ;;; List navigation
;; org-shiftup/down (S-arrow): jump to next list item on same level

 ;;; Stucture (list/heading) editing
 ;; org-meta-return (m-ret): insert heading or item at current level
 ;;     org-insert-heading
 ;; org-insert-heading-respect-content (c-ret): Insert heading at end of subtree
 ;;     org-insert-heading-after-current
 ;; org-insert-subheading: Insert subheading
 ;; org-insert-todo-heading (m-s-ret): insert todo heading or checkbox item
 ;; org-insert-todo-heading-respect-content (c-s-ret): Insert todo heading at end of subtree
 ;; org-insert-todo-subheading

 ;; org-ctrl-c-minus: Convert to list item or cycle list level through different list symbols. If a list is selected as region, remove list symbols.
 ;; (S-arrow): cycle list level through list symbols; cycle header through keywords
 ;; org-ctrl-c-star: Convert to headline or from headline to text
 ;;     org-toggle-heading
 ;; org-list-make-subtree (c-c c-*): Convert entire list to subtree (one-way conversion only)

 ;; org-metaleft/right (M-arrow): Promote/indent / demote/dedent heading/list item
 ;;     org-do-promote
 ;;     NOTE: selection promotes/demotes everything with selection
 ;; org-shiftmetaleft/right (S-M-arrow): Promote/indent / demote/dedent subtree/sublist
 ;; org-shiftmetaup/down (S-M-arrow): move heading or list item up/down
 ;;     The refernce manual is partially incorrect for the above key's desc
 ;; org-metaup/down (M-arrow): move subtree/sublist up/down

 ;; org-ctrl-c-ctrl-c (c-c c-c): Toggle item checkbox, etc. With
 ;;     prefix, create checkbox. Note that there seems to be a bug,
 ;;     where with point on the hyphen of the first list item, c-u c-c
 ;;     c-c will insert checkboxes for other items at the top level,
 ;;     but the behavior doesn't work for other levels.

 ;; C-c ^: Sort list by the method selected from prompt

 ;; org-mark-subtree (c-c @): Repeat to mark subtrees of same level
 ;;     I was too lazy to bother with copy and paste commands

 ;;; Links
 ;; org-insert-link (c-c c-l): insert link, or edit the invisible link portion of a link with a description
 ;;     NOTE: Backspace at beginning of displayed description or end, will remove start and end brackets,
 ;;     which reveals the rest of the link's internals. Selected text becomes the description.
 ;; org-open-at-point (c-c c-o): open link
 ;; org-mark-ring-goto (c-c c-&)
 ;; org-store-link: Auto-generate link. When called in Org file, the generated link points to a radio target at point, or else the current headline
 ;;     For other files, the current line is used
 ;; org-next-link (c-c c-x c-n)
 ;; global insert and open allow org links in any emacs buffer. May be useful for inserting links to org docs within programming
 ;;        language comments. These can be bound to global keybindings

 ;;; TODO
 ;; org-todo (c-c c-t): Select a TODO tag. If switching from TODO to DONE for a repeating task, update the timestamp by the amount of the repeater, and reset the keyword to TODO. In contrast, C-- 1 C-c C-t permanently finishes the repeating task. Repeating tasks are indicated as e.g. +5d, while alerts/reminders as e.g. -4m. If you miss several due dates, you may want to update the timestamp only once for all of these missed deadlines to a future date. This requires ++ instead of +. The .+ repeater likewise updates to a future date, but the new timestamp is relative to the completion time rather than the timestamp. Both deadlines and schedules can have repeaters.
 ;; org-show-todo-tree
 ;; org-todo-list: global todo, collected from all agenda files
 ;; org-toggle-ordered-property: headings with this property can not be marked done until siblings on earlier lines are done
 ;; I need to find a command for inserting repeating timestamps. Right now I have to manually add repeaters.

 ;;; Indentation
 ;; org-fill-paragraph: respects lists!

 ;;; date and time
 ;; org-time-stamp (c-c .): create or update existing timestamp
 ;; org-deadline (c-c c-d): insert deadline keyword with timestamp
 ;; org-schedule (c-c c-s): insert schedule
 ;; org-check-deadlines (c-c / d): show past-due or do within org-deadline-warning-days
 ;;      Reminders can be appended; e.g., <2004-02-29 -5d> uses a 5-day advance notice
 ;;      Positives (+5m) indicate repeaters (repeating tasks). These must come before reminders.
 ;; org-check-before-date (c-c / b): checks deadliens and scheduled items before date
 ;; org-check-after-date (c-c / a)

 ;;; capture
 ;; org-capture: capture to org-default-notes-file

 ;;; attachment
 ;; org-attach (c-c c-a)

 ;;; agenda
 ;; org-agenda
 ;;     NOTE: contextual "a" will show agenda for org-agenda-span

 ;;; properties
 ;; org-set-property-and-value: sets property block
 ;; org-delete-property
 ;; org-toggle-ordered-property: Note that this is not inherited, only affects direct children
 ;; C-u c-u c-u c-t: change todo state, regardless of state blocking (like ordered property)

 ;;; tags
 ;; org-toggle-archive-tag: Won't tab-expand subtrees marked as archived but keeps it in file

 ;; C-c a org-agenda
 ;; C-c a a curr week agenda
 ;; C-c a t global TODO
 ;; org-agenda files (c-c [ or ])
