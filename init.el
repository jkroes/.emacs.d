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
;;    defined.
;;   `autoloadp' identifies autoloaded objects
;;  Re-loading a file does not re-initialize defvar, defcustom, defun
;;  `require' is an alternative to `load'. It searches for a feature in `features',
;;    similar to relative filename args in `load' (including reliance on `load-path'.
;;    One difference is that it will not re-load a file/feature.

;;; package.el config and profiling
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "my-functions.el") ;; Also loads functions required by hydras
(my/init-maybe-profile)
 
;;; Bootstrap `use-package`

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20190716.1829")
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; Install missing packages, load customizations, then load packages
;; (let ((packages '(quelpa help-fns+ general)))
;;   (dolist (pkg packages)
;;     (unless (package-installed-p pkg)
;;       (cond ((string= pkg "help-fns+")
;; 	     (quelpa '(help-fns+ :fetcher wiki)))
;; 	    (t (package-refresh-contents)
;; 	       (package-install pkg)))))
;;     ;; Not sure if customizations need loading prior to requiring packages...
;;   (setq custom-file "~/.emacs.d/emacs-custom.el")
;;   (load custom-file)
;;   (dolist (pkg packages)
;;     (require pkg)))

;;; OS-specific configuration

(cond ((eq system-type 'gnu/linux)
       ;; Hack to open URLs from within WSL using browse-url-* commands
       (when (string-match "Linux.*Microsoft.*Linux"
			   (shell-command-to-string "uname -a"))
	 (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
	       browse-url-generic-args '("/c" "start" "")
	       browse-url-browser-function 'browse-url-generic)))
      ((eq system-type 'windows-nt)
       ;; (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
       ;; 	     shell-file-name explicit-shell-file-name))
       ;; To configure locate:
       ;; Open Git's bash.exe; mkdir /usr/var; updatedb --localpaths='/c/ /d/ /h/'
       (add-to-list 'exec-path "c:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/")
       ;; Use git-bash's find.exe for file jumping
       (setq find-program "C:/Users/jkroes/AppData/Local/Programs/Git/usr/bin/find.exe")))

;;; Custom configuration

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;; Package configuration

;; Bug prevents use of :custom: https://github.com/jwiegley/use-package/issues/702

(use-package smex)
(use-package flx)
(use-package evil-tutor :after evil :bind (:map help-map ("T" . evil-tutor-start)))
(use-package org)
(use-package dracula-theme)
(use-package evil-escape :after evil :config (evil-escape-mode))
(use-package page-break-lines)
(use-package ace-window)
(use-package which-key :bind (:map help-map ("C-w" . which-key-show-keymap)))
(use-package evil
  :config
  ;; (defalias 'evil-insert-state 'evil-emacs-state)    ;; Alternative to disabling insert-state bindings
  (setq evil-normal-state-modes
	'(lisp-interaction-mode                         ;; *scratch*
	  emacs-lisp-mode
	  ess-r-mode
	  fundamental-mode)
        evil-insert-state-modes
	'(inferior-ess-r-mode))
  (evil-mode))

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

(use-package company
  ;; :hook ((after-init . global-company-mode))
  :bind (:map company-mode-map
	      ("<tab>" . company-indent-or-complete-common)
	      :map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)))

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
  (my/defhydra 'hydra-window) ;;Needs to run after hydra-buffer is defined
  (my/defhydra 'hydra-buffer) ;;Needs to run after hydra-window is defined
  (add-hook 'ess-r-mode-hook
	    (lambda ()
	      (my/defhydra 'hydra-r)
	      (my/defhydra 'hydra-r-help)
	      (my/defhydra 'hydra-r-eval)
	      (my/defhydra 'hydra-r-debug))))

;;; General keybindings

(use-package general)

(general-unbind help-map
  "C-h" ;; Enable which-key navigation of help-map bindings
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

(my-definer
  "" nil
  "SPC" 'counsel-M-x
  "'" 'ivy-resume
  "b" 'hydra-buffer/body
  "f" 'my/files-map
  "w" 'hydra-window/body)

(general-define-key
 :prefix-command 'my/files-map
 "b" 'my/bookmarks-map
 "f" 'counsel-find-file
 "i" 'insert-file
 "j" 'counsel-file-jump
 "l" 'counsel-locate
 "r" 'counsel-recentf
 ;; "z" 'counsel-fzf
 )

(general-define-key
 :prefix-command 'my/bookmarks-map
 "d" 'bookmark-delete
 "D" 'counsel-bookmarked-directory
 "e" 'edit-bookmarks
 "j" 'counsel-bookmark ;; TODO: Customize counsel-bookmark action
 ;; list to include delete, rename, and set
 "r" 'bookmark-rename
 "s" 'bookmark-set)

;;; Lisp 

(general-define-key
 :prefix-command 'my/elisp-map
 "c" 'check-parens            ;; Debugging "End of file during parsing"
 "d" 'eval-defun              ;; evals outermost expression containing or following point
 ;; ...and forces reset to initial value within a defvar,
 ;; defcustom, and defface expressions
 "m" 'pp-eval-expression      ;; "m" for minibuffer, where exp is evaluated
 "s" 'pp-eval-last-sexp       ;; evals expression preceding point
 "i" 'eval-print-last-sexp    ;; "i" for insert(ing result)
 "r" 'eval-region)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (my-definer :keymaps 'local
	      "<backtab>" 'counsel-el ;; counsel-assisted completion
	      "m" 'my/elisp-map)))

;;; R

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
    (setq-local ess-indent-offset 4) ;; RStudio style uses a value of 2

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
    ('windows-nt (setenv "R_PROFILE" "C:\\Users\\jkroes\\.emacs.d"))
    ('darwin (setenv "R_PROFILE" "~/.emacs.d")))
  (setq ess-nuke-trailing-whitespace-p t
	;; ess-S-quit-kill-buffers-p 'ask 
	inhibit-field-text-motion nil)) ;; prompt acts as beginning of line if prompt is read-only

;;; Buffer window display management

(setq display-buffer-alist `(("\\*company-documentation\\*"
			      (display-buffer-reuse-mode-window display-buffer-in-side-window)
			      (mode. ess-r-help-mode)
			      (side . right)
			      (slot . 1)
			      (window-width . 0.33)
			      (reusable-frames . nil))
			     ("\\*R Dired"
			      (display-buffer-reuse-mode-window display-buffer-in-side-window)
			      (side . right)
			      (slot . -1)
			      (window-width . 0.33)
			      (reusable-frames . nil))
			     ("\\*R"
			      (display-buffer-reuse-mode-window display-buffer-below-selected)
			      (window-height . 0.5)
			      (reusable-frames . nil))
			     ("\\*Help\\[R"
			      (display-buffer-reuse-mode-window display-buffer-in-side-window)
			      (side . right)
			      (slot . 1)
			      (window-width . 0.33)
			      (reusable-frames . nil))
			     ("\\*Help\\*" display-buffer-same-window)
			     ("\\*Apropos\\*" display-buffer-same-window)))

;;; Further reading:

;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://emacs.sexy/#resources
;; https://www.reddit.com/r/emacs/comments/6s5470/useful_packages/
;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/MilesMcBain/esscss
;; https://www.masteringemacs.org/about

;;; Evil
;;https://github.com/noctuid/evil-guide
;;https://raw.githubusercontent.com/emacs-evil/evil/master/doc/evil.pdf
;;evil-tutor-start
;;https://www.emacswiki.org/emacs/Evil
;;https://emacs.stackexchange.com/questions/12175/instructions-on-how-to-work-with-evil-mode (see config)
;;https://github.com/emacs-evil/evil-collection
;;https://www.linode.com/docs/tools-reference/tools/emacs-evil-mode/
;;https://github.com/noctuid/evil-guide/issues/11
;;https://github.com/emacs-evil/evil/blob/3766a521a60e6fb0073220199425de478de759ad/evil-maps.el

;;; Counsel
;; https://oremacs.com/swiper/
;; https://github.com/abo-abo/swiper/wiki
;; https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
;; https://github.com/abo-abo/hydra/wiki/hydra-ivy-replacement
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#fn.1
;; See ivy info node
;; Relevant maps:
;; minibuffer maps
;; ivy-minibuffer-map
;; counsel command maps (e.g. counsel-find-file-map)
;;  NOTE: '`' shows ?? as the binding. See counsel.el,
;;  counsel-find-file-map, where '`' is bound to a call
;;  to ivy-make-magic-action with arg "b", equiv. to
;;  M-o b
;; Investigate actions for each counsel command
;; E.g. M-o within counsel-M-x contains a jump to def action and
;; a help action
;; Navigation:
;;  counsel-outline (navigates comments)
;; Completion:
;;  indent-for-symbol
;;  counsel-company  
;;  counsel-jedi
;; counsel-set-variable (defcustom completion)
;; ivy-push-view (https://oremacs.com/2016/06/27/ivy-push-view/)
;; ivy-pop-view

;;; ESS
;; ess-show-traceback, ess-show-call-stack, ess-parse-errors (for syntax errors)
;; ESS maps:
;; ess-help-mode-map
;; inferior-ess-mode-map
;; ess-r-help-mode-map
;; ess-watch-mode-mape
;; ess-rdired-mode-map
;; ess-electric-selection-map
;; inferior-ess-mode-map
;; ess-mode-map
;; inferior-ess-r-mode-map
;; electric-indent-mode
;; https://github.com/MilesMcBain/esscss
