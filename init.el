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

;;; Package config and profiling

(defun my/init-maybe-profile (&optional when?)
  "Optionally profile load before or after package initialization. See results of
profiling with `benchmark-init/show-durations-tree' (calls shown in reverse order)
or in a sortable table with `benchmark-init/show-durations-tabulated'.

when? can have values of before-init, after-init, or anything else for no profiling."
  (defun my/profile ()
    (load (concat (car (file-expand-wildcards "~/.emacs.d/elpa/benchmark-init*" t))
		  "/benchmark-init"))
    (add-hook 'after-init-hook 'benchmark-init/deactivate)
    (benchmark-init/activate))
  
  (defun my/init ()
    ;; Configure package.el
    (require 'package)
    (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
			     ("melpa" . "https://melpa.org/packages/")))
    ;; Add pkg files to load-path and Info-directory-list; ready autoloads
    (package-initialize))
  
  (pcase when?
    ('before-init (my/profile) (my/init))
    ('after-init (my/init) (my/profile))
    (_ (my/init))))  ;; Includes nil argument

(my/init-maybe-profile)
  
;;; Bootstrap `use-package`

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20190716.1829")
(require 'use-package)
(setq use-package-verbose t)

;;; Packages to install

(dolist (pkg '(quelpa help-fns+ smex flx hydra which-key page-break-lines evil-escape ace-window
		      general evil-tutor org command-log-mode hercules dracula-theme))
  (unless (package-installed-p pkg)
    (cond ((string= pkg "help-fns+")
	   (quelpa '(help-fns+ :fetcher wiki)))
	  (t (package-refresh-contents)
	     (package-install pkg)))))

;;; Customization

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;; General emacs keybindings

(define-key global-map (kbd "C-x C-c") 'kill-emacs)
;; c-x a ;; abbrev
;; c-x r ;; bookmarks
;; c-x v ;; version control

;;; help-map ("C-h") mods

(general-unbind help-map
  "C-h" ;; Enable which-key navigation of help-map bindings
  "C-d" "s" "B" "C" "L" "g" "h" "n" "M-c" "RET" "C-n" "C-p" "C-t" "C-w" "C-\\")

(general-define-key
 :keymaps 'help-map
 "M" 'describe-minor-mode
 "s" 'describe-symbol
 "T" 'evil-tutor-start
 "C-w" 'which-key-show-keymap)

;;; My leader map

(defun hide-buffers-show-windows ()
  (interactive)
  (dummy-buffers-hide)
  (dummy-windows-show))

(general-define-key
 :prefix-map 'my/buffers-map
 "B" 'counsel-buffer-or-recentf
 "b" 'ivy-switch-buffer ;; faster than counsel-switch-buffer b/c lack of preview
 "l" 'evil-switch-to-windows-last-buffer
 "k" 'kill-buffer ;; nil arg means kill current buffer (ivy auto-selects current buffer also)
 "K" 'my/kill-other-buffers
 "r" 'read-only-mode
 "s" 'my/switch-to-scratch
 ;; "u" 'which-key-undo ;; Currently exits to top-level, not previous level
 ;; "w" 'my/windows-map
 "w" 'hide-buffers-show-windows
 "v" 'view-buffer
 ;; "q" 'my/buffers-mode ;; dummy function for toggling
 "q" 'dummy-buffers-hide
 )

(hercules-def
 :keymap 'my/buffers-map
 :show-funs 'dummy-buffers-show
 ;; :toggle-funs 'my/buffers-mode ;; dummy function for toggling
 :hide-funs '(dummy-buffers-hide
	      counsel-buffer-or-recentf
	      ivy-switch-buffer
	      view-buffer
	      my/kill-other-buffers
	      my/switch-to-scratch
	      ;; which-key-undo ;; Currently exits to top-level, not previous level
	      ))

(general-define-key
 :prefix-command 'my/bookmarks-map
 "d" 'bookmark-delete
 "D" 'counsel-bookmarked-directory
 "e" 'edit-bookmarks
 "j" 'counsel-bookmark ;; TODO: Customize counsel-bookmark action
 ;; list to include delete, rename, and set
 "r" 'bookmark-rename
 "s" 'bookmark-set)

(general-define-key
 :prefix-command 'my/files-map
 "b" 'my/bookmarks-map
 "f" 'counsel-find-file
 "i" 'insert-file
 "j" 'counsel-file-jump
 "l" 'counsel-locate
 "r" 'counsel-recentf
 ;; "z" 'counssel-fzf
 )

(defun my/split-window-right-move ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/split-window-below-move ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun my/delete-other-windows-and-buffers ()
  (interactive)
  (defun select-kill-window-and-buffer (window)
    (select-window window)
    (kill-buffer-and-window))
  (let ((other-windows
	 (delq (selected-window)
	       (window-list (window-frame (selected-window)))))
	(kill-buffer-query-functions ;; Disable prompt to end process buffers
	 (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (mapc 'select-kill-window-and-buffer other-windows)))

(defun hide-windows-show-buffers ()
  (interactive)
  (dummy-windows-hide)
  (dummy-buffers-show))

(defun testing-winner-undo ()
  (winnder-undo)
  (setq this-command 'winner-undo))

(general-define-key
 :prefix-command 'my/windows-map
 "-" 'evil-window-decrease-height
 "+" 'evil-window-increase-height
 "<" 'evil-window-decrease-width
 ">" 'evil-window-increase-width
 "h" 'windmove-left
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 ;; "b" 'my/buffers-map 
 "b" 'hide-windows-show-buffers
 "v" 'my/split-window-right-move
 "x" 'my/split-window-below-move
 "a" 'ace-window
 "s" 'ace-swap-window
 "c" 'evil-window-delete
 "d" 'ace-delete-window
 "m" 'delete-other-windows
 "M" 'my/delete-other-windows-and-buffers
 "r" 'evil-window-rotate-downwards
 "R" 'evil-window-rotate-upwards
 ;; TODO: Fix and test this
 "z" 'testing-winner-undo
 "Z" 'winner-redo
 ;; "q" 'my/windows-mode ;; dummy function for toggling
 "q" 'dummy-windows-hide
 )

(hercules-def
 :keymap 'my/windows-map
 ;; :toggle-funs 'my/windows-mode ;; dummy function for toggling
 :show-funs 'dummy-windows-show
 :hide-funs '(dummy-windows-hide
	      delete-other-windows
	      my/delete-other-windows-and-buffers))


(general-define-key
  :states '(motion insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  "" nil
  "SPC" 'counsel-M-x
  "'" 'ivy-resume
  ;; "b" 'my/buffers-mode
  "b" 'dummy-buffers-show
  "f" 'my/files-map
  "o" 'clm/toggle-command-log-buffer
  ;; "w" 'my/windows-mode
  "w" 'dummy-windows-show
  )

;;; Hydra bindings

;; (general-create-definer my-definer
;;   :states '(motion insert emacs)
;;   :keymaps 'override)

;; (load "~/.emacs.d/my-hydras.el")
;; (my-definer "C-b" 'hydra-buffer/body)
;; (my-definer "C-w" 'hydra-window/body)


(defun my/kill-other-buffers ()
  "Kill other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (buffer-list))))

(defun my/switch-to-scratch ()
  "Switch buffer to *Scratch*."
  (interactive)
  (switch-to-buffer "*scratch*"))


;;; Mode-specific keybindings

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :prefix "C-j"
 "c" 'check-parens            ;; Debugging "End of file during parsing"
 "d" 'eval-defun              ;; evals outermost expression containing or following point
 ;; ...and forces reset to initial value within a defvar,
 ;; defcustom, and defface expressions
 "j" 'counsel-el
 "m" 'pp-eval-expression      ;; "m" for minibuffer, where exp is evaluated
 "s" 'pp-eval-last-sexp       ;; evals expression preceding point
 "i" 'eval-print-last-sexp    ;; "i" for insert(ing result)
 "r" 'eval-region)

;;https://github.com/noctuid/evil-guide
;;https://raw.githubusercontent.com/emacs-evil/evil/master/doc/evil.pdf
;;evil-tutor-start
;;https://www.emacswiki.org/emacs/Evil
;;https://emacs.stackexchange.com/questions/12175/instructions-on-how-to-work-with-evil-mode (see config)
;;https://github.com/emacs-evil/evil-collection
;;https://www.linode.com/docs/tools-reference/tools/emacs-evil-mode/
;;https://github.com/noctuid/evil-guide/issues/11
;;https://github.com/emacs-evil/evil/blob/3766a521a60e6fb0073220199425de478de759ad/evil-maps.el
(use-package evil
  :ensure t
  :config
  ;; (defalias 'evil-insert-state 'evil-emacs-state)    ;; Alternative to disabling insert-state bindings
  ;; Base-emacs modes to start in normal state:
  (setq evil-normal-state-modes
	'(lisp-interaction-mode                         ;; *scratch*
	  emacs-lisp-mode                               ;; .el
	  ess-r-mode)                                   ;; .R
	evil-insert-state-modes
	'(inferior-ess-r-mode)                           ;; ess R console
	)
  (evil-mode)
  (evil-escape-mode))

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
(use-package counsel ;; Installs and loads ivy and swiper as dependencies
  :ensure t
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
	ivy-help-file "~/.emacs.d/ivy-help.org" ;; Custom help file
	)
  (define-key ivy-minibuffer-map (kbd "M-m") 'ivy-mark)
  (define-key ivy-minibuffer-map (kbd "M-u") 'ivy-unmark)
  (define-key ivy-minibuffer-map [remap ivy-done] 'ivy-alt-done)
  (define-key ivy-minibuffer-map [remap ivy-alt-done] 'ivy-done)
  (with-eval-after-load "hydra"
    (defun counsel-hydra-integrate (old-func &rest args)
      "Function used to advise `counsel-hydra-heads' to work with
blue and amranath hydras."
      (hydra-keyboard-quit)
      (apply old-func args)
      (funcall-interactively hydra-curr-body-fn))
    (advice-add 'counsel-hydra-heads :around 'counsel-hydra-integrate)
    ;; Add counsel support to all of my hydras
    (define-key hydra-base-map (kbd ".") 'counsel-hydra-heads))
  ) ;; Usage within minibuffer: C-h m
;; What do these symbols do? 
;;  counsel-find-symbol
;;  counsel-semantic
;; Navigation:
;;  counsel-outline (navigates comments)
;; Completion:
;;  complete-symbol / indent-for-symbol
;;  counsel-company  
;;  counsel-jedi

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy))

;; counsel-set-variable (defcustom completion)

;; ivy-mark
;; ivy-unmark
;; ivy-push-view (https://oremacs.com/2016/06/27/ivy-push-view/)
;; ivy-pop-view

(use-package company
  :ensure t
  ;; For some reason, this enables C-h during completion
  :hook ((after-init . global-company-mode))
  :bind (:map company-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-idle-delay 0
  	company-show-numbers t ;; Use M-1, M-2, etc. to select
  	company-minimum-prefix-length .2 ;; Min # chars before completion
  	;; company-require-match nil
  	;; Remove company-echo-metadata-frontend speedup completion navigation
  	company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
  			    company-preview-if-just-one-frontend))
  (customize-set-variable 'company-global-modes
			  nil))
			  ;; '(ess-r-mode)))



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
(use-package ess
  :ensure t
  :config
  ;; Override Windows' help_type option of "html"
  ;; TODO: Set this as part of the r-newest call (C-j r) as well as
  ;; with force-buffer-current
  (cond ((eq system-type 'windows-nt)
	 (setenv "R_PROFILE" "C:\\Users\\jkroes\\.emacs.d"))
	((eq system-type 'darwin)
	 (setenv "R_PROFILE" "~/.emacs.d")))
  (require 'info-look)                ;; needed for info-lookup-other-window-flag to exist
  ;; (dolist (el '(ess-debug-minor-mode ess-r-help-mode inferior-ess-r-mode))
  ;;   (evil-set-initial-state el 'emacs))
  (add-hook 'ess-r-mode-hook
	    (lambda ()
	      (setq-local skeleton-pair t) ;; TODO: https://www.emacswiki.org/emacs/AutoPairs
	      (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
	      ;; "{" already set to skeleton-pair-insert-maybe
	      (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
	      ;; (add-hook 'ess-idle-timer-functions
	      ;; 		'ess-indent-or-complete
	      ;; 		nil
	      ;; 		'local)
	      (local-set-key (kbd "C-j") 'hydra-r/body)))
  (setq ess-ask-for-ess-directory nil
	ess-default-style 'RStudio
	;; ess-style 'RStudio
	ess-indent-offset 4 ;; override RStudio default of 2
	ess-indent-with-fancy-comments nil ;; Investigate. My comments have had crazy indentation.
	ess-nuke-trailing-whitespace t ;; might want to add to a file-save hook
	;; ess-S-quit-kill-buffers-p 'ask 
        tab-always-indent 'complete
	ess-eval-visibly nil          ;; Do not display input to iESS buffer; do not stall Emacs
	inhibit-field-text-motion nil ;; value of nil means  motions respect fields, meaning
	;; the (current) prompt acts as beginning of line (if prompt is read-only)
	ess-use-company nil
	ess-eldoc-show-on-symbol t ;; Show function signature in echo area when inside function and on symbol
	;; NOTE: May not show until first argument is completed
	ess-eldoc-abbreviation-style nil
	eldoc-echo-area-use-multiline-p t ;; May not have an effect. Unclear.
	display-buffer-alist `(("\\*company-documentation\\*"
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
			       ("\\*R:"
				(display-buffer-reuse-mode-window display-buffer-below-selected)
				(window-height . 0.5)
				(reusable-frames . nil))
			       ("\\*Help\\[R"
				(display-buffer-reuse-mode-window display-buffer-in-side-window)
				(side . right)
				(slot . 1)
				(window-width . 0.33)
				(reusable-frames . nil))
			       ;; TODO: Convert display-buffer-alist to an add statement to separate out non-ess buffers below
			       ("\\*Help\\*" display-buffer-same-window)
			       ("\\*Apropos\\*" display-buffer-same-window))
	info-lookup-other-window-flag t ;; TODO: move
	;; TODO: Specify paths to Windows binary and make this OS-conditional
	;; inferior-ess-r-program "/usr/local/bin/r"
	)
  ;; Function to add the pipe operator (set in map above)
  (defun my/add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (ess-newline-and-indent)))

;; Prevent window displaying company documentation buffer from vanishing when
;; invoking a binding not in company--electric-commands
;; (defun forget-saved-window-config ()
;;   (setq company--electric-saved-window-configuration nil))
;; (advice-add 'company-pre-command :before 'forget-saved-window-config)

(defun show-company-doc-as-ess-help ()
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
	 (obj-help (ess-display-help-on-object selected)))
    (unless obj-help
      (company-show-doc-buffer))
    ))
;; (define-key company-active-map (kbd "C-h") 'show-company-doc-as-ess-help)

;;(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)


;; other-window-scroll-buffer
;; (ess-display-help-apropos (nth company-selection company-candidates))


;; (defun switch-to-debug (old-func &rest args)
;;   (when (ess-debug-active-p)
;;     (mapcar (lambda (buffer)
;; 	      (when (string= "inferior-ess-r-mode" (buffer-local-value 'major-mode buffer))
;; 		(select-window (get-buffer-window buffer))))
;; 	    (buffer-list)))
;;   (apply old-func args))

;; (advice-add 'ess-debug-start :around 'switch-to-debug)
;; (advice-add 'ess-load-file :around 'switch-to-debug)
	      
  ;; (add-hook 'ess-debug-minor-mode-hook
  ;; 	    (lambda ()
  ;; 	      (with-temp-message "This worked?"
  ;; 		(mapcar (lambda (buffer)
  ;; 			(when (string= "inferior-ess-r-mode" (buffer-local-value 'major-mode buffer))
  ;; 			  (select-window (get-buffer-window buffer))))
  ;; 			(buffer-list)))))

;; electric-layout-mode w/ ess-r-mode-hook. Lower down in manual, it says braces are uto-indented,
;;  and user variables are provided to control amount of indentation/style
;; tab-always-indent instead of ess-tab-always-indent or ess-tab-complete-in-script
;; TODO: Make ess-load-file default to current buffer if buffer is an R script
;; TODO: set-initial-state for ess buffers
;; See section 7.4 of ess manual for comments, indents, style
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Motion.html#List-Motion -- recommended by manual
;; See sections 8 onward
;; Investigate whether C-w o is currently enough, or if ess-quit does something additional that is missing 


;;; OS-specific configuration

(cond ((eq system-type 'gnu/linux)
       ;; Hack to open URLs from within WSL using browse-url-* commands
       (when (string-match "Linux.*Microsoft.*Linux"
			   (shell-command-to-string "uname -a"))
	 (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
	       browse-url-generic-args '("/c" "start" "")
	       browse-url-browser-function 'browse-url-generic)))
      ;; Use WSL shell within runemacs.exe on Windows 10
      ;; ((eq system-type 'windows-nt)
      ;;  (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
      ;; 	     shell-file-name explicit-shell-file-name))
      )

;;; Further reading:

;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://emacs.sexy/#resources
;; https://www.reddit.com/r/emacs/comments/6s5470/useful_packages/
;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/MilesMcBain/esscss
;; https://www.masteringemacs.org/about
