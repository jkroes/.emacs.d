;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://emacs.sexy/#resources
;; https://www.reddit.com/r/emacs/comments/6s5470/useful_packages/
;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/MilesMcBain/esscss
;; https://www.masteringemacs.org/about

;; OS-specific configuration
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

;; Basic configuration
(menu-bar-mode -1)                   ;; Disable menu bar
(tool-bar-mode -1)                   ;; Disable tool bar
(winner-mode 1)                      ;; Provide winner-* commands
(global-display-line-numbers-mode 1) ;; Provide line numbers globally
(setq scroll-conservatively 1000000  ;; Seems to prevent auto-centering of point when scrolling
      inhibit-startup-screen t)

;; Global bindings
(global-set-key (kbd "C-h M") 'describe-minor-mode)
(global-set-key (kbd "C-h s") 'describe-symbol)

;; Package config
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)                 ;; https://www.emacswiki.org/emacs/ELPA#toc5

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Packages with zero configuration
(dolist (pkg '(dracula-theme quelpa help-fns+ hydra ranger))
  (unless (package-installed-p pkg)
    (cond ((string= pkg "help-fns+") (quelpa '(help-fns+ :fetcher wiki)))
	  (t (package-refresh-contents)
	     (package-install pkg))))
  (require pkg))

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
  :init
  (setq evil-want-keybinding nil) ;; evil-keybindings.el mangles some mode maps (e.g., Info-mode-map) and
  ;; even motion state keymaps listed in evil-mode-map-alist
  ;; See also evil-want-minibuffer and evil-want-integration to disable other loaded files
  :config
  (evil-mode 1)
  (dolist (el '(Info-mode finder-mode))
    (evil-set-initial-state el 'emacs))
  )

(use-package evil-tutor
  :after evil
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " "
	which-key-prefix-prefix "+")
  :config
  (setq which-key-show-operator-state-maps t ;; Show evil motions?
	which-key-allow-evil-operators t
	which-key-sort-order 'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-compute-remaps t ;; e.g. w/ counsel-mode: (C-h a) apropos-command -> counsel-apropos
	which-key-show-docstrings t
	which-key-max-description-length nil
	which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-side-window-max-height .3)
  (which-key-mode)
  (global-set-key (kbd "C-h M-K") 'which-key-show-keymap) ;; Parallels binding for describe-keymap
  (global-unset-key (kbd "C-h C-h")) ;; enable which-key navigation
  )

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package org
  :ensure t
  :config
  (setq org-log-done t))

;; https://github.com/abo-abo/ace-window/wiki
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; https://oremacs.com/swiper/
;; https://github.com/abo-abo/swiper/wiki
;; https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
;; https://github.com/abo-abo/hydra/wiki/hydra-ivy-replacement
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#fn.1
(use-package counsel ;; Installs and loads ivy and swiper as dependencies
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t ;; include recent files and bookmarks in buffer list
	ivy-count-format "%d/%d " ;; show index/total results in minibuffer 
	ivy-initial-inputs-alist nil ;; disable starting regexp in search
	ivy-height 25))

(use-package command-log-mode
  :ensure t
  :config
  (global-command-log-mode)
  (setq clm/log-command-exceptions* ;; Exclude commands from command-log buffer
	(append clm/log-command-exceptions*
		'(evil-next-line evil-previous-line evil-insert evil-append-line
				 evil-append evil-normal-state evil-forward-char
				 evil-backward-char 'left-char right-char
				 evil-force-normal-state evil-ex))))

;; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; https://github.com/noctuid/general.el
(use-package general
  :after evil
  :ensure t
  :config )

(general-define-key
 :states '(motion normal visual insert emacs)
 :prefix-command 'my-map
 :prefix "SPC"
 :non-normal-prefix (cond ((string= system-type "windows-nt") "C-SPC")
			  ((string= system-type "darwin") "C-@"))
 "SPC" 'execute-extended-command
 "!" 'shell-command
 "'" 'ivy-resume
 "." 'repeat
 "e" 'eval-last-sexp
 "o" 'clm/toggle-command-log-buffer
 ;;  "p" 'pp-eval-expression

 "d" '(:ignore t :wk "debug")
 "dc" 'check-parens ;; Debugging "End of file during parsing"
 ;; Read https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugging.html#Debugging
 
 "f" '(:ignore t :wk "files")
 "ff" 'find-file
 "fi" 'insert-file 
 "fl" 'counsel-locate
 "fr" 'ranger
 
 "fe" '(:ignore t :wk "emacs")
 "fel" 'find-library
 
 "v" '(:ignore t :which-key "view")
 "vd" 'view-emacs-debugging
 "ve" 'view-echo-area-messages ;; view *Messages* buffer
 "vf" 'view-emacs-FAQ
 "vl" 'view-lossage ;; view last 300 keystrokes
 "vp" 'view-emacs-problems  
 "vr" 'info-emacs-manual
 "vt" 'help-with-tutorial ;; emacs tutorial
 "vT" 'evil-tutor-start
 )
;; (define-key evil-insert-state-map (kbd "C-@") 'my-map) ;; MacOS
;; (define-key evil-insert-state-map (kbd "C-SPC") 'my-map) ;; Windows 10
;; The following fails with a commandp error:
;; (global-set-key [remap set-mark-command] 'my-map)

(defhydra hydra-buffer (:color amaranth)
  "Buffer"
  ("b" switch-to-buffer "switch")
  ("l" evil-switch-to-windows-last-buffer "prev")
  ("k" kill-buffer "kill") ;; nil arg means kill current buffer (ivy auto-selects current buffer also)
  ("K" (lambda ()
	 (interactive)
	 (mapc 'kill-buffer
	       (delq (current-buffer)
		     (buffer-list))))
   "kill-others" :color blue)
  ("r" read-only-mode "read-only")
  ("s" (lambda ()
	 (interactive)
	 (switch-to-buffer "*scratch*"))
   "scratch")
  ("v" view-buffer "view")
  ("w" hydra-window/body "Window" :color blue)
  ("q" nil)) ;; Investigate view-mode more sometime
(evil-define-key 'motion 'global (kbd "C-b") nil)
(evil-define-key 'insert 'global (kbd "C-b") nil)
(global-set-key (kbd "C-b") 'hydra-buffer/body)


(defun my-scroll-up (&optional arg)
  (interactive)
  (save-selected-window
    (other-window arg)
    (scroll-up)))
(defun my-scroll-down (&optional arg)
  (interactive)
  (save-selected-window
    (other-window arg)
    (scroll-down)))
(defhydra hydra-window (:color amaranth)
  "Window"
  ("-" evil-window-decrease-height)
  ("+" evil-window-increase-height)
  ("<" evil-window-decrease-width)
  (">" evil-window-increase-width)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("1" (my-scroll-up 1) "scroll-up-other-1")
  ("2" (my-scroll-up 2) nil)
  ("3" (my-scroll-up 3) nil)
  ("4" (my-scroll-up 4) nil)
  ("5" (my-scroll-up 5) nil)
  ("6" (my-scroll-down 1) nil)
  ("7" (my-scroll-down 2) nil)
  ("8" (my-scroll-down 3) nil)
  ("9" (my-scroll-down 4) nil)
  ("0" (my-scroll-down 5) nil)
  ("b" hydra-buffer/body "Buffer" :color blue)
  ("v" (lambda ()
	 (interactive)
	 (split-window-right)
	 (windmove-right))
   "vert")
  ("x" (lambda ()
	 (interactive)
	 (split-window-below)
	 (windmove-down))
   "horz")
  ("a" ace-window "ace")
  ("s" ace-swap-window "swap")
  ("c" evil-window-delete "del")
  ("o" (lambda ()
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
   "one-del-buffers" :color blue)
  ("O" delete-other-windows "one" :color blue)
  ("d" ace-delete-window "ace-del")
  ("r" evil-window-rotate-downwards "rotate")
  ("R" evil-window-rotate-upwards)
  ("z" (progn
	 (winner-undo)
	 (setq this-command 'winner-undo)) "undo") ;; Needed for winner-redo, it appears
  ("Z" winner-redo "reset")
  ("q" nil))
(evil-define-key 'motion 'global (kbd "C-w") nil)
(evil-define-key 'insert 'global (kbd "C-w") nil)
(global-set-key (kbd "C-w") 'hydra-window/body) ;; Take over C-w

(defhydra hydra-r (:color pink)
  "R"
  ("SPC" ess-mark-function-or-para "mark")
  ("a" ess-cycle-assign "assign-cycle") ;; See how electric functions work as hydras...
  ("d" hydra-r-debug/body "R-debug" :color blue)
  ("e" hydra-r-eval/body "R-eval" :color blue)
  ("h" hydra-r-help/body "R-help" :color blue)
  ("j" ess-goto-end-of-function-or-para "func-end")
  ("k" ess-goto-beginning-of-function-or-para "func-beg")
  ("r" (lambda()
	 (interactive)
	 (save-selected-window
	   (run-ess-r-newest)
	   ;;(ess-rdired)
	   ))
   "R-init")
  ("s" ess-switch-to-inferior-or-script-buffer "switch" :color blue)
  ("z" ess-submit-bug-report "report-bug" :color blue)
  ("q" nil :color blue)
  ;; prog-indent-sexp
  ;; ess-indent-exp
  ;; ess-indent-new-comment-line
  ;; ess-complete-object-name 
  )

(defhydra hydra-r-help (:color blue) ;; ess-doc-map
  "R-help"
  ("a" ess-display-help-apropos "apropos")
  ("e" hydra-r-eval/body "R-eval")
  ("i" ess-display-package-index "package-index")
  ("m" ess-manual-lookup "manual")
  ("o" ess-display-help-on-object "help-on-object")
  ("p" ess-describe-object-at-point "object-at-point")
  ("r" hydra-r/body "R" :color blue)
  ("t" ess-display-demos "demos")
  ("v" ess-display-vignettes "vignettes")
  ("w" ess-help-web-search "web")
  ("q" nil)
  )

(defhydra hydra-r-eval (:color pink) ;; ess-rutils-map and ess-extra-map
  "R-eval"
  ("<C-return>" ess-eval-region-or-function-or-paragraph-and-step "reg-func-para")
  ("RET" ess-eval-region-or-line-and-step "reg-line")
  ("b" ess-eval-buffer-from-beg-to-here "to-here")
  ("e" ess-eval-buffer-from-here-to-end "to-end")
  ("E" ess-dirs "emacs-dir")
  ("f" ess-load-file "source")
  ("F" ess-force-buffer-current) ;; Only needed when a detached process is created (e.g. via request-a-process)
  ("i" inferior-ess-reload "reload-proc")
  ("p" ess-request-a-process "proc") ;; Switch process and display its buffer
  ;; ("P" ess-switch-process) ;; Switch process
  ("Q" ess-quit "ess-quit")
  ("s" ess-switch-to-inferior-or-script-buffer "switch")
  ("r" hydra-r/body "R" :color blue)
  ("R" ess-rdired "rdired")
  ("u" ess-use-this-dir "setwd-this")
  ("w" ess-change-directory "setwd")
  ("q" nil :color blue)
  ;; ess-force-buffer-current ;; Currently repeated C-g seems to work
  ;; comint-interrupt-subjob
  ;; ess-interrupt
  )

;; Note that several commands available in the inferior ess R
;; process while debugging are absent:
;; f (finish)
;; s (step)
;; help
;; where
;; <expr>
;; As such, it is best to debug from the inferior process where
;; the additional, built-in functionality is needed
;; TODO: Add commands here to ess-debug-minor-mode-map
(defhydra hydra-r-debug (:color amaranth) ;; ess-debug-minor-mode-map and ess-dev-map
  "R-debug"
  ("c" ess-debug-command-continue "continue")
  ("C" ess-debug-command-quit "quit-debug" :color blue) ;; Investigate diff b/w this and ess-debug-stop
  ("f" ess-debug-flag-for-debugging "flag-func") ;; base:::debug()
  ("g" ess-debug-goto-debug-point)
  ("n" ess-debug-command-next "next")
  ("N" next-error)
  ("p" previous-error)
  ("Q" ess-debug-stop "quit-debug-buffer" :color blue)
  ("s" ess-switch-to-ess "console" :color blue)
  ("t" ess-debug-toggle-error-action) ;; Sets value of error option (e.g. options(error=recover)) for active process
  ("u" ess-debug-command-up "frame-up") ;; TODO: Does this work without recover()?
  ("U" ess-debug-unflag-for-debugging "unflag-func") ;; base:::undebug()
  ("q" nil :color blue)
  ;; ess-debug-goto-input-event-marker
  ;; ess-debug-insert-in-forward-ring
  )

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

(use-package ess
  :after evil
  :ensure t
  :config
  (require 'info-look)                ;; needed for info-lookup-other-window-flag to exist
  (dolist (el '(ess-debug-minor-mode ess-r-help-mode inferior-ess-r-mode))
    (evil-set-initial-state el 'emacs))
  (evil-define-key 'motion 'global (kbd "C-l") nil)
  (evil-define-key 'normal 'global (kbd "C-l") nil)
  (evil-define-key 'insert 'global (kbd "C-l") nil)
  (evil-define-key 'insert 'motion (kbd "C-l") nil)
  (add-hook 'ess-r-mode-hook
	    (lambda ()
	      (setq-local skeleton-pair t) ;; TODO: https://www.emacswiki.org/emacs/AutoPairs
	      (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
	      ;; "{" already set to skeleton-pair-insert-maybe
	      (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
	      (local-set-key (kbd "C-l") 'hydra-r/body)))
  (setq ess-ask-for-ess-directory nil
	ess-S-quit-kill-buffers-p 'ask 
        tab-always-indent 'complete
	ess-eval-visibly nil          ;; Do not display input to iESS buffer; do not stall Emacs
	display-buffer-alist `(("\\*R Dired"
				(display-buffer-reuse-mode-window display-buffer-in-side-window)
				(side . right)
				(slot . -1)
				(window-width . 0.33)
				(reusable-frames . nil))
			       ("\\*R:"
				;; (display-buffer-reuse-window display-buffer-at-bottom)
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
	  ;; ess-local-process-name 'my-r-map))
  ;; (add-hook 'inferior-ess-r-mode-hook
  ;; 	    (lambda ()
  ;;             (setq-local comint-prompt-read-only t) ;; read-only current prompt (">" for ess-R)
  ;; 	      (setq-local comint-scroll-to-bottom-on-input t) ;; scroll to bottom before insertion and yank commands
  ;; 	      (setq-local comint-scroll-to-bottom-on-output nil)  ;; alias for comint-move-point-for-output.
  ;; 	      ;; Scroll to bottom when output arrives, no matter where point is (set to nil to disable)
  ;; 	      (setq-local comint-scroll-show-maximum-output t)  ;; scroll to bottom when output arrives, if point is at bottom
  ;; 	      (setq-local comint-use-prompt-regexp nil) ;; value of nil enables evil motions
  ;; 	      (setq-local inhibit-field-text-motion nil) ;; value of nil means  motions respect fields, meaning
  ;; 	      ;; the (current) prompt acts as beginning of line (if prompt is read-only)
  ;; 	      )
  ;; 	    )

  )

;; electric-layout-mode w/ ess-r-mode-hook. Lower down in manual, it says braces are uto-indented,
;;  and user variables are provided to control amount of indentation/style
;; company mode instead of autocompletion
;; tab-always-indent instead of ess-tab-always-indent or ess-tab-complete-in-script
;; TODO: Make ess-load-file default to current buffer if buffer is an R script
;; TODO: set-initial-state for ess buffers
;; See section 7.4 of ess manual for comments, indents, style
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Motion.html#List-Motion -- recommended by manual
;; See sections 8 onward
;; Investigate whether C-w o is currently enough, or if ess-quit does something additional that is missing 

;; TODO: Insert outside  of outermost expression
;; (defun insert-eval-last-sexp-result ()
;;   (interactive)
;;   (setq current-prefix-arg '(4)) ; C-u
;;   (call-interactively 'eval-last-sexp))

;; http://tuhdo.github.io/helm-intro.html
;; https://github.com/emacs-helm
;; https://github.com/emacs-helm/helm/wiki
;; http://tuhdo.github.io/helm-projectile.html
;; (use-package helm-config) ;; https://github.com/emacs-helm/helm/blob/master/helm-config.el
;; (use-package helm
;;   :ensure t
;;   :bind (("C-x r b" . helm-filtered-bookmarks)
;;  	 ("C-x C-f" . helm-find-files))
;;   :init
;;   (setq helm-mode-fuzzy-match t
;;  	helm-completion-in-region-fuzzy-match t
;;  	helm-candidate-number-list 50)
;;   :config
;;   (helm-mode 1))


;; Configured packages
;; (use-package aggressive-indent
;;   :ensure t
;;   :config
;;   ;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;; Messing with inferior R process
;;   ;; (global-aggressive-indent-mode 1)
;;   ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode) ;; example exclusion
;;   )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-collection key-chord ranger pkg aggressive-indent ess-view ess-R-data-view ess which-key use-package quelpa page-break-lines neotree hydra help-fns+ helm-descbinds general evil-tutor dracula-theme doom-themes counsel command-log-mode ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
