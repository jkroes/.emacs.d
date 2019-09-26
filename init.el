;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#helm
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
(menu-bar-mode -1)                    ;; Miminal UI for text-based emacs
(winner-mode 1)                      ;; Provide winner-* commands
(global-display-line-numbers-mode 1) ;; Provide line numbers globally
(setq scroll-conservatively 1000000) ;; Seems to prevent auto-centering of point when scrolling

;; Package config
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize) ;; https://www.emacswiki.org/emacs/ELPA#toc5

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Packages with zero configuration
(dolist (pkg '(dracula-theme quelpa helm help-fns+ hydra))
  (unless (package-installed-p pkg)
    (cond ((string= pkg "help-fns+") (quelpa '(help-fns+ :fetcher wiki)))
	  (t (package-refresh-contents)
	     (package-install pkg))))
  (require pkg))

;; Configured packages
(use-package aggressive-indent
  :ensure t
  :config
  ;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;; Messing with inferior R process
  ;; (global-aggressive-indent-mode 1)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode) ;; example exclusion
  )

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package helm-descbinds
  :after helm
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-log-done t))

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
  ;; List of states to start in emacs mode
  (dolist (el '(Info-mode finder-mode))
    (evil-set-initial-state el 'emacs))
  )

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
	ivy-initial-inputs-alist nil) ;; disable starting regexp in search
  )

(use-package which-key
  :ensure t
  :after evil ;; equiv. to (setq which-key-allow-evil-operators t)
  :init
  (setq which-key-separator " "
	which-key-prefix-prefix "+")
  :config
  (setq which-key-show-operator-state-maps t ;; Show evil motions?
	which-key-sort-order 'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-compute-remaps t ;; e.g. w/ counsel-mode: (C-h a) apropos-command -> counsel-apropos
	which-key-show-docstrings t
	which-key-max-description-length nil
	which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-side-window-max-height .5)
  (which-key-mode))

(use-package evil-tutor
  :after evil
  :ensure t)

;; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; https://github.com/noctuid/general.el
(use-package general
  :ensure t)

(use-package command-log-mode
  :ensure t
  :config
  (global-command-log-mode)
  (setq clm/log-command-exceptions* ;; Exclude commands from command-log buffer
	(append clm/log-command-exceptions*
		(list 'evil-next-line 'evil-previous-line 'evil-insert 'evil-append-line
		      'evil-append 'evil-normal-state 'evil-forward-char
		      'evil-backward-char 'left-char 'right-char
		      'evil-force-normal-state 'evil-ex))))


;; https://github.com/abo-abo/ace-window/wiki
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (buffer-list))))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun load-init ()
  (interactive)
  (load-file user-init-file))

(defun start-r ()
  (interactive)
  (save-selected-window
    (run-ess-r)
    (ess-rdired)))

(general-define-key
 :states '(motion insert emacs) ;; Several other states inherit motion bindings
 :prefix "C-@" ;; C-SPC and C-2 (https://www.gnu.org/software/emacs/manual/html_node/emacs/Setting-Mark.html)
 :non-normal-prefix: "C-@"
 "" nil ;; if prefix was already bound, unbind it for use as prefix
 
 "SPC" 'execute-extended-command
 "?" 'helm-descbinds
 "!" 'shell-command
 "'" 'ivy-resume
 "." 'repeat
 "e" 'eval-last-sexp
 "l" 'load-init ;; Useful for testing simple configuration tweaks (a full emacs restart may be required for some changes)
 "o" 'clm/toggle-command-log-buffer
 ;;  "p" 'pp-eval-expression

 "b" '(:ignore t :wk "buffers")
 "bb" 'switch-to-buffer
 "bk" 'kill-buffer ;; nil arg means kill current buffer (ivy auto-selects current buffer also)
 "bK" 'kill-other-buffers
 "bl" 'list-buffers ;; Used only to view which buffers are files
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "br" '(read-only-mode :wk "toggle read-only")
 "bs" 'switch-to-scratch
 "bv" 'view-buffer ;; Investigate view-mode more sometime

 "d" '(:ignore t :wk "debug")
 "dc" 'check-parens ;; Debugging "End of file during parsing"
 ;; Read https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugging.html#Debugging
 
 "f" '(:ignore t :wk "files")
 "ff" 'find-file
 "fi" 'insert-file 
 "fl" 'counsel-locate
 
 "fe" '(:ignore t :wk "emacs")
 "fel" 'find-library
 
 "h" '(:ignore t :which-key "help")
 "h?" 'help-for-help
 "ha" 'apropos-command
 "hi" 'info ;; open info directory; see top (`d') for keybindings
 "hk" 'which-key-show-keymap
 "hK" 'describe-keymap
 "hp" 'finder-by-keyword ;; display docs for packages grouped by category
 "hS" 'info-lookup-symbol

 "hd" '(:ignore t :which-key "help-describe")
 "hdb" 'describe-bindings
 "hdc" 'describe-key-briefly
 "hdf" 'describe-function
 "hdk" 'describe-key
 "hdm" 'describe-mode
 "hdM" 'describe-minor-mode
 "hdp" 'describe-package ;; with evil, need to enter insert mode and press RET to select an item
 "hds" 'describe-symbol
 "hdv" 'describe-variable

 "p" '(:ignore t :which-key "prog")
 "pr" '(start-r :wk "R")


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

(defun kill-other-windows-and-buffers ()
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

(evil-define-key 'motion 'global (kbd "C-w") nil)
(evil-define-key 'insert 'global (kbd "C-w") nil)
(defhydra hydra-window (:color amaranth)
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
  ("b" switch-to-buffer "buffer")
  ("C-l" evil-switch-to-windows-last-buffer "last buffer")
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
  ("o" kill-other-windows-and-buffers "one-del-buffers" :color blue)
  ("O" delete-other-windows "one" :color blue)
  ("d" ace-delete-window "ace-del")
  ("r" evil-window-rotate-downwards "rotate")
  ("R" evil-window-rotate-upwards)
  ("z" (progn
	 (winner-undo)
	 (setq this-command 'winner-undo)) "undo") ;; Needed for winner-redo, it appears
  ("Z" winner-redo "reset")
  ("q" nil))
(global-set-key (kbd "C-w") 'hydra-window/body) ;; Take over C-w

;; Leads for making past i/o read-only:
;; https://emacs.stackexchange.com/questions/19163/how-do-i-protect-command-output-in-eshell-and-repl-buffers
;; http://emacshorrors.com/posts/comint-process-echoes.html
;; https://snarfed.org/why_i_run_shells_inside_emacs
;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L26-L48
;; (Source for prior link): https://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode
;; https://snarfed.org/why_i_run_shells_inside_emacs
(use-package ess
  :ensure t
  :config
  (require 'info-look) ;; needed for info-lookup-other-window-flag to exist
  (setq ess-ask-for-ess-directory nil
	display-buffer-alist `(("\\*R Dired"
				(display-buffer-reuse-window display-buffer-in-side-window)
				(side . right)
				(slot . -1)
				(window-width . 0.33)
				(reusable-frames . nil))
			       ("\\*R:"
				(display-buffer-reuse-window display-buffer-at-bottom)
				(window-height . 0.3)
				(reusable-frames . nil))
			        ("\\*Help\\[R"
				(display-buffer-reuse-window display-buffer-in-side-window)
				(side . right)
				(slot . 1)
				(window-width . 0.33)
				(reusable-frames . nil))
			       ("\\*Help\\*" display-buffer-same-window)
			       ("\\*Apropos\\*" display-buffer-same-window))
	info-lookup-other-window-flag t)
  (add-hook 'inferior-ess-r-mode-hook
	    (lambda ()
              (setq-local comint-prompt-read-only t) ;; read-only current prompt (">" for ess-R)
	      (setq-local comint-scroll-to-bottom-on-input t) ;; scroll to bottom before insertion and yank commands
	      (setq-local comint-scroll-to-bottom-on-output nil)  ;; alias for comint-move-point-for-output.
	      ;; Scroll to bottom when output arrives, no matter where point is (set to nil to disable)
	      (setq-local comint-scroll-show-maximum-output t)  ;; scroll to bottom when output arrives, if point is at bottom
	      (setq-local comint-use-prompt-regexp nil) ;; value of nil enables evil motions
	      (setq-local inhibit-field-text-motion nil) ;; value of nil means  motions respect fields, meaning
	      ;; the (current) prompt acts as beginning of line (if prompt is read-only)
	      )))

;; (defun my-comint-preoutput-turn-buffer-read-only (text)
;;   (propertize text 'read-only t))

;; (add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)

;;(setq inferior-ess-r-program "/mnt/c/Program Files/R/R-3.6.1/bin/R.exe")

(use-package ess-R-data-view
  :after ess
  :ensure t) ;; see ess-R-dv-pprint

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pkg aggressive-indent ess-view ess-R-data-view ess which-key use-package quelpa page-break-lines neotree hydra help-fns+ helm-descbinds general evil-tutor dracula-theme doom-themes counsel command-log-mode ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
