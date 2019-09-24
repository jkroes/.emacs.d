;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#helm
;; https://emacs.sexy/#resources
;; https://www.reddit.com/r/emacs/comments/6s5470/useful_packages/
;; https://github.com/emacs-tw/awesome-emacs
;; https://github.com/MilesMcBain/esscss
;; https://www.masteringemacs.org/about

(cond
 ;; Use WSL shell within runemacs.exe on Windows 10
 ;; ((eq system-type 'windows-nt)
 ;;  (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"
 ;; 	shell-file-name explicit-shell-file-name))
 ;; Hack to open URLs from within WSL using browse-url-* commands
 ((eq system-type 'gnu/linux)
  (when (string-match "Linux.*Microsoft.*Linux"
		      (shell-command-to-string "uname -a"))
    (setq
     browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
     browse-url-generic-args '("/c" "start" "")
     browse-url-browser-function 'browse-url-generic))))

;; Miminal UI for text-based emacs
(menu-bar-mode -1)

;; Provide winner-* commands
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Provide line numbers globally
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

;; Override help-like buffers that split the frame
(add-to-list 'display-buffer-alist
	     '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
	     '("*Apropos*" display-buffer-same-window))
(setq info-lookup-other-window-flag t)

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

;; Packages
(use-package dracula-theme
  :ensure t)

(use-package quelpa
  :ensure t)

(unless (package-installed-p 'help-fns+)
  (quelpa
   '(help-fns+ :fetcher wiki)))
(require 'help-fns+)

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

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

(use-package helm-descbinds
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
  (dolist (el
	   '(Info-mode finder-mode))
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
  :init
  (setq which-key-separator " "
	which-key-prefix-prefix "+")
  :config
  (setq which-key-allow-evil-operators t ;; Show evil inner and outer text objects?
	which-key-show-operator-state-maps t ;; Show evil motions?
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

;; https://github.com/abo-abo/hydra
;; https://github.com/abo-abo/hydra/wiki
(use-package hydra
  :ensure t)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun load-init ()
  "Load user-init-file"
  (interactive)
  (load-file user-init-file))

(defun start-r ()
  (interactive)
  (save-selected-window
    (run-ess-r)
    (ess-rdired)))

;;undo-tree-visualize
;; https://github.com/abo-abo/swiper/wiki/ivy-display-function
(general-define-key
 :states '(motion insert) ;; Several other states inherit motion bindings
 :prefix "M-SPC"
 :non-normal-prefix "M-SPC"
 ;; "" nil ;; unbind SPC in evil-motion-state-map to allow its use as a prefix above
 
 "SPC" 'execute-extended-command
 "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")
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
 "pr" '(:ignore t :which-key "R")
 "prr" 'start-r  
 
 "v" '(:ignore t :which-key "view")
 "vd" 'view-emacs-debugging
 "ve" 'view-echo-area-messages ;; view *Messages* buffer
 "vf" 'view-emacs-FAQ
 "vl" 'view-lossage ;; view last 300 keystrokes
 "vp" 'view-emacs-problems  
 "vr" 'info-emacs-manual
 "vt" 'help-with-tutorial ;; emacs tutorial
 "vT" 'evil-tutor-start

 ;; Modified from https://www.youtube.com/watch?v=_qZliI1BKzI
 ;; NOTE: Had trouble binding non-interactive functions like aw-flip-window
 ;; Check out functions listed in link (e.g. hydra-window-scroll)
 "w" (list
      (defhydra hydra-window
	(:color amaranth) ;; Prevent bindings outside this hydra (doesn't override SPC--investigate)
	"window"
	("h" windmove-left)
	("j" windmove-down)
	("k" windmove-up)
	("l" windmove-right)
	("b" ivy-switch-buffer)
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
	("o" delete-other-windows "one" :color blue)
	("a" ace-window "ace")
	("s" ace-swap-window "swap")
	("d" ace-delete-window "del")
	("q" nil "cancel")
	("z" (progn
	       (winner-undo)
	       (setq this-command 'winner-undo))) ;; Needed for winner-redo, it appears
	("Z" winner-redo))
      :which-key "window")
 ;; "wn" 'evil-window-new ;; horizontal split, blank window
 ;; "wr" 'evil-window-rotate-downwards
 ;; "wR" 'evil-window-rotate-upwards
 ;; "ww" 'evil-window-next
 ;; "wW" 'evil-window-prev
 )

(use-package ess
  :ensure t
  :config
  (setq ess-ask-for-ess-directory nil))

;; Compare to ess-view
(use-package ess-R-data-view
  :ensure t) ;; see  ess-R-dv-pprint

(setq display-buffer-alist
      `(("*R Dired"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . -1)
	 (window-width . 0.33)
	 (reusable-frames . nil))
	("*R"
	 (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-height . 0.3)
  	 (reusable-frames . nil))
	("*Help"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . 1)
	 (window-width . 0.33)
	 (reusable-frames . nil))))


;; TODO: Insert outside  of outermost expression
(defun insert-eval-last-sexp-result ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'eval-last-sexp))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ess-R-data-view ess which-key use-package quelpa page-break-lines neotree hydra help-fns+ helm-descbinds general evil-tutor dracula-theme doom-themes counsel command-log-mode ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
