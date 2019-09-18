;; https://dev.to/huytd/emacs-from-scratch-1cg6
;; https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/ 
;; https://github.com/caisah/emacs.dz
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#helm
;; https://emacs.sexy/#resources
;; https://www.reddit.com/r/emacs/comments/6s5470/useful_packages/
;; https://github.com/emacs-tw/awesome-emacs

;; Hack to open URLs from within WSL using browse-url-* commands
(cond
 ((eq system-type 'gnu/linux)
  (when (string-match "Linux.*Microsoft.*Linux"
		      (shell-command-to-string "uname -a"))
    (setq
     browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
     browse-url-generic-args '("/c" "start" "")
     browse-url-browser-function 'browse-url-generic)
)))

;; Miminal UI for text-based emacs
(menu-bar-mode -1)

;; Provide winner-* commands
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Override help-like buffers that split the frame
;; (*info* buffers controlled by customized variable
;; info-lookup-other-window-flag)
(add-to-list 'display-buffer-alist
	     '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
	     '("*Apropos*" display-buffer-same-window))


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

(use-package evil-tutor
  :ensure t)

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; http://tuhdo.github.io/helm-intro.html
;; https://github.com/emacs-helm
;; https://github.com/emacs-helm/helm/wiki
;; http://tuhdo.github.io/helm-projectile.html
(use-package helm-config) ;; https://github.com/emacs-helm/helm/blob/master/helm-config.el
(use-package helm
  :ensure t
  :bind (("C-x r b" . helm-filtered-bookmarks)
 	 ("C-x C-f" . helm-find-files))
  :init
  (setq helm-mode-fuzzy-match t
 	helm-completion-in-region-fuzzy-match t
 	helm-candidate-number-list 50)
  :config
  (helm-mode 1))

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
  :config
  (evil-mode 1)
  ;; Set finder-mode (and subsequent package-menu-mode) in emacs state,
  ;; TODO pending better bindings for it (see evil-collection)
  (evil-set-initial-state 'finder-mode 'emacs)) ;; TODO 


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

;;undo-tree-visualize
;; https://github.com/abo-abo/swiper/wiki/ivy-display-function
(general-define-key
 :states '(motion insert) ;; Several other states inherit motion bindings
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "" nil ;; unbind SPC in evil-motion-state-map to allow its use as a prefix above
 
 "SPC" 'execute-extended-command
 "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")
 "?" 'helm-descbinds
 "!" 'shell-command
 "'" 'ivy-resume
 "." 'repeat
 "o" 'clm/toggle-command-log-buffer

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
 ;; Also see winnder for undoing layout changes: https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html#Window-Convenience
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
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(info-lookup-other-window-flag nil)
 '(package-selected-packages (quote (evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
