; From custom:

   ;; (quote
   ;;  (jupyter emacs-jupyter evil-mc evil-surround multiple-cursors key-chord company projectile counsel hercules company-tabnine 0x0 flx evil-escape ranger pkg aggressive-indent ess-R-data-view ess which-key use-package quelpa page-break-lines hydra help-fns+ helm-descbinds general evil-tutor dracula-theme ffcounsel command-log-mode ace-window)))

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


(use-package jupyter
  :config
  (pcase system-type
    ('gnu/linux
     (add-to-list 'exec-path "/home/jkroes/.local/bin") ;; path to jupyter
     (setq python-shell-interpreter "python3.6"))))

  (add-to-list 'exec-path "c:/Users/jkroes/Documents/Python") ;; interpreter
  (add-to-list 'exec-path "c:/Users/jkroes/Documents/Python/Scripts") ;; pip
  (setenv "PATH" (concat "C:/Users/jkroes/Documents/Python"
                         "c:/Users/jkroes/Documents/Python/Scripts"
                         (getenv "PATH")))) ;; Shell path to interpreter and pip

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
                  (window-width . 0.5)
                  (reusable-frames . nil))
                 ("\\*R"
                  (display-buffer-reuse-mode-window display-buffer-below-selected)
                  (window-height . 0.2)
                  (reusable-frames . nil))
                 ("\\*Help\\[R"
                  (display-buffer-reuse-mode-window display-buffer-in-side-window)
                  (side . right)
                  (slot . 1)
                  (window-width . 0.5)
                  (reusable-frames . nil))
                 ("\\*Help\\*" display-buffer-same-window)
                 ("\\*Apropos\\*" display-buffer-same-window)))


;; (general-create-definer my-definer
;;   :states '(motion insert emacs)
;;   :prefix "SPC"
;;   :non-normal-prefix (general-chord "fd"))
;; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; (use-package key-chord
;;   :config
;;   (key-chord-mode 1))

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
