(with-eval-after-load "counsel" ;; counsel defines "." in hydra-base-map
  (defhydra hydra-buffer ()
    "Buffer"
    ("B" counsel-buffer-or-recentf :color blue)
    ("b" ivy-switch-buffer :color blue) ;; faster than counsel-switch-buffer b/c lack of preview
    ("l" evil-switch-to-windows-last-buffer)
    ("k" kill-buffer) ;; nil arg means kill current buffer (ivy auto-selects current buffer)
    ("K" my/kill-other-buffers :color blue)
    ("r" read-only-mode)
    ("s" my/switch-to-scratch)
    ("v" view-buffer)
    ("w" hydra-window/body :color blue)) 

  ;; TODO: Set which-key-max-description-length to less than 1/2 window; otherwise buffer can't show
  ;; TODO: See counsel-hydra-integrate below. Need to set before you define other hydras?
  (defhydra hydra-window ()
    "Window"
    ("-" evil-window-decrease-height)
    ("+" evil-window-increase-height)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("b" hydra-buffer/body :color blue)
    ("v" my/split-window-right-move)
    ("x" my/split-window-below-move)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("c" evil-window-delete)
    ("d" ace-delete-window)
    ("m" delete-other-windows :color blue)
    ("M" my/delete-other-windows-and-buffers :color blue)
    ("r" evil-window-rotate-downwards)
    ("R" evil-window-rotate-upwards)
    ("z" winner-undo)
    ;;("z" my/hydra-winner-undo)
    ;; ("z" (progn
    ;; 	 (winner-undo)
    ;; 	 (setq this-command 'winner-undo))
    ;;  "winner-undo") ;; Needed for winner-redo, it appears
    ("Z" winner-redo))

  ;; (defhydra hydra-r ()
  ;;   "R"
  ;;   ("SPC" ess-mark-function-or-para "mark")
  ;;   ("a" ess-cycle-assign "assign-cycle") ;; See how electric functions work as hydras...
  ;;   ("d" hydra-r-debug/body "R-debug" :color blue)
  ;;   ("e" hydra-r-eval/body "R-eval" :color blue)
  ;;   ("h" hydra-r-help/body "R-help" :color blue)
  ;;   ("j" ess-goto-end-of-function-or-para "func-end")
  ;;   ("k" ess-goto-beginning-of-function-or-para "func-beg")
  ;;   ("r" (lambda()
  ;; 	 (interactive)
  ;; 	 (save-selected-window
  ;; 	   (run-ess-r-newest)
  ;; 	   ;;(ess-rdired)
  ;; 	   ))
  ;;    "R-init")
  ;;   ("s" ess-switch-to-inferior-or-script-buffer "switch" :color blue)
  ;;   ("z" ess-submit-bug-report "report-bug" :color blue)
  ;;   ("q" nil :color blue)
  ;;   ;; prog-indent-sexp
  ;;   ;; ess-indent-exp
  ;;   ;; ess-indent-new-comment-line
  ;;   ;; ess-complete-object-name 
  ;;   )

  ;; (defhydra hydra-r-help () ;; ess-doc-map
  ;;   "R-help"
  ;;   ("a" ess-display-help-apropos "apropos")
  ;;   ("e" hydra-r-eval/body "R-eval")
  ;;   ("i" ess-display-package-index "package-index")
  ;;   ("m" ess-manual-lookup "manual")
  ;;   ("o" ess-display-help-on-object "help-on-object")
  ;;   ("p" ess-describe-object-at-point "object-at-point")
  ;;   ("r" hydra-r/body "R" :color blue)
  ;;   ("t" ess-display-demos "demos")
  ;;   ("v" ess-display-vignettes "vignettes")
  ;;   ("w" ess-help-web-search "web")
  ;;   ("q" nil)
  ;;   )

  ;; (defhydra hydra-r-eval () ;; ess-rutils-map and ess-extra-map
  ;;   "R-eval"
  ;;   ("<C-return>" ess-eval-region-or-function-or-paragraph-and-step "reg-func-para")
  ;;   ("RET" ess-eval-region-or-line-and-step "reg-line")
  ;;   ("b" ess-eval-buffer-from-beg-to-here "to-here")
  ;;   ("e" ess-eval-buffer-from-here-to-end "to-end")
  ;;   ("E" ess-dirs "emacs-dir")
  ;;   ("f" ess-load-file "source")
  ;;   ("F" ess-force-buffer-current) ;; Only needed when a detached process is created (e.g. via request-a-process)
  ;;   ("i" inferior-ess-reload "reload-proc")
  ;;   ("P" ess-request-a-process "iESS proc") ;; Switch iESS process and its buffer
  ;;   ;; in the iESS buffer
  ;;   ("p" ess-switch-process "ESS proc") ;; Switch process attached to script
  ;;   ("s" ess-switch-to-inferior-or-script-buffer "switch")
  ;;   ("r" hydra-r/body "R" :color blue)
  ;;   ("R" ess-rdired "rdired")
  ;;   ("u" ess-use-this-dir "setwd-this")
  ;;   ("w" ess-change-directory "setwd")
  ;;   ("q" nil :color blue)
  ;;   ;; ess-force-buffer-current ;; Currently repeated C-g seems to work
  ;;   ;; comint-interrupt-subjob
  ;;   ;; ess-interrupt
  ;;   )

  ;; ;; Note that several commands available in the inferior ess R
  ;; ;; process while debugging are absent:
  ;; ;; f (finish)
  ;; ;; s (step)
  ;; ;; help
  ;; ;; where
  ;; ;; <expr>
  ;; ;; As such, it is best to debug from the inferior process where
  ;; ;; the additional, built-in functionality is needed
  ;; ;; TODO: Add commands here to ess-debug-minor-mode-map
  ;; (defhydra hydra-r-debug () ;; ess-debug-minor-mode-map and ess-dev-map
  ;;   "R-debug"
  ;;   ("c" ess-debug-command-continue "continue")
  ;;   ("C" ess-debug-command-quit "quit-debug" :color blue) ;; Investigate diff b/w this and ess-debug-stop
  ;;   ("f" ess-debug-flag-for-debugging "flag-func") ;; base:::debug()
  ;;   ("g" ess-debug-goto-debug-point)
  ;;   ("n" ess-debug-command-next "next")
  ;;   ("N" next-error)
  ;;   ("p" previous-error)
  ;;   ("Q" ess-debug-stop "quit-debug-buffer" :color blue)
  ;;   ("s" ess-switch-to-ess "console" :color blue)
  ;;   ("t" ess-debug-toggle-error-action) ;; Sets value of error option (e.g. options(error=recover)) for active process
  ;;   ("u" ess-debug-command-up "frame-up") ;; TODO: Does this work without recover()?
  ;;   ("U" ess-debug-unflag-for-debugging "unflag-func") ;; base:::undebug()
  ;;   ("q" nil :color blue)
  ;;   ;; ess-debug-goto-input-event-marker
  ;;   ;; ess-debug-insert-in-forward-ring
  ;;   )

  ;; Any hydras that reference each other need to wait to call my/defhydra
  ;; until after both hydras have been defined. This is a current limitation
  ;; of my/defhydra that may be remedied in the future. Also, the name of
  ;; the function should be changed, if I can'tfigure out a way to use
  ;; my/defhydra as advice for defhydra at some point.
  (load "which-key-hacks")
  (my/defhydra 'hydra-window) ;; Needs to run after hydra-buffer is defined
  (my/defhydra 'hydra-buffer) ;; Needs to run after hydra-window is defined
)
