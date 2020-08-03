
(defhydra hydra-r (:color pink)
  "R"
  ("SPC" ess-mark-function-or-para)
  ("a" ess-cycle-assign) ;; See how electric functions work as hydras...
  ("d" hydra-r-debug/body :color blue)
  ("e" hydra-r-eval/body :color blue)
  ("h" hydra-r-help/body :color blue)
  ("j" ess-goto-end-of-function-or-para)
  ("k" ess-goto-beginning-of-function-or-para)
  ("r" my/start-r :color blue)
  ("s" ess-switch-to-inferior-or-script-buffer :color blue)
  ("z" ess-submit-bug-report :color blue)
  ;; prog-indent-sexp
  ;; ess-indent-exp
  ;; ess-indent-new-comment-line
  ;; ess-complete-object-name
  ("q" nil))

(defhydra hydra-r-help (:color pink) ; ess-doc-map
  "R-help"
  ("a" ess-display-help-apropos)
  ("e" hydra-r-eval/body :color blue)
  ("i" ess-display-package-index)
  ("m" ess-manual-lookup)
  ("o" ess-display-help-on-object)
  ("p" ess-describe-object-at-point)
  ("r" hydra-r/body :color blue)
  ("t" ess-display-demos)
  ("v" ess-display-vignettes)
  ("w" ess-help-web-search)
  ("q" nil))

(defhydra hydra-r-eval (:color pink) ; ess-rutils-map and ess-extra-map
  "R-eval"
  ("<C-return>" ess-eval-region-or-function-or-paragraph-and-step)
  ("RET" ess-eval-region-or-line-and-step)
  ("b" ess-eval-buffer-from-beg-to-here)
  ("e" ess-eval-buffer-from-here-to-end)
  ("E" ess-dirs)
  ("f" ess-load-file)
  ("i" inferior-ess-reload)
  ;; ("P" ess-request-a-process) ;; Display selected iESS process and buffer
  ("p" ess-switch-process) ;; Switch process attached to script (current process buffer auto-displays if new,
  ;; but any script evaluation will auto-display attached process buffer if not already visible
  ("s" ess-switch-to-inferior-or-script-buffer)
  ("r" hydra-r/body :color blue)
  ("R" ess-rdired)
  ("u" ess-use-this-dir)
  ("w" ess-change-directory)
  ("q" nil))

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
(defhydra hydra-r-debug (:color pink) ;; ess-debug-minor-mode-map and ess-dev-map
  "R-debug"
  ("c" ess-debug-command-continue)
  ("f" ess-debug-flag-for-debugging) ;; base:::debug()
  ("F" ess-debug-unflag-for-debugging) ;; base:::undebug()
  ("g" ess-debug-goto-debug-point)
  ("n" ess-debug-command-next)
  ("N" next-error)
  ("p" previous-error)
  ("q" ess-debug-command-quit :color blue) ;; Investigate diff b/w this and ess-debug-stop
  ("Q" ess-debug-stop :color blue)
  ("s" ess-switch-to-ESS :color blue)
  ;; ("t" ess-debug-toggle-error-action) ;; Sets value of error option (e.g. options(error=recover)) for active process
  ;; ("u" ess-debug-command-up) ;; NOTE: currently broken. Use recover() from within debugging session (i.e. browse())
  ;; ess-debug-goto-input-event-marker
  ;; ess-debug-insert-in-forward-ring
  ("q" nil))

;; Any hydras that reference each other need to wait to call my/defhydra
;; until after both hydras have been defined. This is a current limitation
;; of my/defhydra that may be remedied in the future. Also, the name of
;; the function should be changed, if I can'tfigure out a way to use
;; my/defhydra as advice for defhydra at some point.
