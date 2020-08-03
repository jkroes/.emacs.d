;;; Hacks to integrate hydras with which-key

;; TODO: Modify which-key-undo-key to undo transient maps

(defun which-key--update ()
  "Modified which-key--update that allows hydras to display."
  (let ((prefix-keys (which-key--this-command-keys))
        delay-time)
    (cond ((and (> (length prefix-keys) 0)
                (or (keymapp (key-binding prefix-keys))
                    ;; Some keymaps are stored here like iso-transl-ctl-x-8-map
                    (keymapp (which-key--safe-lookup-key
                              key-translation-map prefix-keys))
                    ;; just in case someone uses one of these
                    (keymapp (which-key--safe-lookup-key
                              function-key-map prefix-keys)))
                (not which-key-inhibit)
                (or (null which-key-allow-regexps)
                    (which-key--any-match-p
                     which-key-allow-regexps (key-description prefix-keys)))
                (or (null which-key-inhibit-regexps)
                    (not
                     (which-key--any-match-p
                      which-key-inhibit-regexps (key-description prefix-keys))))
                ;; Do not display the popup if a command is currently being
                ;; executed
                (or (and which-key-allow-evil-operators
                         (bound-and-true-p evil-this-operator))
                    (and which-key--god-mode-support-enabled
                         (bound-and-true-p god-local-mode)
                         (eq this-command 'god-mode-self-insert))
                    (null this-command)))
           (when (and (not (equal prefix-keys (which-key--current-prefix)))
                      (or (null which-key-delay-functions)
                          (null (setq delay-time
                                      (run-hook-with-args-until-success
                                       'which-key-delay-functions
                                       (key-description prefix-keys)
                                       (length prefix-keys))))
                          (sit-for delay-time)))
             (setq which-key--automatic-display t)
             (which-key--create-buffer-and-show prefix-keys)
             (when (and which-key-idle-secondary-delay
                        (not which-key--secondary-timer-active))
               (which-key--start-timer which-key-idle-secondary-delay t))))
          ((and which-key-show-transient-maps
                (keymapp overriding-terminal-local-map))
           (which-key--create-buffer-and-show
            nil overriding-terminal-local-map))
          ((and which-key-show-operator-state-maps
                (bound-and-true-p evil-state)
                (eq evil-state 'operator)
                (not (which-key--popup-showing-p)))
           (which-key--show-evil-operator-keymap))
          (which-key--automatic-display
           (which-key--hide-popup)))))

(defun my/defhydra (name)
  "Replace the docstring of each head with that of the function used to create it, and modify
the binding description to reflect the original function name, rather than hydra's derived
name for the head.

Calls to my/defhydra should follow calls to defhydra."
  (let* ((prefix-sans-slash (symbol-name name))
     (prefix (concat prefix-sans-slash "/")))
    ;; Replacements for which-key descriptions of hydra heads. wk does at most a single
    ;; replacement, unless which-key-allow-multiple-replacements is non-nil. In lieu of setting
    ;; that, you can place more targeted regexps at the start of which-key-replacement-alist (by
    ;; push-ing the less comprehensive ones on earlier), as is this done here:
    ;; hydra-window/winner-undo -> winner-undo
    (push `((nil . ,(concat "^" prefix)) . (nil . "")) which-key-replacement-alist)
    ;; hydra-window/body -> hydra-window
    (push `((nil . ,(concat "^" prefix "body$")) . (nil . ,prefix-sans-slash)) which-key-replacement-alist)
    ;; hydra-window/my/delete-other-window-and-buffers-and-exit -> my/delete-other-windows-and-buffers
    (push `((nil . ,(concat "^" prefix "\\(.*\\)-and-exit$")) . (nil . "\\1")) which-key-replacement-alist)
    ;; hydra-window/hydra-buffer/body-and-exit -> hydra-buffer
    (push `((nil . ,(concat "^" prefix "\\(.*\\)/body-and-exit$")) . (nil . "\\1")) which-key-replacement-alist)

    ;; Retrieving/modifying docstrings in case toggled in which-key
    (dolist (h (symbol-value (intern (concat prefix "heads"))))
      (let* ((h-cmd (nth 1 h)) ;; The original command in the hydradef
         ;; hydra renames commands in several possible ways, depending on :color
         (visible-cmd1 (intern (concat prefix (symbol-name h-cmd))))
         (visible-cmd2 (intern (concat (symbol-name visible-cmd1) "-and-exit"))))
    ;; NOTE: To retrieve the original docstring defined in the function, you must
    ;; remove the function-documentation property, which shadows it
    (put (cond ((fboundp visible-cmd1) visible-cmd1)
           ((fboundp visible-cmd2) visible-cmd2))
         'function-documentation
         (if h-cmd
         (documentation h-cmd)
           ;; Note: nil is only useful for blue/amaranth hydras, which don't work
           ;; with which-key paging commands currently
           (concat "Exit " (symbol-name name))))))))


;; Hide implicit hydra commands from which-key
(push '((nil . "hydra--digit-argument") . t) which-key-replacement-alist)
(push '((nil . "hydra--negative-argument") . t) which-key-replacement-alist)
(push '((nil . "hydra--universal-argument") . t) which-key-replacement-alist)

;; Hide which-key popup for certain transient maps
(defun disable-transient-map-temporarily (orig-fun &rest args)
  (if which-key-show-transient-maps
      (progn
        (setq which-key-show-transient-maps nil)
        (apply orig-fun args)
        (setq which-key-show-transient-maps t))
    (apply orig-fun args)))
;; TODO: Add any other commands. Read up on advice and see if you need to
;; improve this ad-hoc advice above.
(advice-add 'evil-search-forward :around 'disable-transient-map-temporarily)
(advice-add 'evil-search-backward :around 'disable-transient-map-temporarily)
