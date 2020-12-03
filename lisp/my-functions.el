;; (defun my/init-maybe-profile (&optional when?)
;;   "Optionally profile load before or after package initialization. See results of
;; profiling with `benchmark-init/show-durations-tree' (calls shown in reverse order)
;; or in a sortable table with `benchmark-init/show-durations-tabulated'.

;; when? can have values of before-init, after-init, or anything else for no profiling."
;;   (defun my/profile ()
;;     (load (concat (car (file-expand-wildcards "~/.emacs.d/elpa/benchmark-init*" t))
;;                   "/benchmark-init"))
;;     (add-hook 'after-init-hook 'benchmark-init/deactivate)
;;     (benchmark-init/activate))

;;   (defun my/init ()
;;     ;; Configure package.el
;;     (require 'package)
;;     (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
;;                              ("melpa" . "https://melpa.org/packages/")))
;;     ;; Add pkg files to load-path and Info-directory-list; ready autoloads
;;     (package-initialize))

;;   (pcase when?
;;     ('before-init (my/profile) (my/init))
;;     ('after-init (my/init) (my/profile))
;;     (_ (my/init))))  ;; Includes nil argument

;; (defun my/start-r ()
;;   "Start an R process."
;;   (interactive)
;;   (save-selected-window
;;     (run-ess-r)
;;     ;;(ess-rdired)
;;     )
;;   (ess-force-buffer-current))
