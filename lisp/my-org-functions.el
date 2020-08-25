(defun my/org-file (dir file)
  "Open org file."
  (interactive)
  (find-file (concat (file-name-as-directory dir) file)))

(defun my/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun my/org-attach-reveal-in-emacs ()
  "Show the attachment directory of the current outline node in dired.
Will create an attachment and folder if it doesn't exist yet.
Respects `org-attach-preferred-new-method'."
  (interactive)
  (deer (org-attach-dir-get-create)))

(defun my/org-open-at-point (&optional arg)
  "Modified to distinguish between headings with attached directories and
files, and to open directories as normal for the original
function--org-attach-reveal or org-attach-reveal-in-emacs--but
to use org-attach-open or org-attach-open-in-emacs for attached files (i.e.,
attachments with the ID property)."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
            ;; Only consider supported types, even if they are not the
            ;; closest one.
            (org-element-lineage
             (org-element-context)
             '(clock comment comment-block footnote-definition
                     footnote-reference headline inline-src-block inlinetask
                     keyword link node-property planning src-block timestamp)
             t))
           (type (org-element-type context))
           (value (org-element-property :value context))
           (my/id (org-element-property :ID context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; No valid link at point.  For convenience, look if something
       ;; looks like a link under point in some specific places.
       ((memq type '(comment comment-block node-property keyword))
        (call-interactively #'org-open-at-point-global))
       ;; On a headline or an inlinetask, but not on a timestamp,
       ;; a link, a footnote reference.
       ((memq type '(headline inlinetask))
        (org-match-line org-complex-heading-regexp)
        (if (and (match-beginning 5)
                 (>= (point) (match-beginning 5))
                 (< (point) (match-end 5)))
            ;; On tags.
            (org-tags-view
             arg
             (save-excursion
               (let* ((beg (match-beginning 5))
                      (end (match-end 5))
                      (beg-tag (or (search-backward ":" beg 'at-limit) (point)))
                      (end-tag (search-forward ":" end nil 2)))
                 (buffer-substring (1+ beg-tag) (1- end-tag)))))
          ;; Not on tags.
          (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
            (`(nil . ,_)
             (require 'org-attach)
             (if my/id
                 (progn
                   (message "Opening attachment-file")
                   (if (equal arg '(4))
                       (org-attach-open-in-emacs)
                     (org-attach-open)))
               (message "Opening attachment-dir")
               (if (equal arg '(4))
                   (my/org-attach-reveal-in-emacs)
                 (org-attach-reveal))))
            (`(,links . ,links-end)
             (dolist (link (if (stringp links) (list links) links))
               (search-forward link nil links-end)
               (goto-char (match-beginning 0))
               (org-open-at-point arg))))))
       ;; On a footnote reference or at definition's label.
       ((or (eq type 'footnote-reference)
            (and (eq type 'footnote-definition)
                 (save-excursion
                   ;; Do not validate action when point is on the
                   ;; spaces right after the footnote label, in order
                   ;; to be on par with behavior on links.
                   (skip-chars-forward " \t")
                   (let ((begin
                          (org-element-property :contents-begin context)))
                     (if begin (< (point) begin)
                       (= (org-element-property :post-affiliated context)
                          (line-beginning-position)))))))
        (org-footnote-action))
       ;; On a planning line.  Check if we are really on a timestamp.
       ((and (eq type 'planning)
             (org-in-regexp org-ts-regexp-both nil t))
        (org-follow-timestamp-link))
       ;; On a clock line, make sure point is on the timestamp
       ;; before opening it.
       ((and (eq type 'clock)
             value
             (>= (point) (org-element-property :begin value))
             (<= (point) (org-element-property :end value)))
        (org-follow-timestamp-link))
       ((eq type 'src-block) (org-babel-open-src-block-result))
       ;; Do nothing on white spaces after an object.
       ((>= (point)
            (save-excursion
              (goto-char (org-element-property :end context))
              (skip-chars-backward " \t")
              (point)))
        (user-error "No link found"))
       ((eq type 'inline-src-block) (org-babel-open-src-block-result))
       ((eq type 'timestamp) (org-follow-timestamp-link))
       ((eq type 'link) (org-link-open context arg))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

(defun my/org-open-at-point-in-emacs ()
  "Make org-open-at-point open attachments in Emacs"
  (interactive)
  (my/org-open-at-point '(4))) ; C-u org-open-at-point
