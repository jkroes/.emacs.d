;; Stolen from hlissner/emacs-doom-themes
;; See also https://github.com/Alexander-Miller/treemacs#theme-support
(defun doom-themes--get-treemacs-extensions (ext)
  "Expand the extension pattern EXT into a list of extensions.
  This is used to generate extensions for `treemacs' from `all-the-icons-icon-alist'."
  (let* ((e (s-replace-all
             '((".\\?" . "") ("\\?" . "") ("\\." . "")
               ("\\" . "") ("^" . "") ("$" . "")
               ("'" . "") ("*." . "") ("*" . ""))
             ext))
         (exts (list e)))
    ;; Handle "[]"
    (when-let* ((s (s-split "\\[\\|\\]" e))
                (f (car s))
                (m (cadr s))
                (l (cadr (cdr s)))
                (mcs (delete "" (s-split "" m))))
      (setq exts nil)
      (dolist (c mcs)
        (push (s-concat f c l) exts)))
    ;; Handle '?
    (dolist (ext exts)
      (when (s-match "?" ext)
        (when-let ((s (s-split "?" ext)))
          (setq exts nil)
          (push (s-join "" s) exts)
          (push (s-concat (if (> (length (car s)) 1)
                              (substring (car s) 0 -1))
                          (cadr s)) exts))))
    exts))

(let ((face-spec 'doom-themes-treemacs-file-face))
  (treemacs-create-theme "doom-atom"
    :config
    (progn
      (treemacs-create-icon
       :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
       :extensions (root))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
       :extensions (dir-open))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
       :extensions (dir-closed))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "package" :v-adjust 0 :face face-spec)) :extensions (tag-open))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "package" :v-adjust 0 :face face-spec))
       :extensions (tag-closed))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face face-spec))
       :extensions (tag-leaf))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face face-spec))
       :extensions (error))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face face-spec))
       :extensions (warning))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face face-spec))
       :extensions (info))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-media" :v-adjust 0 :face face-spec))
       :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                    "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                    "wav" "mp3" "ogg" "midi"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
       :extensions ("adoc" "asciidoc" "bashrc" "c" "cabal" "cabal" "cask" "cc"
                    "clj" "cljc" "cljs" "cpp" "css" "csv" "cxx" "dart"
                    "dockerfile" "dockerfile" "editorconfig" "eex" "el" "elm"
                    "ex" "exs" "fish" "gitconfig" "gitignore" "go" "h" "hh"
                    "hpp" "hs" "htm" "html" "hy" "ideavimrc" "inputrc" "j2"
                    "j2" "java" "jinja2" "jinja2" "jl" "js" "json" "jsx" "kt"
                    "kt" "kts" "lhs" "lisp" "lua" "lua" "makefile" "ml" "mli"
                    "nim" "nim" "nims" "nix" "perl" "php" "pl" "plt" "pm"
                    "pm6" "pp" "pp" "py" "pyc" "r" "racket" "rb" "re" "rei"
                    "rkt" "rktd" "rktl" "rs" "sbt" "scala" "scm" "scrbl"
                    "scribble" "scss" "sh" "sql" "sql" "styles" "sv" "tex"
                    "toml" "tpp" "tridactylrc" "ts" "tsx" "v" "vagrantfile"
                    "vagrantfile" "vh" "vimperatorrc" "vimrc" "vrapperrc"
                    "vue" "xml" "xsl" "yaml" "yml" "zsh" "zshrc"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "book" :v-adjust 0 :face face-spec))
       :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                    "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                    "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or" "html"
                    "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                    "tr3" "oxps" "xps"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
       :extensions ("md" "markdown" "rst" "log" "org" "txt"
                    "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face face-spec))
       :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-pdf" :v-adjust 0 :face face-spec))
       :extensions ("pdf"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face face-spec))
       :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
       :extensions (fallback))))

  (treemacs-create-theme "doom-colors"
    :extends "doom-atom"
    :config
    (progn
      (treemacs-create-icon
       :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
       :extensions (root))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "flame" :height 0.8 :v-adjust 0 :face 'all-the-icons-red))
       :extensions (error))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "stop" :height 0.8 :v-adjust 0 :face 'all-the-icons-yellow))
       :extensions (warning))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face 'all-the-icons-green))
       :extensions (info))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-alltheicon "git" :height 0.85 :v-adjust 0.0 :face 'all-the-icons-red))
       :extensions ("gitignore" "git" "gitconfig" "gitmodules"))

      (dolist (item all-the-icons-icon-alist)
        (let* ((extensions (doom-themes--get-treemacs-extensions (car item)))
               (func (cadr item))
               (args (append (list (cadr (cdr item))) '(:v-adjust -0.05 :height 0.85) (cdr (cddr item))))
               (icon (apply func args)))
          (let* ((icon-pair (cons (format "  %s\t" icon) " "))
                 (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                 (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                 (gui-icon  (car icon-pair))
                 (tui-icon  (cdr icon-pair)))
            (--each extensions
              (ht-set! gui-icons it gui-icon)
              (ht-set! tui-icons it tui-icon)))))

      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
       :extensions ("elc")))))
