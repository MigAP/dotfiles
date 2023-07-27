(defun my-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))


(defun my-insert-rel-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.
  
  Uses relative path to file name from current directory,
  `default-directory'.  See `file-relative-name'."
  (interactive "*fInsert file name: \nP")
         (insert (file-relative-name filename)))

(global-set-key "\C-c\C-i" 'my-insert-rel-file-name)

;; Stolen from: https://www.emacswiki.org/emacs/InteractiveSpell#h5o-12 
(defun dired-do-ispell (&optional arg)
  (interactive "P")
  (dolist (file (dired-get-marked-files
                 nil arg
                 #'(lambda (f)
                     (not (file-directory-p f)))))
    (save-window-excursion
      (with-current-buffer (find-file file)
        (ispell-buffer)))
    (message nil)))
