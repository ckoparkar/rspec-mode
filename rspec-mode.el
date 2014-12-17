(defvar rspec-compile-command "rspec")

(defvar rspec-mode-map (make-sparse-keymap)
  "Rspec mode keymap")

(defun rspec--spec-directory-p (dir)
  (string= (f-filename dir) "spec"))

(defun rspec--spec-directory (dir)
  (cond
   ((equal dir nil) nil)
   ((rspec--spec-directory-p dir) dir)
   (t (rspec--spec-directory (f-parent dir)))
   ))

(defvar rspec-spec-directory nil)
(setq rspec-spec-directory (rspec--spec-directory buffer-file-name))

(defun rspec-goto-current-test ()
  (search-backward-regexp "x?it +[\"'].*[\"']"))

(defun rspec-toggle-deferred ()
  (interactive)
  (save-excursion
	(rspec-goto-current-test)
	(if (looking-back "x")
		(delete-char -1)
	  (insert "x")))
  )

(defun rspec-insert-into-test-buffer (&rest ignore)
  (switch-to-buffer-other-window "*rspec-test*")
  (erase-buffer)
  (insert-buffer "*compilation*")
  (kill-buffer "*compilation*")
  )

(defun rspec-run-all-tests ()
  (interactive)
  (when (equal rspec-spec-directory nil)
   (setq rspec-spec-directory (rspec--spec-directory buffer-file-name)))
  (let ((root-dir (f-parent rspec-spec-directory)))
	(compile (concat "cd " root-dir " && " rspec-compile-command " spec"))
	(add-hook 'compilation-finish-functions 'rspec-insert-into-test-buffer)
	))

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)
(define-key rspec-mode-map (kbd "C-c C-r ra") 'rspec-run-all-tests)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
