(require 'rvm)
(require 'ansi-color)

(defun rspec-compile-command () "rspec")

(defvar rspec-mode-map (make-sparse-keymap)
  "Rspec mode keymap")

(defcustom rspec-use-rvm nil
  "When t, use RVM. Requires rvm.el."
  :type 'boolean
  :group 'rspec-mode)

(defun rspec--spec-directory-p (dir)
  (string= (f-filename dir) "spec"))

(defun rspec--spec-directory (dir)
  (cond
   ((equal dir nil) nil)
   ((rspec--spec-directory-p dir) dir)
   (t (rspec--spec-directory (f-parent dir)))
   ))

(defun rspec-spec-directory ()
  (rspec--spec-directory buffer-file-name))

(defun rspec--lib-directory-p (dir)
  (string= (f-filename dir) "lib"))

(defun rspec--lib-directory (dir)
  (cond
   ((equal dir nil) nil)
   ((rspec--lib-directory-p dir) dir)
   (t (rspec--lib-directory (f-parent dir)))
   ))

(defun rspec-lib-directory ()
  (rspec--lib-directory buffer-file-name))

(defun rspec-root-directory ()
  (f-parent (or (rspec-spec-directory) (rspec-lib-directory))))

(defun rspec-gemfile-exists-p ()
  (f-files (rspec-root-directory) (lambda (file) (equal (f-filename file) "Gemfile"))))

(defun rspec-dot-rspec-exists-p ()
  (f-files (rspec-root-directory) (lambda (file) (equal (f-filename file) ".rspec"))))

(defun rspec-user-compile-opts ()
  (if (rspec-dot-rspec-exists-p)
	  (concat (rspec-compile-command) " " (s-chomp (f-read (car (rspec-dot-rspec-exists-p)))))
	(rspec-compile-command)))

(defun rspec-goto-current-test ()
  (search-backward-regexp "x?it +[\"'].*[\"']"))

(defun rspec-current-spec ()
  (save-excursion
	(and
	 (rspec-goto-current-test)
	 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	))

(defun rspec-current-tag ()
  (let ((spec (rspec-current-spec)))
	(s-match "x?it +[\"'].*[\"'] *, *:?\\([[:word:]]*\\):? *[=>:]*? *[':]?\\([[:word:]]*\\)[':]? *do" spec)
	))

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
  (rspec-colorize-test-buffer)
  (kill-buffer "*compilation*")
  )

(defun rspec-goto-root-directory ()
  (concat "cd" " " (rspec-root-directory) " && "))

(defun rspec-colorize-test-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun rspec-run-command (what &optional run-tag)
  (concat (rspec-goto-root-directory)
		  (when (rspec-gemfile-exists-p)
			"bundle exec ")
		  (rspec-user-compile-opts)
		  " " what
		  (when (and run-tag (rspec-current-tag))
			(concat " --tag " (s-join ":" (rest (rspec-current-tag)))))
		  ))

(defun rspec-run (what &optional run-tag)
  (when rspec-use-rvm (rvm-use-default))
  (compile (rspec-run-command what run-tag))
  (add-hook 'compilation-finish-functions 'rspec-insert-into-test-buffer))

(defun rspec-run-all-tests ()
  (interactive)
  (rspec-run  "spec"))

(defun rspec-run-this-test ()
  (interactive)
  (rspec-run  buffer-file-name))

(defun rspec-run-all-from-folder ()
  (interactive)
  (rspec-run (f-parent buffer-file-name)))

(defun rspec-run-this-test-with-tag ()
  (interactive)
  (rspec-run buffer-file-name t))

(defun rspec-run-all-from-folder-with-tag ()
  (interactive)
  (rspec-run (f-parent buffer-file-name) t))

(defun rspec-run-all-tests-with-tag ()
  (interactive)
  (rspec-run  "spec" t))

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)
(define-key rspec-mode-map (kbd "C-c C-r ra") 'rspec-run-all-tests)
(define-key rspec-mode-map (kbd "C-c C-r rt") 'rspec-run-this-test)
(define-key rspec-mode-map (kbd "C-c C-r rf") 'rspec-run-all-from-folder)

(define-key rspec-mode-map (kbd "C-c C-r rg") 'rspec-run-this-test-with-tag)
(define-key rspec-mode-map (kbd "C-c C-r fg") 'rspec-run-all-from-folder-with-tag)
(define-key rspec-mode-map (kbd "C-c C-r ag") 'rspec-run-all-tests-with-tag)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
