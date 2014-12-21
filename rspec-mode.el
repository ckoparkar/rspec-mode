(require 'rvm)
(require 'ansi-color)

(defvar rspec-compile-command "rspec -c -f d")

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

(defun rspec-root-directory ()
  (f-parent (rspec-spec-directory)))

(defun rspec-gemfile-exists-p ()
  (f-files (rspec-root-directory) (lambda (file) (equal (f-filename file) "Gemfile"))))

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
  (rspec-colorize-test-buffer)
  (kill-buffer "*compilation*")
  )

(defun rspec-goto-root-directory ()
  (concat "cd" " " (rspec-root-directory)))

(defun rspec-colorize-test-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun rspec-run-command (what)
  (concat (rspec-goto-root-directory)
		  " && "
		  (if (rspec-gemfile-exists-p)
			  (concat "bundle exec " rspec-compile-command)
			rspec-compile-command)
		  " " what))

(defun rspec-run (what)
  (when rspec-use-rvm (rvm-use-default))
  (compile (rspec-run-command what))
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

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)
(define-key rspec-mode-map (kbd "C-c C-r ra") 'rspec-run-all-tests)
(define-key rspec-mode-map (kbd "C-c C-r rt") 'rspec-run-this-test)
(define-key rspec-mode-map (kbd "C-c C-r rf") 'rspec-run-all-from-folder)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
