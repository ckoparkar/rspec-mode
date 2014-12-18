(require 'rvm)

(defvar rspec-compile-command "rspec")

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

(defun rspec-compile-command ()
  (if (rspec-gemfile-exists-p)
	  (concat "bundle exec " rspec-compile-command)
	rspec-compile-command))

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

(defun rspec-goto-root-directory ()
  (concat "cd" " " (rspec-root-directory)))

(defun rspec-run (cmd)
  (when rspec-use-rvm (rvm-use-default))
  (compile cmd)
  (add-hook 'compilation-finish-functions 'rspec-insert-into-test-buffer))

(defun rspec-run-all-tests ()
  (interactive)
  (rspec-run (concat (rspec-goto-root-directory) " && " (rspec-compile-command) " spec")))

(defun rspec-run-this-test ()
  (interactive)
  (rspec-run (concat (rspec-goto-root-directory) " && " (rspec-compile-command) " " buffer-file-name)))

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)
(define-key rspec-mode-map (kbd "C-c C-r ra") 'rspec-run-all-tests)
(define-key rspec-mode-map (kbd "C-c C-r rt") 'rspec-run-this-test)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
