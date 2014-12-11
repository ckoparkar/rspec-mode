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

(defun rspec-run-all-tests ()
  (interactive)
  (switch-to-buffer-other-window "*rspec-test*")
  (call-process "rspec" nil "*rspec-test*" t)
  )

(defvar rspec-mode-map (make-sparse-keymap)
  "Rspec mode keymap")

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)
(define-key rspec-mode-map (kbd "C-c C-r ra") 'rspec-run-all-tests)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
