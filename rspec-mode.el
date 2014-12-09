(defun rspec-goto-current-test ()
  (search-backward-regexp "x?it[^ ]*.*do"))

(defun rspec-toggle-deferred ()
  (interactive)
  (rspec-goto-current-test)
  (if (looking-back "x")
	  (delete-char -1)
	(insert "x"))
  )

(defvar rspec-mode-map (make-sparse-keymap)
  "Rspec mode keymap")

(define-key rspec-mode-map (kbd "C-c C-r td") 'rspec-toggle-deferred)

(define-minor-mode rspec-mode
  "Rspec mode"
  nil
  " Rspec")

(provide 'rspec-mode)
