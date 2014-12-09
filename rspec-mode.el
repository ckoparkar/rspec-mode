(defun rspec-goto-current-test ()
  (search-backward-regexp "x?it[^ ]*.*do"))

(defun rspec-toggle-deferred ()
  (interactive)
  (rspec-goto-current-test)
  (if (looking-back "x")
	  (delete-char -1)
	(insert "x"))
  )

(global-set-key (kbd "C-c C-r td") 'rspec-toggle-deferred)

(provide 'rspec-mode)
