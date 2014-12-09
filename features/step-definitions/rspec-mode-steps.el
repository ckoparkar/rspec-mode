;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(eval-when-compile (require 'cl))

(Given "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I go to character \"\\(.+\\)\"$"
  (lambda (char)
	(goto-char (point-min))
	(let ((search (re-search-forward (format "%s" char) nil t))
		  (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
	  (assert search nil message char (espuds-buffer-contents)))))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
  (lambda (pos word)
	(goto-char (point-min))
	(let ((search (re-search-forward (format "%s" word) nil t))
		  (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
	  (assert search nil message word (espuds-buffer-contents))
	  (if (string-equal "front" pos) (backward-word)))))
