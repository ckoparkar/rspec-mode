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

(When "^I wait for the compilation to finish$"
  (lambda ()
	(setq ecukes--waiting-for-compilation t)

	(defun ecukes--compilation-finished (&rest ignore)
	  (setq ecukes--waiting-for-compilation nil)
	  (remove-hook  'compilation-finish-functions 'ecukes--compilation-finished)
	  )

	(add-hook  'compilation-finish-functions 'ecukes--compilation-finished)
    (while ecukes--waiting-for-compilation
	  (accept-process-output nil 0.005))))

(When "^I have passing tests$"
  (lambda ()
	(setq rspec-compile-command "./rspec")
	(defun rspec-spec-directory () "./spec")
	(defun rspec-root-directory () ".")
	(defun f-parent (arg) ".")
    ))

(And "^I have a Gemfile$"
  (lambda ()
	(defun rspec-gemfile-exists-p () t)
    ))

(Then "^I should run tests with \"\\([^\"]+\\)\"$"
	  (lambda (expected)
		(let ((actual (rspec-run-command "spec"))
			  (message "Expected '%s' to be part of '%s', but was not."))
		  (cl-assert (s-contains? expected actual) nil message expected actual))))

(And "^I dont have a Gemfile$"
	 (lambda ()
	   (defun rspec-gemfile-exists-p () nil)
	   ))

(Then "^I should not run tests with \"\\([^\"]+\\)\"$"
	  (lambda (expected)
		(let ((actual (rspec-run-command "spec"))
			  (message "Expected '%s' to be part of '%s', but was not."))
		  (cl-assert (not (s-contains? expected actual)) nil message expected actual))))
