;;; rspec-mode.el --- A minor mode for rspec tests.

;; Author: Chaitanya Koparkar <ckoparkar@live.in>
;; Version: 0.0.1
;; Keywords: rspec ruby

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; rspec-mode is a minor mode for emacs to speed up development when writing tests with [rspec](http://rspec.info/)

;;; Code:

(require 'rvm)
(require 'ansi-color)

(defvar rspec-mode-map (make-sparse-keymap)
  "Rspec mode keymap")

(defcustom rspec-use-rvm nil
  "When t, use RVM. Requires rvm.el."
  :type 'boolean
  :group 'rspec-mode)

(defun rspec--spec-directory-p (dir)
  "t when DIR is a spec directory for a project"
  (string= (f-filename dir) "spec"))

(defun rspec--spec-directory (dir)
  "Returns spec directory for DIR or nil"
  (cond
   ((equal dir nil) nil)
   ((rspec--spec-directory-p dir) dir)
   (t (rspec--spec-directory (f-parent dir)))
   ))

(defun rspec-spec-directory ()
  "Returns spec directory for current file.
   for => ~/user/project/spec/a_spec.rb
   return => ~/user/project/spec"
  (rspec--spec-directory buffer-file-name))

(defun rspec--lib-directory-p (dir)
  "t when DIR is a lib directory for a project"
  (string= (f-filename dir) "lib"))

(defun rspec--lib-directory (dir)
  "Returns lib directory for DIR or nil"
  (cond
   ((equal dir nil) nil)
   ((rspec--lib-directory-p dir) dir)
   (t (rspec--lib-directory (f-parent dir)))
   ))

(defun rspec-lib-directory ()
  "Returns lib directory for current file
   For: ~/user/project/lib/a.rb
   Return: ~/user/project/lib"
  (rspec--lib-directory buffer-file-name))

(defun rspec-root-directory ()
  "Returns root directory for a project
   For: ~/user/project/lib/a.rb or ~/user/project/spec/a_spec.rb
   Return: ~/user/project/"
  (f-parent (or (rspec-spec-directory) (rspec-lib-directory))))

(defun rspec-goto-root-directory ()
  (concat "cd" " " (rspec-root-directory) " && "))

(defun rspec-gemfile-exists-p ()
  "t when Gemfile exists in project root directory"
  (f-files (rspec-root-directory) (lambda (file) (equal (f-filename file) "Gemfile"))))

(defun rspec-dot-rspec-exists-p ()
  "t when .rspec file exists in project root directory"
  (f-files (rspec-root-directory) (lambda (file) (equal (f-filename file) ".rspec"))))

(defun rspec-spec-file-p (file)
  "t when FILE is a spec file
   t for ~/user/project/spec/a_spec.rb
   nil for ~/user/project/lib/a.rb"
  (s-contains? "spec" (-last-item (s-split "_" (-last-item (s-split "/" file))))))

(defun rspec-spec-file-exists-p (file &optional spec-path-p)
  "When SPEC-PATH-P => nil, return t if lib file has corresponding spec file.
   For ~/user/project/lib/a.rb, return t if ~/user/project/spec/a_spec.rb exists
   when SPEC-PATH-P => t, return corresponding spec file path"
  (let
	  ((spec-dir (concat (rspec-root-directory) "/spec"))
	   (spec-sub-dir (concat (s-join "/"(butlast (s-split "/"(s-chop-prefix (rspec-lib-directory) file)))) "/"))
	   (spec-file-name (s-replace ".rb" "_spec.rb"(-last-item (s-split "/" buffer-file-name)))))
	(if spec-path-p
		(concat spec-dir spec-sub-dir spec-file-name)
	  (f-exists? (concat spec-dir spec-sub-dir spec-file-name)))
	))

(defun rspec-user-compile-opts ()
  "Read .rspec file and append options to compile command"
  (if (rspec-dot-rspec-exists-p)
	  (concat (rspec-compile-command) " " (s-chomp (f-read (car (rspec-dot-rspec-exists-p)))))
	(rspec-compile-command)))

(defun rspec-goto-current-test ()
  "Go to the word 'it' for a current test"
  (search-backward-regexp "x?it +[\"'].*[\"']"))

(defun rspec-current-spec ()
  "Return line beginning with 'it' for current test"
  (save-excursion
	(and
	 (rspec-goto-current-test)
	 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	))

(defun rspec-current-tag ()
  "Return tag for current test
   for => it '', focus:true
   return => 'focus:true'"
  (let ((spec (rspec-current-spec)))
	(s-match "x?it +[\"'].*[\"'] *, *:?\\([[:word:]]*\\):? *[=>:]*? *[':]?\\([[:word:]]*\\)[':]? *do" spec)
	))

(defun rspec-toggle-deferred ()
  "Toggle current test between xit/it"
  (interactive)
  (save-excursion
	(rspec-goto-current-test)
	(if (looking-back "x")
		(delete-char -1)
	  (insert "x")))
  )

(defun rspec-insert-into-test-buffer (&rest ignore)
  "Insert contents of *compilation* buffer into *rspec-test*"
  (switch-to-buffer-other-window "*rspec-test*")
  (erase-buffer)
  (insert-buffer "*compilation*")
  (rspec-colorize-test-buffer)
  (kill-buffer "*compilation*")
  )

(defun rspec-colorize-test-buffer ()
  "Parse ansi-color sequences for better o/p"
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(defun rspec-compile-command () "rspec")

(defun rspec-run-command (what &optional run-tag)
  "Returns final command to run rspec with"
  (concat (rspec-goto-root-directory)
		  (when (rspec-gemfile-exists-p)
			"bundle exec ")
		  (rspec-user-compile-opts)
		  " " what
		  (when (and run-tag (rspec-current-tag))
			(concat " --tag " (s-join ":" (rest (rspec-current-tag)))))
		  ))

(defun rspec-run (what &optional run-tag)
  "Generic command that compiles rspec-run command"
  (when rspec-use-rvm (rvm-use-default))
  (compile (rspec-run-command what run-tag))
  (add-hook 'compilation-finish-functions 'rspec-insert-into-test-buffer))


(defun rspec-run-this-test ()
  "Run spec for current file"
  (interactive)
  (cond
   ((rspec-spec-file-p buffer-file-name) (rspec-run buffer-file-name))
   ((rspec-spec-file-exists-p buffer-file-name) (rspec-run (rspec-spec-file-exists-p buffer-file-name t)))
   (t (message "In TDD we trust. No specs exist for this file."))
   ))

(defun rspec-run-all-from-folder ()
  "Run all spec tests from a folder"
  (interactive)
  (rspec-run (f-parent buffer-file-name)))

(defun rspec-run-all-tests ()
  "Run all tests from spec/ dir"
  (interactive)
  (rspec-run  "spec"))

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
