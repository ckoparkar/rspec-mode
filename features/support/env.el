(require 'f)

(defvar rspec-mode-support-path
  (f-dirname load-file-name))

(defvar rspec-mode-features-path
  (f-parent rspec-mode-support-path))

(defvar rspec-mode-root-path
  (f-parent rspec-mode-features-path))

(add-to-list 'load-path rspec-mode-root-path)

(require 'rspec-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 (switch-to-buffer
  (get-buffer-create "*rspec-mode*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (setq set-mark-default-inactive nil)
 (deactivate-mark)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
