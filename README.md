# rspec-mode.el
rspec-mode is a minor mode for emacs to speed up development when writing tests with [rspec](http://rspec.info/)

## Keybindings

All keybindings in cucumber-mode  start with `C-c C-r` followed by an actual shortcut.

* `td`: toggle-deferred will toggle 'pending' to the name of the current test in order to defer it.
* `ra`: run all tests
* `rt`: run the current open spec file
* `rf`: run all tests from current folder
* `rg`: run all tests which match current tag

## Development

Run the tests with:
- cask exec ecukes
- gem install watchr && watchr watch-tests.watchr

