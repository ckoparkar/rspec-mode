# rspec-mode.el [![Build Status](https://travis-ci.org/cskksc/rspec-mode.svg)](https://travis-ci.org/cskksc/rspec-mode)
rspec-mode is a minor mode for emacs to speed up development when writing tests with [rspec](http://rspec.info/)

## Keybindings

All keybindings in cucumber-mode  start with `C-c C-r` followed by an actual shortcut.

* `td`: toggle-deferred will toggle 'xit/it' in theg current test in order to defer it.
* `ra`: run all tests
* `rt`: run this test (run the current open spec file)
* `rf`: run this folder (run all tests from current folder)
* `rg`: run this test with tag
* `fg`: run folder with tag
* `ag`: run all with tag

## Development

Run the tests with:
- cask exec ecukes
- gem install watchr && watchr watch-tests.watchr
