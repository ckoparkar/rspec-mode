Feature: Run single tests

  Scenario: Opens a buffer with test results
    When I turn on rspec-mode
    And I "am" in a spec file
    And I have passing tests
    And I press "C-c C-r rt"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Run test on a spec file
    When I turn on rspec-mode
    And I "am" in a spec file
    And I have passing tests
    And I press "C-c C-r rt"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Run test on a spec file
    When I turn on rspec-mode
    And I "am not" in a spec file
    And I "have" a spec file
    And I have passing tests
    And I press "C-c C-r rt"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"
