Feature: Run all tests
  
  Scenario: Opens a buffer with test results
    When I turn on rspec-mode
    And I have passing tests
    And I press "C-c C-r ra"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"
