Feature: Run all tests
  
  Scenario: Opens a buffer with test results
    When I turn on rspec-mode
    And I have passing tests
    And I press "C-c C-r ra"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Use bundle to run rspec when available
    When I turn on rspec-mode
    And I have passing tests
    And I have a Gemfile
    Then I should run tests with "bundle exec"
    And I press "C-c C-r ra"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Do not bundle to run rspec when not available
    When I turn on rspec-mode
    And I have passing tests
    And I dont have a Gemfile
    Then I should not run tests with "bundle exec"
    And I press "C-c C-r ra"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"
