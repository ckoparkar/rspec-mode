Feature: Run all tests from a folder
  
  Scenario: Opens a buffer with test results
    When I turn on rspec-mode
    And I have passing tests
    And I press "C-c C-r rf"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Use bundle to run rspec when available
    When I turn on rspec-mode
    And I have passing tests
    And I have a Gemfile
    Then I should run tests with "bundle exec"
    And I press "C-c C-r rf"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Do not bundle to run rspec when not available
    When I turn on rspec-mode
    And I have passing tests
    And I dont have a Gemfile
    Then I should not run tests with "bundle exec"
    And I press "C-c C-r rf"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Run test with a tag
    When I insert:
    """
    describe '#random_string' do
      it 'returns a random string of length n', focus: true do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r fg"
    Then I should run tests with "--tag"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"


  Scenario: Run test without a tag
    When I insert:
    """
    describe '#random_string' do
      it 'returns a random string of length n' do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r fg"
    Then I should not run tests with "--tag"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"
