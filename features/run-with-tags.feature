Feature: Run a test with tag

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
    And I press "C-c C-r rg"
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
    And I press "C-c C-r rg"
    Then I should not run tests with "--tag"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"

  Scenario: Run test with :symbol => :symbol hash-rocket
    When I insert:
    """
    describe '#random_string' do
      it 'returns a random string of length n', :focus => :true do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r rg"
    Then I should run tests with "--tag"
    And I wait for the compilation to finish
    And I switch to buffer "*rspec-test*"
    Then I should see "failures"
