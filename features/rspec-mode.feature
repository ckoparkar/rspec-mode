Feature: Defer a test
  In order to quickly defer a test
  As a rspec tester
  I want to just press key please

  Scenario: Attempting to defer a test outside rspec-mode
    When I insert:
    """
    describe '#random_string' do
      it 'returns a random string of length n' do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I go to the front of the word "expect"
    And I press "C-c C-r td"
    Then I should not see "xit"

  Scenario: Deferring a test
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
    And I press "C-c C-r td"
    Then I should see "xit"
    And the cursor should be before "expect"

  Scenario: Undeferring a test
    When I insert:
    """
    describe '#random_string' do
      xit 'returns a random string of length n' do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r td"
    Then I should not see "xit"
    And the cursor should be before "expect"

  Scenario: Deferring a double quoted test
    When I insert:
    """
    describe "#random_string" do
    it "returns a random string of length n" do
    expect(subject.send(:random_string, 10).length).to eq 10
    end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r td"
    Then I should see "xit"
    And the cursor should be before "expect"

  Scenario: Undeferring a double quoted test
    When I insert:
    """
    describe "#random_string" do
    xit "returns a random string of length n" do
    expect(subject.send(:random_string, 10).length).to eq 10
    end
    end
    """
    And I turn on rspec-mode
    And I go to the front of the word "expect"
    And I press "C-c C-r td"
    Then I should see "it"
    And the cursor should be before "expect"
