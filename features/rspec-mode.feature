Feature: Defer a test
  In order to quickly defer a test
  As a rspec tester
  I want to just press key please

  Scenario: Deferring a test
    When I insert:
    """
    describe '#random_string' do
      it 'returns a random string of length n' do
        expect(subject.send(:random_string, 10).length).to eq 10
      end
    end
    """
    And I go to the front of the word "it"
    And I press "C-c C-r td"
    Then I should see "xit"
