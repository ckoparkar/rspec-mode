ENV["WATCHR"] = "1"
system 'clear'

def run(cmd)
  `#{cmd}`
end

def run_all_tests
  system('clear')
  result = run "env PATH=.:$PATH cask exec ecukes"
  puts result
end

def run_test(file)
  system('clear')
  result = run "env PATH=.:$PATH cask exec ecukes"
  puts result
end

run_all_tests
watch('.*.feature') { |file| run_test file }
watch('.*.el') { run_all_tests }

# Ctrl-\
Signal.trap 'QUIT' do
  puts " --- Running all tests ---\n\n"
  run_all_tests
end

@interrupted = false

# Ctrl-C
Signal.trap 'INT' do
  exit
end
