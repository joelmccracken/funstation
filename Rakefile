require "bundler/gem_tasks"
require "rake/testtask"

Rake::TestTask.new do |t|
  t.test_files = FileList['test/unit/**/*_test.rb']
  t.ruby_opts  << "-r minitest/autorun"
end

Rake::TestTask.new(:system) do |t|
  t.test_files = FileList['test/system/**/*_test.rb']
  t.ruby_opts  << "-r minitest/autorun"
end

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList['test/**/*_test.rb']
end

task :default => :spec
