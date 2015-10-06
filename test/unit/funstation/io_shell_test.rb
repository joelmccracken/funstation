require 'minitest/spec'
require 'funstation'

describe Sysadmin::IO::Shell do
  let(:shell) {
    Sysadmin::IO::Shell.new
  }
  it "works" do
    next
    shell.cmd("echo 'testing 1 2 3'").must_equal "testing 1 2 3\n"
  end

  it "maintains state" do
    next
    dir = shell.cmd("mktemp -d -t foo")
    shell.cmd("cd #{dir}")
    shell.cmd("pwd").must_equal dir
  end
end
