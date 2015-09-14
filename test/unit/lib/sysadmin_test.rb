require 'minitest/spec'
require 'funstation'

describe Sysadmin do
  def sys
    Sysadmin::Context.new
  end

  it "does something" do
    sys.cmd("pwd") { |dir|
      dir
    }.reify.must_match /Users/
  end
end
