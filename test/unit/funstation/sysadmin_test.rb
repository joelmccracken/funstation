require 'minitest/spec'
require 'funstation'

describe Sysadmin do
  def sys
    Sysadmin::Context.new
  end

  it "runs things" do
    -> do
      sys.run {
        this_is_not_a_method!
      }
    end.must_raise NoMethodError
  end

  it "does something" do
    sys.run {
      cmd("pwd").then { |dir|
        dir.must_match /Users/
      }
    }
  end
end
