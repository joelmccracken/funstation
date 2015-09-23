require 'minitest/spec'
require 'funstation'

describe Sysadmin do
  def sys
    Sysadmin::Context.new
  end

  describe "#run" do
    it "runs things" do
      -> do
        sys.run {
          this_is_not_a_method!
        }
      end.must_raise NoMethodError
    end

    it "calls whatever is returned with the IO context" do
      was_called = false
      sys.run {
        ->(context) {
          was_called = true
          # TODO assert something about this context thing
        }
      }
      was_called.must_equal true
    end

    it "supports shell commands" do
      was_called = false
      sys.run {
        cmd("echo 'foobar'").then { |output|
          output.must_match "foobar"
          was_called = true
        }
      }
      was_called.must_equal true
    end
  end

  describe "cd" do
    it "allows changing directories" do

    end
  end
end
