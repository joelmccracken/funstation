require 'minitest/autorun'
require 'funstation'

describe "git" do
  let(:sys) {
    Sysadmin::Context.new(
      shell: shell_interface
    )
  }

  let(:shell_interface) {
    class FakeSI
      def shell_command(str)
        if str =~ /git branch --list/
          <<-OUTPUT
  foo
* bar
  baz
          OUTPUT
        else
          raise "Unknown command #{str}"
        end
      end
    end
    FakeSI
  }

  describe "in faked-out repo" do
    it "parses branches" do
      was_run_flag = false
      sys.run {
        git.branches.then { |branches|
          branches.must_equal ["foo", "* bar", "baz"]
          was_run_flag = true
        }
      }
      was_run_flag.must_equal true
    end
  end
end
