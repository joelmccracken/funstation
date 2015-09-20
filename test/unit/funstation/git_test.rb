require 'minitest/autorun'
require 'funstation'

describe "git" do
  let(:sys) {
    Sysadmin::Context.new(
      shell: shell_interface
    )
  }

  let(:shell_interface) {
    class FakeSI < Sysadmin::Monad
      def initialize(cmd)
        @cmd = cmd
      end

      def run
        @cmd
      end
    end
    FakeSI
  }

  describe "in faked-out repo" do
    it "works" do
      next
      was_run_flag = false

      sys.run {
        git.branches.then { |branches|
          branches.must_equal "LOL!!!!"
          was_run_flag = true
        }
      }
      was_run_flag.must_equal true
    end
  end
end
