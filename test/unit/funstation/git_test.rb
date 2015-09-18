require 'minitest/autorun'
require 'funstation'

describe "git" do
  let(:sys) {
    Sysadmin::Context.new(
      shell_src: shell_interface
    )
   }

  let(:shell_interface) {
    class FakeSI < Sysadmin::Monad
      def run
      end
    end
    FakeSI
  }

  describe "in faked-out repo" do
    it "works" do
      sys.run {
        git.branches.then { |branches|
          branches.must_equal "LOL!!!!"
        }
      }
      # branches = git_cmd("branch --list").split("\n").map(&:strip)
    end
  end

end
