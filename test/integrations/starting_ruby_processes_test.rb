require 'minitest/spec'

require 'open3'
require 'pry'

describe "opening a shell" do
  describe "via open3" do
    it "allows for interaction with " do
      i,o,e,p = Open3.popen3("/bin/bash")

      shell_interactor = Class.new do
        attr_reader :i,:o,:e,:p
        def initialize
          @i, @o, @e, @p = Open3.popen3("/bin/bash")
        end

        def puts(cmd)
          i.puts cmd
        end

        def read
          result = ""
          loop do
            begin
              result << o.read_nonblock(1024)
            rescue IO::EAGAINWaitReadable
              return result
            end
          end
        end
      end
    end
  end
end
