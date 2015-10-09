require 'open3'

module Sysadmin
  module IO
    class Shell

      def shell_command cmd
        # process[:in].puts cmd
        # process[:in].close
        # process[:out].gets
      end

      def process
        # @process =
        #   begin
        #     i,o,e = Open3.popen3("/bin/bash")
        #     {
        #       :in => i,
        #       :out => o,
        #       :err => e
        #     }
        #   end
      end
    end
  end
end
