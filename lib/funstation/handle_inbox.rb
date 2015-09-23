module Funstation
  class HandleInbox
    def start(passed_ctx=nil)
      @ctx = passed_ctx

      ctx.run {
        cd("~/tmp_inbox").
          ls.then { |files|
            x = parse_files_into_output_structure(files)
            io.puts(x)
          }
      }
    end

    private
    def ctx
      @ctx ||=
        begin
          Sysadmin::Context.new
        end
    end
  end
end
