module Sysadmin
  class Context
    attr_reader :shell, :git

    def initialize(opts={})
      @shell = opts.fetch(:shell, IO::ShellContext )
      @git   = opts.fetch(:git, IO::Git)
    end

    def run(&block)
      runner = Run.new
      needs_context = runner.instance_eval &block
      needs_context.call(self)
      nil
    end

    class Run
      def cmd(cmd_str)
        Shell.new.cmd(cmd_str)
      end
    end
  end

  class Shell
    def initialize(this=nil, prev=nil)
      @this = this
      @prev = prev
    end

    def cmd(string)
      Shell.new(
        SingleCommand.new(string), self)
    end

    def then(&fn)
      Shell.new(
        ->(prev, context) {
          fn.call(prev)
        },
        self)
    end

    def call(context)
      prev = @prev && @prev.call(context)
      @this && @this.call(prev, context)
    end

    class SingleCommand
      def initialize(cmd)
        @cmd = cmd
      end

      def call(value, context)
        context.shell.new.shell_command @cmd
      end
    end
  end

  module IO
    class ShellContext
      def shell_command cmd
        `#{cmd}`
      end
    end

    class Git
      def initialize(ctx)
        @ctx = ctx
      end

      def branches
        @ctx.cmd("git branch --list").then { |raw|
          raw.split("\n").map(&:strip)
        }
      end
    end
  end
end
