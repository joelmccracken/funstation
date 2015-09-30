require_relative "sysadmin/io/shell"

module Sysadmin
  class Context
    attr_reader :shell, :git

    def initialize(opts={})
      @shell = opts.fetch(:shell, IO::Shell )
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

      def cd(path)
        Shell.new.cd(path)
      end

      def git
        IO::Git.new(self)
      end
    end
  end

  class Shell
    def initialize(this=nil, prev=nil)
      @this = this
      @prev = prev
    end

    def cd(path)
      Shell.new(
        CDCommand.new(path), self)
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

    SingleCommand = Struct.new(:cmd) do
      def call(value, context)
        context.shell.new.shell_command cmd
      end
    end

    CDCommand = Struct.new(:path) do

    end
  end

  module IO
    class Git
      def initialize ctx
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
