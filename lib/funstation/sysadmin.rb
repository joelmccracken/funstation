module Sysadmin
  class Context
    def initialize(opts={})
      @shell_monad = opts.fetch(:shell_src, IO::Command )
      @git_monad = opts.fetch(:git_src, IO::Git)
    end

    def run(&block)
      monad = self.instance_eval &block
      monad.run
      nil
    end

    def cmd(str, &block)
      @shell_monad.new(str, &block)
    end

    def git
      @git_monad.new(self)
    end
  end

  class Monad
    def initialize(prev=nil, &block)
      @prev = prev
      @block = block
    end

    def then(&block)
      self.class.new(self, &block)
    end

    def run
      if @prev
        @block.call @prev.run
      elsif @block
        @block.call
      end
    end
  end

  module IO
    class Command < Monad
      def initialize(cmd)
        @cmd = cmd
      end

      def run
        `#{@cmd}`
      end
    end

    class Git < Monad
      def initialize(ctx)
        @ctx = ctx
      end

      def branches(&block)
        @ctx.cmd("git branch --list").then { |raw|
          block.call(raw.split("\n").map(&:strip))
        }
      end
    end
  end
end
