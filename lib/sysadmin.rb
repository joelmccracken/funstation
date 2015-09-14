module Sysadmin
  class Context
    def run(&block)
      monad = self.instance_eval &block
    end

    def cmd(str, &block)
      IO::Command.new(str, &block)
    end
  end

  class ShellWrap
    def initialize(prev=nil, &block)
      @prev = prev
      @block = block
    end

    def then(&block)
      ShellWrap.new.new(self, &block)
    end

    def reify
      if @prev
        @block.call @prev.reify
      else
        @block.call
      end
    end
  end

  module IO
    class Command < ShellWrap
      def initialize(cmd)
        @cmd = cmd
      end

      def reify
        `#{@cmd}`
      end
    end
  end
end
