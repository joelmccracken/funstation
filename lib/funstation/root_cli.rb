module Funstation
  class RootCLI
    def call(context, args)
      case args[0]
      when "help" then help
      when "dirty" then dirty
      when "config" then config
      when "shell"    then shell
      end
      puts args
    end

    def help
      puts "options: help, dirty, config"
    end

    def config
      puts context.config.inspect
    end

    def shell
      require 'pry'
      binding.pry
    end

    def context
      @context = Context.new
    end
  end
end
