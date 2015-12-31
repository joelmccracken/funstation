module Funstation
  class RootCLI
    def command_options
      {
        :help => "display help for this command",
        :dirty => "check for anything that needs to be cared for",
        :config => "display current configuration",
        :shell => "open a pry session in the process",
        :setup => "run initial set up scripts for installed modules"
      }
    end

    def call(args)
      option = (args[0] || "help").to_sym
      command_options.fetch(option) { raise "#{option} is not a command" }
      send(option)
    end

    def help
      puts "options:"
      command_options.each do |name, description|
        puts format("  %10s:    #{description}", name)
      end
    end

    def dirty
      puts "not implemented yet"
    end

    def config
      require 'pp'
      pp context.config
    end

    def shell
      require 'pry'
      binding.pry
    end

    def setup
      context.each_module do |mod|
        mod.new.setup(context)
      end
    end

    def context
      @context = Context.new(Funstation.registered_modules)
    end
  end
end
