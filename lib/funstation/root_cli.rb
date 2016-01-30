module Funstation
  class RootCLI
    def command_options
      {
        :help => "display help for this command",
        :dirty => "check for anything that needs to be cared for",
        :config => "display current configuration",
        :shell => "open a pry session in the process",
        :setup => "run initial set up scripts for installed modules",
        :daemon => "run the funstation daemon. Used for modules that do some work in the background.",
        :data => "foo"
      }
    end

    def call(args)
      option = (args[0] || "help").to_sym
      command_options.fetch(option) { raise "#{option} is not a command" }
      send(option, args[1..-1])
    end

    def help(args)
      puts "options:"
      command_options.each do |name, description|
        puts format("  %10s:    #{description}", name)
      end
    end

    def data(args)
      pp Data.load
    end


    def dirty(args)
      puts "not implemented yet"
    end

    def config(args)
      require 'pp'
      pp context.config
    end

    def shell(args)
      require 'pry'
      binding.pry
    end

    def setup(args)
      context.each_module do |mod|
        mod.new.setup(context)
      end
    end

    def status(args)
      context.each_module do |mod|
        mod.new.status(context)
      end
    end

    def daemon(args)
      Daemon.new.run(context, args)
    end

    private
    def context
      @context ||= Context.new(Funstation.registered_modules)
    end
  end
end
