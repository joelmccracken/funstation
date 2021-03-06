module Funstation
  class Context
    attr_reader :registered_modules

    def initialize(registered_modules)
      @registered_modules = registered_modules
    end

    def config
      @config ||=
        begin
          init_path = File.expand_path("~/.funstation.d/init.rb")
          content = File.read(init_path)
          config = Config.new
          config.instance_eval(content, init_path, 1)
          config
        end
    end

    def data
      @data ||= Data.load
    end

    def each_module
      config.modules.each do |mod|
        the_mod = registered_modules[mod]

        if the_mod
          yield the_mod
        else
          $stderr.puts "Unknown module listed in configuration file #{mod}"
        end
      end
    end
  end

  class Data

    def self.data_file_location
      File.expand_path "~/.funstation.d/data.yml"
    end

    def self.load
      require 'yaml'
      yaml_file = data_file_location

      data =
        if File.exists? yaml_file
          YAML.load_file(yaml_file)
        else
          {}
        end
      new(data)
    end

    def initialize(data)
      require 'yaml'
      @data = data
    end

    def save
      YAML.dump @data, File.open(self.class.data_file_location, "w")
    end
  end

  class Config
    attr_accessor :config

    def [](x)
      config[x]
    end

    def modules
      config[:modules]
    end
  end
end
