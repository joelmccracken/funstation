module Funstation
  class Context
    def config
      require 'yaml'
      require 'pry'
      binding.pry
      init_path = File.expand_path "~/.funstation.d/init.rb"
      content = File.read(init_path)
      config = Config.new
      config.instance_eval(content, init_path, 1)
      config
    end

    class Config
      attr_accessor :config
    end
  end
end
