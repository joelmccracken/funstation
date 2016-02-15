require "funstation/version"

module Funstation
  def self.register_module(name, mod)
    @registered_modules ||= {}
    @registered_modules[name] = mod
  end
  def self.registered_modules
    @registered_modules
  end
end

require 'funstation/context'
require 'funstation/root_cli'
require 'funstation/dirt_alert'
require 'funstation/handle_inbox'
require 'funstation/git_home_dir'
require 'funstation/daemon'
require 'funstation/git_gateway'
