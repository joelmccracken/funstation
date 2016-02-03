# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'funstation/version'

Gem::Specification.new do |spec|
  spec.name          = "funstation"
  spec.version       = Funstation::VERSION
  spec.authors       = ["Joel McCracken"]
  spec.email         = ["mccracken.joel@gmail.com"]

  spec.summary       = %q{A workstation management system.}
  spec.description   = %q{A workstation management system. Because if its fun, its not work.}
  spec.homepage      = "https://www.github.com/joelmccracken/funstation"

  spec.files         = `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.11"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "minitest", "~> 5.0"
end
