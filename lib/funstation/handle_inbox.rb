module Funstation
  class HandleInbox
    def call(context)
      @ctx = context

      issues = []

      @ctx[:handle_inbox][:directories].each do |dir|
        issues += Dir["#{File.expand_path dir}/*"]
      end

      display issues
    end

    def display issues
      (["#{issues.count} item(s):"] + issues).join "\n"
    end

    private
    def ctx
    end
  end
end
