require 'logger'

module Funstation
  class Daemon
    def setup(context)
    end

    def run(context, args)
      @daemonize = true if args.include?("--daemonize")

      if daemon?
        Process.daemon
      end

      loop do
        info "oswald that ends wald"
        sleep 1
      end
    end

    private

    def info(str)
      logger.info(str)
      puts str unless daemon?
      log_file.flush
    end

    def logger
      @logger ||= Logger.new(log_file)
    end

    def log_file
      @log_file ||= File.open(log_file_path, "a")
    end

    def log_file_path
      File.expand_path("~/.funstation.d/daemon.log")
    end

    def daemon?
      @daemonize
    end
  end
end
