require_relative "./dirt_alert"

module Funstation
  class GitHomeDir
    def setup(context)

      target_dir = File.expand_path("~")
      target_dir_git_dir = File.join(target_dir, ".git")
      if File.exists?(target_dir_git_dir)
        puts "INFO: It appears your home directory is already set up as a git repository"
        puts "Skipping git_home_dir setup. If you want it to be set up, remove the"
        puts "  directory at #{target_dir_git_dir} and run this again."
      else
        repo = context.config[:git_home_dir_repo]
        puts "Setting up home directory as a git directory..."
        GitGateway.new.polite_git_checkout(target_dir: target_dir, repo: repo)
        puts "Done setting up home directory."
      end
    end

    def status(context)
      DirtAlert.new.go
    end
  end

  self.register_module(:git_home_dir, GitHomeDir)
end
