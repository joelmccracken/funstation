module Funstation
  class PoliteGitCheckout
    def checkout(target_dir:, repo:)
      `
      cd #{target_dir}
      git init  > /dev/null 2>&1
      git remote add origin #{repo}  > /dev/null 2>&1
      git fetch > /dev/null 2>&1
      git checkout -t origin/master  > /dev/null 2>&1
      git branch -d origin/master  > /dev/null 2>&1
      git reset --mixed origin/master > /dev/null 2>&1
      git branch --set-upstream-to origin/master > /dev/null 2>&1
      `
    end
  end
end
