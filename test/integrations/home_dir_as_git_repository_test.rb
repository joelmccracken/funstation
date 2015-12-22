require 'minitest/spec'

require 'pry'
require 'tmpdir'

describe "creating a home dir as a git repository" do
  before do
    @git_repo = Dir.mktmpdir(%w{repo- -.git})
    @home_dir = Dir.mktmpdir(%w{users- -home})

    @repo_bashrc_contents = "contents from git repo"

    # create a simple example repository, populate with some things that make this interesting
    `cd #{@git_repo}
     git init . > /dev/null
     echo #{@repo_bashrc_contents} > .bashrc
     git add .
     git commit -m 'first commit'`
  end


  it "adds files from repo if they dont exists in current directory" do
    puts clone_repo @home_dir, @git_repo

    Dir.chdir @home_dir do
      dir_contents = `ls -lah`

      dir_contents.must_match(/\.git/)
      dir_contents.must_match(/\.bashrc/)
    end
  end

  it "keeps files that already exist" do
    Dir.chdir @home_dir do
      home_bashrc_content = "from home dir"
      File.write(".bashrc", home_bashrc_content)
      clone_repo @home_dir, @git_repo
      diff = `git diff`
      diff.must_match(/\+#{home_bashrc_content}/)
      diff.must_match(/\-#{@repo_bashrc_contents}/)
    end
  end

  def clone_repo(home_dir, repo_dir)
    `
    cd #{home_dir}
    git init  > /dev/null 2>&1
    git remote add origin #{repo_dir}  > /dev/null 2>&1
    git fetch > /dev/null 2>&1
    git checkout -t origin/master  > /dev/null 2>&1
    git branch -d origin/master  > /dev/null 2>&1
    git reset --mixed origin/master > /dev/null 2>&1
    git branch --set-upstream-to origin/master > /dev/null 2>&1
    `
  end
end
