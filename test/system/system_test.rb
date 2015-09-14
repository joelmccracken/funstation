require 'minitest/spec'

describe "system test" do
  it "firefox exists in the bin" do
    `PATH="$PATH:/Users/joel/Applications/Firefox.app/Contents/MacOS/" which firefox`
    $?.to_i.must_equal 0
  end
end
