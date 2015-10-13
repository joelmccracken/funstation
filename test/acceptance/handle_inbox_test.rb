require 'minitest/spec'

describe "handle-inbox" do
  before do
    @temp_inbox = `mktemp -d -t temp-inbox`.strip

    `cd #{@temp_inbox}; touch a_file_to_handle`

    @ctx = {
      handle_inbox: {
        directories: [
          @temp_inbox
        ]
      }
    }
  end


  after do
    `rm -rf #{@temp_inbox}`
  end

  it "lists all the files in an inbox directory" do
    output = Funstation::HandleInbox.new.call(@ctx)
    output.must_equal "1 item(s):
#{@temp_inbox}/a_file_to_handle"
  end

  it "lists a second file in an inbox directory" do
    `cd #{@temp_inbox}; touch a_file_to_handle2`
    output = Funstation::HandleInbox.new.call(@ctx)
    output.must_equal "2 item(s):
#{@temp_inbox}/a_file_to_handle
#{@temp_inbox}/a_file_to_handle2"
  end

end

