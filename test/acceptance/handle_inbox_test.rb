require 'minitest/spec'

describe "handle-inbox" do
  before do
    `cd ~; rm -rf tmp_inbox; mkdir tmp_inbox; cd tmp_inbox; touch a_file_to_handle`
  end

  it "lists all the files in an inbox directory" do
    next
    output = Funstation::HandleInbox.new.start()
    output.must_equal "
1 item(s):
~/tmp_inbox/a_file_to_handle
"
  end

  it "lists a second file in an inbox directory" do
    next
    `cd ~; cd tmp_inbox; touch a_file_to_handle2`
    output = Funstation::HandleInbox.new.start()
    output.must_equal "
2 item(s):
~/tmp_inbox/a_file_to_handle
~/tmp_inbox/a_file_to_handle2
"
  end

end

