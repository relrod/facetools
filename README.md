# facetools

These are a collection of BSD-2 licensed tools used for parsing, manipulating
data found in Facebook's "Download Your Information" bundle.

A breakdown of each tool follows.

## facetools-parse

_Usage_: `facetools-parse /path/to/messages.htm /output/directory`

This tool uses the `tagsoup` library to parse the HTML file `messages.htm` which
contains a log of all of your Facebook messages. It then converts each thread
into a JSON file which can be processed by other tools.

Each JSON file contains exactly one list which contains a dictionary for each
message as follows:

```yaml
mUser: The sender's full name
mTimestamp: The timestamp of the message (as it appears in the HTML file)
mMessage: The full contents of the message
```

Note that `mTimestamp` can be parsed using `Data.Time.Format` and the following
formatting string: `"%A, %B %e, %0Y at %l:%M%P %Z"`

`facetools-parse` will also produce a file `/output/directory/index` which
contains a mapping between the JSON file number and the names of the
participants of the conversation.

## facetools-freqchart

_Usage_: `facetools-freqchart /path/to/a/conversation/file.json`

Given a JSON file produced by `facetools-parse`, `facetools-freqchart` will
produce a simple HTML file which embeds a Google "Calendar Chart", useful for
seeing how many messages took place over how many days in a given conversation.
