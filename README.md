## ![private comments logo](site/static/images/logo.png) Private Comments

Private comments allows you to leave “private comments” on specific lines of a codebase that are not stored in the codebase.

Imagine being dropped into a new codebase and having the freedom to leave whatever todo items and breadcrumbs you want without cluttering the codebase. Imagine working on a client’s codebase and not having to worry about what you say, or who sees it.

Editor Plugins speak to a tiny Private Comments REST server running in your computer. For information about the APIs and creating a plugin for you favorite editor please [check out the API docs](https://masukomi.github.io/private_comments)

## Installation

### MacOS via Homebrew
Execute the following lines in your terminal.

```sh
brew tap masukomi/homebrew-apps
brew install private_comments
```

### Other
For other operating systems you'll need to build it from source (see below).

## Usage

Private comments is a simple web server that runs locally and provides an API for editor plugins to interact with.

I'd recommend you launch it by running `private_comments && disown` This will boot the server in the background and allow you to close the window and not worry about it. If you need to stop the server you can run `pkill private_comments`

Once you've got the server running, access the endpoints as described in [the API docs](https://masukomi.github.io/private_comments) or use an editor plugin or the `pc` command line client.

If you've forgotten if the server is running or not you can check by running `pgrep private_comments` That will return the id of the process if it's running or nothing if it isn't.

### Editor Plugins

There is [a Vim Plugin](https://github.com/masukomi/vim_private_comments/) which leverages the `pc` client instead of talking directly to the APIs. This is the easiest way to bootstrap a plugin for your favorite editor. 

See below for more details on creating a plugin for your favorite editor.

**Note:** You'll have to add the executable to your [PATH](https://www.techrepublic.com/article/how-to-add-directories-to-your-path-in-linux/) if you didn't install it with homebrew.

Once the `private_comments` server is running locally you can follow the instructions for the `pc` command line client (see below), or better yet, a plugin for your favorite editor. 


### Specifying a Port
By default private comments runs on port `5749`. To change this, set the `PRIVATE_COMMENTS_PORT` environment variable to the desired port.

For example: In bash you could change it to 3111 by executing it like this.

```
PRIVATE_COMMENTS_PORT=3111 private_comments
```

Alternately you could define it in your `~/.bashrc` with this line

```
export PRIVATE_COMMENTS_PORT=3111
```

### Specifying a Directory
By default `private_comments` will store its data in `~/.config/private_comments`

To specify another directory just set the `PRIVATE_COMMENTS_DIR` environment variable to a valid path.



## Creating An Editor Plugin
There's not a lot to creating an editor plugin, and the Example Client (see below) can be used to rapidly bootstrap the process. Under the covers it implements all the steps in the diagram below. The [Vim plugin](https://github.com/masukomi/vim_private_comments/) is an example of quickly bootstrapping a plugin. 

Please contact [me](https://masukomi.org) if you'd like help creating a plugin for your favorite editor. 

This diagram provides a high level overview of how data flows through the system. If you choose to interact with the API directly (recommended) you can find all the details in [the API docs](https://masukomi.github.io/private_comments).

![diagram of high level data flow](docs/instructional/data_flow.svg)

I recommend using the Example Client (`pc`) to quickly bootstrap a plugin for your editor. Once you've got something that works, and looks good, you can go back and use tools like [libgit2](https://libgit2.org/) to query git without shelling out to the command line (as `pc` does), and make direct HTTP requests to the local `private_comments` server. The source to `pc` can be a reasonable blueprint for how to go about that.


## Example Client
As a tool for testing the server, and bootstrapping plugin development Private Comments comes with an example command line client called `pc`

Following the usage guide below you can record, retrieve, and delete comments. Note that Private Comments can only record comments about lines that have been _committed_ in git. So, if it's a new line, or you've changed the line, you'll need to commit it before commenting.


Note: pc is _not_ intended to be the tool you use to record comments.  It's a testing/proof-of-concept client implementation. It also makes for a _very_ fast way to implement an editor plugin. You just have to manage the capture of new comments and display of existing ones. You can pass the complications of interacting with git and an HTTP API off to the `pc` script.

```
Usage: pc -f file-path [-fclsp] [option values]
     -f --file=<path>                   Relative path from root of git repo. Ex. "src/pc.scm"
     -c --comment=<Comment>             A comment to be stored
     -l --line=<Line Number>            The line number of the comment to be stored
     -s --server=<Server URL>           Private Comments Server Url [default: http://0.0.0.0]
     -p --port=<Server Port>            Private Comments Server Port [default: 5749]
     -h --help                          Display this text
```

To retrieve comments for a given file you'd say something like this:

```
pc -f my/awesome/ruby/file.rb
```

Assuming the Private Comments server was running it'd return a human readable list of comments preceeded by their line number. For example:

```
12: Ask Mary about this.
22: see docs for how this call works
106: OMGWTFBBQ!!!
```

If there aren't any comments it'll just return nothing.


To record a comment on a particular line of code you could say:

```
pc -f my/awesome/ruby/file.rb -l 44 -c "TODO refactor me" 
```

**NOTE: the current implementation only supports commenting on lines of code that have been previously committed to git.**

It will exit with an error code if you're commenting on an uncommitted line. 


You could use this as a quick starting point for an editor plugin, or tweak the code to make it generate different output. In the long run [using the API ](https://masukomi.github.io/private_comments) is going to be a better solution. 




## Building From Source

Once you've cloned this repo you'll need to install [Chicken Scheme](https://www.call-cc.org/) and then run the `install_chicken_eggs.sh` script in the `src` directory. 

If you're hacking on it I recommend running it with `csi`

```
$ csi private_comments.scm
```

When your ready to do a final compile just use the `build.sh` script  a `private_comments` and `pc` executable.


The tests are written in Bash and use the [bash_unit](https://github.com/pgrange/bash_unit#readme) framework. Once you've installed it, added it to your PATH, and compiled your changes with `build.sh` you can run the tests like this:

```
# run from within the src directory

$ bash_unit tests/test_server
$ bash_unit tests/test_client
```

If it's running `private_comments` will be shut down, and a new instance will be run. The new instance will store its test data separately so you don't have to worry about messing up, or loosing, any existing comments you may have created with private comments.

## Contributing

Private Comments is written in [Chicken Scheme](https://www.call-cc.org/). Pull requests with new features, or improved code, are happily welcomed. Fork the repo. Make it better. Submit your changes. Note that all changes must be backwards compatible. We can't break existing plugins. If you're interested in a making a change that would break backwards compatibility please open a ticket to discuss it first. We'll create a `v2` version of the API if it's worth it.

If you're new to Scheme, or Chicken Scheme, don't worry. Just do your best and submit what you come up with. It's all good. I'm not an expert Schemer either.

Please add a test to `src/tests/test_server` (for server changes) or `src/tests/test_client` (for client changes) to confirm your changes are good, and that existing features haven't been broken.

