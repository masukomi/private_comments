*Note:* The lack of activity here doesn't mean this repo is abandoned. It's just _very stable code_ that doesn't need poking. I rely on it daily. ;)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Special Design Features](#special-design-features)
- [Installation](#installation)
- [MacOS via Homebrew](#macos-via-homebrew)
- [Other](#other)
- [Usage](#usage)
- [Editor Plugins](#editor-plugins)
- [Specifying a Port](#specifying-a-port)
- [Specifying a Directory](#specifying-a-directory)
- [Additional Information](#additional-information)
- [Creating An Editor Plugin](#creating-an-editor-plugin)
- [Example Client](#example-client)
- [Building From Source](#building-from-source)
- [Troubleshooting](#troubleshooting)
- [Pre-Commits Warning](#pre-commits-warning)
- [Contributing](#contributing)

<!-- markdown-toc end -->



![private comments logo](site/static/images/logo.png) Private comments allows you to leave “private comments” on specific lines of a codebase that are not stored in the codebase.

Imagine being dropped into a new codebase and having the freedom to leave whatever todo items and breadcrumbs you want without cluttering the codebase. Imagine working on a client’s codebase and not having to worry about what you say, or who sees it.

Imagine leaving yourself a todo comment on a line, and having it disappear when you commit a change to that line. No more obsolete comments!

![preview of emacs plugin](https://masukomi.github.io/private_comments/images/emacs_demo.gif)

Editor Plugins speak to a tiny Private Comments REST server running in your computer. The server uses a little under 7MB of RAM and essentially no CPU. The animated GIF on this page is taking up more resources. 

For information about the APIs and creating a plugin for you favorite editor please read "Creating An Editor Plugin" below.

Note: _Private Comments currently assumes you're using git for your version control system._ It's possible to use with editor plogins that know about other version control systems, but instructions are currently not provided for writing those.

### Special Design Features

* Comments are tied to a specific commit. Each comment is associated with the commit that produced the line you're commenting on. When that line is no longer in the file, the comment will disappear. However, if you roll back your git repository to a prior commit, it will _reappear_ so you can see any comments that were relevant at the time.
* Comments created with Private Comments are stored in a sharable git repository. A group of freelancers working on a client codebase can leave inline comments for each other as they learn the codebase without altering the source code itself. 
* Project names and file names are _never_ stored. This way you can back up your private comments repo to a third party server without violating your NDA. If the third party server is compromised the hackers won't be able to derive anything from a leaked filename or project name. If you're not encrypting your comments then you just need to be careful to not include any secrets in your comments.
* Comments can be encrypted. Encryption is _optionally_ handled on the client side. The Private Comments server only cares that you're passing it a string. It doesn't care what that string is. Encryption / Decryption is fairly easy to add to any private comments browser plugin. 



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

The [Emacs plugin](https://github.com/masukomi/private-comments-mode#readme) does not use the `pc` client and speaks directly to the server. This is the preferred way of doing things.

See below for more details on creating a plugin for your favorite editor.

If you don't use Vim or Emacs you can still view private comments with the `pc` client but the experience is sub-optimal. The `pc` client is really designed for testing and bootstrapping if making http calls from your favorite editor is difficult.
 
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

### Additional Information
- [Backing Up Your Private Comments](https://github.com/masukomi/private_comments/blob/master/BACKING_UP_DATA.md#readme)
- [Competitors](https://github.com/masukomi/private_comments/blob/master/COMPETITORS.md#readme)


## Creating An Editor Plugin
There's not a lot to creating an editor plugin, and the Example Client (see below) can be used to rapidly bootstrap the process. Under the covers it implements all the steps in the diagram below. The [Vim plugin](https://github.com/masukomi/vim_private_comments/) is an example of quickly bootstrapping a plugin. The [Emacs plugin](https://github.com/masukomi/private-comments-mode#readme) speaks directly to the local server, which makes it dramatically easier to inline the comments. 

Please contact [me](https://masukomi.org) if you'd like help creating a plugin for your favorite editor. 

This diagram provides a high level overview of how data flows through the system. If you choose to interact with the API directly (recommended) you can find all the details in [the API docs](https://masukomi.github.io/private_comments).

![diagram of high level data flow](docs/instructional/data_flow.png)

I recommend using the Example Client (`pc`) as a reference to confirm that your plugin is doing the right thing, and that the data you generate is readable by another client. The source to `pc` can be a reasonable blueprint for how to go about building a client.


## Example Client
As a tool for testing the server, and bootstrapping plugin development Private Comments comes with an example command line client called `pc`

Following the usage guide below you can record, retrieve, and delete comments. Note that Private Comments can only record comments about lines that have been _committed_ in git. So, if it's a new line, or you've changed the line, you'll need to commit it before commenting. Please reference the diagram above for how this should work.

Note: pc is _not_ intended to be the tool you use to record comments.  It's a testing/proof-of-concept client implementation. With that said, it certainly _can_ be done and the Vim plugin does exactly that.

```
❯ pc --help
Private Comments client v1.3
Usage: pc -f file-path [-fcdlsp] [option values]
     -f --file=<path>                   Relative path from root of git repo. Ex. "src/pc.scm"
     -c --comment=<Comment>             A comment to be stored
     -d --delete                        Will delete comment at specified location (line & file)
     -l --line=<Line Number>            The line number of the comment to be stored
     -p --port=<Server Port>            Private Comments Server Port [default: 5749]
     -s --server=<Server URL>           Private Comments Server Url [default: http://0.0.0.0]
     -x --debug                         display comments with debugging output
     -h --help                          Display this text

Report bugs at https://github.com/masukomi/private_comments/issues
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

There are two ways to build from source:

- [Manual approach](#manual-approach): this is the original way for building
  from source; and
- [Docker-based approach](#docker-based-approach): you can use this approach to
  build a Linux binary without needing any additional tools besides
  [Docker](https://docker.io).

### Manual approach

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

### Docker-based approach

From the project root directory, run:

```sh
./linux/make.release.sh
```

Once the process is completed, this will produce 64-bit Linux binaries in the
`bin/` directory and an archive with the binaries packaged.  When you run the
command for the first time, it will take a bit to build the docker container
used for the build. Subsequent runs will be much faster.

It is possible to build a numbered version by passing the version in the
environment variable `BUILD_VERSION` as follows:

```sh
env BUILD_VERSION=1.2.3 ./linux/make.release.sh
```

## Contributing

Private Comments is written in [Chicken Scheme](https://www.call-cc.org/). Pull requests with new features, or improved code, are happily welcomed. Fork the repo. Make it better. Submit your changes. Note that all changes must be backwards compatible. We can't break existing plugins. If you're interested in a making a change that would break backwards compatibility please open a ticket to discuss it first. We'll create a `v2` version of the API if it's worth it.

If you're new to Scheme, or Chicken Scheme, don't worry. Just do your best and submit what you come up with. It's all good. I'm not an expert Schemer either.

Please add a test to `src/tests/test_server` (for server changes) or `src/tests/test_client` (for client changes) to confirm your changes are good, and that existing features haven't been broken.
