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
Private Comments is intended to be interacted with via editor plugins. For information about the APIs and creating a plugin for you favorite editor please [check out the API docs](https://masukomi.github.io/private_comments)

To launch it just run `private_comments` Next, access the endpoints as described in [the API docs](https://masukomi.github.io/private_comments).

**Note:** You'll have to add the executable to your [PATH](https://www.techrepublic.com/article/how-to-add-directories-to-your-path-in-linux/) if you didn't install it with homebrew.

## Building From Source

Once you've cloned this repo you'll need to install [Chicken Scheme](https://www.call-cc.org/) and then run the `install_chicken_eggs.sh` script in the `src` directory. 

If you're hacking on it I recommend running it with `csi`

```
$ csi private_comments.scm
```

When your ready to do a final compile just run the following to generate a `private_comments` executable.

```
$ csc -static private_comments.scm
```

## Contributing

Pull requests are happily welcomed. Fork the repo. Make it better. Submit your changes. Note that all changes must be backwards compatible. We can't break existing plugins. If you're interested in a making a change that would break backwards compatibility please open a ticket to discuss it first. We'll create a `v2` version of the API if it's worth it.

If you're new to Scheme, or Chicken Scheme, don't worry. Just do your best and submit what you come up with. It's all good.
