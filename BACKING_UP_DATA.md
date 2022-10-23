## Backing Up Your Data

By default, your private comments are stored in the `PRIVATE_COMMENTS_DIR`. This defaults to `~/.config/private_comments`. They are _not_ uploaded anywhere else.

As with any other important data, if you don't want to risk loosing it, you should back it up.

### Easy Mode
The simplest way is to set `PRIVATE_COMMENTS_DIR` to point to a folder that can be automatically synced to the cloud, or a backup drive. For example, within a folder managed by Dropbox, Google Drive, etc. If your hard drive is automatically backed up, just make sure that the `PRIVATE_COMMENTS_DIR` is being included in your backups.

### Backing Up With Git

A slightly more complicated option is to leverage `post-commit` hooks. Every git repo you leave comments on, has a corresponding repo under the `PRIVATE_COMMENTS_DIR`. In each of those you could add a `post-commit` hook that pushed it to a remote repo. For example, a private repo on GitHub. 

This is slightly hampered by the fact that the repo names are typically cryptographic hashes. This is to guarantee that Private Comments doesn't leak metadata about your project if someone _does_ see a comments repo they shouldn't. However, the folder names come from plugins, and the default is to just take the name of the root folder of your project and run it through a SHA 256 hash. This is what the vim and emacs plugins both do.

Since you know what your project folders are named, you can simply ask sha256 to give you the has of the repo name you're looking for. For example, if you have a "fooberry" repo, you could ask sha256 for its hash with `echo 'my_repo_folder_name' | sha256sum` (on the command line), then find the matching folder name.

If you're using a remote repo to sync comments between multiple devices you'll want to set up a cron job to sync fresh comments down. Conflicts are _incredibly_ improbable if it's just you, but if your private comments repo is shared with others it could happen. I'd recommend a script that pulls down merges, and deals with conflicts by simply adding the conflicted file. This is just text, not code. You can let the next human who sees it clean up the conflict.


### Warning
Regardless of what you do, be sure to not upload comments about work projects to a location that your bosses would consider violating your NDA. For example, if your company doesn't allow code to be push to the public GitHub servers, then don't push your comments there either. If they don't allow Dropbox on work computers, then that's not an option either.

While your comments my be innocent and non-offensive, it's almost guaranteed that they'll eventually contain proprietary information about your employers codebase.

