---
section_weight: 0
page_weight: 1

---

# Getting Started

* [What is Private Comments](#what-is-private-comments)
* [How does it work?](#how-does-it-work)
* [Writing Editor Plugins](#writing-editor-plugins)
    * [Core Concepts](#core-concepts)
    * [Adding / Updating A Comment](#adding-updating-a-comment)
    * [Requesting Comments For a File](#requesting-comments-for-a-file)

## What Is Private Comments

Private comments allows you to leave "private comments" on specific lines of a codebase that are not stored _in_ the codebase. 

Imagine being dropped into a new codebase and having the freedom to leave whatever todo items and breadcrumbs you want without cluttering the codebase. Imagine working on a client's codebase and not having to worry about what you say, or who sees it. 

Even better, Private Comments is guaranteed to not leak any secrets about the codebase you're working on. We can't guarantee what you write won't, but the closest the system comes is reusing git treeishes from the repo you're commenting on.

Thanks to git, you can see the old comments from historical versions of your files, and outdated comments disappear when the lines they comment on change or go away. 

This site explains how it works, and the APIs you'll need to work with in order to create a plugin to support it in your favorite editor. For details on installing the the Private Comments application check out [the GitHub repository](https://github.com/masukomi/private_comments).

## How Does It Work?

Private Comments is a tiny little REST server. A plugin for your favorite editor sends it new and edited comments to save, and requsts a list of existing comments for files you're working on.

Comments are stored in a separate git repo for each project. If you want, you can share your comments repo with teammates.

Please see [the GitHub README](https://github.com/masukomi/private_comments/#readme) for instructions on how to install and run the Private Comments server.


## Writing Editor Plugins

Overall, writing a plugin that works with Private Comments is pretty simple, and you only need to support two endpoints which I'll cover in a moment. You'll likely spend the majority of your time figuring out how to display the comments and let users create or edit them. 

Check out [this diagram](images/data_flow.svg) for a high level overview of the process for viewing and creating comments.


How you choose to display, and edit comments is entirely up to you. Spend time focusing on usability and style. I'd also recommend that you make it easy to toggle the display of comments entirely. 

Once you've released your comment, add it to [the wiki](https://github.com/masukomi/private_comments/wiki).


### Core Concepts
In order to prevent Private Comments from leaking secrets most of the data you provide to PC will be SHA 256 hashes.

A comment requires 3 pieces of information to tie it to the right place in your codebase:

* A SHA 256 hash of the project name
* The commit [treeish](https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddeftree-ishatree-ishalsotreeish) this particular comment is associated with.
* A SHA 256 hash of the path to the file this comment was left on.
* The line number this comment is associated with.

#### Project Names
It doesn't matter if the project name you use for the hash is auto-generated or comes from user input. It is _critical_ that you have a mechanism that allows for users with multiple computers to use the _exact_ same project name on each device. Different names will result in different project name hashes, and that will prevent them from syncing comments between devices.

The simplest solution to this is to let users see the name you're using for the current project. If your editor supports the concept of named projects, that would be the obvious thing to use, but the must be able to override it in your plugin in case they've named the project differently on the other device(s).

Another simple solution is to store it with `git config` on a per repository basis. For example:

```shell
$ git config --add private-comments.project-name "my_project_name"
$ git config --get private-comments.project-name
my_project_name
```

### Adding / Updating A Comment

Prerequisite: The line of code being commented on must have been committed first before Private Comments can persist the comment. Comments on new lines must be managed by your plugin until the new line has been committed.

Prerequisite: You must have the ability to run [git blame](https://git-scm.com/docs/git-blame) on the file. You'll need this to know which lines of the file come from which git commits. More specifically, you'll need to know which treeish each line associated with.

Gather up the things listed in Core Concepts along with the comment, line number, and the treeish of the line being commented on. Then send a `POST` to the `/v1/comments` endpoint with JSON. 

**Note:** If `project_name_hash` has not been seen before a _new_ project will be automatically created, and a new git repository will be initialized. Because of this it is imperative that plugins have a reliable and consistent way of persisting or regenerating project name hashes.

### Requesting Comments for a file

Prerequisite: You must have the ability to run [git blame](https://git-scm.com/docs/git-blame) on the file. You'll need this to know which lines of the file come from which git commits. More specifically, you'll need to know which treeish each line associated with.

Private Comments retrieves comments for you on a per-file basis. Whenever users start displaying a new file, you can sent PC a `GET` request to `/v1/comments` with the required info. What you'll get back is a list of all the comments left on this file, for all the treeishes in the file.  

Use Git Blame to gather up all the treeishes that make up your file. Put together a unique list, 

**Note:** You _may_ also receive comments for lines that no-longer exist in the file. For example. Lets say your "cars.rb" file has had two commits. Lines 1-10 were added in the first commit. Your user left a comment on line 1 and a comment on line 7. Lines 5-10 were then replaced in a second commit. 

When your user opens the file `git blame` tells you that the file is comprised of commits from 2 treeishes. You gather those treeishes up along with the other core concepts and pass them off to PC. Because that first commit's treeish is in the list for this file PC will give you _both_ of the comments for that treeish. The response data will let you know what line and treeish each comment is for. Just throw out any comments where the line number is no longer associated with treeish that came back with the comment.


Summarized Process: 

* run git blame
* make an internal map of line number to treeish.
* generate the required SHA 256 hashes
* pass the hashes + a list of unique treeishes to Private Comments.
* iterate over the comments that returned
  * check if the line number of the comment maps to the same treeish as your internal map and display it if it does.


## Shutting down the server

Should you need it, you can send a request to `/shutdown` to shut down the server. However, it is _strongly_ recommended that you leave it running. Your user may be using multiple editors 
