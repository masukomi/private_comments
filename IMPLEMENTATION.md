## Private Comments Implementation Overview

What follows are thoughts on potential implementation mechanisms and data flow.

Much of this assumes an executable tool that a text editor's plugin would speak to. Creating this tool would simplify life for the plugin creator. However, there is no reason the plugin could not implement all the functionality of the tool itself.


The plugin is expected to run when:

* a file is loaded
  * to get the latest comments
* the file has been updated via git (pull, merge, whatever)
  * to get the latest comments
* the user has created / edited a comment
  * to store the comment


## Commands that can be sent to the tool

* request comments for file
	* GET `/v1/comments?project_name_hash=<hash_here>&file_path_hash=<hash_here>&treeishes=<a>,<b>,<c>,<d>`
* store new/edited comment

You can see [a flow chart of the data flow here](https://raw.githubusercontent.com/masukomi/private_comments/master/data_flow.png)

### Creating and Editing comments

When creating and editing a comment the plugin will send the tool 3 pieces of info

* the hash of the project name
* the file_path_hash (see below)
* the line number being commented on
* the treeish of the commit the comment should be associated with
* the name of the author (optional)
* the email of the author (optional)

There can only be one comment for each project + line number + file hash + treeish tuple. If a comment is sent for creation / editing with the same tuple it will overwrite the old one. Everything will be stored in a git repository so old comments will be retrievable.

Plugin creators _may_ choose to send `author_name` and `author_email` fields. If present these will be provided when comments are requested. If not the returned comments will contain null values. 

**Note:** Plugin creators should feel free to send _additional_ data in the hash. Private Comments will not strip this data out and it will return it when you request comments for a file. HOWEVER you _must_ limit this to data needed to support plugin functionality and _not_ anything that could potentially violate a user's NDA or otherwise get them in trouble if it became public. 

####  JSON Comment (for creation & editing)

```json

{
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87",
  "treeish": "31b8bc225906580b0e2ab78f8144f18ef4769568",
  "line number": 4,
  "comment": "comment text here"
}
```

**Note:** If `project_name_hash` has not been seen before a _new_ project will be automatically created. Because of this it is imperitive that plugins have a reliable and consistent way of persisting or regenerating. 

**Aside:** With regards to the storing and displaying of comments. There are two basic approaches.

1. tie it to the right place in the file with explicit line numbers
2. tie it to the right place in a file with a diff hunk like this:

```diff
"@@ -16,33 +16,40 @@ public class Connection : IConnection..."
```

The second approach allows for better handling of moved lines of code _but_ it leaks information about the code and thus could violate someone's NDA or expose sensitive data to black-hat hackers. 


### Requesting Comments for a file

The editor plugin will utilize git blame to figure out what treeishes were used to create this file and passes a list of them to the tool.


#### JSON Request for comments

```json

{
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87",
  "treeishes": [<treeish 1>, <treeish 2>, <treeish 3>] 
}
```

While we could pass tuples of `<treeish>` + `<line number>` it would result in an excessive amount of data for large files and _most_ of it would correspond to no comments.

#### JSON Response with comments

```json
{
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87"
  "comments": [
                {"line": 4, 
                 "treeish": "31b8bc225906580b0e2ab78f8144f18ef4769568",
                 "comment": "investigate how this works",
                 "author_name": null,
                 "author_email": null},
                {"line": 22,
                 "treeish": "f9caae240f2ab4543e2c814f107447163144058c",
                 "comment": "todo: refactor into multiple methods",
                 "author_name": "Marisa Tomei",
                 "author_email": "mtomei@example.com"}
             ]
}
```

Note: Because Private Comments doesn't know what lines were associated with each treeish, the response may contain comments that were left on lines that are associated with a commit that is still part of the file, but were left on lines that are no longer part of the current file. 

## Securty Notes

One goal of this project is to allow users to store their comments privately on cloud services without worry of violating their NDAs. Obviously the _contents_ of your comments might violate your NDA but usage of the Private Comments system itself _must_ be guaranteed to not leak secrets.

Because of this there are no project names, file names, or file paths in this system. All of these have the potential to reveal secrets about the code being worked on. Instead we use a SHA 256 hash of the project name, and relative file path (from the root of the project) to uniquely identify which file a comment is associated with. So, whenever you see `file_path_hash` or `project_name_hash` that's what it is. Plugin creators are encouraged to salt the hash.

It is up to the plugin creator to decide how (or if) to display the project name to the user. There are 2 things to consider when implementing handling of project names:

1. They must be stable even after the project folder has been renamed
2. When a user starts working on a different computer they must have a way to easily tell Private Comments what the name of the current project is and have it result in the same project hash that was created on the first computer.



## Private Comments Internal Storage

The Private Comments tool will manage a git repository for each project projects the user is working on. Each project has its own folder. Each comment is stored in a separate JSON file. Each file contains the unaltered data passed to it by the plugin. This allows for plugin creators to store additional data with each comment. All data in the JSON in each file will be returned when requesting comments for a file _except_ the project and file hashes. (See example JSON above)

The name of each JSON file is a combination of multiple SHA-256 hashes: 

* treeish
* file_path_hash
* line-number

The treeish is used to create a directory that holds all the `file_path_hash` + `line_number` tuples. E.g.

    <treeish>/<file_path_hash>-<line_number>.json

SHA 256 hashes are 65 characters long, so we should not run afoul of the maximum number of characters in file names (256). 

Here's an example filesystem from a Private Comments installation with two projects that have two comments each.

NOTE: This file naming convention needs to be changed. The current version forces the tool to "grep" the whole project to find comments that match the requested treeishes. 

```text
~/.config/private_comments/
  |- 7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755
      |- .git
      |- ce7ce8b769b6a73f10715e0f5330149e288e23c89b2c30a00ee9cd53591ae56f
        |-9978056c779cd90da8ff74300ed1d8d611a2ae767170d4faf7c0eb705c1e8dfe-123.json
      |- 9978056c779cd90da8ff74300ed1d8d611a2ae767170d4faf7c0eb705c1e8dfe
        |-b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c-22.json
  |- d7b6f7659d6cd366fd78126bd99bbe660acf81e46c23a8846e3d70803a72bf4b
      |- .git
      |- b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
        |-7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730-44.json
      |- 7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730
        |-ce7ce8b769b6a73f10715e0f5330149e288e23c89b2c30a00ee9cd53591ae56f-56.json
```

