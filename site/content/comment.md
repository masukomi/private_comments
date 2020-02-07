---
section_weight: 1
page_weight: 1

---
# Comment

A comment object contains a collection of information about a specific comment

* The SHA 256 hash of its project's name
* The commit [treeish](https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddeftree-ishatree-ishalsotreeish) this particular comment is associated with.
* The SHA 256 hash of the path to the file this comment was left on.
* The line number this comment is associated with.
* The comment itself

In addition to this, the JSON you send along with you comment can include any other metadata your plugin needs to do its job in displaying this comment. It is _stronly_ recommended that you don't include file names, class names, or anything else from the code that could leak secrets that might violate someone's NDA.

The comment itself is just random text to Private Comments. It can be plain text, or encrypted. Silently encrypting and decrypting it would be an excellent layer of protection for your users.

**Note:** The file path hash must be the full path from the project root to the file plus the file name. Without the full path there may be hash collisions for two files with the same name, which will result in returning, or overwriting comments for the wrong file.

## Comment Parameters
|Parameter|Type|Required|Description|
|---------|----|--------|-----------|
| project_name_hash | String | true | A SHA 256 hash of the project name|
| file_path_hash | String | true | A SHA 256 hash of the path to the file from the root of the project.|
| treeish | String | true | The treeish that created the line under comment (from git blame)|
| line_number | Integer | true | An integer indicating the line number being commented on |
| comment | String | true | The comment left by the user |
| anything else | Any | false | Literally any other metadata your plugin wants to store about this comment |

## Comment Endpoints

* [Add / Update a comment](#add-update-a-comment-endpoint)
* [Delete a comment](#delete-a-comment-endpoint)
* [Retrieve comments for a file](#retrieve-comments-for-a-file-endpoint)
* [Server Status](#server-status)

## Add / Update a comment endpoint

```javascript
// add or update a comment
var data = JSON.stringify({
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87",
  "treeish": "777a58e942164e94964999bcc7cd20bbcda28fe55ac38be28e530f58ddad5ad8",
  "line_number": 4,
  "comment": "comment text here"
});

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === this.DONE) {
    console.log(this.responseText);
  }
});

xhr.open("POST", "http://localhost:5749/v1/comments");
xhr.setRequestHeader("content-type", "application/json");

xhr.send(data);
```

```shell
# add or update a comment
curl --request POST \
  --url http://localhost:5749/v1/comments \
  --header 'content-type: application/json' \
  --data '{
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87",
  "treeish": "777a58e942164e94964999bcc7cd20bbcda28fe55ac38be28e530f58ddad5ad8",
  "line_number": 4,
  "comment": "comment text here"
}'
```

> The code above will return JSON structured like this:

```json
{
  "status": "SUCCESS",
  "description": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87-4.json written"
}
```

This endpoint creates or updates a single comment object and takes the full list of parameters noted above.

#### HTTP Request

`POST /v1/comments`

## Delete a comment endpoint

This endpoint deletes a single comment and takes the comment parameters listed above, _minus_ the "comment". Note that the HTTP DELETE verb only officially supports query params. 

This endpoint expects the following query parameters:

| Parameter    | Type | Description |
|--------------|------|-------------|
| `project_hash_name` | String | (see above) |
| `file_path_hash` | String | (see above)|
| `treeish` | String | the single treeish relevant to the current version of line where the associated comment is to be deleted. |
| `line_number` | Integer | true | An integer indicating the line number being commented on |



```javascript
// delete a comment
var data = null;

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === this.DONE) {
    console.log(this.responseText);
  }
});

xhr.open("DELETE", "http://localhost:5749/v1/comments?project_name_hash=PROJECT_NAME_HASH&file_path_hash=FILE_PATH_HASH&treeish=SINGLE_TREEISH&line_number=LINE_NUMBER");

xhr.send(data);
```


```shell
# delete a comment
curl --request DELETE \
  --url 'http://localhost:5749/v1/comments?project_name_hash=PROJECT_NAME_HASH&file_path_hash=FILE_PATH_HASH&treeish=SINGLE_TREEISH&line_number=LINE_NUMBER'
```
> The code above will return JSON structured like this:

```json
{
  "status": "SUCCESS",
  "description": "comment removed"
}
```



#### HTTP Request

`DELETE /v1/comments?project_name_hash=<PROJECT_NAME_HASH>&file_path_hash=<FILE_PATH_HASH>&treeish=<SINGLE_TREEISH>&line_number=<LINE_NUMBER>`

## Retrieve comments for a file endpoint


This endpoint retrieves all the comments relevant to the treeishes in the current verison of the specified file. **Note that this may contain comments that are associated with a _treeish_ that is still in your file, but for a line that has since been replaced with another commit.**

For example: Your first commit adds all the lines of a file. All lines of this file are associated with the same treeish. Then you leave a private comment on the 1st line of the file. The top half of the file is then changed in a subsequent commit. The lines in the bottom half of the file would be associated with the first commit still. Becaues you pass all relevant treeishes, the server would return the comment from the first line, because it was tied with a treeish that is still in the file, BUT that comment would no longer be relevant to the version of the file the user was viewing. It's up to the consumer of the API to address that.

This endpoint expects the following query parameters:

| Parameter| Type | Description |
|----------|------|-------------|
| `project_hash_name` | String | (see above) |
| `file_path_hash` | String | (see above)|
| `treeishes` | String | a comma-separated list of treeishes relevant to the current version of the file (generated via [git blame](https://www.git-scm.com/docs/git-blame)). |



```javascript
// retrieve a comment
var data = null;

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === this.DONE) {
    console.log(this.responseText);
  }
});

xhr.open("GET", "http://localhost:5749/v1/comments?project_name_hash&file_path_hash=faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87&treeishes=777a58e942164e94964999bcc7cd20bbcda28fe55ac38be28e530f58ddad5ad8%2C263fe246fb49b562e501427470efed926dc11cb9f8909be5b1987d3f2adff712");

xhr.send(data);
```

```shell
# retrieve a comment
curl --request GET \
  --url 'http://localhost:5749/v1/comments?project_name_hash=7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755&file_path_hash=faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87&treeishes=777a58e942164e94964999bcc7cd20bbcda28fe55ac38be28e530f58ddad5ad8%2C263fe246fb49b562e501427470efed926dc11cb9f8909be5b1987d3f2adff712'
```

> The code above will return JSON structured like this:

```json
{
  "project_name_hash": "7135459ae30c0d5180b623986c420bf20856461cb6b9b860986a22c7654ed755",
  "file_path_hash": "faf3e6fc36b8524dad3aa4a317e734623f0dfcf6934a659015827406ebfb0c87",
  "comments": [
    {
      "treeish": "777a58e942164e94964999bcc7cd20bbcda28fe55ac38be28e530f58ddad5ad8",
      "line_number": 4,
      "comment": "comment text here"
    }
  ]
}
```

#### HTTP Request

`GET /v1/comments?project_hash_name={project_hash}&file_path_hash={file_path_hash}&treeishes={comma separated list of treeishes}`


## Server Status
Provides a means of performing a quick sanity-check that the server is actually up and running. Note that the status request is not currently versioned.

```javascript
// server status
var data = null;

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === this.DONE) {
    console.log(this.responseText);
  }
});

xhr.open("GET", "http://localhost:5749/status");

xhr.send(data);

```

```shell
# server status
curl --request GET \
  --url http://localhost:5749/status
```

> The code above will return JSON structured like this:

```json
{"status": "ALIVE"}
```


#### HTTP Request

`GET /status`
