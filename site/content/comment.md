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

## Comment Routes

* [Add / Update a comment](#add-update-a-comment-route)
* [Retrieve comments for a file](#retrieve-comments-for-a-file-route)

## Add / Update a comment route

```javascript
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

This endpoint creates or updates a single company object.

#### HTTP Request

`POST /v1/comments`

## Retrieve comments for a file route

```javascript
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

