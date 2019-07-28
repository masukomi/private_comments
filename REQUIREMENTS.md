## Private Comments Overview 

The primary goal of Private Comments is to allow someone to mark up a document to your heart's content without cluttering up the codebase or worrying about accidentally looking unprofessional, or hurting the feelings of others. Furthermore, editor integration is critical so as to keep comments from being lost or forgotten.

In order to support the free flow of thought without worry comments the system must be designed such that it works to actively prevent accidental sharing of comments. To this end comments _must never_ be committed to the same repository as the files being commented on. If they are committed to the same repository as the code they can be accidentally shared with others who have the same repository.

Comments _should_ be shareable with coworkers / team-mates _if_ you desire.

## Editor Integration 

It is intended that the primary mechanism of creating and viewing comments is handled via editor plugins.  Commenting on code in this system should be as seamless and easy as leaving a comment directly in the source code.

Your editor should show you only the comments that are _currently_ relevant to the file you are editing. Looking at a historical version of the file should show you the comments from that point in time.

You should be able to edit comments while viewing a file, but without modifying the file itself. It is up to the plugin creator to decide if this happens inline, like leaving a comment on a line of code in a GitHub PR, in a sidebar, or through some other means. Regardless, "Private Comments" _should_ be visually distinct from normal inline comments. 

It is suggested that plugin creators allow users to easily toggle the display of comments while working.


## Functional Requirements

* Comments must never be committed to the repo
* Comments must be viewable on multiple machines
* Access to comments must be controllable separately from access to the code.
* The persistence layer must never include the code being commented on
  because of potential NDA violations.
* Comments must be tied to a specific version of a specific line of code.
* Comments must persist when the treeish of a commit is changed as the result of a rebase.
* Must not interefere with any existing uses of git tools
* The tool _must_ have no knowledge of the code it is commenting on beyond the associated treeishes of commits.
* The code being commented on must be managed by/in a git repository.

**Note:**  
Future versions may support handling _moved_ code such that your comment moves with the code it's tied to. The initial version will not attempt to address that.

## Implementation Details

Please see [IMPLEMENTATION.md](IMPLEMENTATION.md) for details.
