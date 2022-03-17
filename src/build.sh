#!/bin/sh

# required libraries

# args
# filepath
# http-client
# intarweb
# linenoise
# medea
# message-digest
# sha2
# shell
# simple-exceptions
# simple-loops
# spiffy
# spiffy-request-vars
# uri-common


VERSION="dev_version"
if [ "$1" != "" ]; then
  VERSION=$1
  perl -pi -e "s/VERSION_NUMBER_HERE/$1/" pc.scm
  perl -pi -e "s/VERSION_NUMBER_HERE/$1/" private_comments.scm
fi

echo "Building libraries for $VERSION"

csc -static -unit masutils -cJ masutils.scm
csc -static -unit masufiles -cJ masufiles.scm
csc -static -unit masurequests -cJ masurequests.scm
csc -static -unit pathname-expand -cJ pathname-expand.scm
csc -static -unit comment-recording -cJ comment-recording.scm
csc -static -unit listicles -cJ listicles.scm

echo "Building private_comments executable..."

csc -link masutils \
    -link masufiles \
    -link pathname-expand \
    -static private_comments.scm


echo "Building pc executable..."

csc -link masufiles \
    -link masurequests \
    -link masutils \
    -link comment-recording \
    -link listicles \
    -static pc.scm


if [ "$1" != "" ]; then
  perl -pi -e "s/$1/VERSION_NUMBER_HERE/" pc.scm
  perl -pi -e "s/$1/VERSION_NUMBER_HERE/" private_comments.scm
fi
ARCHITECTURE=$(arch)
version_dir="private_comments_"$ARCHITECTURE"_"$VERSION
echo "creating compressed release file..."
echo "  $version_dir.tgz"
rm -rf $version_dir
mkdir $version_dir
cp private_comments $version_dir/
cp pc $version_dir/

# copy them to the bin directory so that
# you have a dir you can add to your path
# that always has the latest executable in it
mkdir -p ../bin
cp private_comments ../bin/
cp pc ../bin/

# compress it
tar -czf $version_dir.tgz $version_dir
rm -rf $version_dir

echo "here's your SHA for homebrew"
shasum -a 256 $version_dir.tgz

function print_dlibs () {
  binary=$1
  echo "DLIBS used by $binary:"

  DYLIBS=`otool -L $binary | grep "/opt" | awk -F' ' '{ print $1 }'`
  if [ "$DYLIBS" == "" ]; then
    echo "  None!"
  else
    for dylib in $DYLIBS
    do
      echo " - dylib $dylib"
    done
  fi

}

print_dlibs "private_comments"
print_dlibs "pc"

echo "===================="
echo "Done."

