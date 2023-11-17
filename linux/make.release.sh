#! /bin/bash

# Builds a release of private_comments.
# Run from the project root.

set -x

BUILD_VERSION="${BUILD_VERSION:-}"

# Building the build environment, since we don't have a good
# spot to place the buildenv container.
./build/make.buildenv.sh

readonly _script_dir="$(pwd)"

docker run --interactive --tty \
		-u "$(id -u):$(id -g)" \
		-v "${_script_dir}:/src:rw" \
		buildenv \
		/bin/bash -c "cd /src/src && ./build.sh ${BUILD_VERSION}"
