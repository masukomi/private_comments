#! /bin/bash
# Builds a Docker environment for building private_comments.
# Run from the project root:
# 	./linux/make.buildenv.sh

set -x
set -euo pipefail

docker build -t private_comments_buildenv -f linux/Dockerfile .
