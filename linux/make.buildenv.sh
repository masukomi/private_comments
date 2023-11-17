#! /bin/bash
# Builds a Docker environment for building private_comments.
# Run from the project root:
# 	./build/make.buildenv.sh

docker build -t buildenv -f build/Dockerfile .
