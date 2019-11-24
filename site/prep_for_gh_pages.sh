#!/bin/sh

rm -rf public
git worktree add -B gh-pages public upstream/gh-pages
