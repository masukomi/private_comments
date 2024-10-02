#!/bin/sh

# makes sure all the required chicken scheme eggs
# are installed
echo "Will run chicken-install for required eggs (libraries)"


chicken-install args
chicken-install filepath
chicken-install format
chicken-install http-client
chicken-install intarweb
chicken-install linenoise
chicken-install medea
chicken-install message-digest
chicken-install message-digest-utils
chicken-install sha2
chicken-install shell
chicken-install simple-exceptions
chicken-install simple-loops
chicken-install spiffy
chicken-install spiffy-request-vars
chicken-install uri-common
echo "⚠️ WARN: this app requires mdcd (Markdown Code Docs)
which is not currently available via chicken-install for
Chicken Scheme 5.x

If you have not dones so already, please
* clone this repo: https://github.com/masukomi/mdcd
* run: chicken-install from within it.

to build and install the mdcd egg locally."


echo "DONE"
