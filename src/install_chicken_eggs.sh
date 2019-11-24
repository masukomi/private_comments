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
chicken-install sha2
chicken-install shell
chicken-install simple-exceptions
chicken-install simple-loops
chicken-install spiffy
chicken-install spiffy-request-vars
chicken-install uri-common

echo "DONE"
