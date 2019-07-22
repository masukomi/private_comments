#!/bin/sh

# makes sure all the required chicken scheme eggs
# are installed
echo "Will run chicken-install for required eggs"

chicken-install filepath
chicken-install intarweb
chicken-install medea
chicken-install simple-loops
chicken-install spiffy
chicken-install spiffy-request-vars
chicken-install uri-common
chicken-install shell

echo "DONE"
