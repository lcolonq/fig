#!/bin/sh
purs-nix bundle
scp -r main.css main.js index.html assets/ pub.colonq.computer:~/public_html
