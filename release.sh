#!/bin/sh

VERSION=$( qlot exec ros run -e "(format t \"~a\" (slot-value (asdf:find-system :niko) 'asdf:version))" -q)
RELEASE_DIR="Niko_v$VERSION"

mkdir $RELEASE_DIR
cp ./README.md $RELEASE_DIR

sudo docker build -t niko .
sudo docker run -t niko:latest
sudo docker ps -af ancestor=niko:latest -q
sudo docker cp "$(sudo docker ps -af ancestor=niko:latest -q):/usr/bin/niko" $RELEASE_DIR
sudo docker ps -f ancestor=niko -q | xargs sudo docker kill
sudo docker ps -f ancestor=niko -q

tar cf - $RELEASE_DIR | gzip > "$RELEASE_DIR.tar.gz"

ls $RELEASE_DIR/

rm -rf $RELEASE_DIR