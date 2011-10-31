#!/bin/sh
PROJECT=$1
ENTRY=$2
CWD=`pwd`
DIR=$(dirname `which phantomjs`)
cd $DIR

./phantomjs appbuilder/ext-app-builder.js -app-entry $ENTRY -project $CWD/$PROJECT.jsb3
./jsdb -path jsbuilder jsbuilder/bin/JSBuilder.js -p $CWD/$PROJECT.jsb3 -d $CWD

