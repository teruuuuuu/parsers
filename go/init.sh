#!/bin/sh
FILE_PATH=$(cd $(dirname $0) && pwd)
cd $FILE_PATH
export GOPATH=$FILE_PATH
cd src/parser
# dep init
dep ensure