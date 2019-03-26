#!/bin/sh
FILE_PATH=$(cd $(dirname $0) && pwd)
export GOPATH=$FILE_PATH
cd $FILE_PATH
# cd src/parser
go test -v ./src/parser