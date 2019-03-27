#!/bin/sh
FILE_PATH=$(cd $(dirname $0) && pwd)
cd $FILE_PATH
export GOPATH=$FILE_PATH
cd src/parser
# VisualStuduioCodeの場合.vscode/settings.jsonに"go.gopath"が現在のディレクトリに設定して追加
# dep init
# デバッグに必要
# xcode-select --install
dep ensure