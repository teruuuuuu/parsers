for /F "usebackq tokens=*" %%i in (`echo %~dp0`) do @set FILE_PATH=%%i
echo %FILE_PATH%

cd %FILE_PATH%
set GOPATH=%FILE_PATH%
set PATH=%PATH%;%GOPATH%\bin;
REM VisualStuduioCodeの場合.vscode/settings.jsonに"go.gopath"が現在のディレクトリに設定して追加
cd src/parser
dep ensure