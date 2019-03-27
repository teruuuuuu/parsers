# start.bat
for /F "usebackq tokens=*" %%i in (`echo %~dp0`) do @set FILE_PATH=%%i
echo %FILE_PATH%

cd %FILE_PATH%
set GOPATH=%FILE_PATH%
set PATH=%PATH%;%GOPATH%\bin;
cd src/parser
go run main.go