name := "parsers"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
// バージョン確認 https://repo1.maven.org/maven2/com/github/fommil/spray-json-shapeless_2.12/
libraryDependencies += "com.github.fommil" %% "spray-json-shapeless" % "1.4.0"
