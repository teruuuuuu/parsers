package jp.co.teruuu.parser.json

import example.spray.json.{BasicFormats, StandardFormats}

trait DefaultJsonProtocol
  extends BasicFormats
    with StandardFormats

object DefaultJsonProtocol extends DefaultJsonProtocol