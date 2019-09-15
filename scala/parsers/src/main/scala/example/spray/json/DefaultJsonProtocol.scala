package example.spray.json

trait DefaultJsonProtocol
  extends BasicFormats
    with StandardFormats

object DefaultJsonProtocol extends DefaultJsonProtocol