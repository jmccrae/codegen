package eu.liderproject.jsonld

sealed trait Json {
  def toObj : Object
}

object Json {
  def apply(objs : (String, Json)*) = JsonObject(objs.
    map({case (x, y) => JsonField(x, y)}).iterator)
  def apply(objs : Json*) = JsonArray(objs.iterator)
}

case class JsonArray(val values : Iterator[Json]) extends Json {
  def toObj : List[Object] = (values map (_.toObj)).toList
  def next = values.next
  def hasNext = values.hasNext
}

case class JsonObject(val values : Iterator[JsonField]) extends Json {
  def toObj : Map[String, Object] = (values map ({ case JsonField(k,v) => k -> v.toObj })).toMap
  def next = values.next
  def hasNext = values.hasNext
}

case class JsonField(val key : String, val value : Json)

case class JsonString(val value : String) extends Json {
  def toObj = value
}

case class JsonInt(val value : Int) extends Json {
  def toObj = new Integer(value)
}

case class JsonNumber(val value : Double) extends Json {
  def toObj = new java.lang.Double(value)
}

case class JsonBoolean(val value : Boolean) extends Json {
  def toObj = new java.lang.Boolean(value)
}

object JsonNull extends Json {
  def toObj = null
}

case class JsonException(message : String = "", cause : Throwable = null) extends
  RuntimeException(message, cause)

