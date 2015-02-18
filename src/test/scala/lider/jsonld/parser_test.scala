package eu.liderproject.jsonld

import java.io.StringReader
import org.scalatest._

class ParserTest extends FlatSpec with Matchers {

  "parser" should "accept the empty array" in {
    val parser = new JsonParser(new StringReader("[]"))
    val array = parser().asInstanceOf[JsonArray]
    array.hasNext should be (false)
  }

  "parser" should "accept a string literal" in {
    val parser = new JsonParser(new StringReader("[\"foo\"]"))
    val array = parser().asInstanceOf[JsonArray]
    array.hasNext should be (true)
    array.next should be (JsonString("foo"))
  }

  "parser" should "understand all JSON escapes" in {
    val parser = new JsonParser(new StringReader("[\"\\\\ \\\" \\/ \\b \\f \\n \\r \\t \\u000a\"]"))
    val array = parser().asInstanceOf[JsonArray]
    array.next should be (JsonString("\\ \" / \b \f \n \r \t \u000a"))
  }

  "parser" should "understand a list of values" in {
    val parser = new JsonParser(new StringReader("[\"a\",\"b\",\"c\"]"))
    val array = parser().asInstanceOf[JsonArray]
    array.next should be (JsonString("a"))
    array.next should be (JsonString("b"))
    array.next should be (JsonString("c"))
    array.hasNext should be (false)
  }

  "parser" should "understand all number values" in {
    val parser = new JsonParser(new StringReader("[0, -3, 2.5 , -2.5, 3e-9, -2.3e+20]"))
    val array = parser().asInstanceOf[JsonArray]
    array.next should be (JsonInt(0))
    array.next should be (JsonInt(-3))
    array.next should be (JsonNumber(2.5))
    array.next should be (JsonNumber(-2.5))
    array.next should be (JsonNumber(3e-9))
    array.next should be (JsonNumber(-2.3e+20))
  }

  "parser" should "understand empty object" in {
    val parser = new JsonParser(new StringReader("{}"))
    val obj = parser().asInstanceOf[JsonObject]
    obj.hasNext should be (false)
  }

  "parser" should "understand simple objects" in {
    val parser = new JsonParser(new StringReader("{\"foo\" :\"bar\",\"baz\":3}"))
    val obj = parser().asInstanceOf[JsonObject]
    obj.values.next should be (JsonField("foo",JsonString("bar")))
    obj.values.next should be (JsonField("baz",JsonInt(3)))
    obj.values.hasNext should be (false)
  }

  "parser" should "be able to skip iterate" in {
    val parser = new JsonParser(new StringReader("[{\"skip\":\"this\"},{\"process\":\"this\"}]"))
    val array = parser().asInstanceOf[JsonArray]
    array.values.next
    array.values.next.asInstanceOf[JsonObject].values.next should be (JsonField("process", JsonString("this")))
  }


}


