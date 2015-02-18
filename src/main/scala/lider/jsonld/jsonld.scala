package eu.liderproject.jsonld

import java.io.IOException
import java.net.{URL, URI, MalformedURLException, URISyntaxException}

class JsonLDSchema(val mappings : Map[String,JsonLDSchema.TermMapping], 
  val base : Option[URI], val vocab : Option[String], 
  val language : Option[String]) {
  def ++(schema : JsonLDSchema) : JsonLDSchema = new JsonLDSchema(
    mappings ++ schema.mappings,
    (base ++ schema.base).lastOption,
    (vocab ++ schema.vocab).lastOption,
    (language ++ schema.language).lastOption
  )
}

object JsonLDSchema {
  sealed trait MappingType

  case class TypedDataMapping(uri : URI) extends MappingType

  object UntypedDataMapping extends MappingType

  object ObjectMapping extends MappingType

  sealed trait TermMapping {
    def reverse = false
    def _type : MappingType = UntypedDataMapping
  }

  case class TypedMapping(uri : URI, override val _type : MappingType, override val reverse : Boolean = false) extends TermMapping

  case class LangStringMapping(uri : URI, language : String) extends TermMapping

  case class LangContainerMapping(uri : URI) extends TermMapping

  case class ListMapping(uri : URI, override val _type : MappingType) extends TermMapping

  case class AliasMapping(alias : String) extends TermMapping

  case class IndexMapping(uri : URI, override val _type : MappingType) extends TermMapping

  private def mkURI(namespaces : Map[String,String], string : String) : URI = {
    try {
      val uri = if(string.contains(":")) {
        val prefix = string.take(string.indexOf(":"))
        namespaces.get(prefix) match {
          case Some(expansion) => expansion + string.drop(string.indexOf(":")+1)
          case None => string
        }
      } else {
        string
      }
      new URI(uri)
    } catch {
      case x : URISyntaxException => {
        namespaces.get("::base::") match {
          case Some(prefix) => try {
            return new URI(prefix + string)
          } catch {
            case x2 : URISyntaxException =>
          }
          case None => 
        }
        throw new JsonLDException("Invalid base URI: %s" format string)
      }
    }
  }

  private val urlCharset = "charset=(.*)".r

  private def readURL(url : java.net.URL) : java.io.Reader = {
    val conn = url.openConnection()

    val charSet = conn.getContentType().split(";").find(_.trim().toLowerCase().startsWith("charset=")) match {
      case Some(urlCharset(cs)) => cs
      case None => "UTF-8" // Assumption
    }

    new java.io.InputStreamReader(conn.getInputStream(), charSet)
  }

  def fromJson(elem : Json) : JsonLDSchema = elem match {
    case JsonString(urlStr) => {
      try {
        val url = new URL(urlStr)
        val jsonParser = new JsonParser(readURL(url))
        return fromJson(jsonParser())
      } catch {
        case x : MalformedURLException => throw new JsonLDException("Invalid URL for schema %s" format urlStr)
        case x : IOException => throw new JsonLDException("Could not read schema at %s" format urlStr)
        case x : JsonException => throw new JsonLDException("Not a valid JSON object at %s" format urlStr)
      }
    }
    case JsonArray(elems) => elems.map(fromJson(_)).reduce((x,y) => x ++ y)
    case JsonObject(e) => {
      val elems = JsonObject(e).toObj
      // Step 1. Extract all mappings
      val namespaces2 : Map[String,String] = for {
        (k,v) <- elems 
        if !k.contains(":") && !k.startsWith("@") && v.isInstanceOf[JsonString] && !v.toString.startsWith("@")
      } yield {
        (k, v.toString)
      }

      // Step 2. Check @base, @vocab, @language
      val base = elems.get("@base").map(elem => elem match {
        case s : String => mkURI(namespaces2,s)
        case _ => throw new JsonLDException("@base should be a string")
      })
      val vocab = elems.get("@vocab").map(elem => elem match {
        case s : String => s
        case _ => throw new JsonLDException("@vocab should be a string")
      })
      val language = elems.get("@language").map(elem => elem match {
        case s : String => s
        case _ => throw new JsonLDException("@language should be a string")
      })

      val namespaces = vocab match {
        case Some(v) => namespaces2 + ("::base::" -> v)
        case None => namespaces2
      }
    
      // Step 3. Read mapping
      val mappings = for {
        (k,v) <- elems
        if !k.startsWith("@")
      } yield {
        v match {
          case string : String if string.startsWith("@") => {
            k -> AliasMapping(string)
          }
          case string : String => {
            k -> TypedMapping(mkURI(namespaces, string), UntypedDataMapping) 
          }
          case _elems : Map[_,_] => {
            val elems = _elems.asInstanceOf[Map[String,Object]]
            def elemStr(id : String) = elems(id) match {
              case s : String => s
              case _ => throw new JsonLDException("%s should be a string" format id)
            }
            val id = if(elems.contains("@id")) {
              elemStr("@id")
            } else {
              k
            }
 
            val _type = if(elems.contains("@type")) {
              elemStr("@type") match {
                case "@id" => ObjectMapping
                case t => TypedDataMapping(mkURI(namespaces, t))
              }
            } else {
              UntypedDataMapping
            }

            if(elems.contains("@language")) {
              val language = elemStr("@language")

              k -> LangStringMapping(mkURI(namespaces, id), language)
            } else if(elems.contains("@container")) {
              elemStr("@container") match {
                case "@language" => k -> LangContainerMapping(mkURI(namespaces, id))
                case "@list" => k -> ListMapping(mkURI(namespaces, id), _type)
                case "@index" => k -> IndexMapping(mkURI(namespaces, id), _type)
                case "@set" => k -> TypedMapping(mkURI(namespaces, id), UntypedDataMapping)
                case container => throw new JsonLDException("Unexpected container value %s" format container)
              }
            } else if(elems.contains("@reverse")) {
              val reverse = elemStr("@reverse")

              k -> TypedMapping(mkURI(namespaces, reverse), _type, true)

            } else {
              k -> TypedMapping(mkURI(namespaces, id), _type)
            }
          }
          case _ => throw new JsonLDException(v.toString())
        }
      }

      return new JsonLDSchema(mappings, base, vocab, language)
    }
    case _ => throw new JsonLDException("Unexpected value %s" format elem.toString)
  }
}

class JsonLDException(val msg : String = "", val cause : Throwable = null) extends RuntimeException(msg, cause)
