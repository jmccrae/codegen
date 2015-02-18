package eu.liderproject.jsonld

object JsonParser {
  object WhiteSpace {
    def unapply(c : Char) = if(
      c == '\u0009' ||
      (c >= '\u000A' && c <= '\u000d') ||
      c == ' ' ||
      c == '\u0085' ||
      c == '\u00a0' ||
      c == '\u1680' ||
      (c >= '\u2000' && c <= '\u200a') ||
      c == '\u2028' ||
      c == '\u2029' ||
      c == '\u202f' ||
      c == '\u205f' ||
      c == '\u3000'
    ) {
      Some(c)
    } else {
      None
    }
  }

  object HexCharacter {
    def unapply(c : Char) = if(c >= '0' && c <= '9') {
      Some(c.toInt - 48)
    } else if(c >= 'a' && c <= 'f') {
      Some(c.toInt - 87)
    } else if(c >= 'A' && c <= 'F') {
      Some(c.toInt - 55)
    } else {
      None
    }
  }

  val Number = "(-)?([0-9]*)(\\.[0-9]+)?([eE][+-][0-9]+)?".r
}

class JsonParser(reader : java.io.Reader) {
  import JsonParser._

  private val BUF_MAX = 4096
  private var pos = 0
  private var buf = new Array[Char](BUF_MAX)
  private var read = 0

  def apply() : Json = {
    while(read >= 0) {
      while(pos >= read) {
        pos = 0
        read = reader.read(buf)
      }
      buf(pos) match {
        case '{' => 
          pos += 1
          val o = JsonObject(new ObjectIterator())
          return o
        case '[' =>
          pos += 1
          val a = JsonArray(new ArrayIterator())
          return a
        case WhiteSpace(_) =>
          pos += 1
        case c => unexpected("'{' or '['")
      }
    }
    throw new JsonException("Empty document")
  }

  private def readStringLiteral : String = {
    var escaped = false
    var builder = new StringBuilder()
    var finished = false
    pos += 1
    while(read >= 0 && !finished) {
      while(pos >= read) {
        pos = 0
        read = reader.read(buf)
      }
      buf(pos) match {
        case '\\' =>
          if(escaped) {
            builder.append('\\')
            escaped = false
          } else {
            escaped = true
          }
        case '"' => 
          if(escaped) {
            builder.append('"')
            escaped = false
          } else {
            finished = true
          }
        case 'b' => 
          if(escaped) {
            builder.append('\b')
          } else {
            builder.append('b')
          }
          escaped = false
        case 'f' => 
          if(escaped) {
            builder.append('\f')
          } else {
            builder.append('f')
          }
          escaped = false
        case 'n' => 
          if(escaped) {
            builder.append('\n')
          } else {
            builder.append('n')
          }
          escaped = false
        case 'r' => 
          if(escaped) {
            builder.append('\r')
          } else {
            builder.append('r')
          }
          escaped = false
        case 't' => 
          if(escaped) {
            builder.append('\t')
          } else {
            builder.append('t')
          }
          escaped = false
        case 'u' => 
          if(escaped) {
            var pos2 = pos + 1
            var hex = 0
            while(read >= 0 && pos2 < pos + 5) {
              while(pos2 >= read) {
                pos2 = 0
                read = reader.read(buf)
              }
              buf(pos2) match {
                case HexCharacter(i) =>
                  hex = hex * 16 + i
                  pos2 += 1
                case c =>
                  throw new JsonException("Bad hexadecimal string")
              }
            }
            if(pos2 != pos + 5) {
              eod()
            }
            builder.append(hex.toChar)
            pos += 4                      
          } else {
            builder.append('u')
          }
          escaped = false
        case c =>
          builder.append(c)
          escaped = false
      }
      pos += 1
    }
    if(!finished) {
      eod()
    }
    return builder.toString
  }

  def readLiteral : Json = {
    var finished = false
    var builder = new StringBuilder()
    while(read >= 0 && !finished) {
      while(pos >= read) {
        pos = 0
        read = reader.read(buf)
      }
      buf(pos) match {
        case WhiteSpace(_) =>
          finished = true
          pos += 1
        case ',' =>
          finished = true
        case ']' =>
          finished = true
        case '}' =>
          finished = true      
        case c =>
          builder.append(c)
          pos += 1
      }
    }
    builder.toString match {
      case "" => 
        unexpected("literal value")
        null
      case "true" => JsonBoolean(true)
      case "false" => JsonBoolean(false)
      case "null" => JsonNull
      case num @ Number(_,int,frac,exp) => 
        if(int.startsWith("00")) {
          throw new JsonException("Octal literals are not standard Json")
        }
        if(frac == null && exp == null) {
          return JsonInt(num.toInt)
        } else {
          return JsonNumber(num.toDouble)
        }
      case otherwise => 
        unexpected("literal value")
        null
    }
  }

  def eod() { 
    throw new JsonException("Unexpected end of document")
  }

  def unexpected(expected : String) {
    throw new JsonException("Expected %s but found %s" format(expected, new String(buf, pos, math.min(pos + 10, read) - pos)))
  }

  private class ArrayIterator() extends Iterator[Json] {
    private var atEnd = false
    private var child : Option[Json] = None

    def hasNext : Boolean = if(atEnd) {
      return false
    } else {
      child match {
        case Some(j) => 
          j.toObj
          nextField
          child = None
        case None =>
      }      
      if(atEnd) {
        return false
      }
      while(read >= 0) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case ']' => 
            pos += 1
            atEnd = true
            return false
          case WhiteSpace(_) => 
            pos += 1
          case _ => 
            return true
        }
      }
      eod()
      return false
    }

    private def nextField {
      var finished = false
      while(read >= 0 && !finished) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case ',' =>
            finished = true
            pos += 1
          case WhiteSpace(_) =>
            pos += 1
          case ']' =>
            finished = true
            atEnd = true
            pos += 1
          case c => unexpected("',' or ']'")
        }
      }
      if(!finished) {
        eod()
      }
    }

    def next : Json = {
      child match {
        case Some(j) => 
          j.toObj
          nextField
          child = None
        case None =>
      }
      while(read >= 0) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case '"' => 
            val j = JsonString(readStringLiteral)
            nextField
            return j
          case WhiteSpace(_) => 
            pos += 1
          case '{' => 
            pos += 1
            val o = JsonObject(new ObjectIterator())
            child = Some(o)
            return o
          case '[' =>
            pos += 1
            val a = JsonArray(new ArrayIterator())
            child = Some(a)
            return a
          case c => 
            val l = readLiteral
            nextField
            return l
        }
      }
      eod()
      return null
    }
  }

  class ObjectIterator() extends Iterator[JsonField] {
    private var atEnd = false
    private var child : Option[Json] = None

    def hasNext : Boolean = if(atEnd) {
      return false
    } else {
      child match {
        case Some(j) => 
          j.toObj
          nextField
          child = None
        case None =>
      }      
      if(atEnd) {
        return false
      }
      while(read >= 0) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case '}' => 
            pos += 1
            atEnd = true
            return false
          case WhiteSpace(_) => 
            pos += 1
          case _ => 
            return true
        }
      }
      eod()
      return false
    }

    private def nextField {
      var finished = false
      while(read >= 0 && !finished) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case ',' =>
            finished = true
            pos += 1
          case WhiteSpace(_) =>
            pos += 1
          case '}' =>
            finished = true
            atEnd = true
            pos += 1
          case c => unexpected("',' or '}'")
        }
      }
      if(!finished) {
        eod()
      }
    }

    def next : JsonField = {
      child match {
        case Some(j) => 
          j.toObj
          nextField
          child = None
        case None =>
      }
      var fieldName : String = null
      while(read >= 0 && fieldName == null) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case '"' =>
            fieldName = readStringLiteral
          case WhiteSpace(_) =>
            pos += 1
          case c => 
            unexpected("field name")
        }
      }
      if(fieldName == null) {
        eod()
      }
      var separated = false
      while(read >= 0 && !separated) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case ':' =>
            separated = true
            pos += 1
          case WhiteSpace(_) =>
            pos += 1
          case c =>
            unexpected("':'")
        }
      }
      if(!separated) {
        eod()
      }

      while(read >= 0) {
        while(pos >= read) {
          pos = 0
          read = reader.read(buf)
        }
        buf(pos) match {
          case '"' => 
            val j = JsonString(readStringLiteral)
            nextField
            return JsonField(fieldName, j)
          case WhiteSpace(_) => 
            pos += 1
          case '{' => 
            pos += 1
            val o = JsonObject(new ObjectIterator())
            child = Some(o)
            return JsonField(fieldName, o)
          case '[' =>
            pos += 1
            val a = JsonArray(new ArrayIterator())
            child = Some(a)
            return JsonField(fieldName, a)
          case c => 
            val l = readLiteral
            nextField
            return JsonField(fieldName, l)
        }
      }
      eod()
      return null
    }
  }
}
