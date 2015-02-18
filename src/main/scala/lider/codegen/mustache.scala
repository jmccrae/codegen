package eu.liderproject.codegen

import com.github.mustachejava._
import java.io.File
import java.io.Reader
import java.net.URI
import scala.collection.JavaConversions._

object MustacheCodeGen {
  import CodeGen._

  private class TypeConversion(map : Map[String,String]) {
    def apply(foo : String) = map.getOrElse(foo, foo)
  }

  private object TypeConversion {
    def apply(elems : (String,String)*) = new TypeConversion(Map(elems:_*))
  }

  private val typeConversions = Map(
    "java" -> TypeConversion(
      "anyURI" -> "String",
      "any" -> "String",
      "decimal" -> "double",
      "integer" -> "int",
      "string" -> "String",
      "boolean" -> "boolean"
    ),
    "scala" -> TypeConversion(
      "anyURI" -> "String",
      "any" -> "String",
      "decimal" -> "Double",
      "integer" -> "Int",
      "string" -> "String",
      "boolean" -> "Boolean"
    )
  )
  private val stringTypes = Set("string")
  private val integerTypes = Set("integer")
  private val numberTypes = Set("decimal")
  private val boolTypes = Set("boolean")

  private def clazzToMap(clazz : CodeGenClass, types : TypeConversion) : java.util.Map[String, Any] = mapAsJavaMap(Map(
    "Type" -> clazz.name,
    "package" -> clazz.packageName,
    "uri" -> clazz.uri,
    "fields" -> seqAsJavaList(for((CodeGenField(name, uri, range, functional),index) <- clazz.fields.zipWithIndex) yield {
      mapAsJavaMap(Map(
        "name" -> name,
        "Name" -> ucFirst(name),
        "uri" -> uri,
        "range" -> (range match {
          case c2 : CodeGenClass => types(c2.name)
          case v : CodeGenValue => types(v._type)
          case CodeGenAnyURI => types("anyURI")
          case CodeGenAny => types("any")
        }),
        "functional" -> functional,
        "last" -> (name == clazz.fields.last.name),
        "index" -> index,
        "object" -> range.isInstanceOf[CodeGenClass],
        "string" -> (range match {
          case v : CodeGenValue => 
            stringTypes contains (v._type)
          case CodeGenAnyURI => true
          case CodeGenAny => true
          case _ => false
        }),
        "int" -> (range match {
          case v : CodeGenValue => integerTypes contains (v._type)
          case _ => false
        }),
        "number" -> (range match {
          case v : CodeGenValue => numberTypes contains (v._type)
          case _ => false
        }),
        "bool" -> (range match {
          case v : CodeGenValue => boolTypes contains (v._type)
          case _ => false
        })
      ))
    })
  ))


  def generate(template : File, codeGen : CodeGenClass, target : File) {
    generate(new java.io.FileReader(template), template.getName().dropRight(".mustache".size), codeGen, target)
  }

  def generate(template : Reader, templateName : String, codeGen : CodeGenClass, target : File) = {
    val factory = new DefaultMustacheFactory()
    val mustache = factory.compile(template, "codegen")
    val out = new java.io.FileWriter(target)
    val map = clazzToMap(codeGen, typeConversions.getOrElse(templateName, TypeConversion()))
    mustache.execute(out, map)
    out.flush
    out.close
  }
}
