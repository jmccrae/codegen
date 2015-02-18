name := "codegen"

organization := "eu.liderproject"

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
        "org.apache.jena" % "jena-arq" % "2.12.0",
        "com.github.spullara.mustache.java" % "compiler" % "0.8.16",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test"
      )

