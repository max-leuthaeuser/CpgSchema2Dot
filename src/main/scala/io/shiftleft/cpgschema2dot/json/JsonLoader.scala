package io.shiftleft.cpgschema2dot.json

import better.files.File
import org.json4s.native.JsonMethods.parse
import org.json4s.DefaultFormats
import org.json4s.JValue

class JsonLoader {

  implicit val formats: DefaultFormats.type = DefaultFormats

  def loadFromFile(jsonFile: File): JValue =
    jsonFromString(jsonFile.lineIterator.filterNot(_.trim.startsWith("//")).mkString("\n"))

  private def jsonFromString(content: String): JValue =
    parse(content)

  def nodeKeysRaw(json: JValue): JValue =
    json \ "nodeKeys"

  def nodeTypesRaw(json: JValue): JValue =
    json \ "nodeTypes"

  def nodeBaseTraitRaw(json: JValue): JValue =
    json \ "nodeBaseTraits"

  def nameRaw(json: JValue): String =
    (json \ "name").extract[String]

  def valueTypeRaw(json: JValue): String =
    (json \ "valueType").extract[String]

  def cardinalityRaw(json: JValue): String =
    (json \ "cardinality").extract[String]

  def keysRaw(json: JValue): List[String] =
    (json \ "keys").extract[List[String]]

  def isEdgesRaw(json: JValue): List[String] =
    (json \ "is").extract[List[String]]

  def extendsEdgesRaw(json: JValue): List[String] =
    (json \ "extends").extract[List[String]]

  def outEdgesRaw(json: JValue): JValue =
    json \ "outEdges"

  def hasKeysRaw(json: JValue): List[String] =
    (json \ "hasKeys").extract[List[String]]

  def cardinality(json: JValue): String =
    (json \ "cardinality").extract[Option[String]] match {
      case Some(value) => s"($value)"
      case None        => ""
    }
}
