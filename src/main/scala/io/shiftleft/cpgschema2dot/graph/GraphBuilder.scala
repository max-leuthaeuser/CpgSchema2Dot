package io.shiftleft.cpgschema2dot.graph

import io.shiftleft.cpgschema2dot.json.JsonLoader
import io.shiftleft.cpgschema2dot.Config
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JField
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JValue

class GraphBuilder(private val config: Config, private val jsonLoader: JsonLoader) {

  def nodes(json: JValue): List[Node] = {
    val allNodeKeys = jsonLoader.nodeKeysRaw(json).children.map(toNodeKey)

    val allNodes =
      jsonLoader.nodeTypesRaw(json).children.map(n => toNode(n, allNodeKeys))
    val allNodeBaseTraits =
      jsonLoader.nodeBaseTraitRaw(json).children.map(n => toNodeBase(n, allNodeKeys))

    allNodes ++ allNodeBaseTraits
  }

  private def toNode(json: JValue, nodeKeys: List[NodeKey]): Node = {
    val name = jsonLoader.nameRaw(json)
    val keys =
      if (config.noNodeKeys) Nil
      else
        jsonLoader
          .keysRaw(json)
          .map(k => nodeKeys.find(_.name == k).getOrElse(NodeKey(name = k)).toString)
    val isEdges  = jsonLoader.isEdgesRaw(json).map(n => Edge("is", List(n), ""))
    val outEdges = jsonLoader.outEdgesRaw(json)
    val edges = for {
      JObject(edge)                      <- outEdges
      JField("edgeName", JString(label)) <- edge
      JField("inNodes", JArray(list))    <- edge
    } yield {
      val l = list.map { n =>
        val cardinality = jsonLoader.cardinality(n)
        val name        = jsonLoader.nameRaw(n)
        Edge(label, List(name), cardinality)
      }
      l
    }
    Node(name, keys, edges.flatten ++ isEdges)
  }

  private def toNodeKey(json: JValue): NodeKey = {
    val name        = jsonLoader.nameRaw(json)
    val valueType   = jsonLoader.valueTypeRaw(json)
    val cardinality = jsonLoader.cardinalityRaw(json)
    NodeKey(name, valueType, mapCardinality(cardinality))
  }

  private def mapCardinality(cardinality: String): String = cardinality match {
    case "one"       => "(1)"
    case "list"      => "(0:n)"
    case "zeroOrOne" => "(0:1)"
  }

  private def toNodeBase(json: JValue, nodeKeys: List[NodeKey]): Node = {
    val name = jsonLoader.nameRaw(json)
    val keys =
      if (config.noNodeKeys) Nil
      else
        jsonLoader
          .hasKeysRaw(json)
          .map(k => nodeKeys.find(_.name == k).getOrElse(NodeKey(name = k)).toString)
    val is = jsonLoader.extendsEdgesRaw(json).map(n => Edge("is", List(n), ""))
    Node(name, keys, is)
  }

}
