package io.shiftleft.cpgschema2dot

import better.files.File
import org.json4s.JsonAST.{JArray, JField, JString}
import org.json4s.{DefaultFormats, JObject, JValue}
import org.json4s.native.JsonMethods.parse

import scala.sys.process.Process
import scala.util.{Failure, Success, Try}

object CpgSchema2Dot {

  implicit val formats: DefaultFormats.type = DefaultFormats

  private case class NodeKey(name: String, valueType: String = "", cardinality: String = "") {
    override def toString: String =
      if (valueType.isEmpty) {
        s"$name $cardinality"
      } else if (cardinality.isEmpty) {
        s"$name: $valueType"
      } else {
        s"$name: $valueType $cardinality"
      }
  }

  private case class Node(name: String, keys: List[String], outEdges: List[Edge])

  private case class Edge(label: String, inNodes: List[String], cardinality: String)

  private def loadFromFile(jsonFile: File): String =
    jsonFile.lineIterator.filterNot(_.trim.startsWith("//")).mkString("\n")

  private def jsonFromString(content: String): JValue =
    parse(content)

  private def nodeKeysRaw(json: JValue): JValue =
    json \ "nodeKeys"

  private def nodeTypesRaw(json: JValue): JValue =
    json \ "nodeTypes"

  private def nodeBaseTraitRaw(json: JValue): JValue =
    json \ "nodeBaseTraits"

  private def mapCardinality(cardinality: String): String = cardinality match {
    case "one"       => "(1)"
    case "list"      => "(0:n)"
    case "zeroOrOne" => "(0:1)"
  }

  private def toNode(config: Config, json: JValue, nodeKeys: List[NodeKey]): Node = {
    val name = (json \ "name").extract[String]
    val keys =
      if (config.noNodeKeys) Nil
      else
        (json \ "keys")
          .extract[List[String]]
          .map(k => nodeKeys.find(_.name == k).getOrElse(NodeKey(name = k)).toString)
    val isEdges  = (json \ "is").extract[List[String]].map(n => Edge("is", List(n), ""))
    val outEdges = json \ "outEdges"
    val edges = for {
      JObject(edge)                      <- outEdges
      JField("edgeName", JString(label)) <- edge
      JField("inNodes", JArray(list))    <- edge
    } yield {
      val l = list.map { n =>
        val cardinality = (n \ "cardinality").extract[Option[String]] match {
          case Some(value) => s"($value)"
          case None        => ""
        }
        val name = (n \ "name").extract[String]
        Edge(label, List(name), cardinality)
      }
      l
    }
    Node(name, keys, edges.flatten ++ isEdges)
  }

  private def toNodeBase(config: Config, json: JValue, nodeKeys: List[NodeKey]): Node = {
    val name = (json \ "name").extract[String]
    val keys =
      if (config.noNodeKeys) Nil
      else
        (json \ "hasKeys")
          .extract[List[String]]
          .map(k => nodeKeys.find(_.name == k).getOrElse(NodeKey(name = k)).toString)
    val is = (json \ "extends").extract[List[String]].map(n => Edge("is", List(n), ""))
    Node(name, keys, is)
  }

  private def toNodeKey(json: JValue): NodeKey = {
    val name        = (json \ "name").extract[String]
    val valueType   = (json \ "valueType").extract[String]
    val cardinality = (json \ "cardinality").extract[String]
    NodeKey(name, valueType, mapCardinality(cardinality))
  }

  private def nodes(config: Config, json: JValue): List[Node] = {
    val allNodeTypes = nodeTypesRaw(json)
    val allNodeKeys  = nodeKeysRaw(json).children.map(toNodeKey)

    allNodeTypes.children.map(n => toNode(config, n, allNodeKeys)) ++
      nodeBaseTraitRaw(json).children.map(n => toNodeBase(config, n, allNodeKeys))
  }

  private def namedGraphBegin(): StringBuilder = {
    val sb = new StringBuilder
    sb.append(s"digraph cpgschema {\nforcelabels=true;\n")
  }

  private def graphEnd(sb: StringBuilder): String = {
    sb.append("\n}\n")
    sb.toString
  }

  private def nodeToDot(node: Node): String = {
    val keys =
      if (node.keys.nonEmpty)
        node.keys.mkString("<FONT POINT-SIZE='10'>", "<BR/>", "</FONT>")
      else ""
    s""""${node.name}" [ shape=record, label = <<B>${node.name}</B><BR/>$keys> ]"""
  }

  private def edgeToDot(from: String, edge: Edge): String = {
    val edgeLabel = edge.label
    val card = edge.cardinality match {
      case "" => ""
      case c  => s" $c"
    }
    val color = edgeLabel match {
      case l if l == "AST" => "red"
      case l if l == "CFG" => "blue"
      case l if l == "is"  => "gray, arrowhead = empty"
      case _               => "black"
    }
    val labelStr = s""" [ xlabel = "$edgeLabel$card", fontsize=8, color=$color ]"""
    edge.inNodes.map(n => s""""$from" -> "$n"""" + labelStr).mkString("\n")
  }

  private def dotGraph(nodes: List[Node], edgeFilterList: List[String]): String = {
    val sb = namedGraphBegin()

    val filteredNodes = nodes.map(n =>
      n.copy(outEdges = n.outEdges.filter(e => edgeFilterList.exists(e.label.contains))))

    val nodeStrings = filteredNodes.map(nodeToDot)
    val edgeStrings = filteredNodes.flatMap(n => n.outEdges.map(e => edgeToDot(n.name, e)))
    sb.append((nodeStrings ++ edgeStrings).mkString("\n"))

    graphEnd(sb)
  }

  private def createSvgFile(in: File, out: File): Try[String] = {
    Try {
      Process(
        Seq("dot",
            "-Gsplines=ortho",
            "-Tsvg",
            in.path.toAbsolutePath.toString,
            "-o",
            out.path.toAbsolutePath.toString)).!!
    } match {
      case Success(v) => Success(v)
      case Failure(exc) =>
        System.err.println("Executing `dot` failed: is `graphviz` installed?")
        System.err.println(exc)
        Failure(exc)
    }
  }

  private def generateOutput(config: Config,
                             fileName: String,
                             nodes: List[Node],
                             edgeFilterList: List[String]): Unit = {
    val outDotFile = config.outDir / s"$fileName.dot"
    val outSvgFile = config.outDir / s"$fileName.svg"
    if (outDotFile.exists) outDotFile.delete()
    if (outSvgFile.exists) outSvgFile.delete()
    outDotFile.write(dotGraph(nodes, edgeFilterList))
    createSvgFile(outDotFile, outSvgFile)
    println(s"Saved '$outSvgFile' successfully.")
  }

  private def nodesInDir(dir: String,
                         allNodes: List[Node],
                         node: Node,
                         edgeFilterList: List[String]): List[Node] = dir match {
    case "IN" =>
      allNodes.filter { n =>
        val resNodes =
          n.outEdges.filter(e => edgeFilterList.exists(e.label.contains)).flatMap(_.inNodes)
        resNodes.contains(node.name)
      }
    case "OUT" =>
      allNodes.filter { n =>
        val resNodes =
          node.outEdges.filter(e => edgeFilterList.exists(e.label.contains)).flatMap(_.inNodes)
        resNodes.contains(n.name)
      }
  }

  private def run(config: Config): Unit = {
    println(config.toString)

    val jsonContent = loadFromFile(config.jsonFile)
    val json        = jsonFromString(jsonContent)

    config.outDir.createDirectoryIfNotExists()

    val allNodes = nodes(config, json)

    val nodeFilterList = config.selectedNodes match {
      case seq if seq.nonEmpty => seq
      case _                   => allNodes.map(_.name)
    }

    val edgeFilterList = config.selectedEdges match {
      case seq if seq.nonEmpty => seq
      case _                   => allNodes.flatMap(_.outEdges.map(_.label))
    }

    val candidates = allNodes
      .filter(n => nodeFilterList.contains(n.name))
      .map {
        case node if config.resolve =>
          val inNodes = nodesInDir("IN", allNodes, node, edgeFilterList).map { n =>
            n.copy(outEdges = n.outEdges.filter(e => e.inNodes.contains(node.name)))
          }
          val outNodes       = nodesInDir("OUT", allNodes, node, edgeFilterList)
          val dependentNodes = outNodes.map(_.copy(outEdges = List.empty)) ++ inNodes
          val remainingNodes = allNodes
            .filter { n =>
              val l = dependentNodes.flatMap(n => n.outEdges.flatMap(_.inNodes))
              node.name != n.name && !dependentNodes.exists(_.name == n.name) && l.contains(n.name)
            }
            .map(_.copy(outEdges = List.empty))
          node -> (dependentNodes ++ remainingNodes)
        case node =>
          val outNodes = nodesInDir("OUT", allNodes, node, edgeFilterList)
          node -> outNodes.map(_.copy(outEdges = List.empty))
      }

    if (config.saveIndividually) {
      candidates.foreach { case (k, v) => generateOutput(config, k.name, k +: v, edgeFilterList) }
    } else {
      generateOutput(config,
                     config.jsonFile.nameWithoutExtension,
                     candidates.flatMap(c => c._1 +: c._2).distinct,
                     edgeFilterList)
    }

  }

  def main(args: Array[String]): Unit = {
    val argumentsParser: ArgumentsParser = new ArgumentsParser()

    argumentsParser.parse(args) match {
      case Some(config) =>
        run(config)
      case None =>
        argumentsParser.showUsage()
        System.exit(1)
    }
  }
}
