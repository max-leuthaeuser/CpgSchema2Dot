package io.shiftleft.cpgschema2dot

import better.files.File
import org.json4s.JsonAST.{JArray, JField, JString}
import org.json4s.{DefaultFormats, JObject, JValue}
import org.json4s.native.JsonMethods.parse

import scala.sys.process.Process
import scala.util.{Failure, Success, Try}

object CpgSchema2Dot {

  implicit val formats: DefaultFormats.type = DefaultFormats

  private case class Node(name: String, keys: List[String], is: List[String], outEdges: List[Edge])

  private case class Edge(label: String, inNodes: List[String])

  private def loadFromFile(jsonFile: File): String =
    jsonFile.lineIterator.filterNot(_.trim.startsWith("//")).mkString("\n")

  private def jsonFromString(content: String): JValue =
    parse(content)

  private def nodeTypes(json: JValue): JValue =
    json \ "nodeTypes"

  private def nodeBaseTrait(json: JValue): JValue =
    json \ "nodeBaseTraits"

  private def nodes(json: JValue): List[Node] = {
    val allNodeTypes = nodeTypes(json)
    allNodeTypes.children.map { jValue =>
      val name     = (jValue \ "name").extract[String]
      val keys     = (jValue \ "keys").extract[List[String]]
      val is       = (jValue \ "is").extract[List[String]]
      val outEdges = jValue \ "outEdges"
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
          val name       = (n \ "name").extract[String]
          val finalLabel = s"$label $cardinality"
          Edge(finalLabel, List(name))
        }
        l
      }
      Node(name, keys, is, edges.flatten)
    } ++ nodeBaseTrait(json).children.map { n =>
      val name = (n \ "name").extract[String]
      val keys = (n \ "hasKeys").extract[List[String]]
      val is   = (n \ "extends").extract[List[String]]
      Node(name, keys, is, List.empty)
    }
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
    val color = edgeLabel match {
      case l if l.contains("AST") => "red"
      case l if l.contains("CFG") => "blue"
      case _                      => "black"
    }
    val labelStr = s""" [ xlabel = "$edgeLabel", fontsize=8, color=$color ]"""
    edge.inNodes.map(n => s""""$from" -> "$n"""" + labelStr).mkString("\n")
  }

  private def isToDot(node: Node): String = {
    val edgeLabel = "is"
    val labelStr =
      s""" [ xlabel = "$edgeLabel", arrowhead = empty, fontsize=8, color=gray ]"""
    node.is.map(n => s""""${node.name}" -> "$n"""" + labelStr).mkString("\n")
  }

  private def dotGraph(nodes: List[Node]): String = {
    val sb          = namedGraphBegin()
    val nodeStrings = nodes.map(nodeToDot)
    val edgeStrings = nodes.flatMap(n => n.outEdges.map(e => edgeToDot(n.name, e)))
    val isString    = nodes.map(isToDot)
    sb.append((nodeStrings ++ edgeStrings ++ isString).mkString("\n"))
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

  private def generateOutput(config: Config, fileName: String, nodes: List[Node]): Unit = {
    val outDotFile = config.outDir / s"$fileName.dot"
    val outSvgFile = config.outDir / s"$fileName.svg"
    if (outDotFile.exists) outDotFile.delete()
    if (outSvgFile.exists) outSvgFile.delete()
    outDotFile.write(dotGraph(nodes))
    createSvgFile(outDotFile, outSvgFile)
  }

  private def run(config: Config): Unit = {
    val jsonContent = loadFromFile(config.jsonFile)
    val json        = jsonFromString(jsonContent)

    config.outDir.createDirectoryIfNotExists()

    val allNodes = nodes(json)

    val filterList = config.selectedNodes match {
      case seq if seq.nonEmpty => seq
      case _                   => allNodes.map(_.name)
    }

    val candidates = allNodes
      .filter(n => filterList.contains(n.name))
      .flatMap { node =>
        val dependentNodes = if (config.resolve) {
          allNodes.filter { n =>
            val resNodes = node.outEdges.flatMap(_.inNodes)
            val isNodes  = node.is
            resNodes.contains(n.name) || isNodes.contains(n.name)
          }
        } else Nil
        node +: dependentNodes
      }
      .distinct

    if (config.saveIndividually) {
      candidates.foreach(c => generateOutput(config, c.name, List(c)))
    } else {
      generateOutput(config, config.jsonFile.nameWithoutExtension, candidates)
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
