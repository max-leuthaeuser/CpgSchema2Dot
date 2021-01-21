package io.shiftleft.cpgschema2dot.dot

import better.files.File
import io.shiftleft.cpgschema2dot.graph.Edge
import io.shiftleft.cpgschema2dot.graph.Node
import io.shiftleft.cpgschema2dot.Config

import scala.sys.process.Process
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class DotWriter(private val config: Config) {

  def saveOutput(fileName: String, nodes: List[Node], edgeFilterList: List[String]): Unit = {
    config.outDir.createDirectoryIfNotExists()
    val outDotFile = config.outDir / s"$fileName.dot"
    val outSvgFile = config.outDir / s"$fileName.svg"
    if (outDotFile.exists) outDotFile.delete()
    if (outSvgFile.exists) outSvgFile.delete()
    outDotFile.write(dotGraph(nodes, edgeFilterList))
    createSvgFile(outDotFile, outSvgFile)
    println(s"Saved '$outSvgFile' successfully.")
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

}
