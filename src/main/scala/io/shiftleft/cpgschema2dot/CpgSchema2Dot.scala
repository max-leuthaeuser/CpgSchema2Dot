package io.shiftleft.cpgschema2dot

import io.shiftleft.cpgschema2dot.dot.DotWriter
import io.shiftleft.cpgschema2dot.graph._
import io.shiftleft.cpgschema2dot.json.JsonLoader

object CpgSchema2Dot {

  private def nodesInDir(dir: Direction,
                         node: Node,
                         allNodes: List[Node],
                         edgeFilterList: List[String]): List[Node] = dir match {
    case In =>
      allNodes.filter { n =>
        val resNodes =
          n.outEdges.filter(e => edgeFilterList.exists(e.label.contains)).flatMap(_.inNodes)
        resNodes.contains(node.name)
      }
    case Out =>
      allNodes.filter { n =>
        val resNodes =
          node.outEdges.filter(e => edgeFilterList.exists(e.label.contains)).flatMap(_.inNodes)
        resNodes.contains(n.name)
      }
  }

  private def buildNodeFilterList(config: Config, allNodes: List[Node]): List[String] =
    config.selectedNodes match {
      case seq if seq.nonEmpty => seq
      case _                   => allNodes.map(_.name)
    }

  private def buildEdgeFilterList(config: Config, allNodes: List[Node]): List[String] =
    config.selectedEdges match {
      case seq if seq.nonEmpty => seq
      case _                   => allNodes.flatMap(_.outEdges.map(_.label))
    }

  private def filterNodes(allNodes: List[Node], nodeFilterList: List[String]): List[Node] =
    allNodes.filter(n => nodeFilterList.contains(n.name))

  private def traverseNodes(node: Node,
                            allNodes: List[Node],
                            edgeFilterList: List[String]): (Node, List[Node]) = {
    val outNodes = nodesInDir(Out, node, allNodes, edgeFilterList)
    node -> outNodes.map(_.copy(outEdges = List.empty))
  }

  private def traverseNodesAndResolve(node: Node,
                                      allNodes: List[Node],
                                      edgeFilterList: List[String]): (Node, List[Node]) = {
    val inNodes = nodesInDir(In, node, allNodes, edgeFilterList).map { n =>
      n.copy(outEdges = n.outEdges.filter(e => e.inNodes.contains(node.name)))
    }
    val outNodes       = nodesInDir(Out, node, allNodes, edgeFilterList)
    val dependentNodes = outNodes.map(_.copy(outEdges = List.empty)) ++ inNodes
    val remainingNodes = allNodes
      .filter { n =>
        val l = dependentNodes.flatMap(n => n.outEdges.flatMap(_.inNodes))
        node.name != n.name && !dependentNodes.exists(_.name == n.name) && l.contains(n.name)
      }
      .map(_.copy(outEdges = List.empty))
    node -> (dependentNodes ++ remainingNodes)
  }

  private def run(config: Config): Unit = {
    println(config.toString)

    val jsonLoader   = new JsonLoader()
    val graphBuilder = new GraphBuilder(config, jsonLoader)
    val dotWriter    = new DotWriter(config)

    val json           = jsonLoader.loadFromFile(config.jsonFile)
    val allNodes       = graphBuilder.nodes(json)
    val nodeFilterList = buildNodeFilterList(config, allNodes)
    val edgeFilterList = buildEdgeFilterList(config, allNodes)

    val candidates = filterNodes(allNodes, nodeFilterList)
      .map {
        case node if config.resolve =>
          traverseNodesAndResolve(node, allNodes, edgeFilterList)
        case node =>
          traverseNodes(node, allNodes, edgeFilterList)
      }

    if (config.saveIndividually) {
      candidates.foreach { case (k, v) => dotWriter.saveOutput(k.name, k +: v, edgeFilterList) }
    } else {
      dotWriter.saveOutput(config.jsonFile.nameWithoutExtension,
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
