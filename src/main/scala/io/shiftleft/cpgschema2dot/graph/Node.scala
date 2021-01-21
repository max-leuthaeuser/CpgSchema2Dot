package io.shiftleft.cpgschema2dot.graph

case class Node(name: String, keys: List[String], outEdges: List[Edge])
