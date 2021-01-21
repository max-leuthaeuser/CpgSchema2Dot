package io.shiftleft.cpgschema2dot.graph

case class Edge(label: String, inNodes: List[String], cardinality: String)
