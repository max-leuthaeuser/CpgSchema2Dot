package io.shiftleft.cpgschema2dot.graph

case class NodeKey(name: String, valueType: String = "", cardinality: String = "") {
  override def toString: String =
    if (valueType.isEmpty) {
      s"$name $cardinality"
    } else if (cardinality.isEmpty) {
      s"$name: $valueType"
    } else {
      s"$name: $valueType $cardinality"
    }
}
