package io.shiftleft.cpgschema2dot

import better.files.File
import io.shiftleft.cpgschema2dot.Config._

object Config {
  val DEFAULT_BASE_JSON         = "base.json"
  val DEFAULT_OUT_DIR           = "out"
  val DEFAULT_RESOLVE           = false
  val DEFAULT_SAVE_INDIVIDUALLY = false
  val DEFAULT_NO_NODE_KEYS      = false
}

case class Config(jsonFile: File = File(DEFAULT_BASE_JSON),
                  outDir: File = File(DEFAULT_OUT_DIR),
                  selectedNodes: List[String] = List.empty,
                  selectedEdges: List[String] = List.empty,
                  saveIndividually: Boolean = DEFAULT_SAVE_INDIVIDUALLY,
                  resolve: Boolean = DEFAULT_RESOLVE,
                  noNodeKeys: Boolean = DEFAULT_NO_NODE_KEYS) {
  override def toString: String = s"""
       |Current configuration:
       |\t-Input json file: $jsonFile
       |\t-Output directory: $outDir
       |\t-Selected node types: ${selectedNodes.mkString(", ")}
       |\t-Selected edge types: ${selectedEdges.mkString(", ")}
       |\t-Save individual images: $saveIndividually
       |\t-Resolve all direct neighbours: $resolve
       |\t-Draw node keys: ${!noNodeKeys}
       |""".stripMargin
}
