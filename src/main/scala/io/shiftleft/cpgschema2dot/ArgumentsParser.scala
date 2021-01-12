package io.shiftleft.cpgschema2dot

import better.files.File
import scopt.OptionParser

object ArgumentsParser {
  val HELP: String              = "help"
  val PATH_TO_JSON: String      = "<path-to-json>"
  val SELECTED_NODES: String    = "selected-nodes"
  val OUT_DIR: String           = "out-dir"
  val RESOLVE: String           = "resolve"
  val SAVE_INDIVIDUALLY: String = "save-individually"
  val NO_NODE_KEYS: String      = "no-nodekeys"
}

class ArgumentsParser {

  import ArgumentsParser._

  private val parser: OptionParser[Config] = new OptionParser[Config]("CpgSchema2Dot") {
    help(HELP).text("prints this usage text")
    arg[String](PATH_TO_JSON)
      .required()
      .text("path to the json input file")
      .action((x, c) => c.copy(jsonFile = File(x)))
      .validate(path => {
        val f = File(path)
        if (f.exists()) success
        else failure(s"Invalid $PATH_TO_JSON path: '$path'. File does not exist!")
      })
    opt[Seq[String]](SELECTED_NODES)
      .valueName("<node name 1>,<node name 2>,...,<node name n>")
      .action((x, c) => c.copy(selectedNodes = x))
      .text("node names to select for .dot generation; will use all kind of nodes if empty")
    opt[String](OUT_DIR)
      .text(s"output directory (defaults to `${Config.DEFAULT_OUT_DIR}`)")
      .action((x, c) => c.copy(outDir = File(x)))
      .validate(x =>
        if (x.isEmpty) {
          failure("Output dir path cannot be empty")
        } else if (File(x).canonicalFile.parentOption.isEmpty) {
          failure("Parent directory of the output directory does not exist")
        } else success)
    opt[Unit](RESOLVE)
      .text(
        s"enables resolving attributes and edges for all direct neighbours of the selected nodes")
      .action((_, c) => c.copy(resolve = true))
    opt[Unit](SAVE_INDIVIDUALLY)
      .text(s"generate individual files for each selected node (see $SELECTED_NODES)")
      .action((_, c) => c.copy(saveIndividually = true))
    opt[Unit](NO_NODE_KEYS)
      .text(s"disables printing of node keys")
      .action((_, c) => c.copy(noNodeKeys = true))
  }

  def parse(args: Array[String]): Option[Config] =
    parser.parse(args, Config())

  def showUsage(): Unit = println(parser.usage)

}
