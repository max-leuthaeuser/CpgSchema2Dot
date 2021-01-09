# CpgSchema2Dot

Prints our codepropertygraph schema via dot as SVG.

# Installation

Have `dot` available on the command-line (you may need to install `graphviz`).

You can build `CpgSchema2Dot` by running the command below.

``` bash
sbt stage
```

# Invoking
After running `CpgSchema2Dot` by invoking `./CpgSchema2Dot.sh` you should be able to see the
output below.

``` bash
Error: Missing argument <path-to-json>

Try --help for more information.

Usage: CpgSchema2Dot [options] <path-to-json>

  --help
        prints this usage text
  <path-to-json>
        path to the json input file
  --selected-nodes <node name 1>,<node name 2>,...,<node name n>
        node names to select for .dot generation; will use all kind of nodes if empty
  --out-dir <value>
        output directory (defaults to `out`)
  --resolve
        enables resolving attributes and edges for all direct neighbours of the selected nodes
  --save-individually
        generate individual files for each selected node (see selected-nodes)
```

`CpgSchema2Dot` requires at least one argument `<path-to-json>`. This is the path
to the .json schema file from which you would like to generate from.

# Limiting the Output
Restricting the generation to certain node types can be achieved using the `--selected-nodes` argument. E.g.,:
`./CpgSchema2Dot.sh path/to/base.json --selected-nodes METHOD,BLOCK`.
