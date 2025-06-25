##' Create a General Quarto Graphviz Flow Diagram from Plain Text With Variable Insertions
##'
##' 
##' Takes a character string or vector and creates a `graphviz` `dot` flowchart from the input.
##' There are two kinds of input strings or lines.  One defines nodes, the syntax being
##' `nodename:string` where `nodename` is any valid `graphviz` node name and `string` represents
##' a character string with possible variable value substitutions of the form `{{variablename}}`.
##' `string` is automatically wrapped by having newline characters inserted to keep lines
##' within `width` characters.  The second kind of input line is one specifying node
##' connections with `graphviz` `dot` syntax, respecting node names defined in earlier strings.
##' Unlike [makegvflow()], this approach allows a node to connect to any number of other nodes,
##' and allows any flow direction.
##'
##' If a node label contains `\\n`, all `\\n` are replaced with `\n` and ordinary word
##' wrapping is suppressed.  This is how users take control of line splitting.
##' If a node label ends in `//any text`, the text in `any text` will be copied 
##' to inside the node definition after the label is closed with double quote and
##' after a space is added.  This allows node colors and other attributes to be changed.
##'
##' Preceed single quotes in node labels with a single backslash.  Preceed double quotes with double backslashes.
##' 
##' The function uses [knitr::knit_expand()] to apply variable insertions before the diagram is rendered by Quarto.  See [this](https://hbiostat.org/rflow/doverview.html#sec-doverview-filter/) for an example.  ##' @title makedot
##' @param .object. character string or vector of plain text plus possible $\LaTeX$ math delimited by single dollar signs.  An empty initial line is ignore, so the user need not worry about having an initial quote mark on a line by itself.
##' @param direction direction of the flow chart, default is top-down
##' @param shape node shape
##' @param fontsize text font size
##' @param nodesetup extra node setup to include after `direction`
##' @param width text width for word-wrapping
##' @param ... name=value pairs that makes values replace `{{name}}` elements in the markup
##' @param file name of file to hold `graphviz` markup after variable insertions.  Run this in Quarto using a chunk to looks like what is below, which was for `file='graphviz.dot'`.
##' @param onlyprint set to `TRUE` to only print the generated `graphiz` markup and not save it to `file`
##'
##' ````
##' ```{dot}
##' //| label: fig-flow1
##' //| fig-height: 4
##' //| fig-cap: "Chart caption"
##' //| file: graphviz.dot
##' ```
##' ````
##' @return nothing; used to `knitr::knit_expand()` graphviz markup
##' @author Frank Harrell
##' @seealso [makegraphviz()], [makegvflow()]
##' @md
##' @examples
##' x <- '
##' A:Some string of text for {{jj}}
##' B:Another string of test, this one being longer and needing breaking
##' C:A third string defining a node//style="filled,rounded", fillcolor=lightblue
##' A -> B
##' B -> C [label="some text along connecting arc"]
##' C -> A'
##' # Can also use A -> {B, C}
##' makedot(x, jj='tiger', onlyprint=TRUE)

makedot <- function(.object., ..., direction=c('TD', 'LR'),
  shape='box', fontsize=24, nodesetup=NULL, file, width=30, onlyprint=FALSE) {
  direction <- match.arg(direction)

  x <- .object.
  if(length(x) == 1) x <- strsplit(x, '\n')[[1]]
  if(trimws(x[1]) == '') x <- x[-1]

  for(i in 1 : length(x)) {
    y <- x[i]
    if(grepl('^[[:alnum:]\\._]+:', y)) {
      n <- sub(':.*$',  '', y)
      w <- sub('^.*?:', '', y)
      # If line has forced line breaks with \\n, change these to real
      # \n link breaks and don't do any automatic word wrapping
      newline_manual <- grepl('\\n', w, fixed=TRUE)
      if(newline_manual) w <- gsub('\\n', '\n', w, fixed=TRUE)
      extranode_info <- ''
      if(grepl('.*//.*', w)) {
        extranode_info <- paste0(', ', sub('.*//', '', w))
        w <- sub('//.*', '', w)
      }
      if(! newline_manual)
        w <- sapply(strwrap(w, width, simplify=FALSE), paste, collapse='\n')
      x[i] <- paste0(n, '[label="', w, '"', extranode_info, ']')
    }
  }

  head <- paste0('digraph {rankdir =', direction, ';',
            if(length(nodesetup)) paste0('\n', nodesetup, ';'),
            '\nnode [shape=', shape,
                 ', fontsize=', fontsize, '];')
  x <- c(head, x)

  # Close digraph{
  x <- c(x, '}')
  
  if(onlyprint) {
    cat(x, sep='\n')
    return(invisible())
  }

  ki <- knitr::knit_expand
  dotlist <- list(...)
  etext <- do.call('ki', c(list(text=x), dotlist))
  cat(etext, sep='\n', file=file)
  invisible()
}
