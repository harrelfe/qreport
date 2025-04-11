##' Create a Quarto Graphviz Flow Diagram from Plain Text With Variable Insertions
##'
##' 
##' Takes a character string or vector and creates a `graphviz` flowchart from the
##' hierarchy indicated by the number of indended spaces in each line.  The text
##' for the root levels is not indented, and major, minor, and tiny levels are indented
##' 2, 4, or 6 spaces, respectively.  Default color, font size, and fill color are
##' chosen so that the chart is suitable for presentations.  Simple $\LaTeX$ math
##' markup is converted to simple HTML markup using [tth::tth()].  Text lines
##' are automatically wrapped to keep text boxes from being too wide.  Text lines
##' beginning with "+" are combined with the previous major, minor, or tiny text line
##' but separated by a double line break (single break if `lbdouble=FALSE`).
##' 
##' The function uses [knitr::knit_expand()] to apply variable insertions before the diagram is rendered by Quarto.  See [this](https://hbiostat.org/rflow/doverview.html#sec-doverview-filter/) for an example.  ##' @title makegvflow
##' @param .object. character string or vector of plain text plus possible $\LaTeX$ math delimited by single dollar signs.  An empty initial line is ignore, so the user need not worry about having an initial quote mark on a line by itself.
##' @param direction direction of the flow chart, default is top-down
##' @param style node style
##' @param shape node shape
##' @param font font for text in nodes
##' @param fontsize text font size
##' @param fontcolor text color
##' @param fillcolor node fill color
##' @param penwidth thickness of lines for node borders
##' @param arrowcolor arrow color
##' @param arrowsize arrow size
##' @param width text width for word-wrapping
##' @param lbdouble set to `FALSE` to use a single line break for "+" lines
##' @param extracon one or more text strings specifying extra connections between nodes using node names `nijk` for major level `i`, minor level `j`, tiny level `k` (as many of these that are applicable).  For example specify `extracon=c('n1 -> n2', 'n21 -> n31')`. 
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
##' @seealso [makegraphviz()]
##' @md
##' @examples
##' x <- '
##' Root text
##'   Major 1
##'     Minor 11 {{jj}}
##'     Minor 12
##'   Major 2
##'     Minor 21
##'     Minor 22
##'     Minor 23 that is very very long and just keeps going
##'       tiny 231 and $\\alph + \\sum_{i=1}^{n}X_{i}$
##'       tiny 232
##'       + a second line for tiny 232 that is pretty long
##'       + a third line for tiny 232
##'   Major 3
##'     Minor 31
##'       tiny 311'
##' makegvflow(x, extracon='n12 -> n21', jj='tiger', onlyprint=TRUE)

makegvflow <- function(.object., ..., direction=c('TD', 'LR'),
  style='filled', shape='box', font='Times-Roman',
  fontsize=18, fontcolor='blue', fillcolor='azure', penwidth=0.1,
  arrowcolor='blue3', arrowsize=0.7, width=30, lbdouble=TRUE,
  extracon=NULL, file, onlyprint=FALSE) {

  direction <- match.arg(direction)

  # Define line break markup and label delimiters
  mu <- c('html', 'text')[1]
  if(mu == 'html') {
    br     <- '<br></br>'
    ldelim <- '<'
    rdelim <- '>'
  } else {
    br     <- '\n'
    ldelim <- '"'
    rdelim <- '"'
  }

  x <- .object.
  if(length(x) == 1) x <- strsplit(x, '\n')[[1]]
  if(trimws(x[1]) == '') x <- x[-1]
  
  # Rank of each line is the number of indented spaces divided by 2
  # A line whose first non-whitespace character is + is treated as a 
  # continuation of the previous line separated by 2 linebreaks.
  # Such lines are recombined with an earlier line before computing ranks.

  y <- character(0)
  for(i in 1 : length(x)) {
    xi <- trimws(x[i])
    if(grepl('`', xi)) stop('backtick not allowed in text')
    ly <- length(y)
    if(substring(xi, 1, 1) == '+') {
      if(ly == 0) stop('continuation line starting with + not allowed in first line of text')
      # Backtick as separator will not badly distort character counts in word wrapping
      y[ly] <- paste(y[ly], trimws(substring(xi, 2)), sep='`')
    } else y <- c(y, x[i])
  }
  x <- y

  g <- function(y) {
    w <- substring(y, 1 : nchar(y), 1 : nchar(y))
    j <- rle(w)
    r <- if(j$values[1] == ' ') j$lengths[1] / 2 else 0
    if(floor(r) != r)
      stop('must have an even number of indented spaces on the left of strings') 
    r
  }
  r <- sapply(x, g)
  if(r[1] != 0 || sum(r == 0) > 1)
    stop('first line of text must not be indented and must be the only such line')

  # Convert LaTeX expressions set off by $...$ to simple HTML
  # tth will combine lines, so do each line separately
  if(requireNamespace('tth', quietly = TRUE))
    for(i in 1 : length(x)) x[i] <- tth::tth(x[i])
  
  # Insert line breaks to wrap text
  x <- sapply(strwrap(x, width, simplify=FALSE), paste, collapse=br)

  # Finally expand backtick to double or single line break
  x <- gsub('`', if(lbdouble) paste0(br, br) else br, x)

  # Example layout and node names assigned
  #
  # r (rank)
  # 0 Root text           R
  # 1   Major branch      n1
  # 2     Minor branch    n11
  # 2     Minor branch    n12
  # 1   Major branch      n2
  # 2     Minor branch    n21
  # 2     Minor branch    n22
  # 2     Minor branch    n23
  # 3       Tiny branch   n231
  # 3       Tiny branch   n232
  # 1   Major branch      n3
  # 2     Minor branch    n31
  # 3       Tiny branch   n311
  #
  # With connections R -> {n1, n2, n3}, n1 -> {n11, n12}, n2 -> {n21, n22, n23},
  #                  n23 -> {n231, n232}, n3 -> n31; n31 -> n311

  # Compute major branch numbers = cumulative number of lines of rank=1
  M <- cumsum(r == 1)
  # Compute minor branch numbers (0 for major branch lines)
  f <- function(a) ifelse(a, sequence(rle(a)$lengths), 0)
  m <- f(r >= 2) - f(r >= 3)
  # Compute tiny branch numbers (0 for major or minor branch lines)
  t <- f(r >= 3)

  head <- paste0('digraph {
    rankdir =', direction, ';
    node [style=', style, ', shape=', shape,
      ', fontname="', font, '", fontsize=', fontsize,
      ', fontcolor=', fontcolor, ', fillcolor=', fillcolor, 
      ', penwidth=', penwidth, '];
    edge [color=', arrowcolor, ', arrowsize=', arrowsize, '];
    R[label=<', x[1], '>]')
  x[1] <- head

  # Translate major, minor, tiny integers to a string
  s <- ifelse(r == 1, M,
              ifelse(r == 2, paste0(M, m),
                     ifelse(r == 3, paste0(M, m, t), '')))
  for(j in 2 : length(x))
    x[j] <- paste0('n', s[j], '[label=', ldelim, x[j], rdelim, ']')

  # Add connections
  # From root to majors
  iM <- which(r == 1)
  d <- paste0('R -> {', paste('n', sequence(length(iM)), sep='', collapse=', '), '}')
  # From majors to minors
  for(i in sequence(length(iM))) {
    j <- which(M == i & m != 0 & t == 0)  # row numbers of minors to current major
    if(length(j)) {
      d <- c(d, paste0('n', i, ' -> {', paste('n', i, m[j], sep='', collapse=', '), '}'))
      # From minors to tiny
      for(k in j) {
        l <- which(M == i & m == m[k] & t != 0)
        if(length(l)) {
          d <- c(d, paste0('n', i, m[k], ' -> {',
                           paste('n', i, m[k], t[l], sep='', collapse=', '), '}'))
        }
      }
    }
  }

  # Add any extra connections and close digraph{
  x <- c(x, d, extracon, '}')
  
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
