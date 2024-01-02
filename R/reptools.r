##' Print Named List of Vectors
##'
##' Function to print a simple named list of vectors in html
##' Creates a column `name` from the names of the list
##' If a vector element of the list is numeric, it is rounded
##' to `dec` digits to the right of the decimal place.
##' @title htmlList
##' @param x a named list
##' @param dec round to this decimal place
##' @return a `kable`
##' @author Frank Harrell
##' @md
##' @examples
##' set.seed(1)
##' w <- list(A = runif(4), B=rnorm(3))
##' htmlList(w)
htmlList <- function(x, dec=4) {
  g <- function(z)
	  if(is.character(z)) paste(z,             collapse=', ') else
	                      paste(round(z, dec), collapse=', ')
  w <- sapply(x, g)
	d <- data.frame(name=names(w), value=w)
	rownames(d) <- NULL
	kabl(d, col.names=NULL)
	}


##' Front-end to `kable` and `kables`
##'
##' Calls `kable()` if only one table is to be printed.
##' Calls `kable()` for each table and passes it to `kables` if more than one.
##' Accounts for results of `tapply` not being a vector (is an array)
##'
##' @title kabl
##' @param ... one or more objects to pass to `kable`
##' @param caption overall single caption
##' @param digits passed to `kable` and applies to all tables
##' @param col.names passed to `kable`
##' @param row.names passed to `kable`
##' @return result of `kable` or `kables`
##' @author Frank Harrell
##' @md
##' @examples
##' kabl(data.frame(a=1:2, b=3:4), data.frame(x=11:13, y=21:23))
kabl <- function(..., caption=NULL, digits=4, col.names=NA, row.names=NA) {
  w <- list(...)
  tr <- function(x)
    if(is.vector(x) || (is.array(x) && length(dim(x)) == 1)) t(x) else x
  format <- if(knitr::is_latex_output()) 'latex' else 'pipe'
  if(length(w) == 1) {
    w <- w[[1]]
    return( knitr::kable(tr(w), digits=digits, caption=caption,
                         col.names=col.names, row.names=row.names,
                         format=format))
  }
  w <- lapply(w, function(x) knitr::kable(tr(x), digits=digits, format=format))
  knitr::kables(w, caption=caption, format=format)
}

##' Create Text for Running Code Chunk
##'
##' Creates text strings suitable for running through `knitr`.  The chunk is given a random name because certain operations are not allowed by `knitr` without it.
##' @title makecodechunk
##' @param cmd character string vector of commands to run inside chunk
##' @param opts optional list of chunk options, e.g. `list(fig.width=6, fig.cap="This is a caption")`.  See <https://yihui.org/knitr/options/> for a complete list of options.
##' @param results format of results, default is `'asis'`.  May specify `results='markup'`.
##' @param lang language for the chunk
##' @param callout an optional Quarto callout to include after `#|` after the chunk header that affects how the result appears, e.g. `callout='column: margin'`
##' @param h optional height to place after the chunk header after `#|`
##' @param w optional width
##' @return character vector 
##' @author Frank Harrell
##' @md
##' @examples
##' makecodechunk('x <- pi; print(x)')
makecodechunk <- function(cmd, opts=NULL, results='asis', lang='r',
                          callout=NULL, h=NULL, w=NULL) {
  if(! length(cmd) || (is.character(cmd) && length(cmd) == 1 &&
            cmd %in% c('', ' ', "` `"))) return('')

  r <- paste0('results="', results, '"')
  if(length(opts))
    for(oname in names(opts)) {
      op <- opts[[oname]]
      if(is.character(op)) op <- paste0('"', op, '"')
      r <- paste0(r, ',', oname, '=', op)
      }
  
  if(! exists('.chunknumber.')) .chunknumber. <- 0
  .chunknumber. <- .chunknumber. + 1
  ge <- .GlobalEnv
  assign('.chunknumber.', .chunknumber., envir=ge)
  cname <- paste0('chnk', .chunknumber.)
  if(length(callout)) callout <- paste('#|', callout)
  if(length(h)) h <- paste('#| fig.height:', h)
  if(length(w)) w <- paste('#| fig.width:', w)
  c('',
    if(lang == 'r') paste0('```{r ', cname, ',', r, ',echo=FALSE}')
    else            paste0('```{', lang, ' ', cname, '}'),
    callout, cmd, h, w, '```', '')
    }

##' General Case Handling of Quarto Callouts
##'
##' This function generates and optionally runs markdown/R code that runs Quarto callouts such as collapsible notes or marginal notes.  Before rendering `x`, `options(rawmarkup=TRUE)` is set so that `Hmisc::rendHTML` will not try to protect html in things like margins.  Quarto doesn't like the surrounding html protection lines in that context.  The option is set back to its original value after rendering.
##' @title makecallout
##' @param ... can be any of the following
##' * x object to print (if `type='print'`), or one or more formulas whose right hand sides are to be run.  Left side provides labels if needed by the particular callout, and if `raw` is included on the right side any R code chunk run will have `results='asis'` in the chunk header.
##' * callout character string giving the Quarto callout
##' * label character string label if needed and if not obtained from the left side of a formula
##' * type defaults to `'print'` to print an object.  Set to `'run'` to run a chunk or `'cat'` to use `cat()` to render.
##' * now set to `FALSE` to return code instead of running it
##' * results if not using formulas, specifies the formatting option to code in the code header, either `'asis'` (the default) or `'markup'`
##' * close specifies whether to close the callout or to leave it open for future calls
##' * parameters passed to `print`
##' @return if code is not executed, returns a character vector with the code to run
##' @md
##' @author Frank Harrell
##' @examples
##' x <- 1:3
##' co <- '.callout-note collapse="true'
##' makecallout(x, callout=co, label='# thislabel', type='print')
##' makecallout(thislabel ~ x, callout=co, type='print')
makecallout <- function(...) {

  ## Define internal function to keep new variables from clashing
  ## with knitted code

  build <- function(x, callout=NULL, label=NULL,
                    type=NULL, now=TRUE, results='asis',
                    close=length(callout), ...) {
  if(! length(type)) type <- 'print'
  k  <- character(0)

  if('formula' %in% class(x)) {
    v         <- as.character(attr(terms(x), 'variables'))[-1]
    if(! attr(terms(x), 'response')) {  # no left side variable
      label <- NULL
      x     <- v
      } else {
        ## Get left hand side and remove leading/training backticks
        left  <- sub('`$', '', sub('^`', '', v[1]))
        label <- paste('##', left)  # left-hand side
        x     <- v[-1]              # right-hand side
        }
    raw       <- 'raw' %in% x
    if(raw) x <- setdiff(x, 'raw')
    type      <- 'run'
    now       <- TRUE
    results   <- if(raw) 'markup' else 'asis'
  }

  if(length(callout))
    k <- c('', paste0('::: {', callout, '}'),
           if(! length(label)) '', label)

  oraw <- options('rawmarkup')  # used by Hmisc::rendHTML
  on.exit(options(oraw))
  options(rawmarkup=TRUE)
  res <- if(is.character(x) && length(x) == 1 &&
            all(x %in% c('', ' ', "` `"))) ' '
         else
           if(type == 'print') capture.output(print(x, ...))
         else
           if(type == 'cat') x
         else
           makecodechunk(x, results=results)

  k <- c(k, res, if(close) c(':::', ''))
  list(k=k, now=now, type=type)
  }
  .z. <- build(...)
  .k. <- .z.$k
  if(.z.$now) {
    switch(.z.$type,
           print = cat(.k., sep='\n'),  # already print()'d
           cat   = cat(.k., sep='\n'),
           run   = cat(knitr::knit(text=knitr::knit_expand(text=.k.),
                                   quiet=TRUE)))
    return(invisible())
  }
  .k.
}

##' Print an Object in a Collapsible Note
##'
##' Prints an object in a Quarto collapsible note.
##' @title makecnote
##' @param x an object having a suitable `print` method
##' @param label a character string providing a title for the tab.  Default is the name of the argument passed to `makecnote`.
##' @param wide set to `TRUE` to expand the width of the text body
##' @param type default is to `print`; can also be `run`, `cat`
##' @param ... an optional list of arguments to be passed to `print`
##' @return nothing is returned, used for rendering markup
##' @author Frank Harrell
##' @md
##' @examples
##' makecnote('This is some text', label='mylab', wide=TRUE)
makecnote <- function(x,
                      label=paste0('`', deparse(substitute(x)), '`'),
                      wide=FALSE,
                      type=c('print', 'run', 'cat'),
                      ...) {
  type <- match.arg(type)
  co <- paste('.callout-note', if(wide) '.column-page', 'collapse="true"')
  makecallout(x, callout=co,
              label=paste('#', label), type=type, ...)
  invisible()
}

##' Put an Object in the Margin
##'
##' Prints an object in a Quarto column margin.
##' @title makecolmarg
##' @param x an object having a suitable `print` method
##' @param type type of execution
##' @param ... an optional list of arguments to be passed to `print`
##' @return nothing is returned, used to render markup
##' @author Frank Harrell
##' @md
##' @examples
##' makecolmarg(data.frame(x=1:3, y=4:6))
makecolmarg <- function(x, type=c('print', 'run', 'cat'), ...) {
  type <- match.arg(type)
  makecallout(x, callout='.column-margin', type=type, ...)
  invisible()
}

##' Make Quarto Tabs
##'
##' Loops through a series of formulas or elements of a named list and outputs each element into
##' a separate `Quarto` tab.  `wide` and `column` arguments are used to expand the width
##' of the output outside the usual margins.  An `initblank` argument
##' creates a first tab that is empty, or you can specify a formula `` `` ~ `` ``.  This allows one to show nothing
##' until one of the other tabs is clicked.  Multiple commands can be run in one chunk by including multiple right hand terms in a formula.  A chunk can be marked for producing raw output by including a term `raw` somewhere in the formula's right side.  If can be marked for constructing a label and caption by including `+ caption(caption string, label string)`.  The tab number is appended to the label string, and if the label is not provided `baselabel` will be used.
##' @title maketabs
##' @param ... a series of formulas or a single named list.  For formulas the left side is the tab label (if multiple words or other illegal R expressions enclose in backticks) and the right hand side has expressions to evaluate during chunk execution, plus optional `raw`, `caption`, and `fig.size` options.
##' @param wide set to `TRUE` to use a Quarto `column-page` for the body of the text to allow it to use some of the margins
##' @param cwidth specify a legal `Quarto` character string instead of `wide` to specify the width of the output.  These are defined [here](https://quarto.org/docs/authoring/article-layout.html#options-reference/).  Commonly used values are `'column-screen-right'`, `'column-page-left'`, `'column-screen-inset-shaded'`. 
##' @param initblank set to `TRUE` to create a first tab that is blank so that the report will not initially show any tabbed material
##' @param baselabel a one-word character string that provides the base name of `label`s for tabs with figure captions.  The sequential tab number is appended to `baselabel` to obtain the full figure label.  If using formulas the figure label may instead come from `caption(.., label)`. If not specified it is taken to be the name of the current chunk with `fig-` prepended.
##' @param cap applies to the non-formula use of `maketabs` and is an integer vector specifying which tabs are to be given figure labels and captions.
##' @param basecap a single character string providing the base text for captions if `cap` is specified.
##' @param debug set to `TRUE` to output debugging information in file `/tmp/z`
##' @return nothing is returned; used to render markup
##' @author Frank Harrell
##' @md
##' @examples
##' X <- list(A=data.frame(x=1:2), B=data.frame(x=1:2, y=11:12))
##' maketabs(X)
# See https://stackoverflow.com/questions/42631642/
maketabs <- function(..., wide=FALSE, cwidth=if(wide) 'column-page',
                     initblank=FALSE,
                     baselabel=NULL, cap=NULL, basecap=NULL, debug=FALSE) {

  ## Put caption() and fig.size() in parent environment so they can
  ## be executed in that environment so that their argument values
  ## may be found there
  
  en <- parent.frame()
  assign(envir = en, 'caption',
         function(cap, label=NULL) list(label=label, cap=cap)  )
  assign(envir = en, 'fig.size',
         function(width=NULL, height=NULL, column=NULL)
           list(width=width, height=height, column=column)     )

  
  fs <- list(...)
  if(length(fs) == 1 && 'formula' %nin% class(fs[[1]])) {
    fs <- fs[[1]]   # undo list(...) and get to 1st arg to maketabs
    ge <- .GlobalEnv
    assign('.fs.', fs, envir=ge)
  }

  if(! length(baselabel))
    baselabel <- knitr::opts_current$get('label')
  else if(baselabel == 'none') baselabel <- NULL
  if(length(baselabel) && ! grepl('^fig-', baselabel))
      baselabel <- paste0('fig-', baselabel)
  
  yaml   <- paste0('.panel-tabset',
                   if(length(cwidth)) paste0(' .', cwidth))

    k <- c('', paste0('::: {', yaml, '}'), '')
    if(initblank) k <- c(k, '', '##   ', '')

    for(i in 1 : length(fs)) {
      label <- baselabel
      capt  <- NULL
      size  <- NULL
      f <- fs[[i]]
      isform <- FALSE
      if('formula' %in% class(f)) {
        isform <- TRUE
        capt   <- NULL
        v      <- as.character(attr(terms(f), 'variables'))[-1]
        y      <- v[1]   # left-hand side
        y   <- gsub('`', '', y)
        x   <- v[-1]  # right-hand side
        raw       <- 'raw' %in% x
        if(raw) x <- setdiff(x, 'raw')
        ## process caption(..., ...)
        jc <- grep('caption\\(', x)
        if(length(jc)) {
          capt <- eval(parse(text=x[jc]), en)
          if(length(capt$label)) label <- capt$label
          capt   <- capt$cap
          x <- x[- jc]
        }
        ## process fig.size(...)
        sz <- grep('fig.size\\(', x)
        if(length(sz)) {
          siz <- eval(parse(text=x[sz]), en)
          if(length(siz$width))
            size <- paste('fig-width:', siz$width)
          if(length(siz$height))
            size <- c(size, paste('fig-height:', siz$height))
          if(length(siz$column))
            size <- c(size, paste('column:', siz$column))
          x <- x[- sz]
          }
      } else {
        raw  <- FALSE
        y    <- names(fs)[i]
        x    <- paste0('.fs.[[', i, ']]')
        if(i %in% cap) capt <- basecap
      }
      r <- paste0('results="',
                  if(raw) 'markup' else 'asis',
                  '"')
      callout <- NULL
      if(length(label) && length(capt)) {
        lab <- paste0(label, i)
        callout <- c(paste0('label: ', lab),
                     paste0('fig-cap: "',  capt, '"'))
        addCap(lab, capt)
      }
      if(length(size)) callout <- c(callout, size)
      
      k <- c(k, '', paste('##', y), '',
             makecodechunk(x, callout=callout,
                           results=if(raw) 'markup' else 'asis'))
    }
    k <- c(k, ':::', '')

  if(debug) cat(k, sep='\n', file='/tmp/z', append=TRUE)
  cat(knitr::knit(text=k, quiet=TRUE))
  return(invisible())
  }


##' Convert Objects to HTML and View
##'
##' Converts a series of objects created to html.
##' Displays these in the RStudio View pane.
##' If RStudio is not running displays in an external browser.
##' Assumes there is an `html` method for the objects (e.g., objects
##' are result of `Hmisc::describe` or `Hmisc::contents`.
##' User can page through the different outputs with the arrow keys
##' in the RStudio View pane
##' @title htmlView
##' @param ... any number of objects for which an `html` method exists
##' @return nothing is returned; used to launch a browser on html text
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##'  htmlView(contents(d1), contents(d2))
##'  htmlView(describe(d1), describe(d2, descript='Second Dataset'))
##'  htmlView(contents(d), describe(d))
##' }
htmlView <- function(...) {
  viewer <- getOption('viewer', default=utils::browseURL)
  w <- list(...)
  td <- tempdir()
  if(! dir.exists(td)) dir.create(td)
  for(u in w) {
    fi <- paste0(tempfile(), '.html')
    h  <- html(u)
    writeLines(as.character(h), fi)
    viewer(fi)
  }
}

##' Convert to HTML and Eternally View Objects
##'
##' `htmlViewx` is similar to `htmlView` except that an external viewer is
##' launched, and the first object is opened in a new window.
##' Subsequent objects are opened in a new tab in the last created
##' window.  Set `options(vbrowser='command line to run browser')`
##' to use a browser other than `Vivaldi`.
##' Defaults to opening a new window for only the first object, and adding
##' tabs after that.
##' @title htmlViewx
##' @param ... a series of objects for which an `html` method exists
##' @param tab set to `'all'` to add even the first object to an existing window.
##' @return does not return a value; launches a browser
##' @examples
##' \dontrun{
##' options(prType='html')
##' htmlViewx(contents(d), describe(d))
##' }
##' @author Frank Harrell
htmlViewx <- function(..., tab=c('notfirst', 'all', 'none')) {
  tab <- match.arg(tab)
  view <- getOption('vbrowser')
  if(! length(view)) view <- 'vivaldi'
  w <- list(...)
  td <- tempdir()
  if(! dir.exists(td)) dir.create(td)

  i <- 0
  for(u in w) {
    i <- i + 1
    fi <- paste0(tempfile(), '.html')
    h  <- html(u)
    writeLines(as.character(h), fi)
    cmd <- paste0(view,
                  switch(tab,
                         all  = ' -new-tab',
                         none = ' -new-window',
                         notfirst = if(i == 1) ' -new-window' else ' -new-tab' ))
    browseURL(fi, browser=cmd)
  }
}

##' Run a Series of Data Checks and Report
##'
##' Function to run various data checks on a data table.
##'
##' Checks are run separately for each part of the `expression` vector `checks`.  For each single expression, the variables listed in the output are all the variables mentioned in the expression plus optional variables whose names are in the character vector `id`. `%between% c(a,b)` in expressions is printed as `[a,b]`.  The output format is plain text unless `html=TRUE` which also puts each table in a separate Quarto tab.  See [here](https://www.fharrell.com/post/rflow/) for examples.
##' @title dataChk
##' @param d a data table
##' @param checks a vector of expressions that if satisfied causes records to be listed
##' @param id option vector of variable names to serve as IDs
##' @param html set to `TRUE` to create HTML output and put each check in a separate tab, also creating summary tabs
##' @param omit0 set to `TRUE` to ignore checks finding no observations
##' @param byid if `id` is given set `byid=TRUE` to also list a data frame with all flagged conditions, sorted by `id` 
##' @param nrows maximum number of rows to allow to be printed 
##' @return an invisible data frame containing variables `check` (the expression checked) and `n` (the number of records satisfying the expression)
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' dataChk(mydata)
##' }
dataChk <- function(d, checks, id=character(0),
                    html=FALSE, omit0=FALSE, byid=FALSE, nrows=500) {
  if(byid && length(id) < 1) stop('must specify id when byid=TRUE')
  s  <- NULL
  X  <- Dat <- list()
  dashes <- paste0(rep('-', getOption('width')), collapse='')
  fmt <- if(html)
           function(name, data)
             htmltools::HTML(c('<pre>', paste(data, '\n'),
                               '</pre>'))
         else function(name, data) c(dashes, name, dashes, data)
  
  for(i in 1 : length(checks)) {
    x <- checks[i]
    cx <- as.character(x)
    cx <- gsub('%between% c\\((.*?)\\)', '[\\1]', cx)
    form <- as.formula(paste('~', cx))
    ## Find all variables mentioned in expression
    vars.involved <- all.vars(form)
    z  <- d[eval(x), c(id, vars.involved), with=FALSE]
    no <- nrow(z)
    if(byid && no > 0) {
      Da <- z[, id, with=FALSE]
      Da[, Check := cx]
      Da[, Values := do.call(paste, z[, vars.involved, with=FALSE])]
      Dat[[cx]] <- Da
      }
    z <- if(no == 0) 'n=0' else capture.output(print(z, nrows=nrows))
    z <- fmt(cx, z)
    if(no > 0 || ! omit0) X[[cx]] <- z
    s <- rbind(s, data.frame(Check=cx, n=no))
  }
  if(byid) {
    Dat <- rbindlist(Dat, fill=TRUE)
    # setcolorder(Dat, c(id, 'Check', setdiff(names(Dat), c(id, 'Check'))))
    setkeyv(Dat, id)
    u <- paste('By', paste(id, collapse=', '))
    X[[u]] <- fmt(u, capture.output(print(Dat, nrows=nrows)))
    }
  X$Summary <- fmt('Summary', capture.output(print(s, nrows=nrows)))
  if(html) maketabs(X, initblank=TRUE)
  else for(z in X) cat(z, sep='\n')
  if(byid) invisible(s) else invisible(Dat)
}


##' Separate Chunk Plot
##'
##' Runs a plot on its own `Rmarkdown/Quarto` `knitr` Chunk.  The plot will have its own caption and size, and short captions are placed in the markdown TOC
##'
##' Expressions cannot be re-used, i.e., each expression must evaluate to the right quantity after the chunk in which the `scplot` calls are made is finished, and the new constructed chunk is input.  To input and run the constructed chunk:
##' `{r child='scplot.Rmd'}` preceeded and following by 3 back ticks.
##' Hmisc::putHcap is used to markup regular and short captions `cap, scap`.  Short caption appears in TOC.  If no `scap`, then `cap` is used for this. To change the `putHcap` `subsub` argument set `options(scplot.subsub='## ')` for example.
##' @title scplot
##' @param command an command that causes a plot to be rendered
##' @param cap long caption
##' @param scap short caption
##' @param w width of plot
##' @param h height of plot
##' @param id a string ID for the plot.  Defaults to the current chunk label if `knitr` is running
##' @return no value return; outputs R Markdown/Quarto markup
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' scplot(id='chunkid')   # initialize output file scplot.Rmd
##' # or use scplot() to use the current chunk name as the id
##' # scplot(plotting expression, caption, optional short caption, w, h)
##' # scplot(plotting expression ...)
##' }

scplot <- function(command, cap=NULL, scap=NULL, w=5, h=4, id=NULL) {

  command <- as.character(sys.call())
  if(length(command) == 1) id <- knitr::opts_current$get('label')

  ge <- .GlobalEnv
  if(length(id)) {
    cat('', sep='', file='scplot.Rmd')
    assign('.iscplot.',   0, envir=ge)
    assign('.idscplot.', id, envir=ge)
    return(invisible())
    }

  .iscplot. <- .iscplot. + 1

  cname <- paste0(.idscplot., .iscplot.)
  subsub <- getOption('scplot.subsub', TRUE)
  label  <- Hmisc::putHcap(cap, scap=scap, subsub=subsub, file=FALSE)

  k <- c(paste0('\n\n```{r ', cname, ',results="asis",echo=FALSE,fig.width=',
                w, ',fig.height=', h, '}\n'), paste0(command[2], '\n```\n\n'))
  cat(label, k, sep='', file='scplot.Rmd', append=TRUE)
  assign('.iscplot.', .iscplot., envir=ge)
  invisible()
}


##' Produce a Report Section Detailing Missing Values in a Dataset
##'
##' Quantifies frequencies of missing observations on a variable and missing variables on an observaton and produces variables tables and (depending on the number of `NA`s) multiple graphic displays in Quarto tabs.  The results are best understood by referring to [this](https://hbiostat.org/rflow/case.html#missing-data/).
##' @title missChk
##' @param data data frame or table to analyze
##' @param use a formula or character vector specifying which variables to consider if not all those in `data`
##' @param exclude a formula or character vector specifying which variables to exclude from consideration
##' @param type specify `'seq'` to return a summary of sequential exclusions rather than produce a report
##' @param maxpat maximum number of missing data patterns allowed when counting occurrences of all combinations of variables' `NA`s
##' @param maxcomb maximum number of combinations for which to produce a combination dot plot
##' @param excl1pat set to `FALSE` to not list distinct combinatons that only occur for one observation
##' @param sortpatterns set to `FALSE` to not sort patterns in decreasing frequency of missingness
##' @param prednmiss set to `TRUE` to use ordinal regression to predict the number of missing variables on an observation from the values of all the non-missing variables
##' @param omitpred a formula or character vector specifying a list of predictors not to use when predicting number of missing variables
##' @param baselabel base label for Quarto tabs made with [qreport::maketabs()]
##' @param ... passed to [combplotp()]
##' @return nothing; outputs Quarto/RMarkdown text and tabs for a full report section
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' missChk(mydata)
##' }
missChk <- function(data, use=NULL, exclude=NULL,
                    type=c('report', 'seq'),
                    maxpat=15, maxcomb=25, excl1pat=TRUE,
                    sortpatterns=TRUE,
                    prednmiss=FALSE, omitpred=NULL,
                    baselabel=NULL, ...) {

  type     <- match.arg(type)
  cargs    <- list(...)
  namedata <- deparse(substitute(data))
  prtype <- .Options$prType
  
  
  d    <- copy(data)
  setDT(d)
  if(length(use)) {
    if(inherits(use, 'formula')) use <- all.vars(use)
    d <- d[, ..use]
  }
  if(length(exclude)) {
    if(inherits(exclude, 'formula')) exclude <- all.vars(exclude)
    use <- setdiff(names(d), exclude)
    d <- d[, ..use]
  }
  
  p <- ncol(d)

  ismiss <- function(x)
    if(is.character(x) | is.factor(x))
      is.na(x) | trimws(x) == '' else is.na(x)

  ## Replace each variable with missingness indicator
  di <- d[, lapply(.SD, ismiss)]
  
  ## Hierarchical exclusions

  exc <- do.call('seqFreq', c(di, list(noneNA=TRUE)))
  if(type == 'seq') return(exc)

  na.per.var <- apply(di, 2, sum)
  na.per.obs <- apply(di, 1, sum)

  asisOut <- function(...) {
  x <- list(...)
  x <- if(length(x) > 1) paste0(unlist(x)) else x[[1]]
  knitr::asis_output(x)
  }

  if(all(na.per.var == 0)) 
    return(asisOut('No NAs on any of the',
                   p, ' variables examined.'))

  surrq <- function(x) paste0('`', x, '`')

  vmiss <- names(na.per.var)[na.per.var > 0]
  dm    <- d[, ..vmiss]
  pm    <- length(vmiss)

  cat('\n', p - pm, 'variables have no NAs and', pm,
      'variables have NAs\n\n')

  cat(namedata, ' has ', nrow(d), ' observations (', sum(na.per.obs == 0),
              ' complete) and ', ncol(d), ' variables (', sum(na.per.var == 0),
              ' complete)\n', sep='')
  
  if(sum(na.per.var) > 0) {
    z <- data.frame(Minimum=c(min(na.per.var), min(na.per.obs)),
                    Maximum=c(max(na.per.var), max(na.per.obs)),
                    Mean   =round(c(mean(na.per.var), mean(na.per.obs)), 1),
                    row.names=c('Per variable', 'Per observation'))
    print(kabl(z, caption='Number of NAs'))
    
    tab <- table(na.per.var)
    print(kabl(tab,
               caption='Frequency distribution of number of NAs per variable'))
    tab <- table(na.per.obs)
    print(kabl(tab,
               caption='Frequency distribution of number of incomplete variables per observation'))
  }
  
  if(pm < max(20, maxpat)) {
    nap <- na.pattern(dm)
    nap <- matrix(nap, ncol=1, dimnames=list(names(nap), 'Count'))
    n1  <- sum(nap[,1] == 1)
    patex <- ''
    if(excl1pat && n1 > 0) {
      patex <- paste(n1,
                     'patterns with frequency of 1 not listed\n')
      nap <- nap[nap[,1] > 1, 1, drop=FALSE]
      }

    if(nrow(nap) <= maxpat) {
      cat('Frequency counts of all combinations of NAs\n\n',
          'Variables in column order are:',
          paste(surrq(vmiss), collapse=', '), '\n\n', patex,
                sep='')
      if(sortpatterns) {
        i   <- order(- nap[, 1])
        nap <- nap[i, , drop=FALSE]
        }
      print(kabl(nap))
      return(invisible())
    }
  }

  ge <- .GlobalEnv
  .naclus. <- naclus(dm)
  assign('.naclus.', .naclus., envir=ge)
  abb <- pm > 40
  ptypes <- c('na per var' = 'NAs/var',
              'na per obs' = 'NAs/obs',
              'mean na'    = 'Mean # var NA when other var NA',
              'na per var vs mean na' =
                'NAs/var vs mean # other variables NA',
              'clus'       = 'Clustering of missingness')
  tabs <- vector('list', length(ptypes))
  for(i in seq(ptypes)) {
    cap <- if(i == 1) paste0('+ caption("Missing data patterns in `',
                             namedata, '`"',
                             if(length(baselabel))
                               paste0(', "', baselabel, '"'),
                             ')')
    lab <- surrq(ptypes[i])
    f <- if(i < 5) paste0(lab, ' ~ ',
                          'naplot(.naclus., which="', names(ptypes[i]),
                          '")', cap)
         else
           paste0(lab, ' ~ ', 'plot(.naclus., abbrev=', abb, ')')
    tabs[[i]] <- as.formula(f)
  }

  ## Add tab for sequential NA exclusions
  .seqmisstab. <- table(exc)
  assign('.seqmisstab.', .seqmisstab., envir=ge)
  
  tabs <- c(tabs, Sequential ~
                    kabl(.seqmisstab.,
                         caption='Sequential frequency-ordered exclusions due to NAs'))
  dm <- dm[, lapply(.SD, is.na)]
  ## Produce combination plot for the right number of variables with NAs
  if(pm <= maxcomb) {
    .combplotp. <- do.call('combplotp', c(list(data=dm, maxcomb=maxcomb), cargs))
    assign('.combplotp.', .combplotp., envir=ge)
     tabs <- c(tabs,
               `NA combinations` ~ .combplotp.)
  }

  if(prednmiss && (pm < p) && any(na.per.var == 0)) {
    ## Predict using ordinal regression the number of missing variables
    ## from all the non-missing variables
    ## Compute the number of missing variables per observation
    preds <- names(na.per.var)[na.per.var == 0]
    if(length(omitpred)) {
      omitv <- if(is.character(omitpred)) omitpred
               else
                 all.vars(omitpred)
      preds <- setdiff(preds, omitv)
      }
    form <- paste('na.per.obs ~', paste(preds, collapse=' + '))
    f <- rms::lrm(as.formula(form), data=d)
    if(f$fail)
      cat('prednmiss=TRUE led to failure of ordinal model to fit\n\n')
    else {
      assign('.misschkanova.', anova(f), envir=ge)
      assign('.misschkfit.',   f,        envir=ge)
      options(prType='html')
      on.exit(options(prType=prtype))
      tabs <- c(tabs,
                `Predicting # NAs per obs` ~ print(.misschkfit., coefs=FALSE) +
                                             plot(.misschkanova.))
      }
  }

  do.call(maketabs, c(list(initblank=TRUE, baselabel=baselabel),
                      list(tabs)))
}

##' Determine Types of Variables
##'
##' For all the variables in a data frame/table, analyzes them to determine types: continuous, nonnumeric, and discrete.  `include` and `exclude` can be vector or right-side-only formulas.
##' @title varType
##' @param data data frame or table to analyze
##' @param include formula or vector of variable names to attend to 
##' @param exclude a formula or character vector specifying which variables to exclude from consideration
##' @param ndistinct minimum number of distinct numeric values a variable must have to be considered continuous
##' @param nnonnum maximum number of distinct values a variable can have to be considered discrete
##' @return `list` of vectors
##' @author Frank Harrell
##' @examples
##' set.seed(1)
##' d <- data.frame(i=1:100, x=runif(100), y=sample(1:3, 100, TRUE),
##'                 w=sample(c('cat','dog','giraffe'), 100, TRUE),
##'                 v=sample(letters, 100, TRUE))
##' varType(d)
##' @md
varType <- function(data, include=NULL, exclude=NULL,
                    ndistinct=10, nnonnum=20) {

  g <- function(x) {
    nnum <- is.character(x) || is.factor(x)
    lu   <- length(unique(x))
    fcase(! nnum && lu >= ndistinct, 'continuous',
          nnum && lu > nnonnum,      'nonnumeric',
          default = 'discrete')
  }
  
  if(! is.data.frame(data)) return(g(data))

  if(length(include) && ! is.character(include))
    include <- all.vars(include)
  if(length(exclude) && ! is.character(exclude))
    exclude <- all.vars(exclude)
  v <- if(length(include)) include else setdiff(names(data), exclude)
  
  s <- sapply(if(is.data.table(data)) data[, ..v] else data[v], g)
  split(names(s), s)
}

##' Find Continuous Variables
##'
##' Uses [varType()] to type the variables then retrieves the vector of names of continuous ones.
##' @title conVars
##' @param ... passed to [varType()]
##' @return character vector
##' @author Frank Harrell
##' @examples
##' \dontrun{
##' conVars(mydata)
##' }
conVars <- function(...) varType(...)$continuous

##' Find Discrete Variables
##'
##' Uses [varType()] to type the variables then retrieves the vector of names of discrete ones.
##' @title disVars
##' @param ... passed to [varType()]
##' @return character vector
##' @author Frank Harrell
##' @examples
##' \dontrun{
##' disVars(mydata)
##' }
disVars <- function(...) varType(...)$discrete

##' Convert Vector of Variables Names to a Right-Sided Formula
##'
##' Given a vector of character strings, turns them into a formula with no left hand side variable.
##' @title asForm
##' @param x character vector
##' @return formula
##' @author Frank Harrell
##' @examples
##' asForm(letters[1:6])
asForm  <- function(x) as.formula(paste('~', paste(x, collapse=' + ')))

##' Create a Quarto Mermaid Diagram Chunk With Variable Insertions
##'
##' Takes a character string or vector and uses [knitr::knit_expand()] to apply variable insertions before the diagram is rendered by Quarto.  See [this](https://hbiostat.org/rflow/doverview.html#fig-mermaid1/) for an example.
##' @title makemermaid
##' @param .object. character string or vector with `mermaid` markup
##' @param ... name=value pairs that makes values replace `{{name}}` elements in the markup
##' @param file name of file to hold `mermaid` markup after variable insertions.  Run this in Quarto using a chunk to looks like the following, which was for `file='mermaid1.mer'`.
##'
##' ````
##' ```{mermaid}
##' %%| fig-cap: "Consort diagram produced by `mermaid`"
##' %%| label: fig-mermaid1
##' %%| file: mermaid1.mer
##' ```
##' ````
##' @return nothing; used to `knitr::knit_expand()` mermaid markup
##' @author Frank Harrell
##' @seealso [makegraphviz()]
##' @md
makemermaid <- function(.object., ..., file) {
  code <- strsplit(.object., '\n')[[1]]
  ki <- knitr::knit_expand
  etext <- do.call('ki', c(list(text=code), list(...)))
  cat(etext, sep='\n', file=file)
  invisible()
}

##' Create a Quarto Graphviz dot Diagram Chunk With Variable Insertions
##'
##' Takes a character string or vector and uses [knitr::knit_expand()] to apply variable insertions before the diagram is rendered by Quarto.  See [this](https://hbiostat.org/rflow/doverview.html#sec-doverview-filter/) for an example.  Unlike `mermaid`, `graphviz` can include user-defined linkages to specific parts of a node (e.g., a single word in a line of text) to another part of the chart, and can render tables.  If an inclusion is `...` is a data frame or table, it will be properly rendered inside the diagram.
##' @title makegraphviz
##' @param .object. character string or vector with `graphviz` markup
##' @param ... name=value pairs that makes values replace `{{name}}` elements in the markup
##' @param file name of file to hold `graphviz` markup after variable insertions.  Run this in Quarto using a chunk to looks like the following, which was for `file='graphviz.dot'`.
##'
##' ````
##' ```{dot}
##' //| label: fig-doverview-graphviz
##' //| fig-height: 4
##' //| fig-cap: "Consort diagram produced with `graphviz` with detailed exclusion frequencies in a separate node"
##' //| file: graphviz.dot
##' ```
##' ````
##' @return nothing; used to `knitr::knit_expand()` graphviz markup
##' @author Frank Harrell
##' @seealso [makemermaid()]
##' @md
makegraphviz <- function(.object., ..., file) {
  x <- strsplit(.object., '\n')[[1]]
  # Translate `foo` to <font color='darkblue' face='courier'>foo</font>
  # face=Lucida Console resulted in overwriting of text
  code <- gsub('`(.*?)`', "<font color='darkblue' face='courier'>\\1 </font>", x)
  ki <- knitr::knit_expand
  dotlist <- list(...)
  L <- length(dotlist)
  # Function to strip off style info done by html.data.frame
  mtab <- function(d) {
    k <- ncol(d)
    w <- matrix('', nrow=nrow(d) + 1, ncol=k)
    w[1, ] <- paste0('<td><font color="darkblue"><b>', names(d),
                    '</b></font></td>')
    for(i in 1 : k) {
      di <- d[[i]]
      f  <- trimws(format(di))
      w[-1, i] <- f
      align <- if(length(unique(nchar(f))) == 1) 'CENTER'
        else if(is.numeric(di)) 'RIGHT' else 'LEFT'
      w[-1, i] <- paste0('<td ALIGN="', align, '">',
                          w[-1, i], '</td>')
    }
    w <- apply(w, 1, function(x) paste0('<tr>',
                paste(x, collapse=''), '</tr>'))
    c('<table border="0" cellborder="0" cellspacing="0">',
          w[1], '<HR/>', w[-1], '</table>')
  }
  if(L) for(i in 1 : L)
    if(is.data.frame(dotlist[[i]]))
      dotlist[[i]] <- mtab(dotlist[[i]])
  etext <- do.call('ki', c(list(text=code), dotlist))
  cat(etext, sep='\n', file=file)
  invisible()
}

##' Make Variable Clustering Quarto Report Section
##'
##' Draws a variable clustering dendrogram and optionally graphically depicts a correlation matrix.  See [this](https://hbiostat.org/rflow/descript.html#describing-variable-interrelationships/) for an example.  Uses [Hmisc::varclus()].
##' @title cClus
##' @param d a data frame or table
##' @param exclude formula or vector of character strings containing variables to exclude from analysis
##' @param corrmatrix set to `TRUE` to use [Hmisc::plotCorrM()] to depict a Spearman rank correlation matrix.
##' @param redundancy set to `TRUE` to run [Hmisc::redun()] on non-excluded variables
##' @param spc set to `TRUE` to run [Hmisc::princmp()] to do a sparse principal component analysis with the argument `method='sparse'` passed
##' @param trans set to `TRUE` to run [Hmisc::transace()] to transform each predictor before running redundancy or principal components analysis. `transace` is run on the stacked filled-in data if `imputed` is given.
##' @param rexclude extra variables to exclude from `transace` transformating-finding, redundancy analysis, and sparce principal components (formula or character vector)
##' @param fracmiss if the fraction of `NA`s for a variable exceeds this the variable will not be included
##' @param maxlevels if the maximum number of distinct values for a categorical variable exceeds this, the variable will be dropped
##' @param minprev the minimum proportion of non-missing observations in a category for a binary variable to be retained, and the minimum relative frequency of a category before it will be combined with other small categories
##' @param imputed an object created by [Hmisc::aregImpute()] or [mice::mice()] that contains information from multiple imputation that causes `vClus` to create all the filled-in datasets, stack them into one tall dataset, and pass that dataset to [Hmisc::redun()] or [Hmisc::princmp()] so that `NA`s can be handled efficiently in redundancy analysis and sparse principal components, i.e., without excluding partial records.  Variable clustering and the correlation matrix are already efficient because they use pairwise deletion of `NA`s.
##' @param horiz set to `TRUE` to draw the dendrogram horizontally
##' @param label figure label for Quarto
##' @param print set to `FALSE` to not let `dataframeReduce` report details
##' @param redunargs a `list()` of other arguments passed to [Hmisc::redun()]
##' @param spcargs a `list()` of other arguments passed to [Hmisc::princmp()]
##' @param transaceargs a `list()` of other arguments passed to [Hmisc::transace()]
##' @param spcfile a character string specifying an `.rds` R binary file to hold the results of sparse principal component analysis.  Using [Hmisc::runifChanged()], if the file name is specified and no inputs have changed since the last run, the result is read from the file.  Otherwise a new run is made and the file is recreated if `spcfile` is specified.  This is done because sparse principal components can take several minutes to run on large files.
##' @param transacefile similar to `spcfile` and can be used when `trans=TRUE`
##' @return makes Quarto tabs and prints output, returning nothing unless `spc=TRUE` or `trans=TRUE` are used, in which case a list with components `princmp` and/or `transace` is returned and these components can be passed to special `print` and `plot` methods for `spc` or to `ggplot_transace`.  The user can put scree plots and PC loading plots in separate code chunks that use different figure sizes that way.
##' @seealso [Hmisc::varclus()], [Hmisc::plotCorrM()], [Hmisc::dataframeReduce()], [Hmisc::redun()], [Hmisc::princmp()], [Hmisc::transace()]
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' vClus(mydata, exclude=.q(country, city))
##' }
vClus <- function(d, exclude=NULL, corrmatrix=FALSE, redundancy=FALSE,
                  spc=FALSE, trans=FALSE, rexclude=NULL,
                  fracmiss=0.2, maxlevels=10, minprev=0.05, imputed=NULL,
                  horiz=FALSE, label='fig-varclus', print=TRUE,
                  redunargs=NULL, spcargs=NULL, transaceargs=NULL,
                  transacefile=NULL, spcfile=NULL) {

  w <- as.data.frame(d)  # needed by dataframeReduce
  if(length(exclude)) {
    if(! is.character(exclude)) exclude <- all.vars(exclude)
    w <- w[setdiff(names(w), exclude)]
  }
  if(length(rexclude) && ! is.character(rexclude))
    rexclude <- all.vars(rexclude)
  
  w <- dataframeReduce(w, fracmiss=fracmiss, maxlevels=maxlevels,
                       minprev=minprev, print=FALSE)
  rinfo <- attr(w, 'info')
  if(print && length(rinfo))
    print(kabl(attr(w, 'info'),
               caption='Variables removed or modified'))
  
  form <- as.formula(paste('~', paste(names(w), collapse=' + ')))
  v <- varclus(form, data=w)
  if(! corrmatrix) {
    if(horiz) plot(as.dendrogram(v$hclust), horiz=TRUE)
    else plot(v)
  }
  if(corrmatrix) {
    ge <- .GlobalEnv
    assign('.varclus.', v, envir=ge)
    rho <- varclus(form, data=w, trans='none')$sim
    assign('.varclus.gg.', plotCorrM(rho, xangle=90)[[1]], envir=ge)
    cap <- 'Spearman rank correlation matrix.  Positive correlations are blue and negative are red.'
    form1 <- `Correlation Matrix` ~ .varclus.gg. + caption(cap, label=label) +
      fig.size(width=9.25, height=8.5)
    form2 <- `Variable Clustering` ~
      plot(as.dendrogram(.varclus.$hclus), horiz=TRUE) +
      fig.size(height=4.5, width=7.5)
    form3 <- `Variable Clustering` ~ plot(.varclus.) +
      fig.size(height=5.5, width=7.5)
    if(horiz) maketabs(form1, form2, initblank=TRUE)
    else
      maketabs(form1, form3, initblank=TRUE)
    }
  
  if(trans | redundancy | spc) {
    formr <- as.formula(paste('~', paste(setdiff(names(w), rexclude),
                                         collapse=' + ')))
    if(length(imputed)) w <- do.call(rbind, completer(imputed, mydata=d))
  }

  R <- list()
  
  if(trans) {
    args <- c(list(formr, data=w), transaceargs)
    if(! length(transacefile)) z <- do.call(transace, args)
    else {
      g <- function() do.call(transace, args)
      z <- runifChanged(g, args, file=transacefile)
      }
    R$transace <- z
    formr      <- z$transformed
    w          <- NULL
  }
  
  if(redundancy) {
    red <- do.call(redun, c(list(formr, data=w), redunargs))
    cat(htmlVerbatim(red), sep='\n')
  }
  
  if(spc) {
    args <- c(list(formr, data=w, method='sparse'), spcargs)
    if(! length(spcfile)) p <- do.call(princmp, args)
    else {
      g <- function() do.call(princmp, args)
      p <- runifChanged(g, args, file=spcfile)
    }
    R$princmp <- p
  }
  invisible(R)
}


##' Produce a Data Overview Quarto Section
##'
##' Produces a multi-tabbed dataset overview as exemplified [here](https://hbiostat.org/rflow/doverview.html#sec-doverview-data/).  This includes provision of data about data such as variable type, symmetry, missingness, rarest and most common values.
##' @title dataOverview
##' @param d a data frame or table
##' @param d2 optional second dataset used for analyzing uniqueness of subject IDs
##' @param id optional formula providing names of subject identifiers
##' @param plot specifies type of plot, defaulting to `'scatter'`
##' @param pr set to `FALSE` to omit detailed table and present only graphics
##' @param which when two datasets are given which one should be the focus
##' @param dec certain summary statistics are rounded to the nearest `dec` places
##' @return nothing; renders a report with Quarto/RMarkdown
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' dataOverview(mydata, secondarydataset)
##' }
dataOverview <- function(d, d2=NULL, id=NULL,
                         plot=c('scatter', 'dot', 'none'),
                         pr=nvar <= 50, which=1, dec=3) {
  nam1 <-                deparse(substitute(d ))
  nam2 <- if(length(d2)) deparse(substitute(d2))
  plot <- match.arg(plot)

  if(which == 2 && ! length(d2))
    stop('which=2 only applies when second dataset is provided')
  
  d <- copy(d)
  setDT(d)
  if(length(d2)) {
    d2 <- copy(d2)
    setDT(d2)
  }

  ## From rmsb package, augmented to handle dates/times:
  distSym <- function(x, prob=0.9, na.rm=FALSE) {
    if(na.rm) x <- x[! is.na(x)]
    x <- unclass(x) 
    a <- (1. - prob) / 2.
    w <- quantile(x, probs=c(a / 2., 1. - a / 2.))
    xbar <- mean(x)
    as.vector((w[2] - xbar) / (xbar - w[1]))
  }

  lun <- function(x) length(unique(x))
  
  id1 <- id2 <- FALSE
  ids1 <- ids2 <- NULL
  idv <- if(length(id)) all.vars(id)
  
  nid <- if(length(idv) == 1) paste('ID variable', idv)
         else
           paste('ID variables', paste(idv, collapse=' '))

  if(length(id)) {
    id1 <- all(idv %in% names(d))
    if(id1)
      ids1 <- unique(d[, do.call(paste0, .SD), .SDcols=idv])
    if(length(d2)) {
      id2 <- all(idv %in% names(d2))
      if(id2) ids2 <- unique(d2[, do.call(paste0, .SD), .SDcols=idv])
      }
  }

  ismiss <- function(x)
    if(is.character(x) | is.factor(x))
      is.na(x) | trimws(x) == '' else is.na(x) 
  na         <- sapply(d, ismiss) * 1
  na.per.var <- apply(na, 2, sum)
  na.per.obs <- apply(na, 1, sum)

  cat(nam1, ' has ', nrow(d), ' observations (', sum(na.per.obs == 0),
              ' complete) and ', ncol(d), ' variables (', sum(na.per.var == 0),
              ' complete)', sep='')
  if(length(d2)) {
    vcommon <- sum(names(d) %in% names(d2))
    cat(' of which', vcommon, 'variables are in', nam2)
  } else cat('\n')
  
  if(id1) {
    cat('There are', length(ids1), 'unique values of', nid, 'in', nam1)
    if(length(d2) && id2) {
      ncommon <- sum(ids1 %in% ids2)
      cat(' with', ncommon, 'unique IDs also found in', nam2)
    }
    cat('\n')
  }
  if(length(d2)) {
    cat(nam2, 'has', nrow(d2), 'observations and', ncol(d2), 'variables')
    vcommon <- sum(names(d2) %in% names(d))
    cat(' of which', vcommon, 'are in', nam1, '\n')
    if(id2) {
      cat('There are', length(ids2), 'unique values of', nid, 'in', nam2, '\n')
      if(id1) {
        ncommon <- sum(ids2 %in% ids1)
        cat(ncommon, 'unique IDs are found in', nam1, '\n\n')
      }
    } 
  }
  
  ## Get variables types
  w <- switch(which, d, d2)
  nvar <- ncol(w)

  g <- function(x) {
    type <- varType(x)
    info <- NA
    distinct <- lun(x)
    if(type == 'continuous') {
      sym <- distSym(x, na.rm=TRUE)
      x <- round(x, dec)
    }
    
    tab <- table(x)
    n   <- sum(tab)
    fp  <- tab / n
    if(type != 'continuous') sym <- 1. - mean(abs(fp - 1. / length(tab)))
    info <- if(distinct < 2) 0 else (1 - sum(fp ^ 3)) / (1 - 1 / n / n)
    low  <- which.min(tab)
    hi   <- which.max(tab)
                  
    list(type       = upFirst(type),
         distinct   = distinct,
         info       = info,
         symmetry   = sym,
         NAs        = sum(is.na(x)),
         mincat     = names(tab)[low],
         mincatfreq = unname(tab[low]),
         maxcat     = names(tab)[hi],
         maxcatfreq = unname(tab[hi]))
  }

  z <- lapply(w, g)
  r <- rbindlist(z, idcol='variable')
  if(pr) {
    s <- copy(r)
    setnames(s, .q(variable, type, distinct, info, symmetry, NAs, mincat,
                   mincatfreq, maxcat, maxcatfreq),
             .q(Variable, Type, Distinct, Info, Symmetry, NAs, "Rarest Value",
                "Frequency of Rarest Value", Mode, "Frequency of Mode"))
    print(kabl(s, digits=3))
  }
  
  if(plot == 'none') return(invisible())
  
  breaks <- function(mf) {
    br  <- pretty(c(0, mf), 10)
    mbr <- c(0, 10, 20, 30, 40, 50, 100, if(mf >= 200) seq(200, mf, by=100))
    mbr <- mbr[mbr < mf]
    mbr <- setdiff(mbr, br)
    list(br=br, mbr=mbr)
    }
    
  if(plot == 'dot') {
    r <- r[, .(variable, type, distinct, NAs, mincatfreq, maxcatfreq)]
    m <- melt(r, id.vars=c('variable', 'type'),
            variable.name='what', value.name='Freq')
    s <- split(m, m$type)
    b <- breaks(max(m$Freq))
    br <- b$br; mbr <- b$mbr
    gg <- function(data)
      ggplot(data, aes(y=variable, x=Freq, col=what)) + geom_point() +
        scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
        xlab('') + ylab('Frequency') +
        guides(color=guide_legend(title='')) +
        theme(legend.position='bottom')
    g <- lapply(s, gg)
  } else if(plot == 'scatter') {
    r[, txt := paste(variable,
                     paste('distinct values:', distinct),
                     paste('NAs:', NAs),
                     paste('Info:', round(info, 3)),
                     paste('Symmetry:', round(symmetry, 3)),
                     paste0('lowest frequency (', mincatfreq, ') value:',
                            mincat),
                     paste0('highest frequency (', maxcatfreq, ') value:',
                            maxcat), sep='<br>')]

    b <- breaks(max(r$distinct))
    br <- b$br; mbr <- b$mbr

    gg <- function(data) {
      cap <- paste(nrow(data), 'variables and', nrow(w),
                   'observations\nNumber of NAs is color coded')
      lnna <- levels(data$.nna.)
      ggplotlyr(
        if(any(trimws(as.character(data$.nna.)) != '0') &&
           length(unique(as.integer(data$.nna.))) > 1)
          ggplot(data, aes(x=distinct, y=symmetry,
                       color=as.integer(.nna.), label=txt)) +
          scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
          scale_color_gradientn(colors=viridis::viridis(min(10, length(lnna))),
                              breaks=1 : length(lnna),
                              labels=lnna) +
          geom_point() + xlab('Number of Distinct Values') +
          ylab('Symmetry') +
          labs(caption=cap) +
          guides(color=guide_legend(title='NAs'))
        else
          ggplot(data, aes(x=distinct, y=symmetry,
                       label=txt)) +
          scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
          geom_point() + xlab('Number of Distinct Values') +
          ylab('Symmetry') +
          labs(caption=cap)
      )
        #        theme(legend.position='bottom')
      }
    r[, .nna. := cut2(NAs, g=12)]
    s <- split(r, r$type)
    g <- lapply(s, gg)
  }
  print(kabl(r[, paste(levels(.nna.), collapse=' ')],
             'Intervals of frequencies of NAs used for color-coding plots'))
  if(plot == 'scatter') {
    cap <- paste('Plot of the degree of symmetry of the distribution of a variable (value of 1.0 is most symmetric) vs. the number of distinct values of the variable.  Hover over a point to see the variable name and detailed characteristics.')
    maketabs(g, initblank=TRUE, basecap=cap, cap=1)
    }
  else maketabs(g, initblank=TRUE)
  invisible()
}

##' Multiple Dataset Overview
##'
##' Provides an overview of the data tables inside a giant list.  The result returned (invisible) is a data table containing for each variable a comma-separated list of datasets containing that variable (other than `id` variables).
##' @title multDataOverview
##' @param X list object containing any number of data frames/tables
##' @param id formula containing a single subject identifier, e.g., `id = ` patient.id`
##' @return invisibly, a data table
##' @author Frank Harrell
##' @seealso [dataOverview()]
##' @md
##' @examples
##' \dontrun{
##' multDataOverview(list(data1=mydata1, data2=mydata2), id = ~ subject.id)
##' }
multDataOverview <- function(X, id=NULL) {
  if(length(id)) id <- all.vars(id)
  k <- length(X)
  nam <- lapply(X, names)
  nuv <- length(unique(unlist(nam)))
  common <- names(X[[1]])
  if(k > 1) for(i in 2 : k) {
    common <- intersect(common, names(X[[i]]))
    if(! length(common)) break  # intersection already empty
  }
common  <- sort(common)
ncommon <- length(common)
cat(k, 'datasets\n')
cat(nuv, 'distinct variable names across datasets\n')
if(ncommon) cat('Variables in all datasets:', paste(common, collapse=', '), '\n')

cat('\nFrequencies of variable classes used, other than labelled:\n')
w <- lapply(X, function(x) sapply(x, function(y) paste(setdiff(class(y), 'labelled'), collapse='+')))
print(table(unlist(w)))

w <- lapply(X, function(x) sapply(x, function(y) inherits(y, 'labelled')))
cat('\nVariables with labels:', sum(unlist(w)), 'out of', length(unlist(w)), '\n\n')

w <- data.frame(row.names=names(X),
                rows=sapply(X, nrow),
                columns=sapply(X, length))
if(length(id)) {
  uid <- function(x) if(id %in% names(x)) length(unique(x[[id]])) else NA
  w$'Distinct IDs' <- sapply(X, uid)
}
cat('\n')
print(w)

nvar <- sapply(nam, length)
# For each variable name count the number of datasets containing it
w <- data.table(dsname=rep(names(X), nvar), variable=unlist(nam))
if(length(id)) w <- subset(w, variable != id)
# For each variable create a comma-separated list of datasets
# containing it
invisible(w[, .(datasets=paste(sort(dsname), collapse=', ')), keyby=variable])
}


##' Add Figure Captions to a Dataset
##'
##' Fetches the figure caption and optional short caption from the currently running code chunk (under `knitr`) and appends them to a running caption dataset named `.captions.` in the global environment.  This facilites customizing a table of figures in a report.
##' @title addCap
##' @param label figure label to use if not fetched from chunk information
##' @param cap caption to use if not from chunk
##' @param scap short caption to use if not from chunk
##' @return invisible list with `label`, `cap`, `scap`
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' # Called from inside a knitr chunk and all information pulled from
##' # chunk information
##' addCap()
##' }
addCap <- function(label=NULL, cap=NULL, scap=NULL) {
  g <- function(tag1, tag2) {
    r <- knitr::opts_current$get(tag1)
    if(! length(r)) r <- knitr::opts_current$get(tag2)
    r
    }
  h <- function() {
    lab <- g('label')
    if(length(lab) && ! grepl('^fig-', lab)) lab <- paste0('fig-', lab)
    lab
    }
  if(! length(label)) label <- h()
  deb <- .Options$debugaddCap; deb <- length(deb) && deb
  if(deb) cat('label:', label, '\n', file='/tmp/z', append=TRUE)
  if(! length(label))              return(invisible(list(NULL, NULL, NULL)))
  if(is.logical(label) && ! label) return(invisible(list(NULL, NULL, NULL)))
  if(! length(cap))  cap  <- g('fig.cap',  'cap')
  if(! length(scap)) scap <- g('fig.scap', 'scap')
  if(! length(cap) && length(scap))  cap  <- scap
  if(! length(scap) && length(cap))  scap <- cap
  ge <- .GlobalEnv
  if(! exists('.captions.')) assign('.captions.', NULL, envir=ge)
  info <- data.frame(label=label, cap=cap, scap=scap)
  if(deb) prn(info, file='/tmp/z')
  if(! length(.captions.) || label %nin% .captions.$label)
    assign('.captions.', rbind(.captions., info), envir=ge)
  invisible(list(label=label, cap=cap, scap=scap))
}

##' Save Caption Dataset in External File
##'
##' Uses [base::saveRDS()] to save the `.captions.` dataset to a user file.
##' @title saveCap
##' @param basename base file name to which `-captions.rds` will be appended 
##' @return nothing; used to create a saved RDS dataset of caption information
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' saveCap('chapter3')
##' }
saveCap <- function(basename)
  if(exists('.captions.'))
    saveRDS(.captions., file=paste0(basename, '-captions.rds'), compress='xz')

##' Pretty Printing of Captions Dataset
##'
##' Uses `kable` to print the caption information saved in `.captions.`.
##' @title printCap
##' @param book set to `TRUE` to not use `format='html'` when running `kable`
##' @return `kable` object
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' princCap()
##' }
printCap <- function(book=FALSE) {
  if(book) {
    files <- list.files(pattern='.*-captions.rds')
    .captions. <- NULL
    for(f in files) .captions. <- rbind(.captions., readRDS(f))
    }
  cap <- .captions.[c('label', 'scap')]
  cap$label <- paste0('@', cap$label)
  names(cap) <- c('Figure', 'Short Caption')
  if(book) knitr::kable(cap, row.names=FALSE, format='pipe')
  else     knitr::kable(cap, row.names=FALSE, format='html')
}

##' Set `knitr` to Automatically Call `addCap` in Figure-Producing Chunks
##'
##' Adds a `knitr` hook that takes effect before the chunk is run.  The hook function retrieves figure information from the current chunk to give to `addCap`.
##' @title hookaddcap
##' @param loc if non-`NULL` will be used to set the `knitr` chunk option `fig.cap.location`
##' @return nothing; calls `knitr` hook and chunk option setting functions
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' hookaddcap()
##' }
hookaddcap <- function(loc=NULL) {
  cf <- function(before, options, envir) {
    if(! before) return()
    label   <- knitr::opts_current$get('label')
    cap     <- options$fig.cap
    if(! length(cap)) cap <- options$cap
    if(length(cap) && is.call(cap))   cap <- eval(cap)
    ## Chunk produced a figure if label: fig- and fig-cap were
    if(! length(cap) || cap == '' || ! grepl('^fig-', label)) return()
    scap    <- options$fig.scap
    if(! length(scap)) scap <- options$scap
    if(length(scap) && is.call(scap)) scap <- eval(scap)
    if( ! length(scap) || scap == '') scap <- cap
    ## addCap will ignore an entry if .captions. already has an entry
    ## with the same label.  So if use manually put addCap() inside a chunk
    ## it is likely that a second entry will be avoided
    addCap(label, cap, scap)
    }
  knitr::knit_hooks$set(addcapfile=cf)
  knitr::opts_chunk$set(addcapfile=TRUE)
  if(length(loc)) knitr::opts_chunk$set(fig.cap.location=loc)
}

##' Set Nice Defaults for Base Graphics Parameters
##'
##' This function tries to set [graphics::par()] to make base graphics look more publication-ready.
##' @title spar
##' @param mar see `par`
##' @param lwd see `par`
##' @param mgp see `par`
##' @param tcl see `par`
##' @param xpd see `par`
##' @param las see `par`
##' @param bot additional lines of space to set aside for the bottom of the graph for extra subtitles etc.
##' @param left additional lines to set aside at left
##' @param top same for top
##' @param rt same for right margin
##' @param ps see `par`
##' @param mfrow see `par`
##' @param axes see `par`
##' @param cex.lab see `par`
##' @param cex.axis see `par`
##' @param ... other parameters passed as-is to [graphics::par()]
##' @return nothing; side effect of setting `par()`
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' spar(top=2, bot=1)  # leave extra space for titles
##' }
spar <-
  function(mar=if(!axes)
                 c(2.25+0.6+bot-.45*multi,2*(las==1)+2.2+left,
                   .5+top+.25*multi, .5+rt) else
                 c(3.25+0.6+bot-.45*multi,2*(las==1)+3.7+left,
                   .5+top+.25*multi, .5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                 if(multi) c(1.5+0.83,    0.365-0.03, 0) else
                           c(2.4-.4+0.83, 0.475-0.03, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE, las=1,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 12 else 15,
                 mfrow=NULL, axes=TRUE, cex.lab=1.15, cex.axis=.8,
                 ...) {
  multi <- length(mfrow) > 0
  par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
      cex.lab=cex.lab, cex.axis=cex.axis, las=las, ...)
  if(multi) par(mfrow=mfrow)
}

## For illustrating in-line R codes so back ticks will appear
##' Protecting Backticks for Illustrating In-line R Code
##'
##' This function pastes back ticks around a string so those extra back ticks don't have to appear in the user's code in a report.  This prevents Quarto from intervening.
##' @title rwrap 
##' @param x a character string
##' @return `x` surrounded by backtick r and backtick
##' @author Frank Harrell
##' @md
##' @examples
##' rwrap('pi')
rwrap <- function(x) paste0('\\`r ', x, '\\`')

## The following doesn't work
## Usage for 80% font size: `r fontstyle(80)`    . . .  `r endfont`
# fontstyle <- function(size=NULL, color=NULL)
#   paste0('<p style="',
#   if(length(size))  paste0('font-size: ', size,  '%;'),
#   if(length(color)) paste0('color:',      color, ';'), '">')
# endfont  <- '</p>'

##' Time an Expression and Report in Quarto Margin
##'
##' Function to time an expression, printing the result of [base::system.time()] in the right margin, and storing the result of system.time in `.systime.` in the global environment so tha the user can refer to it.
##' @title timeMar
##' @param x an expression to execute
##' @return invisibly, the result of the expression
##' @author Frank Harrell
##' @md
##' @seealso [hooktime()]
##' @examples
##' \dontrun{
##' g <- function(...){}  # define a function to run slowly
##' result <- timeMar(g())
##' }
timeMar <- function(x) {
  ge <- .GlobalEnv
  stime <- system.time(.res. <- x)
  assign('.systime.', stime, envir=ge)
  k <- capture.output(stime)
  # change trailing blank to s
  k[2] <- sub(' $', 's', k[2])
  k  <- c('```', k, '```')
  makecolmarg(k, type='cat')
  invisible(.res.)
}

##' Create knitr Hook for Reporting Execution Time for Chunks
##'
##' Creates a hook called `time` that can be activated by including `time=TRUE` in a chunk header.  The chunk's execution time in seconds will be printed in a very small html font at the end of the chunk's output.
##' @title hooktime
##' @param all set to `TRUE` to time every chunk without the need for `time=TRUE` in the chunk header
##' @return nothing
##' @seealso [this](https://bookdown.org/yihui/rmarkdown-cookbook/time-chunk.html/) and [timeMar()]
##' @author Frank Harrell
##' @md
hooktime <- function(all=FALSE) {
  timeit <- function(before, options, envir) {
    if(before) {
      ge <- .GlobalEnv
      assign('.start.time', Sys.time(), envir=ge)
    }
    else {
      et <- paste0(round(as.numeric(Sys.time() - .start.time), 3), 's')
      paste0('<span style="font-size:64%">Execution time: ', et, '</span>')
    }
  }
  knitr::knit_hooks$set(time=timeit)
  if(all) knitr::opts_chunk$set(time=TRUE)
  invisible()
}

  
##' Apply Derived Variable Specifications
##' 
##' Function to apply derived variable specifications derv to a data table `d`.   Actions on `d` are done in place, so call the function using `runDeriveExpr(d, derv object)` and not by running `d <- runDeriveExpr(d, derv object)`.
##' See [this](https://hbiostat.org/rflow/manip.html#sec-manip-recexp/) for an example.
##' @title runDeriveExpr
##' @param d a data table
##' @param derv a `list` of expressions to evaluate
##' @param pr set `pr=FALSE` to suppress information messages
##' @return nothing; used to print information and add variables to data table
##' @author Frank Harrell
##' @md
##' @examples
##' require(data.table)
##' d <- data.table(ht=c(68,  60), wt=c(280, 135), chol=c(120, 150))
##' derived <- list(
##'   list(bmi = expression(703 * wt / ht ^ 2),
##'        label='Body Mass Index',
##'        units='Kg/m^2'),
##'   list(bsa=expression(0.007184 * (0.4536 * wt) ^ 0.425 * (2.54 * ht) ^ 0.725),
##'        label='Body Surface Area',
##'        units='m^2', drop=.q(wt, ht) )   )
##' runDeriveExpr(d, derived)
##' print(d)
##' contents(d)
runDeriveExpr <- function(d, derv, pr=TRUE) {
  addlabu <- function(x, lab, un) {
    if(length(lab)) label(x) <- lab
    if(length(un))  units(x) <- un
    x
  }
  allvars <- names(d)
  for(w in derv) {
    vname <- names(w)[1]
    ex    <- w[[1]]
    lab   <- w$label
    un    <- w$units
    dr    <- w$drop
    ll    <- length(lab)
    lu    <- length(un)
    if(pr) {
      z <- if(vname %in% allvars)
             paste('Existing variable', vname, 'changed')
           else
             paste('Derived variable', vname, 'added')
      cat(z,
          if(ll || lu)
            paste0(' with', if(ll) ' label', if(lu) ' units'),
          if(length(dr)) paste0('; dropped variables ',
                               paste(dr, collapse=', ')),
          '\n', sep='')
      }
    d[, (vname) := eval(ex)]
    if(length(dr)) d[, (dr) := NULL]
    if(ll || lu)
      set(d, j=vname, value=addlabu(d[[vname]], lab, un))
  }
  invisible()
}

utils::globalVariables(c('.captions.', 'Check', 'Values', 'variable', 'type',
                       'distinct', 'info', 'symmetry', 'NAs', 'mincat',
                       'mincatfreq', 'maxcat', 'maxcatfreq', 'Variable',
                       'Type', 'Distinct', 'Info', 'Symmetry', 'Mode',
                       '.', 'what', 'txt', '.nna.', '.chunknumber.',
                       '.fs.', '..use', '..vmiss', '.naclus.', '.seqmisstab.',
                       '.combplotp.', 'misschkanova.', 'misschkfit.',
                       'dsname', '.iscplot.', '.idscplot.', '.varclus.',
                       '.varclus.gg.', '..v', '..use', '..vmiss',
                       '.start.time'))
