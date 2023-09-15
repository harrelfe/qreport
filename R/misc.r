#' Set qreport Options
#'
#' @param study an optional study mnemonic (character string) needed when multiple studies are being analyzed (or when one study is divided into distinct strata)
#' @param \dots a series of options for which non-default values are desired:
#' \itemize{
#'  \item{\code{tx.pch}:}{symbols corresponding to treatments}
#'  \item{\code{tx.col}:}{colors corresponding to treatments}
#'  \item{\code{tx.linecol}:}{colors for lines in line plots}
#'  \item{\code{nontx.col}:}{colors for categories other than treatments}
#'  \item{\code{tx.lty}:}{line types corresponding to treatments}
#'  \item{\code{tx.lwd}:}{line widths corresponding to treatments}
#'  \item{\code{tx.var}:}{character string name of treatment variable}
#'  \item{\code{er.col}:}{2-vector with names \code{"enrolled","randomized"} containing colors to use for enrolled and randomized in needle displays}
#'  \item{\code{alpha.f}:}{single numeric specifying alpha adjustment to be applied to all colors.  Default is 1 (no adjustment)}
#'  \item{\code{denom}:}{named vector with overall sample sizes}
#' }
#' See https://github.com/plotly/plotly.py/blob/master/plotly/colors.py#L83-L87/
#' @return no returned value, used to set `options()`
#' @md
#' @export
#' @examples
#' setqreportOption(tx.var='treatment', denom=c(enrolled=1000, randomized=800, a=398, b=402))
setqreportOption <- function(..., study=' ') {
  hop     <- getOption('qreport')
  default <- if(length(hop)) hop[[study]]
  opts    <- list(...)
  alpha.f <- if(length(opts$alpha.f)) opts$alpha.f else 1
  ## Used to use tx.col = adjustcolor(c('black', '#0080ff'), alpha.f=alpha.f)
  royalblue  <- '#4169E1'
  darkorange <- '#FF8C00'
  if(! length(default))
    default <-
      list(tx.pch = 16:17,
           tx.col     = c(royalblue, darkorange),
           tx.linecol = c(royalblue, darkorange),
           nontx.col = adjustcolor(c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
             "#66a61e", "#e6ab02", "#a6761d", "#666666"),
             alpha.f=alpha.f),  ## see colorbrewer2.org
           tx.lty = c(1, 1), tx.lwd = c(1, 2),
           tx.var = '', er.col = NULL, alpha.f = 1,
           denom = c(enrolled=NA, randomized=NA))
  
  if(length(opts)) {
    if(any(names(opts) %nin% names(default)))
      stop(paste('qreport options must be one of the following:',
                 paste(names(default), collapse=' ')))
    default[names(opts)] <- opts
  }
  i <- names(opts$denom) %nin% c('enrolled', 'randomized')
  if(any(i) && sum(opts$denom[i]) != opts$denom['randomized'])
    stop('sum of # subjects randomized to each treatment must = total number randomized')
  if(! length(default$er.col))
    default$er.col <-
      adjustcolor(setdiff(c('red', 'green', "#0080ff", "#ff00ff",
                            "darkgreen", "#ff0000", "orange", "#00ff00",
                            "brown"),
                          default$tx.col)[1 : 2], alpha.f=alpha.f)
  hop[[study]] <- default
  options(qreport = hop)
  invisible()
}

#' Get qreport Options
#'
#' Get qreport options, assigning default values of unspecified options.
#'
#' @param opts character vector containing list of option names to retrieve.  If only one element, the result is a scalar, otherwise a list.  If \code{opts} is not specified, a list with all current option settings is returned.
#' @param study character string specifying an optional study designation
#' @return getching `qreport` options
#' @export
#' @examples
#' \dontrun{
#' getqreportOption('tx.var')
#' }
#' @md
getqreportOption <- function(opts=NULL, study=' ') {
  hop <- getOption('qreport')
  if(! length(hop)) return(hop)
  hop <- hop[[study]]
  if(! length(hop)) return(hop)
  hop <- if(length(opts)) hop[opts] else hop
  if(length(opts) == 1) hop <- hop[[1]]
  hop
}

#' Compute Sample Fractions
#'
#' Uses denominators stored with \code{setqreportOption} along with counts specified to \code{SampleFrac} to compute fractions of subjects in current analysis
#'
#' @param n integer vector, named with \code{"enrolled","randomized"} and optionally also including treatment levels.
#' @param nobsY a result of the the \code{nobsY} Hmisc function
#' @param table set to \code{TRUE} to return as an attribute \code{"table"} a character string containing an HTML table showing the pertinent frequencies created from \code{n} and the \code{denom} option, and if \code{nobsY} is present, adding another table with response variable-specific counts.
#' @param study character string with study ID
#' @export
#' @return named vector of relative sample sizes with an attribute `table` with frequency counts
#' @md
#' @examples
#' setqreportOption(tx.var='treatment', denom=c(enrolled=1000, randomized=800, a=398, b=402))
#' sampleFrac(getqreportOption('denom'))
sampleFrac <- function(n, nobsY=NULL, table=TRUE, study=' ') {
  denom <- getqreportOption('denom', study=study)
  if(any(is.na(denom))) stop('denom must be defined with setqreportOption()')
  if(names(n)[1] != 'enrolled')
    n <- structure(c(n[1], n), names=c('enrolled', names(n)))
  if(all(names(n) %in% c('enrolled', 'randomized')))
    denom <- denom[unique(c('enrolled', names(n)))]
  if(length(n) != length(denom))
    stop('length of n does not equal length of denom')
  if(any(names(n) != names(denom)))
    stop('n does not have same names as denom in the same order')
  f <- n / denom
  if(any(f > 1.)) warning('A sample fraction > 1.0; assuming analysis is to compare randomized and non-randomized subjects\nfraction capped at 1.0')
  f <- pmin(f, 1.)
  if(! table) return(f)
  tab <- data.frame(Category=names(n), N=denom, Used=n)

  if(length(nobsY)) {
    if(length(m <- nobsY$nobsg)) {
      m <- t(m)
      tab2 <- cbind(Variable=rownames(m), as.data.frame(m))
    }
    else {
      m <- nobsY$nobs
      tab2 <- data.frame(Variable=names(m), N=m)
    }
    tab <- list(tab, tab2)
  }
  attr(f, 'table') <- tab
  f
}

#' Draw Needles
#'
#' Create an html base64 string from a png graphic to draw needles for current sample sizes.  Uses colors set by call to \code{setqreportOptions}.
#'
#' @param sf output of \code{sampleFrac}
#' @param study character string specifying study ID
#' @return a base64 representation of a png graphic, suitable for inclusion in html
#' @export
#' @examples
#' setqreportOption(tx.var='treatment', denom=c(enrolled=1000, randomized=800, a=398, b=402))
#' dNeedle(sampleFrac(getqreportOption('denom')))
dNeedle <- function(sf, study=' ') {
  co <- getqreportOption(c('er.col', 'tx.col'), study=study)
  co <- c(co$er.col, co$tx.col)
  tobase64image(pngNeedle(sf, col=co))
}


##' Print to File for Debugging
##'
##' If `options(dumpfile="...")` is set, uses `Hmisc::prn()` to print objects for debugging
##' @param x input to `prn`
##' @param txt text label, defaults to name of `x` argument
##' @author Frank Harrell
##' @return no result, used only for printing debugging information
##' @md
pdumpit <- function(x, txt=as.character(substitute(x))) {
  fi <- .Options$dumpfile
  if(length(fi) && is.character(fi))
    prn(x, txt, file=fi)
  invisible()
}


## fig-subcap does not do what we need so instead of issuing that,
## store scap if there is a label
##' Create Quarto Figure Caption
##'
##' Creates a Quarto label and caption and uses `addCap()` to add to running list of figures
##' @title putQcap
##' @param ... one or more character strings to form the caption
##' @param scap a character string subcaption
##' @param label figure label
##' @return string vector with YAML components `label`, `fig-cap`, `fig-scap`
##' @author Frank Harrell
##' @examples
##' putQcap('First part of caption', 'second part', scap='subcaption', label='xx')
##' @md
putQcap <- function(..., scap=NULL, label=NULL) {
  if(! length(label)) {
    label <- knitr::opts_current$get('label')
    if(length(label)) label <- paste0('fig-', label)
    }
  if(! length(label)) stop('must provide label')
  lcap <- unlist(list(...))
  if(length(lcap)) lcap <- paste(lcap, collapse=' ')
  
  if(! length(lcap) && ! length(scap)) return('')

  a <- addCap(label, lcap, scap)
  if(length(a$label))
    c(paste0('label: ',   a$label),
      paste0('fig-cap: "', a$cap, '"' ),
      if(length(a$scap)) paste0('fig-scap: "', a$scap, '"') )
}

utils::globalVariables(c('Freq', '.group.'))

