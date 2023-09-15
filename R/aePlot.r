#' Adverse Event Plot
#'
#' Generates graphics for binary event proportions
#' 
#' Generates dot charts showing proportions of subjects having events (at any time).  Events can be categorized by a single level or by major and minor levels (e.g., body system and preferred terms).  When there are two treatments, half-width CLs of treatment differences are drawn, centered at the midpoint of the two proportions, and CLs for differences appear in hover text.   Input data must contain one record per event, with this record containing the event name.  If there is more than one event of a given type per subject, unique subject ID must be provided.  Denominators come from \code{qreport} options when computing event incidence proportions.  Instead, when a named vector \code{exposure} is specified, with names equal to the treatments, \code{exposure} is used as the denominator so that the exponential distribution hazard rate is computed, i.e., events per unit of exposure time.  When a subject has only one event of each type, the usual interpretation holds.  When a subject has multiple events, the estimate is events per person per time unit.  A character variable \code{expunit} defines the time units.   It is assumed that only randomized subjects are included in the dataset.  Whenever the number of events of a given type is zero for a group, the event frequency is changed to 0.5 so that one may compute confidence intervals on the log scale as well as hazard ratios.
#'
#' For an example with output see https://hbiostat.org/rflow/descript.html#adverse-event-chart/
#'
#' @param formula a formula with one or two left hand variables (the first representing major categorization and the second minor), and 1-2 right hand variables.  One of the right hand variables may be enclosed in \code{id()} to indicate the presence of a unique subject ID.  The remaining variable is treatment.
#' @param data input data frame
#' @param subset subsetting criteria
#' @param na.action function for handling \code{NA}s when creating analysis frame
#' @param exposure a numeric vector whose length is the number of treatments, with names equal to the treatment names
#' @param expunit character string specifying the time units for \code{exposure}
#' @param study character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{setqreportOption}}.  Single study reports just use \code{study=' '}.
#' @param refgroup a character string specifying which treatment group is subtracted when computing risk differences.  If there are two treatments the default is the first one listed in \code{qreport options}.
#' @param minincidence a number between 0 and 1 specifying the minimum incidence in any stratum that must hold before an event is included in the summary.  When \code{exposure} is given, \code{minincidence} applies to the hazard rate.
#' @param conf.int confidence level for difference in proportions (passed to \code{dotchartpl})
#' @param etype a character string describing the nature of the events, for example \code{"adverse events"}, \code{"serious adverse events"}.  Used in figure captions.
#' @param popts a list of additional options to pass to \code{dotchartpl}
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used.
#' @param tail a character string to add to end of automatic caption
#' @param size default is standard text body width.  Set to \code{"wide"} to render plot with \code{column: page-inset-left}.
#' @param label label for figure.  \code{fig-} is placed in front of this label.  Default uses the name of the code chunk.  If a label is defined by the time the graph is produced that label will be used instead of the code chunk.
#' @return no return value, called for knitting with \code{knitr}
#' @author Frank Harrell
#' @export
#' @importFrom Formula Formula model.part
#' @examples
#' # See test.Rnw in tests directory

aePlot <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                   exposure=NULL, expunit='',
                   study=' ', refgroup=NULL,
                   minincidence=0,
                   conf.int=0.95,
                   etype='adverse events',
                   head=NULL, tail=NULL,
                   size=c('regular', 'wide'),
                   popts=NULL, label=NULL) {

  size <- match.arg(size)
  popts <- c(popts, list(colors=getqreportOption('tx.col', study=study)))

  smaller2 <- function(x) paste0('<small><small>', x, '</small></small>')
  
  form <- Formula::Formula(formula)
  environment(form) <- new.env(parent = environment(form))
  en <- environment(form)
  assign(envir = en, 'id', function(x) x)

  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
   else model.frame(form, data=data, na.action=na.action)
  X <- model.part(form, data=Y, rhs=1)
  Y <- model.part(form, data=Y, lhs=1)
  nY <- ncol(Y)

  rhs <- terms(form, rhs=1, specials='id')
  sr  <- attr(rhs, 'specials')
  ## specials counts from lhs variables
  wid <- sr$id
  if(length(wid)) wid <- wid - nY

  major <- NULL
  if(nY > 1) major <- Y[[1]]
  event <- Y[[nY]]
  id <- 1 : length(event)
  nX <- ncol(X)
  gname <- glabel <- ''
  if(nX > 1 + length(wid))
    stop('cannot have more than one right hand variable other than id variable')
  if(length(wid))  id <- X[[wid]]
  j <- setdiff(1 : nX, wid)
  if(length(j)) {
    group <- X[[j]]
    gname <- names(X)[j]
    glabel <- label(group, default=gname)
  } else {
    group <- factor(rep('', length(event)))
    gname <- glabel <- ''
  }

  expos <- length(exposure) > 0
  event <- as.factor(event)
  levels(event) <- upFirst(levels(event))
  uevent <- levels(event)
  nue    <- length(uevent)
  N      <- getqreportOption('denom', study=study)
  n      <- if(expos) exposure
               else
                 N[setdiff(names(N), c('enrolled', 'randomized'))]
  groups <- names(n)
  group  <- as.character(group)
  if(length(groups) == 2 && ! length(refgroup)) refgroup <- groups[1]

  ## For proportions count number of subjects having >= 1 events of
  ## a certain type
  ## For hazards, count multiple events per subject
  g <- function(i) {
      idi   <- id[i]
      num   <- length(unique(idi))
      denom <- n[group[i[1]]]
      if(num > denom) return(c(.num=unname(num), .denom=unname(denom),
                               .lower=NA, .upper=NA))
      ci <- binconf(num, denom, method='wilson', alpha = 1. - conf.int)
      c(.num=num, .denom=unname(denom),
        .lower=unname(ci[, 'Lower']),
        .upper=unname(ci[, 'Upper']))
  }

  zcrit <- qnorm((1 + conf.int) / 2)
  
  if(expos)
    g <- function(i) {
      num   <- length(i)
      denom <- unname(exposure[group[i[1]]])
      selog <- 1. / num
      logh  <- log(num / denom)
      c(.num=num, .denom=denom,
        .lower=exp(logh - zcrit * selog),
        .upper=exp(logh + zcrit * selog))
      }

  z <- summarize(1 : length(group),
                 if(length(major)) list(major=major, event=event, group=group)
                 else
                   list(event=event, group=group),
                 g, stat.name=NULL)

  popts$col    <- popts$colors
  popts$colors <- NULL
  xl <- if(expos) paste('Events Per Person', upFirst(expunit)) else 'Proportion'

  fun <- ifun <- function(x) x
  if(expos) {
    fun  <- exp
    ifun <- log
  }

  eu <- expunit
  if(eu != '' && substring(eu, nchar(eu)) != 's')
    eu <- paste0(eu, 's')
  p <- with(z,
            do.call('dotchartpl',
                    c(list(ifun(.num / .denom), major=major,
                           minor=event,
                           group=group, num=.num, denom=.denom,
                           numlabel=if(expos) 'events' else '',
                           denomlabel=if(expos) eu else '',
                           lower=ifun(.lower), upper=ifun(.upper),
                           fun=fun, ifun=ifun,
                           op=if(expos) '/' else '-',
                           refgroup=refgroup, conf.int=conf.int,
                           minkeep=ifun(minincidence),
                           xlab=xl),
                      popts)))

  rem <- sub(' :', '', attr(p, 'levelsRemoved'))
  small <- length(rem)
  
  if(! length(head))
    head <- paste(if(expos) 'Rate of' else 'Proportion of', etype,
                  'by', upFirst(glabel, lower=TRUE),
                  if(expos) 'sorted by descending hazard rate ratio' else
                            'sorted by descending risk difference')
  if(length(minincidence) && minincidence > 0 && small > 0)
    head <- paste0(head, '. ', small, ' events with less than ',
                   minincidence,
                   if(expos) ' rate' else ' incidence',
                   ' in all groups are not shown',
                   if(small < 11) smaller2(paste0(' (',
                                           paste(rem, collapse=', '), ')')),
                   '.')

  shortcap <- paste(if(expos) 'Rate of' else 'Proportion of',
                    etype, 'by',
                    upFirst(glabel, lower=TRUE))

  ned <- function(used) {
    sf <- sampleFrac(used, study=study)
    tab <- attr(sf, 'table')
    structure(dNeedle(sf, study=study),
              table=knitr::kable(tab, format='html', row.names=FALSE))
  }
  extra <- function(x) c(x, '<br><br>', attr(x, 'table'))

  N[1] <- N[2]    # assume used only randomized subjects
                  # N[2] out of original N[1] subjects

  makecolmarg(extra(ned(N)), type='cat')
  caps <- putQcap(head, scap=shortcap, label=label)
  ## May want to add 'fig-location: margin'
  if(size == 'wide') caps <- c('column: page-inset-left', caps)

  ge <- .GlobalEnv
  assign('.aePlot.', p, envir=ge)
  k <- makecodechunk('.aePlot.', callout=caps)
  cat(knitr::knit(text=k, quiet=TRUE))
}
