#' @importFrom roxygen2 roxy_tag_parse roxy_tag_rd
#'
NULL

#' Document Dataset Generation
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @name exdata
#' @rdname exdata
#'
#' @examples
#' #' Dataset Documentation File
#' #'
#' #' Simply add an `@exdata` tag to the Roxygen documentation for a dataset.
#' #' For its one and only argument, it takes the name of the data-raw file
#' #' used to generate the dataset, without extensions
#' #'
#' #' @format A dataset
#' #'
#' #' @exdata dataset
#' #'
#' "dataset"
#'
NULL

#' @return \code{roxy_tag_parse}: A \code{\link[roxygen2]{roxy_tag}} object
#' with a value being a two-length list:
#' \describe{
#'  \item{\code{name}}{Name of the dataset}
#'  \item{\code{text}}{Code to generate the dataset}
#' }
#'
#' @importFrom rprojroot find_root
#' @importFrom roxygen2 roxy_tag_warning tag_name
#'
#' @rdname exdata
#' @method roxy_tag_parse roxy_tag_exdata
#' @export
#'
roxy_tag_parse.roxy_tag_exdata <- function(x) {
  x <- tag_name(x = x)
  root <- find_root(criterion = 'DESCRIPTION')
  data.raw <- list.files(
    path = file.path(root, 'data-raw'),
    pattern = paste0(paste0('^', x$val, '\\.[Rr]$')),
    full.names = TRUE
  )
  if (length(x = data.raw) != 1) {
    return(roxy_tag_warning(x = x, "no raw data file for ", x$val))
  }
  parsed <- tryCatch(
    expr = parse_data_raw(con = data.raw),
    error = function(e) {
      return(e)
    }
  )
  if (inherits(x = parsed, what = 'condition')) {
    return(roxy_tag_warning(x = x, parsed$message))
  }
  text <- paste(
    paste('# Generate the', x$val, 'dataset'),
    'if (FALSE) {',
    indent(text = parsed),
    '}\n',
    sep = '\n'
  )
  x$val <- list(
    name = x$val,
    text = text
  )
  return(x)
}

#' @return \code{roxy_tag_rd}: An \code{\link[roxygen2]{rd_section}} object of
#' type \dQuote{examples} with the value being the code to generate the dataset
#'
#' @importFrom roxygen2 rd_section
#'
#' @rdname exdata
#' @method roxy_tag_rd roxy_tag_exdata
#' @export
#'
roxy_tag_rd.roxy_tag_exdata <- function(x, base_path, env) {
  return(rd_section(type = 'examples', value = x$val$text))
}

indent <- function(text, n = 2L, type = c('space', 'tab')) {
  type <- match.arg(arg = type)
  sep <- paste(
    rep_len(x = c(space = ' ', tab = '\t')[type], length.out = n),
    collapse = ''
  )
  text <- unlist(x = strsplit(x = text, split = '\n'))
  for (i in seq_along(along.with = text)) {
    line <- text[i]
    text[i] <- ifelse(
      test = nchar(x = line),
      yes = paste0(sep, line),
      no = line
    )
  }
  return(paste(text, collapse = '\n'))
}

parse_data_raw <- function(con) {
  directives <- paste('## exdata:', c('begin', 'end'))
  # Read in the file
  text <- trimws(x = readLines(con = con), which = 'right')
  if (!length(x = text)) {
    return(NULL)
  }
  # Find start/end directives
  ind <- seq_len(length.out = length(x = text))
  for (i in ind) {
    names(x = ind)[i] <- switch(
      EXPR = text[i],
      '## exdata: begin' = 'start',
      '## exdata: end' = 'end',
      NA_character_
    )
  }
  ind <- ind[!is.na(x = names(x = ind))]
  # Handle cases without start/end directives
  if (!length(x = ind)) {
    ind <- c(start = 1L, end = length(x = text))
  }
  # Handle missing start/end directives
  ind <- Filter(f = function(x) x >= 1 && x <= length(x = text), x = ind)
  if (!grepl(pattern = '^start', x = names(x = ind)[1])) {
    ind <- append(x = ind, values = c(start = 1L), after = 0L)
  }
  if (!grepl(pattern = '^end', x = names(x = ind)[length(x = ind)])) {
    ind <- append(x = ind, values = c(end = length(x = text)))
  }
  # Handle duplicate start/end directives
  ind.remove <- vapply(
    X = seq_along(along.with = ind),
    FUN = function(i) {
      pattern <- ifelse(
        test = grepl(pattern = '^start', x = names(x = ind)[i]),
        yes = '^start',
        no = '^end'
      )
      return(isTRUE(x = grepl(pattern = pattern, x = names(x = ind)[i - 1L])))
    },
    FUN.VALUE = logical(length = 1L)
  )
  ind <- ind[!ind.remove]
  # Create text chunks
  ind <- split(x = ind, f = names(x = ind))[c('start', 'end')]
  if (length(x = ind$start) != length(x = ind$end)) {
    stop("Failed to identify start/end directives")
  }
  for (i in seq_along(along.with = ind$start)) {
    if (ind$start[i] >= ind$end[i]) {
      stop("Mismatched start/end directives")
    }
  }
  chunks <- vapply(
    X = seq_along(along.with = ind$start),
    FUN = function(i) {
      slice <- seq.int(from = ind$start[i], to = ind$end[i])
      chunk <- text[slice]
      chunk <- chunk[!chunk %in% directives]
      chunk <- paste(chunk, collapse = '\n')
      return(trimws(x = chunk, which = 'right'))
    },
    FUN.VALUE = character(length = 1L),
    USE.NAMES = FALSE
  )
  chunks <- Filter(f = nchar, x = chunks)
  return(paste(chunks, collapse = '\n\n'))
}
