# Internal helpers shared by SEDA factorial analyses.
#
# FactoMineR::dimdesc() computes and orders the statistics. These helpers only
# normalise the result so it can be displayed in typed jamovi tables.

.seda_empty_dimdesc <- function() {
  list(
    continuous = data.frame(
      dimension = character(), variable = character(),
      correlation = numeric(), p = numeric(), n = integer(),
      stringsAsFactors = FALSE
    ),
    categorical = data.frame(
      dimension = character(), variable = character(),
      r2 = numeric(), p = numeric(),
      stringsAsFactors = FALSE
    ),
    categories = data.frame(
      dimension = character(), category = character(),
      estimate = numeric(), p = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

.seda_dim_label <- function(x, fallback) {
  x <- if (length(x) == 0 || is.na(x) || !nzchar(x)) fallback else x
  number <- sub(".*?([0-9]+)$", "\\1", x)
  if (!identical(number, x) && grepl("^[0-9]+$", number))
    return(paste("Dimension", number))
  x
}

.seda_dimdesc_frame <- function(x) {
  if (is.null(x))
    return(NULL)

  ans <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(ans) || nrow(ans) == 0)
    return(NULL)

  if (is.null(rownames(ans)))
    rownames(ans) <- as.character(seq_len(nrow(ans)))
  ans
}

.seda_dimdesc_column <- function(x, candidates, fallback = NULL) {
  if (is.null(x) || ncol(x) == 0)
    return(numeric())

  normalised <- tolower(gsub("[^[:alnum:]]", "", names(x)))
  wanted <- tolower(gsub("[^[:alnum:]]", "", candidates))
  hit <- match(wanted, normalised, nomatch = 0L)
  hit <- hit[hit > 0L]

  if (length(hit) > 0)
    return(suppressWarnings(as.numeric(x[[hit[[1]]]])))
  if (!is.null(fallback) && fallback >= 1 && fallback <= ncol(x))
    return(suppressWarnings(as.numeric(x[[fallback]])))
  rep(NA_real_, nrow(x))
}

.seda_dimdesc_n <- function(dimdesc, variables) {
  x <- tryCatch(dimdesc$call$X, error = function(e) NULL)
  if (is.null(x))
    return(rep(NA_integer_, length(variables)))

  vapply(variables, function(variable) {
    if (!variable %in% names(x))
      return(NA_integer_)
    as.integer(sum(!is.na(x[[variable]])))
  }, integer(1))
}

.seda_category_label <- function(x) {
  sub("\\s*=\\s*", " = ", x, perl = TRUE)
}

.seda_tidy_dimdesc <- function(dimdesc) {
  out <- .seda_empty_dimdesc()
  if (is.null(dimdesc) || !is.list(dimdesc))
    return(out)

  dim_names <- names(dimdesc)
  if (is.null(dim_names))
    dim_names <- rep("", length(dimdesc))

  for (i in seq_along(dimdesc)) {
    if (tolower(dim_names[[i]]) == "call")
      next

    dimension <- .seda_dim_label(dim_names[[i]], paste("Dimension", i))
    description <- dimdesc[[i]]
    if (is.null(description) || !is.list(description))
      next

    block_names <- names(description)
    if (is.null(block_names))
      block_names <- rep("", length(description))

    for (j in seq_along(description)) {
      block <- .seda_dimdesc_frame(description[[j]])
      if (is.null(block))
        next

      kind <- tolower(gsub("[^[:alnum:]]", "", block_names[[j]]))
      labels <- rownames(block)

      if (startsWith(kind, "quanti")) {
        out$continuous <- rbind(out$continuous, data.frame(
          dimension = rep(dimension, nrow(block)),
          variable = labels,
          correlation = .seda_dimdesc_column(block, "correlation", 1L),
          p = .seda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          n = .seda_dimdesc_n(dimdesc, labels),
          stringsAsFactors = FALSE
        ))
      } else if (startsWith(kind, "quali")) {
        out$categorical <- rbind(out$categorical, data.frame(
          dimension = rep(dimension, nrow(block)),
          variable = labels,
          r2 = .seda_dimdesc_column(block, c("R2", "R.square", "Rsquared"), 1L),
          p = .seda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          stringsAsFactors = FALSE
        ))
      } else if (startsWith(kind, "category")) {
        out$categories <- rbind(out$categories, data.frame(
          dimension = rep(dimension, nrow(block)),
          category = .seda_category_label(labels),
          estimate = .seda_dimdesc_column(block, "Estimate", 1L),
          p = .seda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  rownames(out$continuous) <- NULL
  rownames(out$categorical) <- NULL
  rownames(out$categories) <- NULL
  out
}

.seda_fill_dimdesc_table <- function(table, data) {
  has_rows <- !is.null(data) && nrow(data) > 0
  table$setVisible(visible = has_rows)
  if (!has_rows)
    return(invisible(NULL))

  for (i in seq_len(nrow(data))) {
    table$addRow(rowKey = i)
    table$setRow(rowNo = i, values = as.list(data[i, , drop = FALSE]))
  }
  invisible(NULL)
}
