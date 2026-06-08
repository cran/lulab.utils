#' Make a Table 1 summary
#'
#' Create a stratified Table 1, save it as an Excel workbook, and return the
#' table as a data frame.
#'
#' @param df A data frame.
#' @param ycol A single character string naming the grouping variable.
#' @param xcol A character vector naming variables to summarize.
#' @param xlabels Optional labels for the groups. Defaults to the observed
#'   levels of `ycol`.
#' @param result_dir Directory where `Table1.xlsx` will be saved.
#' @param verbose Logical; if `TRUE`, print the output path.
#'
#' @return A data frame containing the rendered Table 1.
#' @author Zhen Lu
#' @export
#'
#' @examples
#' \donttest{
#' data("melanoma", package = "boot")
#' melanoma2 <- melanoma
#' melanoma2$status <- factor(
#'   melanoma2$status,
#'   levels = c(2, 1, 3),
#'   labels = c("Alive", "Melanoma death", "Non-melanoma death")
#' )
#' Table1(
#'   df = melanoma2,
#'   ycol = "status",
#'   xcol = c("time", "age", "sex"),
#'   result_dir = tempdir()
#' )
#' }
Table1 <- function(df, ycol, xcol, xlabels = NULL, result_dir, verbose = TRUE) {
  if (missing(df) || missing(ycol) || missing(xcol) || missing(result_dir)) {
    stop("`df`, `ycol`, `xcol`, and `result_dir` are required.", call. = FALSE)
  }
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(ycol) || length(ycol) != 1 || !ycol %in% names(df)) {
    stop("`ycol` must be a single column name in `df`.", call. = FALSE)
  }
  if (!is.character(xcol) || length(xcol) < 1 || !all(xcol %in% names(df))) {
    stop("`xcol` must contain column names in `df`.", call. = FALSE)
  }
  if (!dir.exists(result_dir)) {
    dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  }

  df[[ycol]] <- .lulab_prepare_group(df[[ycol]], xlabels)
  xlabels <- levels(df[[ycol]])

  render_strat <- function(strata, ...) {
    labels <- names(strata)
    n <- vapply(strata, nrow, integer(1))
    ifelse(n == 0, labels, sprintf("%s<br/>(N=%d)", labels, n))
  }

  render_continuous <- function(x) {
    stats <- table1::stats.default(x)
    mean_sd <- sprintf("%.02f \u00B1 %.02f", stats$MEAN, stats$SD)
    median_iqr <- sprintf("%.02f (%.02f, %.02f)", stats$MEDIAN, stats$Q1, stats$Q3)

    if (.lulab_is_normal(x)) {
      c("", `Mean +/- SD` = mean_sd, `Median (Q1, Q3)` = median_iqr)
    } else {
      c("", `Median (Q1, Q3)` = median_iqr, `Mean +/- SD` = mean_sd)
    }
  }

  render_categorical <- function(x) {
    c("", vapply(table1::stats.default(x), function(y) {
      sprintf("%d (%.02f)", y$FREQ, y$PCT)
    }, character(1)))
  }

  pvalue_column <- function(x, name, ...) {
    test <- .lulab_group_test(x)
    rendered <- table1::render.default(x = .lulab_combine_strata(x), name = name, ...)
    c(.lulab_format_pvalue(test$p_value), rep("", max(length(rendered) - 1, 0)))
  }

  formula <- stats::as.formula(paste("~", paste(xcol, collapse = "+"), "|", ycol))
  result <- table1::table1(
    formula,
    data = df,
    droplevels = FALSE,
    render.strat = render_strat,
    render.continuous = render_continuous,
    render.categorical = render_categorical,
    extra.col = list(`P-value` = pvalue_column),
    overall = "Total"
  )

  result <- as.data.frame(result, stringsAsFactors = FALSE)
  empty_names <- which(names(result) == "")
  if (length(empty_names) > 0L) {
    names(result)[empty_names[1]] <- "P-value"
  }
  output_file <- file.path(result_dir, "Table1.xlsx")
  openxlsx::write.xlsx(result, file = output_file, asTable = TRUE)

  if (isTRUE(verbose)) {
    cat(sprintf("Table1.xlsx has been saved in your specified folder of:\n%s\n", result_dir))
  }

  result
}

.lulab_prepare_group <- function(x, xlabels = NULL) {
  if (is.null(xlabels)) {
    if (is.factor(x)) {
      xlabels <- levels(droplevels(x))
    } else {
      xlabels <- unique(stats::na.omit(as.character(x)))
    }
  }
  factor(x, levels = xlabels)
}

.lulab_combine_strata <- function(x) {
  if (any(vapply(x, is.factor, logical(1)))) {
    levels <- unique(unlist(lapply(x, levels), use.names = FALSE))
    factor(unlist(lapply(x, as.character), use.names = FALSE), levels = levels)
  } else {
    unlist(x, use.names = FALSE)
  }
}

.lulab_is_normal <- function(x) {
  x <- stats::na.omit(x)
  if (!is.numeric(x) || length(unique(x)) < 3L) {
    return(FALSE)
  }
  if (length(x) > 5000L) {
    return(stats::ks.test(scale(x), "pnorm")$p.value > 0.05)
  }
  stats::shapiro.test(x)$p.value > 0.05
}

.lulab_group_test <- function(x) {
  is_numeric_variable <- all(vapply(x, is.numeric, logical(1)))
  values <- if (is_numeric_variable) {
    unlist(x, use.names = FALSE)
  } else {
    unlist(lapply(x, as.character), use.names = FALSE)
  }
  group <- rep(seq_along(x), lengths(x))
  ok <- !is.na(values) & !is.na(group)
  values <- values[ok]
  group <- factor(group[ok])

  if (length(values) == 0L || nlevels(group) < 2L) {
    return(.lulab_test_result(NA_real_, NA_real_, "not enough groups"))
  }

  if (is_numeric_variable) {
    .lulab_numeric_group_test(values, group)
  } else {
    .lulab_categorical_group_test(values, group)
  }
}

.lulab_numeric_group_test <- function(values, group) {
  group_sizes <- table(group)
  if (any(group_sizes < 2L) || length(unique(values)) < 2L) {
    return(.lulab_test_result(NA_real_, NA_real_, "not enough observations"))
  }

  normal_by_group <- tapply(values, group, .lulab_is_normal)
  all_normal <- all(unlist(normal_by_group), na.rm = TRUE)

  if (nlevels(group) == 2L) {
    if (all_normal) {
      variance_p <- tryCatch(stats::var.test(values ~ group)$p.value, error = function(e) NA_real_)
      var_equal <- is.na(variance_p) || variance_p > 0.05
      test <- stats::t.test(values ~ group, var.equal = var_equal)
      .lulab_test_result(unname(test$p.value), unname(test$statistic), ifelse(var_equal, "two-sample t-test", "Welch t-test"))
    } else {
      test <- stats::wilcox.test(values ~ group, exact = FALSE)
      .lulab_test_result(unname(test$p.value), unname(test$statistic), "Wilcoxon rank-sum test")
    }
  } else if (all_normal) {
    variance_p <- tryCatch(stats::bartlett.test(values ~ group)$p.value, error = function(e) NA_real_)
    if (is.na(variance_p) || variance_p > 0.05) {
      model <- stats::aov(values ~ group)
      summary_model <- summary(model)[[1]]
      .lulab_test_result(summary_model[["Pr(>F)"]][1], summary_model[["F value"]][1], "ANOVA")
    } else {
      test <- stats::kruskal.test(values ~ group)
      .lulab_test_result(unname(test$p.value), unname(test$statistic), "Kruskal-Wallis test")
    }
  } else {
    test <- stats::kruskal.test(values ~ group)
    .lulab_test_result(unname(test$p.value), unname(test$statistic), "Kruskal-Wallis test")
  }
}

.lulab_categorical_group_test <- function(values, group) {
  values <- factor(values)
  tbl <- table(values, group)
  if (nrow(tbl) < 2L || ncol(tbl) < 2L || any(rowSums(tbl) == 0L) || any(colSums(tbl) == 0L)) {
    return(.lulab_test_result(NA_real_, NA_real_, "not enough categories"))
  }

  chi <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE))
  use_fisher <- any(chi$expected < 5) || sum(tbl) < 40L

  if (use_fisher) {
    fisher <- tryCatch(
      stats::fisher.test(tbl),
      error = function(e) stats::fisher.test(tbl, simulate.p.value = TRUE, B = 2000)
    )
    .lulab_test_result(unname(fisher$p.value), NA_real_, "Fisher exact test")
  } else {
    .lulab_test_result(unname(chi$p.value), unname(chi$statistic), "Pearson chi-square test")
  }
}

.lulab_test_result <- function(p_value, statistic, test_name) {
  list(p_value = p_value, statistic = statistic, test_name = test_name)
}

.lulab_format_pvalue <- function(p_value) {
  if (length(p_value) == 0L || is.na(p_value)) {
    return("")
  }
  if (p_value < 0.001) {
    "<0.001"
  } else {
    format(round(p_value, 3), nsmall = 3)
  }
}
