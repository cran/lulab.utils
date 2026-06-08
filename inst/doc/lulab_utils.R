## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lulab.utils)

## -----------------------------------------------------------------------------
data("melanoma", package = "boot")
check_cha("status", melanoma)

## -----------------------------------------------------------------------------
round2(c(1.25, -1.25), digits = 1)

## -----------------------------------------------------------------------------
melanoma2 <- melanoma
melanoma2$status <- factor(
  melanoma2$status,
  levels = c(2, 1, 3),
  labels = c("Alive", "Melanoma death", "Non-melanoma death")
)

head(Table1(
  df = melanoma2,
  ycol = "status",
  xcol = c("time", "age", "sex"),
  result_dir = tempdir(),
  verbose = FALSE
))

