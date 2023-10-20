# Build --> Install and Restart
# [Edit this] path to the Workbench for your computer.
my_wb <- "~/Desktop/workbench"

library(testthat)
library(ciftiTools)
if (interactive()) { ciftiTools.setOption("wb_path", my_wb) }
library(fMRItools)

need_pkg <- c("ciftiTools", "fMRIscrub", "ggplot2", "cowplot", "fastICA")
have_pkgs <- vapply(need_pkg, function(q){requireNamespace(q, quietly=TRUE)}, FALSE)

if (all(have_pkgs)) {
  library(ciftiTools)
  if (interactive()) { ciftiTools.setOption("wb_path", my_wb) }

  library(fMRIscrub)
  library(ggplot2)
  library(cowplot)
  library(fastICA)

  test_check("fMRIscrub")
}