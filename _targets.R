library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

tar_option_set(
  packages = c("tibble"),
  format = "rds",
  workspace_on_error = TRUE
)

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Load functions for the pipeline
source("R/tar_slides.R")

 list(
 ## Xaringan slides

  ### Knit xaringan slides ----
  #
  # Use dynamic branching to get a list of all .Rmd files in slides/ and knit them
  #
  # The main index.qmd page loads xaringan_slides as a target to link it as a dependency
  # Use dynamic branching to get a list of all knitted slide .html files and
  # convert them to PDF with pagedown
  #
  # The main index.qmd page loads xaringan_pdfs as a target to link it as a dependency
  tar_files(xaringan_files, list.files(here_rel("diapos"),
                                       pattern = "\\.Rmd",
                                       full.names = TRUE)),
  tar_target(xaringan_slides,
             render_xaringan(xaringan_files),
             pattern = map(xaringan_files),
             format = "file"),           
             
    tar_files(quarto_files, list.files(here_rel("diapos"),
                                       pattern = "\\.qmd",
                                       full.names = TRUE)),
  tar_target(slide_pdfs,
             quarto::quarto_render(quarto_files),
             pattern = map(quarto_files),
             format = "file"),

#  tar_files(path_eval_quarto,
#   list.files(here_rel("evaluations"),
#               pattern = "\\.qmd",
#              full.names = TRUE)
#  ),
#  tar_target(evaluations_pdfs,
#             quarto_to_pdf(path_eval_quarto),
#             pattern = map(path_eval_quarto)),

  # tar_files(path_exercice_quarto,
  #           list.files(here_rel("exercices"),
  #                      pattern = "\\.qmd",
  #                      full.names = TRUE)
  # ),
  # tar_target(exercices_pdfs,
  #            quarto_to_pdf(path_exercice_quarto),
  #            pattern = map(path_exercice_quarto)),
  # Class schedule file ----
  tar_target(schedule_file, here_rel("files", "horaire.csv"), format = "file"),


  ## Build site ----
  tar_quarto(site, path = ".")
)
