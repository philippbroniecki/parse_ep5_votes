# pacakge list
# Imports:
#   rlang (>= 0.4.9),
# 	dplyr (>= 1.0.4),
# 	tibble (>= 3.0.4),
# 	xml2 (>= 1.3.2),
# 	purrr (>= 0.3.4),
# 	stringr (>= 1.4.0),
# 	stringi (>= 1.7.12),
# 	docxtractr (>= 0.6.5),
# 	doRNG (>= 1.8.2),
# 	foreach (>= 1.5.2),
# 	doSNOW (>= 1.0.20),
# 	quanteda (>= 3.2.4),
# 	tidyr (>= 1.3.0),
# 	doParallel (>= 1.0.17),
# 	rvest (>= 1.0.3),
# 	lubridate (>= 1.9.2),
# 	crayon (>= 1.5.2),
# 	cld2 (>= 1.2.4)
library(foreach)
library(doParallel)
library(tidyverse)

data_dir <- sprintf("%s/", getwd())
overwrite <- FALSE
verbose <- TRUE
start_date <- NULL
end_date <- NULL
cores <- 1
docx_dir <- sprintf("%sraw/", data_dir)
libre_office_path <- "C:/Program Files/LibreOffice/program/soffice.exe"

source("utils.R")
source("fill_in_cells.R")
source("parse_votes.R")
source("parse_vote_results.R")
source("search_votes.R")
source("check_votes_meta.R")
source("chk_date_tally_consistency.R")
source("clean_votes_01.R")
source("clean_votes_02.R")
source("clean_votes_03.R")
source("split_vote_function.R")
source("add_rcv_type_var.R")

parse_votes(
  data_dir = data_dir,
  overwrite = overwrite,
  verbose = verbose,
  cores = cores
)

clean_votes_01(data_dir = data_dir, overwrite = overwrite, verbose = verbose)
clean_votes_02(data_dir = data_dir, verbose = verbose)
clean_votes_03(data_dir = data_dir, verbose = verbose)


# check the 2 datasets
load(sprintf("%s/parsed/votes.RData", data_dir))
load(sprintf("%s/parsed/votes_meta.RData", data_dir))
