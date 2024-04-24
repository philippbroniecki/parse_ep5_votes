# install the package and document so that the functions are also available in
# foreach (once and after changes to parse_votes() or functions it calls)
devtools::document()
devtools::install()

data_dir <- sprintf("%s/", getwd())
overwrite <- FALSE
verbose <- TRUE
start_date <- NULL
end_date <- NULL
cores <- 1
docx_dir <- sprintf("%sraw/", data_dir)
libre_office_path <- "C:/Program Files/LibreOffice/program/soffice.exe"

#  load all functions
devtools::load_all()

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
