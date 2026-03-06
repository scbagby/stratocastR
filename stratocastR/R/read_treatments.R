# Generated from create-stratocastR.Rmd: do not edit by hand

#' Read in the treatment file
#'
#' @param filename (character) Plate layout file (.csv, .xlsx), following the
#'   `make_layout_file()` format
#' @return A tidy dataframe with one row for each well and two columns:
#'   - well:  the well ID, e.g., "A1"
#'   - treatment:  the experimental condition in the well
read_treatments <- function(treatments) {
    filetype <- xfun::file_ext(treatments)
    stopifnot("Plate layout file must be either .csv or .xlsx" =
                  filetype %in% c("csv", "xlsx"))
    if (filetype == "csv") {
        metadata <- readr::read_csv(treatments, col_types = cols())
    } else {
        metadata <- readxl::read_xlsx(treatments)
    }
    metadata <- {metadata |>
                     tidyr::pivot_longer(-platerow,
                                         names_to = "platecol",
                                         names_prefix = "c",
                                         values_to = "treatment") |>
                     dplyr::mutate(well = paste0(platerow, platecol)) |>
                     dplyr::select(well, treatment)}
    return(metadata)
}

#' Read in the data file and optionally merge in sample info.
#' 
#' @param filename (character) Cerillo CSV file
#' @param treatments (character) Plate layout CSV file, following the `make_layout_file()` format
#' @return A tidy dataframe with one row for each well's data and metadata at each timepoint
#' @export
read_platedata <- function(filename, treatments = NULL) {
    stopifnot("Data file must be a .csv" = xfun::file_ext(filename) == "csv")
    data <- {readr::read_csv(filename, col_types = cols(), skip = 12,
                             name_repair = "unique_quiet") |>
                 dplyr::rename("datetime" = `Date and Time`,
                               "duration.h" = `Duration (Hours)`,
                               "duration.min" = `Duration (Minutes)`,
                               "unixtime" = `UNIX Timestamp`,
                               "airtemp.C" = `Air Temp`) |>
                 tidyr::pivot_longer(!(datetime:airtemp.C),
                                     names_to = "well", values_to = "absorbance") |>
                 dplyr::mutate(timestamp = anytime(unixtime)) |>
                 dplyr::filter(!grepl("\\.\\.\\.", well)) |> 
                 dplyr::select(timestamp, duration.h, duration.min, airtemp.C,
                               well, absorbance)}
    if (!is.null(treatments))
    {
        metadata <- read_treatments(treatments)
        data <- dplyr::left_join(data, metadata)
    }
    return(data)       
}

#' Get the measurement interval length and convert to minutes
#'
#' @param filename (character) Cerillo CSV file
#' @return The interval between measurements (in minutes) in the experiment
#' @export
get_interval_min <- function(filename) {
    stopifnot("Data file must be a .csv" = xfun::file_ext(filename) == "csv")
    header <- readLines(filename, n = 12)
    interval <- header[which(grepl("Interval", header))]
    stopifnot("No interval metadata found; check file header" =
                  length(interval) != 0)
    interval_min <- as.numeric(sub(".*:,", "", interval)) / 60
    return(interval_min)
}
