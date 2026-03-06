# Generated from create-stratocastR.Rmd: do not edit by hand

#' Define how treatment conditions should feed into background subtraction, as test wells or as blanks
#'
#' @param test (character) The treatment group that should be corrected by background subtraction;
#'   should match a `treatment` value in the plate layout
#' @param blanks (character) The treatment group that should be used as the background to subtract;
#'   should match a `treatment` value in the plate layout
#' @param ignore (character; defaults to NULL) Any treatment group(s) that should be ignored in analysis
#' @param treatments (character) Plate layout CSV file, following the `make_layout_file()` format
#' @return A dataframe with one row per well and three columns:
#'   - well:  the well ID, e.g., "A1"
#'   - bgtype:  the categorical column defining each well as "blank", "test", or "ignore".  Blank wells
#'     are used to determine background; test wells have background subtracted from them; ignore wells
#'     are ignored.
#'   - bggroup:  a column defining groups within which background subtraction should be performed.
#'     Here, 1 for all test or blank wells, otherwise NA.
#' @seealso background_by_wells() for defining more complex background-subtraction rules with multiple
#'   groups
#' @export
background_by_treatment <- function(test, blanks, ignore = NULL, treatments) {
    bg <- {read_treatments(treatments) |>
               mutate(bgtype = dplyr::case_when(treatment == test ~ "test",
                                                treatment == blanks ~ "blank",
                                                treatment %in% ignore ~ "ignore",
                                                TRUE ~ NA),
                      bggroup = dplyr::case_when(treatment %in% c(test, blanks) ~ 1,
                                                 TRUE ~ NA))}
    if (any(is.na(bg$bgtype))) {warning("Not all treatments were assigned background-type values")}
    return(bg)
}

#' Create a blank template file for defining background layout
#'
#' @param filename (character) Destination filename, with .xlsx or .csv extension
#' @return If filename's extension is .xlsx, an Excel file with two sheets
#'   - `layout`, containing the background layout
#'   - `params`, defining the strings used in the layout
#'   If filename's extension is .csv, a .csv file with header lines defining the strings used
#'   in the layout and a table containing the layout.
#' @export
make_bg_file <- function(filename) {
    filetype <- xfun::file_ext(filename)
    stopifnot("Plate layout file must be either .csv or .xlsx" =
                  filetype %in% c("csv", "xlsx"))
    template <- {expand.grid(platerow = LETTERS[1:8], platecol = paste0("c", 1:12),
                             val = NA) |>
                     tidyr::pivot_wider(names_from = platecol, values_from = val)}
    header <- c("delim", "blank", "test", "ignore")
    instructions <- c("# Fill in each well's background group and well type, separated",
                      "# by the delimiter (e.g., '1;b': group 1, semicolon delimiter, well type b).",
                      "# Within a group, test wells will be background-corrected using blank wells.")
    if (filetype == "csv") {
        readr::write_csv(template, filename)
        proc <- readLines(filename)
        header <- paste0(header, ": ")
        writeLines(c("# Fill in the delimiter character (not a comma!) and the code for each well type.",
                     header,
                     instructions,
                     proc), filename)
    } else {
        params <- data.frame(parameter = header, value = rep("", 4),
                             instructions = c("Fill in the delimiter character",
                                              "and the code for each well type", "", ""))
        template <- dplyr::bind_rows(template, data.frame(platerow = instructions))
        writexl::write_xlsx(list(layout = template, params = params), filename)
    }
}

#' Define how wells should feed into background subtraction, as test wells or as blanks
#'
#' @param bg (character) Background layout file (.csv or .xlsx), following the
#'   `make_bg_file()` format. Values in the file should take the form `group;type` (e.g., 1;B,
#'   1;blank, 1;test); the delimiter between group and type can be specified via `bgdelim`
#' @return A dataframe with one row per well and three columns:
#'   - well:  the well ID, e.g., "A1"
#'   - bgtype:  the categorical column defining each well as "blank", "test", or "ignore".  Blank wells
#'     are used to determine background; test wells have background subtracted from them; ignore wells
#'     are ignored.
#'   - bggroup:  a column defining groups within which background subtraction should be performed.  E.g.,
#'     if two different media were used in the plate, background subtraction should be performed 
#'     separately for wells using each medium.
#' @seealso background_by_treatment() for defining simpler background-subtraction rules with a single
#'   group
#' @export
background_by_wells <- function(bgfile) {
    bgfiletype <- xfun::file_ext(bgfile)
    stopifnot("Background layout file must be either .csv or .xlsx" =
                  bgfiletype %in% c("csv", "xlsx"))
    if (bgfiletype == "csv") {
        bg <- readr::read_csv(bgfile, skip = 4, comment = "#", col_types = cols())
        bgdelim <- grep("delim", readLines(bgfile, n = 4), value = TRUE) |> sub("^.*:[ ]?", "", .)
        blank <- grep("blank", readLines(bgfile, n = 4), value = TRUE) |> sub("^.*:[ ]?", "", .)
        test <- grep("test", readLines(bgfile, n = 4), value = TRUE) |> sub("^.*:[ ]?", "", .)
        ignore <- grep("ignore", readLines(bgfile, n = 4), value = TRUE) |> sub("^.*:[ ]?", "", .)
    } else {
        bg <- {readxl::read_xlsx(bgfile, sheet = "layout") |>
                   filter(!grepl("^#", platerow))}
        params <- {readxl::read_xlsx(bgfile, sheet = "params") |>
                       select(-instructions)}
        bgdelim <- params$value[params$parameter == "delim"]
        blank <- params$value[params$parameter == "blank"]
        test <- params$value[params$parameter == "test"]
        ignore <- params$value[params$parameter == "ignore"]
    }
    bg <- {bg |>
               tidyr::pivot_longer(-platerow,
                                   names_to = "platecol",
                                   names_prefix = "c",
                                   values_to = "bgcode") |>
               dplyr::mutate(well = paste0(platerow, platecol)) |>
               tidyr::separate_wider_delim(bgcode, names = c("bggroup", "bgtype"),
                                           delim = bgdelim) |>
               dplyr::mutate(bgtype = dplyr::case_when(bgtype == blank ~ "blank",
                                                       bgtype == test ~ "test",
                                                       bgtype == ignore ~ "ignore",
                                                       TRUE ~ NA)) |>
               dplyr::select(well, bgtype, bggroup)}
    if (any(is.na(bg$bgtype))) {warning("Some wells are not marked as blank, test, or ignore")}
    return(bg)
}
