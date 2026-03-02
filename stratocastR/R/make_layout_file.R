# Generated from create-stratocastR.Rmd: do not edit by hand

#' Create a fresh plate layout file, as .xlsx or .csv.
#'
#' @param filename (character) Destination filename, with .xlsx or .csv extension
#' @export
make_layout_file <- function(filename) {
    filetype <- xfun::file_ext(filename)
    stopifnot("Plate layout file must be either .csv or .xlsx" =
                  filetype %in% c("csv", "xlsx"))
    template <- {expand.grid(platerow = LETTERS[1:8], platecol = paste0("c", 1:12),
                             val = NA) %>%
                     tidyr::pivot_wider(names_from = platecol, values_from = val)}
    if (filetype == "csv") {
        readr::write_csv(template, filename)
    } else {
        writexl::write_xlsx(template, filename)
    }
}
