# Generated from create-stratocastR.Rmd: do not edit by hand

#' Perform background subtraction on a plate reader data file.
#'
#' @param data (dataframe) The output of `read_platedata()`; may additionally have been processed through
#'   `detect_outliers()`
#' @param bg (dataframe) The output of `background_by_treatment()` or `background_by_well()`
#' @param iter (numeric) Number of outlier-detection iterations for `forecast::tsoutliers()` to run.
#'   Defaults to 1.
#' @param basis (character) Should background values be aggregated by mean or by median before
#'   subtraction?
#' @param interval (numeric) Length of time step in minutes, as read by `get_interval_min()`; required
#'   if `exclude.outliers` is TRUE.
#' @param exclude.outliers (logical) Should background values flagged as outliers be excluded from
#'   calculation of background?  Applies only if `data` has been processed through `detect_outliers()`.
#'   Defaults to TRUE.
#' @param threshold (numeric) The minimum number of blank wells allowed to be used for background
#'   subtraction at each timepoint.  Defaults to 3.
#' @return A dataframe that adds three new columns to data:
#'   - bgtype:  the categorical column defining each well as "blank", "test", or "ignore".  Blank wells
#'     are used to determine background; test wells have background subtracted from them; ignore wells
#'     are ignored.  Merged in from bg.
#'   - bggroup:  a column defining groups within which background subtraction should be performed.  E.g.,
#'     if two different media were used in the plate, background subtraction should be performed 
#'     separately for wells using each medium.  Merged in from bg.
#'   - bg.n:  a count of blank values aggregated into each group's background value at each time.
#'   - abs.corr:  a column of background-subtracted values, absorbance - background.  For "blank" and
#'     "test" wells, these values will be the per-well per-time absorbance minus the group's
#'     `basis`-aggregated blank absorbance; for "ignore" wells, NA.
#'   - flag:  notes on the background processing applied to obtain each row's abs.corr value
#' @export
subtract_background <- function(data, bg, iter = 1, basis = c("mean", "median"), interval = NULL,
                                exclude.outliers = TRUE, threshold = 3) {
    corr.string <- paste0("Abs.corr corrected with ", basis, " of within-group blanks")
    data <- dplyr::left_join(data, bg)
    bgfeeder <- dplyr::filter(data, bgtype == "blank")
    if (exclude.outliers) {
        if (!"is.outlier.abs" %in% colnames(bgfeeder)) {
            stopifnot("Outlier analysis requires an interval length to be supplied" =
                          is.numeric(interval))
            bgfeeder <- detect_outliers(data = bgfeeder, interval = interval,
                                        iter = iter, incol = "absorbance")
            data <- dplyr::left_join(data, bgfeeder) ### <---- issues here
        }
        bgfeeder <- dplyr::filter(bgfeeder, !is.outlier.abs)
    }
    bgs <- {as.data.frame(bgfeeder) %>%
                dplyr::group_by(bggroup, duration.min) %>%
                dplyr::summarise(bg.agg = rlang::exec(basis, absorbance),
                                 bg.n = dplyr::n())}
    data <- {dplyr::left_join(data, bgs) %>%
                 dplyr::mutate(bg.n = tidyr::replace_na(bg.n, 0),
                               abs.corr = dplyr::case_when(bgtype == "ignore" ~ NA,
                                                           bg.n < threshold ~ NA,
                                                           TRUE ~ absorbance - bg.agg),
                               flag = dplyr::case_when(bgtype != "ignore" & bg.n < threshold ~
                                                           "Insufficient data for background subtraction",
                                                       bgtype != "ignore" & bg.n >= threshold ~
                                                           corr.string,
                                                       TRUE ~ NA))}
    return(data)
}
