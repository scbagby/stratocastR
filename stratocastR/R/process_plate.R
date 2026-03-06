# Generated from create-stratocastR.Rmd: do not edit by hand

#' Perform end-to-end plate reader data analysis, optionally saving the resulting plot
#'
#' @param filename (character) Cerillo CSV file
#' @param treatments (character) Plate layout CSV file, following the `make_layout_file()` format
#' @param background.file (character) Background layout .csv or .xlsx file, following the
#'   `make_bg_file()` format.  Defaults to NULL.  Either `background.file` or `background.treatments`
#'   must be supplied.
#' @param background.treatments (list) Named list of conditions to be handled as test wells, blanks,
#'   and ignores (e.g., `list(test = c("BG.Cab", "Azo"), blank = "Control", ignore = "Empty")).  
#'   Defaults to NULL.  Either `background.file` or `background.treatments` must be supplied.
#' @param plot.title (character) A brief description of the experiment, to print as the title of the
#'   analysis figure.  Other annotations will automatically capture data-analysis parameters; 
#'   plot title should briefly describe the experiment.
#' @param expt.dates (character) The start date or date range of the experiment, to print as the plot's
#'   subtitle.
#' @param outfile (character) Output filename, if analysis figure is to be saved.  Defaults to NULL.
#' @param display.plot (logical) Should the analysis figure be displayed on a graphics device?  Can
#'   be slow.  Defaults to FALSE.
#' @param ts.iter (numeric) Number of iterations to use in outlier detection.  Defaults to 1.
#' @param exclude.bg.outliers (logical) Should outlier readings in blank wells be removed before
#'   aggregating background measurements?  Defaults to TRUE.
#' @param bg.basis (character) Should blank wells' absorbance readings be aggregated by mean or median
#'   for background subtraction? Defaults to "median".
#' @param bg.threshold (numeric) Minimum number of blank wells with data that are required for
#'   background subtraction to proceed in a background group at a given timepoint?  Defaults to 3.
#' @param show.ignores (logical) Should data from wells of the "ignore" type be plotted?  Defaults to
#'   FALSE.
#' @param plot.threshold (numeric) What fraction of wells with outliers is too many for the data to be 
#'   shown? For timepoints when the fraction of outliers exceeds plot.threshold, all measurements will 
#'   be treated as outliers for plotting.  Defaults to 1, i.e., any number of outliers is acceptable.
#' @param plot.dropstyle (character) How should outliers be plotted?  They can be dropped altogether,
#'   from points and lines ("drop"); they can be dropped from the points but kept in the lines,
#'   so that they are visible but deemphasized ("pointless"); or they can be included fully (NULL).
#' @param use.corr (logical) Should background-subtracted or raw values be shown in test wells?  When
#'   `use.corr = TRUE`, background-subtracted values are shown for test wells, while raw values are
#'   shown for blank wells (and "ignore" wells, if plotted), with the aggregate background measure
#'   overlaid on these wells for a visual reference.  When `use.corr = FALSE`, only raw values are
#'   shown across the entire plate.
#' @return A list of length 2, containing
#'   - a dataframe of processed data
#'   - a patchwork plot object
#'   Additionally, if `display.plot` is TRUE, the analysis figure is displayed on a graphics device; 
#'   if an output filename is supplied, the analysis figure is saved to disk.    
#' @export
process_plate <- function(filename, treatments, background.file = NULL, background.treatments = NULL,
                          plot.title, expt.dates, outfile = NULL, display.plot = FALSE, ts.iter = 1,
                          exclude.bg.outliers = TRUE, bg.basis = "median", bg.threshold = 3,
                          show.ignores = FALSE, plot.threshold = 1, plot.dropstyle = "pointless",
                          use.corr = TRUE) {
    stopifnot("Either background.file or background.treatments must be supplied" =
                  is.null(background.file) + is.null(background.treatments) == 1)
    if (plot.threshold > 1 & plot.threshold <= 96) {
        warning("Converting plot threshold to fraction of wells on plate, but comparison is to fraction of blank and test wells.  Consider supplying a fraction rather than a total number of wells.")
        plot.threshold <- plot.threshold / 96
    }
    # Read in data
    plate <- read_platedata(filename, treatments)
    interval.min <- get_interval_min(filename)

    # Set background groups
    if (!is.null(background.file)) {
        bg <- background_by_wells(bg = background.file)
    } else {
        bg <- background_by_treatment(test = background.treatments$test,
                                      blanks = background.treatments$blanks,
                                      ignore = background.treatments$ignore)
    }
    bgcounts <- {bg |>
                     dplyr::filter(bgtype != "ignore") |>
                     dplyr::group_by(bgtype) |>
                     dplyr::summarise(n.meas = dplyr::n())}
    nonignores <- sum(bgcounts$n.meas)

    # If exclude.bg.outliers is TRUE, detect per-timepoint outliers in blanks.
    # Aggregate blanks to be used for background subtraction within each group
    # and apply background subtraction.
    bgsub <- subtract_background(data = plate, bg = bg, interval = interval.min, 
                                 iter = ts.iter, exclude.outliers = exclude.bg.outliers,
                                 basis = bg.basis, threshold = bg.threshold)

    # Detect outliers in background-subtracted test wells.  
    bgsub.bi <- dplyr::filter(bgsub, bgtype %in% c("blank", "ignore"))
    bgsub.t <- {dplyr::filter(bgsub, bgtype == "test") |>
                    dplyr::select(-is.outlier.abs, -n.out.bgtype) |>
                    detect_outliers(data = ., interval = interval.min, iter = ts.iter,
                                    incol = "abs.corr") |>
                    dplyr::mutate(flag = case_when(bgtype == "test" & is.outlier.abs ~
                                                       "Outlier after background subtraction",
                                                   bgtype == "test" ~ flag,
                                                   TRUE ~ flag))}
    bgsub.all <- dplyr::bind_rows(bgsub.bi, bgsub.t)

    # Tally absorbance outliers and check for outliers in temperature data.
    problem.steps <- {bgsub.all |> 
                          dplyr::group_by(duration.min) |>
                          dplyr::summarise(n.out = sum(is.outlier.abs, na.rm = TRUE)) |>
                          dplyr::mutate(pct.out = n.out / nonignores)}
    bgsub.all <- {dplyr::left_join(bgsub.all, problem.steps) |>
                      detect_outliers(interval.min, incol = "airtemp.C")}
    
    # Set caption to capture data-handling choices
    if (use.corr) {
        corrstring <- paste("Measurements in experimental wells corrected by subtracting the",
                             bg.basis,
                             "of blanks in each group.",
                             ifelse(exclude.bg.outliers,
                                    "Outliers in blanks were removed before aggregation.",
                                    NULL),
                             "Black lines show aggregated background.\n")
    } else {corrstring <- NULL}
    if (plot.threshold < 1) {
        thrstring <- paste("and all timepoints when the fraction of outliers exceeds",
                           plot.threshold)
    } else {thrstring <- NULL}
    if (plot.dropstyle == "drop") {
        outstring <- paste("Outliers", thrstring, "have been masked in points and lines.\n")
    } else if (plot.dropstyle == "pointless") {
        outstring <- paste("Outliers", thrstring,
                           "have been masked in points but are shown in gray lines.\n")
    } else {outstring <- NULL}
    if (!show.ignores) {
        ignorestring <- "Measurements from `ignore` wells are not shown."
    } else {ignorestring <- NULL}
    plot.caption <- paste0(corrstring, outstring, ignorestring)

    # Plot the results and save
    p <- plot_abs(bgsub.all, threshold = plot.threshold, dropstyle = plot.dropstyle,
                  use.corr = use.corr, show.ignores = show.ignores, title = plot.title,
                  dates = expt.dates, caption = plot.caption)
    if (display.plot) {p}
    if (!is.null(outfile)) {ggsave(outfile, width = 14.5, height = 8)}
    
    return(list(data = bgsub.all, plot = p))
}
