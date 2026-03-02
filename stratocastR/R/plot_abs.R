# Generated from create-stratocastR.Rmd: do not edit by hand

#' Plot plate-reader data by well, by row, and by column, including temperature and outlier status.
#'
#' @param data (dataframe) Plate-reader data processed through `detect_outliers()`
#' @param exclude.outliers (logical) Should points identified as outliers be hidden? Defaults to
#'   TRUE.
#' @param threshold (integer) What fraction of wells with outliers is too many for the data to be 
#'   shown? For timepoints when the fraction of outliers exceeds plot.threshold, all measurements will 
#'   be treated as outliers for plotting.  Defaults to 1, i.e., any number of outliers is acceptable.
#' @param show.ignores (logical) Should data from wells of bgtype "ignore" be plotted?
#' @param dropstyle (character) If outliers are to be dropped, how should they be dropped?  With
#'   `dropstyle = "drop"`, outliers are removed from data altogether; lines drawn across the missing
#'   region will be drawn straight between the last point before the gap and the first point after.
#'   With `dropstyle = "pointless", outliers are not plotted as points but are included in the line
#'   graph.  `dropstyle` is checked only if `exclude.outliers` is TRUE.
#' @param use.corr (logical) For test wells, should background-subtracted (TRUE) or raw (FALSE)
#'   measurements be plotted?  (Raw measurements are always plotted for blanks and, if plotted, ignores.)
#' @param title (character) A brief description of the experiment to print as the plot's title.
#' @param dates (character) The start date or date range of the experiment, to print as the plot's
#'   subtitle.
#' @param caption (character) A caption, e.g., for tracking background-subtraction choices or other
#'   experimental notes.  Defaults to NULL.
#' @export
plot_abs <- function(data, exclude.outliers = TRUE, threshold = 1, show.ignores = FALSE, 
                     dropstyle = c("drop", "pointless", NULL), use.corr = TRUE, title, dates,
                     caption = NULL) {
    if (use.corr) {
        stopifnot("use.corr is TRUE but no background-corrected values provided" =
                      "abs.corr" %in% colnames(data))
        data <- dplyr::mutate(data, abs.toplot = dplyr::case_when(bgtype == "test" ~ abs.corr,
                                                                  TRUE ~ absorbance))
    } else {
        data <- dplyr::mutate(data, abs.toplot = absorbance)
    }
    if (!show.ignores) {data <- dplyr::filter(data, bgtype != "ignore")}

    # Subset and aggregate data for different panels
    toplot.T <- {dplyr::filter(data, well == "A1") %>%
                     dplyr::select(duration.h, duration.min, airtemp.C, n.out)}
    toplot <- dplyr::mutate(data,
                            platerow = substr(well, 1, 1),
                            platecol = gsub("[A-H]", "", well) %>%
                                factor(levels = 1:12, ordered = TRUE))
    toplot.n <- dplyr::filter(toplot, platerow == "A")
    boxleft <- quantile(toplot.T$duration.h, probs = 0.01)
    boxright <- quantile(toplot.T$duration.h, probs = 0.06)
    lableft <- quantile(toplot.T$duration.h, probs = 0.08)
    bggroups <- {as.data.frame(toplot) %>%
                     dplyr::filter(!grepl("outlier", bggroup)) %>%
                     dplyr::group_by(platerow, platecol) %>%
                     dplyr::summarise(grouplab =
                                          paste0("Group ", first(bggroup))) %>%
                     dplyr::mutate(xmin = boxleft,
                                   xmax = boxright,
                                   lableft = lableft) %>%
                     dplyr::ungroup()}
    ngroups <- length(unique(bggroups$grouplab))
    toplot.points <- toplot
    if (exclude.outliers) {
        toplot.points <- dplyr::filter(toplot.points,
                                       !is.outlier.abs | bgtype == "ignore",
                                       pct.out <= threshold)
        if (dropstyle == "drop") {
            toplot.line <- toplot.points
        } else {
            toplot.line <- toplot
        }
    }

    # Build plot elements
    p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = toplot.line,
                           ggplot2::aes(duration.h, abs.toplot, group = well),
                           colour = "gray", linewidth = 0.2) +
        ggplot2::geom_point(data = toplot.points,
                            ggplot2::aes(duration.h, abs.toplot, colour = treatment), size = 0.1) +
        ggplot2::geom_line(data = toplot, ggplot2::aes(duration.h, bg.agg), linewidth = 0.3) +
        ggplot2::labs(y = "Absorbance", colour = "Condition") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(text = ggplot2::element_text(family = "Avenir Next Condensed"),
                       legend.position = "bottom",
                       axis.title.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                       legend.spacing.x = unit(1, "pt"),
                       legend.spacing.y = unit(1, "pt"),
                       legend.key.width = unit(8, "pt"),
                       legend.key.height = unit(8, "pt"),
                       legend.margin = margin(2, 60, 2, 0),
                       panel.grid = ggplot2::element_blank()) +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2,
                                                       override.aes = list(size = 2)))
    p.bycol <- p + ggplot2::facet_grid(. ~ platecol, drop = FALSE)
    p.byrow <- p + ggplot2::facet_grid(platerow ~ ., drop = FALSE) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank())
    p.bywell <- p + ggplot2::facet_grid(platerow ~ platecol, drop = FALSE)
    if (ngroups > 1) {
        p.bywell <- p.bywell +
            geom_rect(data = bggroups,
                      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0.95, ymax = 1.05,
                          fill = grouplab)) +
            geom_text(data = bggroups,
                      ggplot2::aes(x = lableft, y = 1, label = grouplab),
                      hjust = 0, size = 2, family = "Avenir Next Condensed") +
            scale_fill_brewer(palette = "Dark2", guide = "none")
    }
    p.T <- ggplot2::ggplot(toplot.T, ggplot2::aes(duration.h, airtemp.C)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = "Duration (h)", y = "Air\ntemp\n(\u00b0C)") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(text = ggplot2::element_text(family = "Avenir Next Condensed"),
                       axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                       panel.grid = ggplot2::element_blank())
    p.nout <- ggplot2::ggplot(toplot.n) +
        ggplot2::geom_segment(ggplot2::aes(duration.h, y = 0, yend = 1, colour = n.out)) +
        ggplot2::labs(y = "Outliers\nper time", colour = "Number of \noutlier wells") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(text = ggplot2::element_text(family = "Avenir Next Condensed"),
              strip.background = ggplot2::element_blank(),
              strip.text.x = ggplot2::element_blank(),
              legend.position = "bottom",
              axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank(),
              legend.margin = margin_auto(2, unit = "pt"),
              panel.grid = ggplot2::element_blank()) +
        ggplot2::scale_colour_viridis_c(option = "inferno") +
        ggplot2::facet_grid(. ~ platecol, drop = FALSE)
    p.Tstrip <- ggplot2::ggplot(filter(toplot.n, is.outlier.temp)) +
        ggplot2::geom_segment(ggplot2::aes(duration.h, y = 0, yend = 1), colour = "black",
                              linewidth = 0.1) +
        ggplot2::geom_segment(data = dplyr::filter(toplot.line,
                                                   duration.min == min(duration.min) |
                                                   duration.min == max(duration.min)),
                              ggplot2::aes(duration.h, y = 0, yend = 1), colour = "black", alpha = 0) +
        ggplot2::labs(x = "Duration (h)", y = "Air temp\noutliers") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(text = ggplot2::element_text(family = "Avenir Next Condensed"),
              strip.background = ggplot2::element_blank(),
              strip.text.x = ggplot2::element_blank(),
              legend.position = "bottom",
              axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              legend.margin = margin_auto(0, unit = "pt"),
              panel.grid = ggplot2::element_blank()) +
        ggplot2::facet_grid(. ~ platecol, drop = FALSE)
    layout <- "
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
AAAAAAAAAAAB
CCCCCCCCCCCD
EEEEEEEEEEEG
FFFFFFFFFFFG
"
    p.master <- patchwork::wrap_plots(A = p.bywell, B = p.byrow, C = p.bycol, D = p.T,
                                      E = p.nout, F = p.Tstrip, G = guide_area(),
                                      design = layout, guides = "collect") +
        patchwork::plot_annotation(title = title, subtitle = dates, caption = caption) &
        ggplot2::theme(text = element_text(family = "Avenir Next Condensed"))
    return(p.master)
}
