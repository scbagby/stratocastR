Sarah Bagby

# stratocastR: tidy processing and plotting of data from the Cerillo Stratus plate-reader

Stratus data files exported from Cerillo Labrador in .csv format contain
several un-commented header rows of metadata, then a blank line, then a
row of column names, and finally one row of data for each timepoint
measured. \`stratocastR\` aims to streamline analysis and visualization
of this data. *This package expects that the data file is the raw .csv
export with no manual editing.*

Given the data and metadata (Cerillo file, plate layout, background
setup, experiment name and dates for the plot title and subtitle) and an
output filename, running \`stratocastR::process_plate()\` will

- read in plate-reader data and the length of the interval between
  measurements
- read in the plate layout
- set background groups, so that, e.g., blank wells with medium A feed
  into background subtraction for test wells with medium A, blanks with
  B for B tests, etc.
- detect per-timepoint outliers in blanks
- aggregate blanks to be used for background subtraction within each
  group
- apply background subtraction
- detect outliers in background-subtracted test wells
- plot the results and save

# Installation

With \`devtools\` installed, you can install stratocastR from github
using

    remotes::install_github("scbagby/stratocastR-project", subdir = "stratocastR")

# Getting started

To use \`stratocastR\`, you need an output .csv file from the Stratus;
let’s say for this example that it’s called “my_data.csv”. You also need
to know the layout of your 96-well plate, and you need to get that
information into standardized format. Start by generating a template
file for defining the plate layout.

    library(stratocastR)
    make_layout_file("my_layout.xlsx") # alternatively:  make_layout_file("my_layout.csv")

Edit and save the resulting file to show the contents of each well. The
labels you use in this file will eventually wind up in your plot, and
they should be informative. To simplify processing, we also want to be
able to tell R whether each condition is a blank, an experimental
(“test”) condition, or something to ignore.

In a simple experiment with one type of blank (let’s say you’ve called
it “LB control” in your plate layout file) and one type of experimental
condition (let’s say you’ve called it “Species 1 in LB”), this is very
simple:

    bg <- background_by_treatment(test = "Species 1 in LB", blanks = "LB control", ignore = NULL,
                                  treatments = "my_layout.xlsx")

If your plate is more complicated (let’s say you have different media in
different wells, so that you need to be careful about which blanks are
used to correct which test wells), you need to do a little more work, by
preparing a second type of layout file.

    make_bg_file("my_background_layout.xlsx")  # alternatively:  make_bg_file("my_background_layout.csv")

Edit and save the resulting file, following the instructions given in
the file. If you’ve generated an Excel file, you’ll need to edit both
the \`layout\` sheet and the \`params\` sheet. If you’ve generated a
.csv file, you’ll need to edit both the header section and the csv
section. Either way, once you’ve saved, you’re ready to go.

    mydata <- process_plate(filename = "my_data.csv",
                            treatments = "my_layout.xlsx",
                            background.file = NULL, # <-- supply the background layout file if you made one; 
                            background.treatments = bg, # <-- otherwise, supply the bg dataframe you made
                            plot.title = "Testing reproducibility with Species 1 grown in LB",
                            expt.dates = "Run 2026-02-01 to 2026-02-05", # <-- to become the plot subtitle
                            outfile = "my_growth_plot.png")

This should produce an output file showing background-subtracted growth
profiles in the context of temperature data and the count of outliers
per timepoint across the plate.
\`mydata$data\` is the dataframe of measurements, metadata, outlier calls, and background-subtracted absorbance values, ready for use in calculating growth rates, etc.  \`mydata$plot\`
is a patchwork ggplot that you can manipulate like any patchwork or
ggplot object.
