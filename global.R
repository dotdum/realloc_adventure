# global.R script for Time-Use Reallocation shinydashboard
# Objects that are not reactive are written here
# -----------------------------------------------------------------------------
# Load package libraries
require(shinydashboard)
require(ggplot2)
require(compositions)
#need old version of r2d3 or it doesn't work for the histogram
#  packageurl <- "https://cran.r-project.org/src/contrib/Archive/r2d3/r2d3_0.2.3.tar.gz"
 # install.packages(packageurl, repos=NULL, type="source")
require(r2d3)
require(robustbase)

# Define model objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Source models
rdat_fls <- "dat/model.bf.RData"

fl_exists <- file.exists(rdat_fls)
if (!all(fl_exists)) {
  stop(
    paste0(
      "Please make sure the file(s) ", 
      paste(rdat_fls[fl_exists], collapse = ", "),
      " are available to the app. Currently not found."
    )
  )
}

load("dat/model.bf.RData") #, verbose = TRUE)

# Define activity names and number of activities
activity <- c('Sleep', 'Screen', 'PA', 'QuietT', 'PassiveTrans', 'School', 'Domestic_SelfCare')
nact <- length(activity)

# Define error messages
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Negative composition error
  err1.string <- paste0(
    "Time allocation does not add up to 24 hours."
  )
  
  err2.string <- paste0(
    "Reallocated times must add up to zero hours."
  )
  
  
  ##need an error when you try to reallocate more than what's available (Cant get negative time...)
  
  