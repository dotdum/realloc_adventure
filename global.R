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
require(foreach)

####################################
### set this TRUE for debug mode ###
####################################
debug_mode <- TRUE




# Define model objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Source models
rdat_fls <- c("dat/lm_ln_bf.RData", "old-dat/model.bf.RData")

fl_exists <- file.exists(rdat_fls)
if (!all(fl_exists)) {
  stop(
    paste0(
      "Please make sure the file(s) ", 
      paste(rdat_fls[!fl_exists], collapse = ", "),
      " are available to the app. Currently not found."
    )
  )
}

load("old-dat/model.bf.RData") #
load(rdat_fls[1]) # from "dat/lm_ln_bf.RData", load the objects: "beta_ln_bf", "vcov_ln_bf", "resdf_ln_bf"

# Define activity names and number of activities
activity_nms <- c('Sleep', 'Screen', 'PA', 'QuietT', 'PassiveTrans', 'School', 'Domestic_SelfCare')
nact <- length(activity_nms)

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
  
  
  
  
sanitise_ilrs <- function(x) {
  
  if ("rmult" %in% class(x)) {
    class(x) <- NULL # remove "rcomp" class, will either result in numeric vector or matrix
    attr(x, "orig") <- NULL # remove original composition info (issue with indexes)
    if ("numeric" %in% class(x)) { # if vector turn into 1 row matrix
      x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))
    }
  }
  return(x)
  
}

  

poly2 <- function(x) {
  
  # make sure is matrix
  x <- sanitise_ilrs(x)
  
  n <- ncol(x) 
  cnames <- colnames(x)
  
  if (is.null(cnames)) {
    cnames <- paste0("c", 1L:n)
  }
  
  # get all tuples of (j,k) where j <= k
  tups <- subset(expand.grid(j = 1:n, k = 1:n), j <= k)
  tups <- tups[order(tups$j, tups$k), ] # make sure consistent ordering
  j <- tups$j
  k <- tups$k
  
  # drop = FALSE is to make sure 1 row matrices don't become vectors
  sq_out <- x[, j, drop = FALSE] * x[, k, drop = FALSE] 
  colnames(sq_out) <- paste0(cnames[j], ":", cnames[k])
  
  return(cbind(x, sq_out))
  
}



  

make_x0 <- function(betas, ilrs, sex, age, sep, pub) {
  
  beta_nms <- names(betas)
  ilr_nms <- beta_nms[grepl("^ilrs_", beta_nms)]
  
  ilrs <- sanitise_ilrs(ilrs)
  
  # beta_nms <- gsub("ilrs_", "", beta_nms, fixed = TRUE)
  # ilr_nms <- gsub("ilrs_", "", ilr_nms, fixed = TRUE)
  p <- length(betas)
  
  # initialise model matrix
  x0 <- matrix(0, nrow = nrow(ilrs), ncol = p, dimnames = list(NULL, beta_nms))
  
  # populate model matrix intercept column
  x0[, "(Intercept)"] <- 1
  
  # populate ilr columns in model matrix 
  x0_ilrs <- poly2(ilrs)
  x0_ilrs_nms <- {colnames(x0_ilrs) <- paste0("ilrs_", colnames(x0_ilrs))}
  ### testing
  # print(x0_ilrs_nms)
  # print(ilr_nms)
  
  if (!(all(x0_ilrs_nms %in% ilr_nms) & all(ilr_nms %in% x0_ilrs_nms))) {
    stop("The ilrs in the model and the supplied ilrs have differing names (or possibly differ in number)")
  }
  # if in different orders, make x0_ilrs order the same as ilr_nms
  if (!all(x0_ilrs_nms == ilr_nms)) {
    reorder <- match(ilr_nms, x0_ilrs_nms)
    x0_ilrs <- x0_ilrs[reorder]
    x0_ilrs_nms <- colnames(x0_ilrs)
    if (!all(x0_ilrs_nms == ilr_nms)) {
      stop("ilr names supplied in `ilrs` could not be reordered to match the ilr names in `mod`")
    }
  }
  
  x0[, ilr_nms] <- x0_ilrs
  
  # populate covariate columns in model matrix 
  # covariates in model matrix are dealt with in a vectorised way for when there is more than one row in ilrs
  x0[, "cov.sex2"] <- as.numeric(as.character(sex) == "2")
  x0[, "cov.age"] <- age
  x0[, "cov.sep"] <- sep
  puberty_vals <- 
    as.integer(
      gsub(
        "cov.puberty", 
        "", 
        beta_nms[grepl("cov.puberty", beta_nms, fixed = TRUE)], 
        fixed = TRUE
      )
    )
  pub <- as.character(pub)
  for (i in puberty_vals) {
    x0[, paste0("cov.puberty", i)] <- as.numeric(pub == as.character(i))
  }
  
  return(x0)
  
}

  

get_pred_bounds <- function(betas, x0, beta_vcov, df, bound = 0, alpha = 0.05) {
  betas <- matrix(betas, ncol = 1)
  crit_val <- qt(1 - alpha / 2, df = df)
  foreach(i = 1:nrow(x0), .combine = c) %do% {
    x0_i <- x0[i, , drop = FALSE]
    as.numeric(x0_i %*% betas) + 
      bound * crit_val * sqrt(as.numeric(x0_i %*% beta_vcov %*% t(x0_i)))
  }
}
  
  
  
  
  
  
  
  