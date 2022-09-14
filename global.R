# global.R script for Time-Use Reallocation shinydashboard
# Objects that are not reactive are written here


# ---- libs ----

# Load package libraries
library(shiny)
require(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(compositions)
library(foreach)


# ---- debug boolean ----

####################################
### set this TRUE for debug mode ###
####################################

debug_mode <- TRUE


# --- load rdata ----

# Source pre-created model elements
rdat_fls <- c("dat/lm_ln_fat.RData", "dat/lm_psy.RData", "dat/lm_aca.RData")

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

# from "dat/lm_ln_bf.RData", load the objects: beta_ln_bf, vcov_ln_bf, resdf_ln_bf
# from "dat/lm_psy.RData", load the objects:     beta_psy,   vcov_psy,   resdf_psy
# from "dat/lm_aca.RData", load the objects:     beta_aca,   vcov_aca,   resdf_aca
for (f in rdat_fls) {
  load(f)
}


# Define activity names and number of activities
activity_nms <- c('Sleep', 'Screen', 'PA', 'QuietT', 'PassiveTrans', 'School', 'Domestic_SelfCare')
nact <- length(activity_nms)

# ---- define error messages ----

# unfeasible compositional parts error
err0.string <- paste0(
  "Initial time-use values have been reset as unfeasible values detected."
)

# Negative composition error
err1.string <- paste0(
  "Time allocation does not add up to 24 hours."
)

err2.string <- paste0(
  "Reallocated times must add up to zero hours."
)
  
  
# ---- UI_side_fns ----
  
# toggle_plots <- function(x) {
#   shinyjs::toggleElement("plot1", condition = function(x) {x %% 2 == 0} )
# }
  
disclaim_str <-
  paste(
    "*Differences in outcomes are not an indication of cause-and-effect",
    "as underlying data are cross-sectional. Also, note, time-use data were",
    "self-reported by study participants and may be subject to inaccuracies",
    "and biases."
  )

inp_wdth <- 6
  
  
# ---- server_side_fns ----
  
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
  



# For confidence intervals of the difference in lognormal predictions see:
# Guang Yong Zou, Julia Taleban, Cindy Y. Huo (2009)
# "Confidence interval estimation for lognormal data with application to health economics"
# This implements equation (4) of Zou et al.
diff_lognorm_cis <- function(m1, l1, u1, m2, l2, u2, r = 0) {
  
  ctr <- m1 - m2
  
  marg_lo <- 
    sqrt(
      (m1 - l1) ^ 2 + (u2 - m2) ^ 2 - 2 * r * (m1 - l1) * (u2 - m2)
    )
  marg_hi <- 
    sqrt(
      (u1 - m1) ^ 2 + (m2 - l2) ^ 2 - 2 * r * (u1 - m1) * (m2 - l2)
    )
  
  ci <- c(ctr - marg_lo, ctr + marg_hi)
  
  return(ci)
  
}

  
  
shiny_gg_theme <- function() {
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, colour = "grey50"),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey50")
  ) 
}
  
  
  