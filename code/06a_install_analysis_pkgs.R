# ---------------------------------------------------------------------------- #
# Install Analysis Packages
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes
# ---------------------------------------------------------------------------- #

# R version 4.1.2 (to install, see https://cran.r-project.org/bin/windows/base/old/ )

# To install and load package versions used in "06b_analysis.Rmd", (a) first comment 
# out "library()" calls in that script (as RStudio loads the latest versions of some 
# dependencies in that script the moment the script is opened--before any code is run),
# (b) save and close that script, (c) restart R (CTRL+SHIFT+F10 on Windows), and (d) 
# run the present script, which installs specific package versions mostly from source

# ---------------------------------------------------------------------------- #
# Define source URLs for packages, dependencies, and subdependencies
# ---------------------------------------------------------------------------- #

contrib_url <- "https://cran.r-project.org/src/contrib/"

tidyverse_src_url      <- paste0(contrib_url, "tidyverse_2.0.0.tar.gz")

here_src_url           <- paste0(contrib_url, "here_1.0.1.tar.gz")

# Note: "brms 2.20.4" has the following dependencies and subdependencies that 
# must be installed before "brms" can be installed from source URL

rstan_src_url          <- paste0(contrib_url, "Archive/rstan/rstan_2.26.23.tar.gz")

  # Note: Although "igraph 1.2.4.2" is a dependency of "threejs 0.3.3", it is not recommended
  # to install "igraph" from source; see https://r.igraph.org/articles/installation-troubleshooting.
  # Thus, the newer "igraph 1.4.2" (compatible with R 4.1) is installed from binary via 
  # "install.packages("igraph")" below.

threejs_src_url        <- paste0(contrib_url, "threejs_0.3.3.tar.gz") # For "shinystan 2.6.0"
shinystan_src_url      <- paste0(contrib_url, "shinystan_2.6.0.tar.gz")
Brobdingnag_src_url    <- paste0(contrib_url, "Archive/Brobdingnag/Brobdingnag_1.2-6.tar.gz") # For "bridgesampling 1.1-2"
bridgesampling_src_url <- paste0(contrib_url, "bridgesampling_1.1-2.tar.gz")
brms_src_url           <- paste0(contrib_url, "Archive/brms/brms_2.20.4.tar.gz")

# Note: "mnormt 2.1.1" is a dependency of "psych 2.3.9"

mnormt_src_url         <- paste0(contrib_url, "mnormt_2.1.1.tar.gz")
psych_src_url          <- paste0(contrib_url, "Archive/psych/psych_2.3.9.tar.gz")

# ---------------------------------------------------------------------------- #
# Install packages, dependencies, and subdependencies 
# ---------------------------------------------------------------------------- #

install.packages(tidyverse_src_url,      repos = NULL, type = "source")

install.packages(here_src_url,           repos = NULL, type = "source")

install.packages(rstan_src_url,          repos = NULL, type = "source")
install.packages("igraph", type = "binary") # Version 1.4.2
install.packages(threejs_src_url,        repos = NULL, type = "source")
install.packages(shinystan_src_url,      repos = NULL, type = "source")
install.packages(Brobdingnag_src_url,    repos = NULL, type = "source")
install.packages(bridgesampling_src_url, repos = NULL, type = "source")
install.packages(brms_src_url,           repos = NULL, type = "source")

install.packages(mnormt_src_url,         repos = NULL, type = "source")
install.packages(psych_src_url,          repos = NULL, type = "source")

# ---------------------------------------------------------------------------- #
# Load packages
# ---------------------------------------------------------------------------- #

library(tidyverse) # 2.0.0
library(here)      # 1.0.1
library(brms)      # 2.20.4
library(psych)     # 2.3.9

# Confirm that R version and loaded package versions match those above

R.Version()$version.string == "R version 4.1.2 (2021-11-01)"

sessionInfo()[["otherPkgs"]]$tidyverse$Version == "2.0.0"
sessionInfo()[["otherPkgs"]]$here$Version      == "1.0.1"
sessionInfo()[["otherPkgs"]]$brms$Version      == "2.20.4"
sessionInfo()[["otherPkgs"]]$psych$Version     == "2.3.9"