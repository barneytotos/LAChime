`%notin%` <- Negate(`%in%`)

##############################################################
## This file setups the directory and installs packages etc.
##############################################################
installer <- function(package, repo="http://cran.rstudio.com/"){
  # install a package if it isn't installed
  if (package %notin%  installed.packages()[, "Package"]){
    install.packages(package, dependencies = TRUE, repo=repo)
  } else {
    cat(sprintf('Verified %s installed\n', package))
  }
}

# List of dependent packages
packages = c(
  'dplyr',
  'purrr',
  'rstan',
  'ggplot2',
  'patchwork'
)

# Install + source the packages
._ = lapply(packages, installer)

# Load global dependencies dependencies
packages = c(
  'dplyr',
  'purrr',
  'ggplot2',
  'patchwork'
)
._ = lapply(packages, library, character.only=TRUE)

# Initialize global variables
rm(list=ls())
