# renv activation is OPT-IN. The default setup uses the machine's own R
# library (the production machine manages R packages with Nix). Set
# RENV_ACTIVATE_PROJECT=TRUE to use the pinned renv library instead;
# global-scripts/config.R applies the same switch for scripted runs launched
# from any directory. renv.lock records the validated package versions.
if (tolower(Sys.getenv("RENV_ACTIVATE_PROJECT", "FALSE")) %in% c("true", "t", "1"))
  source("renv/activate.R")
