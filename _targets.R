## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

future::plan(future::multisession, workers = 8)

source("code/nss_targets.R")
source("code/plfs_targets.R")

list(nss_hh_itemcodes_targets,
     nss_common_items_targets,
     nss_hh_cons_targets,
     nss_items_pce_estimates_targets,
     nss_items_pce_estimates_st_targets#,
     #plfs_targets,
     #plfs_estimates_targets
     )
