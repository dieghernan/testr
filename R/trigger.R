

hlp_install <- function(pkg) {
  if (isFALSE(requireNamespace(pkg, quietly = TRUE))) {
    install.packages(pkg)
  }
}

hlp_install("mapSpain")
hlp_install("sf")
hlp_install("dplyr")
hlp_install("slippymath")
hlp_install("rgdal")
hlp_install("av")

output_dir <- file.path("assets", "gif")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
} else {
  print("Dir already exists!")
}

source("R/02_Create GIFS.R")
