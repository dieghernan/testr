renv::init()
renv::dependencies()
renv::snapshot()
renv::status()
renv::install("magick")
renv::clean()
renv::restore()
library(magick)

renv::paths$library()

Sys.getenv("RENV_PATHS_ROOT")
