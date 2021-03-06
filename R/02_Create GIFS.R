# 1. Load libraries ----
rm(list = ls())


library(mapSpain)
library(sf)
library(raster)
library(dplyr)
# library(tmap)
library(slippymath)
# library(grid)
# library(rgdal)
# library(osmdata)
# library(rtweet)
# library(jsonlite)
library(stringr)
# library(lubridate)
library(rayshader)
library(magick)
library(elevatr)
library(rgdal)




# Load helper funs
source("R/xxx_funs.R")

init <- Sys.time()
time <- as.character(format(Sys.time(), tz = "CET", usetz = TRUE))


# message("Connect with twitter")
#
# api_key <- Sys.getenv("TWITTER_API_KEY")
# api_secret_key <- Sys.getenv("TWITTER_API_SECRET")
# access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
# access_token_secret <- Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
#
#
# ## authenticate via web browser
# token <- create_token(
#   app = "spainmunic",
#   consumer_key = api_key,
#   consumer_secret = api_secret_key,
#   access_token = access_token,
#   access_secret = access_token_secret
# )


# 2. Load data ----

mapdata <- esp_get_munic(
  year = 2019,
  epsg = 4326,
  cache_dir = "data",
  moveCAN = FALSE
) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))


# LAU_CODE is INE CODE
mapdata <- mapdata %>% filter(!is.na(LAU_CODE))

# Clean names of municipalities - First option as per INE

mapdata <- mapdata %>% mutate(
  name1 = word(name, sep = " /"),
  namepref = str_extract(name1, pattern = "\\b[^,]+$"),
  namenopref = word(name1, sep = ","),
  newname = ifelse(
    namepref == namenopref,
    namenopref,
    paste0(str_to_title(namepref), " ",
           namenopref)
  ),
  # Replace for newname
  name = stringr::str_squish(newname)
)

# Fix names
codauto <-
  esp_codelist %>% dplyr::select(codauto, ccaa.shortname.es) %>% unique()

cpro <- esp_codelist %>% select(cpro, prov.shortname.es) %>%
  unique()

mapdata <- mapdata %>% left_join(codauto) %>% left_join(cpro)

mapdata <- mapdata %>%
  select(codauto,
         ccaa.shortname.es,
         cpro,
         prov.shortname.es,
         LAU_CODE,
         LAU_CODE_NUM,
         name)

data <- st_drop_geometry(mapdata)



# Load log

if (file.exists("assets/datalog.csv")) {
  datalog <-
    read.csv2("assets/datalog.csv",
              sep = ",",
              stringsAsFactors = FALSE)
  
} else {
  # If it doesn't exist, create one empty
  datalog <- data[1, ]
  datalog[1,] <- "xxx"
  datalog$datetime <- time
}


# 3. Select randomly after clean up ----

data_filter <-  data[!data$LAU_CODE_NUM %in% datalog$LAU_CODE_NUM, ]

# Override

if (file.exists("assets/override.csv")) {
  message("Override!")
  override <-
    read.csv2("assets/override.csv",
              sep = ",",
              stringsAsFactors = FALSE)
  override <- override[1,]
  file.remove("assets/override.csv")
  data_filter <-
    data[data$LAU_CODE_NUM %in% override$LAU_CODE_NUM, ]
  
  # Dedupe
  datalog <- datalog[datalog$LAU_CODE_NUM != data$LAU_CODE_NUM,]
}

# Stop if we are done

if (nrow(data_filter) < 1) {
  message("End with maps - All cities have been mapped")
  quit()
}


sel <- round(runif(1, 1, nrow(data_filter)), 0)

data_filter <- data_filter[sel,]


# Get and register munic
munic <- mapdata[mapdata$LAU_CODE == data_filter$LAU_CODE,]

# Add pop
pop <- mapSpain::pobmun19 %>%
  mutate(LAU_CODE = paste0(cpro, cmun)) %>%
  mutate(pob19 = paste0(prettyNum(
    pob19, big.mark = ".", decimal.mark = ","
  ))) %>%
  select(LAU_CODE, pob19)

munic <- munic %>% left_join(pop)


df <- munic %>% st_drop_geometry() %>%
  mutate(datetime = time)

message("Munic selected: ",
        df$name,
        " ",
        df$LAU_CODE)

# Get square around munic
square <- square_bbox(munic)

cent <- st_coordinates(square)

# 4. Get DEM -----

sq <- st_sf(d = 1, square) %>% as_Spatial()

DEM <- get_elev_raster(sq, z = 11, clip = "bbox") %>%
  crop(extent(sq))

minR <- min(dim(DEM)[1:2])
maxR <- max(dim(DEM)[1:2])
# Disaggregate to have better final resolution
if (minR < 600) {
  DEM <- disaggregate(DEM, fact = round(600 / minR))
} else if (maxR > 800) {
  DEM <- aggregate(DEM, fact = round(maxR / 900))
}

# Assign min to NAs - It's sea on this DEM provider
DEM[is.na(DEM)] <- min(values(DEM), na.rm = TRUE)

# 5. Get Tiles ----
# Analyze zoom
gz <- slippymath::bbox_tile_query(st_bbox(square))

# max 24 tiles, min 4 with an upgrade
zoom <- max(gz[gz$total_tiles %in% 4:24, "zoom"])

# Function to get tiles an avoid errors

hlp_gettile <-
  function(munic,
           provider,
           zoom,
           mask = TRUE,
           crop = TRUE) {
    get <- tryCatch(
      esp_getTiles(
        munic,
        provider,
        mask = mask,
        crop = crop,
        zoom = zoom,
        verbose = FALSE
      ),
      error = function(e) {
        return(TRUE)
      }
    )
    
    if (isTRUE(get)) {
      message("Error with zoom ", zoom, ". Try with ", zoom - 1)
      if (zoom < 1) {
        stop("Aborted")
        
      }
      get <- hlp_gettile(munic, provider, zoom - 1, mask)
    } else {
      message("Success with zoom ", zoom)
      return(get)
    }
  }
overlay_raster <- hlp_gettile(square, "PNOA", zoom, mask = FALSE)


mask <- mask(overlay_raster, munic)

tmppng <- tempfile(fileext = ".png")


# Convert to png
png(tmppng,
    height = dim(overlay_raster)[1],
    width = dim(overlay_raster)[2])
par(mar = c(0, 0, 0, 0))
plotRGB(overlay_raster, alpha = 0.8 * 255)
plotRGB(mask, add = TRUE, bgalpha = 0)
dev.off()

img_overlay <- png::readPNG(tmppng)

# 6. Rayshade!! ----

sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
sub <- paste(sub, collapse = ", ")
title <- paste0(munic$name, "\n", sub)

pop <- paste0("Population (2019): ", munic$pob19)
foot <-  paste0(pop,
                "\n",
                "Infraestructura de Datos Espaciales de Espa\u00f1a (IDEE)")

# Correction to zscale
fact <-
  (max(values(DEM), na.rm = TRUE) - min(values(DEM), na.rm = TRUE)) / 100

# Gif config
gif_file <- file.path("assets", "gif", munic$LAU_CODE)

n_frames <- 180

theta_val <-
  transition_values(
    from = 0,
    to = (360),
    steps = n_frames,
    one_way = TRUE,
    type = "lin"
  )
theta_val <- ifelse(theta_val > 360, theta_val - 360, theta_val)


phi_val1 <-
  transition_values(
    from = 90,
    to = 10,
    steps = (n_frames / 2) - 10,
    one_way = TRUE,
    type = "cos"
  )

phi_val <- c(phi_val1, rep(10, 20), rev(phi_val1))

zoom_val <- transition_values(
  from = 1,
  to = .5,
  steps = n_frames,
  one_way = FALSE,
  type = "cos"
)

# Rayshade 3D with png overlay

DEM_mat <- raster_to_matrix(DEM)

DEM_mat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(img_overlay) %>%
  plot_3d(DEM_mat, zscale = 5 + fact)

# Render gif
render_movie(
  gif_file,
  title_text = title,
  title_position = "north",
  title_size = 16,
  type = "custom",
  frames = n_frames,
  fps = 30,
  phi = phi_val,
  zoom = zoom_val,
  theta = theta_val,
  loop = TRUE,
  subtitle_text = foot,
  subtitle_position = "southeast",
  subtitle_size = 13,
  progbar = TRUE
)

rgl::rgl.close()

# 7. Save datalog ----
timejson <-
  as.character(format(Sys.time(), tz = "CET", usetz = TRUE))

df$datetime <- timejson

df <- df %>% select(-pob19)
datalog <- rbind(datalog, df) %>% filter(cpro != "xxx") %>% unique()
# Save datalog if everything was correct
write.table(datalog,
            "./assets/datalog.csv",
            sep = ",",
            row.names = FALSE)

r2 <- Sys.time() - init
message("Time elapsed: ", format(round(r2, 2)))

rm(list = ls())
