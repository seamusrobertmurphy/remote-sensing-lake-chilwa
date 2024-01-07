#install.packages(c("sits", "cowplot", "googleway", 
#                   "ggplot2", "ggrepel", "ggspatial", 
#                   "libwgeom", "sf", "rnaturalearth", 
#                   "rnaturalearthdata", "jsonlite", "keyring", "rstac"))



library(keyring)
key_set("DEAFRICA_ACCESS_KEY")



library("sits")
library("sf")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")


theme_set(theme_bw())


world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)
plot1 = (gworld <- ggplot(data = world) +
           geom_sf(aes(fill = region_wb)) +
           geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
                     fill = NA, colour = "black", size = 1.5) +
           scale_fill_viridis_d(option = "plasma") +
           theme(panel.background = element_rect(fill = "azure"),
                 panel.border = element_rect(fill = NA)))

plot2  = (ggulf <- ggplot(data = world) +
           geom_sf(aes(fill = region_wb)) +
           annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
                    fontface = "italic", color = "grey22", size = 6) +
           coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
           scale_fill_viridis_d(option = "plasma") +
           theme(legend.position = "none", axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
                 panel.border = element_rect(fill = NA)))

cowplot::plot_grid(gworld, ggulf, nrow = 1, rel_widths = c(2.3, 1))

sites_locator <- st_as_sf(data.frame(
  longitude = c(32, 36), latitude = c(-8,-16)), 
  coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

malawi = ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  annotate(geom = "text", x = 35.5, y = -8.5, label = "Malawi", 
           color = "grey22", size = 4.5) +
  coord_sf(xlim = c(32, 36.5), ylim = c(-8.5, -18)) +
  xlab("Longitude")+ ylab("Latitude") + 
  theme(panel.grid.major = element_line(
    colour = gray(0.5), linetype = "dashed", size = 0.5), 
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA))

chilwa <- ggplot(roi) +
  geom_sf() +
  theme_void() +
  # Add a border to the inset
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.background = element_rect(fill = "grey95")
  )

ggdraw() +
  draw_plot(malawi) +
  draw_plot(chilwa,
            height = 0.15,
            x = -0.05,
            y = 0.15
  )
#MPC data: LAKE CHILWA
#roi <- c(lon_min = 37.5, lat_min = -9.5,
#         lon_max = 42.5, lat_max = -12.5)


sits_list_collections(source="MPC")

LSC2LS_cube_MPC <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "CLOUD"),
  roi = roi,
  start_date = "2007-06-01",
  end_date = "2009-06-10"
)

sits_cube_copy(
  cube       = LSC2LS_cube_MPC,
  output_dir = "./tempdir",
  res        = 30,
  roi        = roi
)

# Regularize the cube to 15 day intervals
reg_cube_chilwa <- sits_regularize(
  cube       = LSC2LS_cube_MPC,
  output_dir = "./tempdir",
  res        = 30,
  period     = "P15D",
  multicores = 2
)

sits_timeline(LSC2LS_cube_MPC)

plot(LSC2LS_cube_MPC, red = "RED", green = "GREEN", blue = "BLUE", date="2008-11-02")

plot(reg_cube_chilwa, red = "RED", green = "GREEN", blue = "BLUE")










stac_query = rstac::stac_search(
  stac_source, 
  collections = "landsat-c2ard-sr",
  datetime = "1990-02-01T00:00:00Z/2020-03-01T12:31:12Z",
  bbox = c(-47.02148,-17.35063,-42.53906,-12.98314)) |>
  rstac::post_request() 
























library(ggplot2)
ee = readEE(system.file("~/Downloads/landsat5_c2_l2_t1.csv", package = "RStoolbox"))
ee = read_csv("/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube/USGS/landsat5_c2_l2_t1.csv")
ggplot(ee) + 
  geom_segment(aes(x = Date, xend = Date, y = 0, yend = 100 - Cloud.Cover, 
                   col = as.factor(Year))) +
  scale_y_continuous(name = "Scene quality (% clear sky)")
stackMeta(ee, quantity = "all", category = "image", allResolutions = FALSE)

#######################  usgs
stac_source <- rstac::stac(
  "https://landsatlook.usgs.gov/stac-server/")

rstac::get_request(stac_source)
collections_query <- stac_source |>
  rstac::collections()
class(collections_query)
available_collections <- rstac::get_request(
  collections_query)
available_collections

roi = sf::st_read("~/Desktop/git_repos/remote-sensing-lake-chilwa/roi/chilwa_watershed_4326.shp")[1, ] |>
  sf::st_transform(4326) |>
  sf::st_bbox()
roi

stac_query = rstac::stac_search(stac_source,
                                collections = "landsat-c2ard-sr",
                                datetime = "1990-02-01T00:00:00Z/2020-03-01T12:31:12Z",
                                bbox = c(-47.02148,-17.35063,-42.53906,-12.98314)) |>
  rstac::post_request() 

stac_query %>% ext_filter(
  q = stac_source,
  collections = "landsat-c2ard-sr", limit = 1000,
  datetime = "1990-02-01T00:00:00Z/2020-03-01T12:31:12Z",
  bbox = c(-47.02148,-17.35063,-42.53906,-12.98314)
)

stac_query %>% ext_filter(
  collection == "landsat-c2ard-sr" && 
    t_intersects(datetime, time_range) &&
    s_intersects(geometry, bbox_geojson) &&
    platform == "landsat-8" && 
    `eo:cloud_cover` <= 5) 


lapply(stac_query$features, 
       \(x) names(x$properties)) |> 
  unlist() |> unique()
lapply(stac_query$features, 
       \(x) data.frame(
         id = x$id, platform = x$properties$platform)) |> 
  do.call(what = rbind)

bbox_geojson <- rstac::cql2_bbox_as_geojson(roi)
time_range <- rstac::cql2_interval("1990-03-01", "2020-01-31")



executed_stac_query = get_request(stac_query)
assets_download(
  executed_stac_query,
  asset_names = "thumbnail", 
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube/USGS")











































rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
  rstac::ext_filter(
    collection == "landsat-c2-l2" &&
      t_intersects(datetime, ) &&
      s_intersects(geometry, ) && 
      `eo:cloud_cover` < 10) 

rstac::post_request()


bbox = c(-81.96624,26.38141,-81.80351,26.56457)



executed_stac_query = rstac::get_request(stac_query)
executed_stac_query




####################### AWS rstac
s_obj <- stac("https://brazildatacube.dpi.inpe.br/stac/")
get_request(s_obj)
#> ###STACCatalog
#> - id: bdc
#> - description: Brazil Data Cube Catalog
#> - field(s): description, id, stac_version, links
bdc_stac_token = ""
url_service = "https://brazildatacube.dpi.inpe.br/tiler/tms"
#MPC data: LAKE CHILWA
#roi <- c(lon_min = 37.5, lat_min = -9.5,
#         lon_max = 42.5, lat_max = -12.5)

it_obj <- s_obj |>
  stac_search(collections = "LANDSAT-MOZ_30_1M_STK-1",
              bbox = c(36.4, -16.0, 35.0, -14.6),
              limit = 100) |> 
  get_request()

it_obj

it_obj <- s_obj |> 
  ext_filter(
    collection == "LANDSAT-MOZ_30_1M_STK-1" &&
      `eo:cloud_cover` <= 10 && bbox == c(36.4, -16.0, 35.0, -14.6) && 
      anyinteracts(datetime, interval("1990-02-01", "2020-02-01"))
  ) |>
  post_request()

######################## BDC wtss
# Connect to the WTSS server at INPE Brazil
wtss_inpe <-  "https://brazildatacube.dpi.inpe.br/wtss/"
Rwtss::list_coverages(wtss_inpe)
desc <- Rwtss::describe_coverage(wtss_inpe, name = "LANDSAT-MOZ_30_1M_STK-1")

roi = sf::st_read("~/Desktop/git_repos/remote-sensing-lake-chilwa/roi/chilwa_watershed_4326.shp") |>
  sf::st_transform(4326) |>
  sf::st_bbox()

roi = sf::st_transform(roi, 32636)
plot(st_geometry(roi), reset=FALSE)
ggplot(roi) +
  geom_sf() +
  coord_sf(default_crs = sf::st_crs(32636))



LSC2LS_cube_MPC <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2", 
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "CLOUD"),
  roi = roi,
  start_date = "2007-06-01",
  end_date = "2008-06-01"
)

# Regularize the cube to 15 day intervals
reg_cube_chilwa <- sits_regularize(
  cube       = LSC2LS_cube_MPC,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/reg_cube",
  res        = 30,
  period     = "P30D",
  multicores = 2,
  roi        = roi
)

sits_timeline(reg_cube_chilwa)
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-06-02")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-07-02")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-08-31")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-09-30")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-10-30")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-11-29")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2007-12-29")
reg_cube_chilwa |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", date="2008-01-28")

sits_list_collections()

LS2M_cube_BDC <- sits_cube(
  source = "BDC",
  collection = "LANDSAT-2M",
  tiles = "072052",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "SWIR22", "CLOUD"),
  start_date = "2007-06-01",
  end_date = "2008-06-01"
)

roi <- c(
  lon_min = 35.0, lat_min = -16.0,
  lon_max = 36.4, lat_max = -14.6
)

LSC2L2_cube_AWS <- sits_cube(
  source = "AWS",
  collection = "LANDSAT-C2-L2",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "CLOUD"),
  roi = roi,
  start_date = "2007-06-01",
  end_date = "2008-06-01"
)

LSC2L2_cube_MPC <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "CLOUD"),
  roi = roi,
  start_date = "2007-06-01",
  end_date = "2008-06-01"
)


LSC2L2_cube_DEA <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16", "CLOUD"),
  roi = roi,
  start_date = "2007-06-01",
  end_date = "2008-06-01"
)

sits_list_collections(source = "DEAFRICA")


LSC2L2_local_MPC = sits_cube_copy(
  cube       = LSC2L2_cube_MPC,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube/MPC",
  res        = 30,
  roi        = roi
)

LSC2L2_local_MPC <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  data_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube/MPC"
)



LSC2L2_local_AWS(
  cube       = LSC2L2_cube_AWS,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube/MPC",
  res        = 30,
  roi        = roi
)


reg_cube_AWS <- sits_regularize(
  cube       = LSC2LS_cube_AWS,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/reg_cube/AWS",
  res        = 30,
  period     = "P30D",
  multicores = 2,
  roi        = roi
)

reg_cube_MPC <- sits_regularize(
  cube       = LSC2L2_local_MPC,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/reg_cube/MPC",
  res        = 30,
  period     = "P30D",
  multicores = 2,
  roi        = roi
)
sits_timeline(reg_cube_MPC)

reg_cube_MPC |>
  dplyr::filter(tile == "167070") |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", 
       date="2007-06-01") ##TCI plotted


mosaic_cube_chilwa <- sits_mosaic(
  cube = reg_cube_chilwa,
  roi = roi,
  crs = "EPSG:4326",
  output_dir = tempdir()
)

LSC2LS_cube_MPC |>
  dplyr::filter(tile == "167071") |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", 
       date="2007-06-01")

LSC2LS_cube_MPC |>
  dplyr::filter(tile == "167070") |>
  plot(red = "RED", green = "GREEN", blue = "BLUE", 
       date="2007-06-01") ##TCI plotted

sits_cube_copy(
  cube       = LSC2LS_cube_MPC,
  output_dir = "/media/seamus/TOSHIBA_EXT/chilwa/data/raw_cube",
  res        = 30,
  roi        = roi
)


reg_cube_chilwa <- sits_cube(
  source = "MPC",
  collection = "LANDSAT-C2-L2",
  bands = c("BLUE", "RED", "GREEN", "NIR08", "SWIR16"),
  roi = roi,
  data_dir = "/media/seamus/Ubuntu 22_04 LTS amd64/chilwa/dataset"
)

sits_timeline(reg_cube_chilwa)

reg_cube_chilwa |>
  dplyr::filter(tile == "166071") |>
  plot(red = "RED", green = "GREEN", blue = "BLUE")


plot(reg_cube_chilwa, red = "RED", green = "GREEN", blue = "BLUE", date="2007-11-29")

mosaic_cube_chilwa <- sits_mosaic(
  cube = reg_cube_chilwa,
  roi = roi,
  crs = "EPSG:4326",
  output_dir = tempdir()
)

sits_timeline(reg_cube_chilwa)


