
library(sits)
library(sf)
library(ggplot2)
library(gdalcubes)
library(rnaturalearth)
library(rnaturalearthdata)
library(rstac)
library(terra)
library(gdalcubes)

roi = sf::st_read("~/Desktop/git_repos/remote-sensing-lake-chilwa/roi/chilwa_watershed_4326.shp")[1, ] |>
  sf::st_transform(4326) |>
  sf::st_bbox()
roi

library(rstac)
s = stac("https://landsatlook.usgs.gov/stac-server/")
items = s |>
  stac_search(collections = "landsat-c2l2-sr",
              bbox = c(roi["xmin"],roi["ymin"],
                       roi["xmax"],roi["ymax"]), 
              datetime = "1990-02-01T00:00:00Z/2020-03-01T12:31:12Z") |>
  post_request() |> items_fetch(progress = FALSE)
length(items$features)

library(gdalcubes)
L2T1_collection = stac_image_collection(
  items$features,  
  property_filter = function(x) {x[["eo:cloud_cover"]] < 10})
L2T1_collection

assets = c("blue","green","nir08","red","swir16","swir22")
L2T1_collection = stac_image_collection(
  items$features, asset_names = assets, 
  property_filter = function(x) {x[["eo:cloud_cover"]] < 10})
L2T1_collection

gdalcubes_options(parallel = 8)
v = cube_view(srs="EPSG:4326", dx=30, dy=30, dt="P30D", 
              aggregation="median", resampling = "average",
              extent=list(t0 = "1990-02-01", t1 = "2020-02-01",
                          left=roi["xmin"], right=roi["xmax"],
                          top=roi["ymax"], bottom=roi["ymin"]))
v

L2T1_reg_cube = raster_cube(L2T1_collection, v) 

#gdalcubes::animate(select_bands(
#  L2T1_reg_cube, c("red","green","blue")), 
#  rgb=3:1,
#  zlim=c(0,20000), 
#  fps=1,loop=1
#  )
#plot(rgb = 3:1, zlim = c(0,2500))


library(raster)
library(RStoolbox)
#load an example dataset
data(lsat)

#make up some endmember spectra: water and land
em_names <- c("water", "land")
pts <- data.frame(class=em_names, cell = c(47916,5294))
em <- lsat[pts$cell]
rownames(em) <- em_names

#unmix the image for water and land
probs <- mesma(lsat, em, method = "NNLS")

#take a look
raster::hist(probs$water)
print(lsat)
print(probs)
plot(probs)


## Run tasseled cap (exclude thermal band 6)
lsat_tc <- tasseledCap(lsat[[c(1:5,7)]], sat = "Landsat5TM")
lsat_tc
plot(lsat_tc)
