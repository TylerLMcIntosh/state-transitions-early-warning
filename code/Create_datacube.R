

#install.packages(c("gdalcubes", "magrittr", "raster", "stars", "mapview", "viridis"))

library(gdalcubes)
library(dplyr)
library(cubelyr)
library(stars)


##############LCMAP cube #############
#############################################################################################
wd = "E:/Twensday-03/LCMAP_raw"

setwd(wd)

LCMAP.files = list.files("E:/Twensday-03/LCMAP_raw", pattern = ".tif", recursive = TRUE, full.names = TRUE)

head(LCMAP.files, 10)

sum(file.size(LCMAP.files)) /1000^3

#add_collection_format("https://github.com/Chathu84/ecotransform_datacube/blob/main/file_format.json", name = NULL)

get_year = as.Date(substr(basename(LCMAP.files), 17, 20), "%Y")
fname_all = basename(tools::file_path_sans_ext(LCMAP.files))
b_name = substr(fname_all, 35, nchar(fname_all))
my_collect = create_image_collection(LCMAP.files, date_time = get_year, band_names = b_name)
my_collect


extent(my_collect, srs = "EPSG:4326")

## create a cube with different spatial resolution and different space and time extent
lcmap.overview.30m = cube_view(srs = "EPSG:3857", extent = my_collect, dx = 30, dy = 30, dt = "P1Y", resampling = "average"
                            , aggregation = "median")

lcmap.overview.30m

lcmap.cube.30m = raster_cube(my_collect, lcmap.overview.30m)
lcmap.cube.30m

lcmap.cube.30m.lcpri = select_bands(lcmap.cube.30m, c("SCMAG"))
#plot(lcmap.cube.30m.lcpri)
#animate(lcmap.cube.30m.lcpri)

lcmap.cube.30m_max = reduce_time(lcmap.cube.30m.lcpri, "max(SCMAG)")

lcmap.cube.30m_var = reduce_time(lcmap.cube.30m.lcpri, "var(SCMAG)")
dev.new(width=2, height=1)
plot(lcmap.cube.30m_var)

lcmap.cube.30m_med = reduce_time(lcmap.cube.30m.lcpri, "median(SCMAG)")
dev.new(width=2, height=1)
plot(lcmap.cube.30m_med)

lcmap.cube.30m.var = reduce_time(lcmap.cube.30m, names=c("sd_SCMAG", "sd_SCSTAB","sd_SCTIME","sd_SCLAST","sd_SCMQA"), 
                                "var(SCMAG)","var(SCSTAB)","var(SCTIME)","var(SCLAST)","var(SCMQA)")




write_tif(
  lcmap.cube.30m.var,
  dir = wd,
  prefix = basename(tempfile(pattern = "cube_var_")),
  overviews = FALSE,
  COG = FALSE,
  rsmpl_overview = "nearest",
  creation_options = NULL,
  write_json_descr = FALSE,
  pack = NULL
)


var_LCMAP = stack("cube_var_2a8c230f2671986-01-01.tif")
plot(var_LCMAP)
######spatial sampling and through time analysis

lcmap.overview.500m = cube_view(srs = "EPSG:3857", extent = my_collect, dx = 500, dy = 500, dt = "P1Y", resampling = "average"
                               , aggregation = "median")

lcmap.overview.500m

lcmap.cube.500m = raster_cube(my_collect, lcmap.overview.500m)
lcmap.cube.500m

lcmap.cube.500m.sd = reduce_time(lcmap.cube.500m, names=c("sd_SCMAG", "sd_SCSTAB","sd_SCTIME","sd_SCLAST","sd_SCMQA"), 
                                "sd(SCMAG)","sd(SCSTAB)","sd(SCTIME)","sd(SCLAST)","sd(SCMQA)")




write_tif(
  lcmap.cube.500m.sd,
  dir = wd,
  prefix = basename(tempfile(pattern = "cube_sd_500")),
  overviews = FALSE,
  COG = FALSE,
  rsmpl_overview = "nearest",
  creation_options = NULL,
  write_json_descr = FALSE,
  pack = NULL
)

lcmap.cube.30m.lcconf = select_bands(lcmap.cube.30m, c("LCPCONF"))

lcmap.cube.30m.lcconf.cnt = reduce_time(lcmap.cube.30m.lcconf, "count(LCPCONF)")



library(magrittr) # get the pipe

# update v by changing temporal extent only
v1 = cube_view(view=lcmap.overview.90m, extent=list(t0="2000-01-01", t1="2005-01-01")) 
raster_cube(my_collect, v1)  %>%
  select_bands(c("LCPRI")) %>%
  apply_pixel("LCPRI * 1") %>% # apply scale
  window_time(kernel=c(-1,1), window=c(1,0)) %>%
  plot(col=terrain.colors, key.pos=1,t = 2:6, nrow = 6)

write_ncdf(lcmap.cube.90m, file.path(wd, basename(tempfile(fileext = ".nc"))))
gdalcubes_options(ncdf_compression_level = 0)



lcmap.clear_mask = image_mask("LCPRI", values=c(0, 1, 2, 5, 6, 7, 8), invert = TRUE)

x = raster_cube(my_collect, lcmap.cube.90m, mask=lcmap.clear_mask) 

# v.subarea.90m = cube_view(extent=list(left=-106.486, right=-104.6231, bottom=36.23572, top=37.70194, 
#                                       t0="1986-03-15", t1="2021-03-15"), dt="P1Y", dx=90, dy=90, srs="EPSG:4326", 
#                           aggregation = "median", resampling = "average")
# 
# lcmap.cube.overview = raster_cube(my_collect, v.subarea.90m)
# lcmap.cube.overview


