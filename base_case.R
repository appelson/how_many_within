#------------------------ Loading Libraries --------------------
# Load required libraries
library(osmdata)
library(sf)
library(tmap)
library(tidyverse)
library(units)

#----------------------- Getting Data ------------------------------------
# Loading schools in NYC
school_query <- opq(bbox = "New York City") %>%
  add_osm_feature(key = "amenity", value = "school")

# Loading bars in NYC
bar_query <- opq(bbox = "New York City") %>%
  add_osm_feature(key = "amenity", value = "bar")

# Extracting amenity locations
schools <- osmdata_sf(school_query)
bars <- osmdata_sf(bar_query)

# School coordinates
school_point <- schools$osm_points %>%
  filter(amenity == "school") # ensuring that we are getting real school objects

# School centroid coordinates
school_centroid <- schools$osm_polygons %>%
  st_centroid() %>% # calculating the centroid of the polygons
  filter(amenity == "school") # ensuring that we are getting real school objects

# Combining coordinates and centroid coordinates
all_school <- bind_rows(school_centroid, school_point)

# Transforming and setting CRS
st_crs(all_school) <- 4326 

# Bar coordinates
bar_point <- bars$osm_points %>%
  filter(amenity == "bar") # ensuring that we are getting real school objects

# Bar centroid coordinates
bar_centroid <- bars$osm_polygons %>%
  st_centroid() %>% # calculating the centroid of the polygons
  filter(amenity == "bar") # ensuring that we are getting real school objects

# Combining coordinates and centroid coordinates
all_bar <- bind_rows(bar_centroid, bar_point)

# Transforming and setting CRS
st_crs(all_bar) <- 4326

# Defining a function to return a plottable bounding box
get_geombox <- function(location){
  coordinate <- getbb(location, format_out = "matrix")
  geometry_box <- st_as_sfc(st_bbox(c(xmin = coordinate[1], 
                                      xmax = coordinate[3], 
                                      ymin = coordinate[2], 
                                      ymax = coordinate[4]), crs = 4326))
  return(geometry_box)
}

# Defining a function to return a plottable bounding box
get_geombox <- function(location){
  coordinate <- getbb(location, format_out = "matrix")
  geometry_box <- st_as_sfc(st_bbox(c(xmin = coordinate[1], 
                                      xmax = coordinate[3], 
                                      ymin = coordinate[2], 
                                      ymax = coordinate[4]), crs = 4326))
  return(geometry_box)
}

# Creating a bounding box
bounding_box <- get_geombox("New York City")

# Calculating the difference
difference <- st_make_valid(st_difference(bounding_box, nyc_boundary))

# Plotting using tmap
m <- tm_shape(nyc_boundary) +
  tm_borders("black") +
  tm_fill(col = "blue", alpha = 0.3) +
  tm_shape(difference) +
  tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "black") +
  tm_layout(frame = FALSE) +
  tm_add_legend(
    labels = c("Area inside NYC Boundary", "Area between NYC Boundary and Bounding Box"),
    col = c("blue", "red"),
    type = "fill")

tmap_leaflet(m)

# Getting New York City boundary
nyc_boundary <- getbb("New York City", format_out = "sf_polygon")$multipolygon

# Clipping schools within the NYC boundary
clipped_school <- st_intersection(all_school, st_transform(nyc_boundary, 4326))

# Clipping bars within the NYC boundary
clipped_bar <- st_intersection(all_bar, st_transform(nyc_boundary, 4326))


#----------------------- Adding a Perimeter -----------------------------
# Changing the CRS from 4326 to 7856
plot_school <- st_transform(clipped_school, crs = st_crs(7856))
plot_bar <- st_transform(clipped_bar, crs = st_crs(7856))


# Creating a buffer
buffer <- plot_bar %>%
  st_buffer(dist = set_units(304.8, m)) %>%  # creates a 304.8 meter buffer
  summarise()

#---------------------- Intersecting Geography -----------------------------

# Calculating the intersection between the buffer and schools
bounded_schools <- st_intersection(plot_school, buffer)

# Calculating the number of bounded schools in NYC
n_bounded <- nrow(bounded_schools)

# Calculating the proportion of bounded schools out of all schools in NYC
round(100 * (n_bounded/nrow(plot_school)) ,2)

# Defining our map
m <- tm_shape(nyc_boundary) +
  tm_borders() +
  tm_shape(plot_school) +
  tm_dots(col = "blue", popup.vars = c("name")) +
  tm_shape(plot_bar) +
  tm_dots(col = "red", popup.vars = c("name")) + 
  tm_shape(buffer) +
  tm_polygons(col = "pink", alpha = 0.4) +
  tm_shape(bounded_schools) + 
  tm_dots(col = "yellow", popup.vars = c("name")) +
  tm_add_legend(title = "New York",
                labels = c("Bars",
                           "Schools more than 304.8 meters away from a bar",
                           "Schools less than 304.8 meters away from a bar",
                           "304.8 meter radius around all bars"),
                col = c("red", "blue", "yellow", "pink"),
                type = "fill")

tmap_leaflet(m)