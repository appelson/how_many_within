#------------------------ Loading Libraries --------------------
# Load required libraries
library(osmdata)
library(sf)
library(tmap)
library(tidyverse)
library(units)


#------------------- Necessary components -----------------------
# Exit function
exit <- function() { invokeRestart("abort") } 

# Defining amenities
amenities <- available_tags("amenity")$Value

#------------- Defining the closeness function -----------------------

# Closeness function
closeness <- function(location, amenity1, amenity2, radius){
  
  # Checking for correct amenities
  if(!(amenity1 %in% amenities) | !(amenity2 %in% amenities)){
    print("Choose an amenity in the list: ")
    print(amenities)
    exit()
  }
  
  # Checking for correct radii
  if(!is.numeric(radius) | radius < 1){
    print("Choose a numeric radius greater than 0")
    exit()
  }
  
  # Defining query 1
  tryCatch({
    query_1 <- opq(bbox = location) %>%
      add_osm_feature(key = "amenity", value = amenity1) %>%
      osmdata_sf()
  }, error = function(e) {
    print("Choose a new location")
    exit()
  })
  
  # Defining query 2
  tryCatch({
    query_2 <- opq(bbox = location) %>%
      add_osm_feature(key = "amenity", value = amenity2) %>%
      osmdata_sf()
  }, error = function(e) {
    print("Choose a new location")
    exit()
  })
  
  
  # Checks to ensure that query_1 is not empty
  if (nrow(query_1$osm_points) == 0 & nrow(query_1$osm_polygons) == 0) {
    output_text1 <- paste0("Choose a new reference amenity in the list:")
    print(amenities)
    print(output_text1)
    exit()
  }
  
  # Checks to ensure that query_2 is not empty
  if (nrow(query_2$osm_points) == 0 & nrow(query_2$osm_polygons) == 0) {
    output_text2 <- paste0("Choose a new test amenity in the list:")
    print(amenities)
    print(output_text2)
    exit()
  }
  
  # Choosing a boundary location
  location_boundary <- tryCatch(
    {
      result <- getbb(location, format_out = "sf_polygon")$multipolygon
      if (is.null(result)) {
        getbb(location, format_out = "sf_polygon")$geometry
      } else {
        result
      }
    },
    error = function(e) {
      getbb(location, format_out = "sf_polygon")$geometry
    }
  )
  
  # amenity1 coordinates
  query_1_point <- query_1$osm_points
  
  # amenity1 centroid coordinates
  query_1_centroid <- query_1$osm_polygons %>%
    st_centroid()
  
  # Combining coordinates and centroid coordinates
  all_query1 <- bind_rows(query_1_point, query_1_centroid)
  
  # Transforming the CRS  
  st_crs(all_query1) <- 4326
  
  # Filtering amenities to just that of amenity1
  all_query1 <- all_query1 %>%
    filter(amenity == amenity1)
  
  # amenity2 coordinates  
  query_2_point <- query_2$osm_points
  
  # amenity2 centroid coordinates
  query_2_centroid <- query_2$osm_polygons %>%
    st_centroid()
  
  # Combining coordinates and centroid coordinates
  all_query2 <- bind_rows(query_2_point, query_2_centroid)
  
  # Transforming the CRS
  st_crs(all_query2) <- 4326
  
  # Filtering our coordinates to our location boundary
  clipped_query_1 <- st_intersection(all_query1, st_transform(location_boundary, 4326))
  clipped_query_2 <- st_intersection(all_query2, st_transform(location_boundary, 4326))
  
  # Changing the CRS from 4326 to 7856
  plot_query_1 <- st_transform(clipped_query_1, crs = st_crs(7856))
  plot_query_2 <- st_transform(clipped_query_2, crs = st_crs(7856))
  
  # Adding a buffer
  buffer <- plot_query_1 %>%
    st_buffer(dist = set_units(radius, m)) %>% 
    summarise()
  
  # Plot without an intersection
  if(nrow(st_intersection(plot_query_2, buffer)) == 0){
    
    m <- tm_shape(location_boundary) +
      tm_borders() +
      tm_shape(plot_query_1) +
      tm_dots(col = "blue", popup.vars = c("name")) +
      tm_shape(plot_query_2) +
      tm_dots(col = "red", popup.vars = c("name")) + 
      tm_shape(buffer) +
      tm_polygons(col = "pink", alpha = 0.4) +
      # Creating a legend
      tm_add_legend(title = location,
                    labels = c(paste0(amenity1, "s"),  
                               paste0(amenity2, 
                                      "s more than ",
                                      radius, 
                                      " meters from a ",
                                      amenity1),
                               paste0(amenity2, 
                                      "s less than ",
                                      radius, 
                                      " meters from a ",
                                      amenity1),
                               paste0(radius, 
                                      " meter radius around all ", 
                                      amenity1,
                                      "s")),
                    col = c("blue", "red", "yellow", "pink"),
                    type = "fill")
    
    print("No intersections")
  } else{
    # Plot without an intersection
    bounded_query_2 <- st_intersection(plot_query_2, buffer)
    m <- tm_shape(location_boundary) +
      tm_borders() +
      tm_shape(plot_query_1) +
      tm_dots(col = "red", popup.vars = c("name")) +
      tm_shape(plot_query_2) +
      tm_dots(col = "blue", popup.vars = c("name")) + 
      tm_shape(buffer) +
      tm_polygons(col = "pink", alpha = 0.4) +
      tm_shape(bounded_query_2) + 
      tm_dots(col = "yellow", popup.vars = c("name")) +
      # Creating a legend
      tm_add_legend(title = location,
                    labels = c(paste0(amenity1, "s"), 
                               paste0(amenity2, 
                                      "s more than ",
                                      radius, 
                                      " meters from a ",
                                      amenity1),
                               paste0(amenity2, 
                                      "s less than ",
                                      radius, 
                                      " meters from a ",
                                      amenity1),
                               paste0(radius, 
                                      " meter radius around all ", 
                                      amenity1, "s")),
                    col = c("red", "blue", "yellow", "pink"),
                    type = "fill")
  }
  
  # Number of intersections
  n_intersect = nrow(st_intersection(plot_query_2, buffer))
  
  # Number of amenity1
  n_query_1 = nrow(clipped_query_1)
  
  # Number of amenity2
  n_query_2 = nrow(clipped_query_2)
  
  # Percent of amenity2 intersections
  percent = round(100*(n_intersect/n_query_2),2)
  
  query_1_text <- paste0(nrow(clipped_query_1), 
                         " ",
                         amenity1, 
                         "s found")
  
  query_2_text <- paste0(nrow(clipped_query_2), 
                         " ",
                         amenity2, 
                         "s found")
  
  # Sentence form of output
  text <- paste0("There were ",
                 query_1_text,
                 " and ",
                 query_2_text,
                 ". Of these ",
                 amenity2,
                 "s "
                 ,n_intersect,
                 " (",
                 percent, 
                 "%) are within ", 
                 radius, 
                 " meters of at least one ",
                 amenity1, 
                 " in ",
                 location)
  
  # Returning outputs
  print(text)
  return(tmap_leaflet(m))
  
}

#------------- Defining the how_many_within wrapper function -----------------------

# How_many_within function
how_many_within <- function(string){
  suppressWarnings({
    word_list <- strsplit(str_replace(string, "\\?",""), " ")[[1]]  
    amenity1 <- word_list[12]
    amenity2 <- gsub(".{1}$", "",word_list[3])
    radius <- as.numeric(word_list[6])
    
    location <- paste(word_list[14:length(word_list)], collapse = " ")
    closeness(location, amenity1, amenity2, radius)
  })
}

#------------------------- Testing the Function ------------------------------------

# Running our function on the base case
how_many_within("How many school's are within 304.8 meters of at least one bar in New York City?")

# Cities in the United States
cities <- c(
  "New York City", "Los Angeles", "Chicago", "Houston",
  "Phoenix", "Philadelphia", "San Antonio",
  "San Diego", "Dallas", "San Jose", "Austin", "Jacksonville", "San Francisco",
  "Columbus", "Indianapolis", "Seattle", "Denver", "Washington, D.C.",
  "Boston", "Las Vegas", "Nashville", "Portland", "Atlanta", "Miami",
  "Charlotte", "Raleigh", "Minneapolis", "St. Louis", "Tampa", "Pittsburgh",
  "Cincinnati", "Cleveland", "New Orleans", "Kansas City", "Orlando",
  "Sacramento", "San Bernardino", "Austin", "Salt Lake City", "Louisville",
  "Riverside", "Milwaukee", "Hartford", "Buffalo", "Rochester", "Richmond",
  "Birmingham", "Spokane", "Tacoma", "Fresno", "Omaha", "Albuquerque",
  "Baton Rouge", "Boise", "Little Rock", "Providence", "Jackson",
  "Salt Lake City", "Worcester", "Knoxville", "Richmond", "Huntsville",
  "Grand Rapids", "Chattanooga", "Des Moines", "Tallahassee", "Buffalo",
  "Madison", "Shreveport", "Reno", "Montgomery", "Lubbock", "Boise",
  "Baton Rouge", "Birmingham", "Augusta", "Spokane", "Modesto", "Fayetteville",
  "Tacoma", "Oxnard", "Boise", "Aurora", "Oceanside", "Santa Clarita",
  "Garden Grove", "Oceanside", "Santa Rosa", "Fort Lauderdale", "Salinas",
  "Springfield", "Santa Maria", "Tallahassee", "Mesquite", "Syracuse",
  "Daytona Beach", "Springfield", "Lancaster", "Eugene", "Corpus Christi",
  "Wichita", "Oxnard", "Reno", "Fort Collins", "Allentown", "Evansville",
  "Abilene", "Biloxi", "Charleston", "Kalamazoo", "South Bend", "Lafayette",
  "Bakersfield", "Macon", "Aurora", "Huntsville", "Santa Barbara",
  "Worcester", "Knoxville", "Fayetteville", "Killeen", "Santa Clara",
  "Fort Wayne", "Santa Maria", "Hollywood", "Salem", "Clarksville",
  "Lakewood", "Torrance", "Alexandria", "Escondido", "Sunnyvale", "Pasadena",
  "Fullerton", "Orange", "Hampton", "Gainesville", "Miramar", "McAllen",
  "Pasadena", "Orange", "Fullerton", "Hampton", "Gainesville", "Miramar",
  "McAllen", "Pasadena", "Alexandria", "Escondido", "Sunnyvale", "Pasadena",
  "Alexandria", "Escondido", "Sunnyvale", "Clarksville", "Lakewood", "Torrance",
  "Pasadena", "Alexandria", "Escondido", "Sunnyvale", "Pasadena", "Alexandria",
  "Escondido", "Sunnyvale", "Pasadena", "Alexandria", "Escondido", "Sunnyvale"
)

# Setting a random seed 
set.seed(22)

# Defining variables randomly
amenity1 <- amenities[sample(1:length(amenities),1)]
amenity2 <- amenities[sample(1:length(amenities),1)]
radius <- sample(1:5000, 1)
location <- cities[sample(1:length(cities), 3)]

# Running the analysis on the random locations

# Hampton, Virginia
how_many_within("How many atms are within 416 meters of at least one pharmacy in Hampton, Virginia?")

# Denver, Colorado
how_many_within("How many atms are within 416 meters of at least one pharmacy in Denver, Colorado?")

# Tallahassee, Florida
how_many_within("How many atms are within 416 meters of at least one pharmacy in Tallahassee, Florida?")

# Running the analysis on a state
how_many_within("How many ice_creams are within 1609.34 meters of at least one prison in California?")