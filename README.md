# how_many_within

## What is this?
In this GitHub I have included all relevant code to use and build that `how_many_within` function described in this Medium Article. The function uses OpenStreetMap's R library `osmdata` to pull coordinates from two amenities within a location and then says how many of one of the amenities is within a set distance from the other object.

Specifically, the user inputs a string into the function like-so:

`how_many_within("How many [amenity1] are within [radius] meters of at least one [amenity2] in [location]?")`

And the function outputs:

"There were [n amenity2][amenity2] found and [n amenity1][amenity1] found. Of these [amenity1]s [n intersect of amenity1 and the boundary around amenity2] ([% intersect of amenity1 and the boundary around amenity2]) are within [radius] meters of at least one [amenity2] in [location]"

Along with a map of labelled `amenity1`s and `amenity2`s in the location. 

## Test
`how_many_within("How many atms are within 416 meters of at least one pharmacy in Hampton, Virginia?")`

"There were 7 pharmacys found and 8 atms found. Of these atms 2 (25%) are within 416 meters of at least one pharmacy in Hampton, Virginia"

`how_many_within("How many atms are within 416 meters of at least one pharmacy in Denver, Colorado?")`

"There were 51 pharmacys found and 93 atms found. Of these atms 23 (24.73%) are within 416 meters of at least one pharmacy in Denver, Colorado"

`how_many_within("How many atms are within 416 meters of at least one pharmacy in Tallahassee, Florida?")`

"There were 22 pharmacys found and 11 atms found. Of these atms 4 (36.36%) are within 416 meters of at least one pharmacy in Tallahassee, Florida"


## Making it better
Please feel free to make this function even better (especially the grammar).
