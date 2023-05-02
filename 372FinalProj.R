# load my libraries
library(tidyverse)
library(lubridate)
library(sf)
library(igraph)
library(tidycensus)

# To understand the demographics of my service area, I first imported data from 
# the American Community Survey 5-year estimates. I chose 5 years because I wanted
# a larger sample size.
acs_vars = load_variables(2019, "acs5")

# I saved it to a file and viewed it in Excel to choose my desired variables.
write_csv(acs_vars, "2019acs5yr.csv")

# Now I'm going to retrieve demographic data from ACS on Durham city by block group
# I chose block group because it was the smallest geographic unit I could use and
# I want my Census data to be descriptive of my study area
durham_city_demo = get_acs(
  geography="block group",
  variables=c(
    "total_pop"="B01003_001",
    "white"="B02001_002",
    "black"="B02001_003",
    "indigenous"="B02001_004",
    "asian"="B02001_005",
    "two_or_more"="B02001_008",
    "work_live_msa"="B08016_003",
    "work_diff_msa"="B08016_006",
    "vehicle_transit"="B08301_002",
    "public_transit"="B08301_010",
    "bike_transit"="B08301_018",
    "walk_transit"="B08301_019",
    "work_remote"="B08301_021",
    "hs_diploma"="B15003_017",
    "bachelors"="B15003_022",
    "masters"="B15003_023",
    "public_assistance_income"="B19057_002",
    "vacancies"="B25002_003",
    "black_owned_home_occupied"="B25003B_002"
  ),
  year=2019,
  state="NC",
  survey="acs5",
  output="wide"
)

# Importing the service area and an NC blockgroups shapefile
city_boundary = read_sf("Data/durham_city_boundaries.shp")
block_groups = read_sf("Data/2022_blockgroups/tl_2022_37_bg.shp")

# Join the census data to the block group shapefile
bg_demographics = left_join(block_groups, durham_city_demo, by="GEOID")

# Before completing the spatial join, I have to reproject the two shapefiles so they
# match (to NC State Plane, i.e. the project CRS)
city_boundary = st_transform(city_boundary, 32119)
bg_demographics = st_transform(bg_demographics, 32119)

# Run the spatial join
durham_city_dems = st_intersection(city_boundary, bg_demographics)

# Now I have all of my data in one table so I'm going to start making my graphs.
# First, I need to delete all null values.
races = filter(durham_city_dems, !is.na(whiteE), !is.na(blackE), !is.na(indigenousE), !is.na(asianE), !is.na(two_or_moreE))

# I used groupby and summarize to select the attributes I specifically wanted for
# this graph
race = group_by(races, NAME) %>%
  summarize(
    whiteE=sum(whiteE),
    blackE=sum(blackE),
    indigenousE=sum(indigenousE),
    asianE=sum(asianE),
    two_or_moreE=sum(two_or_moreE)
    )

# I was getting a bit confused with making my dataframe for my graph so I looked
# this up and it worked. It organized my table in the way I wanted so I could make 
# my graph function
race <- data.frame(
  Race=c("White","Black","Indigenous","Asian","Two or More") ,  
  Population=c(120215,90449,520,9549,7065)
)

# Then I made a bar chart based on my data frame
ggplot(race, aes(x=Race, y=Population)) +
  geom_col()

# I'm also going to look at commuter behavior, so I followed the same steps
commuter_behave = filter(durham_city_dems, !is.na(vehicle_transitE), !is.na(public_transitE), 
                         !is.na(bike_transitE), !is.na(walk_transitE), !is.na(work_remoteE))

commuters = group_by(commuter_behave, NAME) %>%
  summarize(
    vehicle_transitE=sum(vehicle_transitE),
    public_transitE=sum(public_transitE),
    bike_transitE=sum(bike_transitE),
    walk_transitE=sum(walk_transitE),
    work_remoteE=sum(work_remoteE)
  )

commuters <- data.frame(
  Mode=c("Car","Public Transit","Bike","Walk","Remote") ,  
  Population=c(102274,4492,778,2606,7093)
)

ggplot(commuters, aes(x=Mode, y=Population)) +
  geom_col()


# Understanding data prior to QGIS analysis

# Before starting all of my work in QGIS, I wanted to better understand my data

# I imported my Durham road network shapefile so I can identify the unique 
# types to create a walkability methodology 
durham_roadways = read_sf("Data/durham_city_roadways.shp")
durham_sidewalks = read_sf("Data/durham_city_sidewalks.shp")

unique(durham_roadways$FTR_CODE)
unique(durham_roadways$func_class)
unique(durham_sidewalks$FUNCCLASS)

# I think the func_class attribute does a better job at depicting road types. I
# will have to further look into what some of the function classes mean
# Now I want to see if there are missing speed limits and lane entries
sum(is.na(durham_roadways$SPEED_LMT))
sum(is.na(durham_roadways$LANES))

# There are zero null values for both! That makes defining and selecting walkable
# streets much easier.

# The rest of my network analysis was conducted in QGIS
