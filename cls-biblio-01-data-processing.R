# cls biblio data processing
# MD 2024


# Packages and useful functions -------------------------------------------

list.of.packages <- c("tidyverse",
                      "bibliometrix",
                      "openalexR",
                      "dimensionsR",
                      "pubmedR",
                      "rscopus",
                      "tmaptools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


# smol things
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
median.na <- function(x) median(x, na.rm= T)
min.na <- function(x) min(x, na.rm = T)
max.na <- function(x) max(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)
sum.na <- function(x) sum(x, na.rm = T)



# Load data ---------------------------------------------------------------

# get WOS data
d <- read_tsv("data/cls-wos-export-2024-03-15.txt")

# this being WoS, there are loads of fields. These are the most relevant:

# AF author full names (AU is initialised, less useful)
# C1 authors, full affiliations, addresses
# C3 institutions (sep=";") — this field is much more coarse grained
# RP corresponding author
# OI ORCIDs (if known)
# FU funding project 
# FP funding partner 


# get affiliations
authors_and_affiliations <- d$C1

# first extrude the authors part
affiliations <- gsub("\\[.*?\\] ", "", authors_and_affiliations)
# now separate them
affiliations_separated <- unlist(strsplit(affiliations, "; "))

# get departments + institutions from the unstructured affiliations string
institutions <- gsub(",.*","", affiliations_separated)

# alternatively you could get institutions from the C3 field
# institutions <- trimws(unlist(strsplit(d$C3, ";")))

# BTW a quick look at the unique entries shows that WOS data is pretty messy,
# with duplictates and typos
sort(unique(institutions))

# get countries from affiliations
countries <- sub(".*?,\\s*(.*?)$", "\\1", affiliations_separated)
# we probably don't want the postal code in the country name
countries[str_detect(countries,"USA")] <- "USA"

# get cities (and or in some cases provinces, annoyingly)
cities <- sub(".*?,\\s*(.*?),\\s*[^,]*$", "\\1", affiliations_separated)

# make a df `locations`, do some cleaning
locations <- as_tibble(data.frame(affiliations_separated,institutions,cities,countries))
names(locations) <- c("affiliation_raw","institution","city_raw","country")

# the city field is actually an address field
# usually, BUT NOT ALWAYS, the last element is the city
locations$city <- stringi::stri_extract_last_words(locations$city_raw)

# annoyingly, quite a lot of data points require manual cleanup
# print this to spot the most obvious errors
sort(unique(locations$city))

corrections <- list("00101" = "Helsinki",
                    "01006" = "Bilbao",
                    "04310" = "Seoul",
                    "0DS" = "London",
                    "200062" = "Shanghai",
                    "20009" = "San Sebastian",
                    "200012" = "Shanghai",
                    "200122" = "Shanghai",
                    "2006" = "Sydney",
                    "2109" = "Sydney",
                    "2751" = "Sydney",
                    "28015" = "Madrid",
                    "28248" = "Madrid",
                    "2E7" = "Edmonton",
                    "361005" = "Xiamen",
                    "3BH" = "Oxford",
                    "4072" = "Brisbane",
                    "6GG" = "Oxford",
                    "7610658" = "Santiago",
                    "AB" = "Edmonton",
                    "ACT" = "Canberra",
                    "Andrews" = "St Andrews",
                    "Angeles" = "Los Angeles",
                    "Arbor" = "Ann Arbor",
                    "Antrim" = "Belfast",
                    "Avon" = "Bristol",
                    "Bilt" = "De Bilt",
                    "BC" = "Victoria",
                    "C" = "Aarhus",
                    "Catalunya" = "Barcelona",
                    "Coast" = "Cape Coast",
                    "Colchester" = "Essex",
                    "Coll" = "State College",
                    "Delhi" = "New Delhi",
                    "DF" = "Mexico City",
                    "Diego" = "San Diego",
                    "Fe" = "Santa Fe",
                    "Gables" = "Miami",
                    "Gelderland" = "Nijmegen",
                    "Gwynedd" = "Bangor",
                    "HD" = "Nijmegen",
                    "Hague" = "The Hague",
                    "Heide" = "Huis Ter Heide",
                    "Jolla" = "La Jolla",
                    "Juan" = "San Juan", #Puerto Rico
                    "Keynes" = "Milton Keynes",
                    "Kong" = "Hong Kong",
                    "Lanark" = "Glasgow",
                    "Lancs" = "Preston",
                    "Middx" = "Uxbridge",
                    "Midlands" = "Birmingham",
                    "Midlothian" = "Edinburgh",
                    "MT" = "Haifa",
                    "Nadu" = "Tamil Nadu",
                    "Neuve" = "Louvain La Neuve",
                    "Nouvelle" = "Paris",
                    "NSW" = "Sydney",
                    "ON" = "Toronto",
                    "Pk" = "State College",
                    "PL" = "Vitoria",
                    "Pradesh" = "Uttar Pradesh",
                    "Provence" = "Aix En Provence",
                    "PQ" = "Quebec",
                    "Qld" = "Brisbane",
                    "Radboud" = "Nijmegen",
                    "RM" = "Rome",
                    "SA" = "Adelaide",
                    "Sebastian" = "San Sebastian",
                    "Shertogenbosch" = "Den Bosch",
                    "sHertogenbosch" = "Den Bosch",
                    "Shi" = "Okazaki Shi",
                    "Stockhol" = "Stockholm",
                    "Vic" = "Melbourne",
                    "WA" = "Perth",
                    "York" = "New York",
                    "Yorkshire" = "York",
                    "Yvette" = "Paris")

correct_city <- function(city_name) {
  if (city_name %in% names(corrections)) {
    return(corrections[[city_name]])
  } else {
    return(city_name)
  }
}
locations$city <- sapply(locations$city, correct_city)
sort(unique(locations$city))

# also some corrections for institutions

corr_institutions <- list("Int Max Planck Res Sch Language Sci" = "Max Planck Inst Psycholinguist",
                          "IMPRS Language Sci" = "Max Planck Inst Psycholinguist",
                          "Ctr Language Studies" = "Radboud Univ Nijmegen",
                          "Vrije Univ" =  "Vrije Univ Amsterdam",
                          "Wageningen Univ & Res" = "Wagening Univ",
                          "Zuyd Univ Appl Sci" = "Zuyd Univ",
                          "University College London" = "UCL")
correct_institution <- function(institution) {
  if (institution %in% names(corr_institutions)) {
    return(corr_institutions[[institution]])
  } else { 
    return(institution)
  }
}
locations$institution <- sapply(locations$institution, correct_institution)
sort(unique(locations$institution))

# get coordinates for cities

# first correct PRC to China so that OSM will correctly geocode Chinese cities
locations <- locations |>
  mutate(country = ifelse(country == "Peoples R China", "China", country)) |>
  mutate(city_country = paste(city,country,sep=", "))

# then get unique cities
unique_cities <- sort(unique(locations$city_country))

# and use geocode_OSM to get coordinates (this takes a minute)
library(tmaptools)
all_coordinates <- geocode_OSM(unique_cities,as.data.frame=T)

# whip them into lat_lon format and merge with the locations df
lat_lon <- all_coordinates |>
  select(query,lat,lon) |>
  dplyr::rename("city_country" = "query")
locations <- locations |>
  left_join(lat_lon,by="city_country",relationship="many-to-many")

write_csv(locations,"data/locations_raw.csv")
