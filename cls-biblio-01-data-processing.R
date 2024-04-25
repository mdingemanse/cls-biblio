# cls biblio data processing
# MD 2024


# Packages and useful functions -------------------------------------------

list.of.packages <- c("tidyverse",
                      "bibliometrix",
                      "openalexR",
                      "dimensionsR",
                      "pubmedR",
                      "rscopus",
                      "tmaptools",
                      "roadoi",
                      "ggthemes")
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



# Organize datasets -------------------------------------------------------


#* Web of Science data, 2018-2023 ------------------------------------------


# how did we get the data?


# QUERY 1: 2018-2023 papers with a "Radboud University Nijmegen" affiliation and
# additionally a "Centre For Language Studies" affiliation in the Department
# field:
# https://www.webofscience.com/wos/woscc/summary/32292d0f-feb3-4906-b96e-0a5ae62f24a6-d78cf0fb/relevance/1

# QUERY 2: 2018-2023 papers with a "Radboud University Nijmegen" affiliation and
# additionally a "CLS" mention in the Address field:
# https://www.webofscience.com/wos/woscc/summary/7527056e-4bee-42fc-9d71-842665408eaa-d78ca383/relevance/1

# QUERY 3: same, but with a "Language and Communication, Nijmegen" mention in the Address
# field:
# https://www.webofscience.com/wos/woscc/summary/dcd1aa90-3328-4ee4-bb8d-9ed57d101afd-d79219bf/relevance/1

# QUERY 4: same, but with a "Language and Communication" mention in the Address
# field:
# https://www.webofscience.com/wos/woscc/summary/dcd1aa90-3328-4ee4-bb8d-9ed57d101afd-d79219bf/relevance/1
# this query didn't deliver anything we did not already have and adds a few
# non-CLS papers, so not using it below

# get WOS data
d1 <- read_tsv("data/cls-wos-query1-20240322.txt")
d2 <- read_tsv("data/cls-wos-query2-20240322.txt")
d3 <- read_tsv("data/cls-wos-query3-20240322.txt")

d <- rbind(d1,d2)

# there are only 4 duplicates, so query 2 was a good addition
d |>
  group_by_all() |>
  filter(n() >1) |>
  ungroup()

# adding d3
d <- rbind(d,d3)

# this adds 15 duplicates but still yields another 10 new papers
d |>
  group_by_all() |>
  filter(n() >1) |>
  ungroup()

d <- d |>
  group_by_all() |>
  slice(1) |>
  ungroup()

# For quality control, write a list of papers by CLS authors without CLS
# affiliation:
d3.noCLS <- d3 |>
  filter(!grepl("Language Studies",C1)) |>
  filter(!grepl("Language Studies",RP)) |>
  select(AF,TI,SO,DT,C1,C3,RP,EM)
#View(d3.noCLS)

write_excel_csv(d3.noCLS,"data/cls-papers-without-cls-affiliation.csv")

# upon inspection there's only one here that really doesn't belong (as in,
# having no authors that were at any point affiliated with CLS)
d <- d |>
  filter(AF != "Pika, Simone; Wilkinson, Ray; Kendrick, Kobin H.; Vernes, Sonja C.")



# this being WoS, there are loads of fields. These are the most relevant:

# AF author full names (AU is initialised, less useful)
# C1 authors, full affiliations, addresses
# C3 institutions (sep=";") — this field is much more coarse grained
# RP corresponding author
# OI ORCIDs (if known)
# FU funding project 
# FP funding partner
# DI doi (can be empty if monograph, in which case see D2)
# D2 doi of container

# set doi to lowercase across the board
d <- d |>
  mutate(DI = tolower(DI),
         D2 = tolower(D2))

# I considered setting DI to D2 if DI=NA and D2 has a value, but this affects
# only 6 items, one of which is then a book attributed wrongly to CLS
only_container_doi <- d |>
  filter(!is.na(D2) & is.na(DI))


write_csv(d, "data/cls-wos-2018_2023.csv")


#* Altmetric data, 2018-2023 -----------------------------------------------

# How did we get the data? Using a Free Librarian account, downloading the max
# number of DOIs, and repeating this until the list of DOIs in
# cls-just_the_dois.txt is exhausted; then merge all.

d.a <- readxl::read_xlsx("data/cls-altmetric-merged.xlsx") |>
  select(-c("Authors at my Institution","Departments","Journal ISSNs","Handle.net IDs","ISBN","National Clinical Trial ID","URI","PubMedCentral ID","ADS Bibcode","arXiv ID","RePEc ID","SSRN","URN","Google+ mentions","Badge URL")) |>
  dplyr::rename("altmetric.score" = "Altmetric Attention Score",
                "title" = "Title",
                "journal" = "Journal/Collection Title",
                "type" = "Output Type",
                "pubdate" = "Publication Date",
                "doi" = "DOI",
                "id.pubmed" = "PubMed ID",
                "news" = "News mentions",
                "blogs" = "Blog mentions",
                "policy" = "Policy mentions",
                "patents" = "Patent mentions",
                "twitter" = "X mentions",
                "weibo" = "Weibo mentions",
                "facebook" = "Facebook mentions",
                "wikipedia" = "Wikipedia mentions",
                "linkedin" = "LinkedIn mentions",
                "reddit" = "Reddit mentions",
                "pinterest" = "Pinterest mentions",
                "f1000" = "F1000 mentions",
                "qa" = "Q&A mentions",
                "youtube" = "Video mentions",
                "syllabi" = "Syllabi mentions",
                "mendeley" = "Number of Mendeley readers",
                "citations" = "Number of Dimensions citations",
                "details" = "Details Page URL",
                "publisher" = "Publisher Names"
  ) |>
  mutate(doi = tolower(doi)) # always set DOIs to lowercase
write_csv(d.a,"data/cls-altmetric.csv")


#* DOIs: get most inclusive list -------------------------------------------

# From METIS, hand-curated, we get a first list of DOIs:

doi.metis <- read_csv("data/cls-just_the_dois.txt",col_names = "doi") |>
  mutate(doi = tolower(doi)) |> unique()

# take the most inclusive set of DOIs from WOS, remove duplicates, set to
# lowercase
doi.wos <- d |>
  select(DI) |> unique() |> drop_na() |>
  dplyr::rename(doi = DI)


in_wos_notin_metis <- setdiff(doi.wos, doi.metis)
in_metis_notin_wos <- setdiff(doi.metis,doi.wos)

write_csv(in_wos_notin_metis,"data/cls-dois_in_wos_notin_metis.txt")


# What is in WoS but not in Metis? quick inspection in Zotero shows that many of
# these apparently *are* in metis, so the doi export from metis was somehow not
# complete.

# e.g., the following dois don't appear in the cls-just_the_dois.txt but they
# are actually in the metis output; something must have gone wrong in the
# reduction of that RIS file to just DOIs.
check_these_dois <- c("10.1075/celcr.22.01nac",
                      "10.1075/celcr.22.03nac",
                      "10.1075/celcr.22.04rei",
                      "10.1515/9783110730081-016",
                      "10.1007/978-3-319-78771-8_20",
                      "10.1016/j.jpsychores.2019.03.031",
                      "10.1017/s1366728917000116",
                      "10.1007/978-3-319-75487-1_28",
                      "10.4324/9780429199691-13",
                      "10.1007/978-3-319-69830-4_3", # this one?
                      "10.3389/fnhum.2019.00398",
                      "10.1109/icassp43922.2022.9747785",
                      )

d |>
  filter(DI %in% check_these_dois) %>%
  select(AU,DI) %>% arrange(DI)

# anyway, it's useful to have an inclusive list of DOIs

doi.all <- unique(unlist(c(doi.metis,doi.wos)))

write(doi.all,file="data/cls-dois_from_metis_and_wos.txt")


# * Unpaywall data --------------------------------------------------------

# get unpaywall data
# actually when I ran this I got a load of "Request failed []]404]" errors
# d.unpaywall <- roadoi::oadoi_fetch(dois = doi.all,
#                     email = "mark.dingemanse@ru.nl",
#                     .progress = "text",
#                     .flatten = TRUE)

d.u1 <- read_csv("data/cls-unpaywall-batch1.csv")
d.u2 <- read_csv("data/cls-unpaywall-batch2.csv")
d.u <- dplyr::full_join(d.u1,d.u2) |>
  mutate(year = ifelse(is.na(published_date),NA,str_extract(published_date, "^\\d{4}")))

write_csv(d.u,file="data/cls-unpaywall.csv")


# * Open data & repositories ----------------------------------------------

# Open data

# How did we get this? Manually looking for papers that mention OSF or Radboud
# Repository datasets or code on github

d.opendata <- read_csv('data/cls-opendata.csv') |>
  mutate(prop = has_repo/papers_with_dois * 100)

# How did we get the data? Supplied by Henk vd Heuvel, April 15, 2024

d.repos <- readxl::read_xlsx('data/cls-RU-repository-allcollections-2024-04-15_05-43.xlsx')

# make names usable
names(d.repos) <- sub(" ", "_", tolower(names(d.repos)))

d.repos <- d.repos |>
  mutate(creation_date = as.POSIXct(creation_date)) |>
  mutate(year = as.numeric(lubridate::year(creation_date)))


#* WoS locations and affiliations ------------------------------------------


# get affiliations
authors_and_affiliations <- d$C1

# first extrude the authors part
affiliations <- gsub("\\[.*?\\] ", "", authors_and_affiliations)
# now separate them
affiliations_separated <- unlist(strsplit(affiliations, "; "))

# get departments + institutions from the unstructured affiliations string
institutions <- gsub(",.*","", affiliations_separated)

# alternatively you could get institutions from the C3 field, but this is much
# more coarse grained (e.g., will "Max Planck Society" for MPI for Psycholinguistics)

# institutions <- trimws(unlist(strsplit(d$C3, ";")))

# BTW a quick look at the unique entries shows that WOS data is pretty messy,
# with duplictates and typos
sort(unique(institutions))

# get countries from affiliations
countries <- sub(".*?,\\s*(.*?)$", "\\1", affiliations_separated)
# we probably don't want the postal code in the country name
countries[str_detect(countries,"USA")] <- "USA"
countries[str_detect(countries,"Peoples R China")] <- "China"


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
                    "Paso" = "El Paso",
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

# city_country helps geocode more accurately
locations <- locations |>
  mutate(city_country = paste(city,country,sep=", "))

# get unique cities
unique_cities <- sort(unique(locations$city_country))

# and use geocode_OSM to get coordinates (this takes a minute)
all_coordinates <- tmaptools::geocode_OSM(unique_cities,as.data.frame=T)

# whip them into lat_lon format and merge with the locations df
lat_lon <- all_coordinates |>
  select(query,lat,lon) |>
  dplyr::rename("city_country" = "query")
locations <- locations |>
  left_join(lat_lon,by="city_country",relationship="many-to-many")

write_csv(locations,"data/cls-locations_raw.csv")




# Analyses ----------------------------------------------------------------

#* Open access and open science -----------------------------------------------

d.u |>
  group_by(genre) |>
  summarise(n=n())

#plottable df

d.p <- d.u |>
  drop_na(year) |>
  filter(year > 2017 & year < 2024) |>
  mutate(type = case_when(
    genre %in% c("proceedings","proceedings-article","posted-content") ~ "proceedings",
    .default = as.character(genre)
  )) |>
  mutate(open = ifelse(oa_status == "closed","closed","open")) |>
  mutate(oa_status = ordered(oa_status, levels=c("closed","gold","bronze","green","hybrid"))) |>
  mutate(oa_status_simpler =
           case_when(
             oa_status == "closed" ~ "closed",
             oa_status %in% c("gold","hybrid") ~ "open",
             oa_status %in% c("bronze","green") ~ "green",
             .default = as.character(oa_status)
           ))
  

d.u |>
  drop_na(year) |>
  filter()

d.p |>
  ggplot(aes(x=year,fill=oa_status_simpler)) +
  theme_economist() +
  theme(plot.title.position = "plot") +
  ggtitle("A. Open access status") +
  labs(x="year",y="", fill="") +
  geom_bar(position="fill") 
p1 <- last_plot()


d.opendata |>
  ggplot(aes(x=year,y=prop)) +
  theme_economist() +
  theme(plot.title.position = "plot") +
  ggtitle("B. Open science trends") +
  ylim(0,20) +
  labs(x="year",y="% of papers with open data or code") +
  geom_line()
p2 <- last_plot()
  

d.repos |>
  filter(year != 2024) |>
  ggplot(aes(x=year)) +
  theme_economist() +
  ggtitle("C. Repository deposits") + 
  xlim(c(2018,2024)) +
  labs(x="year",y="number of deposits") + 
  geom_bar()
p3 <- last_plot()


cowplot::plot_grid(p1,p2,p3,nrow=1)
ggsave('figures/panel_openscience.png',width=12,height=4,bg="white")

# what is the year on year increase in repositories?
d.repos |>
  group_by(year) |>
  dplyr::summarise(n=n(),
                   views=sum(views),
                   downloads=sum(downloads),
                   viewers=sum(viewers),
                   downloaders=sum(downloaders))


d.repos |>
  group_by(year) |>
  dplyr::summarise(views=sum(views),downloads=sum(downloads),viewers,downloaders)


#* Altmetric data --------------------------------------------------------

d.a |>
  mutate(year = lubridate::year(pubdate)) |>
  filter(altmetric.score > 0,
         year %in% c(2018:2023)) |>
  group_by(year) |>
  dplyr::summarise(altmetric=sum(altmetric.score),
                   news=sum(news),
                   wikipedia=sum(wikipedia),
                   ref=sum(mendeley),
                   cites=sum(citations))

# do Mendeley saves predict cites?
d.a.cites <- d.a |>
  mutate(year = lubridate::year(pubdate)) |>
  filter(year %in% c(2018:2023)) |>
  select(year,doi,mendeley,citations) |>
  arrange(desc(mendeley))

cor.test(d.a.cites$mendeley,d.a.cites$citations)

cor.test(d.a.cites$mendeley,d.a.cites$citations)

d.a.cites |>
  group_by(year) |>
  dplyr::summarise(r=cor.test(mendeley,citations)$estimate)
                   