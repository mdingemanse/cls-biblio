# CLS bibliometrics: Map
# MD 2024


# Packages and useful functions

list.of.packages <- c("tidyverse",
                      "viridis",
                      "ggthemes",
                      "bibliometrix",
                      "openalexR",
                      "dimensionsR",
                      "pubmedR",
                      "rscopus",
                      "ggmap",
                      "tmaptools",
                      "ggrepel")
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

# useful resources



# Load data ---------------------------------------------------------------

locations.raw <- read_csv("data/cls-locations_raw.csv")

# simplify df and create one that has counts
locations <- locations.raw |>
  select(country, city, institution, lat, lon)
locations_counts <- locations |>
  group_by(city,lat,lon,country) |>
  summarise(count = n()) |>
  ungroup() |>
  arrange(desc(count))

locations_institutions_counts <- locations |>
  group_by(city,institution,lat,lon,country) |>
  summarise(count = n()) |>
  ungroup() |>
  arrange(desc(count))

# some descriptive stats
top25_institutions <- locations_institutions_counts |>
  slice(2:26) |> select(institution) |> unlist() |> as.vector()

top25_outside_nijmegen <- locations_institutions_counts |>
  filter(city != "Nijmegen") |>
  slice(1:25) |> select(institution) |> unlist() |> as.vector()

top25_outside_NL <- locations_institutions_counts |>
  filter(country != "Netherlands") |>
  slice(1:25) |> select(institution) |> unlist() |> as.vector()

top25_places <- locations_counts |>
  filter(city != "Nijmegen") |>
  slice(1:25) |> select(city) |> unlist() |> as.vector()

top25_places_outside_NL <- locations_counts |>
  filter(country != "Netherlands") |>
  slice(1:25) |> select(city) |> unlist() |> as.vector()


# Network -----------------------------------------------------------------

# all places outside Nijmegen are there because they are part of the CLS
# co-authorship network. So we want to draw lines from all those places to
# Nijmegen, using line width as aesthetic for frequency.


network <- locations_counts |>
  mutate(from = "Nijmegen", to = city) |>
  select(from,to,count,lat,lon) |>
  rename("lat.to" = "lat",
         "lon.to" = "lon") |>
  mutate(lat.n = 51.8,
         lon.n = 5.84) |>
  filter(to != "Nijmegen")

network_institutions <- locations_institutions_counts |>
  mutate(from = "Nijmegen", to = city) |>
  select(from,to,institution,count,lat,lon) |>
  rename("lat.to" = "lat",
         "lon.to" = "lon") |>
  mutate(lat.n = 51.8,
         lon.n = 5.84) |>
  filter(institution != "Radboud Univ Nijmegen")


# some desc stats
unique(network$to)
unique(locations$country)


# Maps --------------------------------------------------------------------

# Let's map. This is using theme_map() and a simple world outline from ggplot.

# for the labels, there are too many to plot all so we're using ggrepel. We want
# to privilege labels by frequency, so we plot in two layers: first the labels
# not in the top 25 outside NL (with geom_text_repel); next the places in the
# top 25 (with geom_label_repel). Using two calls makes it possible to have some
# choice in what labels get hidden because of overlaps.
network %>%
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + 
  theme(legend.position="none") +
  borders("world",colour=NA,fill="#cccccc") +
  coord_cartesian(ylim=c(-55,80),xlim=c(-150,170)) +
  geom_point(aes(alpha=log(count)),
             colour="#730e04") +
  geom_curve(aes(x=lon.to,xend=lon.n,
                 y=lat.to,yend=lat.n,
                 linewidth=count,
                 alpha=count),
             colour="#730e04") +
  geom_text_repel(data = . %>% filter(to %notin% top25_places_outside_NL),
                  aes(label=to),
                  segment.color="#cccccc",
                  min.segment.length = 20,
                  color="#000000",
                  alpha=0.50,
                  max.overlaps = 10) +
  geom_label_repel(data = . %>% filter(to %in% top25_places_outside_NL),
                  aes(label=to),
                  segment.color="#cccccc",
                  segment.alpha=0.5,
                  min.segment.length = 1,
                  label.size=NA,
                  fill= alpha(c("white"),0.5),
                  max.overlaps = 20) 
ggsave("figures/cls-network-world.png",width=12,height=6,bg="white")

# generate an empty canvas that can be used for animating a slide
network %>%
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + 
  theme(legend.position="none",
        text=element_text(family="Open Sans")) +
  borders("world",colour=NA,fill="#cccccc") +
  coord_cartesian(ylim=c(-55,80),xlim=c(-150,170))
ggsave("cls-network-world-canvas.png",width=12,height=6,bg="white")


# cropped to Europe


# some good coordinates for cropping
# northern europe: lat = c(45,58) lon = c(-10,15)
# bigger europe: lat = c(37,58) lon=c(-12,36)

crop_coordinates <- list(lat = c(37,60),  # lat = y
                         lon = c(-24,31)) # lon = x


network.europe <- network |>
  filter(lat.to > crop_coordinates$lat[1], lon.to > crop_coordinates$lon[1]) |>
  filter(lat.to < crop_coordinates$lat[2], lon.to < crop_coordinates$lon[2])


# some customizations here to fix labels — main reason is to show the top
# frequent but leave no isolated points unlabelled
these_labels <- c("Aberdeen", "Avignon", "Barcelona", "Belfast", "Bilbao", "Boras", "Cologne", "Corfu", "Dublin", "Graz", "Gothenburg", "Hamburg", "Leuven", "Madrid", "Milan", "Oxford", "Paris", "Rome", "Stockholm", "Uppsala", "Warsaw")

# the cropping using coord_cartesian() makes it possible
network.europe |>
  filter(count > 1, to != "Sittard") |>
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + theme(legend.position="none") +
  borders("world",colour="white",fill="#cccccc") +
  coord_cartesian(ylim=c(crop_coordinates$lat[1],crop_coordinates$lat[2]),
                  xlim=c(crop_coordinates$lon[1],crop_coordinates$lon[2])) +
  geom_point(colour="#730e04") +
  geom_curve(aes(x=lon.to,xend=lon.n,
                 y=lat.to,yend=lat.n,
                 linewidth=log(count),
                 alpha=count),
             colour="#730e04") +
  geom_text_repel(data=. %>% filter(to %in% these_labels | count > 4),
                  aes(label=to),
                  min.segment.length = 40,
                  max.overlaps = 10)
ggsave("figures/cls-network-europe.png",width=12,height=6.7555,bg="white")


crop_northernEU <- list(lat = c(47,57),  # lat = y
                        lon = c(-10,25)) # lon = x

network_institutions.europe <- network_institutions |>
  filter(lat.to > crop_northernEU$lat[1], lon.to > crop_northernEU$lon[1]) |>
  filter(lat.to < crop_northernEU$lat[2], lon.to < crop_northernEU$lon[2])


network_institutions.europe |>
  filter(institution %in% top25_outside_nijmegen,to !="Nijmegen") |>
  group_by(to) %>% slice(1) |>
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + theme(legend.position="none") +
  borders("world",colour="white",fill="#cccccc") +
  coord_cartesian(ylim=c(crop_northernEU$lat[1],crop_northernEU$lat[2]),
                  xlim=c(crop_northernEU$lon[1],crop_northernEU$lon[2])) +
  geom_point(colour="#730e04") +
  geom_curve(aes(x=lon.to,xend=lon.n,
                 y=lat.to,yend=lat.n,
                 alpha=count),
             linewidth=1.5,
             colour="#730e04") +
  geom_text_repel(aes(label=to),
                  min.segment.length = 20,
                  max.overlaps = Inf)
ggsave("figures/cls-network-institutions_europe.png",width=12,height=6.7555,bg="white")


# Experimental: network on globe ------------------------------------------

# make it a globe

# BTW adding labels doesn't work yet here, probably because geom_text_repel has
# difficulty dealing with labels that fall behind the globe (and these are hard
# to filter)
network %>%
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + theme(legend.position="none") +
  borders("world",colour="#ffffff",fill="#cccccc") +
  coord_map("ortho",
            orientation = c(40,0,-50),
            ylim = c(-90, 160),
            xlim = c(-120, 120)) +
  geom_point(color="#730e04") +
  geom_segment(aes(x=lon.to,xend=lon.n,
                   y=lat.to,yend=lat.n,
                   linewidth=count,
                   alpha=count),
               colour="#730e04") 
# geom_text_repel(aes(label=to),
#                 max.overlaps = 50)
ggsave("figures/cls-network-globe.png",width=12,height=12,bg="white")



# Institutions ------------------------------------------------------------

# or by institutions

network_institutions %>%
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + 
  theme(legend.position="none") +
  borders("world",colour=NA,fill="#cccccc") +
  coord_cartesian(ylim=c(-55,80),xlim=c(-150,170)) +
  geom_point(colour="#730e04") +
  geom_curve(aes(x=lon.to,xend=lon.n,
                 y=lat.to,yend=lat.n,
                 linewidth=count,
                 alpha=count),
             colour="#730e04") +
  geom_text_repel(aes(label=institution),
                  min.segment.length = 10)
ggsave("figures/cls-network-institutions-world.png",width=12,height=6,bg="white")

# or with only the top 25
# this needs work consolidating spelling variants etc.
# probably better to use WoS institution field
network_institutions %>%
  filter(institution %in% top25_outside_NL &
           to != "Nijmegen") |>
  ggplot(aes(lon.to,lat.to)) +
  theme_map() + 
  theme(legend.position="none") +
  borders("world",colour=NA,fill="#cccccc") +
  coord_cartesian(ylim=c(-55,80),xlim=c(-150,170)) +
  geom_point(colour="#730e04") +
  geom_curve(aes(x=lon.to,xend=lon.n,
                 y=lat.to,yend=lat.n,
                 linewidth=count,
                 alpha=count),
             colour="#730e04") +
  geom_text_repel(aes(label=institution),
                  max.overlaps = Inf)
#ggsave("cls-network-institutions-world.png",width=12,height=6,bg="white")



# institutions
institutions <- d$C3

institutions_separated <- unlist(strsplit(institutions, "; "))


