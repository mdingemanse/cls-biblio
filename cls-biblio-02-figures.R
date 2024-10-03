# cls-biblio analyses and figures
# MD 2024



# Packages and useful functions -------------------------------------------

list.of.packages <- c("tidyverse",
                      #"bibliometrix",
                      #"openalexR",
                      "tmaptools",
                      "roadoi",
                      "ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

source('helper-functions.R')


# Load data ---------------------------------------------------------------

d.u <- read.csv("data/cls-unpaywall.csv")

d.opendata <- read_csv('data/cls-opendata.csv') |>
  mutate(prop = has_repo/papers_with_dois * 100)

d.repos <- read_csv('data/cls-repos.csv')

d.a <- read.csv( 'data/cls-altmetric.csv')

d.dimensions <- readxl::read_excel('data/cls-Dimensions-Publication-2024-03-04_11-17-50.xlsx')




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
           )) |>
  mutate(oa_status_simpler_d =
           case_when(
             oa_status_d == "closed" ~ "closed",
             oa_status_d == "diamond" ~ "diamond",
             oa_status_d %in% c("gold","hybrid") ~ "open",
             oa_status_d %in% c("bronze","green") ~ "green",
             .default = as.character(oa_status_d)
           ))
  


d.u |>
  drop_na(year) |>
  filter()

d.p |>
  ggplot(aes(x=year,fill=oa_status_simpler)) +
  theme_economist() + theme(plot.background = element_blank()) +
  theme(plot.title.position = "plot") +
  ggtitle("A. Open access status") +
  labs(x="year",y="", fill="") +
  scale_fill_manual(values = c(c_ladybug,"green3","green4")) +
  geom_bar(position="fill") 
p1 <- last_plot()


d.opendata |>
  ggplot(aes(x=year,y=prop)) +
  theme_economist() + theme(plot.background = element_blank()) +
  theme(plot.title.position = "plot") +
  ggtitle("B. Open science trends") +
  ylim(0,20) +
  labs(x="year",y="% of papers with open data or code") +
  geom_line()
p2 <- last_plot()


d.repos |>
  filter(year != 2024) |>
  ggplot(aes(x=year)) +
  theme_economist() + theme(plot.background = element_blank()) +
  ggtitle("C. Repository deposits") + 
  xlim(c(2018,2024)) +
  labs(x="year",y="number of deposits") + 
  geom_bar()
p3 <- last_plot()


cowplot::plot_grid(p1,p2,p3,nrow=1)
ggsave('figures/panel_openscience.png',width=12,height=4,bg="white")

# OA by type in more detail, for appendix
d.p |>
  ggplot(aes(x=year,fill=oa_status_simpler_d)) +
  theme_economist() + theme(plot.background = element_blank()) +
  theme(plot.title.position = "plot") +
  ggtitle("Open access status by publication type") +
  labs(x="year",y="", fill="") +
  scale_fill_manual(values = c(c_ladybug,"green1","green3","green4")) +
  geom_bar() +
  facet_wrap(~ type)
ggsave('figures/panel_oa_by_type.png',width=12,height=4,bg="white")



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


# *Dimensions data --------------------------------------------------------

# We use the subset of CLS articles identified through Dimensions for a citation
# analysis. Note that Dimensions uses CrossRef-supplied citations, likely
# undercounting citations relatively to for instance Google Scholar.

# We find that after one to two years, at least 90 of CLS articles in Dimensions
# is already cited at least once. This climbs up to 99% after five years,
# meaning that most CLS research *included in Dimensions* ends up having impact
# in terms of citations.

d.d <- d.dimensions |>
  mutate(cited = ifelse(`Times cited` > 0,1,0)) 

d.d |>
  group_by(PubYear) |>
  dplyr::summarise(n=n(),
                   prop_cited=sum(cited)/n,
                   citations=sum(`Times cited`),
                   max_cited=max.na(`Times cited`),
                   median_cited=median.na(`Times cited`))


# *Altmetric data --------------------------------------------------------

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

d.a.cites |>
  mutate(period = ifelse(year < 2022, "first", "second")) |>
  group_by(period) |>
  dplyr::summarise(r=cor.test(mendeley,citations)$estimate)
