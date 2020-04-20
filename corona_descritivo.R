library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(googledrive)
library(randomcoloR)
library(gghighlight)


setwd("~/Desktop/covid_19")
options("scipen"=100, digits = 4)
texto45 = theme(axis.text.x=element_text(angle=45, hjust=1))
theme_set(theme_minimal())

my_blob =  list(
  geom_point(),
  geom_line(alpha = 0),
  geom_smooth(se = FALSE),
  gghighlight(TRUE,  label_key = location, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

my_caption =  list(theme(plot.caption=element_text(hjust = 0)),
                   labs(caption = "Sources: Johns Hopkins University and Brazilian Ministry of Wealthy"))


# Ingestion Ministry of Wealthy ------------------

minha_planilha = "1L1CnyeKA8ZJprzFCa3ZiRIzcP44mahmcG4M_hnlbMFQ"
arquivo = "./data/ministry_of_wealthy.xlsx"

drive_download(as_id(minha_planilha), path = arquivo, overwrite = TRUE)
confirmados = read_excel(arquivo, sheet = "confirmed", col_names = TRUE) %>% 
              gather(X__1, valor, -X__2, -X__1) %>%
              rename(location = X__2, date = X__1, total= valor) %>%
              mutate(campo = "total_cases")
str(confirmados)

obitos = read_excel(arquivo, sheet = "deaths", col_names = TRUE) %>%
              gather(X__1, valor, -X__2, -X__1) %>%
              rename(location = X__2, date = X__1, total = valor) %>%
              mutate(campo = "total_deaths")
str(obitos)

brasil = rbind(confirmados, obitos) %>% spread(campo, total) %>%
         mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
         filter(!is.na(total_cases)) %>%
         mutate(source = "ministry")
      
unique((brasil$date))

brasil %>% filter(location == "SP") %>% tail



# Ingestion Johns Hopkins --------

mundo_obitos = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
                        check.names=FALSE, stringsAsFactors = FALSE) %>%
               rename(location = `Country/Region`) %>%
               mutate(Lat = NULL, 
                      Long = NULL,
                      `Province/State` = NULL) %>%
               gather(date, total, -location) %>%
               group_by(location, date) %>%
               summarise(total = sum(total)) %>%
               mutate(date = as.Date(date, "%m/%d/%y"),
                      campo = "total_deaths") %>%
               ungroup

mundo_casos = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                       check.names=FALSE) %>%
              rename(location = `Country/Region`) %>%
              mutate(Lat = NULL, 
                     Long = NULL,
                     `Province/State` = NULL) %>%
              gather(date, total, -location) %>%
              group_by(location, date) %>%
              summarise(total = sum(total)) %>%
              mutate(date = as.Date(date, "%m/%d/%y"),
                     campo = "total_cases") %>%
              ungroup

mundo = rbind(mundo_casos, mundo_obitos) %>% 
        mutate(source = "university") %>%
        spread(campo, total) %>% rbind(brasil)

mundo %>% filter(location == "Brazil") %>% arrange(date) %>% tail
mundo %>% filter(location == "BR") %>% arrange(date) %>% tail
mundo %>% filter(location == "US") %>% arrange(date) %>% tail

sort(unique(mundo$date))
sort(unique(mundo$location))

log_scale = scale_y_continuous(trans='log10', labels = scales::comma)

countries = "Brazil|SP|RJ|United States|Portugal|Italy|Spain|Iran|China|France|United Kingdom|Argentina|Colombia|Chile|Australia|US|Mexico|India|Egypt|Japan|Korea|Turkey"

# Confirmed ------------------

a = mundo %>% filter(source == "university") %>% 
          group_by(date) %>%
          summarise(total_cases = sum(total_cases)) %>%
          ggplot(aes(date, total_cases)) +
          geom_point() +
          geom_smooth(se = FALSE) + 
          labs(title = "Confirmed cases (total)", y = "Confirmed") +
          my_caption

a

b = mundo %>% filter(grepl(countries, location)) %>%
          filter(total_cases > 100) %>%
          ggplot(aes(date, total_cases, color = location)) +
          my_blob +
          log_scale +
          labs(title = "Confirmed cases (by country/states)", y = "Confirmed (logarithmic scale)")  +
          my_caption

b


c = mundo %>% filter(grepl(countries, location)) %>%
          filter(total_cases > 200) %>%
          group_by(location) %>% 
          mutate(dia = row_number()) %>%
          filter(dia < 40) %>%
          ggplot(aes(dia, total_cases, color = location)) +
          my_blob +
          log_scale +
          #coord_cartesian(xlim = c(0,30)) +
          labs(title = "Confirmed cases (D = 0 when cases > 100)", x = "D (Day)", y = "Confirmed (logarithmic scale)") +
          my_caption

c


# Deaths ------------------

d = mundo %>%
        filter(source == "university") %>% 
        group_by(date) %>%
        summarise(total_deaths = sum(total_deaths)) %>%
        ggplot(aes(date, total_deaths)) +
        geom_point() +
        geom_smooth(se = FALSE) + 
        labs(title = "Death cases (total)", y = "Deaths") +
        my_caption

d

e = mundo %>% filter(date > as.Date("2020-02-15")) %>%
          filter(total_deaths > 5) %>%
          filter(grepl(countries, location)) %>%
          ggplot(aes(date, total_deaths, color = location)) +
          my_blob +
          log_scale +
          labs(title = "Death cases (by country/state)", y = "Deaths (logarithmic scale)") +
          my_caption

e

f = mundo %>% filter(grepl(countries, location)) %>%
          filter(total_deaths > 10) %>%
          group_by(location) %>% 
          mutate(dia = row_number()) %>%
          filter(dia < 40) %>%
          ggplot(aes(dia, total_deaths, color = location)) +
          my_blob +
          log_scale +
          #coord_cartesian(xlim=c(0,20)) +
          labs(title = "Death cases (D = 0 when cases > 10)", y = "Deaths (logarithmic scale)") +
          my_caption

f

k = mundo %>% filter(grepl(countries, location)) %>%
          group_by(location) %>%
          mutate(deaths = total_deaths - lag(total_deaths)) %>% 
          ggplot(aes(date, deaths, color = location)) +
          my_blob +
          log_scale +
          labs(title = "Death cases (new)", y = "Deaths (logarithmic scale)") +
          my_caption

k

l = mundo %>% filter(source == "ministry") %>%
          filter(grepl("SP|MG|RJ|RS|PR|BA|RJ|AM|CE|PE", location)) %>%
          mutate(deaths = total_deaths - lag(total_deaths)) %>% 
          ggplot(aes(date, deaths, color = location)) +
          my_blob +
          log_scale +
          labs(title = "Death cases (new)", y = "Deaths (logarithmic scale)") +
          my_caption

l

# Mortality Rate ------------------

g = mundo %>% filter(source == "university") %>%  
          group_by(date) %>%
          summarise(total_deaths = sum(total_deaths, na.rm = TRUE), total_cases = sum(total_cases, na.rm = TRUE)) %>%
          mutate(mortality = (total_deaths / total_cases)*100) %>% 
          filter(!is.na(mortality)) %>%
          ggplot(aes(date, mortality)) +
          geom_point() +
          geom_smooth(se = FALSE) +
          labs(title = "Mortality rate (total)", y = "Mortality rate (%)") +
          my_caption

g

i = mundo %>% filter(date > as.Date("2020-03-10")) %>%
          filter(grepl(countries, location)) %>%
          mutate(mortality = (total_deaths / total_cases) *100) %>% 
          filter(!is.na(mortality)) %>%
          ggplot(aes(date, mortality, color = location)) +
          my_blob +
          labs(title = "Mortality rate (by country/state)", y = "Mortality rate (%)") +
          my_caption

i


j = mundo %>% filter(date > as.Date("2020-03-10")) %>%
  filter(grepl("SP|MG|RJ|RS|PR|BA|RJ|AM|CE|PE", location)) %>%
  mutate(mortality = (total_deaths / total_cases) *100) %>% 
  filter(!is.na(mortality)) %>%
  ggplot(aes(date, mortality, color = location)) +
  my_blob +
  labs(title = "Mortality rate (Brazil)", y = "Mortality rate (%)") +
  my_caption

j
     
w_ = 15
h_ = 7

ggsave(a, filename = "./img/confirmed_total.png", width = w_, height = h_)
ggsave(b, filename = "./img/confirmed_detail.png", width = w_, height = h_)
ggsave(c, filename = "./img/confirmed_compare.png", width = w_, height = h_)
ggsave(d, filename = "./img/deaths_total.png", width = w_, height = h_)
ggsave(e, filename = "./img/deaths_detail.png", width = w_, height = h_)
ggsave(f, filename = "./img/deaths_compare.png", width = w_, height = h_)
ggsave(k, filename = "./img/deaths_new.png", width = w_, height = h_)
ggsave(l, filename = "./img/deaths_new_brazil.png", width = w_, height = h_)
ggsave(g, filename = "./img/mortality_total.png", width = w_, height = h_)
ggsave(i, filename = "./img/mortality_detail.png", width = w_, height = h_)
ggsave(j, filename = "./img/mortality_brazil.png", width = w_, height = h_)


# Cities --------------------

cities = read.csv("./data/cases-brazil-cities-time.csv")

cities %>% filter(state == "SP") %>%
           filter(grepl("São Carlos|Rio Claro|Jundiaí|São Pedro|Campinas", city)) %>%
           group_by(city) %>%
           arrange(date, .by_group = TRUE) %>%
           summarise(date = last(date),
                     deaths = last(deaths),
                     totalCases = last(totalCases)) %>%
           arrange(-totalCases) %>% View
