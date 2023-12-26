library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(googledrive)
library(randomcoloR)
library(gghighlight)

setwd("~/Desktop/workspace/git/covid_19")
options("scipen"=100, digits = 4)
texto45 = theme(axis.text.x=element_text(angle=45, hjust=1))
theme_set(theme_bw())

my_blob =  list(
  geom_point(),
  geom_line(alpha = 0),
  geom_smooth(se = FALSE),
  gghighlight(TRUE,  label_key = location, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1, size = 3))
)

my_caption =  list(theme(plot.caption=element_text(hjust = 0)),
                   labs(caption = "Sources: Johns Hopkins University and Ministério da Saúde do Brasil"))


# Ingestion Ministério da Saúde ------------------

file = "./data/ministerio_da_saude.xlsx"
confirmed = read_excel(file, sheet = "confirmed", col_names = TRUE) %>%
              gather("...1", valor, -"...2", -"...1") %>%
              rename(location = "...2", date = "...1", total= valor) %>%
              mutate(campo = "total_cases")
# str(confirmed)

deaths = read_excel(file, sheet = "deaths", col_names = TRUE) %>%
         gather("...1", valor, -"...2", -"...1") %>%
         rename(location = "...2", date = "...1", total= valor) %>%
         mutate(campo = "total_deaths")
# str(deaths)

brazil = rbind(confirmed, deaths) %>% spread(campo, total) %>%
         mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
         filter(!is.na(total_cases)) %>%
         mutate(source = "ministry")

brazil %>% filter(location == "SP") %>% tail
 

# Ingestion Johns Hopkins --------

world_deaths = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
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

world_cases = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
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


world = rbind(world_cases, world_deaths) %>% 
        mutate(source = "university") %>%
        spread(campo, total) %>% 
        rbind(brazil %>% arrange(date))

world %>% filter(location == "Brazil") %>% arrange(date) %>% tail
world %>% filter(location == "SP") %>% arrange(date) %>% tail

sort(unique(world$date))
sort(unique(world$location))

log_scale = scale_y_continuous(trans='log10', labels = scales::comma)

countries = "Brazil|SP|RJ|Portugal|Italy|Spain|Iran|China|France|United Kingdom|Argentina|Colombia|Chile|US|Mexico|India|Turkey|Russia"
#countries = "Switzerland|Sweden|Luxembourg|Finland|Denmark|Germany"
#countries = "Brazil|SP|RJ|United Kingdom|Australia|US|Canada|Belgium|Switzerland|Netherlands"


# Confirmed ------------------

a = world %>% filter(source == "university") %>% 
       group_by(date) %>%
       summarise(total_cases = sum(total_cases)) %>%
       ggplot(aes(date, total_cases)) +
       geom_point() +
       geom_smooth(se = FALSE) + 
       labs(title = "Confirmed cases (global)", y = "Confirmed") +
       my_caption

a

b = world %>% filter(grepl(countries, location)) %>%
       filter(total_cases > 100) %>%
       ggplot(aes(date, total_cases, color = location)) +
       my_blob +
       log_scale +
       labs(title = "Confirmed cases (by country/states)", y = "Confirmed (logarithmic scale)")  +
       my_caption

b


c = world %>% filter(grepl(countries, location)) %>%
       filter(total_cases > 200) %>%
       group_by(location) %>% 
       mutate(dia = row_number()) %>%
       filter(dia < 100) %>%
       ggplot(aes(dia, total_cases, color = location)) +
       my_blob +
       log_scale +
       #coord_cartesian(xlim = c(0,30)) +
       labs(title = "Confirmed cases (D = 0 when cases > 100)", x = "D (Day)", y = "Confirmed (logarithmic scale)") +
       my_caption

c


# Deaths ------------------

d = world %>%
       filter(source == "university") %>% 
       group_by(date) %>%
       summarise(total_deaths = sum(total_deaths)) %>%
       ggplot(aes(date, total_deaths)) +
       geom_point() +
       geom_smooth(se = FALSE) + 
       labs(title = "Death cases (global)", y = "Deaths") +
       my_caption

d
          
e = world %>% filter(date > as.Date("2020-02-15")) %>%
       filter(total_deaths > 5) %>%
       filter(grepl(countries, location)) %>% 
       ggplot(aes(date, total_deaths, color = location)) +
       my_blob +
       log_scale +
       labs(title = "Death cases (by country/state)", y = "Deaths (logarithmic scale)") +
       my_caption

e


f = world %>% #filter(grepl(countries, location)) %>%
       filter(total_deaths > 10000) %>%
       group_by(location) %>%
       mutate(dia = row_number()) %>%
       filter(dia < 70) %>%
       ggplot(aes(dia, total_deaths, color = location)) +
       my_blob +
       log_scale +
       #coord_cartesian(xlim=c(0,20)) +
       labs(title = "Death cases (D = 0 when cases > 10000)", x = "D (Day)", y = "Deaths (logarithmic scale)") +
       my_caption

f

l = world %>% filter(source == "ministry") %>% 
       filter(grepl("SP|MG|RJ|RS|PR|BA|RJ|AM|CE|PE|BR|MA", location)) %>%
       #arrange(location) %>% View
       group_by(location) %>% 
       mutate(deaths = total_deaths - lag(total_deaths)) %>% 
       ggplot(aes(date, deaths, color = location)) +
       my_blob +
       log_scale +
       labs(title = "New death cases (Brazil)", y = "Deaths (logarithmic scale)") +
       my_caption

l

# Mortality Rate ------------------

g = world %>% filter(source == "university") %>%  
       group_by(date) %>%
       summarise(total_deaths = sum(total_deaths, na.rm = TRUE), total_cases = sum(total_cases, na.rm = TRUE)) %>%
       mutate(mortality = (total_deaths / total_cases)*100) %>% 
       filter(!is.na(mortality)) %>%
       ggplot(aes(date, mortality)) +
       geom_point() +
       geom_smooth(se = FALSE) +
       labs(title = "Mortality rate (global)", y = "Mortality rate (%)") +
       my_caption

g

i = world %>% filter(date > as.Date("2020-03-10")) %>%
       filter(grepl(countries, location)) %>%
       mutate(mortality = (total_deaths / total_cases) *100) %>% 
       filter(!is.na(mortality)) %>%
       ggplot(aes(date, mortality, color = location)) +
       my_blob +
       labs(title = "Mortality rate (by country/state)", y = "Mortality rate (%)") +
       my_caption

i


j = world %>% filter(date > as.Date("2020-03-10")) %>%
       filter(grepl("SP|MG|RJ|RS|PR|BA|RJ|AM|CE|PE|MA", location)) %>%
       mutate(mortality = (total_deaths / total_cases) *100) %>% 
       filter(!is.na(mortality)) %>%
       ggplot(aes(date, mortality, color = location)) +
       my_blob +
       labs(title = "Mortality rate (Brazil)", y = "Mortality rate (%)") +
       my_caption

j
     

# Cities --------------------

cities = read.csv("./data/cases-brazil-cities-time.csv") 

sort(unique(cities$city))

o = cities %>%
       filter(city %in% c("São Paulo/SP", "Rio de Janeiro/RJ")) %>%
       mutate(date = as.Date(date)) %>% 
       group_by(city) %>%
       arrange(date) %>% 
       mutate(deaths = deaths - lag(deaths)) %>% 
       select(city, date, deaths) %>%
       ggplot(aes(date, deaths), fill = city) +
       geom_point() +
       geom_smooth() +
       facet_wrap(city ~ ., ncol = 2)

o

p = cities %>%
       filter(state %in% c("SP")) %>%
       filter(grepl("Osasco|Campinas|São Bernardo|Diadema|Sorocaba|Jundiaí|Guarulhos|Santo André|São Caetano|Santos", city)) %>%
       #filter(grepl("Sorocaba|Bauru|Limeira|Campinas|Piracicaba|São Carlos|Rio Claro|Jaú", city)) %>%
       mutate(date = as.Date(date)) %>% 
       group_by(city) %>%
       arrange(date) %>% 
       mutate(deaths = deaths - lag(deaths)) %>% 
       select(city, date, deaths) %>%
       ggplot(aes(date, deaths), fill = city) +
       geom_point() +
       geom_smooth() +
       facet_wrap(city ~ ., nrow = 2)
  
p

w_ = 15
h_ = 10

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
ggsave(o, filename = "./img/cities_capital.png", width = w_, height = h_)
ggsave(p, filename = "./img/cities.png", width = w_, height = h_)
