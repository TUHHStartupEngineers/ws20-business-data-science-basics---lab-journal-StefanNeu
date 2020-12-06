library(tidyverse)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

grouped_covid_data_tbl <- covid_data_tbl %>%

              filter(year == 2020) %>%

              filter(countriesAndTerritories == "Germany" | countriesAndTerritories == "Spain" |
                       countriesAndTerritories == "United_Kingdom" | countriesAndTerritories == "France" |
                       countriesAndTerritories == "United_States_of_America") %>%

              select(dateRep, day, month, cases, countriesAndTerritories) %>%

              mutate(absDay = yday(dmy(dateRep))) %>%

              arrange(absDay) %>%

              group_by(countriesAndTerritories) %>%

              mutate(cum_cases = cumsum(cases))

ylabel = c(0, 5, 10, 15)
xlabel = c("February, April, June, August, October, December")

grouped_covid_data_tbl %>%

              ggplot(aes(x = lubridate::dmy(dateRep), y = cum_cases, color = countriesAndTerritories)) +

              geom_step(size=0.75) +

              scale_y_continuous(limits = c(0,16e6), labels=paste0(ylabel, "M")) +
              scale_x_date(date_breaks = "2 months", date_labels = "%b") +

  scale_color_viridis_d(option = "A") +

              geom_label(data = filter(grouped_covid_data_tbl, cum_cases ==last(cum_cases),
                                       countriesAndTerritories %in% "United_States_of_America"),
                         aes(label = cum_cases, fill = countriesAndTerritories), hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11], show.legend = FALSE) +

              labs(
                title = "COVID-19 confirmed cases worldwide",
                subtitle = "As of 06.12.2020, the increase in cases is very strong for the USA",
                x = "Year 2020",
                y = "Cummulative Cases",
                color = "Countries"
              )

world_map <- map_data("world")

covid_death_map <- covid_data_tbl%>%
  
    mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
    
    mutate(countriesAndTerritories = case_when(
      countriesAndTerritories == "United Kingdom" ~ "UK",
      countriesAndTerritories == "United States of America" ~ "USA",
      countriesAndTerritories == "Czechia" ~ "Czech Republic",
      TRUE ~ countriesAndTerritories
    )) %>%
  
    mutate(region = countriesAndTerritories) %>%
    
    select(region, deaths, popData2019) %>%
    
    group_by(region) %>%
    
    summarise(mortalityRate = sum(deaths)/popData2019) %>%
    
    distinct(region, .keep_all = TRUE) %>%
    
    mutate(mortalityRate_perc = scales::percent(mortalityRate, accuracy = 0.001))


covid_death_map <- covid_death_map %>%
  
  left_join(world_map, by = "region") %>%
  mutate(`Mortality Rate` = mortalityRate)


covid_death_map %>% ggplot() + 
  
  geom_map(aes(x = long, y = lat, map_id = region, fill = `Mortality Rate`, 
               label = mortalityRate_perc), map = world) +

  scale_fill_continuous(labels = scales::percent_format(accuracy = 0.01), low = "#fc0303", high = "black") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed Covid-19 deaths worldwide",
    x = "",
    y = "",
    caption = "Date: 06/12/2020"
  ) 
  
  

