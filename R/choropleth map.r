##### Set up #####
library(odbc)
library(readxl)
library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("ggthemes")
library(ggthemes)
library(scales)
library(lubridate)
library(RColorBrewer)
library(knitr)
library(markdown)
library(rmarkdown)
library(zoo)
install.packages("MMWRweek")
library(MMWRweek)
library(splitstackshape)
install.packages("eeptools")
library(eeptools)
install.packages("choroplethr")
library(choroplethr)
install.packages("choroplethrZip")
library(choroplethrZip)
library(mapproj)
install.packages("geojsonio")
library(geojsonio)
library(broom)
install.packages("plotly")
library(plotly)


COVID19_DB <- dbConnect(odbc::odbc(),
                        DRIVER     = "SQL Server",
                        Server    = "",
                        Database  = "COVID19_DB",
                        Trusted_Connection = "True",
                        UID       = "",  
                        PWD       = "",
                        Port      = 1433)


zip_data <- dbGetQuery(COVID19_DB,
                       'SELECT [PERSON_SEARCH_ID] AS vedss_id,
                              CAST([Inv_start_dt] AS DATE) AS inv_start_date,
                              CAST([Event_DT] AS DATE) AS event_date,
                              CAST([Notification_submit_dt] AS DATE) as vedss_notification,
                             [PATIENT_CITY] AS inv_city,
                             [PATIENT_ZIP] AS inv_zip,
                             [PATIENT_COUNTY] AS inv_county,
                             [N_ADDRESS] AS geocode_address,
                             [N_CITY] AS geocode_city,
                             [N_STATE] AS geocode_state,
                             [N_ZIP] AS geocode_zip,
                             [N_COUNTY] AS geocode_county,
                             [N_CNTFIPS] AS geocode_fips,
                             [N_ZIP9] AS geocode_zip9
                             FROM [COVID19_DB].[dbo].[VW_COVID_CASE_DATAMART_VEDSS]
                       WHERE CASE_COUNT > 0')


levels(va_zips$county)

va_zips <- search_state("VA") %>%
  select(county, zipcode, major_city) %>%
  mutate(
    across(c(county, zipcode), ~factor(.x)))

rapp_zips <- va_zips %>%
  filter(county == "Caroline County" | county == "Fredericksburg City" |
           county == "Fredericksburg city" | county == "King George County" | county == "Spotsylvania County" | county == "Stafford County")

all_dates <- as_tibble_col(seq.Date(as.Date("2020-03-01"), as.Date(today()-1),
                                    "day"), column_name = "dates")

dates_and_zips <-full_join(all_dates, va_zips, by = character())

dates_and_zips_rapp <- dates_and_zips %>%
  filter(county  %in% rapp_zips$county)

zip_data_rapp <- zip_data %>%
  clean_names() %>%
  mutate(across(everything(),
                ~na_if(.x, "")),
         inv_zip = str_sub(inv_zip, 1, 5),
         geocode_zip5 = str_sub(geocode_zip9, 1, 5),
         across(c(ends_with("zip"), ends_with("city"),starts_with("geocode"),
                  ends_with("county")),
                ~factor(.x)),
         across(c(contains("date"), vedss_notification), ~lubridate::ymd(.x)),
         counting_date = as.Date(coalesce(inv_start_date,
                                          vedss_notification,
                                          event_date)),
         zip_best = coalesce(geocode_zip5, geocode_zip, inv_zip))  %>%
  filter(zip_best %in% rapp_zips$zipcode) %>%
  add_count(zip_best, counting_date, name = "zip_daily_cases") %>%
  select(zip_best, counting_date, zip_daily_cases) %>%
  right_join(., dates_and_zips_rapp, by = c("counting_date" = "dates",
                                            "zip_best" = "zipcode")) %>%
  distinct(zip_best, counting_date, .keep_all = TRUE) %>%
  filter(!is.na(zip_best)) %>%
  arrange(zip_best, counting_date) %>%
  group_by(zip_best) %>%
  mutate(zip_daily_cases = replace_na(zip_daily_cases, 0),
         zip_total_cases = sum(zip_daily_cases),
         zip_cumulative_cases = order_by(counting_date,
                                         cumsum(zip_daily_cases))) %>%
  ungroup()


rapp_cases_total <- zip_data_rapp %>%
  select(-counting_date) %>%
  distinct(zip_best, zip_total_cases, .keep_all = T)


spdf <- geojson_read("C:/Users/oeb95317/Documents/va_virginia_zip_codes_geo.min.json",  what = "sp")
spdf_fortify <- tidy(spdf, region = "ZCTA5CE10")

spdf_rapp <- spdf_fortify %>%
  filter(id %in% rapp_zips$zipcode)

rapp_full_data <- spdf_rapp %>%
  left_join(., zip_data_rapp, by = c("id" = "zip_best"))

zip_labels_rapp <- aggregate(cbind(long, lat) ~ id, data=spdf_rapp, FUN=mean)


ggplot(rapp_full_data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = zip_total_cases)) +
  geom_text(data = zip_labels_rapp, aes(long, lat, label = id), size = 2) +
  labs(title = "Rappahannock Health District",
       subtitle = "COVID-19 Cases by ZIP Code",
       fill="Total Cases in Zip") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  coord_map()



ggplot(lf_cases_total_spdf, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = lf_cases_total)) +
  geom_text(data = zip_labels_lf, aes(long, lat, label = id), size = 2) +
  labs(title = "Lord Fairfax Health District",
       subtitle = "COVID-19 Cases by ZIP Code",
       fill="Total Cases in Zip") +
  scale_fill_distiller(palette = "YlOrRd", direction = -1) +
  theme_void() +
  coord_map()


ggplot(pw_cases_total_spdf, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = zip_total_cases)) +
  geom_text(data = zip_labels_pw, aes(long, lat, label = id), size = 2) +
  labs(title = "Prince Wiliam Health District",
       subtitle = "COVID-19 Cases by ZIP Code",
       fill="Total Cases in Zip") +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme_void() +
  coord_map()


pw_map


ggplotly(pw_map, tooltip = c("id", "zip_total_cases"))