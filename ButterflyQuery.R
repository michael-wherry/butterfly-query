library(tidyverse)
library(readxl)

rm(list = ls())

na_placeholders <- c("", "//s+","N/A", "n/a","N/a", "n/A", "NA", "UNKNOWN", "unknown")
df_scanned_wings <- read_xlsx("Data/Unclean/Cleaned Data LWA .xlsx", .name_repair = "universal", na = na_placeholders)
df_scanned_butterfly <- read_xlsx("Data/Unclean/CompletePierisData_2022-03-09.xlsx", .name_repair = "universal", na = na_placeholders)
rm(na_placeholders)

df_butterfly <- df_scanned_butterfly %>%
  rename_with(~ sub("(?:dwc.:?)?(.)", "\\L\\1", .x, perl = TRUE)) %>%
  rename(coreId = coreid) %>%
  rename(lengthLW = lWingLength) %>%
  rename(widthLW = lWingWidth) %>%
  rename(apexLW = lBlackPatchApex) %>%
  rename(anteriorSpotLW = lAnteriorSpotM3) %>%
  rename(posteriorSpotLW = lPosteriorSpotCu2) %>%
  rename(interspotLW = lInterspotPA) %>%
  rename(lengthRW = rWingLength) %>%
  rename(widthRW = rWingWidth) %>%
  rename(apexRW = rBlackPatchApex) %>%
  rename(anteriorSpotRW = rAnteriorSpotM3) %>%
  rename(posteriorSpotRW = rPosteriorSpotCu2) %>%
  rename(interspotRW = rInterspotPA)

df_wings <- df_scanned_wings %>%
  rename(coreId = core.ID) %>%
  rename(lengthLW = LW.length) %>%
  rename(widthLW = LW.width) %>%
  rename(apexLW = LW.apex.A) %>%
  rename(lengthRW = RW.length) %>%
  rename(widthRW = RW.width) %>%
  rename(apexRW = RW.apex.A) %>%
  drop_na() # Shouldn't have sparse entries

rm(df_scanned_butterfly)
rm(df_scanned_wings)

# Butterflies

df_butter_location <- df_butterfly %>%
  select(coreId, decimalLatitude, decimalLatitudeUpdated, decimalLongitude, decimalLongitudeUpdated) %>%
  mutate(latitude = coalesce(decimalLatitude, as.numeric(decimalLatitudeUpdated))) %>%
  mutate(longitude = coalesce(decimalLongitude, as.numeric(decimalLongitudeUpdated))) %>%
  select(coreId, latitude, longitude)

df_butter_date <- df_butterfly %>%
  select(coreId, year, yearUpdated, month, day, dayOfYearUpdated, startDayofYearUpdated, endDayofYearUpdated) %>%
  mutate(avgDayOfYear = floor(rowMeans(cbind(startDayofYearUpdated, endDayofYearUpdated)))) %>%
  mutate(dayOfYear = coalesce(dayOfYearUpdated, avgDayOfYear)) %>%
  mutate(mergedYear = coalesce(gsub("![0-9]", "", year), as.character(yearUpdated))) %>%
  mutate(monthFromDayOfYear = lubridate::month(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(dayFromDayOfYear = lubridate::day(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(mergedMonth = coalesce(monthFromDayOfYear, month)) %>%
  mutate(mergedDay = coalesce(as.character(dayFromDayOfYear), day)) %>%
  mutate(date =  as.Date(paste(mergedYear, mergedMonth, mergedDay, sep = "/")))

df_butter_date <- df_butter_date %>%
  select(coreId, date, mergedYear) %>%
  rename(year = mergedYear) # Still missing a small handful of dates

df_butter_stats <- df_butterfly %>%
  select(coreId, sexUpdated,
         lengthLW, widthLW, apexLW, posteriorSpotLW, anteriorSpotLW, interspotLW,
         lengthRW, widthRW, apexRW, posteriorSpotRW, anteriorSpotRW, interspotRW) %>%
  mutate(across(!c(coreId, sexUpdated, interspotLW, interspotRW), ~as.numeric(.x))) %>%
  left_join(df_wings) %>%
  mutate(sex = coalesce(sex, sexUpdated)) %>%
  mutate(sex = gsub("(?=f|F)[^\\?]*", "female", sex, perl = TRUE)) %>%
  mutate(sex = gsub("(?=m|M)[^\\?]*", "male", sex, perl = TRUE)) %>% 
  select(!sexUpdated)

df_clean_butter <- df_butter_date %>%
  left_join(df_butter_stats, by = "coreId") %>%
  left_join(df_butter_location, by = "coreId")

write_csv(df_clean_butter, "Data/Clean/Butterfly.csv")