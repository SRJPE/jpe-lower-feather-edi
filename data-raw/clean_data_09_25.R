library(tidyverse)
library(readxl)
library(weathermetrics)

# catch  #
catch_raw <- read_xlsx(here::here("data-raw", "lower_feather_catch.xlsx")) |>
  # select(-actualCount) |>
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2)),
         actualCount = case_when(actualCount != "Yes" ~ "No",
                                 TRUE ~ actualCount)) |>
  glimpse()

write_csv(catch_raw, here::here("data", "lower_feather_catch.csv"))

# trap
trap_raw <- read_xlsx(here::here("data-raw", "lower_feather_trap.xlsx")) |>
  arrange(subSiteName, visitTime) |>
  mutate(counterAtStart = ifelse(counterAtStart > 6150, NA, counterAtStart), # setting outlier of 6150 in counterAtStart to NA
         trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2)),
         waterTemp = ifelse(waterTempUnit == "°F", fahrenheit.to.celsius(waterTemp), waterTemp)) |> # doing the conversion "manually"
  select(-waterTempUnit) |> # compared to tisdale, this query did not have "lightPenetration
  glimpse()

write_csv(trap_raw, here::here("data", "lower_feather_trap.csv"))

# recaptures
recaptures_raw <- read_xlsx(here::here("data-raw", "lower_feather_recapture.xlsx")) |> # note that there are 11 na values for finalRun
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2))) |> # no lifestage, forklength
  glimpse()

write_csv(recaptures_raw, here::here("data", "lower_feather_recapture.csv"))

# releases
release_raw <- read_xlsx(here::here("data-raw", "lower_feather_release.xlsx")) |> #no markLifeStage, sourceOfFishSite, appliedMarkCode
  glimpse()
write_csv(release_raw, here::here("data", "lower_feather_release.csv"))

# releasefish
release_fish_raw <- read_xlsx(here::here("data-raw", "lower_feather_releasefish.xlsx")) |> # note that all forklength values are empty
  glimpse()
write_csv(release_fish_raw, here::here("data", "lower_feather_release_fish.csv"))

# read in clean data to double check --------------------------------------
catch <- read_csv(here::here("data", "lower_feather_catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "lower_feather_trap.csv")) |> glimpse()
recapture <- read_csv(here::here("data", "lower_feather_recapture.csv")) |> glimpse()
release <- read_csv(here::here("data", "lower_feather_release.csv")) |> glimpse()
release_fish <- read_csv(here::here("data", "lower_feather_release_fish.csv")) |> glimpse()
