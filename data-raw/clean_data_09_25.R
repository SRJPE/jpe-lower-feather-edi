library(tidyverse)
library(readxl)
# Note that we will no longer use this clean data script.
# Ashley met with Claire to update queries to remove the appropriate tables
# All additional processing will happen when we pull in data to database.

# TODO - check if above notes are still accurate
# catch  #TODO checked with Claire on why visitTime2 was not added, waiting for answer
catch_raw <- read_xlsx(here::here("data-raw", "LFR RST Catch Raw EDI Query.v.11.15.24.xlsx")) |>
  select(-actualCount) |> # should we delete this row, as in original clean_data?
  glimpse()

write_csv(catch_raw, here::here("data", "lower_feather_catch.csv"))

# trap - #TODO checked with Claire on why visitTime2 was not added, waiting for answer
trap_raw <- read_xlsx(here::here("data-raw", "LFR RST Trap Visit EDI Query.v.11.15.24.xlsx")) |>
  mutate(waterTempUnit = if_else(waterTempUnit == "°C", "Celsius", "Fahrenheit")) |> # is this code not necessary?
  glimpse()
write_csv(trap_raw, here::here("data", "lower_feather_trap.csv"))

# recaptures
recaptures_raw <- read_xlsx(here::here("data-raw", "LFR RST Recapture EDI Query.v.11.15.24.xlsx")) |> # note that there are 11 na values for finalRun
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2))) |>
  glimpse()

write_csv(recaptures_raw, here::here("data", "lower_feather_recapture.csv"))

# releases
release_raw <- read_xlsx(here::here("data-raw", "LFR RST Release EDI Query.v.11.15.24.xlsx")) |>
  glimpse()
write_csv(release_raw, here::here("data", "lower_feather_release.csv"))

# releasefish
release_fish_raw <- read_xlsx(here::here("data-raw", "LFR RST ReleaseFish EDI Query.v.11.15.24.xlsx")) |> # note that all forklength values are empty
  glimpse()
write_csv(release_fish_raw, here::here("data", "lower_feather_release_fish.csv"))

# read in clean data to double check --------------------------------------
catch <- read_csv(here::here("data", "lower_feather_catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "lower_feather_trap.csv")) |> glimpse()
recapture <- read_csv(here::here("data", "lower_feather_recapture.csv")) |> glimpse()
release <- read_csv(here::here("data", "lower_feather_release.csv")) |> glimpse()
release_fish <- read_csv(here::here("data", "lower_feather_release_fish.csv")) |> glimpse()
