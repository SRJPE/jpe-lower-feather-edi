library(knitr)
library(lubridate)
library(readr)
library(zip)
library(dplyr)
library(stringr)

clean_zip_data <- function(trap_path, new_path) {
  # Temporary directories for extraction and writing
  folder_path <- "data/lower_feather.zip"
  temp_dir <- tempdir()
  temp_dir <- normalizePath(temp_dir, winslash = "/")
  original_wd <- getwd()

  # Unzip new_path
  unzip(folder_path, exdir = temp_dir)
  trap_file <- file.path(temp_dir, basename(trap_path))

  trap_data <- readr::read_csv(trap_file, col_type = list(
    projectDescriptionID = col_double(),
    trapVisitID = col_double(),
    visitTime = col_datetime(format = ""),
    visitTime2 = col_datetime(format = ""),
    siteName = col_character(),
    subSiteName = col_character(),
    visitType = col_character(),
    fishProcessed = col_character(),
    trapFunctioning = col_character(),
    counterAtStart = col_double(),
    counterAtEnd = col_double(),
    rpmRevolutionsAtStart = col_double(),
    rpmRevolutionsAtEnd = col_double(),
    includeCatch = col_character(),
    discharge = col_double(),
    waterVel = col_double(),
    waterTemp = col_double(),
    turbidity = col_double(),
    dissolvedOxygen = col_double(),
    conductivity = col_double()
  )) |>
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2)))
  if (c("waterTempUnit") %in% names(trap_data)) {
    trap_data <- trap_data |>
      mutate(
        waterTemp = ifelse(waterTempUnit == "°F", (waterTemp - 32) * 5/9, waterTemp)
      ) |>
      select(-any_of("waterTempUnit"))
  }
  new_file <- file.path(temp_dir, basename(new_path))
  cleaned_data <- if (grepl("lower_feather_catch.csv", new_path)) {
    readr::read_csv(new_file, col_type = list(
      ProjectDescriptionID = col_double(),
      catchRawID = col_double(),
      trapVisitID = col_double(),
      commonName = col_character(),
      releaseID = col_double(),
      atCaptureRun = col_character(),
      fishOrigin = col_character(),
      lifeStage = col_character(),
      forkLength = col_double(),
      totalLength = col_double(),
      n = col_double(),
      visitTime = col_datetime(format = ""),
      visitTime2 = col_datetime(format = ""),
      visitType = col_character(),
      siteName = col_character(),
      subSiteName = col_character(),
      atCaptureRun = col_character(),
      finalRun = col_character(),
      actualCount=col_character()
    )) |>
      mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                                 T ~ visitTime)),
             trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                               T ~ visitTime2)),
             actualCount = case_when(actualCount != "Yes" ~ "No",
                                     TRUE ~ actualCount)) |>
      arrange(subSiteName, visitTime) |>
      left_join(trap_data |>
                  select(trapVisitID, trap_start_date, trap_end_date))
  }else if(grepl("lower_feather_recapture.csv", new_path)) {
        readr::read_csv(new_file, col_type = list(
            ProjectDescriptionID = col_double(),
            catchRawID = col_double(),
            trapVisitID = col_double(),
            commonName = col_character(),
            releaseID = col_double(),
            atCaptureRun = col_character(),
            finalRun = col_character(),
            fishOrigin = col_character(),
            n = col_double(),
            visitTime = col_datetime(format = ""),
            visitTime2 = col_datetime(format = ""),
            visitType = col_character(),
            siteName = col_character(),
            subSiteName = col_character(),
            markType = col_character(),
            markColor = col_character(),
            markPosition = col_character()
          )) |>
            mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                                     T ~ visitTime)),
                 trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                                   T ~ visitTime2))) |>
            left_join(trap_data |>
                        select(trapVisitID, visitTime, visitTime2, trap_start_date, trap_end_date) |>
                        distinct()) |>
            # mutate(run = ifelse(run %in% c("Not applicable (n/a)", "Not recorded"), NA, run)) |>
            select(ProjectDescriptionID, catchRawID, trapVisitID, commonName, releaseID, atCaptureRun, finalRun, fishOrigin, n,
                   visitTime, visitTime2, visitType, siteName, subSiteName, markType, markColor, markPosition, trap_start_date,
                   trap_end_date)
  }else if(grepl("lower_feather_release.csv", new_path)) {
        readr::read_csv(new_file, , col_types = list(
          projectDescriptionID = col_double(),
          releaseID = col_double(),
          releaseTime = col_datetime(format = ""),
          commonName = col_character(),
          markedRun = col_character(),
          markedFishOrigin = col_character(),
          releaseSite = col_character(),
          releaseSubSite = col_character(),
          nReleased = col_double(),
          testDays = col_double(),
          appliedMarkType = col_character(),
          appliedMarkColor = col_character(),
          appliedMarkPosition = col_character()
          )) |>
          mutate(releaseSubSite = ifelse(releaseSubSite == "N/A", NA, releaseSubSite),
                 appliedMarkColor = ifelse(appliedMarkColor == "Not applicable (n/a)", NA, appliedMarkColor),
                 appliedMarkPosition = str_replace(appliedMarkPosition, ",", ":"))
  }else if(grepl("lower_feather_releasefish.csv", new_path)){
        readr::read_csv(new_file, col_types = list(
            projectDescriptionID = col_double(),
            releaseFishID = col_double(),
            releaseID = col_double(),
            forkLength = col_double()
          )) |>
            mutate(releaseFishID = as.character(releaseFishID),
                   releaseID = as.character(releaseID))
  }
  # Write updated data back to the temporary directory
  write_csv(trap_data, trap_file)
  write_csv(cleaned_data, new_file)
  setwd(temp_dir)
  files_to_zip <- list.files(pattern = "^lower_feather", recursive = TRUE)

  zip(
    zipfile = file.path(original_wd, folder_path),
    files =  files_to_zip
  )
  setwd(original_wd)
}

clean_current_year_data <- function(trap_path, new_path) {
  trap_data <- readr::read_csv(trap_path, col_type = list(
    projectDescriptionID = col_double(),
    trapVisitID = col_double(),
    visitTime = col_datetime(format = ""),
    visitTime2 = col_datetime(format = ""),
    siteName = col_character(),
    subSiteName = col_character(),
    visitType = col_character(),
    fishProcessed = col_character(),
    trapFunctioning = col_character(),
    counterAtStart = col_double(),
    counterAtEnd = col_double(),
    rpmRevolutionsAtStart = col_double(),
    rpmRevolutionsAtEnd = col_double(),
    includeCatch = col_character(),
    discharge = col_double(),
    waterVel = col_double(),
    waterTemp = col_double(),
    turbidity = col_double(),
    dissolvedOxygen = col_double(),
    conductivity = col_double()
  )) |>
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2)))
  if (c("waterTempUnit") %in% names(trap_data)) {
    trap_data <- trap_data |>
      mutate(
        waterTemp = ifelse(waterTempUnit == "°F", (waterTemp - 32) * 5/9, waterTemp)
      ) |>
      select(-any_of("waterTempUnit"))
  }
  cleaned_data <- if (grepl("10yr_lower_feather_catch.csv", new_path)) {
    readr::read_csv(new_path, col_type = list(
      ProjectDescriptionID = col_double(),
      catchRawID = col_double(),
      trapVisitID = col_double(),
      commonName = col_character(),
      releaseID = col_double(),
      atCaptureRun = col_character(),
      fishOrigin = col_character(),
      lifeStage = col_character(),
      forkLength = col_double(),
      totalLength = col_double(),
      n = col_double(),
      visitTime = col_datetime(format = ""),
      visitTime2 = col_datetime(format = ""),
      visitType = col_character(),
      siteName = col_character(),
      subSiteName = col_character(),
      atCaptureRun = col_character(),
      finalRun = col_character(),
      actualCount=col_character()
    )) |>
      mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                                 T ~ visitTime)),
             trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                               T ~ visitTime2)),
             actualCount = case_when(actualCount != "Yes" ~ "No",
                                     TRUE ~ actualCount)) |>
      arrange(subSiteName, visitTime) |>
      left_join(trap_data |>
                  select(trapVisitID, trap_start_date, trap_end_date))
  }else if(grepl("10yr_lower_feather_recapture.csv", new_path)) {
    readr::read_csv(new_path, col_type = list(
      ProjectDescriptionID = col_double(),
      catchRawID = col_double(),
      trapVisitID = col_double(),
      commonName = col_character(),
      releaseID = col_double(),
      atCaptureRun = col_character(),
      finalRun = col_character(),
      fishOrigin = col_character(),
      n = col_double(),
      visitTime = col_datetime(format = ""),
      visitTime2 = col_datetime(format = ""),
      visitType = col_character(),
      siteName = col_character(),
      subSiteName = col_character(),
      markType = col_character(),
      markColor = col_character(),
      markPosition = col_character()
    )) |>
      mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                                 T ~ visitTime)),
             trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                               T ~ visitTime2))) |>
      left_join(trap_data |>
                  select(trapVisitID, visitTime, visitTime2, trap_start_date, trap_end_date) |>
                  distinct()) |>
      select(ProjectDescriptionID, catchRawID, trapVisitID, commonName, releaseID, atCaptureRun, finalRun, fishOrigin, n,
             visitTime, visitTime2, visitType, siteName, subSiteName, markType, markColor, markPosition, trap_start_date,
             trap_end_date)
  }else if(grepl("10yr_lower_feather_release.csv", new_path)) {
    readr::read_csv(new_path, , col_types = list(
      projectDescriptionID = col_double(),
      releaseID = col_double(),
      releaseTime = col_datetime(format = ""),
      commonName = col_character(),
      markedRun = col_character(),
      markedFishOrigin = col_character(),
      releaseSite = col_character(),
      releaseSubSite = col_character(),
      nReleased = col_double(),
      testDays = col_double(),
      appliedMarkType = col_character(),
      appliedMarkColor = col_character(),
      appliedMarkPosition = col_character()
    )) |>
      mutate(releaseSubSite = ifelse(releaseSubSite == "N/A", NA, releaseSubSite),
             appliedMarkColor = ifelse(appliedMarkColor == "Not applicable (n/a)", NA, appliedMarkColor),
             appliedMarkPosition = str_replace(appliedMarkPosition, ",", ":"))
  }else if(grepl("lower_feather_releasefish.csv", new_path)){
    readr::read_csv(new_path, col_types = list(
      projectDescriptionID = col_double(),
      releaseFishID = col_double(),
      releaseID = col_double(),
      forkLength = col_double()
    )) |>
      mutate(releaseFishID = as.character(releaseFishID),
             releaseID = as.character(releaseID))
  }
  write_csv(trap_data, trap_path)
  write_csv(cleaned_data, new_path)
}

path <- sort(c("lower_feather_catch.csv",
               "lower_feather_release.csv",
               "lower_feather_recapture.csv",
               "lower_feather_releasefish.csv"
              ))

full_trap_path <- paste0("data/lower_feather.zip/", "lower_feather_trap.csv")
full_new_data_path <- paste0("data/lower_feather.zip/", path)
mapply(clean_zip_data, full_trap_path, full_new_data_path)
current_year_path <- sort(c("data/10yr_lower_feather_catch.csv",
                            "data/10yr_lower_feather_release.csv",
                            "data/10yr_lower_feather_recapture.csv",
                            "data/lower_feather_releasefish.csv"))
# current_year_path <- current_year_path[file.exists(current_year_path)]
current_year_trap_path <- "data/10yr_lower_feather_trap.csv"
mapply(clean_current_year_data, current_year_trap_path, current_year_path)


