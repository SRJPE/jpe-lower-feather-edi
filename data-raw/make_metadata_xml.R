library(EMLaide)
library(dplyr)
library(readxl)
library(EML)
library(readr)

secret_edi_username = Sys.getenv("EDI_USERNAME")
secret_edi_password = Sys.getenv("EDI_PASSWORD")

datatable_metadata <- dplyr::tibble(
  filepath = character(),
  attribute_info = character(),
  datatable_description = character(),
  datatable_url = character()
)

file_list = c("current_year_lower_feather_catch.csv",
              "current_year_lower_feather_recapture.csv",
              "current_year_lower_feather_release.csv",
              "current_year_lower_feather_releasefish.csv",
              "current_year_lower_feather_trap.csv")
metadata_info = c("data-raw/metadata/lower_feather_catch_metadata.xlsx",
                  "data-raw/metadata/lower_feather_recapture_metadata.xlsx",
                  "data-raw/metadata/lower_feather_release_metadata.xlsx",
                  "data-raw/metadata/lower_feather_releasefish_metadata.xlsx",
                  "data-raw/metadata/lower_feather_trap_metadata.xlsx")
description = c("Daily catch",
                "Recaptured catch",
                "Release trial summary",
                "Release fish summary",
                "Daily trap operations")
url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-lower-feather-edi/main/data/",
             c("current_year_lower_feather_catch.csv",
               "current_year_lower_feather_recapture.csv",
               "current_year_lower_feather_release.csv",
               "current_year_lower_feather_releasefish.csv",
               "current_year_lower_feather_trap.csv"))
# datatable_metadata <-
#   dplyr::tibble(filepath = c("data/lower_feather_catch.csv",
#                              "data/lower_feather_trap.csv",
#                              "data/lower_feather_recapture.csv",
#                              "data/lower_feather_release.csv",
#                              "data/lower_feather_release_fish.csv"),
#                 attribute_info = c("data-raw/metadata/lower_feather_catch_metadata.xlsx",
#                                    "data-raw/metadata/lower_feather_trap_metadata.xlsx",
#                                    "data-raw/metadata/lower_feather_recapture_metadata.xlsx",
#                                    "data-raw/metadata/lower_feather_release_metadata.xlsx",
#                                    "data-raw/metadata/lower_feather_releasefish_metadata.xlsx"),
#                 datatable_description = c("Daily catch",
#                                           "Daily trap operations",
#                                           "Recaptured fish",
#                                           "Released fish",
#                                           "Released fish"), #TODO check this description
#                 datatable_url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-lower-feather-edi/main/data/",
#                                        c("lower_feather_catch.csv",
#                                          "lower_feather_trap.csv",
#                                          "lower_feather_recapture.csv",
#                                          "lower_feather_release.csv",
#                                          "lower_feather_release_fish.csv")))
for (i in seq_along(file_list)){
  x <- read_csv(paste0("data/",file_list[i]))
  if (nrow(x) > 0){
    filepath <- paste0("data/", file_list[i])
    attribute_info <- metadata_info[i]
    datatable_description <- description[i]
    datatable_url <- url[i]

    datatable_metadata <- bind_rows(
      datatable_metadata,
      dplyr::tibble(filepath = filepath,
                    attribute_info = attribute_info,
                    datatable_description = datatable_description,
                    datatable_url = datatable_url)
    )
  }
}
zipped_entity_metadata <- c("file_name" = "lower_feather.zip",
                            "file_description" = "Zipped folder",
                            "file_type" = "zip",
                            "physical" = list(create_physical(file_path = "data/lower_feather.zip",
                                                              data_url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-lower-feather-edi/main/data/",
                                                                                "lower_feather.zip")))
)
excel_path <- "data-raw/metadata/lower_feather_metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.md" # original, bulleted methods are in the .docx file
catch_df <- readr::read_csv(unz("data/lower_feather.zip", "lower_feather_catch.csv"))
catch_coverage <- tail(catch_df$visitTime, 1)
metadata$coverage$end_date <- lubridate::floor_date(catch_coverage, unit = "days")
wb <- openxlsx::createWorkbook()
for (sheet_name in names(metadata)) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeData(wb, sheet = sheet_name, x = metadata[[sheet_name]], rowNames = FALSE)
}
openxlsx::saveWorkbook(wb, file = excel_path, overwrite=TRUE)
vl <- readr::read_csv("data-raw/version_log.csv", col_types = c('c', "D"))
previous_edi_number <- tail(vl['edi_version'], n=1)
previous_edi_number <- previous_edi_number$edi_version
previous_edi_ver <- as.numeric(stringr::str_extract(previous_edi_number, "[^.]*$"))
current_edi_ver <- as.character(previous_edi_ver + 1)
previous_edi_id_list <- stringr::str_split(previous_edi_number, "\\.")
previous_edi_id <- sapply(previous_edi_id_list, '[[', 2)
current_edi_number <- paste0("edi.", previous_edi_id, ".", current_edi_ver)
new_row <- data.frame(
  edi_version = current_edi_number,
  date = as.character(Sys.Date())
)
vl <- bind_rows(vl, new_row)
write.csv(vl, "data-raw/version_log.csv", row.names=FALSE)

dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata) |>
  add_other_entity(zipped_entity_metadata)

# GO through and check on all units
custom_units <- data.frame(id = c("number of rotations", "NTU", "revolutions per minute", "number of fish", "days",
                                  "see waterTempUnit", "microSiemenPerCentimeter"),
                           unitType = c("dimensionless", "dimensionless", "dimensionless", "dimensionless",
                                        "dimensionless", "dimensionless","dimensionless"),
                           parentSI = c(NA, NA, NA, NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA, NA, NA, NA),
                           description = c("number of rotations",
                                           "nephelometric turbidity units, common unit for measuring turbidity",
                                           "number of revolutions per minute",
                                           "number of fish counted",
                                           "number of days",
                                           "see designated column for units of water temperature collected",
                                           "units for measuring conductivity"))


unitList <- EML::set_unitList(custom_units)
eml <- list(packageId = current_edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
)

EML::write_eml(eml, paste0(current_edi_number, ".xml"))
message("EML Metadata generated")
print(paste0("Current EDI Number:", current_edi_number))
EMLaide::update_edi_package(user_id = secret_edi_username,
                            password = secret_edi_password,
                            eml_file_path = paste0(getwd(), "/", current_edi_number, ".xml"),
                            existing_package_identifier = paste0("edi.",previous_edi_id, ".", previous_edi_ver, ".xml"),
                            environment = "production")
