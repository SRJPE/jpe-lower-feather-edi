library(EDIutils)
library(tidyverse)
library(tidyverse)
library(readxl)
library(EML)
library(EMLaide)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/lower_feather_catch.csv",
                             "data/lower_feather_trap.csv",
                             "data/lower_feather_recapture.csv",
                             "data/lower_feather_release.csv",
                             "data/lower_feather_release_fish.csv"),
                attribute_info = c("data-raw/metadata/lower_feather_catch_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_trap_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_recapture_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_release_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_releasefish_metadata.xlsx"),
                datatable_description = c("Daily catch",
                                          "Daily trap operations",
                                          "Recaptured fish",
                                          "Released fish",
                                          "Released fish"), #TODO check this description
                datatable_url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-lower-feather-edi/edi-updates-9-25/data/", #TODO updates this path when merged
                                       c("lower_feather_catch.csv",
                                         "lower_feather_trap.csv",
                                         "lower_feather_recapture.csv",
                                         "lower_feather_release.csv",
                                         "lower_feather_release_fish.csv")))

excel_path <- "data-raw/metadata/lower_feather_metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.md" # original, bulleted methods are in the .docx file

#edi_number <- reserve_edi_id(user_id = Sys.getenv("edi_user_id"), password = Sys.getenv("edi_password"))
# edi_number <- "edi.1500.1" # reserved 9-20-2023 under srjpe account
edi_number <- "edi.1500.3" # update ?

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
  add_datatable(datatable_metadata)

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
eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
)

EML::write_eml(eml, paste0(edi_number, ".xml"))
EML::eml_validate(paste0(edi_number, ".xml"))

EMLaide::evaluate_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), paste0(edi_number, ".xml"))
View(report_df)
# EMLaide::upload_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), paste0(edi_number, ".xml"))


