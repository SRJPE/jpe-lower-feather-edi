library(EDIutils)
library(tidyverse)
library(tidyverse)
library(readxl)
library(EML)
library(EMLaide)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/catch.csv",
                             "data/trap.csv",
                             "data/recaptures.csv",
                             "data/release.csv"),
                attribute_info = c("data-raw/metadata/lower_feather_catch_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_trap_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_recapture_metadata.xlsx",
                                   "data-raw/metadata/lower_feather_release_metadata.xlsx"),
                datatable_description = c("Daily catch",
                                          "Daily trap operations",
                                          "Recaptured fish",
                                          "Released fish"),
                datatable_url = paste0("https://raw.githubusercontent.com/FlowWest/jpe-lower-feather-edi/main/data/",
                                       c("catch.csv",
                                         "trap.csv",
                                         "recaptures.csv",
                                         "release.csv")))

excel_path <- "data-raw/metadata/lower_feather_metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.md" # original, bulleted methods are in the .docx file
#edi_number <- reserve_edi_id(user_id = Sys.getenv("EDI_USER_ID"), password = Sys.getenv("EDI_PASSWORD"))
# edi_number <- fill in with reserved edi number

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
edi_number
EML::write_eml(eml, paste0(edi_number, ".xml"))
EML::eml_validate(paste0(edi_number, ".xml"))
