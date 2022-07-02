#

library(tidyverse)


# Extracting the relevant info from a CSV dump of Kjøretøyregisteret

vehicles <-
  readr::read_csv2(
    #"H:/Programmering/R/vehicles_csv_dump/kjoretoyinfo.csv",
    "C:/Users/snohan/Desktop/kjoretoyinfo.csv",
    #n_max = 1500000,
    col_select = c(
      tekn_kjm_farge,
      tekn_reg_aar,
      tekn_reg_status,
      tekn_merkenavn,
      tekn_tknavn
    )
   ) %>%
  dplyr::filter(
    tekn_kjm_farge != "UTENFOR_OFFENTLIG_VEG",
    tekn_reg_status == "REGISTRERT",
    tekn_reg_aar > 20000000,
    tekn_tknavn %in% c(
      "M1",
      "M1G",
      "M2",
      "M2G",
      "M3",
      "M3G",
      "N1",
      "N1G",
      "N2",
      "N2G",
      "N3",
      "N3G",
      "T1",
      "T2",
      "T3",
      "T4",
      "T5",
      "TR"
    )
  )

saveRDS(
  vehicles,
  file = "C:/Users/snohan/Desktop/vehicles.rds"
)

vehicles <-
  readRDS(
    "C:/Users/snohan/Desktop/vehicles.rds"
  )

vehicle_brands_and_types <-
  vehicles |>
  dplyr::mutate(
    vehicle_type = dplyr::case_when(
      tekn_tknavn %in% c("M1", "M1G") ~ "Personbil",
      tekn_tknavn %in% c("M2", "M2G", "M3", "M3G") ~ "Buss",
      tekn_tknavn %in% c("N1", "N1G") ~ "Varebil",
      tekn_tknavn %in% c("N2", "N2G", "N3", "N3G") ~ "Lastebil",
      tekn_tknavn %in% c("T1", "T2", "T3", "T4", "T5", "TR") ~ "Traktor",
    ),
    vehicle_brand = dplyr::case_when(
      tekn_merkenavn == "TESLA MOTORS" ~ "TESLA",
      tekn_merkenavn == "BMW I" ~ "BMW",
      tekn_merkenavn == "CASE IH" ~ "CASE",
      tekn_merkenavn == "JAGUAR CARS" ~ "JAGUAR",
      tekn_merkenavn == "JAGUAR LAND ROVER LIMITED" ~ "JAGUAR",
      tekn_merkenavn == "FORD-CNG-TECHNIK" ~ "FORD",
      tekn_merkenavn == "KNAUS TABBERT" ~ "KNAUS",
      tekn_merkenavn == "MERCEDES-BENZ" ~ "MERCEDES",
      tekn_merkenavn == "MERCEDES-AMG" ~ "MERCEDES",
      tekn_merkenavn == "MERCEDES SPRINTER/BUS FACTORY" ~ "MERCEDES",
      tekn_merkenavn == "VALTRA VALMET" ~ "VALTRA",
      tekn_merkenavn == "A.I.TRUCKMASTER" ~ "TRUCKMASTER",
      tekn_merkenavn == "A.I. TRUCKMASTER" ~ "TRUCKMASTER",
      tekn_merkenavn == "A.I TRUCK MASTER" ~ "TRUCKMASTER",
      tekn_merkenavn == "AUTOMOBILI LAMBORGHINI S.P.A." ~ "LAMBORGHINI",
      tekn_merkenavn == "MASSEY-FERGUSON" ~ "MASSEY FERGUSON",
      TRUE ~ tekn_merkenavn
    )
  ) |>  dplyr::select(
    vehicle_brand,
    vehicle_type
  ) |>
  # Pure terrain vehicles:
  dplyr::filter(
    !(vehicle_brand %in% c(
      "ARCTIC CAT",
      "BRP",
      "CFMOTO",
      "DANGEL",
      "DRESEL MCT",
      "HSUN",
      "LINHAI",
      "QUADDY",
      "TGB"
      ))
  )

vehicle_brand_counts <-
  vehicle_brands_and_types |>
  dplyr::group_by(
    vehicle_brand
  ) |>
  dplyr::summarise(
    n = n(),
    .groups = "drop"
  )

saveRDS(
  vehicle_brand_counts,
  file = "C:/Users/snohan/Desktop/vehicle_brand_counts.rds"
)
