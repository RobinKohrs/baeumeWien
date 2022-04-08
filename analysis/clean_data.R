library(tidyverse)
library(glue)
library(here)
library(sf)
library(jsonlite)

# path --------------------------------------------------------------------
path = here("data_raw/baeume.json")


# read json ---------------------------------------------------------------
raw = read_sf(path)

# clean -------------------------------------------------------------------
raw %>%
  rename_with(~ tolower(.x),
              .cols = everything()) %>%
  select(
    -c(
      id,
      objectid,
      datenfuehrung,
      gebietsgruppe,
      pflanzjahr_txt,
      stammumfang_txt,
      se_anno_cad_data
    )
  ) -> df_clean


# write it out ------------------------------------------------------------
out_dir_geo = here("output/geodata/")
if(!dir.exists(out_dir_geo)) dir.create(out_dir_geo, recursive = T)
out_file = here(out_dir_geo, "cleaned_data.geojson")
write_sf(df_clean, out_file)




