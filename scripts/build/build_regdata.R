setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# County-quarter panel ---------------------------------------------------------

#### Prep

# Labor data
df_lfp <- read_dta(paste0(path_data_int, "/Labor/lau_county_quarterly.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr, quarter = rfrnc_qtroy) %>%
  dplyr::select(cntyfp, year, quarter, lau_lfp)

df_qwi_agegrp <- read_dta(paste0(path_data_int, "/Labor/qwi_agegrp_county_quarterly.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr, quarter = rfrnc_qtroy) %>%
  dplyr::select(cntyfp, year, quarter, agegrp, qwi_emptotal, qwi_payroll) %>%
  pivot_wider(names_from = agegrp, 
              values_from = c(qwi_emptotal, qwi_payroll),
              names_sep = "_",
              names_glue = "{.value}_{agegrp}")

df_qwi <- read_dta(paste0(path_data_int, "/Labor/qwi_county_quarterly.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr, quarter = rfrnc_qtroy) %>%
  dplyr::select(cntyfp, year, quarter, qwi_emptotal, qwi_payroll)

df_qwi_naics2 <- read_dta(paste0(path_data_int, "/Labor/qwi_naics2_county_quarterly.dta"))%>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr, quarter = rfrnc_qtroy) %>%
  dplyr::select(cntyfp, year, quarter, industry, qwi_emptotal, qwi_payroll) %>%
  pivot_wider(names_from = industry, 
              values_from = c(qwi_emptotal, qwi_payroll),
              names_sep = "_",
              names_glue = "{.value}_{industry}")
id_cols <- c("cntyfp", "year", "quarter")
other_cols <- sort(setdiff(names(df_qwi_naics2), id_cols))
new_col_order <- c(id_cols, other_cols)
df_qwi_naics2 <- df_qwi_naics2 %>% select(all_of(new_col_order))

df_qwi_naic3 <- read_dta(paste0(path_data_int, "/Labor/qwi_naics3_ag_county_quarterly.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr, quarter = rfrnc_qtroy) %>%
  dplyr::select(cntyfp, year, quarter, industry, qwi_emptotal) %>%
  pivot_wider(names_from = industry, 
              values_from = c(qwi_emptotal),
              names_sep = "_",
              names_glue = "{.value}_{industry}")
other_cols <- sort(setdiff(names(df_qwi_naic3), id_cols))
new_col_order <- c(id_cols, other_cols)
df_qwi_naic3 <- df_qwi_naic3 %>% select(all_of(new_col_order))

# NWS dust storm data
df_nws_ds <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_ds_and_dswarnings_combined_quarterly.csv")) %>%
  mutate(cntyfp = paste0(stfp, cntyfp)) %>%
  dplyr::select(-c(stfp, stabv))

# ASOS dust storm data


# Weather data


# Drought data


# Pollution data


# Population data
df_seer <- read_dta(paste0(path_data_int, "/Demographics/seer_county_year.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr) %>%
  dplyr::select(cntyfp, year, everything())

df_seer_agegrp <- read_dta(paste0(path_data_int, "/Demographics/seer_agegrp_county_year.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr) %>%
  dplyr::select(cntyfp, year, everything()) %>%
  pivot_wider(names_from = agegrp, 
              values_from = c(seer_pop),
              names_sep = "_",
              names_glue = "{.value}_{agegrp}")

# Demographic data

#### Merge the county-quarter data

#### Merge in the county-year data

#### Output

# County-year panel ------------------------------------------------------------

#### Prep

# Dust storm data

# Population data

# Weather data

# Drought data

# Migration data
df_migration <- read_dta(paste0(path_data_int, "/Demographics/irs_inflow_outflow_county_year.dta")) %>%
  filter(rfrnc_yr >= 2006 & rfrnc_yr <= 2019) %>%
  mutate(cntyfp = sprintf("%05d", countyfip)) %>%
  dplyr::rename(year = rfrnc_yr) %>%
  dplyr::select(cntyfp, year, everything())

#### Merge

#### Output

# County-day panel -------------------------------------------------------------

#### Prep

# NWS dust storm data
df_nws_ds_s <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_ds_and_dswarnings_combined_stormlevel.csv"))

# ASOS dust storm data

# Weather data

# Pollution data

# Population data

# Drought data?

#### Merge

#### Output

