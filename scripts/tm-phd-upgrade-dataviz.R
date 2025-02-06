# Date: 2025-02-06
# Author: T.Murphy 
# Description 
# This script produces simple graphs from flow cytometry data exported from
# FlowJo analysis software. The graphs form part of the figures for TM's
# Phd upgrade report/presentation. 

path <- file.path("home", "tmurphy", "phd_work",
                  "25-02-06-graphs-for-phd-upgrade")
setwd(path)

# load necessary packages
install.packages("pacman")
library(pacman)
p_load("readxl", "tidyverse")

# Data exported as multi-sheet .xlsx file. Read in each sheet and store as
# seperate objects.
xlsxpath <- "data/numerical_data_for_upgrade_report.xlsx"

sheet_names <- excel_sheets(xlsxpath)

excel_list <- lapply(sheet_names, function(sheet){
  
  read_excel(xlsxpath, sheet = sheet)
})

# give elements of the list the original spreadsheet names
names(excel_list) <- sheet_names

# save each as a csv file in the output folder (for other purposes)
lapply(names(excel_list), function(sheet_name) {
  
  csv_file <- paste0("output/", sheet_name, ".csv")
  
  write.csv(excel_list[[sheet_name]], file = csv_file, row.names = FALSE)
})

# store each of the elements of a list as seperate objects
for (sheet in sheet_names) {
  assign(sheet, read_excel(xlsxpath, sheet = sheet))
}

# tidy up the names of the dataframes
df1_bd <- `23-04-24-brain-dissociation`
df2_viab <- `23-05-09-BD_with_viability_dye`
df3_cd11b <- `23-05-18-cd11b-macs`
df4_acsa2 <- `23-08-11-ACSA2-MACS-test`
df5_mcfc <- `23-08-17-MACSvsFACS_data`
df6_mm24 <- `23-08-17-MACSvsFACS_data`
df7_fans <- `pilot-nuclei-experiments`

# remove the old dfs
rm(list = sheet_names)

# Processing of all datasets

# dataset 1: df1_bd
df1_bd <- df1_bd %>%
  rename(total_events = Count) %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_pct_total = `Cells | Freq. of Parent`) %>% 
  mutate(cells_pct_total = parse_number(cells_pct_total))

# Calculate estimations of cell numbers given that
# 100uL of 11ml was used for pre-debris recording, and 100uL
# of 1ml was used for post-debris recording.
# also calculate post-debris removal enrichment factor
df1_bd <- df1_bd %>% 
  mutate(est_cells_total = case_when(
    str_detect(sample, "pre-debris") ~ cells_count * 110,
    str_detect(sample, "post-debris") ~ cells_count * 10
  )) %>% 
  # make two new columns for sample letter and pre- or post satus
  mutate(
    sample_group = str_extract(sample, "^[A-Z]"),
    sample_type = if_else(str_detect(sample, "pre-debris"),
                          "pre-debris",
                          "post-debris")
  ) %>% 
  # compute enrichment by sample group
  group_by(sample_group) %>% 
  mutate(
    pre_pct = cells_pct_total[sample_type == "pre-debris"],
    enrichment = if_else(sample_type == "post-debris", cells_pct_total / pre_pct, NA_real_)
  ) %>% 
  ungroup() %>% 
  select(-pre_pct)

# dataset 2: df2_viab

df2_viab <- df2_viab %>% 
  rename(total_events = Count) %>% 
  rename(cells_pct_total = `Cells | Freq. of Parent`) %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_pct_viable_cells = `Cells/Viable cells | Freq. of Parent`) %>% 
  rename(cells_pct_viable_sample = `Cells/Viable cells | Freq. of Grandparent`) %>% 
  rename(cells_count_viable = `Cells/Viable cells | Count`) %>% 
  mutate(est_total_viable_cells = cells_count_viable * 10)
  
# dataset 3: df3_cd11b
df3_cd11b <- df3_cd11b %>% 
  rename(total_events = Count) %>% 
  rename(cells_pct_total = `Cells | Freq. of Parent`) %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_pct_viable_cells = `Cells/Viable cells | Freq. of Parent`) %>% 
  rename(cells_pct_viable_sample = `Cells/Viable cells | Freq. of Grandparent`) %>% 
  rename(cells_count_viable = `Cells/Viable cells | Count`) %>% 
  mutate(
    condition = case_when(
      str_detect(sample, "-") ~ "negative_fraction",
      str_detect(sample, "[0-9]+x") ~ "positive_fraction",
      str_detect(sample, "^[A-Z]") ~ "post_debris"
    )
  )

# dataset 4: df4_acsa2
df4_acsa2 <- df4_acsa2 %>% 
  rename(total_events = Count) %>% 
  rename(cells_pct_total = `Cells | Freq. of Parent`) %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_pct_viable_cells = `Cells/Viable cells | Freq. of Parent`) %>% 
  rename(cells_pct_viable_sample = `Cells/Viable cells | Freq. of Grandparent`) %>% 
  rename(cells_count_viable = `Cells/Viable cells | Count`) %>% 
  mutate(
    condition = case_when(
      str_detect(sample, "-") ~ "negative_fraction",
      str_detect(sample, "[A-Z]\\+") ~ "positive_fraction",
      str_detect(sample, "^[A-Z]+\\s") ~ "post_debris"
    )
  )

# dataset 5: df5: 

