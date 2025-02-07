# Date: 2025-02-06
# Author: T.Murphy 
# Description 
# This script produces simple graphs from flow cytometry data exported from
# FlowJo analysis software. The graphs form part of the figures for TM's
# Phd upgrade report/presentation. 

path <- file.path("home", "tmurphy", "phd_work",
                  "25-02-06-graphs-for-phd-upgrade")


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
df6_mm24 <- `23-09-21-multimacs`
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
# columns names are awful, rename the lot.
df5_mcfc <- df5_mcfc %>%
  rename(cells_pct = `Cells | Freq. of Parent`) %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_vaible_count = `Cells/Viable cells | Count`) %>% 
  rename(cells_viable_pct = `Cells/Viable cells | Freq. of Cells`) %>% 
  rename(oligodendrocyte_count = `Cells/Viable cells/ O4+,CD11B- | Count`) %>% 
  rename(oligodendrocyte_pct = `Cells/Viable cells/ O4+,CD11B- | Freq. of Viable cells`) %>% 
  rename(microglia_count = `Cells/Viable cells/CD11B+ | Count`) %>% 
  rename(microglia_pct = `Cells/Viable cells/CD11B+ | Freq. of Viable cells`) %>% 
  rename(astrocyte_count = `Cells/Viable cells/CD11B-, O4-/ACSA2+ | Count`) %>% 
  rename(astrocyte_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2+ | Freq. of Viable cells`) %>% 
  rename(neurons_hi_count = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons* | Count`) %>% 
  rename(neurons_hi_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons* | Freq. of Viable cells`) %>% 
  rename(neurons_lo_count = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons** | Count`) %>% 
  rename(neurons_lo_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons** | Freq. of Viable cells`)

# tidy up the names of the FACS samples to broadly match the other sample names
df5_mcfc <- df5_mcfc %>%
  mutate(
    sample = case_match(
      sample,
      "2023-08-17_TM gentleMACS brain sort_Sort 1508 A.fcs" ~ "FACS_sort_1508_A.fcs",
      "2023-08-17_TM gentleMACS brain sort_Sort 1508 B.fcs" ~ "FACS_sort_1508_B.fcs",
      "2023-08-17_TM gentleMACS brain sort_Sort 1608 B.fcs" ~ "FACS_sort_1608_B.fcs",
      "2023-08-17_TM gentleMACS brain sort_Sort 1608 C.fcs" ~ "FACS_sort_1608_C.fcs",
      "2023-08-17_TM gentleMACS brain sort_Sort 1608 D.fcs" ~ "FACS_sort_1608_D.fcs",
      .default = sample
    )
  )

# dataset 6: df6_mm24
# very similar to df5 except one less column.
df6_mm24 <- df6_mm24 %>% 
  rename(cells_count = `Cells | Count`) %>% 
  rename(cells_vaible_count = `Cells/Viable cells | Count`) %>% 
  rename(cells_viable_pct = `Cells/Viable cells | Freq. of Cells`) %>% 
  rename(oligodendrocyte_count = `Cells/Viable cells/ O4+,CD11B- | Count`) %>% 
  rename(oligodendrocyte_pct = `Cells/Viable cells/ O4+,CD11B- | Freq. of Viable cells`) %>% 
  rename(microglia_count = `Cells/Viable cells/CD11B+ | Count`) %>% 
  rename(microglia_pct = `Cells/Viable cells/CD11B+ | Freq. of Viable cells`) %>% 
  rename(astrocyte_count = `Cells/Viable cells/CD11B-, O4-/ACSA2+ | Count`) %>% 
  rename(astrocyte_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2+ | Freq. of Viable cells`) %>% 
  rename(neurons_hi_count = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons* | Count`) %>% 
  rename(neurons_hi_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons* | Freq. of Viable cells`) %>% 
  rename(neurons_lo_count = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons** | Count`) %>% 
  rename(neurons_lo_pct = `Cells/Viable cells/CD11B-, O4-/ACSA2-/neurons** | Freq. of Viable cells`)
  

# dataset 7:
# sort out column names
df7_fans <- df7_fans %>% 
  rename(nuclei_pct_total = `scatter nuclei | Freq. of Parent`) %>% 
  rename(single_DAPI_pos_pct_total = `scatter nuclei/single DAPI+ nuclei | Freq. of Total`) %>% 
  rename(microglial_nuclei_pct_dapi = `scatter nuclei/single DAPI+ nuclei/NeuN-, PU.1+/triple -ve PU.1+ | Freq. of single DAPI+ nuclei`) %>% 
  rename(microglial_nuclei_mfi = `scatter nuclei/single DAPI+ nuclei/NeuN-, PU.1+/triple -ve PU.1+ | Median (PU1 PE (YG)-A)`) %>% 
  rename(astrocyte_nuclei_pct_dapi = `scatter nuclei/single DAPI+ nuclei/PU.1-/NeuN-, SOX2+/triple -ve SOX2+ | Freq. of single DAPI+ nuclei`) %>% 
  rename(astrocyte_nuclei_mfi = `scatter nuclei/single DAPI+ nuclei/PU.1-/NeuN-, SOX2+/triple -ve SOX2+ | Median (SOX2 PE-Cy7 (YG)-A)`) %>% 
  rename(neuron_nuclei_pct_dapi = `scatter nuclei/single DAPI+ nuclei/PU.1-/SOX2-/triple -ve NeuN+ | Freq. of single DAPI+ nuclei`) %>% 
  rename(neuron_nuclei_mfi = `scatter nuclei/single DAPI+ nuclei/PU.1-/SOX2-/triple -ve NeuN+ | Median (NeuN AF488*-A)`) %>% 
  rename(oligo_nuclei_pct_dapi = `scatter nuclei/single DAPI+ nuclei/PU.1-/SOX2-/triple -ve SOX10+ | Freq. of single DAPI+ nuclei`) %>% 
  rename(oligo_nuclei_mfi = `scatter nuclei/single DAPI+ nuclei/PU.1-/SOX2-/triple -ve SOX10+ | Median (SOX10 AF594*-A)`)

# add column to categorise samples by "pilot 1", "pilot 2", or the 3rd RML brain
df7_fans <- df7_fans %>% 
  mutate(
    experiment = case_when(
      str_detect(sample, "383") ~ "pilot_1",
      str_detect(sample, "pilot") ~ "pilot_2",
      str_detect(sample, "rml") ~ "single_rml"
    )
  )

# all of the dataframes contain percentage symbols after the values and the 
# values are incorectly codes. 

rm_percent_sign <- function(df) {
  df %>% 
    mutate(across(everything(), ~ {
      # leave numeric columns alone
      if (is.numeric(.)) {
        .
      } else {
        # this is overkill but ensures we operate on character values
        col_as_char <- as.character(.)
        if (any(grepl("%", col_as_char, fixed = TRUE), na.rm = TRUE)) {
          # convert to number and remove %
          parse_number(col_as_char)
        } else {
          .
        }
        
      }
    }))
}

# We already removed percentages from the first df so do this on 2-6
df2_viab  <- rm_percent_sign(df2_viab)
df3_cd11b <- rm_percent_sign(df3_cd11b)
df4_acsa2 <- rm_percent_sign(df4_acsa2)
df5_mcfc  <- rm_percent_sign(df5_mcfc)
df6_mm24  <- rm_percent_sign(df6_mm24)
df7_fans  <- rm_percent_sign(df7_fans)
