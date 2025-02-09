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
p_load("readxl", "tidyverse", "ggsci")

# Data exported as multi-sheet .xlsx file. Read in each sheet and store as
# seperate objects.
xlsxpath <- "data/numerical_data_for_upgrade_report.xlsx"

sheet_names <- excel_sheets(xlsxpath)

excel_list <- lapply(sheet_names, function(sheet){
  
  read_excel(xlsxpath, sheet = sheet)
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

# Now that we've tidied up all the data, we save each df as a csv.
# use original sheet names as file names

# remove the one sheet we have not processed
file_names <- sheet_names[-7]

# put the dataframes in a list to process together
df_list <- list(df1_bd,
                df2_viab,
                df3_cd11b,
                df4_acsa2,
                df5_mcfc,
                df6_mm24,
                df7_fans)

# give elements of the list the original spreadsheet names
names(df_list) <- file_names

# save each as a csv file in the output folder (for other purposes)
lapply(names(df_list), function(file_name) {
  
  csv_file <- paste0("output/", file_name, ".csv")
  
  write.csv(df_list[[file_name]], file = csv_file, row.names = FALSE)
})

# save the sheet we did not include in the wrangling
df_mfi <- as.data.frame(excel_list[7])
mfi_csv <- paste0("output/", sheet_names[7], ".csv")
write_csv(df_mfi, file = mfi_csv)


###
# Data Visualisation and Analysis
###

# specify some aesthetic options that can be added to any ggplot.
toms_theme <- list(
  theme_linedraw(),
  theme(
    legend.position = "none",
    axis.title = element_text(size = 24),
    axis.text = element_text(size =18),
    panel.background = element_rect(fill = "white", colour = NA),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )
)
## df1 analysis
# summarise the df1 data to plot
df1_sum <- df1_bd %>% 
  group_by(sample_type) %>% 
  summarise(
    mean_cells_pct_total = mean(cells_pct_total, na.rm = TRUE),
    sd_cells_pct_total = sd(cells_pct_total, na.rm = TRUE)
  )
# Give better names for graphing purposes, re-level factors for bar ordering.
df1_sum[1,1] <- "Post"
df1_sum[2,1] <- "Pre"
df1_sum <- df1_sum[c(2,1),]
df1_sum$sample_type <- factor(df1_sum$sample_type, levels = c("Pre", "Post"))

# make the plot from the summarised data.
p1_bdbar <- ggplot(df1_sum, aes(x = sample_type,
                                y = mean_cells_pct_total,
                                fill = sample_type)) +
  geom_bar(stat = "identity", width =0.3, position = position_dodge()) +
  geom_errorbar(aes(
    ymin = mean_cells_pct_total - sd_cells_pct_total,
    ymax = mean_cells_pct_total + sd_cells_pct_total
  ),
  width = 0.2,
  position = position_dodge(0.7)
  ) +
  labs(
    x = "Debris removal status",
    y = "% Cells"
  ) +
  theme_linedraw() +
  scale_fill_ucscgb()

# add the data points as dots, requires refactoring sample_type in df1_bd
as.factor(df1_bd$sample_type)
df1_bd$sample_type <- fct_recode(df1_bd$sample_type,
                                           Pre = "pre-debris",
                                           Post = "post-debris")
p1_bdbar + geom_jitter(
  data = df1_bd,
  aes(x = sample_type,
      y = cells_pct_total),
  width = 0.1,
  size = 2
)

p1_bdbar <- p1_bdbar + toms_theme
p1_bdbar  

p1_file <- paste0("output/", sheet_names[1], "_1", ".png")
ggsave(filename = p1_file, device = "png", dpi = 300, height = 5, width = 5)   


# get summary statistics for results section (make this reusable function)
generate_summary <- function(data) {
  #get dataframe name as string
  data_name <- deparse(substitute(data))
  
  # calculate summary stats foi any numeric columns in the dataframe
  summary_data <- data %>% 
    summarise(across(
      where(~ is.numeric(.x)),
      list(mean = ~mean(.x, na.rm =TRUE),
           sd = ~sd(.x, na.rm = TRUE))
    ))
  # create new object adding _summary as a suffix
  summary_name <- paste0(data_name, "_summary")
  
  # assign to new data frame
  assign(summary_name, summary_data, envir = .GlobalEnv)
  
}

generate_summary(df1_bd)


# df2 analysis

# create the summary
generate_summary(df2_viab)

# create plot of viable cells percetnage
p2_viability <- ggplot(df2_viab, aes(x = factor(1), y = cells_pct_viable_cells, fill = factor(1))) +
  stat_summary(
    fun = mean,
    geom = "bar",
    width = 0.3
  ) +
  scale_fill_ucscgb()+
  
  stat_summary(
    fun.data = function(x) {
      m <-  mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      data.frame(y = m, ymin = m - s, ymax = m + s)
    },
    geom = "errorbar",
    width = 0.2,
  ) +
  geom_jitter(width = 0.1, size = 2) +
  
  labs(x = "Samples", y = "Viable cell %") +
  theme_linedraw() +
  toms_theme +
  scale_x_discrete(breaks = NULL, labels = NULL)

# plot similar plot for viable cells percentage of total
p3_viability_total <- ggplot(df2_viab, aes(x = factor(1), y = cells_pct_viable_sample, fill = factor(1))) +
  stat_summary(
    fun = mean,
    geom = "bar",
    width = 0.3
  ) +
  scale_fill_ucscgb()[2] +
  
  stat_summary(
    fun.data = function(x) {
      m <-  mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      data.frame(y = m, ymin = m - s, ymax = m + s)
    },
    geom = "errorbar",
    width = 0.2,
  ) +
  geom_jitter(width = 0.1, size = 2) +
  
  labs(x = "Samples", y = "Viable cell %") +
  theme_linedraw() +
  toms_theme +
  scale_x_discrete(breaks = NULL, labels = NULL)
