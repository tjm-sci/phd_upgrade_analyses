# Date: 2025-02-06
# Author: T.Murphy 
# Description 
# This script produces simple graphs from flow cytometry data exported from
# FlowJo analysis software. The graphs form part of the figures for TM's
# Phd upgrade report/presentation. 

path <- file.path("home", "tmurphy", "phd_work",
                  "25-02-06-graphs-for-phd-upgrade")


# load necessary packages
if (!require(pacman)){
  install.packages("pacman")
}
library(pacman)
p_load("readxl", "tidyverse", "ggsci", "ggdist", "scales")
p_load_current_gh("erocoar/gghalves")
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

# add column for grouping samples with
df5_mcfc <- df5_mcfc %>% 
  mutate(
    sample_group = case_when(
      str_detect(sample, "MACS Pre-sort") ~ "pre_sort",
      str_detect(sample, "MACS -") ~ "MACS_neuronal",
      str_detect(sample, "MACS \\+") ~ "MACS_glial",
      str_detect(sample, "FACS") ~ "FACS"
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
toms_theme_no_legend <- list(
  theme_linedraw(),
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size =12),
        legend.position = "none"))

# same except with a legend
toms_theme_legend <- list(
  theme_linedraw(),
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size =12)))

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
  scale_fill_aaas() +
  toms_theme_no_legend

# add the data points as dots, requires refactoring sample_type in df1_bd
as.factor(df1_bd$sample_type)
df1_bd$sample_type <- fct_recode(df1_bd$sample_type,
                                           Pre = "pre-debris",
                                           Post = "post-debris")
p1_bdbar <- p1_bdbar + geom_jitter(
  data = df1_bd,
  aes(x = sample_type,
      y = cells_pct_total),
  width = 0.1,
  size = 2
) +
  toms_theme_no_legend

p1_bdbar  

p1_file <- paste0("output/", sheet_names[1], "_1", ".png")
ggsave(filename = p1_file, device = "png", dpi = 300, height = 5, width = 5)   


# get summary statistics for results section (make this reusable function)
generate_summary <- function(data, group.by = NULL) {
  # get name of data frame as string
  data_name <- deparse(substitute(data))
  
  # if a grouping column is provided as argument, function groups data first
  # beore calculating column by coumn summaries
  if(!is.null(group.by)) {
    grouping_column <- enquo(group.by)
    summary_data <- data %>% 
      group_by(!!grouping_column) %>% 
      summarise(across(
        where(is.numeric),
        list(mean = ~ mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE))
              ),
        .groups = "drop")
  } else {
          summary_data <- data %>% 
            summarise(across(
              where(is.numeric),
              list(means = ~ mean(.x, na.rm = TRUE),
                   sd = ~ sd(.x, na.rm = TRUE))
            ))
  }
  summary_name <- paste0(data_name, "_summary")
  assign(summary_name, summary_data, envir = .GlobalEnv)
  }
 
# run our summary function on all data
generate_summary(df1_bd, group.by = df1_bd$sample_type)
df1_summary_filename <- paste0("output/", sheet_names[1], "_summary.csv")
write_csv(df1_bd_summary, file = df1_summary_filename)

# df2 analysis

# create the summary
generate_summary(df2_viab)
df2_summary_filename <- paste0("output/", sheet_names[2], "_summary.csv")
write_csv(df2_viab_summary, file = df2_summary_filename)

# create plot of viable cells percetnage
p2_viability <- ggplot(df2_viab, aes(x = factor(1), y = cells_pct_viable_cells, fill = factor(1))) +
  stat_summary(
    fun = mean,
    geom = "bar",
    width = 0.3
  ) +
  scale_fill_aaas()+
  
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
  toms_theme_no_legend +
  scale_x_discrete(breaks = NULL, labels = NULL)

# plot similar plot for viable cells percentage of total
p3_viability_total <- ggplot(df2_viab, aes(x = factor(1), y = cells_pct_viable_sample, fill = factor(1))) +
  stat_summary(
    fun = mean,
    geom = "bar",
    width = 0.3
  ) +
  scale_fill_manual(values = pal_ucscgb("default")(2)[2]) +
  
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
  
  labs(x = "Samples", y = "Viable cell % of total events") +
  theme_linedraw() +
  toms_theme_no_legend +
  scale_x_discrete(breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(0, 60))

# save the two plots for this dataset
p2_file <- paste0("output/", sheet_names[2], "_1", ".png")
p3_file <-  paste0("output/", sheet_names[2], "_2", ".png")
ggsave(plot = p2_viability, filename = p2_file, device = "png", dpi = 300, height = 5, width = 5)
ggsave(plot = p3_viability_total, filename = p3_file, device = "png", dpi = 300, height = 5, width = 5)


# df3 graphs & analysis
generate_summary(df3_cd11b, group.by = df3_cd11b$condition)
df3_summary_filename <- paste0("output/", sheet_names[3], "_summary.csv")
write_csv(df3_cd11b_summary, file = df3_summary_filename)

df3_cd11b <- df3_cd11b %>% 
  mutate(condition = factor(condition,
                             levels = c(
                               "post_debris",
                               "negative_fraction",
                               "positive_fraction"
                             )))

p4_cd11b <- ggplot(df3_cd11b,
                   aes(x = condition,
                       y = cells_pct_viable_cells,
                       fill = condition)) +
  stat_summary(fun = mean,
               geom = "bar",
               width = 0.3) +
  
  stat_summary(fun.data = function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s) 
  },
  geom = "errorbar",
  width = 0.3) +
  
  geom_jitter(width = 0.1, 
              size = 2) +
  
  labs(x = "Fraction",
       y = "Viable cells %") +
  theme_linedraw() +
  scale_fill_aaas() +
  toms_theme_no_legend +
  theme(legend.position = "none")
  
# make same graph for total of  sample viability
p5_cd11b <- ggplot(df3_cd11b,
                   aes(x = condition,
                       y = cells_pct_viable_sample,
                       fill = condition)) +
  stat_summary(fun = mean,
               geom = "bar",
               width = 0.3) +
  
  stat_summary(fun.data = function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s) 
  },
  geom = "errorbar",
  width = 0.3) +
  
  geom_jitter(width = 0.1, 
              size = 2) +
  
  labs(x = "Fraction",
       y = "Viable cells % of total events") +
  theme_linedraw() +
  scale_fill_aaas() +
  toms_theme_no_legend +
  theme(legend.position = "none")

# save both plots
# save the two plots for this dataset
p4_file <- paste0("output/", sheet_names[3], "_1", ".png")
p5_file <-  paste0("output/", sheet_names[3], "_2", ".png")
ggsave(plot = p4_cd11b, filename = p4_file, device = "png", dpi = 300, height = 5, width = 8)
ggsave(plot = p5_cd11b, filename = p5_file, device = "png", dpi = 300, height = 5, width = 8)

# df6 acsa2 dataset
# generate summary
generate_summary(df4_acsa2, group.by = df4_acsa2$condition)
df4_summary_filename <- paste0("output/", sheet_names[4], "_summary.csv")
write_csv(df4_acsa2, file = df4_summary_filename)

# factorise the conditons for plotting order to be correct
df4_acsa2 <- df4_acsa2 %>% 
  mutate(condition = factor(condition,
                            levels = c(
                              "post_debris",
                              "negative_fraction",
                              "positive_fraction"
                            )))

# plots
p6_acsa2 <- ggplot(df4_acsa2,
                   aes(x = condition,
                       y = cells_pct_viable_cells,
                       fill = condition)) +
  stat_summary(fun = mean,
               geom = "bar",
               width = 0.3) +
  
  stat_summary(fun.data = function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s) 
  },
  geom = "errorbar",
  width = 0.3) +
  
  geom_jitter(width = 0.15, 
              size = 2) +
  
  labs(x = "Fraction",
       y = "Viable cells %") +
  theme_linedraw() +
  scale_fill_aaas() +
  #scale_fill_manual(values = pal_ucscgb("default")(6)[4:6]) +
  toms_theme_no_legend +
  theme(legend.position = "none")

p7_acsa2 <- ggplot(df4_acsa2,
                   aes(x = condition,
                       y = cells_pct_viable_sample,
                       fill = condition)) +
  stat_summary(fun = mean,
               geom = "bar",
               width = 0.3) +
  
  stat_summary(fun.data = function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s) 
  },
  geom = "errorbar",
  width = 0.3) +
  
  geom_jitter(width = 0.15, 
              size = 2) +
  
  labs(x = "Fraction",
       y = "Viable cells % of total events") +
  theme_linedraw() +
  #scale_fill_aaas() +
  scale_fill_aaas() +
  toms_theme_no_legend +
  theme(legend.position = "none")

# save the two acsa graphs
p6_file <- paste0("output/", sheet_names[4], "_1", ".png")
p7_file <-  paste0("output/", sheet_names[4], "_2", ".png")
ggsave(plot = p6_acsa2, filename = p6_file, device = "png", dpi = 300, height = 5, width = 8)
ggsave(plot = p7_acsa2, filename = p7_file, device = "png", dpi = 300, height = 5, width = 8)


# df5
# make summary of dataset and save
generate_summary(df5_mcfc, group.by = df5_mcfc$sample_group)
df5_summary_filename <- paste0("output/", sheet_names[5], "_summary.csv")
write_csv(df5_mcfc_summary, file = df5_summary_filename)

# plotting df5
# add a column called sample_group that states where the sample came from
# reorder this as a factor for ordering the graph's x-axis
df5_mcfc <- df5_mcfc %>% 
  mutate(
    sample_group = factor(sample_group,
      levels = c("pre_sort",
                 "MACS_glial",
                 "MACS_neuronal",
                 "FACS")
    )
  )

# create a longer format dataframe where rows are repeated once for each cell
# type/ sample-type combo
df5_long <- df5_mcfc %>% 
  pivot_longer(
    cols = c(
      oligodendrocyte_pct,
      microglia_pct,
      astrocyte_pct,
      neurons_hi_pct,
      neurons_lo_pct,
    ),
    names_to = "cell_type",
    values_to = "pct"
  ) %>% 
  
  mutate(
    cell_type = factor(cell_type,
                       levels = c(
                         "oligodendrocyte_pct",
                         "microglia_pct",
                         "astrocyte_pct",
                         "neurons_hi_pct",
                         "neurons_lo_pct"
                       )
                       )
  )

# We need to prevent the error bars form going below zero. Write a function to
# force lower error bar to zero if negative value is assumed 

mean_sdl_pos <- function(x, mult = 1){
  m <- mean(x)
  s <- sd(x)
  ymin <- m - mult * s
  ymax <- m + mult * s
  if (ymin < 0) ymin <- 0
  c(y = m, ymin = ymin, ymax = ymax)
}

# create a grouped bar plot of the data
p8_mcfc_bar <- ggplot(df5_long, aes(x = sample_group, y = pct, fill = cell_type)) +
  
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
   geom_jitter(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
    size = 1.8,
    shape = 21
  ) +
  
  stat_summary(
    fun.data = mean_sdl_pos,
    fun.args = list(mult = 1),
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.5,
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  labs(
    x = "Sample type",
    y = "% of total events",
    color = "cell type"
    ) +
  
  scale_fill_aaas() + 
  scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  toms_theme_legend

# Half box plots, half dot
p9_mcfc_rncl <- ggplot(df5_long, aes(x = sample_group, y = pct, fill = cell_type)) +
 
   # plot boxplots on left
  geom_half_boxplot(
    nudge = 0.03,
    side = "l",
    width = 1,
    outlier.shape = NA,
    position = position_dodge(width = 0.9)
  )+
  # plot dots on right
  geom_half_point(
    width = 0.2,
    shape = 21,
    size = 2,
    inherit.aes = TRUE,
    side = "r",
    alpha = 0.8,
    position = position_dodge(width = 0.9)
    ) +
  
  labs(
    x = "Sample Type",
    y = "Percentage of total events (%)",
    fill = "Cell type"
  ) +
  scale_fill_aaas() +
  toms_theme_legend

# do the same for the absolute cell count
# i.e. create longer data frame, then code the cell counts as ordered factors
df5_long_counts <- df5_mcfc %>% 
  pivot_longer(
    cols = c(
      oligodendrocyte_count,
      microglia_count,
      astrocyte_count,
      neurons_hi_count,
      neurons_lo_count,
    ),
    names_to = "cell_type",
    values_to = "count"
  ) %>% 
  
  mutate(
    cell_type = factor(cell_type,
                       levels = c(
                         "oligodendrocyte_count",
                         "microglia_count",
                         "astrocyte_count",
                         "neurons_hi_count",
                         "neurons_lo_count"
                       )
    )
  )

# create a grouped bar plot of the data for the count data.
p10_mcfc_counts_bar <- ggplot(df5_long_counts, aes(x = sample_group, y = count, fill = cell_type)) +
  
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
    size = 1.8,
    shape = 21
  ) +
  
  stat_summary(
    fun.data = mean_sdl_pos,
    fun.args = list(mult = 1),
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.5,
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  scale_y_log10(
    labels = label_log(base = 10, digits = 2)
    )+
    labs(
    x = "Sample type",
    y = "Cell count",
    color = "Cell type"
  ) +
  scale_fill_aaas() + 
  toms_theme_legend

# Half box plots, half dot for count data.
p11_mcfc_half <- ggplot(df5_long_counts, aes(x = sample_group, y = count, fill = cell_type)) +
  
  # overlay boxplot
  geom_half_boxplot(
    nudge = 0.03,
    side = "l",
    width = 1,
    outlier.shape = NA,
    position = position_dodge(width = 0.9)
  )+
  
  geom_half_point(
    width = 0.2,
    shape = 21,
    size = 2,
    inherit.aes = TRUE,
    side = "r",
    alpha = 0.8,
    position = position_dodge(width = 0.9)
  ) +
  
  labs(
    x = "Sample Type",
    y = "Cell count",
    fill = "Cell type"
  ) +
  scale_y_log10(labels = label_log(base = 10, digits = 2)
  ) +
  scale_fill_aaas() +
  toms_theme_legend

# save the 4 plots made for df5
p8_file <- paste0("output/", sheet_names[5], "_bars_pct", ".png")
p9_file <-  paste0("output/", sheet_names[5], "_box_pct", ".png")
p10_file <-  paste0("output/", sheet_names[5], "_bars_count", ".png")
p11_file <-  paste0("output/", sheet_names[5], "_box_count", ".png")

# save em up
ggsave(plot = p8_mcfc_bar , filename = p8_file, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p9_mcfc_rncl, filename = p9_file, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p10_mcfc_counts_bar, filename = p10_file, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p11_mcfc_half, filename = p11_file, device = "png", dpi = 300, height = 5, width = 12)


# inferential statistics:
# to directly compare MACS versus FACS we need to take only matched samples
# that were present for MACS and FACS analyses. 
# We need to compare the percentage neurons in MACS_neuronal and the counts

# FILL THIS IN #

# df6 analysis 
# first add a cell fraction column that groups based on presence of string in 
# sample name column
df6_mm24 <- df6_mm24 %>% mutate(
  # new column describing what cell type we expect in each sample
  MACS_fraction = case_when(
    str_detect(sample, "Astrocytes") ~ "ACSA2_fraction",
    str_detect(sample, "Microglia")  ~ "CD11B_fraction",
    str_detect(sample, "Oligos") ~ "O4_fraction",
    str_detect(sample, "Neurons") ~ "neuron_fraction"
  )
) %>% 
  # order these as a factor for consistent plot order
  mutate(MACS_fraction = factor(MACS_fraction,
                                levels = c(
                                  "O4_fraction",
                                  "CD11B_fraction",
                                  "ACSA2_fraction",
                                  "neuron_fraction"
                                ))) %>% 
  #renaming the columns to distigusih them from our new MACS_fraction column
  rename(O4_PE_pct = oligodendrocyte_pct) %>% 
  rename(CD11B_APC_pct = microglia_pct ) %>% 
  rename(ACSA2_FITC_pct = astrocyte_pct) %>% 
  rename(neuron_hi_pct = neurons_hi_pct) %>% 
  rename(neuron_lo_pct = neurons_lo_pct) %>% 
  # do the same for the count columns
  rename(O4_PE_count = oligodendrocyte_count) %>% 
  rename(CD11B_APC_count = microglia_count) %>% 
  rename(ACSA2_FITC_count = astrocyte_count) %>% 
  rename(neuron_hi_count = neurons_hi_count) %>% 
  rename(neuron_lo_count = neurons_lo_count) 

# save the updated data as a csv  
df6_mm24_filename <- paste0("output/", sheet_names[6], ".csv")
write_csv(x = df6_mm24, file = df6_mm24_filename)
# generate summary statistics, save that as a seperate csv too.
df6_mm24_summary <- generate_summary(df6_mm24, group.by = df6_mm24$MACS_fraction)
df6_mm24_summary_filename <- paste0("output/", sheet_names[6], "_summary.csv")
write_csv(x = df6_mm24_summary, file = df6_mm24_summary_filename)


# As we did with df5, we will now make the data longer so there is repeated
# rows for the per cell type data 
df6_long_pct <- df6_mm24 %>% 
  pivot_longer(
    cols = c(
      O4_PE_pct,
      CD11B_APC_pct,
      ACSA2_FITC_pct,
      neuron_hi_pct,
      neuron_lo_pct,
    ),
    names_to = "cell_type_marker",
    values_to = "pct"
  ) %>% 
  # Making the new column an ordered factor
  mutate(
    cell_type_marker = factor(cell_type_marker, levels = c(
                         "O4_PE_pct",
                         "CD11B_APC_pct",
                         "ACSA2_FITC_pct",
                         "neuron_hi_pct",
                         "neuron_lo_pct"
                       )
    )
  )

# repeat for the count datq
df6_long_count <- df6_mm24 %>% 
  pivot_longer(
    cols = c(
      O4_PE_count,
      CD11B_APC_count,
      ACSA2_FITC_count,
      neuron_hi_count,
      neuron_lo_count,
    ),
    names_to = "cell_type_marker",
    values_to = "count"
  ) %>% 
  # Making the new column an ordered factor
  mutate(
    cell_type_marker = factor(cell_type_marker, levels = c(
      "O4_PE_count",
      "CD11B_APC_count",
      "ACSA2_FITC_count",
      "neuron_hi_count",
      "neuron_lo_count"
    )
    )
  )

# make the graphs as we did before with the previous df5
# grouped bar plot

# create a grouped bar plot of the percentage data
p12_mm24_pct_bar <- ggplot(df6_long_pct, aes(x = MACS_fraction, y = pct, fill = cell_type_marker)) +
  
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
    size = 1.8,
    shape = 21
  ) +
  
  stat_summary(
    fun.data = mean_sdl_pos,
    fun.args = list(mult = 1),
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.5,
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  labs(
    x = "Target population",
    y = "Percentage of total events (%)",
    color = "cell_type_marker"
  ) +
  
  scale_fill_aaas() + 
  scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  toms_theme_legend

# Box and dot plots for percentage data
p13_mm24_pct_box <- ggplot(df6_long_pct, aes(x = MACS_fraction, y = pct, fill = cell_type_marker)) +
  
  # plot boxplots on left
  geom_half_boxplot(
    nudge = 0.03,
    side = "l",
    width = 1,
    outlier.shape = NA,
    position = position_dodge(width = 0.9)
  )+
  # plot dots on right
  geom_half_point(
    width = 0.2,
    shape = 21,
    size = 2,
    inherit.aes = TRUE,
    side = "r",
    alpha = 0.8,
    position = position_dodge(width = 0.9)
  ) +
  
  labs(
    x = "Target population",
    y = "Percentage of total events (%)",
    fill = "cell_type_marker"
  ) +
  scale_fill_aaas() +
  toms_theme_legend

# Repeat for the cell count data
# create a grouped bar plot of the percentage data
p14_mm24_count_bar <- ggplot(df6_long_count, aes(x = MACS_fraction, y = count, fill = cell_type_marker)) +
  
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9),
    size = 1.8,
    shape = 21
  ) +
  
  stat_summary(
    fun.data = mean_sdl_pos,
    fun.args = list(mult = 1),
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.5,
    linewidth = 0.5,
    alpha = 0.8
  ) +
  scale_y_log10(
    labels = label_log(base = 10, digits = 2)
  ) +
  labs(
    x = "Target population",
    y = "Cell count",
    color = "cell_type_marker"
  ) +
  
  scale_fill_aaas() + 
  toms_theme_legend

# Box and dot plots for percentage data
p15_mm24_box_counts <- ggplot(df6_long_count, aes(x = MACS_fraction, y = count, fill = cell_type_marker)) +
  
  # plot boxplots on left
  geom_half_boxplot(
    nudge = 0.03,
    side = "l",
    width = 1,
    outlier.shape = NA,
    position = position_dodge(width = 0.9)
  )+
  # plot dots on right
  geom_half_point(
    width = 0.2,
    shape = 21,
    size = 2,
    inherit.aes = TRUE,
    side = "r",
    alpha = 0.8,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_log10(
    labels = label_log(base = 10, digits = 2)
    ) +
                  
  labs(
    x = "Target population",
    y = "Cell Count",
    fill = "cell_type_marker"
  ) +
  scale_fill_aaas() +
  toms_theme_legend

# save the df6 plots 
p12_filename <- paste0("output/", sheet_names[6], "_plot_1.png")
p13_filename <- paste0("output/", sheet_names[6], "_plot_2.png")
p14_filename <- paste0("output/", sheet_names[6], "_plot_3.png")
p15_filename <- paste0("output/", sheet_names[6], "_plot_4.png")

ggsave(plot = p12_mm24_pct_bar, filename = p12_filename, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p13_mm24_pct_box, filename = p13_filename, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p14_mm24_count_bar, filename = p14_filename, device = "png", dpi = 300, height = 5, width = 12)
ggsave(plot = p15_mm24_box_counts, filename = p15_filename, device = "png", dpi = 300, height = 5, width = 12)


# df7
# encode the sample name as a factor
df7_fans$sample <- as.factor(df7_fans$sample)
# Make a seprate plot for each cell type as a % of DAPI+ nuclei
nuclei_types <- c("microglial_nuclei_pct_dapi",
                  "astrocyte_nuclei_pct_dapi",
                  "oligo_nuclei_pct_dapi",
                  "neuron_nuclei_pct_dapi")
plot_list <- list()

# loop over each nuclei type column and make a plot
for (nt in nuclei_types){
  p <- ggplot(df7_fans, aes_string(x = "experiment", y = nt, fill = "experiment"))+
    geom_half_boxplot(
      nudge = 0.03,
      side = "l",
      width = 1,
      outlier.shape = NA,
      position = position_dodge(width = 0.9)
    )+
    # plot dots on right
    geom_half_point(
      width = 0.2,
      shape = 21,
      size = 3,
      inherit.aes = TRUE,
      side = "r",
      alpha = 0.8,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      title = paste(gsub("nuclei_pct_dapi", "", nt),"nuclei"),
      x = ("Expeirment"),
      y = "% DAPI+ events"
    ) +
    toms_theme_no_legend +
    scale_fill_aaas()
  plot_list[[nt]] <- p
}
# save each plot as single object and adjust y axes
pu1 <- plot_list[["microglial_nuclei_pct_dapi"]] + scale_y_continuous(limits = c(0,5) )
sox2 <- plot_list[["astrocyte_nuclei_pct_dapi"]] + scale_y_continuous(limits = c(0,6))
sox10 <-plot_list[["oligo_nuclei_pct_dapi"]] + scale_y_continuous(limits = c(4,16))
neun <- plot_list[["neuron_nuclei_pct_dapi"]] + scale_y_continuous(limits = c(45, 75))

# use grid extra to arrange plots in a grid
library(gridExtra)
fans_dapi_pct <- grid.arrange(
  pu1,
  sox2, 
  sox10,
  neun 
)

# repeat the process for the median fluorescnce intensity
nuclei_types_mfi <- c("microglial_nuclei_mfi",
                      "astrocyte_nuclei_mfi",
                      "oligo_nuclei_mfi",
                      "neuron_nuclei_mfi")
plot_list_mfi <- list()

# loop over each nuclei type column and make a plot
for (nt_mfi in nuclei_types_mfi){
  p_mfi <- ggplot(df7_fans, aes_string(x = "experiment", y = nt_mfi, fill = "experiment"))+
    geom_half_boxplot(
      nudge = 0.03,
      side = "l",
      width = 1,
      outlier.shape = NA,
      position = position_dodge(width = 0.9)
    )+
    # plot dots on right
    geom_half_point(
      inherit.aes = TRUE,
      #aes(color = "experiment", fill = "sample"),
      width = 0.2,
      shape = 21,
      size = 3,
      side = "r",
      alpha = 0.8,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      title = paste(gsub("nuclei_mfi", "", nt_mfi),"MFI"),
      x = "Experiment",
      y = "Channel MFI"
    ) +
    toms_theme_no_legend +
    scale_fill_aaas()
  plot_list_mfi[[nt_mfi]] <- p_mfi
}

# save each plot as single object and adjust y axes
pu1_mfi <- plot_list_mfi[["microglial_nuclei_mfi"]] + scale_y_continuous(limits = c(100, 600))
sox2_mfi <- plot_list_mfi[["astrocyte_nuclei_mfi"]] + scale_y_continuous(limits = c(0, 700))
sox10_mfi <-plot_list_mfi[["oligo_nuclei_mfi"]]     + scale_y_continuous(limits = c(200, NA))
neun_mfi <- plot_list_mfi[["neuron_nuclei_mfi"]]    + scale_y_continuous(limits = c(500, NA))

# use grid extra to arrange plots in a grid
library(gridExtra)
fans_dapi_mfi <- grid.arrange(
  pu1_mfi,
  sox2_mfi, 
  sox10_mfi,
  neun_mfi 
)

dapi_pct_grid_filename <- paste0("output/", sheet_names[7], "_1.png")
mfi_grid_filename <- paste0("output/", sheet_names[7], "_2.png")
ggsave(filename = dapi_pct_grid_filename, plot = fans_dapi_pct, dpi = 300,height = 10, width = 10 )
ggsave(filename = mfi_grid_filename, plot = fans_dapi_mfi, dpi = 300, height = 10, width = 10)


