library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(broom)
library(purrr)
library(outliers)
library(gplots)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#===================================================================
# import screen data ##################################
clean_screen_data<-read_csv("pa14nr_all_data.csv")
  
Pa14NR_hit_list<-clean_screen_data %>%
  filter(hit == "Y") %>%
  select(-Loc_ID, -hit) %>%
  rename("Pa14 gene locus" = `Active Gene Locus`) %>%
  rename("PA01 gene locus" = `PAO1 Orthologs`) %>%
  rename("Gene name" = `Active Gene Name`) %>%
  rename("Gene description" = `Active Gene Description`) %>%
  pivot_longer(., -c("Pa14 gene locus", "PA01 gene locus","Gene name","Gene description"), values_to = "z_score") %>%
  group_by(`Pa14 gene locus`) %>%
  mutate("z-score" = round(mean(z_score), 2)) %>%
  ungroup() %>%
  select(-name, - z_score) %>%
  unique() %>%
  arrange(desc(`z-score`)) %>%
  write_csv(., "pa14nr_hit_table.csv")
#===================================================================
# import validation data ##################################
combined_LB_Pa14_Tn_growth_curves<-read_csv("combined_LB_Pa14_Tn_growth_curves.csv") %>%
  select(-1)
combined_LB_Pa14_Tn_competition_data<-read_csv("combined_Tn_LB_competition.csv") %>%
  select(-1) %>%
  mutate(Condition = case_when(Condition == "JV1_fresh_LB" ~ "Fresh media",
                               Condition == "JV1_01_2_A05" ~ "KPPR1 + Pa14_51430", 
                               Condition == "JV1_02_2_A01" ~ "KPPR1 + Pa14_16930", 
                               Condition == "JV1_02_3_A03" ~ "KPPR1 + Pa14_09810", 
                               Condition == "JV1_04_3_G05" ~ "KPPR1 + Pa14_64590", 
                               Condition == "JV1_04_4_B05" ~ "KPPR1 + Pa14_05300", 
                               Condition == "JV1_06_1_A01" ~ "KPPR1 + Pa14_18520", 
                               Condition == "JV1_06_1_C05" ~ "KPPR1 + Pa14_01960", 
                               Condition == "JV1_08_1_A12" ~ "KPPR1 + Pa14_09480", 
                               Condition == "JV1_08_1_G07" ~ "KPPR1 + Pa14_19120", 
                               Condition == "JV1_11_2_A06" ~ "KPPR1 + Pa14_48650", 
                               Condition == "JV1_11_2_G10" ~ "KPPR1 + Pa14_58770", 
                               Condition == "JV1_12_4_C11" ~ "KPPR1 + Pa14_06570", 
                               Condition == "JV1_12_4_D09" ~ "KPPR1 + Pa14_32750", 
                               Condition == "JV1_14_1_D06" ~ "KPPR1 + Pa14_62830", 
                               Condition == "JV1_14_4_D12" ~ "KPPR1 + Pa14_17250",
                               Condition == "JV1_14_4_G05" ~ "KPPR1 + Pa14_20440",
                               Condition == "JV1_15_2_B11" ~ "KPPR1 + Pa14_68350",
                               Condition == "JV1_15_3_E03" ~ "KPPR1 + Pa14_72840",
                               Condition == "JV1_JV450" ~ "KPPR1 + Pa14",
                               Condition == "JV1_01_2_A05_sp" ~ "Pa14_51430 spent media", 
                               Condition == "JV1_02_2_A01_sp" ~ "Pa14_16930 spent media", 
                               Condition == "JV1_02_3_A03_sp" ~ "Pa14_09810 spent media", 
                               Condition == "JV1_04_3_G05_sp" ~ "Pa14_64590 spent media", 
                               Condition == "JV1_04_4_B05_sp" ~ "Pa14_05300 spent media", 
                               Condition == "JV1_06_1_A01_sp" ~ "Pa14_18520 spent media", 
                               Condition == "JV1_06_1_C05_sp" ~ "Pa14_01960 spent media", 
                               Condition == "JV1_08_1_A12_sp" ~ "Pa14_09480 spent media", 
                               Condition == "JV1_08_1_G07_sp" ~ "Pa14_19120 spent media", 
                               Condition == "JV1_11_2_A06_sp" ~ "Pa14_48650 spent media", 
                               Condition == "JV1_11_2_G10_sp" ~ "Pa14_58770 spent media", 
                               Condition == "JV1_12_4_C11_sp" ~ "Pa14_06570 spent media", 
                               Condition == "JV1_12_4_D09_sp" ~ "Pa14_32750 spent media", 
                               Condition == "JV1_14_1_D06_sp" ~ "Pa14_62830 spent media", 
                               Condition == "JV1_14_4_D12_sp" ~ "Pa14_17250 spent media",
                               Condition == "JV1_14_4_G05_sp" ~ "Pa14_20440 spent media",
                               Condition == "JV1_15_2_B11_sp" ~ "Pa14_68350 spent media",
                               Condition == "JV1_15_3_E03_sp" ~ "Pa14_72840 spent media",
                               Condition == "JV1_JV450_sp" ~ "Pa14 spent media"))

all_tn_abs_data<-read_csv("combined_Tn_phen_absord_data.csv")
#===================================================================
# graph Tn GC data ##################################
# calculate Pa14 Tn AUCs
Pa14_Tn_LB_names<-combined_LB_Pa14_Tn_growth_curves %>%
  select(strain) %>%
  unique()
Pa14_Tn_LB_trial_1_AUC_calc<-combined_LB_Pa14_Tn_growth_curves %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Pa14_Tn_LB_trial_1_AUC<-c(AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_51430),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_16930),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_09810),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_64590),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_05300),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_18520),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_01960),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_09480),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_19120),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_48650),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_58770),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_06570),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_32750),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_62830),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_17250),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_20440),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_68350),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14_72840),
                          AUC(Pa14_Tn_LB_trial_1_AUC_calc$Time, Pa14_Tn_LB_trial_1_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(Pa14_Tn_LB_names, .) %>%
  mutate(trial = "trial_1")
Pa14_Tn_LB_trial_2_AUC_calc<-combined_LB_Pa14_Tn_growth_curves %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Pa14_Tn_LB_trial_2_AUC<-c(AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_51430),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_16930),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_09810),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_64590),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_05300),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_18520),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_01960),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_09480),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_19120),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_48650),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_58770),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_06570),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_32750),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_62830),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_17250),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_20440),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_68350),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14_72840),
                          AUC(Pa14_Tn_LB_trial_2_AUC_calc$Time, Pa14_Tn_LB_trial_2_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(Pa14_Tn_LB_names, .) %>%
  mutate(trial = "trial_2")
Pa14_Tn_LB_trial_3_AUC_calc<-combined_LB_Pa14_Tn_growth_curves %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Pa14_Tn_LB_trial_3_AUC<-c(AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_51430),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_16930),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_09810),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_64590),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_05300),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_18520),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_01960),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_09480),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_19120),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_48650),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_58770),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_06570),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_32750),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_62830),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_17250),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_20440),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_68350),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14_72840),
                          AUC(Pa14_Tn_LB_trial_3_AUC_calc$Time, Pa14_Tn_LB_trial_3_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(Pa14_Tn_LB_names, .) %>%
  mutate(trial = "trial_3")
Pa14_Tn_LB_trial_4_AUC_calc<-combined_LB_Pa14_Tn_growth_curves %>%
  filter(trial == "trial_4") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Pa14_Tn_LB_trial_4_AUC<-c(AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_51430),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_16930),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_09810),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_64590),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_05300),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_18520),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_01960),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_09480),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_19120),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_48650),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_58770),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_06570),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_32750),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_62830),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_17250),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_20440),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_68350),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14_72840),
                          AUC(Pa14_Tn_LB_trial_4_AUC_calc$Time, Pa14_Tn_LB_trial_4_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(Pa14_Tn_LB_names, .) %>%
  mutate(trial = "trial_4")

#combine AUC data#
Pa14_Tn_LB_compiled_AUC<-rbind(Pa14_Tn_LB_trial_1_AUC, Pa14_Tn_LB_trial_2_AUC, Pa14_Tn_LB_trial_3_AUC, Pa14_Tn_LB_trial_4_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup() 

# run stats
Pa14_Tn_LB_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = Pa14_Tn_LB_compiled_AUC))

#graph rhl LB growth 24hr AUC data
ggplot(Pa14_Tn_LB_compiled_AUC, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, fill = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2,
             shape = 21,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 6) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0,25,50,75,100),
                     labels = c(0,25,50,75,100)) +
  scale_fill_manual(name = "Strain",
                    breaks = c("Pa14_51430",
                               "Pa14_16930",  
                               "Pa14_09810",
                               "Pa14_64590",
                               "Pa14_05300",
                               "Pa14_18520",
                               "Pa14_01960",
                               "Pa14_09480",
                               "Pa14_19120",
                               "Pa14_48650",
                               "Pa14_58770",
                               "Pa14_06570",
                               "Pa14_32750",
                               "Pa14_62830",
                               "Pa14_17250",
                               "Pa14_20440",
                               "Pa14_68350",
                               "Pa14_72840",
                               "Pa14"),
                    values = c("#f0f8ff",
                               "#e7feff",  
                               "#e7f6ff",
                               "#d0fefe",
                               "#d5ffff",
                               "#ccffff",
                               "#b2ffff",
                               "#c6f5fe",
                               "#b2fcff",
                               "#c1f5f5",
                               "#abffff",
                               "#99ffff",
                               "#bce1eb",
                               "red",
                               "#b0e1f1",
                               "#b0dced",
                               "#a8dced",
                               "#73fdff",
                               "#000080")) +
  scale_x_discrete(limits = c("Pa14",
                            "Pa14_01960",
                            "Pa14_05300",
                            "Pa14_06570",
                            "Pa14_09480",
                            "Pa14_09810",
                            "Pa14_16930",  
                            "Pa14_17250",
                            "Pa14_18520",
                            "Pa14_19120",
                            "Pa14_20440",
                            "Pa14_32750",
                            "Pa14_48650",
                            "Pa14_51430",
                            "Pa14_58770",
                            "Pa14_62830",
                            "Pa14_64590",
                            "Pa14_68350",
                            "Pa14_72840"),
                    labels = c("PA14",
                               "PA14_01960",
                               "PA14_05300",
                               "PA14_06570",
                               "PA14_09480",
                               "PA14_09810",
                               "PA14_16930",  
                               "PA14_17250",
                               "PA14_18520",
                               "PA14_19120",
                               "PA14_20440",
                               "PA14_32750",
                               "PA14_48650",
                               "PA14_51430",
                               "PA14_58770",
                               "PA14_62830",
                               "PA14_64590",
                               "PA14_68350",
                               "PA14_72840")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  geom_richtext(data=tibble(x=16, y=35), fill = NA, label.color = NA, label="*p* = 1.6e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Pa14_Tn_LB_growth_AUC_24hr.pdf", height = 6, width = 12, unit = "cm")
#===================================================================
# graph Tn competition data ##################################
# select Pa14NR Tn T24 co-culture data
LB_Pa14_Tn_co_culture_data <- combined_LB_Pa14_Tn_competition_data %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "Pa14NR_Tn_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
LB_Pa14_Tn_co_culture_ttest<-pairwise.t.test(LB_Pa14_Tn_co_culture_data$log_fold_change, 
                                             LB_Pa14_Tn_co_culture_data$Condition, p.adjust.method = "none")
# plot data
LB_Pa14_Tn_co_culture_data %>%
  filter(Condition != "KPPR1 + Pa14") %>%
  ggplot(., aes(x=Condition, y=log_fold_change, fill = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2,
             shape = 21,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = "Log fold change<br>from WT PA14") +
  scale_y_continuous(limits = c(-1, 2.5),
                     breaks = c(-1, 0, 1, 2),
                     labels = c(-1, 0, 1, 2)) +
  scale_fill_manual(name = "Strain",
                    breaks = c("Fresh media",
                               "KPPR1 + Pa14_01960",
                               "KPPR1 + Pa14_05300",
                               "KPPR1 + Pa14_06570",
                               "KPPR1 + Pa14_09480",
                               "KPPR1 + Pa14_09810",
                               "KPPR1 + Pa14_16930",  
                               "KPPR1 + Pa14_17250",
                               "KPPR1 + Pa14_18520",
                               "KPPR1 + Pa14_19120",
                               "KPPR1 + Pa14_20440",
                               "KPPR1 + Pa14_32750",
                               "KPPR1 + Pa14_48650",
                               "KPPR1 + Pa14_51430",
                               "KPPR1 + Pa14_58770",
                               "KPPR1 + Pa14_62830",
                               "KPPR1 + Pa14_64590",
                               "KPPR1 + Pa14_68350",
                               "KPPR1 + Pa14_72840"),
                    values = c("black",
                               "red",
                               "#f0f8ff",  
                               "red",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "red",
                               "red",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff"),
                    labels = c("Fresh media",
                               "KPPR1 + PA14_01960",
                               "KPPR1 + PA14_05300",
                               "KPPR1 + PA14_06570",
                               "KPPR1 + PA14_09480",
                               "KPPR1 + PA14_09810",
                               "KPPR1 + PA14_16930",  
                               "KPPR1 + PA14_17250",
                               "KPPR1 + PA14_18520",
                               "KPPR1 + PA14_19120",
                               "KPPR1 + PA14_20440",
                               "KPPR1 + PA14_32750",
                               "KPPR1 + PA14_48650",
                               "KPPR1 + PA14_51430",
                               "KPPR1 + PA14_58770",
                               "KPPR1 + PA14_62830",
                               "KPPR1 + PA14_64590",
                               "KPPR1 + PA14_68350",
                               "KPPR1 + PA14_72840")) +
  scale_x_discrete(name = "Strain",
                    breaks = c("Fresh media",
                               "KPPR1 + Pa14_01960",
                               "KPPR1 + Pa14_05300",
                               "KPPR1 + Pa14_06570",
                               "KPPR1 + Pa14_09480",
                               "KPPR1 + Pa14_09810",
                               "KPPR1 + Pa14_16930",  
                               "KPPR1 + Pa14_17250",
                               "KPPR1 + Pa14_18520",
                               "KPPR1 + Pa14_19120",
                               "KPPR1 + Pa14_20440",
                               "KPPR1 + Pa14_32750",
                               "KPPR1 + Pa14_48650",
                               "KPPR1 + Pa14_51430",
                               "KPPR1 + Pa14_58770",
                               "KPPR1 + Pa14_62830",
                               "KPPR1 + Pa14_64590",
                               "KPPR1 + Pa14_68350",
                               "KPPR1 + Pa14_72840"),
                    labels = c("Fresh media",
                               "KPPR1 + PA14_01960",
                               "KPPR1 + PA14_05300",
                               "KPPR1 + PA14_06570",
                               "KPPR1 + PA14_09480",
                               "KPPR1 + PA14_09810",
                               "KPPR1 + PA14_16930",  
                               "KPPR1 + PA14_17250",
                               "KPPR1 + PA14_18520",
                               "KPPR1 + PA14_19120",
                               "KPPR1 + PA14_20440",
                               "KPPR1 + PA14_32750",
                               "KPPR1 + PA14_48650",
                               "KPPR1 + PA14_51430",
                               "KPPR1 + PA14_58770",
                               "KPPR1 + PA14_62830",
                               "KPPR1 + PA14_64590",
                               "KPPR1 + PA14_68350",
                               "KPPR1 + PA14_72840")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey")) +
  geom_richtext(data=tibble(x=2, y=1.1), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=1), fill = NA, label.color = NA, label="*p* = 0.03" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=1), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=10, y=2.3), fill = NA, label.color = NA, label="*p* = 4e-13" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=12, y=1.4), fill = NA, label.color = NA, label="*p* = 0.04" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=14, y=2), fill = NA, label.color = NA, label="*p* = 3e-11" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=16, y=2.3), fill = NA, label.color = NA, label="*p* = 2.5e-14" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/Pa14_Tn_LB_co_culture.pdf", height = 8, width = 16, unit = "cm")

# select Pa14NR Tn T24 spent media culture data
LB_Pa14_Tn_spent_media_culture_data <- combined_LB_Pa14_Tn_competition_data %>%
  filter(culture_conditions == "spent" | culture_conditions == "control" ) %>%
  filter(competition == "spent_Pa14NR_Tn_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
LB_Pa14_Tn_spent_media_culture_ttest<-pairwise.t.test(LB_Pa14_Tn_spent_media_culture_data$log_fold_change, 
                                             LB_Pa14_Tn_spent_media_culture_data$Condition, p.adjust.method = "none")
# plot data
LB_Pa14_Tn_spent_media_culture_data %>%
  filter(Condition != "Pa14 spent media") %>%
  ggplot(., aes(x=Condition, y=log_fold_change, fill = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2,
             shape = 21,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = "Log fold change<br>from WT PA14") +
  scale_y_continuous(limits = c(-1.5, 4),
                     breaks = c(-1, 0, 1, 2, 3, 4),
                     labels = c(-1, 0, 1, 2, 3, 4)) +
  scale_fill_manual(name = "Strain",
                    breaks = c("Fresh media",
                               "Pa14_01960 spent media",
                               "Pa14_05300 spent media",
                               "Pa14_06570 spent media",
                               "Pa14_09480 spent media",
                               "Pa14_09810 spent media",
                               "Pa14_16930 spent media",  
                               "Pa14_17250 spent media",
                               "Pa14_18520 spent media",
                               "Pa14_19120 spent media",
                               "Pa14_20440 spent media",
                               "Pa14_32750 spent media",
                               "Pa14_48650 spent media",
                               "Pa14_51430 spent media",
                               "Pa14_58770 spent media",
                               "Pa14_62830 spent media",
                               "Pa14_64590 spent media",
                               "Pa14_68350 spent media",
                               "Pa14_72840 spent media"),
                    values = c("black",
                               "#f0f8ff",
                               "#f0f8ff",  
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "red",
                               "#f0f8ff",
                               "#f0f8ff",
                               "#f0f8ff"),
                    labels = c("Fresh media",
                               "PA14_01960 spent media",
                               "PA14_05300 spent media",
                               "PA14_06570 spent media",
                               "PA14_09480 spent media",
                               "PA14_09810 spent media",
                               "PA14_16930 spent media",  
                               "PA14_17250 spent media",
                               "PA14_18520 spent media",
                               "PA14_19120 spent media",
                               "PA14_20440 spent media",
                               "PA14_32750 spent media",
                               "PA14_48650 spent media",
                               "PA14_51430 spent media",
                               "PA14_58770 spent media",
                               "PA14_62830 spent media",
                               "PA14_64590 spent media",
                               "PA14_68350 spent media",
                               "PA14_72840 spent media")) +
  scale_x_discrete(name = "Strain",
                    breaks = c("Fresh media",
                               "Pa14_01960 spent media",
                               "Pa14_05300 spent media",
                               "Pa14_06570 spent media",
                               "Pa14_09480 spent media",
                               "Pa14_09810 spent media",
                               "Pa14_16930 spent media",  
                               "Pa14_17250 spent media",
                               "Pa14_18520 spent media",
                               "Pa14_19120 spent media",
                               "Pa14_20440 spent media",
                               "Pa14_32750 spent media",
                               "Pa14_48650 spent media",
                               "Pa14_51430 spent media",
                               "Pa14_58770 spent media",
                               "Pa14_62830 spent media",
                               "Pa14_64590 spent media",
                               "Pa14_68350 spent media",
                               "Pa14_72840 spent media"),
                    labels = c("Fresh media",
                               "PA14_01960 spent media",
                               "PA14_05300 spent media",
                               "PA14_06570 spent media",
                               "PA14_09480 spent media",
                               "PA14_09810 spent media",
                               "PA14_16930 spent media",  
                               "PA14_17250 spent media",
                               "PA14_18520 spent media",
                               "PA14_19120 spent media",
                               "PA14_20440 spent media",
                               "PA14_32750 spent media",
                               "PA14_48650 spent media",
                               "PA14_51430 spent media",
                               "PA14_58770 spent media",
                               "PA14_62830 spent media",
                               "PA14_64590 spent media",
                               "PA14_68350 spent media",
                               "PA14_72840 spent media")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey")) +
  geom_richtext(data=tibble(x=10, y=2.3), fill = NA, label.color = NA, label="*p* = 3.2e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=14, y=3.4), fill = NA, label.color = NA, label="*p* = 3.7e-10" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=16, y=3.5), fill = NA, label.color = NA, label="*p* = 1.1e-8" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/Pa14_Tn_LB_spent_media_culture.pdf", height = 8, width = 16, unit = "cm")

all_tn_abs_data %>%
  filter(Abs == 695) %>%
  ggplot(aes(strain, median)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2,
             color = "#000080",
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_abs),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  scale_x_discrete(limits = c("JV450", "JV494","JV486", "JV499"),
                   labels = c("PA14", bquote("PA14::himar::"*italic(rhlR)),
                              bquote("PA14::himar::"*italic(psqA)), bquote("PA14::himar::"*italic(tpiA)))) +
  labs(x = NULL, y = bquote("Abs"[695])) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"))
ggsave("graphs/Pa14_Tn_Abs695.pdf", height = 8, width = 8, unit = "cm")
