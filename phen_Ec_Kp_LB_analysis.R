library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(DescTools)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#===================================================================
# import data ##################################
combined_phen_Ec_growth<-read_csv("combined_phen_Ec_competition.csv") %>%
  select(-1)
#===================================================================
# define aesthetics ##################################
spent_culture_y_lim = c(-5,6.1)
spent_culture_y_breaks = c(-4,-2, 0, 2, 4, 6)
spent_culture_y_labels = c(-4,-2, 0, 2, 4, 6)

phen_Ec_spent_culture_levels<-c("JV10_LB","JV10_ss",
                                "JV10_JV531_sp","JV10_JV534_sp","JV10_JV537_sp", "JV10_JV540_sp")
phen_Ec_spent_culture_labels<-c("Fresh media","13F11 spent media",
                                "MG1655pEmpty<br>spent media","MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")
phen_Ec_spent_culture_shapes<-c(21,22,22,22,22,22)
phen_Ec_spent_culture_colors<-c("grey", "grey",
                                "#FF7900", "#E3AC36", "#990000", "#00CEC8")
#===================================================================
# plot spent media culture ##################################
# select mouse T24 spent media culture data
combined_phen_Ec_growth_data <-combined_phen_Ec_growth %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Phen_Ec") %>%
  filter(Timepoint_hr == "T24") %>%
  mutate(LOD = case_when(CFU_per_mL_total == 199 ~ "Y",
                         TRUE ~ "N"))
# run stats
phen_Ec_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_growth_data))
# plot data
ggplot(combined_phen_Ec_growth_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_x_discrete(limits = phen_Ec_spent_culture_levels,
                   labels = phen_Ec_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = phen_Ec_spent_culture_levels, 
                     labels = phen_Ec_spent_culture_labels,
                     values = phen_Ec_spent_culture_colors) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = phen_Ec_spent_culture_levels, 
                     labels = phen_Ec_spent_culture_labels,
                     values = phen_Ec_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4), yend = c(4), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.5), fill = NA, label.color = NA, label="*p* = 1.4e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=3.8), fill = NA, label.color = NA, label="*p* = 0.68" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=4.2), fill = NA, label.color = NA, label="*p* = 0.23" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=-3), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6, y=2.4), fill = NA, label.color = NA, label="*p* = 4.6e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/phen_Ec_LB_spent_culture.pdf", height = 8, width = 8, unit = "cm")

# anerobic assay
aner_culture_data <- combined_phen_Ec_growth %>%
  filter(competition == "Phen_Ec_ane") %>%
  filter(Timepoint_hr == "T24")
# run stats
aner_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = aner_culture_data))
# plot data
ggplot(aner_culture_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_x_discrete(limits = phen_Ec_spent_culture_levels,
                   labels = phen_Ec_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = phen_Ec_spent_culture_levels, 
                    labels = phen_Ec_spent_culture_labels,
                    values = phen_Ec_spent_culture_colors) +
  scale_shape_manual(breaks = phen_Ec_spent_culture_levels, 
                     labels = phen_Ec_spent_culture_labels,
                     values = phen_Ec_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_Ec_LB_spent_aner_culture.pdf", height = 8, width = 8, unit = "cm")
#===================================================================
# plot spent media DTT culture ##################################
# select mouse T24 spent media culture data
combined_phen_Ec_DTT_data <-combined_phen_Ec_growth %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "DTT") %>%
  filter(Timepoint_hr == "T24") %>%
  group_by(trial, culture_conditions) %>%
  mutate(log_fold_change_no_DTT = log(fold_change/(fold_change[1]), 10)) %>%
  ungroup() %>%
  group_by(Condition) %>%
  mutate(mean_log_fold_change_no_DTT = mean(log_fold_change_no_DTT)) %>%
  ungroup() %>%
  filter(Condition != "JV537_sp_DTT_50" & Condition != "LB_DTT_50")
# run stats
DTT_stats_537_data<-combined_phen_Ec_DTT_data %>%
  filter(Condition == "JV537_sp" | Condition == "JV537_sp_DTT_0.5" | Condition == "JV537_sp_DTT_5" )
DTT_stats_LB_data<-combined_phen_Ec_DTT_data %>%
  filter(Condition == "LB" | Condition == "LB_DTT_0.5" | Condition == "LB_DTT_5")

phen_Ec_spent_culture_537_DTT_aov <- TukeyHSD(aov(log_fold_change_no_DTT ~ Condition, data = DTT_stats_537_data))
phen_Ec_spent_culture_LB_DTT_aov <- TukeyHSD(aov(log_fold_change_no_DTT ~ Condition, data = DTT_stats_LB_data))
# plot data
ggplot(combined_phen_Ec_DTT_data, aes(x=Condition, y=log_fold_change_no_DTT, fill = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change_no_DTT, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log[10]*"("*Fold~change~from~no~DTT*")")) +
  scale_x_discrete(limits = c("LB", "LB_DTT_0.5","LB_DTT_5", "JV537_sp", "JV537_sp_DTT_0.5","JV537_sp_DTT_5"),
                   labels = c("Fresh media", "Fresh media<br>+ 0.5 mM DTT",
                              "Fresh media<br>+ 5 mM DTT",
                              "MG1655pPhzA-GS*M (PYR)<br>spent media", "MG1655pPhzA-GS*M (PYR)<br>spent media<br>+ 0.5 mM DTT",
                              "MG1655pPhzA-GS*M (PYR)<br>spent media<br>+ 5 mM DTT")) +
  scale_fill_manual(breaks = c("LB", "JV537_sp","LB_DTT_0.5", "JV537_sp_DTT_0.5","LB_DTT_5", "JV537_sp_DTT_5"),
                    values = c("black", "#990000", "#333333", "#e00000","#666666", "#ff2929")) +
  scale_shape_manual(breaks = c("LB", "JV537_sp","LB_DTT_0.5", "JV537_sp_DTT_0.5","LB_DTT_5", "JV537_sp_DTT_5"),
                     values = c(16,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1,4,4), xend = c(2,3,5,6), 
          y = c(0.5,0.75,0.5,0.75), yend = c(0.5,0.75,0.5,0.75), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=0.6), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=0.85), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=0.6), fill = NA, label.color = NA, label="*p* = 0.21" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=0.85), fill = NA, label.color = NA, label="*p* = 0.005" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/5MPCA_DTT_LB_spent_culture.pdf", height = 10, width = 11, unit = "cm")
#===================================================================
# plot spent media Ec culture diversity panel ##################################
# select T24 spent media culture data
combined_phen_Ec_div_data <-combined_phen_Ec_growth %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Phen_Ec_div") %>%
  filter(Timepoint_hr == "T24") %>%
  group_by(Condition) %>%
  mutate(mean_log_fold_change = mean(log_fold_change)) %>%
  ungroup() %>%
  mutate(LOD = case_when(CFU_per_mL_total == 199 ~ "Y",
                         TRUE ~ "N")) %>%
  mutate(strain = str_remove(Condition, "fresh_LB_"),
         strain = str_remove(strain, "JV450_sp_"),
         strain = str_remove(strain, "JV531_sp_"),
         strain = str_remove(strain, "JV534_sp_"),
         strain = str_remove(strain, "JV537_sp_"),
         strain = str_remove(strain, "JV540_sp_"))
# run stats
combined_phen_Ec_div_KPPR1_data<-combined_phen_Ec_div_data %>%
  filter(strain == "KPPR1")
phen_Ec_div_KPPR1_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_KPPR1_data))
combined_phen_Ec_div_JV282_data<-combined_phen_Ec_div_data %>%
  filter(strain == "JV282")
phen_Ec_div_JV282_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_JV282_data))
combined_phen_Ec_div_JV543_data<-combined_phen_Ec_div_data %>%
  filter(strain == "JV543")
phen_Ec_div_JV543_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_JV543_data))
combined_phen_Ec_div_UTI89_data<-combined_phen_Ec_div_data %>%
  filter(strain == "UTI89")
phen_Ec_div_UTI89_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_UTI89_data))
combined_phen_Ec_div_CFT073_data<-combined_phen_Ec_div_data %>%
  filter(strain == "CFT073")
phen_Ec_div_CFT073_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_CFT073_data))
combined_phen_Ec_div_JV25_data<-combined_phen_Ec_div_data %>%
  filter(strain == "JV25")
phen_Ec_div_JV25_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = combined_phen_Ec_div_JV25_data))
# plot data
# KPPR1
ggplot(combined_phen_Ec_div_KPPR1_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("K. pneumoniae")~"KPPR1")) +
  scale_x_discrete(limits = c("fresh_LB_KPPR1", "JV450_sp_KPPR1", "JV531_sp_KPPR1",
                              "JV534_sp_KPPR1","JV537_sp_KPPR1", "JV540_sp_KPPR1"),
                   labels = c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                              "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_KPPR1", "JV450_sp_KPPR1", "JV531_sp_KPPR1",
                               "JV534_sp_KPPR1","JV537_sp_KPPR1", "JV540_sp_KPPR1"), 
                    values = c("black", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_KPPR1", "JV450_sp_KPPR1", "JV531_sp_KPPR1",
                                "JV534_sp_KPPR1","JV537_sp_KPPR1", "JV540_sp_KPPR1"), 
                     values = c(21,22,22,
                                22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave("graphs/phen_Ec_LB_spent_culture_div_KPPR1.pdf", height = 8, width = 10, unit = "cm")

# JV282
ggplot(combined_phen_Ec_div_JV282_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("K. oxytoca")~"JV282")) +
  scale_x_discrete(limits = c("fresh_LB_JV282", "JV450_sp_JV282", "JV531_sp_JV282",
                              "JV534_sp_JV282","JV537_sp_JV282", "JV540_sp_JV282"),
                   labels = c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                              "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_JV282", "JV450_sp_JV282", "JV531_sp_JV282",
                               "JV534_sp_JV282","JV537_sp_JV282", "JV540_sp_JV282"), 
                    values = c("grey", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_JV282", "JV450_sp_JV282", "JV531_sp_JV282",
                                "JV534_sp_JV282","JV537_sp_JV282", "JV540_sp_JV282"), 
                     values = c(21,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,3,3,3), xend = c(2,4,5,6), 
           y = c(5,4.3,5,5.7), yend = c(5,4.3,5,5.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.35), fill = NA, label.color = NA, label="*p* = 0.017" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.65), fill = NA, label.color = NA, label="*p* = 0.034" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=5.35), fill = NA, label.color = NA, label="*p* = 1.1e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=6.05), fill = NA, label.color = NA, label="*p* = 4.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phen_Ec_LB_spent_culture_div_JV282.pdf", height = 10, width = 10, unit = "cm")

# JV543
ggplot(combined_phen_Ec_div_JV543_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("K. oxytoca")~"JV543")) +
  scale_x_discrete(limits = c("fresh_LB_JV543", "JV450_sp_JV543", "JV531_sp_JV543",
                              "JV534_sp_JV543","JV537_sp_JV543", "JV540_sp_JV543"),
                   labels = c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                              "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_JV543", "JV450_sp_JV543", "JV531_sp_JV543",
                               "JV534_sp_JV543","JV537_sp_JV543", "JV540_sp_JV543"), 
                    values = c("grey", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_JV543", "JV450_sp_JV543", "JV531_sp_JV543",
                                "JV534_sp_JV543","JV537_sp_JV543", "JV540_sp_JV543"), 
                     values = c(21,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))  +
  annotate("segment", x = c(1,3,3,3), xend = c(2,4,5,6), 
           y = c(5,4.3,5,5.7), yend = c(5,4.3,5,5.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.35), fill = NA, label.color = NA, label="*p* = 6.5e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.65), fill = NA, label.color = NA, label="*p* = 0.71" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=5.35), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=6.05), fill = NA, label.color = NA, label="*p* = 0.012" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phen_Ec_LB_spent_culture_div_JV543.pdf", height = 10, width = 10, unit = "cm")

# UTI89
ggplot(combined_phen_Ec_div_UTI89_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("E. coli")~"UTI89")) +
  scale_x_discrete(limits = c("fresh_LB_UTI89", "JV450_sp_UTI89", "JV531_sp_UTI89",
                              "JV534_sp_UTI89","JV537_sp_UTI89", "JV540_sp_UTI89"),
                   labels =  c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                               "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_UTI89", "JV450_sp_UTI89", "JV531_sp_UTI89",
                               "JV534_sp_UTI89","JV537_sp_UTI89", "JV540_sp_UTI89"), 
                    values = c("grey", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_UTI89", "JV450_sp_UTI89", "JV531_sp_UTI89",
                                "JV534_sp_UTI89","JV537_sp_UTI89", "JV540_sp_UTI89"), 
                     values = c(21,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,3,3,3), xend = c(2,4,5,6), 
           y = c(5,4.3,5,5.7), yend = c(5,4.3,5,5.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.35), fill = NA, label.color = NA, label="*p* = 2.3e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.65), fill = NA, label.color = NA, label="*p* = 7e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=5.35), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=6.05), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)  
ggsave("graphs/phen_Ec_LB_spent_culture_div_UTI89.pdf", height = 10, width = 10, unit = "cm")

# CFT073
ggplot(combined_phen_Ec_div_CFT073_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("E. coli")~"CFT073")) +
  scale_x_discrete(limits = c("fresh_LB_CFT073", "JV450_sp_CFT073","JV531_sp_CFT073",
                              "JV534_sp_CFT073","JV537_sp_CFT073", "JV540_sp_CFT073"),
                   labels = c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                              "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_CFT073", "JV450_sp_CFT073","JV531_sp_CFT073",
                               "JV534_sp_CFT073","JV537_sp_CFT073", "JV540_sp_CFT073"), 
                    values = c("grey", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_CFT073", "JV450_sp_CFT073","JV531_sp_CFT073",
                                "JV534_sp_CFT073","JV537_sp_CFT073", "JV540_sp_CFT073"), 
                     values = c(21,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))  +
  annotate("segment", x = c(1,3,3,3), xend = c(2,4,5,6), 
           y = c(5,4.3,5,5.7), yend = c(5,4.3,5,5.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.35), fill = NA, label.color = NA, label="*p* = 1.3e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.65), fill = NA, label.color = NA, label="*p* = 1.3e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=5.35), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=6.05), fill = NA, label.color = NA, label="*p* = 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phen_Ec_LB_spent_culture_div_CFT073.pdf", height = 10, width = 10, unit = "cm")

# JV25
ggplot(combined_phen_Ec_div_JV25_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"]), title = bquote(italic("E. faecalis")~"JV25")) +
  scale_x_discrete(limits = c("fresh_LB_JV25", "JV450_sp_JV25", "JV531_sp_JV25",
                              "JV534_sp_JV25","JV537_sp_JV25", "JV540_sp_JV25"),
                   labels = c("Fresh media", "PA14 spent media", "MG1655pEmpty<br>spent media",
                              "MG1655pPhzA-G<br>spent media (PCA)","MG1655pPhzA-GS*M<br>spent media (PYR)", "MG1655pPhzA-GSM<br>spent media (PYO)")) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_fill_manual(breaks = c("fresh_LB_JV25", "JV450_sp_JV25", "JV531_sp_JV25",
                               "JV534_sp_JV25","JV537_sp_JV25", "JV540_sp_JV25"), 
                    values = c("grey", "#000080", "#FF7900", "#E3AC36", "#990000", "#00CEC8")) +
  scale_color_manual(breaks = c("Y", "N"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("fresh_LB_JV25", "JV450_sp_JV25", "JV531_sp_JV25",
                                "JV534_sp_JV25","JV537_sp_JV25", "JV540_sp_JV25"), 
                     values = c(21,22,22,22,22,22)) +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,3,3,3), xend = c(2,4,5,6), 
           y = c(5,4.3,5,5.7), yend = c(5,4.3,5,5.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.35), fill = NA, label.color = NA, label="*p* = 0.086" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.65), fill = NA, label.color = NA, label="*p* = 0.031" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=5.35), fill = NA, label.color = NA, label="*p* = 0.22" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=6.05), fill = NA, label.color = NA, label="*p* = 0.98" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phen_Ec_LB_spent_culture_div_JV25.pdf", height = 10, width = 10, unit = "cm")