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
combined_glu_competition<-read_csv("combined_glu_competition.csv") %>%
  select(-1)
#===================================================================
# define aesthetics ##################################
co_culture_y_lim = c(0,7)
co_culture_y_breaks = c(0, 2, 4, 6)
co_culture_y_labels = c(0, 2, 4, 6)

spent_culture_y_lim = c(-2,6)
spent_culture_y_breaks = c(-2, 0, 2, 4, 6)
spent_culture_y_labels = c(-2, 0, 2, 4, 6)

mouse_spent_culture_comp_levels<-c("KPPR1_fresh_lb",
                                   "KPPR1_self_spent_water","KPPR1_self_spent_glu",
                                   "KPPR1_spent_jv3_water","KPPR1_spent_jv3_glu",
                                   "KPPR1_spent_jv4_water","KPPR1_spent_jv4_glu",
                                   "KPPR1_spent_jv5_water","KPPR1_spent_jv5_glu")
mouse_spent_culture_comp_labels<-c("Fresh media",
                                   "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 0.4% Glu",
                                   "145.1 spent<br>media + water", "145.1 spent<br>media + 0.4% Glu",
                                   "191.1 spent<br>media + water","191.1 spent<br>media + 0.4% Glu",
                                   "193.1 spent<br>media + water","193.1 spent<br>media + 0.4% Glu")
mouse_spent_culture_comp_shapes<-c(16,15,17,15,17,15,17,15,17)
mouse_spent_culture_comp_colors<-c("black", 
                                   "black", "black",
                                   "#0147AB", "#0147AB",
                                   "#008EEC", "#008EEC", 
                                   "#42E0D1", "#42E0D1")

lab_spent_culture_comp_levels<-c("KPPR1_fresh_lb",
                                   "KPPR1_self_spent_water","KPPR1_self_spent_glu",
                                   "KPPR1_spent_Pa01_water","KPPR1_spent_Pa01_glu",
                                   "KPPR1_spent_Pa14_water","KPPR1_spent_Pa14_glu")
lab_spent_culture_comp_labels<-c("Fresh media",
                                   "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 0.4% Glu",
                                   "PAO1 spent<br>media + water", "PAO1 spent<br>media + 0.4% Glu",
                                   "PA14 spent<br>media + water","PA14 spent<br>media + 0.4% Glu")
lab_spent_culture_comp_shapes<-c(16,15,17,15,17,15,17)
lab_spent_culture_comp_colors<-c("black", 
                                   "black", "black",
                                   "#28AB87", "#28AB87",
                                   "#000080", "#000080")

rhl_spent_culture_comp_levels<-c("KPPR1_fresh_lb",
                                   "KPPR1_self_spent_water","KPPR1_self_spent_glu",
                                   "KPPR1_spent_Pa14_water","KPPR1_spent_Pa14_glu",
                                   "KPPR1_spent_rhlR_water","KPPR1_spent_rhlR_glu",
                                   "KPPR1_spent_rhlI_water","KPPR1_spent_rhlI_glu")
rhl_spent_culture_comp_labels<-c("Fresh media",
                                   "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 0.4% Glu",
                                   "PA14 spent<br>media + water", "PA14 spent<br>media + 0.4% Glu",
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlR* spent", sep = ""), "media + water", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlR* spent", sep = ""), "media + 0.4% Glu", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlI* spent", sep = ""), "media + water", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlI* spent", sep = ""), "media + 0.4% Glu", sep ="<br>"))
rhl_spent_culture_comp_shapes<-c(16,15,17,15,17,0,2,0,2)
rhl_spent_culture_comp_colors<-c("black",
                                 "black", "black",
                                 "#000080", "#000080",
                                 "#000080", "#000080",
                                 "#000080", "#000080")
#===================================================================
# plot co-culture ##################################
# select mouse T24 comp spent media data
mouse_spent_culture_comp_data <- combined_glu_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "mouse_Pa_Kp_glu_comp") %>%
  filter(Timepoint_hr == "T24")
# run stats
mouse_spent_culture_comp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = mouse_spent_culture_comp_data))
# plot data
ggplot(mouse_spent_culture_comp_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_x_discrete(limits = mouse_spent_culture_comp_levels,
                   labels = mouse_spent_culture_comp_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = mouse_spent_culture_comp_levels, 
                     labels = mouse_spent_culture_comp_labels,
                     values = mouse_spent_culture_comp_colors) +
  scale_shape_manual(breaks = mouse_spent_culture_comp_levels, 
                     labels = mouse_spent_culture_comp_labels,
                     values = mouse_spent_culture_comp_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(2,4,6,8), xend = c(3,5,7,9), 
           y = c(5.7, 3.7, 1.4, 2.1), yend = c(5.7, 3.7, 1.4, 2.1), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=6), fill = NA, label.color = NA, label="*p* = 2.1e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=4), fill = NA, label.color = NA, label="*p* = 0.25" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=1.7), fill = NA, label.color = NA, label="*p* = 0.98" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8.5, y=2.4), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=5.4), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.3), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=0.7), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=1.5), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_glu_spent_comp_culture.pdf", height = 8, width = 11, unit = "cm")

# select lab T24 comp spent media data
lab_spent_culture_comp_data <- combined_glu_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "lab_Pa_Kp_glu_comp") %>%
  filter(Timepoint_hr == "T24")
# run stats
lab_spent_culture_comp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = lab_spent_culture_comp_data))
# plot data
ggplot(lab_spent_culture_comp_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_x_discrete(limits = lab_spent_culture_comp_levels,
                   labels = lab_spent_culture_comp_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = lab_spent_culture_comp_levels, 
                     labels = lab_spent_culture_comp_labels,
                     values = lab_spent_culture_comp_colors) +
  scale_shape_manual(breaks = lab_spent_culture_comp_levels, 
                     labels = lab_spent_culture_comp_labels,
                     values = lab_spent_culture_comp_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(2,4,6), xend = c(3,5,7), 
           y = c(5.5, 4.1, 3.5), yend = c(5.5, 4.1, 3.5), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=5.8), fill = NA, label.color = NA, label="*p* < 8e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=4.4), fill = NA, label.color = NA, label="*p* = 2.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=3.8), fill = NA, label.color = NA, label="*p* = 1.8e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=5.1), fill = NA, label.color = NA, label="*p* = 0.6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.6), fill = NA, label.color = NA, label="*p* = 3.7e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=3), fill = NA, label.color = NA, label="*p* = 1.3e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_glu_spent_comp_culture.pdf", height = 8, width = 9, unit = "cm")

# select rhl T24 comp spent media data
rhl_spent_glu_comp_data <- combined_glu_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "rhl_glu_comp") %>%
  filter(Timepoint_hr == "T24")
# run stats
rhl_spent_glu_comp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = rhl_spent_glu_comp_data))
# plot data
ggplot(rhl_spent_glu_comp_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_x_discrete(limits = rhl_spent_culture_comp_levels,
                   labels = rhl_spent_culture_comp_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = rhl_spent_culture_comp_levels, 
                     labels = rhl_spent_culture_comp_labels,
                     values = rhl_spent_culture_comp_colors) +
  scale_shape_manual(breaks = rhl_spent_culture_comp_levels, 
                     labels = rhl_spent_culture_comp_labels,
                     values = rhl_spent_culture_comp_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(2,4,6,8), xend = c(3,5,7,9), 
           y = c(5.5,3.7,5.5,5.5), yend = c(5.5,3.7,5.5,5.5), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=5.8), fill = NA, label.color = NA, label="*p* = 1.7e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=4), fill = NA, label.color = NA, label="*p* = 6.3e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=5.8), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8.5, y=5.8), fill = NA, label.color = NA, label="*p* = 7.1e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=5.1), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.2), fill = NA, label.color = NA, label="*p* = 1.9e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=4.7), fill = NA, label.color = NA, label="*p* = 0.67" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=5), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/rhl_glu_spent_comp_culture.pdf", height = 8, width = 11, unit = "cm")
