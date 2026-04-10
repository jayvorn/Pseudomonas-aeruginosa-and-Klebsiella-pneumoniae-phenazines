library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(DescTools)
library(svglite)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#===================================================================
# import data ##################################
combined_cas_competition<-read_csv("combined_cas_competition.csv") %>%
  select(-1)
combined_cas_growth_curves<-read_csv("combined_cas_growth_curves.csv") %>%
  select(-1)
#===================================================================
# define aesthetics ##################################
co_culture_y_lim = c(0,7)
co_culture_y_breaks = c(0, 2, 4, 6)
co_culture_y_labels = c(0, 2, 4, 6)

spent_culture_y_lim = c(-2,6.2)
spent_culture_y_breaks = c(-2, 0, 2, 4, 6)
spent_culture_y_labels = c(-2, 0, 2, 4, 6)

growth_x_limits = c(0, 24)
growth_x_breaks = c(0,6,12,18,24)
growth_x_labels = c(0,6,12,18,24)

growth_y_limits = c(0.001, 10)
growth_y_breaks = c(0.001, 0.01, 0.1, 1, 10)
growth_y_labels = c(0.001, 0.01, 0.1, 1, 10)

mouse_co_culture_levels<-c("Fresh_Cas","KPPR1_145.1","KPPR1_191.1","KPPR1_193.1")
mouse_co_culture_labels<-c("KPPR1 alone","KPPR1 + 145.1","KPPR1 + 191.1","KPPR1 + 193.1")
mouse_co_culture_shapes<-c(16,16,16,16)
mouse_co_culture_colors<-c("black", "#0147AB", "#008EEC", "#42E0D1")

mouse_spent_culture_levels<-c("Fresh_Cas","Self_Spent_Cas","145.1_Spent_Cas","191.1_Spent_Cas","193.1_Spent_Cas")
mouse_spent_culture_labels<-c("Fresh media","KPPR1 spent media","145.1 spent media","191.1 spent media","193.1 spent media")
mouse_spent_culture_shapes<-c(16,15,15,15,15)
mouse_spent_culture_colors<-c("black", "black","#0147AB", "#008EEC", "#42E0D1")

mouse_spent_culture_comp_levels<-c("KPPR1_fresh_LB",
                                   "KPPR1_self_spent_water","KPPR1_self_spent_cas",
                                   "145.1_spent_water","145.1_spent_cas",
                                   "191.1_spent_water","191.1_spent_cas",
                                   "193.1_spent_water","193.1_spent_cas")
mouse_spent_culture_comp_labels<-c("Fresh media",
                                   "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 1.0% Cas",
                                   "145.1 spent<br>media + water", "145.1 spent<br>media + 1.0% Cas",
                                   "191.1 spent<br>media + water","191.1 spent<br>media + 1.0% Cas",
                                   "193.1 spent<br>media + water","193.1 spent<br>media + 1.0% Cas")
mouse_spent_culture_comp_shapes<-c(16,15,17,15,17,15,17,15,17)
mouse_spent_culture_comp_colors<-c("black", 
                                   "black", "black",
                                   "#0147AB", "#0147AB",
                                   "#008EEC", "#008EEC", 
                                   "#42E0D1", "#42E0D1")

lab_co_culture_levels<-c("KPPR1_fresh_cas","KPPR1_PA01","KPPR1_Pa14")
lab_co_culture_labels<-c("KPPR1 alone","KPPR1 + PAO1","KPPR1 + PA14")
lab_co_culture_shapes<-c(16,16,16)
lab_co_culture_colors<-c("black", "#28AB87", "#000080")

lab_spent_culture_levels<-c("KPPR1_fresh_cas", "KPPR1_spent_cas","KPPR1_pa01_spent_cas","KPPR1_pa14_spent_cas")
lab_spent_culture_labels<-c("Fresh media","KPPR1 spent media","PAO1 spent media","PA14 spent media")
lab_spent_culture_shapes<-c(16,15,15,15)
lab_spent_culture_colors<-c("black", "black", "#28AB87", "#000080")

lab_spent_culture_comp_levels<-c("KPPR1_fresh_lb",
                                 "KPPR1_self_spent_water","KPPR1_self_spent_cas",
                                 "KPPR1_spent_Pa01_water","KPPR1_spent_Pa01_cas",
                                 "KPPR1_spent_Pa14_water","KPPR1_spent_Pa14_cas")
lab_spent_culture_comp_labels<-c("Fresh media",
                                   "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 1.0% Cas",
                                   "PAO1 spent<br>media + water", "PAO1 spent<br>media + 1.0% Cas",
                                   "PA14 spent<br>media + water","PA14 spent<br>media + 1.0% Cas")
lab_spent_culture_comp_shapes<-c(16,15,17,15,17,15,17)
lab_spent_culture_comp_colors<-c("black",
                                 "black", "black",
                                 "#28AB87", "#28AB87",
                                 "#000080", "#000080")

MG1655_co_culture_levels<-c("KPPR1_fresh_cas","KPPR1_MG1655_cas")
MG1655_co_culture_labels<-c("KPPR1 alone","KPPR1 + MG1655")
MG1655_co_culture_shapes<-c(16,16)
MG1655_co_culture_colors<-c("black", "#FF7900")

MG1655_spent_culture_levels<-c("KPPR1_fresh_cas", "KPPR1_spent_cas","KPPR1_mg1655_spent_cas")
MG1655_spent_culture_labels<-c("Fresh media","KPPR1 spent media","MG1655 spent media")
MG1655_spent_culture_shapes<-c(16,15,15)
MG1655_spent_culture_colors<-c("black", "black", "#FF7900")

Tn13F11_co_culture_levels<-c("KPPR1_fresh_cas","KPPR1_13F11_cas")
Tn13F11_co_culture_labels<-c("KPPR1 alone","KPPR1 + 13F11")
Tn13F11_co_culture_shapes<-c(16,16)
Tn13F11_co_culture_colors<-c("black", "grey")

Tn13F11_spent_culture_levels<-c("KPPR1_fresh_cas", "KPPR1_spent_cas","KPPR1_13f11_spent_cas")
Tn13F11_spent_culture_labels<-c("Fresh media","KPPR1 spent media","13F11 spent media")
Tn13F11_spent_culture_shapes<-c(16,15,15)
Tn13F11_spent_culture_colors<-c("black", "black", "grey")

rhl_co_culture_levels<-c("KPPR1_fresh_cas","KPPR1_pa14_cas","KPPR1_rhlr_cas","KPPR1_rhli_cas")
rhl_co_culture_labels<-c("KPPR1 alone","KPPR1 + PA14",bquote("KPPR1 + PA14"*Delta*italic("rhlR")),bquote("KPPR1 + PA14"*Delta*italic("rhlI")))
rhl_co_culture_shapes<-c(16,16,1,2)
rhl_co_culture_colors<-c("black", "#000080", "#000080","#000080")

rhl_spent_culture_levels<-c("KPPR1_fresh_lb","KPPR1_self_spent_cas","KPPR1_spent_Pa14_cas","KPPR1_spent_rhlR_cas","KPPR1_spent_rhlI_cas")
rhl_spent_culture_labels<-c("KPPR1 alone","KPPR1 spent media","PA14 spent media",bquote("PA14"*Delta*italic("rhlR")~"spent media"),bquote("PA14"*Delta*italic("rhlI")~"spent media"))
rhl_spent_culture_shapes<-c(16,15,15,1,2)
rhl_spent_culture_colors<-c("black", "black", "#000080", "#000080","#000080")

rhl_spent_culture_comp_levels<-c("KPPR1_fresh_lb",
                                 "KPPR1_self_spent_water","KPPR1_self_spent_cas",
                                 "KPPR1_spent_Pa14_water", "KPPR1_spent_Pa14_cas",
                                 "KPPR1_spent_rhlR_water","KPPR1_spent_rhlR_cas",
                                 "KPPR1_spent_rhlI_water","KPPR1_spent_rhlI_cas")
rhlr_text<-bquote("Pa14"*Delta*italic("rhlR")~"spent")
rhl_spent_culture_comp_labels<-c("Fresh media",
                                 "KPPR1 spent<br>media + water", "KPPR1 spent<br>media + 1.0% Cas",
                                 "PA14 spent<br>media + water", "PA14 spent<br>media + 1.0% Cas",
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlR* spent", sep = ""), "media + water", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlR* spent", sep = ""), "media + 1.0% Cas", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlI* spent", sep = ""), "media + water", sep ="<br>"),
                                 paste(paste(paste("PA14", "D", sep = ""), "*rhlI* spent", sep = ""), "media + 1.0% Cas", sep ="<br>"))
rhl_spent_culture_comp_shapes<-c(16,15,17,15,17,0,2,0,2)
rhl_spent_culture_comp_colors<-c("black",
                                 "black", "black",
                                 "#000080", "#000080",
                                 "#000080", "#000080",
                                 "#000080", "#000080")

mouse_growth_levels<-c("KPPR1","145.1","191.1","193.1")
mouse_growth_labels<-c("KPPR1","145.1","191.1","193.1")
mouse_growth_shapes<-c(16,16,16,16)
mouse_growth_colors<-c("black", "#0147AB", "#008EEC", "#42E0D1")

lab_growth_levels<-c("KPPR1","PA01","PA14")
lab_growth_labels<-c("KPPR1","PAO1","PA14")
lab_growth_shapes<-c(16,16,16)
lab_growth_colors<-c("black","#28AB87", "#000080")

mg1655_growth_levels<-c("KPPR1","MG1655")
mg1655_growth_labels<-c("KPPR1","MG1655")
mg1655_growth_shapes<-c(16,16)
mg1655_growth_colors<-c("black","#FF7900")

Tn13F11_growth_levels<-c("KPPR1","13F11")
Tn13F11_growth_labels<-c("KPPR1","13F11")
Tn13F11_growth_shapes<-c(16,16)
Tn13F11_growth_colors<-c("black","grey")

rhl_growth_levels<-c("KPPR1","PA14", "RHLR", "RHLI")
rhl_growth_labels<-c("KPPR1","PA14",bquote("PA14"*Delta*italic("rhlR")),bquote("PA14"*Delta*italic("rhlI")))
rhl_growth_shapes<-c(16,16,1,2)
rhl_growth_colors<-c("black","#000080","#000080","#000080")
#===================================================================
# plot co-culture ##################################
# select mouse T24 co-culture data
mouse_co_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "Mouse_Pa_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
mouse_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = mouse_co_culture_data))
# plot data
ggplot(mouse_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = mouse_co_culture_levels,
                   labels = mouse_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = mouse_co_culture_levels, 
                     labels = mouse_co_culture_labels,
                     values = mouse_co_culture_colors) +
  scale_shape_manual(breaks = mouse_co_culture_levels, 
                     labels = mouse_co_culture_labels,
                     values = mouse_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), 
           y = c(5.7, 6.2, 6.7), yend = c(5.7, 6.2, 6.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.9), fill = NA, label.color = NA, label="*p* = 3.6e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=6.4), fill = NA, label.color = NA, label="*p* = 2.8e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=6.9), fill = NA, label.color = NA, label="*p* = 3.3e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_cas_co_culture.pdf", height = 8, width = 4, unit = "cm")

# select lab T24 co-culture data
lab_co_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "lab_Pa") %>%
  filter(Timepoint_hr == "T24")
# run stats
lab_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = lab_co_culture_data))
# plot data
ggplot(lab_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = lab_co_culture_levels,
                   labels = lab_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = lab_co_culture_levels, 
                     labels = lab_co_culture_labels,
                     values = lab_co_culture_colors) +
  scale_shape_manual(breaks = lab_co_culture_levels, 
                     labels = lab_co_culture_labels,
                     values = lab_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1), xend = c(2,3), 
           y = c(5,5.5), yend = c(5,5.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.2), fill = NA, label.color = NA, label="*p* = 0.06" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=5.7), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_cas_co_culture.pdf", height = 8, width = 3.5, unit = "cm")

# select MG1655 T24 co-culture data
MG1655_co_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "MG1655_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
MG1655_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = MG1655_co_culture_data))
# plot data
ggplot(MG1655_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = MG1655_co_culture_levels,
                   labels = MG1655_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = MG1655_co_culture_levels, 
                     labels = MG1655_co_culture_labels,
                     values = MG1655_co_culture_colors) +
  scale_shape_manual(breaks = MG1655_co_culture_levels, 
                     labels = MG1655_co_culture_labels,
                     values = MG1655_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(5), yend = c(5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.3), fill = NA, label.color = NA, label="*p* = 0.39" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/MG1655_cas_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select Tn13F11 T24 co-culture data
Tn13F11_co_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "Tn13F11_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
Tn13F11_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = Tn13F11_co_culture_data))
# plot data
ggplot(Tn13F11_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = Tn13F11_co_culture_levels,
                   labels = Tn13F11_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = Tn13F11_co_culture_levels, 
                     labels = Tn13F11_co_culture_labels,
                     values = Tn13F11_co_culture_colors) +
  scale_shape_manual(breaks = Tn13F11_co_culture_levels, 
                     labels = Tn13F11_co_culture_labels,
                     values = Tn13F11_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(5.8), yend = c(5.8), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=6.2), fill = NA, label.color = NA, label="*p* = 0.5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Tn13F11_cas_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select rhlR T24 co-culture data
rhl_co_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "rhl_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
rhl_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = rhl_co_culture_data))
# plot data
ggplot(rhl_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = rhl_co_culture_levels,
                   labels = rhl_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = rhl_co_culture_levels, 
                     labels = rhl_co_culture_labels,
                     values = rhl_co_culture_colors) +
  scale_shape_manual(breaks = rhl_co_culture_levels, 
                     labels = rhl_co_culture_labels,
                     values = rhl_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1,1,2,2), xend = c(2,3,4,3,4), 
           y = c(4.6, 5.2, 5.8, 4, 0.5), yend = c(4.6, 5.2, 5.8, 4, 0.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.8), fill = NA, label.color = NA, label="*p* = 9.5e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=5.4), fill = NA, label.color = NA, label="*p* = 6.9e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=6), fill = NA, label.color = NA, label="*p* = 0.01" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=4.2), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=0.2), fill = NA, label.color = NA, label="*p* = 9.2e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/rhl_cas_co_culture.pdf", height = 8, width = 4, unit = "cm")
#===================================================================
# plot spent media culture ##################################
# select mouse T24 spent media data
mouse_spent_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Mouse_Pa_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
mouse_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = mouse_spent_culture_data))
# plot data
ggplot(mouse_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = mouse_spent_culture_levels,
                   labels = mouse_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = mouse_spent_culture_levels, 
                     labels = mouse_spent_culture_labels,
                     values = mouse_spent_culture_colors) +
  scale_shape_manual(breaks = mouse_spent_culture_levels, 
                     labels = mouse_spent_culture_labels,
                     values = mouse_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2,2,2), xend = c(2,3,4,5), 
           y = c(5.5, 4.9, 4.2, 3.5), yend = c(5.5, 4.9, 4.2, 3.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.8), fill = NA, label.color = NA, label="*p* = 8.1e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=5.2), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=4.5), fill = NA, label.color = NA, label="*p* = 3.3e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=3.8), fill = NA, label.color = NA, label="*p* = 0.02" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_cas_spent_culture.pdf", height = 8, width = 5, unit = "cm")

# select lab T24 spent data
lab_spent_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "lab_Pa_spent") %>%
  filter(Timepoint_hr == "T24")
# run stats
lab_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = lab_spent_culture_data))
# plot data
ggplot(lab_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = lab_spent_culture_levels,
                   labels = lab_spent_culture_labels) +
  scale_y_continuous(limits = c(-6,6.5),
                     breaks = c(-6,-3,0,3,6),
                     labels = c(-6,-3,0,3,6)) +
  scale_color_manual(breaks = lab_spent_culture_levels, 
                     labels = lab_spent_culture_labels,
                     values = lab_spent_culture_colors) +
  scale_shape_manual(breaks = lab_spent_culture_levels, 
                     labels = lab_spent_culture_labels,
                     values = lab_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2,2), xend = c(2,3,4), 
           y = c(6.1,4.5,3.5), yend = c(6.1,4.5,3.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=6.5), fill = NA, label.color = NA, label="*p* = 4.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=4.9), fill = NA, label.color = NA, label="*p* = 1.5e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=3.9), fill = NA, label.color = NA, label="*p* < 2e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/lab_cas_spent_culture.pdf", height = 8, width = 4, unit = "cm")

# select MG1655 T24 spent data
MG1655_spent_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "MG1655_spent") %>%
  filter(Timepoint_hr == "T24")
# run stats
MG1655_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = MG1655_spent_culture_data))
# plot data
ggplot(MG1655_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = MG1655_spent_culture_levels,
                   labels = MG1655_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = MG1655_spent_culture_levels, 
                     labels = MG1655_spent_culture_labels,
                     values = MG1655_spent_culture_colors) +
  scale_shape_manual(breaks = MG1655_spent_culture_levels, 
                     labels = MG1655_spent_culture_labels,
                     values = MG1655_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2), xend = c(2,3), 
           y = c(5.8,3.5), yend = c(5.8,3.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=6.2), fill = NA, label.color = NA, label="*p* = 4.5e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.9), fill = NA, label.color = NA, label="*p* = 0.81" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/MG1655_cas_spent_culture.pdf", height = 8, width = 3.5, unit = "cm")

# select Tn13F11 T24 spent data
Tn13F11_spent_culture_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Tn13F11_spent") %>%
  filter(Timepoint_hr == "T24")
# run stats
Tn13F11_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = Tn13F11_spent_culture_data))
# plot data
ggplot(Tn13F11_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = Tn13F11_spent_culture_levels,
                   labels = Tn13F11_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = Tn13F11_spent_culture_levels, 
                     labels = Tn13F11_spent_culture_labels,
                     values = Tn13F11_spent_culture_colors) +
  scale_shape_manual(breaks = Tn13F11_spent_culture_levels, 
                     labels = Tn13F11_spent_culture_labels,
                     values = Tn13F11_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2), xend = c(2,3), 
           y = c(5.1,3.5), yend = c(5.1,3.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.5), fill = NA, label.color = NA, label="*p* = 3.1e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.9), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Tn13F11_cas_spent_culture.pdf", height = 8, width = 3.5, unit = "cm")

# select rhlR T24 spent data
rhl_spent_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "rhl_Pa_spent") %>%
  filter(Timepoint_hr == "T24")
# run stats
rhl_spent_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = rhl_spent_data))
# plot data
ggplot(rhl_spent_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = rhl_spent_culture_levels,
                   labels = rhl_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = rhl_spent_culture_levels, 
                     labels = rhl_spent_culture_labels,
                     values = rhl_spent_culture_colors) +
  scale_shape_manual(breaks = rhl_spent_culture_levels, 
                     labels = rhl_spent_culture_labels,
                     values = rhl_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2,3,3), xend = c(2,3,4,5), 
           y = c(5, 3.5, 4.2, 4.9), yend = c(5, 3.5, 4.2, 4.9), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.3), fill = NA, label.color = NA, label="*p* = 4.9e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.8), fill = NA, label.color = NA, label="*p* = 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.5), fill = NA, label.color = NA, label="*p* = 3.6e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) + 
  geom_richtext(data=tibble(x=4, y=5.2), fill = NA, label.color = NA, label="*p* = 6.8e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/rhl_cas_spent_culture.pdf" , height = 8, width = 5, unit = "cm")

# select mouse T24 comp spent media data
mouse_spent_culture_comp_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Mouse_Pa_Kp_cas_comp") %>%
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
           y = c(5.4, 3.7, 3, 3.7), yend = c(5.4, 3.7, 3, 3.7), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=5.7), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=4), fill = NA, label.color = NA, label="*p* = 1.2e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=3.3), fill = NA, label.color = NA, label="*p* = 1.8e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8.5, y=4), fill = NA, label.color = NA, label="*p* = 3.3e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=5), fill = NA, label.color = NA, label="*p* = 0.55" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.4), fill = NA, label.color = NA, label="*p* = 0.03" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=2.6), fill = NA, label.color = NA, label="*p* = 4.2e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=3.3), fill = NA, label.color = NA, label="*p* = 6.3e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_cas_spent_comp_culture.pdf", height = 8, width = 11, unit = "cm")

# select lab T24 comp spent media data
lab_spent_cas_comp_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Lab_Pa_Kp_cas_comp") %>%
  filter(Timepoint_hr == "T24")
# run stats
lab_spent_cas_comp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = lab_spent_cas_comp_data))
# plot data
ggplot(lab_spent_cas_comp_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
           y = c(5.9, 3.9, 2.1), yend = c(5.9, 3.9, 2.1), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=6.2), fill = NA, label.color = NA, label="*p* = 1e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=4.3), fill = NA, label.color = NA, label="*p* = 0.25" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=2.4), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=5.4), fill = NA, label.color = NA, label="*p* = 1.00" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.5), fill = NA, label.color = NA, label="*p* = 6.5e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=1.6), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_cas_spent_comp_culture.pdf", height = 8, width = 9, unit = "cm")

# select rhl T24 comp spent media data
rhl_spent_cas_comp_data <- combined_cas_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "rhl_Kp_cas_comp") %>%
  filter(Timepoint_hr == "T24")
# run stats
rhl_spent_cas_comp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = rhl_spent_cas_comp_data))
# plot data
ggplot(rhl_spent_cas_comp_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
           y = c(5.5,2.5,4,4), yend = c(5.5,2.5,4,4), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=5.8), fill = NA, label.color = NA, label="*p* = 1.5e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4.5, y=2.8), fill = NA, label.color = NA, label="*p* = 0.91" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6.5, y=4.3), fill = NA, label.color = NA, label="*p* = 0.26" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8.5, y=4.3), fill = NA, label.color = NA, label="*p* = 0.86" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=4.8), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=2), fill = NA, label.color = NA, label="*p* <1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=3.5), fill = NA, label.color = NA, label="*p* = 0.74" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=3.5), fill = NA, label.color = NA, label="*p* = 0.53" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/rhl_cas_spent_comp_culture.pdf", height = 8, width = 11, unit = "cm")
#===================================================================
# plot cas growth curves ##################################
mouse_cas_growth_data <-combined_cas_growth_curves %>%
  filter(expt == "Mouse_Pa")

ggplot(mouse_cas_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 8)) 
ggsave("graphs/mouse_cas_growth.pdf", height = 4, width = 8, unit = "cm")

lab_Pa_cas_growth_data <-combined_cas_growth_curves %>%
  filter(expt == "Lab_Pa")

ggplot(lab_Pa_cas_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
                     breaks = lab_growth_levels,
                     labels = lab_growth_labels,
                     values = lab_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = lab_growth_levels,
                     labels = lab_growth_labels,
                     values = lab_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 8)) 
ggsave("graphs/lab_Pa_cas_growth.pdf", height = 4, width = 8, unit = "cm")

MG1655_cas_growth_data <-combined_cas_growth_curves %>%
  filter(expt == "MG1655")

ggplot(MG1655_cas_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
                     breaks = mg1655_growth_levels,
                     labels = mg1655_growth_labels,
                     values = mg1655_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = mg1655_growth_levels,
                     labels = mg1655_growth_labels,
                     values = mg1655_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 8)) 
ggsave("graphs/mg1655_cas_growth.pdf", height = 4, width = 8, unit = "cm")

Tn13F11_cas_growth_data <-combined_cas_growth_curves %>%
  filter(expt == "Tn13F11")

ggplot(Tn13F11_cas_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
                     breaks = Tn13F11_growth_levels,
                     labels = Tn13F11_growth_labels,
                     values = Tn13F11_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = Tn13F11_growth_levels,
                     labels = Tn13F11_growth_labels,
                     values = Tn13F11_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 8)) 
ggsave("graphs/13F11_cas_growth.pdf", height = 4, width = 8, unit = "cm")

rhl_cas_growth_data <-combined_cas_growth_curves %>%
  filter(expt == "rhl_Pa")

ggplot(rhl_cas_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
                     breaks = rhl_growth_levels,
                     labels = rhl_growth_labels,
                     values = rhl_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = rhl_growth_levels,
                     labels = rhl_growth_labels,
                     values = rhl_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_text(size = 8)) 
ggsave("graphs/rhl_cas_growth.pdf", height = 4, width = 8, unit = "cm")
#===================================================================
# plot cas AUC ##################################
# calculate mouse cas AUC 6hr
mouse_cas_growth_trial_1_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_cas_growth_trial_1_AUC<-c(AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mouse_cas_growth_trial_2_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_cas_growth_trial_2_AUC<-c(AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mouse_cas_growth_trial_3_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_cas_growth_trial_3_AUC<-c(AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine mouse cas growth 6hr AUC data
mouse_AUC_6hr<-rbind(mouse_cas_growth_trial_1_AUC, mouse_cas_growth_trial_2_AUC, mouse_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mouse_AUC_6hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mouse_AUC_6hr))

#graph mouse cas growth 6hr AUC data
ggplot(mouse_AUC_6hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-6 hr"])) +
  scale_x_discrete(limits = mouse_growth_levels) +
  scale_y_continuous(limits = c(0, 6),
                     breaks = c(0,2,4,6),
                     labels = c(0,2,4,6)) +
  scale_color_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), y = c(3,3.7,4.4), yend = c(3,3.7,4.4), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=3.3), fill = NA, label.color = NA, label="*p* = 0.51" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=4.0), fill = NA, label.color = NA, label="*p* = 0.57" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=4.7), fill = NA, label.color = NA, label="*p* = 0.89" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/mouse_cas_growth_AUC_6hr.pdf", height = 6, width = 4, unit = "cm")

# calculate mouse cas AUC 24hr
mouse_cas_growth_trial_1_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
mouse_cas_growth_trial_1_AUC<-c(AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_1_AUC_calc$Time, mouse_cas_growth_trial_1_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mouse_cas_growth_trial_2_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mouse_cas_growth_trial_2_AUC<-c(AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_2_AUC_calc$Time, mouse_cas_growth_trial_2_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mouse_cas_growth_trial_3_AUC_calc <- mouse_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mouse_cas_growth_trial_3_AUC<-c(AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$KPPR1),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`145.1`),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`191.1`),
                               AUC(mouse_cas_growth_trial_3_AUC_calc$Time, mouse_cas_growth_trial_3_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine mouse cas growth 24hr AUC data
mouse_AUC_24hr<-rbind(mouse_cas_growth_trial_1_AUC, mouse_cas_growth_trial_2_AUC, mouse_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mouse_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mouse_AUC_24hr))

#graph mouse cas growth 24hr AUC data
ggplot(mouse_AUC_24hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_x_discrete(limits = mouse_growth_levels) +
  scale_y_continuous(limits = c(20, 100),
                     breaks = c(20,40,60,80,100),
                     labels = c(20,40,60,80,100)) +
  scale_color_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = mouse_growth_levels,
                     labels = mouse_growth_labels,
                     values = mouse_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), y = c(85,95,55), yend = c(85,95,55), color = "black") +
  geom_richtext(data=tibble(x=1.52, y=88), fill = NA, label.color = NA, label="*p* = 4.8e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=98), fill = NA, label.color = NA, label="*p* = 1.2e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=51), fill = NA, label.color = NA, label="*p* = 2.2e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/mouse_cas_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")

# calculate lab pa cas AUC 6hr
lab_Pa_cas_growth_data_trial_1_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_cas_growth_trial_1_AUC<-c(AUC(lab_Pa_cas_growth_data_trial_1_AUC_calc$Time, lab_Pa_cas_growth_data_trial_1_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_data_trial_1_AUC_calc$Time, lab_Pa_cas_growth_data_trial_1_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_data_trial_1_AUC_calc$Time, lab_Pa_cas_growth_data_trial_1_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
lab_Pa_cas_growth_data_trial_2_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_cas_growth_trial_2_AUC<-c(AUC(lab_Pa_cas_growth_data_trial_2_AUC_calc$Time, lab_Pa_cas_growth_data_trial_2_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_data_trial_2_AUC_calc$Time, lab_Pa_cas_growth_data_trial_2_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_data_trial_2_AUC_calc$Time, lab_Pa_cas_growth_data_trial_2_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
lab_Pa_cas_growth_data_trial_3_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_cas_growth_trial_3_AUC<-c(AUC(lab_Pa_cas_growth_data_trial_3_AUC_calc$Time, lab_Pa_cas_growth_data_trial_3_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_data_trial_3_AUC_calc$Time, lab_Pa_cas_growth_data_trial_3_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_data_trial_3_AUC_calc$Time, lab_Pa_cas_growth_data_trial_3_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine lab pa cas AUC data
lab_Pa_cas_AUC_6hr<-rbind(lab_Pa_cas_growth_trial_1_AUC, lab_Pa_cas_growth_trial_2_AUC, lab_Pa_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
lab_Pa_cas_AUC_6hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = lab_Pa_cas_AUC_6hr))

#graph lab pa cas growth 6hr AUC data
ggplot(lab_Pa_cas_AUC_6hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-6 hr"])) +
  scale_x_discrete(limits = lab_growth_labels) +
  scale_y_continuous(limits = c(0,6),
                     breaks = c(0,2,4,6),
                     labels = c(0,2,4,6)) +
  scale_color_manual(name = "Strain",
                     breaks = lab_growth_labels,
                     labels = lab_growth_labels,
                     values = lab_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = lab_growth_labels,
                     labels = lab_growth_labels,
                     values = lab_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1), xend = c(2,3), y = c(5,5.5), yend = c(5,5.5), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.2), fill = NA, label.color = NA, label="*p* = 1.3e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=5.7), fill = NA, label.color = NA, label="*p* = 0.17" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_pa_cas_growth_AUC_6hr.pdf", height = 6, width = 4, unit = "cm")

# calculate lab pa cas growth AUC 24hr
lab_Pa_cas_growth_trial_1_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
lab_Pa_cas_growth_trial_1_AUC<-c(AUC(lab_Pa_cas_growth_trial_1_AUC_calc$Time, lab_Pa_cas_growth_trial_1_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_trial_1_AUC_calc$Time, lab_Pa_cas_growth_trial_1_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_trial_1_AUC_calc$Time, lab_Pa_cas_growth_trial_1_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
lab_Pa_cas_growth_trial_2_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
lab_Pa_cas_growth_trial_2_AUC<-c(AUC(lab_Pa_cas_growth_trial_2_AUC_calc$Time, lab_Pa_cas_growth_trial_2_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_trial_2_AUC_calc$Time, lab_Pa_cas_growth_trial_2_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_trial_2_AUC_calc$Time, lab_Pa_cas_growth_trial_2_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
lab_Pa_cas_growth_trial_3_AUC_calc <- lab_Pa_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
lab_Pa_cas_growth_trial_3_AUC<-c(AUC(lab_Pa_cas_growth_trial_3_AUC_calc$Time, lab_Pa_cas_growth_trial_3_AUC_calc$KPPR1),
                                AUC(lab_Pa_cas_growth_trial_3_AUC_calc$Time, lab_Pa_cas_growth_trial_3_AUC_calc$PA01),
                                AUC(lab_Pa_cas_growth_trial_3_AUC_calc$Time, lab_Pa_cas_growth_trial_3_AUC_calc$PA14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine lab_Pa cas growth 24hr AUC data
lab_Pa_AUC_24hr<-rbind(lab_Pa_cas_growth_trial_1_AUC, lab_Pa_cas_growth_trial_2_AUC, lab_Pa_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
lab_Pa_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = lab_Pa_AUC_24hr))

#graph lab Pa cas growth 24hr AUC data
ggplot(lab_Pa_AUC_24hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_x_discrete(limits = lab_growth_labels) +
  scale_y_continuous(limits = c(20, 100),
                     breaks = c(20,40,60,80,100),
                     labels = c(20,40,60,80,100)) +
  scale_color_manual(name = "Strain",
                     breaks = lab_growth_labels,
                     labels = lab_growth_labels,
                     values = lab_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = lab_growth_labels,
                     labels = lab_growth_labels,
                     values = lab_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1), xend = c(2,3), y = c(90,97), yend = c(90,97), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=93), fill = NA, label.color = NA, label="*p* = 1.4e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=100), fill = NA, label.color = NA, label="*p* = 2.7e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_pa_cas_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")

# calculate mg1655 cas growth AUC 24hr
mg1655_cas_growth_trial_1_AUC_calc <- MG1655_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
mg1655_cas_growth_trial_1_AUC<-c(AUC(mg1655_cas_growth_trial_1_AUC_calc$Time, mg1655_cas_growth_trial_1_AUC_calc$KPPR1),
                                AUC(mg1655_cas_growth_trial_1_AUC_calc$Time, mg1655_cas_growth_trial_1_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mg1655_cas_growth_trial_2_AUC_calc <- MG1655_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mg1655_cas_growth_trial_2_AUC<-c(AUC(mg1655_cas_growth_trial_2_AUC_calc$Time, mg1655_cas_growth_trial_2_AUC_calc$KPPR1),
                                AUC(mg1655_cas_growth_trial_2_AUC_calc$Time, mg1655_cas_growth_trial_2_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mg1655_cas_growth_trial_3_AUC_calc <- MG1655_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mg1655_cas_growth_trial_3_AUC<-c(AUC(mg1655_cas_growth_trial_3_AUC_calc$Time, mg1655_cas_growth_trial_3_AUC_calc$KPPR1),
                                AUC(mg1655_cas_growth_trial_3_AUC_calc$Time, mg1655_cas_growth_trial_3_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine MG1655 cas growth 24hr AUC data
mg1655_AUC_24hr<-rbind(mg1655_cas_growth_trial_1_AUC, mg1655_cas_growth_trial_2_AUC, mg1655_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mg1655_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mg1655_AUC_24hr))

#graph mg1655 cas growth 24hr AUC data
ggplot(mg1655_AUC_24hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_x_discrete(limits = mg1655_growth_levels) +
  scale_y_continuous(limits = c(20, 100),
                     breaks = c(20,40,60,80,100),
                     labels = c(20,40,60,80,100)) +
  scale_color_manual(name = "Strain",
                     breaks = mg1655_growth_levels,
                     labels = mg1655_growth_labels,
                     values = mg1655_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = mg1655_growth_levels,
                     labels = mg1655_growth_labels,
                     values = mg1655_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), y = c(50), yend = c(50), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=53), fill = NA, label.color = NA, label="*p* = 0.84" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mg1655_cas_growth_AUC_24hr.pdf", height = 6, width = 3.5, unit = "cm")

# calculate 13F11 cas growth AUC 24hr
Tn13F11_cas_growth_trial_1_AUC_calc <- Tn13F11_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Tn13F11_cas_growth_trial_1_AUC<-c(AUC(Tn13F11_cas_growth_trial_1_AUC_calc$Time, Tn13F11_cas_growth_trial_1_AUC_calc$KPPR1),
                                 AUC(Tn13F11_cas_growth_trial_1_AUC_calc$Time, Tn13F11_cas_growth_trial_1_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
Tn13F11_cas_growth_trial_2_AUC_calc <- Tn13F11_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
Tn13F11_cas_growth_trial_2_AUC<-c(AUC(Tn13F11_cas_growth_trial_2_AUC_calc$Time, Tn13F11_cas_growth_trial_2_AUC_calc$KPPR1),
                                 AUC(Tn13F11_cas_growth_trial_2_AUC_calc$Time, Tn13F11_cas_growth_trial_2_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
Tn13F11_cas_growth_trial_3_AUC_calc <- Tn13F11_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
Tn13F11_cas_growth_trial_3_AUC<-c(AUC(Tn13F11_cas_growth_trial_3_AUC_calc$Time, Tn13F11_cas_growth_trial_3_AUC_calc$KPPR1),
                                 AUC(Tn13F11_cas_growth_trial_3_AUC_calc$Time, Tn13F11_cas_growth_trial_3_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine MG1655 cas growth 24hr AUC data
Tn13F11_AUC_24hr<-rbind(Tn13F11_cas_growth_trial_1_AUC, Tn13F11_cas_growth_trial_2_AUC, Tn13F11_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
Tn13F11_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = Tn13F11_AUC_24hr))

#graph 13F11 cas growth 24hr AUC data
ggplot(Tn13F11_AUC_24hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_x_discrete(limits = Tn13F11_growth_levels) +
  scale_y_continuous(limits = c(20, 100),
                     breaks = c(20,40,60,80,100),
                     labels = c(20,40,60,80,100)) +
  scale_color_manual(name = "Strain",
                     breaks = Tn13F11_growth_levels,
                     labels = Tn13F11_growth_labels,
                     values = Tn13F11_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = Tn13F11_growth_levels,
                     labels = Tn13F11_growth_labels,
                     values = Tn13F11_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), y = c(50), yend = c(50), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=53), fill = NA, label.color = NA, label="*p* = 0.35" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/Tn13F11_cas_growth_AUC_24hr.pdf", height = 6, width = 3.5, unit = "cm")

# calculate rhl cas growth AUC 24hr
rhl_cas_growth_trial_1_AUC_calc <- rhl_cas_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
rhl_cas_growth_trial_1_AUC<-c(AUC(rhl_cas_growth_trial_1_AUC_calc$Time, rhl_cas_growth_trial_1_AUC_calc$KPPR1),
                             AUC(rhl_cas_growth_trial_1_AUC_calc$Time, rhl_cas_growth_trial_1_AUC_calc$PA14),
                             AUC(rhl_cas_growth_trial_1_AUC_calc$Time, rhl_cas_growth_trial_1_AUC_calc$RHLR),
                             AUC(rhl_cas_growth_trial_1_AUC_calc$Time, rhl_cas_growth_trial_1_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
rhl_cas_growth_trial_2_AUC_calc <- rhl_cas_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
rhl_cas_growth_trial_2_AUC<-c(AUC(rhl_cas_growth_trial_2_AUC_calc$Time, rhl_cas_growth_trial_2_AUC_calc$KPPR1),
                             AUC(rhl_cas_growth_trial_2_AUC_calc$Time, rhl_cas_growth_trial_2_AUC_calc$PA14),
                             AUC(rhl_cas_growth_trial_2_AUC_calc$Time, rhl_cas_growth_trial_2_AUC_calc$RHLR),
                             AUC(rhl_cas_growth_trial_2_AUC_calc$Time, rhl_cas_growth_trial_2_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
rhl_cas_growth_trial_3_AUC_calc <- rhl_cas_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
rhl_cas_growth_trial_3_AUC<-c(AUC(rhl_cas_growth_trial_3_AUC_calc$Time, rhl_cas_growth_trial_3_AUC_calc$KPPR1),
                             AUC(rhl_cas_growth_trial_3_AUC_calc$Time, rhl_cas_growth_trial_3_AUC_calc$PA14),
                             AUC(rhl_cas_growth_trial_3_AUC_calc$Time, rhl_cas_growth_trial_3_AUC_calc$RHLR),
                             AUC(rhl_cas_growth_trial_3_AUC_calc$Time, rhl_cas_growth_trial_3_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine rhl cas growth 24hr AUC data
rhl_AUC_24hr<-rbind(rhl_cas_growth_trial_1_AUC, rhl_cas_growth_trial_2_AUC, rhl_cas_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
rhl_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = rhl_AUC_24hr))

#graph 13F11 cas growth 24hr AUC data
ggplot(rhl_AUC_24hr, aes(x=strain, y=mean_AUC)) +
  geom_point(aes(x=strain, y=AUC, color = strain, shape = strain),
             position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=strain, y=mean_AUC, group = strain),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x= "Strain", y= bquote(AUC["Time = 0-24 hr"])) +
  scale_x_discrete(limits = rhl_growth_levels, 
                   labels = rhl_growth_labels) +
  scale_y_continuous(limits = c(20, 100),
                     breaks = c(20,40,60,80,100),
                     labels = c(20,40,60,80,100)) +
  scale_color_manual(name = "Strain",
                     breaks = rhl_growth_levels,
                     labels = rhl_growth_labels,
                     values = rhl_growth_colors) +
  scale_shape_manual(name = "Strain",
                     breaks = rhl_growth_levels,
                     labels = rhl_growth_labels,
                     values = rhl_growth_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2,2), xend = c(2,3,4), y = c(80,90,50), yend = c(80,90,50), color = "black") +
  geom_richtext(data=tibble(x=1.6, y=83), fill = NA, label.color = NA, label="*p* = 1.2e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=93), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=46), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/rhl_cas_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")