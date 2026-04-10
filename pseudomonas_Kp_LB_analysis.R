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
combined_LB_competition<-read_csv("combined_LB_competition.csv") %>%
  select(-1)
combined_LB_growth_curves<-read_csv("combined_LB_growth_curves.csv") %>%
  select(-1)
#===================================================================
# define aesthetics ##################################
co_culture_y_lim = c(0,6)
co_culture_y_breaks = c(0, 2, 4, 6)
co_culture_y_labels = c(0, 2, 4, 6)

spent_culture_y_lim = c(-2,6)
spent_culture_y_breaks = c(-2, 0, 2, 4, 6)
spent_culture_y_labels = c(-2, 0, 2, 4, 6)

growth_x_limits = c(0, 24)
growth_x_breaks = c(0,6,12,18,24)
growth_x_labels = c(0,6,12,18,24)

growth_y_limits = c(0.001, 10)
growth_y_breaks = c(0.001, 0.01, 0.1, 1, 10)
growth_y_labels = c(0.001, 0.01, 0.1, 1, 10)

mouse_co_culture_levels<-c("Fresh_LB","KPPR1_145.1","KPPR1_191.1","KPPR1_193.1")
mouse_co_culture_labels<-c("KPPR1 alone","KPPR1 + 145.1","KPPR1 + 191.1","KPPR1 + 193.1")
mouse_co_culture_shapes<-c(16,16,16,16)
mouse_co_culture_colors<-c("black", "#0147AB", "#008EEC", "#42E0D1")

mouse_spent_culture_levels<-c("Fresh_LB","Self_spent_LB","145.1_Spent_LB","191.1_Spent_LB","193.1_Spent_LB")
mouse_spent_culture_labels<-c("Fresh media","KPPR1 spent media","145.1 spent media","191.1 spent media","193.1 spent media")
mouse_spent_culture_shapes<-c(16,15,15,15,15)
mouse_spent_culture_colors<-c("black", "black","#0147AB", "#008EEC", "#42E0D1")

PA01_co_culture_levels<-c("KPPR1_fresh_LB","KPPR1_PA01")
PA01_co_culture_labels<-c("KPPR1 alone","KPPR1 + PAO1")
PA01_co_culture_shapes<-c(16,16)
PA01_co_culture_colors<-c("black", "#28AB87")

PA01_spent_culture_levels<-c("KPPR1_fresh_LB", "KPPR1_self_spent","KPPR1_spent_PA01")
PA01_spent_culture_labels<-c("Fresh media","KPPR1 spent media","PAO1 spent media")
PA01_spent_culture_shapes<-c(16,15,15)
PA01_spent_culture_colors<-c("black", "black", "#28AB87")

Pa14_co_culture_levels<-c("KPPR1_fresh_LB","KPPR1_Pa14")
Pa14_co_culture_labels<-c("KPPR1 alone","KPPR1 + PA14")
Pa14_co_culture_shapes<-c(16,16)
Pa14_co_culture_colors<-c("black", "#000080")

Pa14_spent_culture_levels<-c("KPPR1_fresh_LB", "KPPR1_self_spent","KPPR1_spent_Pa14")
Pa14_spent_culture_labels<-c("Fresh media","KPPR1 spent media","PA14 spent media")
Pa14_spent_culture_shapes<-c(16,15,15)
Pa14_spent_culture_colors<-c("black", "black", "#000080")

MG1655_co_culture_levels<-c("KPPR1_fresh_LB","KPPR1_MG1655")
MG1655_co_culture_labels<-c("KPPR1 alone","KPPR1 + MG1655")
MG1655_co_culture_shapes<-c(16,16)
MG1655_co_culture_colors<-c("black", "#FF7900")

MG1655_spent_culture_levels<-c("KPPR1_fresh_LB", "KPPR1_self_spent","KPPR1_MG1655_spent")
MG1655_spent_culture_labels<-c("Fresh media","KPPR1 spent media","MG1655 spent media")
MG1655_spent_culture_shapes<-c(16,15,15)
MG1655_spent_culture_colors<-c("black", "black", "#FF7900")

Tn13F11_co_culture_levels<-c("KPPR1_fresh_LB","KPPR1_13F11")
Tn13F11_co_culture_labels<-c("KPPR1 alone","KPPR1 + 13F11")
Tn13F11_co_culture_shapes<-c(16,16)
Tn13F11_co_culture_colors<-c("black", "grey")

Tn13F11_spent_culture_levels<-c("KPPR1_fresh_LB", "KPPR1_self_spent","KPPR1_13F11_spent")
Tn13F11_spent_culture_labels<-c("Fresh media","KPPR1 spent media","13F11 spent media")
Tn13F11_spent_culture_shapes<-c(16,15,15)
Tn13F11_spent_culture_colors<-c("black", "black", "grey")

rhl_co_culture_levels<-c("KPPR1_fresh_LB","KPPR1_Pa14","KPPR1_Pa_sm_32","KPPR1_Pa_sm_52")
rhl_co_culture_labels<-c("KPPR1 alone","KPPR1 + PA14",bquote("KPPR1 + PA14"*Delta*italic("rhlR")),bquote("KPPR1 + PA14"*Delta*italic("rhlI")))
rhl_co_culture_shapes<-c(16,16,1,2)
rhl_co_culture_colors<-c("black", "#000080", "#000080","#000080")

rhl_spent_culture_levels<-c("KPPR1_fresh_LB","KPPR1_self_spent","KPPR1_Pa_14_spent","KPPR1_Pa_sm32_spent","KPPR1_Pa_sm52_spent")
rhl_spent_culture_labels<-c("Fresh media","KPPR1 spent media","PA14 spent media",bquote("PA14"*Delta*italic("rhlR")~"spent media"),bquote("PA14"*Delta*italic("rhlI")~"spent media"))
rhl_spent_culture_shapes<-c(16,15,15,1,2)
rhl_spent_culture_colors<-c("black", "black", "#000080", "#000080","#000080")

phn_co_culture_levels<-c("KPPR1_fresh_lb","KPPR1_JV450","KPPR1_JV462",
                         "KPPR1_JV467","KPPR1_JV468",
                         "KPPR1_JV463", "KPPR1_JV466")
phn_co_culture_labels<-c("KPPR1 alone","KPPR1 + PA14",bquote("KPPR1 + PA14"*Delta*italic("phzA-G")),
                         bquote("KPPR1 + PA14"*Delta*italic("phzH")), bquote("KPPR1 + PA14"*Delta*italic("phzM")), 
                         bquote("KPPR1 + PA14"*Delta*italic("phzS")), bquote("KPPR1 + PA14"*Delta*italic("phzHMS")))
phn_co_culture_shapes<-c(16,16,1,
                         0,2,5,6)
phn_co_culture_colors<-c("black", "#000080", "#000080","#000080","#000080","#000080","#000080")

phn_spent_levels<-c("KPPR1_fresh_lb","KPPR1_self_spent","KPPR1_JV450_spent","KPPR1_JV462_spent","KPPR1_JV467_spent",
                    "KPPR1_JV468_spent","KPPR1_JV463_spent","KPPR1_JV466_spent")
phn_spent_labels<-c("Fresh media","KPPR1 spent media","PA14 spent media",bquote("PA14"*Delta*italic("phzA-G")~"spent media"),
                    bquote("PA14"*Delta*italic("phzH")~"spent media"), bquote("PA14"*Delta*italic("phzM")~"spent media"), 
                    bquote("PA14"*Delta*italic("phzS")~"spent media"), bquote("PA14"*Delta*italic("phzHMS")~"spent media"))
phn_spent_shapes<-c(16,15,15,1,
                    0,2,5,6)
phn_spent_colors<-c("black","black","#000080", "#000080","#000080","#000080","#000080","#000080")


pa_anaer_co_culture_levels<-c("JV1_fresh_LB","JV1_JV3", "JV1_JV4", "JV1_JV5", 
                              "JV1_JV13","JV1_JV450")
pa_anaer_co_culture_labels<-c("KPPR1 alone","KPPR1 + 145.1","KPPR1 + 191.1","KPPR1 + 193.1",
                         "KPPR1 + PAO1", "KPPR1 + PA14")
pa_anaer_co_culture_shapes<-c(16,16,16,16,
                              16,16)
pa_anaer_co_culture_colors<-c("black", "#0147AB", "#008EEC", "#42E0D1",
                         "#28AB87", "#000080")

mouse_growth_levels<-c("KPPR1","145.1","191.1","193.1")
mouse_growth_labels<-c("KPPR1","145.1","191.1","193.1")
mouse_growth_shapes<-c(16,16,16,16)
mouse_growth_colors<-c("black", "#0147AB", "#008EEC", "#42E0D1")

lab_growth_levels<-c("KPPR1","PA01","Pa14")
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
mouse_co_culture_data <- combined_LB_competition %>%
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
         y = c(4.7, 5.2, 5.7), yend = c(4.7, 5.2, 5.7), 
         color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.9), fill = NA, label.color = NA, label="*p* = 6.3e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=5.4), fill = NA, label.color = NA, label="*p* = 1.4e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=5.9), fill = NA, label.color = NA, label="*p* = 4.3e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_LB_co_culture.pdf", height = 8, width = 4, unit = "cm")

# select PA01 T24 co-culture data
PA01_co_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "PA01_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
PA01_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = PA01_co_culture_data))
# plot data
ggplot(PA01_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = PA01_co_culture_levels,
                   labels = PA01_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = PA01_co_culture_levels, 
                     labels = PA01_co_culture_labels,
                     values = PA01_co_culture_colors) +
  scale_shape_manual(breaks = PA01_co_culture_levels, 
                     labels = PA01_co_culture_labels,
                     values = PA01_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4.7), yend = c(4.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.9), fill = NA, label.color = NA, label="*p* = 0.032" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/PA01_LB_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select Pa14 T24 co-culture data
Pa14_co_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "Pa14_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
Pa14_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = Pa14_co_culture_data))
# plot data
ggplot(Pa14_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = Pa14_co_culture_levels,
                   labels = Pa14_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = Pa14_co_culture_levels, 
                     labels = Pa14_co_culture_labels,
                     values = Pa14_co_culture_colors) +
  scale_shape_manual(breaks = Pa14_co_culture_levels, 
                     labels = Pa14_co_culture_labels,
                     values = Pa14_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4.7), yend = c(4.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.9), fill = NA, label.color = NA, label="*p* = 5.83e-05" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Pa14_LB_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select MG1655 T24 co-culture data
MG1655_co_culture_data <- combined_LB_competition %>%
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
           y = c(4.7), yend = c(4.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.9), fill = NA, label.color = NA, label="*p* = 0.72" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/MG1655_LB_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select Tn13F11 T24 co-culture data
Tn13F11_co_culture_data <- combined_LB_competition %>%
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
           y = c(4.7), yend = c(4.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.9), fill = NA, label.color = NA, label="*p* = 0.88" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Tn13F11_LB_co_culture.pdf", height = 8, width = 3, unit = "cm")

# select rhlR T24 co-culture data
rhl_co_culture_data <- combined_LB_competition %>%
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
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), 
           y = c(4.7, 5.3, 5.9), yend = c(4.7, 5.3, 5.9), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.95), fill = NA, label.color = NA, label="*p* < 4e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=5.55), fill = NA, label.color = NA, label="*p* = 0.25" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=6.15), fill = NA, label.color = NA, label="*p* = 7.3e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/rhl_LB_co_culture.pdf", height = 8, width = 4, unit = "cm")

# select phn T24 co-culture data
phn_co_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "phn_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
phn_co_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = phn_co_culture_data))
# plot data
ggplot(phn_co_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = phn_co_culture_levels,
                   labels = phn_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = phn_co_culture_levels, 
                     labels = phn_co_culture_labels,
                     values = phn_co_culture_colors) +
  scale_shape_manual(breaks = phn_co_culture_levels, 
                     labels = phn_co_culture_labels,
                     values = phn_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4.7), yend = c(4.7), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=4), fill = NA, label.color = NA, label="*p* = 9e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=2), fill = NA, label.color = NA, label="*p* = 0.14" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=4), fill = NA, label.color = NA, label="*p* = 1.9e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6, y=4.5), fill = NA, label.color = NA, label="*p* = 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=4), fill = NA, label.color = NA, label="*p* = 8e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phn_LB_co_culture.pdf", height = 8, width = 9, unit = "cm")

# select pa_anaer T24 co-culture data
pa_anaer_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "co-culture") %>%
  filter(competition == "pa_anaer") %>%
  filter(Timepoint_hr == "T24")
# run stats
pa_anaer_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = pa_anaer_data))
# plot data
ggplot(pa_anaer_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = pa_anaer_co_culture_levels,
                   labels = pa_anaer_co_culture_labels) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = pa_anaer_co_culture_levels, 
                     labels = pa_anaer_co_culture_labels,
                     values = pa_anaer_co_culture_colors) +
  scale_shape_manual(breaks = pa_anaer_co_culture_levels, 
                     labels = pa_anaer_co_culture_labels,
                     values = pa_anaer_co_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave("graphs/pa_anaer_LB_co_culture.pdf", height = 8, width = 7, unit = "cm")

# select pa_diff T24 co-culture data
pa_diff_data <- combined_LB_competition %>%
  filter(competition == "pa_diff") %>%
  filter(Timepoint_hr == "T24")%>%
  mutate(media = case_when(str_detect(Condition, "Pa_agar") ~ "Pa_agar",
                           str_detect(Condition, "LB_rif_30") ~ "Kp_agar"))
# breakout media types
pa_diff_data_pa<-pa_diff_data %>% filter(media == "Pa_agar")
pa_diff_data_kp<-pa_diff_data %>% filter(media == "Kp_agar") %>%
  mutate(Condition = str_remove(Condition, "_LB_rif_30"),
         Condition = case_when(Condition == "JV1" ~ "JV1_fresh_LB",
                               TRUE ~ Condition))
# run stats
pa_diff_data_pa_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = pa_diff_data_pa))
pa_diff_data_kp_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = pa_diff_data_kp)) #check to ensure stats okay
# plot data
ggplot(pa_diff_data_pa, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = c("JV3_Pa_agar","JV1_JV3_Pa_agar", "JV4_Pa_agar", "JV1_JV4_Pa_agar","JV5_Pa_agar","JV1_JV5_Pa_agar", 
                              "JV13_Pa_agar",  "JV1_JV13_Pa_agar", "JV450_Pa_agar","JV1_JV450_Pa_agar"),
                   labels = c("145.1 alone", "145.1 + KPPR1", "191.1 alone","191.1 + KPPR1",  "193.1 alone", "193.1 + KPPR1", 
                              "PAO1 alone","PAO1 + KPPR1", "PA14 alone","PA14 + KPPR1")) +
  scale_y_continuous(limits = co_culture_y_lim,
                     breaks = co_culture_y_breaks,
                     labels = co_culture_y_labels) +
  scale_color_manual(breaks = c("JV3_Pa_agar","JV1_JV3_Pa_agar", "JV4_Pa_agar", "JV1_JV4_Pa_agar","JV5_Pa_agar","JV1_JV5_Pa_agar", 
                                "JV13_Pa_agar",  "JV1_JV13_Pa_agar", "JV450_Pa_agar","JV1_JV450_Pa_agar"), 
                     values = c("#0147AB","#0147AB", "#008EEC","#008EEC", "#42E0D1","#42E0D1",
                                "#28AB87", "#28AB87","#000080","#000080")) +
  scale_shape_manual(breaks = c("JV3_Pa_agar","JV1_JV3_Pa_agar", "JV4_Pa_agar", "JV1_JV4_Pa_agar","JV5_Pa_agar","JV1_JV5_Pa_agar", 
                                "JV13_Pa_agar",  "JV1_JV13_Pa_agar", "JV450_Pa_agar","JV1_JV450_Pa_agar"), 
                     values = c(17,2,17,2,17,2,
                                17,2,17,2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,3,5,7,9), xend = c(2,4,6,8,10), 
           y = c(5.2,5,5,5,5), yend = c(5.2,5,5,5,5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.5), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=5.3), fill = NA, label.color = NA, label="*p* = 1.00" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5.5, y=5.3), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7.5, y=5.3), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9.5, y=5.3), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/pa_agar_LB_co_culture.pdf", height = 8, width = 9, unit = "cm")
#===================================================================
# plot spent media culture ##################################
# select mouse T24 spent media data
mouse_spent_culture_data <- combined_LB_competition %>%
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
  geom_richtext(data=tibble(x=1.5, y=5.8), fill = NA, label.color = NA, label="*p* = 3.6e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=5.2), fill = NA, label.color = NA, label="*p* = 0.38" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=4.5), fill = NA, label.color = NA, label="*p* = 4.7e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=3.8), fill = NA, label.color = NA, label="*p* = 7.8e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mouse_LB_spent_culture.pdf", height = 8, width = 5, unit = "cm")

# select PA01 T24 spent data
PA01_spent_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "PA01_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
PA01_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = PA01_spent_culture_data))
# plot data
ggplot(PA01_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = PA01_spent_culture_levels,
                   labels = PA01_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = PA01_spent_culture_levels, 
                     labels = PA01_spent_culture_labels,
                     values = PA01_spent_culture_colors) +
  scale_shape_manual(breaks = PA01_spent_culture_levels, 
                     labels = PA01_spent_culture_labels,
                     values = PA01_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2), xend = c(2,3), 
           y = c(4.7,3), yend = c(4.7,3), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* = 6.5e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.3), fill = NA, label.color = NA, label="*p* = 0.49" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/PA01_LB_spent_culture.pdf", height = 8, width = 4, unit = "cm")

# select Pa14 T24 spent data
Pa14_spent_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Pa14_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
Pa14_spent_culture_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = Pa14_spent_culture_data))
# plot data
ggplot(Pa14_spent_culture_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = Pa14_spent_culture_levels,
                   labels = Pa14_spent_culture_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = Pa14_spent_culture_levels, 
                     labels = Pa14_spent_culture_labels,
                     values = Pa14_spent_culture_colors) +
  scale_shape_manual(breaks = Pa14_spent_culture_levels, 
                     labels = Pa14_spent_culture_labels,
                     values = Pa14_spent_culture_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2), xend = c(2,3), 
           y = c(4.7,3), yend = c(4.7,3), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* = 1.1e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.3), fill = NA, label.color = NA, label="*p* = 1.6e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)  
ggsave("graphs/Pa14_LB_spent_culture.pdf", height = 8, width = 4, unit = "cm")

# select MG1655 T24 spent data
MG1655_spent_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "MG1655_Kp") %>%
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
           y = c(4.7,3), yend = c(4.7,3), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* = 5.8e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.3), fill = NA, label.color = NA, label="*p* = 0.93" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/MG1655_LB_spent_culture.pdf", height = 8, width = 4, unit = "cm")

# select Tn13F11 T24 spent data
Tn13F11_spent_culture_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "Tn13F11_Kp") %>%
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
           y = c(4.7,3), yend = c(4.7,3), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* = 5.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.3), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/Tn13F11_LB_spent_culture.pdf", height = 8, width = 4, unit = "cm")

# select rhlR T24 spent data
rhl_spent_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "rhl_Kp") %>%
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
  geom_richtext(data=tibble(x=1.5, y=5.3), fill = NA, label.color = NA, label="*p* = 6.7e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.8), fill = NA, label.color = NA, label="*p* = 1.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=4.5), fill = NA, label.color = NA, label="*p* = 9.1e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) + 
  geom_richtext(data=tibble(x=4, y=5.2), fill = NA, label.color = NA, label="*p* = 2.7e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/rhl_LB_spent_culture.pdf" , height = 8, width = 5, unit = "cm")

# select phn T24 spent data
phn_spent_data <- combined_LB_competition %>%
  filter(culture_conditions == "control" | culture_conditions == "spent") %>%
  filter(competition == "spent_phn_Kp") %>%
  filter(Timepoint_hr == "T24")
# run stats
phn_spent_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = phn_spent_data))
# plot data
ggplot(phn_spent_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
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
  scale_x_discrete(limits = phn_spent_levels,
                   labels = phn_spent_labels) +
  scale_y_continuous(limits = spent_culture_y_lim,
                     breaks = spent_culture_y_breaks,
                     labels = spent_culture_y_labels) +
  scale_color_manual(breaks = phn_spent_levels, 
                     labels = phn_spent_labels,
                     values = phn_spent_colors) +
  scale_shape_manual(breaks = phn_spent_levels, 
                     labels = phn_spent_labels,
                     values = phn_spent_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1, 2), xend = c(2, 3), 
           y = c(4.7,3), yend = c(4.7,3), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=3.3), fill = NA, label.color = NA, label="*p* = 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=3.1), fill = NA, label.color = NA, label="*p* = 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=3.7), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6, y=3.1), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=3.5), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8, y=3), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/phn_LB_spent_culture.pdf", height = 8, width = 10, unit = "cm")
#===================================================================
# plot LB growth curves ##################################
mouse_LB_growth_data <-combined_LB_growth_curves %>%
  filter(expt == "Mouse_Pa")

ggplot(mouse_LB_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
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
ggsave("graphs/mouse_LB_growth.pdf", height = 4, width = 8, unit = "cm")

lab_Pa_LB_growth_data <-combined_LB_growth_curves %>%
  filter(expt == "Lab_Pa")

ggplot(lab_Pa_LB_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
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
ggsave("graphs/lab_Pa_LB_growth.pdf", height = 4, width = 8, unit = "cm")

MG1655_LB_growth_data <-combined_LB_growth_curves %>%
  filter(expt == "MG1655")

ggplot(MG1655_LB_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
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
ggsave("graphs/mg1655_LB_growth.pdf", height = 4, width = 8, unit = "cm")

Tn13F11_LB_growth_data <-combined_LB_growth_curves %>%
  filter(expt == "Tn13F11")

ggplot(Tn13F11_LB_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
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
ggsave("graphs/13F11_LB_growth.pdf", height = 4, width = 8, unit = "cm")

rhl_LB_growth_data <-combined_LB_growth_curves %>%
  filter(expt == "rhl_Pa")

ggplot(rhl_LB_growth_data, aes(x=Time, y=mean_OD600, color = strain, shape = strain)) +
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
ggsave("graphs/rhl_LB_growth.pdf", height = 4, width = 8, unit = "cm")

ggplot(combined_LB_Pa14_Tn_growth_curves, aes(x=Time, y=mean_OD600, color = strain)) +
  geom_line(alpha = 1, linewidth = 0.5) +
  geom_point(alpha = 1, size = 1) + 
  geom_linerange(aes(ymin = mean_OD600-sem_OD600, ymax = mean_OD600+sem_OD600),
                 linewidth = 0.2,
                 show.legend = FALSE) +
  labs(x= "Time (hr)", y= bquote(OD[600])) +
  guides(color=guide_legend(ncol=2)) +
  scale_x_continuous(limits = growth_x_limits,
                     breaks = growth_x_breaks,
                     labels = growth_x_labels) +
  scale_y_continuous(trans = "log10", 
                     limits = growth_y_limits,
                     breaks = growth_y_breaks,
                     labels = growth_y_labels) +
  scale_color_manual(name = "Strain",
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
                                "#000080"),
                     labels = c("PA14_51430",
                                "PA14_16930",  
                                "PA14_09810",
                                "PA14_64590",
                                "PA14_05300",
                                "PA14_18520",
                                "PA14_01960",
                                "PA14_09480",
                                "PA14_19120",
                                "PA14_48650",
                                "PA14_58770",
                                "PA14_06570",
                                "PA14_32750",
                                "PA14_62830",
                                "PA14_17250",
                                "PA14_20440",
                                "PA14_68350",
                                "PA14_72840",
                                "PA14"),) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        legend.position = "right",
        legend.text = element_text(size = 6)) 
ggsave("graphs/Pa14_Tn_LB_growth.pdf", height = 7, width = 14, unit = "cm")
#===================================================================
# plot LB AUC ##################################
# calculate mouse LB AUC 6hr
mouse_LB_growth_trial_1_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_LB_growth_trial_1_AUC<-c(AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$KPPR1),
               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`145.1`),
               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`191.1`),
               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mouse_LB_growth_trial_2_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_LB_growth_trial_2_AUC<-c(AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$KPPR1),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`145.1`),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`191.1`),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mouse_LB_growth_trial_3_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
mouse_LB_growth_trial_3_AUC<-c(AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$KPPR1),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`145.1`),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`191.1`),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine mouse LB growth 6hr AUC data
mouse_AUC_6hr<-rbind(mouse_LB_growth_trial_1_AUC, mouse_LB_growth_trial_2_AUC, mouse_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mouse_AUC_6hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mouse_AUC_6hr))

#graph mouse LB growth 6hr AUC data
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
  scale_y_continuous(limits = c(1, 10.5),
                     breaks = c(2,4,6,8,10),
                     labels = c(2,4,6,8,10)) +
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
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), y = c(8,9,10), yend = c(8,9,10), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=8.4), fill = NA, label.color = NA, label="*p* = 0.035" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=9.4), fill = NA, label.color = NA, label="*p* = 0.005" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=10.4), fill = NA, label.color = NA, label="*p* = 0.022" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/mouse_LB_growth_AUC_6hr.pdf", height = 6, width = 4, unit = "cm")

# calculate mouse LB AUC 24hr
mouse_LB_growth_trial_1_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
mouse_LB_growth_trial_1_AUC<-c(AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$KPPR1),
                               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`145.1`),
                               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`191.1`),
                               AUC(mouse_LB_growth_trial_1_AUC_calc$Time, mouse_LB_growth_trial_1_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mouse_LB_growth_trial_2_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mouse_LB_growth_trial_2_AUC<-c(AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$KPPR1),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`145.1`),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`191.1`),
                               AUC(mouse_LB_growth_trial_2_AUC_calc$Time, mouse_LB_growth_trial_2_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mouse_LB_growth_trial_3_AUC_calc <- mouse_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mouse_LB_growth_trial_3_AUC<-c(AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$KPPR1),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`145.1`),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`191.1`),
                               AUC(mouse_LB_growth_trial_3_AUC_calc$Time, mouse_LB_growth_trial_3_AUC_calc$`193.1`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mouse_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine mouse LB growth 24hr AUC data
mouse_AUC_24hr<-rbind(mouse_LB_growth_trial_1_AUC, mouse_LB_growth_trial_2_AUC, mouse_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mouse_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mouse_AUC_24hr))

#graph mouse LB growth 24hr AUC data
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
  scale_y_continuous(limits = c(60, 90),
                     breaks = c(60,70,80,90),
                     labels = c(60,70,80,90)) +
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
  annotate("segment", x = c(1,1,1), xend = c(2,3,4), y = c(78,82,86), yend = c(78,82,86), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=79.5), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=83.5), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=87.5), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/mouse_LB_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")

# calculate lab pa LB AUC 6hr
lab_Pa_LB_growth_data_trial_1_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_LB_growth_trial_1_AUC<-c(AUC(lab_Pa_LB_growth_data_trial_1_AUC_calc$Time, lab_Pa_LB_growth_data_trial_1_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_data_trial_1_AUC_calc$Time, lab_Pa_LB_growth_data_trial_1_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_data_trial_1_AUC_calc$Time, lab_Pa_LB_growth_data_trial_1_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
lab_Pa_LB_growth_data_trial_2_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_LB_growth_trial_2_AUC<-c(AUC(lab_Pa_LB_growth_data_trial_2_AUC_calc$Time, lab_Pa_LB_growth_data_trial_2_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_data_trial_2_AUC_calc$Time, lab_Pa_LB_growth_data_trial_2_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_data_trial_2_AUC_calc$Time, lab_Pa_LB_growth_data_trial_2_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
lab_Pa_LB_growth_data_trial_3_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") %>%
  filter(Time <= 6)
lab_Pa_LB_growth_trial_3_AUC<-c(AUC(lab_Pa_LB_growth_data_trial_3_AUC_calc$Time, lab_Pa_LB_growth_data_trial_3_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_data_trial_3_AUC_calc$Time, lab_Pa_LB_growth_data_trial_3_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_data_trial_3_AUC_calc$Time, lab_Pa_LB_growth_data_trial_3_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine lab pa LB AUC data
lab_Pa_LB_AUC_6hr<-rbind(lab_Pa_LB_growth_trial_1_AUC, lab_Pa_LB_growth_trial_2_AUC, lab_Pa_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
lab_Pa_LB_AUC_6hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = lab_Pa_LB_AUC_6hr))

#graph lab pa LB growth 6hr AUC data
ggplot(lab_Pa_LB_AUC_6hr, aes(x=strain, y=mean_AUC)) +
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
  scale_y_continuous(limits = c(1, 11),
                     breaks = c(2,4,6,8,10),
                     labels = c(2,4,6,8,10)) +
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
  annotate("segment", x = c(1,1), xend = c(2,3), y = c(9.5,10.5), yend = c(9.5,10.5), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=9.9), fill = NA, label.color = NA, label="*p* = 4.1e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=10.9), fill = NA, label.color = NA, label="*p* = 1.9e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_pa_LB_growth_AUC_6hr.pdf", height = 6, width = 4, unit = "cm")

# calculate lab pa LB growth AUC 24hr
lab_Pa_LB_growth_trial_1_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
lab_Pa_LB_growth_trial_1_AUC<-c(AUC(lab_Pa_LB_growth_trial_1_AUC_calc$Time, lab_Pa_LB_growth_trial_1_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_trial_1_AUC_calc$Time, lab_Pa_LB_growth_trial_1_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_trial_1_AUC_calc$Time, lab_Pa_LB_growth_trial_1_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
lab_Pa_LB_growth_trial_2_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
lab_Pa_LB_growth_trial_2_AUC<-c(AUC(lab_Pa_LB_growth_trial_2_AUC_calc$Time, lab_Pa_LB_growth_trial_2_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_trial_2_AUC_calc$Time, lab_Pa_LB_growth_trial_2_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_trial_2_AUC_calc$Time, lab_Pa_LB_growth_trial_2_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
lab_Pa_LB_growth_trial_3_AUC_calc <- lab_Pa_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
lab_Pa_LB_growth_trial_3_AUC<-c(AUC(lab_Pa_LB_growth_trial_3_AUC_calc$Time, lab_Pa_LB_growth_trial_3_AUC_calc$KPPR1),
                               AUC(lab_Pa_LB_growth_trial_3_AUC_calc$Time, lab_Pa_LB_growth_trial_3_AUC_calc$PA01),
                               AUC(lab_Pa_LB_growth_trial_3_AUC_calc$Time, lab_Pa_LB_growth_trial_3_AUC_calc$Pa14)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(lab_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine lab_Pa LB growth 24hr AUC data
lab_Pa_AUC_24hr<-rbind(lab_Pa_LB_growth_trial_1_AUC, lab_Pa_LB_growth_trial_2_AUC, lab_Pa_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
lab_Pa_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = lab_Pa_AUC_24hr))

#graph lab Pa LB growth 24hr AUC data
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
  scale_y_continuous(limits = c(60, 100),
                     breaks = c(60,70,80,90,100),
                     labels = c(60,70,80,90,100)) +
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
  annotate("segment", x = c(1,1), xend = c(2,3), y = c(90,95), yend = c(90,95), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=92), fill = NA, label.color = NA, label="*p* = 0.76" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=97), fill = NA, label.color = NA, label="*p* = 1.4e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/lab_pa_LB_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")

# calculate mg1655 LB growth AUC 24hr
mg1655_LB_growth_trial_1_AUC_calc <- MG1655_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
mg1655_LB_growth_trial_1_AUC<-c(AUC(mg1655_LB_growth_trial_1_AUC_calc$Time, mg1655_LB_growth_trial_1_AUC_calc$KPPR1),
                                AUC(mg1655_LB_growth_trial_1_AUC_calc$Time, mg1655_LB_growth_trial_1_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
mg1655_LB_growth_trial_2_AUC_calc <- MG1655_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mg1655_LB_growth_trial_2_AUC<-c(AUC(mg1655_LB_growth_trial_2_AUC_calc$Time, mg1655_LB_growth_trial_2_AUC_calc$KPPR1),
                                AUC(mg1655_LB_growth_trial_2_AUC_calc$Time, mg1655_LB_growth_trial_2_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
mg1655_LB_growth_trial_3_AUC_calc <- MG1655_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
mg1655_LB_growth_trial_3_AUC<-c(AUC(mg1655_LB_growth_trial_3_AUC_calc$Time, mg1655_LB_growth_trial_3_AUC_calc$KPPR1),
                                AUC(mg1655_LB_growth_trial_3_AUC_calc$Time, mg1655_LB_growth_trial_3_AUC_calc$MG1655)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(mg1655_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine MG1655 LB growth 24hr AUC data
mg1655_AUC_24hr<-rbind(mg1655_LB_growth_trial_1_AUC, mg1655_LB_growth_trial_2_AUC, mg1655_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
mg1655_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = mg1655_AUC_24hr))

#graph mg1655 LB growth 24hr AUC data
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
  scale_y_continuous(limits = c(50, 100),
                     breaks = c(60,70,80,90,100),
                     labels = c(60,70,80,90,100)) +
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
  annotate("segment", x = c(1), xend = c(2), y = c(95), yend = c(95), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=97), fill = NA, label.color = NA, label="*p* = 7.5e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/mg1655_LB_growth_AUC_24hr.pdf", height = 6, width = 3.5, unit = "cm")

# calculate 13F11 LB growth AUC 24hr
Tn13F11_LB_growth_trial_1_AUC_calc <- Tn13F11_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
Tn13F11_LB_growth_trial_1_AUC<-c(AUC(Tn13F11_LB_growth_trial_1_AUC_calc$Time, Tn13F11_LB_growth_trial_1_AUC_calc$KPPR1),
                                AUC(Tn13F11_LB_growth_trial_1_AUC_calc$Time, Tn13F11_LB_growth_trial_1_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
Tn13F11_LB_growth_trial_2_AUC_calc <- Tn13F11_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
Tn13F11_LB_growth_trial_2_AUC<-c(AUC(Tn13F11_LB_growth_trial_2_AUC_calc$Time, Tn13F11_LB_growth_trial_2_AUC_calc$KPPR1),
                                AUC(Tn13F11_LB_growth_trial_2_AUC_calc$Time, Tn13F11_LB_growth_trial_2_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
Tn13F11_LB_growth_trial_3_AUC_calc <- Tn13F11_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
Tn13F11_LB_growth_trial_3_AUC<-c(AUC(Tn13F11_LB_growth_trial_3_AUC_calc$Time, Tn13F11_LB_growth_trial_3_AUC_calc$KPPR1),
                                AUC(Tn13F11_LB_growth_trial_3_AUC_calc$Time, Tn13F11_LB_growth_trial_3_AUC_calc$`13F11`)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(Tn13F11_growth_labels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine MG1655 LB growth 24hr AUC data
Tn13F11_AUC_24hr<-rbind(Tn13F11_LB_growth_trial_1_AUC, Tn13F11_LB_growth_trial_2_AUC, Tn13F11_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
Tn13F11_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = Tn13F11_AUC_24hr))

#graph 13F11 LB growth 24hr AUC data
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
  scale_y_continuous(limits = c(50, 100),
                     breaks = c(60,70,80,90,100),
                     labels = c(60,70,80,90,100)) +
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
  annotate("segment", x = c(1), xend = c(2), y = c(95), yend = c(95), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=97), fill = NA, label.color = NA, label="*p* = 0.93" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/Tn13F11_LB_growth_AUC_24hr.pdf", height = 6, width = 3.5, unit = "cm")

# calculate rhl LB growth AUC 24hr
rhl_LB_growth_trial_1_AUC_calc <- rhl_LB_growth_data %>%
  filter(trial == "trial_1") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600")
rhl_LB_growth_trial_1_AUC<-c(AUC(rhl_LB_growth_trial_1_AUC_calc$Time, rhl_LB_growth_trial_1_AUC_calc$KPPR1),
                             AUC(rhl_LB_growth_trial_1_AUC_calc$Time, rhl_LB_growth_trial_1_AUC_calc$PA14),
                             AUC(rhl_LB_growth_trial_1_AUC_calc$Time, rhl_LB_growth_trial_1_AUC_calc$RHLR),
                             AUC(rhl_LB_growth_trial_1_AUC_calc$Time, rhl_LB_growth_trial_1_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_1")
rhl_LB_growth_trial_2_AUC_calc <- rhl_LB_growth_data %>%
  filter(trial == "trial_2") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
rhl_LB_growth_trial_2_AUC<-c(AUC(rhl_LB_growth_trial_2_AUC_calc$Time, rhl_LB_growth_trial_2_AUC_calc$KPPR1),
                             AUC(rhl_LB_growth_trial_2_AUC_calc$Time, rhl_LB_growth_trial_2_AUC_calc$PA14),
                             AUC(rhl_LB_growth_trial_2_AUC_calc$Time, rhl_LB_growth_trial_2_AUC_calc$RHLR),
                             AUC(rhl_LB_growth_trial_2_AUC_calc$Time, rhl_LB_growth_trial_2_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_2")
rhl_LB_growth_trial_3_AUC_calc <- rhl_LB_growth_data %>%
  filter(trial == "trial_3") %>%
  select(Time, strain, OD600) %>%
  pivot_wider(names_from = "strain", values_from = "OD600") 
rhl_LB_growth_trial_3_AUC<-c(AUC(rhl_LB_growth_trial_3_AUC_calc$Time, rhl_LB_growth_trial_3_AUC_calc$KPPR1),
                             AUC(rhl_LB_growth_trial_3_AUC_calc$Time, rhl_LB_growth_trial_3_AUC_calc$PA14),
                             AUC(rhl_LB_growth_trial_3_AUC_calc$Time, rhl_LB_growth_trial_3_AUC_calc$RHLR),
                             AUC(rhl_LB_growth_trial_3_AUC_calc$Time, rhl_LB_growth_trial_3_AUC_calc$RHLI)) %>%
  as_tibble_col(column_name = "AUC") %>%
  bind_cols(as_tibble_col(rhl_growth_levels, column_name = "strain"), .) %>%
  mutate(trial = "trial_3")

#combine rhl LB growth 24hr AUC data
rhl_AUC_24hr<-rbind(rhl_LB_growth_trial_1_AUC, rhl_LB_growth_trial_2_AUC, rhl_LB_growth_trial_3_AUC) %>%
  mutate(log_AUC = log10(AUC)) %>%
  group_by(strain) %>%
  mutate(mean_AUC = mean(AUC)) %>%
  mutate(sem_AUC = sd(AUC)/sqrt(length((AUC)))) %>%
  ungroup()

# run stats
rhl_AUC_24hr_aov<-TukeyHSD(aov(log_AUC ~ strain, data = rhl_AUC_24hr))

#graph rhl LB growth 24hr AUC data
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
  scale_y_continuous(limits = c(50, 100),
                     breaks = c(60,70,80,90,100),
                     labels = c(60,70,80,90,100)) +
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
  annotate("segment", x = c(1,2,2), xend = c(2,3,4), y = c(85,95,60), yend = c(85,95,60), color = "black") +
  geom_richtext(data=tibble(x=1.5, y=87), fill = NA, label.color = NA, label="*p* = 0.01" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=97), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=57), fill = NA, label.color = NA, label="*p* = 0.98" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave("graphs/rhl_LB_growth_AUC_24hr.pdf", height = 6, width = 4, unit = "cm")

