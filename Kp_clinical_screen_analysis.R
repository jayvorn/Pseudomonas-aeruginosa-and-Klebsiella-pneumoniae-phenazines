library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(DescTools)
library(ggrepel)

# set environment##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#================================================
# import data ##################################
all_kp_screen_data<-read_csv("combined_Kp_clin_screen_data.csv")

val_data<-read_csv("Kp_clin_validation.csv")

ec_phen_data<-read_csv("Kp_clinical_Ec.csv")
#================================================
# graph screen data ##################################
KPPR1_LB_mean<-all_kp_screen_data %>%
  filter(Media == "LB") %>%
  filter(Group == "ctl") %>%
  .[1,9]
KPPR1_LB_SD_hi<-all_kp_screen_data %>%
  filter(Media == "LB") %>%
  filter(Group == "ctl") %>%
  .[1,10]
KPPR1_LB_SD_lo<-all_kp_screen_data %>%
  filter(Media == "LB") %>%
  filter(Group == "ctl") %>%
  .[1,11]
KPPR1_pa14_mean<-all_kp_screen_data %>%
  filter(Media == "pa14_spent") %>%
  filter(Group == "ctl") %>%
  .[1,9]
KPPR1_pa14_SD_hi<-all_kp_screen_data %>%
  filter(Media == "pa14_spent") %>%
  filter(Group == "ctl") %>%
  .[1,10]
KPPR1_pa14_SD_lo<-all_kp_screen_data %>%
  filter(Media == "pa14_spent") %>%
  filter(Group == "ctl") %>%
  .[1,11]

all_kp_screen_data %>%
  filter(Media == "pa14_spent") %>%
  select(Strain, Media, mean_OD) %>%
  distinct() %>%
  ggplot() +
  geom_hline(yintercept = 1.38, color = "red", linetype = "solid") +
  geom_hline(yintercept = 0.0236, color = "orange", linetype = "solid") +
  geom_jitter(aes(x=Media, y=mean_OD, color = Media),
              alpha = 1,
              shape = 21,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE)+ 
  guides(color = "none") +
  geom_richtext(data=tibble(x=1.4, y=1.38), fill = "red", label.color = NA, label="Fresh media" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_richtext(data=tibble(x=1.4, y=0.0236), fill = "orange", label.color = NA, label="PA14 spent<br> media" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  scale_x_discrete(breaks = c("pa14_spent"),
                   labels = c(bquote("Clinical Kp"))) +  
  scale_color_manual(breaks = c("pa14_spent"), 
                     values = c("#000080")) +
  labs(x = NULL, y = bquote(OD[600])) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Kp_screen_OD.pdf"), height = 8, width = 6, unit = "cm")
#================================================
# graph validation data ##################################
TukeyHSD(aov(log_fold_change ~ Condition, data = val_data))

ggplot(val_data, aes(x=Condition, y=log_fold_change, color = Condition, shape = Condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_y_continuous(limits = c(-1,6),
                     breaks = c(0, 2, 4, 6),
                     labels = c(0, 2, 4, 6))+
  scale_x_discrete(limits = c("KPPR1_fresh_lb", "Pa14_KPPR1", "Pa14_JV87", "Pa14_JV209", "Pa14_JV249", "Pa14_JV250","Pa14_JV414"), 
                   labels = c("KPPR1 alone", "PA14 + KPPR1", "PA14 + JV87", "PA14 + JV209", "PA14 + JV249", "PA14 + JV250","PA14 + JV414")) +
  scale_color_manual(breaks = c("KPPR1_fresh_lb", "Pa14_KPPR1", "Pa14_JV87", "Pa14_JV209", "Pa14_JV249", "Pa14_JV250","Pa14_JV414"), 
                     labels = c("KPPR1 alone", "PA14 + KPPR1", "PA14 + JV87", "PA14 + JV209", "PA14 + JV249", "PA14 + JV250","PA14 + JV414"),
                     values = c("black", "#000080", "#c7522a", "#e5c185", "#fbf2c4", "#74a892","#008585")) +
  scale_shape_manual(breaks = c("KPPR1_fresh_lb", "Pa14_KPPR1", "Pa14_JV87", "Pa14_JV209", "Pa14_JV249", "Pa14_JV250","Pa14_JV414"), 
                     labels = c("KPPR1 alone", "PA14 + KPPR1", "PA14 + JV87", "PA14 + JV209", "PA14 + JV249", "PA14 + JV250","PA14 + JV414"),
                     values = c(15,16,16,16,16,16,16)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(5.5), yend = c(5.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=5.8), fill = NA, label.color = NA, label="*p* = 2e-6" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=2.2), fill = NA, label.color = NA, label="*p* = 0.90" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=2.6), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=2.8), fill = NA, label.color = NA, label="*p* = 0.90" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6, y=2.6), fill = NA, label.color = NA, label="*p* = 0.97" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=3), fill = NA, label.color = NA, label="*p* = 0.91" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave(glue("graphs/clin_Kp_val.pdf"), height = 8, width = 8, unit = "cm")

all_kp_screen_data %>%
  filter(Strain == "JV87" |
           Strain == "JV209" |
           Strain == "JV249" |
           Strain == "JV250" |
           Strain == "JV414") %>%
  select(Strain, Media, mean_OD) %>%
  distinct() %>%
  ggplot() +
  geom_point(aes(x=Media, y=mean_OD, color = Media, shape = Media),
              alpha = 1,
              size = 1.5,
              show.legend = FALSE) + 
  geom_text_repel(aes(x=Media, y=mean_OD,label = Strain), 
                  size = 3) + 
  scale_x_discrete(breaks = c("LB", "pa14_spent"),
                   labels = c("Fresh media", "PA14 spent media")) +  
  scale_color_manual(breaks = c("LB", "pa14_spent"), 
                     values = c("black", "#000080")) +
  scale_shape_manual(breaks = c("LB", "pa14_spent"), 
                     values = c(16, 0)) +
  labs(x = NULL, y = bquote(OD[600])) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Kp_screen_OD_val_only.pdf"), height = 8, width = 8, unit = "cm")
#================================================
# graph Ec media data ##################################
ec_phen_data<-ec_phen_data %>%
  select(Kp_strain, Ec_strain, med_blank_corr_read) %>%
  distinct() %>%
  mutate(group = case_when(Kp_strain == "JV1" ~ "ctl",
                           TRUE ~ "exp")) %>%
  group_by(Ec_strain) %>%
  mutate(mean_med_blank_corr_read = mean(med_blank_corr_read)) %>%
  ungroup()

TukeyHSD(aov(med_blank_corr_read ~ Ec_strain, data = ec_phen_data))

ggplot(ec_phen_data, aes(x = Ec_strain, y = med_blank_corr_read, color = Ec_strain)) +
  geom_line(data = ec_phen_data[ec_phen_data$group == "exp", ], aes(group = Kp_strain, shape = group, fill = group),
            position = position_jitterdodge(jitter.width = 0.01, seed = 02122022),
            color = "black",
            alpha = 0.2) +
  geom_point(data = ec_phen_data[ec_phen_data$group == "exp", ], aes(group = Kp_strain, shape = group, fill = group),
             position = position_jitterdodge(jitter.width = 0.01, seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_line(data = ec_phen_data[ec_phen_data$group == "ctl", ], aes(group = Kp_strain, shape = group, fill = group),
            position = position_jitterdodge(jitter.width = 0.01, seed = 02122022),
            color = "black",
            alpha = 0.25) +
  geom_point(data = ec_phen_data[ec_phen_data$group == "ctl", ], aes(group = Kp_strain, shape = group, fill = group),
             position = position_jitterdodge(jitter.width = 0.01, seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_point(aes(x=Ec_strain, y=mean_med_blank_corr_read),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  guides(color = "none", fill = "none") +
  labs(x = NULL, y = bquote(OD[600])) +
  scale_x_discrete(limits = c("JV534", "JV537", "JV540"),
                   labels = c("MG1655pPhzA-G<br>spent media","MG1655pPhzA-GS*M<br>spent media", "MG1655pPhzA-GSM<br>spent media")) +
  scale_color_manual(breaks = c("JV534", "JV537", "JV540"),
                     values = c("#E3AC36", "#990000", "#00CEC8")) +
  scale_fill_manual(breaks = c("ctl", "exp"),
                     values = c("black","white")) +
  scale_shape_manual(name = "",
                     breaks = c("ctl", "exp"),
                     values = c(21,0),
                     labels = c("KPPR1", "Clinical strain")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1, 1), xend = c(2, 3), 
           y = c(0.325, 0.375), yend = c(0.325, 0.375), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=0.35), fill = NA, label.color = NA, label="*p* = 4e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=0.4), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) 
ggsave(glue("graphs/clin_Kp_screen_Ec_phen_media_OD.pdf"), height = 8, width = 10, unit = "cm")