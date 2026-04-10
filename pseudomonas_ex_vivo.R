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
all_gut_data<-read_csv("all_gut_data.csv")

all_BALF_data<-read_csv("all_BALF_data.csv")

all_blad_data<-read_csv("all_blad_data.csv")
#===================================================================
# graph ex vivo LI data ##################################
all_gut_data %>%
  filter(Strain == "JV1") %>%
  ggplot(aes(x=Pa_strain, y=log_CFU_per_mL, color = Pa_strain, shape = Pa_strain)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Pa_strain, y=mean_log_CFU_per_mL),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log[10]*"(KPPR1 CFU/mL)")) +
  scale_y_continuous(limits = c(4,10),
                     breaks = c(4,6,8,10),
                     labels = c(4,6,8,10))+
  scale_x_discrete(limits = c("none", "JV3", "JV4", "JV5"),
                   labels = c("KPPR1 alone", "KPPR1 + 145.1", "KPPR1 + 191.1", "KPPR1 + 193.1")) +
  scale_color_manual(breaks =c("none", "JV3", "JV4", "JV5"),
                     values = c("black", "#0147AB", "#008EEC", "#42E0D1")) +
  scale_shape_manual(breaks = c("none", "JV3", "JV4", "JV5"),
                     values = c(16, 15, 15, 15)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/ex_vivo_comp_gut_kp_cfus.pdf"), height = 8, width = 6, unit = "cm")

all_gut_data %>%
  filter(Strain != "JV1") %>%
  ggplot(aes(x=Strain, y=log_CFU_per_mL, color = Strain, shape = Strain)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=Strain, y=mean_log_CFU_per_mL),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log[10]*"(CFU/mL)")) +
  scale_y_continuous(limits = c(4,10),
                     breaks = c(4,6,8,10),
                     labels = c(4,6,8,10))+
  scale_x_discrete(limits = c("JV3", "JV4", "JV5"),
                   labels = c("KPPR1 + 145.1", "KPPR1 + 191.1", "KPPR1 + 193.1")) +
  scale_color_manual(breaks =c("JV3", "JV4", "JV5"),
                     values = c("#0147AB", "#008EEC", "#42E0D1")) +
  scale_shape_manual(breaks = c("JV3", "JV4", "JV5"),
                     values = c(15, 15, 15)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/ex_vivo_comp_gut_pa_cfus.pdf"), height = 8, width = 4, unit = "cm")

# graph BALF data
BALF_t_test<-all_BALF_data %>%
  filter(media == "LB_rif")
pairwise.t.test(BALF_t_test$log_fold_change, BALF_t_test$inoculum, p.adjust.method = "none")

BALF_t_test %>%
  ggplot(aes(x=inoculum, y=log_fold_change, color=inoculum, shape=inoculum)) +
  geom_line(aes(group=mouse),
            position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0,seed = 02122022),
            alpha = 0.25,
            color = "black",
            show.legend = FALSE) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=inoculum, y=mean_log_fold_change),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
 # scale_y_continuous(limits = c(-1,6),
  #                   breaks = c(0, 2, 4, 6),
   #                  labels = c(0, 2, 4, 6))+
  scale_x_discrete(limits = c("JV1_JV450", "JV1_JV462"),
                   labels = c("KPPR1 + PA14", bquote("KPPR1 + PA14"*Delta*italic(phzA-G)))) +
  scale_color_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c("#000080", "#000080")) +
  scale_shape_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c(16, 1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))+
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4), yend = c(4), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.3), fill = NA, label.color = NA, label="*p* = 0.73" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/ex_vivo_comp_BALF_kp_fc.pdf"), height = 8, width = 5, unit = "cm")

all_BALF_data %>%
  filter(media != "LB_rif") %>%
  ggplot(aes(x=inoculum, y=log_CFU_per_mL, color=inoculum, shape=inoculum)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=inoculum, y=mean_log_CFU_per_mL),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log[10]*"(CFU/mL)")) +
  scale_y_continuous(limits = c(6,12),
                     breaks = c(6,8,10,12),
                    labels = c(6,8,10,12))+
  scale_x_discrete(limits = c("JV1_JV450", "JV1_JV462"),
                   labels = c("KPPR1 + PA14", bquote("KPPR1 + PA14"*Delta*italic(phzA-G)))) +
  scale_color_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c("#000080", "#000080")) +
  scale_shape_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c(16, 1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/ex_vivo_comp_BALF_pa_cfus.pdf"), height = 8, width = 5, unit = "cm")

# graph bladder data
blad_t_test<-all_blad_data %>%
  filter(media == "LB_rif")
pairwise.t.test(blad_t_test$log_fold_change, blad_t_test$inoculum, p.adjust.method = "none")

blad_t_test %>%
  ggplot(aes(x=inoculum, y=log_fold_change, color=inoculum, shape=inoculum)) +
  geom_line(aes(group=mouse),
            position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0,seed = 02122022),
            alpha = 0.25,
            color = "black",
            show.legend = FALSE) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=inoculum, y=mean_log_fold_change),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
 # scale_y_continuous(limits = c(-1,6),
  #                   breaks = c(0, 2, 4, 6),
   #                  labels = c(0, 2, 4, 6))+
  scale_x_discrete(limits = c("JV1_JV450", "JV1_JV462"),
                   labels = c("KPPR1 + PA14", bquote("KPPR1 + PA14"*Delta*italic(phzA-G)))) +
  scale_color_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c("#000080", "#000080")) +
  scale_shape_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c(16, 1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))+
  annotate("segment", x = c(1), xend = c(2), 
           y = c(4), yend = c(4), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=4.3), fill = NA, label.color = NA, label="*p* = 0.044" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/ex_vivo_comp_bladder_kp_fc.pdf"), height = 8, width = 5, unit = "cm")

all_blad_data %>%
  filter(media != "LB_rif") %>%
  ggplot(aes(x=inoculum, y=log_CFU_per_mL, color=inoculum, shape=inoculum)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0,seed = 02122022),
             alpha = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=inoculum, y=mean_log_CFU_per_mL),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote(Log[10]*"(CFU/mL)")) +
  scale_y_continuous(limits = c(6,12),
                     breaks = c(6,8,10,12),
                     labels = c(6,8,10,12))+
  scale_x_discrete(limits = c("JV1_JV450", "JV1_JV462"),
                   labels = c("KPPR1 + PA14", bquote("KPPR1 + PA14"*Delta*italic(phzA-G)))) +
  scale_color_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c("#000080", "#000080")) +
  scale_shape_manual(breaks = c("JV1_JV450", "JV1_JV462"),
                     values = c(16, 1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/ex_vivo_comp_blad_pa_cfus.pdf"), height = 8, width = 5, unit = "cm")
