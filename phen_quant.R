library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(DescTools)
library(EnvStats)

# set environment##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#================================================
# import data ##################################
MIC_MBC<-read_csv("combined_aerobic_MIC_MBC.csv")

MIC_anaero<-read_csv("combined_anaerobic_MIC.csv")

MIC_anaero_NO3<-read_csv("combined_anaerobic_NO3_MIC.csv")

pyo_std<-read_csv("PYO_std.csv")

JV537_std<-read_csv("5MPCA_std.csv")

PYO_5MPCA_check<-read_csv("PYO_5MPCA_checkerboard.csv")
#================================================
# graph data ##################################
ggplot(MIC_MBC, aes(condition, MIC_ug_per_mL, color = condition, shape = media)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_point(aes(x=condition, y=median_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote("MIC ("*mu*"g/mL)")) +
  guides(color = "none") +
  scale_y_continuous(limits = c(1.953125,3000),
                     breaks = c(1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(breaks = c("rif_fresh", "kan_fresh", "pyo_fresh", "pyo_spent"),
                   labels = c("Rifampicin", "Kanamycin", "Pyocyanin", "Pyocyanin")) +
  scale_color_manual(breaks = c("rif_fresh", "kan_fresh", "pyo_fresh", "pyo_spent"),
                     labels = c("Rifampicin", "Kanamycin", "Pyocyanin", "Pyocyanin"),
                     values = c("orange", "black", "#000080", "#000080")) +
  scale_shape_manual(name = "Media",
                     breaks = c("fresh", "spent"),
                     labels = c("Fresh", "Spent"),
                     values = c(16,1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/pyo_MIC.pdf"), height = 8, width = 7, unit = "cm")

ggplot(MIC_MBC, aes(condition, MBC_ug_per_mL, color = condition, shape = media)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_point(aes(x=condition, y=median_MBC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote("MBC ("*mu*"g/mL)")) +
  guides(color = "none") +
  scale_y_continuous(limits = c(1.953125,3000),
                     breaks = c(1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(breaks = c("rif_fresh", "kan_fresh", "pyo_fresh", "pyo_spent"),
                   labels = c("Rifampicin", "Kanamycin", "Pyocyanin", "Pyocyanin")) +
  scale_color_manual(breaks = c("rif_fresh", "kan_fresh", "pyo_fresh", "pyo_spent"),
                     labels = c("Rifampicin", "Kanamycin", "Pyocyanin", "Pyocyanin"),
                     values = c("orange", "black", "#000080", "#000080")) +
  scale_shape_manual(name = "Media",
                     breaks = c("fresh", "spent"),
                     labels = c("Fresh", "Spent"),
                     values = c(16,1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  geom_richtext(data=tibble(x=1, y=31), fill = NA, label.color = NA, label="2" ,aes(x=x, y=y), inherit.aes=FALSE, size=4) +
  geom_richtext(data=tibble(x=2, y=3000), fill = NA, label.color = NA, label="8" ,aes(x=x, y=y), inherit.aes=FALSE, size=4) +
  geom_richtext(data=tibble(x=3, y=1000), fill = NA, label.color = NA, label="32" ,aes(x=x, y=y), inherit.aes=FALSE, size=4) +
  geom_richtext(data=tibble(x=4, y=250), fill = NA, label.color = NA, label="1" ,aes(x=x, y=y), inherit.aes=FALSE, size=4)
ggsave(glue("graphs/pyo_MBC.pdf"), height = 8, width = 7, unit = "cm")

PYO_anaero_fresh_stats<-MIC_anaero%>%
  filter(media == "fresh",
         phenazine == "PYO") 
pairwise.t.test(PYO_anaero_fresh_stats$MIC, PYO_anaero_fresh_stats$oxygen, p.adjust.method = "none")
PCA_anaero_fresh_stats<-MIC_anaero%>%
  filter(media == "fresh",
         phenazine == "PCA") 
pairwise.t.test(PCA_anaero_fresh_stats$MIC, PCA_anaero_fresh_stats$oxygen, p.adjust.method = "none")
HP_anaero_fresh_stats<-MIC_anaero%>%
  filter(media == "fresh",
         phenazine == "1HP") 
pairwise.t.test(HP_anaero_fresh_stats$MIC, HP_anaero_fresh_stats$oxygen, p.adjust.method = "none")

MIC_anaero %>%
  filter(media == "fresh") %>%
  ggplot(aes(phenazine, MIC, color = phenazine, shape = oxygen, group = oxygen)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_point(aes(x=phenazine, y=median_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote("MIC ("*mu*"g/mL)")) +
  guides(color = "none") +
  scale_y_continuous(limits = c(1.953125,3000),
                     breaks = c(1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(limits = c("PCA", "1HP", "PYO"),
                   labels = c("PCA", "1-HP", "PYO")) +
  scale_color_manual(breaks = c("PCA", "1HP", "PYO"),
                     values = c("#E3AC36", "#CD9671", "#000080")) +
  scale_shape_manual(name = "Condition",
                     breaks = c("aerobic", "anaerobic"),
                     values = c(16, 17),
                     labels = c("Aerobic", "Anaerobic")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 8)) +
  geom_richtext(data=tibble(x=1, y=750), fill = NA, label.color = NA, label="*p* = 0.72" ,aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  geom_richtext(data=tibble(x=2, y=2000), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  geom_richtext(data=tibble(x=3, y=500), fill = NA, label.color = NA, label="*p* < 2e-16" ,aes(x=x, y=y), inherit.aes=FALSE, size=3)
ggsave(glue("graphs/phen_MIC_anaero_fresh.pdf"), height = 8, width = 7, unit = "cm")

PYO_anaero_spent_stats<-MIC_anaero%>%
  filter(media == "spent",
         phenazine == "PYO") 
pairwise.t.test(PYO_anaero_spent_stats$MIC, PYO_anaero_spent_stats$oxygen, p.adjust.method = "none")
PCA_anaero_spent_stats<-MIC_anaero%>%
  filter(media == "spent",
         phenazine == "PCA") 
pairwise.t.test(PCA_anaero_spent_stats$MIC, PCA_anaero_spent_stats$oxygen, p.adjust.method = "none")
HP_anaero_spent_stats<-MIC_anaero%>%
  filter(media == "spent",
         phenazine == "1HP") 
pairwise.t.test(HP_anaero_spent_stats$MIC, HP_anaero_spent_stats$oxygen, p.adjust.method = "none")

MIC_anaero %>%
  filter(media == "spent") %>%
  ggplot(aes(phenazine, MIC, color = phenazine, shape = oxygen, group = oxygen)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  geom_point(aes(x=phenazine, y=median_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote("MIC ("*mu*"g/mL)")) +
  guides(color = "none") +
  scale_y_continuous(limits = c(1.953125,3000),
                     breaks = c(1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(limits = c("PCA", "1HP", "PYO"),
                   labels = c("PCA", "1-HP", "PYO")) +
  scale_color_manual(breaks = c("PCA", "1HP", "PYO"),
                     values = c("#E3AC36", "#CD9671", "#000080")) +
  scale_shape_manual(name = "Condition",
                     breaks = c("aerobic", "anaerobic"),
                     values = c(21, 24),
                     labels = c("Aerobic", "Anaerobic")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 8)) +
  geom_richtext(data=tibble(x=1, y=750), fill = NA, label.color = NA, label="*p* = 0.12" ,aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  geom_richtext(data=tibble(x=2, y=2000), fill = NA, label.color = NA, label="*p* = 0.70" ,aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  geom_richtext(data=tibble(x=3, y=500), fill = NA, label.color = NA, label="*p* = 0.01" ,aes(x=x, y=y), inherit.aes=FALSE, size=3)
ggsave(glue("graphs/phen_MIC_anaero_spent.pdf"), height = 8, width = 7, unit = "cm")

pairwise.t.test(MIC_anaero_NO3$MIC, MIC_anaero_NO3$media, p.adjust.method = "none")

MIC_anaero_NO3 %>%
  ggplot(aes(media, MIC, color = phenazine, shape = media)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             color = "#000080",
             show.legend = FALSE) +
  geom_point(aes(x=media, y=median_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = bquote("MIC ("*mu*"g/mL)")) +
  guides(color = "none") +
  scale_y_continuous(limits = c(1.953125,3000),
                     breaks = c(1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(limits = c("M9-glu", "M9-glu-N"),
                   labels = c("M9 minimal medium<br>+ 0.5% glucose", "M9 minimal medium<br>+ 0.5% glucose + 10 mM nitrate")) +
  scale_shape_manual(name = "Condition",
                     breaks = c("M9-glu", "M9-glu-N"),
                     values = c(17, 18)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey")) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(62.5), yend = c(62.5), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=100), fill = NA, label.color = NA, label="*p* = 1.00" ,aes(x=x, y=y), inherit.aes=FALSE, size=3)
ggsave(glue("graphs/phen_MIC_NO3_anaero_spent.pdf"), height = 10, width = 6, unit = "cm")

PYO_5MPCA_check %>%
  filter(JV537_MIC == "Y") %>%
  mutate(JV537_dil = as.numeric(JV537_dil)) %>%
  mutate(mean_JV537_MIC = mean(JV537_dil)) %>%
  ggplot(aes(JV537_MIC, JV537_dil, color = JV537_MIC)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             shape = 1,
             show.legend = FALSE) +
  geom_point(aes(x=JV537_MIC, y=mean_JV537_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8) +
  labs(x = NULL, y = "MIC (% spent media)") +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(0,25,50,75,100))+
  scale_x_discrete(breaks = c("Y"),
                   labels = c("PYR")) +
  scale_color_manual(breaks = c("Y"),
                     values = c("#990000")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/5MPCA_MIC.pdf"), height = 8, width = 3.5, unit = "cm")

ggplot(PYO_5MPCA_check, aes(x=factor(JV537_dil, level = c(0, 1.5625, 3.125, 6.25,12.5, 25, 50, 100)), 
                            y=PYO_MIC, color = JV537_dil)) +
  geom_hline(yintercept = 0.4882812, color = "red", linetype = "dashed") +
  geom_line(aes(group = trial),
            position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0, seed = 02122022),
            #color = "black",
            alpha = 0.25,
            show.legend = FALSE) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0, seed = 02122022),
             alpha = 1,
             shape = 1,
             size = 2.5,
             show.legend = FALSE) +
  geom_point(aes(x=JV537_dil, y=mean_PYO_MIC),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = "% MG1655pPhzA-GS*M (PYR) spent media", y = bquote("PYO MIC ("*mu*"g/mL)")) +
  scale_y_continuous(limits = c(0.4882812,3000),
                     breaks = c(0.4882812, 1.953125,7.8125,31.25,125,500,2000),
                     labels = scales::number_format(accuracy = 1, big.mark = ","),
                     trans="log2")+
  scale_x_discrete(breaks = c(0, 1.5625, 3.125, 6.25,12.5, 25, 50, 100),
                   labels = c(0, 1.56, 3.13, 6.25,12.5, 25, 50, 100)) +
  scale_color_manual(breaks = c(0, 1.5625, 3.125, 6.25,12.5, 25, 50, 100),
                     values = c("#000080", "#19198c", "#323299", "#4c4ca6", "#6666b2", "#7f7fbf", "#9999cc", "#b2b2d8")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/5MPCA_pyo_checkerboard.pdf"), height = 8, width = 10, unit = "cm")

std_levels<-c("std","PA01", "Pa14", "JV3","JV4","JV5")
std_labels<-c("Standard", "PAO1", "PA14", "145.1", "191.1", "193.1")
std_shapes<-c(16,15,15,15,15,15)
std_colors<-c("grey", "#28AB87", "#000080","#0147AB", "#008EEC", "#42E0D1")

ggplot(pyo_std, aes(median_pyo_ug_mL, median_OD_695, color = label, shape = label)) +
  geom_point(alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  labs(x = bquote(mu*"g/mL pyocyanin"), y = bquote(Abs[695])) +
  scale_x_continuous(limits = c(0.1, 300),
                     breaks = c(0.1, 1, 10, 100),
                     labels = c(0.1, 1, 10, 100),
                     trans = "log10") +
  scale_y_continuous(limits = c(0, 1.5),
                     breaks = c(0, 0.5,1,1.5),
                     labels = c(0, 0.5,1,1.5)) +
  scale_color_manual(name = "Sample", 
                     breaks = std_levels,
                     labels = std_labels,
                     values = std_colors) +
  scale_shape_manual(name = "Sample", 
                     breaks = std_levels,
                     labels = std_labels,
                     values = std_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/pyo_std_curve.pdf"), height = 7, width = 12, unit = "cm")

ggplot(JV537_std, aes(median_per_JV537, median_OD_500, color = label, shape = label)) +
  geom_point(alpha = 1,
             size = 2.5,
             show.legend = TRUE) +
  labs(x = "% MG1655pPhzA-GS*M spent media", y = bquote(Abs[500])) +
  scale_x_continuous(limits = c(1, 100),
                     breaks = c(1.56,6.25,25,100),
                     labels = c(1.56,6.25,25,100),
                     trans = "log2") +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = c(0, 0.1,0.2,0.3),
                     labels = c(0, 0.1,0.2,0.3)) +
  scale_color_manual(name = "Sample", 
                     breaks = std_levels,
                     labels = std_labels,
                     values = std_colors) +
  scale_shape_manual(name = "Sample", 
                     breaks = std_levels,
                     labels = std_labels,
                     values = std_shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/5MPCA_std_curve.pdf"), height = 7, width = 12, unit = "cm")

