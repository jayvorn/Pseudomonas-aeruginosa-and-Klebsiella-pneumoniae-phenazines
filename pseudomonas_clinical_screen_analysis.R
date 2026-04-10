library(readxl)
library(tidyverse)
library(glue)
library(dplyr)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(DescTools)
library(ggrepel)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#===================================================================
# import data ##################################
all_pa_screen_data <- read_csv("combined_clin_pa_restrict_data.csv")

val_data<-read_csv("combined_clin_Pa_validation.csv")

combined_clin_spent_data<-read_csv("combined_clin_Pa_spent_data.csv")

PYO_5MPCA_spectra<-read_csv("5MPCA_PYO_spectra.csv") %>%
  pivot_longer(cols = c("5MPCA_reduced", "5MPCA", "5MPCA_oxy", "PYO_reduced", "PYO", "PYO_oxy"), values_to = "absorbance") %>%
  filter(Wavelength != "300" & Wavelength != "320")

OD500_data<-read_csv("Pa_clin_OD500.csv")

clin_phen_absord_data <- read_csv("combined_clin_phen_absord_data.csv")
#===================================================================
# define aesthetics ##################################
PYO_5MPCA_breaks<-c("5MPCA_reduced", "5MPCA", "5MPCA_oxy", "PYO_reduced", "PYO", "PYO_oxy")
PYO_5MPCA_labels<-c("MG1655pPhzA-GS*M<br>spent media + 5mM DTT", "MG1655pPhzA-GS*M<br>spent media", "MG1655pPhzA-GS*M<br>spent media + 0.3% peroxide",
                   "Pyocyanin<br>+ 5mM DTT", "Pyocyanin", "Pyocyanin<br> + 0.3% peroxide")
PYO_5MPCA_colors<-c("#FFDA03", "#960019", "#FF0800", "#C6DBEF", "#00CEC8", "#000080")
#===================================================================
# graph screen data ##################################
JV1_alone_mean<-all_pa_screen_data %>%
  filter(Sample == "KPPR1") %>%
  mutate(mean_rel_fluor = mean(mean_rel_fluor)) %>%
  .[1,17]
Pa14_mean<-all_pa_screen_data %>%
  filter(Sample == "Pa14") %>%
  mutate(mean_rel_fluor = mean(mean_rel_fluor)) %>%
  .[1,17]

all_pa_screen_data %>%
  filter(Sample != "KPPR1" & Sample != "Pa14") %>%
  select(Sample, Group, mean_rel_fluor) %>%
  distinct() %>%
  ggplot() +
  geom_hline(yintercept = 79452, color = "red", linetype = "solid") +
  geom_hline(yintercept = 29785, color = "orange", linetype = "solid") +
  geom_jitter(aes(x=Group, y=mean_rel_fluor, color = Group),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_richtext(data=tibble(x=1.35, y=79452), fill = "red", label.color = NA, label="KPPR1<br>alone" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_richtext(data=tibble(x=1.4, y=29785), fill = "orange", label.color = NA, label="KPPR1<br>+ PA14" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  labs(x = NULL, y = "Normalized fluorescense (A.U.)") +
  scale_y_continuous(limits = c(10000, 115000),
                     breaks = c(10000, 30000, 50000, 70000, 90000, 110000),
                     labels = c(10000, 30000, 50000, 70000, 90000, 110000)) +
  scale_x_discrete(breaks = c("exp"),
                   labels = c(bquote("Clinical Pa"))) +
  scale_color_manual(breaks = c("exp"), 
                     values = c("#42f5d7")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen.pdf"), height = 8, width = 6, unit = "cm")

restriction_site_data<-all_pa_screen_data%>%
  filter(Sample != "KPPR1" & Sample != "Pa14") %>%
  filter(Clin_code != "Other") %>%
#  filter(Clin_code != "Wound") %>%
#  filter(Clin_code != "Blood") %>%
  select(Sample, Clin_code, mean_rel_fluor) %>%
  mutate(log_mean_rel_fluor = log(mean_rel_fluor, 10)) %>%
  group_by(Clin_code) %>%
  mutate(mean_group_rel_fluor = mean(mean_rel_fluor)) %>%
  ungroup() %>%
  mutate(urine = case_when(Clin_code == "Urine" ~ "Y",
                           Clin_code != "Urine" ~ "N")) %>%
  group_by(urine) %>%
  mutate(mean_urine_rel_fluor = mean(mean_rel_fluor)) %>%
  ungroup() %>%
  ungroup() %>%
  mutate(lung = case_when(Clin_code == "Respiratory" ~ "Y",
                           Clin_code != "Respiratory" ~ "N")) %>%
  group_by(lung) %>%
  mutate(mean_lung_rel_fluor = mean(mean_rel_fluor)) %>%
  ungroup() %>%
  unique()

TukeyHSD(aov(log_mean_rel_fluor ~ Clin_code, data = restriction_site_data))
pairwise.t.test(log(restriction_site_data$mean_rel_fluor, 10), restriction_site_data$urine, p.adjust.method = "none")
pairwise.t.test(log(restriction_site_data$mean_rel_fluor, 10), restriction_site_data$lung, p.adjust.method = "none")

restriction_site_data %>%
  ggplot() +
  geom_hline(yintercept = 79452, color = "red", linetype = "solid") +
  geom_hline(yintercept = 29785, color = "orange", linetype = "solid") +
  geom_jitter(aes(x=Clin_code, y=mean_rel_fluor, color = Clin_code),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=Clin_code, y=mean_group_rel_fluor),
             shape = 95,
             size = 8) +
  geom_richtext(data=tibble(x=4.35, y=79452), fill = "red", label.color = NA, label="KPPR1<br>alone" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_richtext(data=tibble(x=4.4, y=29785), fill = "orange", label.color = NA, label="KPPR1<br>+ Pa14" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  labs(x = "Site", y = "Normalized fluorescense (A.U.)") +
  scale_y_continuous(limits = c(5000, 115000),
                     breaks = c(10000, 30000, 50000, 70000, 90000, 110000),
                     labels = c(10000, 30000, 50000, 70000, 90000, 110000)) +
  scale_x_discrete(limits = c("Blood", "Respiratory", "Urine", "Wound"),
                   labels = c("Blood", "Respiratory", "Urine", "Wound")) +
  scale_color_manual(breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c("red", "blue", "#DA9100", "purple")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,2,3,2), xend = c(4,3,4,4), 
           y = c(8000, 100000, 15000,107000), yend = c(8000, 100000, 15000, 107000), 
           color = "black") +
  geom_richtext(data=tibble(x=2.5, y=11000), fill = NA, label.color = NA, label="*p* = 0.15" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2.5, y=103000), fill = NA, label.color = NA, label="*p* = 0.19" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3.5, y=18000), fill = NA, label.color = NA, label="*p* = 3.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=110000), fill = NA, label.color = NA, label="*p* = 0.30" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_screen_site.pdf"), height = 8, width = 12, unit = "cm")

restriction_site_data %>%
  ggplot() +
  geom_jitter(aes(x=urine, y=mean_rel_fluor, color = urine),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=urine, y=mean_urine_rel_fluor),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = "Normalized fluorescense (A.U.)") +
  scale_y_continuous(limits = c(5000, 115000),
                     breaks = c(10000, 30000, 50000, 70000, 90000, 110000),
                     labels = c(10000, 30000, 50000, 70000, 90000, 110000)) +
  scale_x_discrete(limits = c("Y", "N"),
                   labels = c("Urine", "Non-urine")) +
  scale_color_manual(breaks = c("Y", "N"), 
                     values = c("#DA9100", "grey")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))  +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(100000), yend = c(100000), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=105000), fill = NA, label.color = NA, label="*p* = 5.8e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_screen_urine.pdf"), height = 8, width = 6, unit = "cm")

restriction_site_data %>%
  ggplot() +
  geom_jitter(aes(x=lung, y=mean_rel_fluor, color = lung),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=lung, y=mean_lung_rel_fluor),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = "Normalized fluorescense (A.U.)") +
  scale_y_continuous(limits = c(5000, 115000),
                     breaks = c(10000, 30000, 50000, 70000, 90000, 110000),
                     labels = c(10000, 30000, 50000, 70000, 90000, 110000)) +
  scale_x_discrete(limits = c("Y", "N"),
                   labels = c("Lung", "Non-lung")) +
  scale_color_manual(breaks = c("Y", "N"), 
                     values = c("blue", "grey")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))  +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(100000), yend = c(100000), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=105000), fill = NA, label.color = NA, label="*p* = 0.42" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_screen_lung.pdf"), height = 8, width = 6, unit = "cm")
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
  scale_x_discrete(limits = c("JV1_LB", "JV1_JV36", "JV1_JV38", "JV1_JV40", "JV1_JV46", "JV1_JV65",
                              "JV1_JV69", "JV1_JV201", "JV1_JV202", "JV1_JV257", "JV1_JV273"), 
                   labels = c("KPPR1 alone", "KPPR1 + JV36", "KPPR1 + JV38", "KPPR1 + JV40", "KPPR1 + JV46", "KPPR1 + JV65",
                              "KPPR1 + JV69", "KPPR1 + JV201", "KPPR1 + JV202", "KPPR1 + JV257", "KPPR1 + JV273")) +
  scale_color_manual(breaks = c("JV1_LB", "JV1_JV36", "JV1_JV38", "JV1_JV40", "JV1_JV46", "JV1_JV65",
                                "JV1_JV69", "JV1_JV201", "JV1_202", "JV1_JV257", "JV1_JV273"),
                     labels = c("KPPR1 alone", "KPPR1 + JV36", "KPPR1 + JV38", "KPPR1 + JV40", "KPPR1 + JV46", "KPPR1 + JV65",
                                "KPPR1 + JV69", "KPPR1 + JV201", "KPPR1 + JV202", "KPPR1 + JV257", "KPPR1 + JV273"),
                     values = c("black", "#0a2d2e", "#1c4e4f", "#436e6f", "#6a8e8f", "#879693",
                                "#a49e97", "#deae9f", "#efd7cf", "#f7ebe7", "black")) +
  scale_shape_manual(breaks = c("JV1_LB", "JV1_JV36", "JV1_JV38", "JV1_JV40", "JV1_JV46", "JV1_JV65",
                                "JV1_JV69", "JV1_JV201", "JV1_JV202", "JV1_JV257", "JV1_JV273"),
                     labels = c("KPPR1 alone", "KPPR1 + JV36", "KPPR1 + JV38", "KPPR1 + JV40", "KPPR1 + JV46", "KPPR1 + JV65",
                                "KPPR1 + JV69", "KPPR1 + JV201", "KPPR1 + JV202", "KPPR1 + JV257", "KPPR1 + JV273"),
                     values = c(15,16,16,16,16,16,
                                16,16,16,16,1)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  geom_richtext(data=tibble(x=2, y=4.2), fill = NA, label.color = NA, label="*p* = 0.09" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=3, y=4.6), fill = NA, label.color = NA, label="*p* = 0.14" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=4, y=2.4), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=5, y=2.8), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=6, y=2.4), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=7, y=2.8), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=8, y=1.4), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=9, y=4.2), fill = NA, label.color = NA, label="*p* = 7.7e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=10, y=1.6), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=11, y=4.6), fill = NA, label.color = NA, label="*p* = 0.99" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_val.pdf"), height = 8, width = 12, unit = "cm")

all_pa_screen_data %>%
  filter(Sample == "JV36" |
           Sample == "JV38" |
           Sample == "JV40" |
           Sample == "JV46" |
           Sample == "JV65"|
           Sample == "JV69"|
           Sample == "JV201"|
           Sample == "JV202"|
           Sample == "JV257"|
           Sample == "JV273") %>%
  select(Sample, Group, mean_rel_fluor) %>%
  distinct() %>%
  ggplot() +
  geom_hline(yintercept = 79452, color = "red", linetype = "solid") +
  geom_hline(yintercept = 29785, color = "orange", linetype = "solid") +
  geom_point(aes(x=Group, y=mean_rel_fluor, color = Group),
             alpha = 1,
             shape = 1,
             size = 1,
             show.legend = FALSE) + 
  geom_text_repel(aes(x=Group, y=mean_rel_fluor,label = Sample), 
                  size = 3) +
  labs(x = NULL, y = "Normalized fluorescense (A.U.)") +
  scale_y_continuous(limits = c(10000, 115000),
                     breaks = c(10000, 30000, 50000, 70000, 90000, 110000),
                     labels = c(10000, 30000, 50000, 70000, 90000, 110000)) +
  scale_x_discrete(breaks = c("exp"),
                   labels = c(bquote("Clinical Pa"))) +
  scale_color_manual(breaks = c("exp"), 
                     values = c("#42f5d7")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen_val_only.pdf"), height = 8, width = 6, unit = "cm")

# plot clinical spent cultures
#select mouse T24 spent data
clin_data <- combined_clin_spent_data %>%
  filter(Timepoint_hr == "T24")
# run stats
clin_aov <- TukeyHSD(aov(log_fold_change ~ Condition, data = clin_data))
# plot data
ggplot(clin_data, aes(x=Condition, y=log_fold_change, fill = Condition, shape = Condition, color = LOD)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 1,
             size = 2.5,
             shape = 22,
             show.legend = FALSE) +
  geom_point(aes(x=Condition, y=mean_log_fold_change, group = Timepoint_hr),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 8,
             show.legend = FALSE) +
  labs(x = NULL, y = bquote(Log~fold~change["24 hours"])) +
  scale_y_continuous(limits = c(-5,5),
                     breaks = c(-4,-2, 0, 2, 4),
                     labels = c(-4,-2, 0, 2, 4)) +
  scale_x_discrete(limits = c("JV1_LB", "JV1_self_spent", "JV1_JV450_spent", "JV1_JV46_spent", "JV1_JV69_spent"), 
                   labels = c("Fresh media","KPPR1<br>spent media", "PA14<br>spent media","JV46<br>spent media", "JV69<br>spent media")) +
  scale_fill_manual(breaks = c("JV1_LB", "JV1_self_spent", "JV1_JV450_spent", "JV1_JV46_spent", "JV1_JV69_spent"), 
                    values = c("black","black","#000080", "#6667AB", "#FA7268")) +
  scale_color_manual(name = "LOD",
                     breaks = c("Y","N"), 
                     labels = c("Yes", "No"),
                     values = c("red", "black")) +
  scale_shape_manual(breaks = c("JV1_LB", "JV1_self_spent", "JV1_JV450_spent", "JV1_JV46_spent", "JV1_JV69_spent"),
                     values = c(2, 15, 15, 15,15)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1,1), xend = c(2,3), 
           y = c(1.9,2.6), yend = c(1.9,2.6), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=2.2), fill = NA, label.color = NA, label="*p* < 1e-7" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5) +
  geom_richtext(data=tibble(x=2, y=2.9), fill = NA, label.color = NA, label="*p* = 4.6e-4" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave("graphs/clin_Pa_spent_culture_val.pdf", height = 8, width = 8, unit = "cm")
#================================================
# graph spectra ##################################
PYO_5MPCA_spectra %>%
  ggplot(aes(x = Wavelength, y = absorbance, color = name)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 500, linewidth = 2, color = "red", alpha = 0.4) +
  geom_vline(xintercept = 695, linewidth = 2, color = "red", alpha = 0.4) +
  labs(x = "Wavelength", y = "Absorbance") +
  scale_color_manual(name = "Compound",
                       breaks = PYO_5MPCA_breaks,
                     labels = PYO_5MPCA_labels, 
                     values = PYO_5MPCA_colors) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 7))
ggsave("graphs/PYO_5MPCA_spectra.pdf", height = 10, width = 15, unit = "cm")
#================================================
# graph phen data ##################################
clin_phen_absord_data %>%
  filter(OD == "OD695") %>%
  ggplot(aes(x=OD, y=mean_absorb, color = condition, shape = condition)) +
  geom_jitter(alpha = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = TRUE) + 
  labs(x = NULL, y = bquote(Abs[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c(1, 16, 0)) +
  scale_color_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c("darkgrey", "#000080", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_text(size = 10)) 
ggsave(glue("graphs/clin_Pa_695.pdf"), height = 8, width = 10, unit = "cm")

clin_phen_absord_data %>%
  filter(OD == "OD500") %>%
  select(strain, OD, condition, mean_absorb) %>%
  distinct() %>%
  ggplot(aes(x=OD, y=mean_absorb, color = condition, shape = condition)) +
  geom_jitter(alpha = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = TRUE) + 
  labs(x = NULL, y = bquote(Abs[500])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G")), "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c(1, 16, 0, 15, 15)) +
  scale_color_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G")), "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c("darkgrey", "#000080", "#000080","#990000", "#00CEC8")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_text(size = 10)) 
ggsave(glue("graphs/clin_Pa_500.pdf"), height = 8, width = 10, unit = "cm")

clin_phen_absord_data %>%
  pivot_wider(names_from = OD, values_from = mean_absorb) %>%
  mutate(mean_sum_absorb = (OD500+OD695)) %>%
  mutate(x = "x") %>%
  filter(condition == "exp" | condition == "pos_ctl" | condition == "neg_ctl") %>%
  ggplot(aes(x = x, y=mean_sum_absorb, color = condition, shape = condition)) +
  geom_jitter(alpha = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = TRUE) + 
  labs(x = NULL, y = bquote(Abs[500]~"+"~Abs[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c(1, 16, 0)) +
  scale_color_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c("darkgrey", "#000080", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_text(size = 10)) 
ggsave(glue("graphs/clin_Pa_sum_500_695.pdf"), height = 8, width = 10, unit = "cm")

clin_phen_absord_data %>%
  pivot_wider(names_from = OD, values_from = mean_absorb) %>%
  mutate(mean_ratio_absorb = (OD500/OD695)) %>%
  mutate(x = "x") %>%
  filter(condition == "exp" | condition == "pos_ctl" | condition == "neg_ctl") %>%
  ggplot(aes(x = x, y=mean_ratio_absorb, color = condition, shape = condition)) +
  geom_jitter(alpha = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = TRUE) + 
  labs(x = NULL, y = bquote(OD[500]~"/"~OD[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c(1, 16, 0)) +
  scale_color_manual(name = "Strain",
                     breaks = c("exp", "pos_ctl", "neg_ctl"), 
                     labels = c("Clinical isolate", "PA14", bquote("PA14"*Delta*italic("phzA-G"))),
                     values = c("darkgrey", "#000080", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_text(size = 10)) 
ggsave(glue("graphs/clin_Pa_ratio_500_695.pdf"), height = 8, width = 10, unit = "cm")
#================================================
# graph phen corr data ##################################
clean_screen_data<-all_pa_screen_data %>%
  select(Sample, mean_rel_fluor) %>%
  rename(strain = Sample) %>%
  mutate(strain = case_when(strain == "Pa14" ~ "PA14",
                            TRUE ~ strain)) %>%
  filter(strain != "KPPR1") %>%
  unique()

clean_phen_data<-clin_phen_absord_data %>%
  filter(condition == "pos_ctl" | condition == "exp") %>%
  select(strain, condition, OD, mean_absorb) %>%
  mutate(condition = case_when(condition == "pos_ctl" ~ "PA14", 
                               TRUE ~ "Clinical isolate")) %>%
  mutate(strain = case_when(condition == "PA14" ~ "PA14",
                             TRUE ~ strain))

corr_data_all<-left_join(clean_phen_data, clean_screen_data, by = "strain") %>%
  filter(strain != "JV273" & strain != "JV424")%>%
  select(strain, OD, condition, mean_absorb, mean_rel_fluor) %>%
  distinct() %>%
  pivot_wider(names_from = OD, values_from = c(mean_absorb)) %>%
  mutate(mean_ratio_absorb = OD500/OD695) %>%
  mutate(mean_sum_absorb = OD500 + OD695)

corr_data_500<-left_join(clean_phen_data, clean_screen_data, by = "strain") %>%
  filter(OD == "OD500") %>%
  filter(strain != "JV273" & strain != "JV424")
  
corr_data_695<-left_join(clean_phen_data, clean_screen_data, by = "strain") %>%
  filter(OD == "OD695") %>%
  filter(strain != "JV273" & strain != "JV424")

cor.test(corr_data_500$mean_absorb, corr_data_500$mean_rel_fluor, method = "spearman")
cor.test(corr_data_695$mean_absorb, corr_data_695$mean_rel_fluor, method = "spearman")
cor.test(corr_data_all$mean_sum_absorb, corr_data_all$mean_rel_fluor, method = "spearman")
cor.test(corr_data_all$OD500, corr_data_all$OD695,method = "spearman")
cor.test(corr_data_all$mean_ratio_absorb, corr_data_all$mean_rel_fluor,method = "spearman")

ggplot(corr_data_500, aes(x = mean_absorb, y = mean_rel_fluor, color = condition, shape = condition)) +
  geom_smooth(method = "lm", se = FALSE, color = "#990000", linewidth = 0.5) +
  geom_point(alpha = 1,
             size = 1.5,
             show.legend = TRUE) +
  geom_richtext(data=tibble(x=0.1, y=90000), fill = NA, label.color = NA, label="*r* = -0.43, *p* = 3.6e-10",
                aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(y = "Normalized fluorescense (A.U.)", x = bquote(Abs[500])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c(1, 16)) +
  scale_color_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c("darkgrey", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_500_screen_corr.pdf"), height = 9, width = 13, unit = "cm")

ggplot(corr_data_695, aes(x = mean_absorb, y = mean_rel_fluor, color = condition, shape = condition)) +
  geom_smooth(method = "lm", se = FALSE, color = "#00CEC8", linewidth = 0.5) +
  geom_point(alpha = 1,
             size = 1.5,
             show.legend = TRUE) +
  geom_richtext(data=tibble(x=0.07, y=90000), fill = NA, label.color = NA, label="*r* = -0.43, *p* = 6.9e-10",
                aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(y = "Normalized fluorescense (A.U.)", x = bquote(Abs[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c(1, 16)) +
  scale_color_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c("darkgrey", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_695_screen_corr.pdf"), height = 9, width = 13, unit = "cm")

ggplot(corr_data_all, aes(x = mean_sum_absorb, y = mean_rel_fluor, color = condition, shape = condition)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_point(alpha = 1,
             size = 1.5,
             show.legend = TRUE) +
  geom_richtext(data=tibble(x=0.1, y=90000), fill = NA, label.color = NA, label="*r* = -0.48, *p* = 2.7e-12",
                aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(y = "Normalized fluorescense (A.U.)", x = bquote(Abs[500]~"+"~Abs[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c(1, 16)) +
  scale_color_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c("darkgrey", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_sum_500_695_screen_corr.pdf"), height = 9, width = 13, unit = "cm")

corr_data_all %>%
  ggplot(aes(x=OD500, y=OD695, color = condition, shape = condition)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_point(alpha = 1,
             size = 1.5,
             show.legend = TRUE) + 
  geom_richtext(data=tibble(x=0.14, y=0.13), fill = NA, label.color = NA, label="*r* = 0.58, *p* < 2.2e-16",
                aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(x = bquote(Abs[500]), y = bquote(Abs[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c(1, 16)) +
  scale_color_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c("darkgrey", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_500_695_corr.pdf"), height = 8, width = 12, unit = "cm")

corr_data_all %>%
  ggplot(aes(x=mean_ratio_absorb, y=mean_rel_fluor, color = condition, shape = condition)) +
  geom_vline(xintercept = 1, color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  geom_point(alpha = 1,
             size = 1.5,
             show.legend = TRUE) + 
  geom_richtext(data=tibble(x=40, y=90000), fill = NA, label.color = NA, label="*r* = 0.25, *p* = 3.7e-4",
                aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(y = "Normalized fluorescense (A.U.)", x = bquote(OD[500]~"/"~OD[695])) +
  scale_shape_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c(1, 16)) +
  scale_color_manual(name = "Strain",
                     breaks = c("Clinical isolate", "PA14"), 
                     labels = c("Clinical isolate", "PA14"),
                     values = c("darkgrey", "#000080")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_OD_ratio_corr.pdf"), height = 8, width = 12, unit = "cm")
#================================================
# graph phen site data ##################################
phen_site_data<-clin_phen_absord_data%>%
  filter(condition == "exp") %>%
  filter(strain != "JV424") %>%
  left_join(., clinical_sites, by = c("strain" = "Sample")) %>%
  filter(Clin_code != "Other") %>%
  select(strain, Clin_code, OD, mean_absorb) %>%
  pivot_wider(names_from = OD, values_from = mean_absorb)  %>%
  mutate(sum_absorb = OD500 + OD695) %>%
  group_by(Clin_code) %>%
  mutate(mean_group_OD500 = mean(OD500)) %>%
  mutate(mean_group_OD695 = mean(OD695)) %>%
  mutate(mean_sum_absorb = mean(sum_absorb)) %>%
  ungroup() %>%
  mutate(urine = case_when(Clin_code == "Urine" ~ "Y",
                           Clin_code != "Urine" ~ "N")) %>%
  group_by(urine) %>%
  mutate(mean_urine_OD500 = mean(OD500)) %>%
  mutate(mean_urine_OD695 = mean(OD695)) %>%
  ungroup() %>%
  unique()

TukeyHSD(aov(log(OD500,10) ~ Clin_code, data = phen_site_data))
TukeyHSD(aov(log(OD695,10) ~ Clin_code, data = phen_site_data))
TukeyHSD(aov(log(sum_absorb,10) ~ Clin_code, data = phen_site_data))
pairwise.t.test(phen_site_data$OD500, phen_site_data$urine)
pairwise.t.test(phen_site_data$OD695, phen_site_data$urine)

phen_site_data %>%
  ggplot() +
  geom_jitter(aes(x=Clin_code, y=OD500, color = Clin_code),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=Clin_code, y=mean_group_OD500),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = bquote(Abs[500])) +
  scale_x_discrete(limits = c("Blood", "Respiratory", "Urine", "Wound"),
                   labels = c("Blood", "Respiratory", "Urine", "Wound")) +
  scale_color_manual(breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c("red", "blue", "#DA9100", "purple")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen_OD500_site.pdf"), height = 8, width = 12, unit = "cm")

phen_site_data %>%
  ggplot() +
  geom_jitter(aes(x=Clin_code, y=OD695, color = Clin_code),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=Clin_code, y=mean_group_OD695),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = bquote(Abs[695])) +
  scale_x_discrete(limits = c("Blood", "Respiratory", "Urine", "Wound"),
                   labels = c("Blood", "Respiratory", "Urine", "Wound")) +
  scale_color_manual(breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c("red", "blue", "#DA9100", "purple")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen_OD695_site.pdf"), height = 8, width = 12, unit = "cm")

phen_site_data %>%
  ggplot() +
  geom_jitter(aes(x=Clin_code, y=sum_absorb, color = Clin_code),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=Clin_code, y=mean_sum_absorb),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = bquote(Abs[500]~"+"~Abs[695])) +
  scale_x_discrete(limits = c("Blood", "Respiratory", "Urine", "Wound"),
                   labels = c("Blood", "Respiratory", "Urine", "Wound")) +
  scale_color_manual(breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c("red", "blue", "#DA9100", "purple")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen_OD695_OD500_site.pdf"), height = 8, width = 12, unit = "cm")

site_corr_data<-phen_site_data %>%
  rename(Sample = strain) %>%
  left_join(., restriction_site_data, by = "Sample") %>%
  select(Sample, Clin_code.x, mean_rel_fluor, OD500, OD695, sum_absorb) %>%
  rename(Clin_code = Clin_code.x)

urine_site_corr_data <- site_corr_data %>%
  filter(Clin_code == "Urine")
cor.test(urine_site_corr_data$sum_absorb, urine_site_corr_data$mean_rel_fluor, method = "spearman")
lung_site_corr_data <- site_corr_data %>%
  filter(Clin_code == "Respiratory")
cor.test(lung_site_corr_data$sum_absorb, lung_site_corr_data$mean_rel_fluor, method = "spearman")
blood_site_corr_data <- site_corr_data %>%
  filter(Clin_code == "Blood")
cor.test(blood_site_corr_data$sum_absorb, blood_site_corr_data$mean_rel_fluor, method = "spearman")
wound_site_corr_data <- site_corr_data %>%
  filter(Clin_code == "Wound")
cor.test(wound_site_corr_data$sum_absorb, wound_site_corr_data$mean_rel_fluor, method = "spearman")

ggplot() +
  geom_point(data=site_corr_data, aes(x=sum_absorb, y=mean_rel_fluor, color = Clin_code, shape = Clin_code),
             alpha = 1,
             size = 1.5,
             show.legend = TRUE) +
  geom_smooth(data=urine_site_corr_data, aes(x=sum_absorb, y=mean_rel_fluor),
              method = lm, 
              se = FALSE,
              color = "#DA9100") +
  geom_richtext(data=tibble(x=0.17, y=78000), fill = "yellow", label.color = NA,
                label="Urine: r = -0.38, *p* = 4.0e-3" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_smooth(data=lung_site_corr_data, aes(x=sum_absorb, y=mean_rel_fluor),
              method = lm, 
              se = FALSE,
              color = "blue") +
  geom_richtext(data=tibble(x=0.17, y=84000), fill = "lightblue", label.color = NA,
                label="Respiratory: r = -0.55, *p* = 2.6e-5" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_smooth(data=blood_site_corr_data, aes(x=sum_absorb, y=mean_rel_fluor),
              method = lm, 
              se = FALSE,
              color = "red") +
  geom_richtext(data=tibble(x=0.17, y=90000), fill = "pink", label.color = NA,
                label="Blood: r = -0.77, *p* = 0.1" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  geom_smooth(data=wound_site_corr_data, aes(x=sum_absorb, y=mean_rel_fluor),
              method = lm, 
              se = FALSE,
              color = "purple") +
  geom_richtext(data=tibble(x=0.17, y=72000), fill = "lavender", label.color = NA,
                label="Wound: r = -0.23, *p* = 0.22" ,aes(x=x, y=y), inherit.aes=FALSE, size=2) +
  labs(y = "Normalized fluorescense (A.U.)", x = bquote(Abs[500]~"+"~Abs[695])) +
  scale_shape_manual(name = "Site",
                     breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c(0,1,2,3)) +
  scale_color_manual(name = "Site",
                     breaks = c("Blood", "Respiratory", "Urine", "Wound"), 
                     values = c("red", "blue", "#DA9100", "purple")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/clin_Pa_screen_OD695_OD500_site_corr.pdf"), height = 8, width = 14, unit = "cm")

phen_site_data %>%
  ggplot() +
  geom_jitter(aes(x=urine, y=OD500, color = urine),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=urine, y=mean_urine_OD500),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = bquote(Abs[500])) +
  scale_x_discrete(limits = c("Y", "N"),
                   labels = c("Urine", "Non-urine")) +
  scale_color_manual(breaks = c("Y", "N"), 
                     values = c("#DA9100", "grey")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(0.2), yend = c(0.2), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=0.215), fill = NA, label.color = NA, label="*p* = 0.44" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_screen_OD500_urine.pdf"), height = 8, width = 6, unit = "cm")

phen_site_data %>%
  ggplot() +
  geom_jitter(aes(x=urine, y=OD695, color = urine),
              alpha = 1,
              shape = 1,
              size = 1.5,
              width = 0.15, 
              show.legend = FALSE) + 
  geom_point(aes(x=urine, y=mean_urine_OD695),
             shape = 95,
             size = 8) +
  labs(x = "Site", y = bquote(Abs[695])) +
  scale_x_discrete(limits = c("Y", "N"),
                   labels = c("Urine", "Non-urine")) +
  scale_color_manual(breaks = c("Y", "N"), 
                     values = c("#DA9100","grey")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))  +
  annotate("segment", x = c(1), xend = c(2), 
           y = c(0.15), yend = c(0.15), 
           color = "black") +
  geom_richtext(data=tibble(x=1.5, y=0.16), fill = NA, label.color = NA, label="*p* = 0.07" ,aes(x=x, y=y), inherit.aes=FALSE, size=2.5)
ggsave(glue("graphs/clin_Pa_screen_OD695_urine.pdf"), height = 8, width = 6, unit = "cm")