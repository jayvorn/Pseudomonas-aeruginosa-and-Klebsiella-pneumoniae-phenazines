library(readxl)
library(tidyverse)
library(dplyr)
library(ggtext)
library(ggplot2)
library(ggrepel)
library(ggbreak)
library(EnvStats)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
# plot Ec LC-MS data ##################################
read_csv("207-PCAfragment_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" |Sample == "JV531" | Sample == "JV534" | Sample == "JV537" | Sample == "JV540") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450", "JV531", "JV534", "JV537", "JV540"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450", "JV531", "JV534", "JV537", "JV540"),
                         labels = c("Media control (LB)", "PA14","MG1655pEmpty", 
                                    "MG1655pPhzA-G", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"))) %>%
  na.omit() %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "PCA",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14","MG1655pEmpty", 
                                "MG1655pPhzA-G", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c("black",  "#000080", "#FF7900", "#E3AC36", 
                                "#990000", "#00CEC8")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_LCMS_PCA_207.pdf", height = 15, width = 8, unit = "cm")

read_csv("211-pyo_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" |Sample == "JV531" | Sample == "JV534" | Sample == "JV537" | Sample == "JV540"
         | Sample == "PYO") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "PYO", "JV450", "JV531", "JV534", "JV537", "JV540"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "PYO", "JV450", "JV531", "JV534", "JV537", "JV540"),
                         labels = c("Media control (LB)", "PYO", "PA14","MG1655pEmpty", 
                                    "MG1655pPhzA-G", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  scale_y_continuous(limits = c(0, 100000),
                     breaks = c(0, 50000, 100000),
                     labels = c("0e+00", "5e+04", "1e+05")) +
  labs(title = "PYO",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PYO", "PA14","MG1655pEmpty", "MG1655pPhzA-G", 
                                "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c("black", "#000080","#000080", "#FF7900", "#E3AC36", 
                                "#990000", "#00CEC8")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_LCMS_PYO_small_scale.pdf", height = 15, width = 8, unit = "cm")

read_csv("211-pyo_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450"),
                         labels = c("Media control (LB)", "PA14"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "PYO",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14"),
                     values = c("black", "#000080")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_LCMS_PYO_PA14.pdf", height = 15, width = 8, unit = "cm")

read_csv("239-5MPCA_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" |Sample == "JV531" | Sample == "JV534" | Sample == "JV537" | Sample == "JV540"
         | Sample == "PYO") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "PYO", "JV450", "JV531", "JV534", "JV537", "JV540"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "PYO","JV450", "JV531", "JV534", "JV537", "JV540"),
                         labels = c("Media control (LB)", "PYO","PA14","MG1655pEmpty", 
                                    "MG1655pPhzA-G", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "5MPCA",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PYO","PA14","MG1655pEmpty", "MG1655pPhzA-G", 
                                "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c("black", "pink","#000080", "#FF7900", "#E3AC36", 
                                "#990000", "#00CEC8")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_LCMS_5MPCA.pdf", height = 20, width = 8, unit = "cm")

read_csv("197-1HP_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" |Sample == "JV531" | Sample == "JV534" | Sample == "JV537" | Sample == "JV540"
         | Sample == "1HP") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "1HP","JV450", "JV531", "JV534", "JV537", "JV540"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "1HP","JV450", "JV531", "JV534", "JV537", "JV540"),
                         labels = c("Media control (LB)","1-HP", "PA14","MG1655pEmpty", 
                                    "MG1655pPhzA-G", "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "1-HP",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "1-HP","PA14","MG1655pEmpty", "MG1655pPhzA-G", 
                                "MG1655pPhzA-GS*M", "MG1655pPhzA-GSM"),
                     values = c("black", "#7c4d26","#000080", "#FF7900", "#E3AC36", 
                                "#990000", "#00CEC8")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_LCMS_1HP.pdf", height = 20, width = 8, unit = "cm")
# plot Pa LC-MS data ##################################
read_csv("207-PCAfragment_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" | Sample == "JV534" | Sample == "JV46") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450", "JV534","JV46"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450", "JV534","JV46"),
                         labels = c("Media control (LB)", "PA14", "MG1655pPhzA-G","JV46"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "PCA",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14", "MG1655pPhzA-G","JV46"),
                     values = c("black", "#000080", "#E3AC36", "#6667AB")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_clin_Pa_LCMS_PCA.pdf", height = 11, width = 8, unit = "cm")

read_csv("211-pyo_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" | Sample == "JV540" |Sample == "JV46") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450", "JV540", "JV46"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450", "JV540", "JV46"),
                         labels = c("Media control (LB)", "PA14", "MG1655pPhzA-GSM", "JV46"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "PYO",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14", "MG1655pPhzA-GSM","JV46"),
                     values = c("black", "#000080", "#00CEC8","#6667AB")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_clin_Pa_LCMS_PYO.pdf", height = 11, width = 8, unit = "cm")

read_csv("239-5MPCA_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450" | Sample == "JV537" |Sample == "JV46") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450", "JV537", "JV46"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450", "JV537", "JV46"),
                         labels = c("Media control (LB)", "PA14", "MG1655pPhzA-GS*M","JV46"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "5MPCA",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14", "MG1655pPhzA-GS*M","JV46"),
                     values = c("black", "#000080","#990000", "#6667AB")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_clin_Pa_LCMS_5MPCA.pdf", height = 11, width = 8, unit = "cm")

read_csv("239-5MPCA_long_format.csv") %>%
  filter(Sample == "LB" | Sample == "JV450"| Sample == "JV537" | Sample == "JV463" |Sample == "Pa14_TnPhzS") %>%
  mutate(Sample = factor(Sample, levels = c("LB", "JV450", "JV537", "JV463", "Pa14_TnPhzS"))) %>%
  mutate(Sample = factor(Sample, 
                         levels = c("LB", "JV450", "JV537", "JV463", "Pa14_TnPhzS"),
                         labels = c("Media control (LB)", "PA14", "MG1655pPhzA-GS*M","JV463", "Pa14_TnPhzS"))) %>%
  ggplot(aes(x = RetTime, y = EIC, color = Sample)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Sample, ncol = 1) +
  labs(title = "5MPCA",x = "Time (minutes)", y = "Relative Abundance") +
  scale_color_manual(breaks = c("Media control (LB)", "PA14", "MG1655pPhzA-GS*M","JV463", "Pa14_TnPhzS"),
                     values = c("black", "#000080","#990000", "#000080","#000080")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10))
ggsave("graphs/phen_phzS_KO_Pa_LCMS_5MPCA.pdf", height = 13, width = 8, unit = "cm")