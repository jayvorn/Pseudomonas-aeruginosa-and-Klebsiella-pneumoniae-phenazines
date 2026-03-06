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
library(DescTools)
library(gplots)

# set environment ##################################
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
#===================================================================
# define aesthetics ##################################
levels<-c("other","145", "191", "193")
labels<-c("Other","145", "191", "193")
shapes<-c(16,15,15,15)
colors<-c("grey", "#0147AB", "#008EEC", "#42E0D1")
#===================================================================
# import data ##################################
original_mouse_cfus<-read_csv("original_mouse_cfus.csv")

gut_colonization_data<-read_csv("gut_colonization_data.csv")
#===================================================================
# graph data ##################################
ggplot(original_mouse_cfus, aes(x = sample, y = log_cfu_g, color = pa_code, shape = pa_code)) +
  geom_line(data = original_mouse_cfus[1:86,], aes(group = Mouse),
            position =  position_jitter(width = 0.1, seed = 1),
            alpha = 0.6) +
  geom_point(data = original_mouse_cfus[1:86,], 
             position = position_jitter(width = 0.1, seed = 1),
             alpha = 0.6,
             size = 1.25,
             show.legend = FALSE) + 
  geom_line(data = original_mouse_cfus[87:92,], aes(group = Mouse)) +
  geom_point(data = original_mouse_cfus[87:92,], 
             alpha = 1,
             size = 1.5,
             show.legend = FALSE) + 
  labs(x = NULL, y = bquote(Log[10]~"CFU/g tissue")) +
  scale_x_discrete(breaks = c("feces_24_hr", "gut_48_hr"),
                   labels = c(bquote(Feces["24hr"]), bquote(LI["48hr"]))) +
  scale_color_manual(name = "Mouse",
                     breaks = levels,
                     labels = labels,
                     values = colors) +
  scale_shape_manual(name = "Mouse",
                     breaks = levels,
                     labels = labels,
                     values = shapes) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10)) 
ggsave(glue("graphs/pa_mouse_cfus.pdf"), height = 8, width = 7, unit = "cm")

gut_colonization_data %>%
  filter(Tissue == "feces") %>%
  filter(Strain_ref == "JV4") %>%
  ggplot(aes(x=as.numeric(Day_cont), y=log_CFU_per_g, color = Group, group = Sample)) +
  geom_line(alpha = 0.25, 
            linewidth = 0.5,
            color = "#1E90FF",
            show.legend = FALSE) +
  geom_point(alpha = 0.25,
             shape = 16,
             color = "#1E90FF",
             show.legend = FALSE) +
  geom_line(aes(y=mean_log_CFU_per_g, color = Group, group = Group),
            alpha = 1, 
            linewidth = 0.75,
            color = "#1E90FF",
            show.legend = FALSE) +
  geom_linerange(aes(ymin = mean_log_CFU_per_g-sem_log_CFU_per_g, ymax = mean_log_CFU_per_g+sem_log_CFU_per_g, group = Group),
                 linewidth = 0.5,
                 color = "#1E90FF",
                 show.legend = FALSE) +
  geom_point(aes(y=mean_log_CFU_per_g, group = Group),
             shape = 16,
             size = 2.5,
             color = "#1E90FF",
             show.legend = FALSE) +
  geom_segment(aes(x = 7, y = 10.5, xend = 7, yend = 8.5),
               arrow = arrow(length = unit(0.2, "cm")),
               lineend = "round",
               color = "black",
               show.legend = FALSE) +
  geom_richtext(data=tibble(x=5.9, y=11), fill = NA, label.color = NA, label="KPPR1 inoculation" ,aes(x=x, y=y), inherit.aes=FALSE, size=3) +
  labs(x = "Day", y = "Log CFU 191.1/g feces") +
  scale_x_continuous(limits = c(1,7),
                     breaks = c(1,2,3,4,5,6,7),
                     labels = c(1,2,3,4,5,6,7)) +
  scale_y_continuous(limits = c(1, 12),
                     breaks = c(2, 4, 6, 8, 10, 12),
                     labels = c(2, 4, 6, 8, 10, 12)) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/in_vivo_pilot_pa_feces_cfus.pdf"), height = 6, width = 8, unit = "cm")

gut_colonization_data %>%
  filter(Tissue == "feces") %>%
  filter(Strain_ref == "JV1") %>%
  ggplot(aes(x=as.numeric(Day_cont), y=log_CFU_per_g, color = Group)) +
  geom_line(aes(x=as.numeric(Day_cont), y=log_CFU_per_g, group = Sample),
            alpha = 0.25, 
            linewidth = 0.5,
            show.legend = FALSE) +
  geom_point(alpha = 0.25,
             shape = 16,
             show.legend = FALSE) +
  geom_line(aes(y=mean_log_CFU_per_g, color = Group),
            alpha = 1, 
            linewidth = 0.75,
            show.legend = FALSE) +
  geom_linerange(aes(ymin = mean_log_CFU_per_g-sem_log_CFU_per_g, ymax = mean_log_CFU_per_g+sem_log_CFU_per_g, group = Group),
                 linewidth = 0.5,
                 show.legend = FALSE) +
  geom_point(aes(y=mean_log_CFU_per_g, group = Group),
             shape = 16,
             size = 2.5,
             show.legend = TRUE) +
  labs(x = "Day", y = "Log CFU KPPR1/g feces") +
  scale_x_continuous(limits = c(8,13),
                     breaks = c(8,9,10,11,12,13),
                     labels = c(1,2,3,4,5,6)) +
  scale_y_continuous(limits = c(1, 12),
                     breaks = c(2, 4, 6, 8, 10, 12),
                     labels = c(2, 4, 6, 8, 10, 12)) +
  scale_color_manual(name = "Group",
                     breaks = c("ctrl", "exp"),
                     labels = c("KPPR1 alone", "KPPR1 + 191.1"),
                     values = c("black", "red")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/in_vivo_pilot_kp_feces_cfus.pdf"), height = 6, width = 12, unit = "cm")

gut_colonization_data %>%
  filter(Tissue == "cecum") %>%
  filter(LOD != "Yes") %>%
  ggplot(aes(x=Group, y=log_CFU_per_g, color = Group)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 0.5,
             show.legend = FALSE,
             shape = 16,
             size = 2) +
  geom_point(aes(x=Group, y=mean_log_CFU_per_g),
             position = position_dodge(width = 0.75),
             color = "black",
             shape = 95,
             size = 6) +
  labs(x = NULL, y = "Log CFU KPPR1/g cecum") +
  scale_x_discrete(breaks = c("ctrl", "exp"),
                   labels = c("KPPR1 alone", "KPPR1 + 191.1")) +
  scale_y_continuous(limits = c(1, 12),
                     breaks = c(2, 4, 6, 8, 10, 12),
                     labels = c(2, 4, 6, 8, 10, 12)) +
  scale_color_manual(name = "Group",
                     breaks = c("ctrl", "exp"),
                     labels = c("KPPR1 alone", "KPPR1 + 191.1"),
                     values = c("black", "red")) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 12),
        axis.text.x = element_markdown(color = "black", size = 10, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "right",
        legend.text = element_markdown(size = 10))
ggsave(glue("graphs/in_vivo_pilot_kp_cecum_cfus.pdf"), height = 8, width = 4, unit = "cm")