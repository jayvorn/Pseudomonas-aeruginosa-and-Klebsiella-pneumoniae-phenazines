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
strains<-c("KPPR1", "PA01", "Pa14", "145.1", "191.1", "193.1", "JV531")
colors<-c("black", "#28AB87", "#000080","#0147AB", "#008EEC", "#42E0D1", "#FF7900")
pcoa_levels<-c("JV1","PA01", "PA14", "JV3","JV4","JV5", "JV531")
pcoa_labels<-c("KPPR1", "PAO1", "PA14", "145.1", "191.1", "193.1", "MG1655pEmpty")
pcoa_shapes<-c(16,15,15,15,15,15, 22)
pcoa_colors<-c("black", "#28AB87", "#000080","#0147AB", "#008EEC", "#42E0D1", "#FF7900")
#===================================================================
# import data ##################################
compiled_data<-read_csv("combined_biolog.csv")
#================================================
# Graph heatmap ordinated ######################
compiled_data_heatmap<-compiled_data %>%
  select(Strain, Chemical, chem_mean) %>%
  distinct() %>%
  pivot_wider(., names_from = "Strain", values_from = "chem_mean") %>%
  select(-Chemical) %>%
  as.matrix(.)

heatmap_rownames<-compiled_data %>%
  select(Strain, Chemical, chem_mean) %>%
  distinct() %>%
  pivot_wider(., names_from = "Strain", values_from = "chem_mean") %>%
  select(Chemical) %>%
  as.matrix(.)

rownames(compiled_data_heatmap)<-heatmap_rownames
colnames(compiled_data_heatmap)<-c("KPPR1", "145.1", "191.1", "193.1", "PAO1", "PA14", "JV531")
compiled_data_heatmap<-compiled_data_heatmap[,c(1,5,6,2,3,4,7)]

pdf("graphs/biolog_heatmap.pdf", height = 9, width = 12, bg = "white", colormodel = "cmyk")

heatmap.2(compiled_data_heatmap, 
          dendrogram = "both",
          trace = "none",
          key = TRUE,
          key.title = "Growth",
          key.ylab = NA,
          key.xlab = "OD595",
          key.ytickfun = NULL,
          density.info = "none",
          lmat = rbind(c(0,3,4), c(2,1,0)),
          lhei = c(0.75, 4),
          lwid = c(0.75, 3, 1),
          offsetRow = -0.2,
          offsetCol = -0.2,
          cexRow = 0.3,
          cexCol = 1.5)

dev.off()
# Create BioLog PCA ######################
# evaluate sample dissimilarity
all_data_pcoa=compiled_data %>%
  select(Chemical, Strain, chem_mean)%>%
  distinct()%>%
  pivot_wider(names_from=Chemical,values_from=chem_mean)%>%
  replace(is.na(.),0) %>%
  select(-`Negative Control`) %>%
  column_to_rownames("Strain")

# generate distance matrix
read_dist=vegdist(all_data_pcoa, binary=FALSE, method = "euclidean")

# plot pcoa
biolog_pcoa=cmdscale(read_dist, eig = TRUE, add = TRUE)
biolog_pcoa_positions=biolog_pcoa$points
colnames(biolog_pcoa_positions) = c("axis_1", "axis_2")

biolog_pcoa_positions %>%
  as_tibble(rownames = "sample") %>%
  ggplot(aes(x=axis_1, y=axis_2, color=sample, shape = sample)) +
  geom_point(alpha = 1, size = 2.5) +
  labs(x = "Axis 1 (73.5%)", y = "Axis 2 (17.7%)") +
  scale_color_manual(name = "Strain",
                     breaks = pcoa_levels,
                     values = pcoa_colors,
                     labels = pcoa_labels) +
  scale_shape_manual(name = "Strain",
                     breaks = pcoa_levels,
                     values = pcoa_shapes,
                     labels = pcoa_labels)+
  scale_x_continuous(limits = c(-20, 20),
                     breaks = c(-20,-10,0,10,20),
                     labels = c(-20,-10,0,10,20)) +
  scale_y_continuous(limits = c(-22, 22),
                     breaks = c(-20,-10,0,10,20),
                     labels = c(-20,-10,0,10,20)) +
  theme_classic() +
  theme(axis.title = element_markdown(size = 14),
        axis.text.x = element_markdown(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "lightgrey"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 10)) +
  guides(color = guide_legend(nrow = 2))
ggsave("graphs/biolog_PCoA.pdf", height = 10, width = 9, unit = "cm")