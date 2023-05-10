rm(list = ls()) 

##Library packages
library(ggplot2)
library(ggpubr)

##read in data
data <- read.csv("Figure_6_raw_input_data.csv", header=TRUE)

###characterising data structure and type
data$Core<- as.factor(data$Core)
data$Species<- as.factor(data$Species)
data$Species <- factor(data$Species, levels = c("Perca fluviatilis", 
                                                "Anguilla australis", 
                                                "Echyridella menziesii"))
data$Meta_presence_absence <- as.factor(data$Meta_presence_absence)
data$Meta_presence_absence <- factor(data$Meta_presence_absence, levels = c("Present", 
                                                                            "Absent"))
data$Method <- as.factor(data$Method)
data$Method <- factor(data$Method, levels = c("ddPCR", 
                                              "Metabarcoding"))
data$Time_median <- as.factor(data$Time_median)

###Creating labels for graphs
species_names <- list("Perca fluviatilis" = "Perca fluviatilis", 
                     "Anguilla australis" = "Anguilla australis",
                     "Echyridella menziesii" = "Echyridella menziesii")

Period_labels <- list(
  'European'="Post-European settlement",
  'Maori'="M\u101ori settlement",
  "Prehuman" ="Prehuman")

##plot_labeller function
plot_labeller <- function(variable,value){
  if (variable=='Species') {
    return(species_names[value])
  } else {
    return(Period_labels[value])
  }
}

###Presence absence plots for ddPCR and metabarcoding results separated by species
Depocenter_figure <- ggplot(subset(data, Core == "Depocenter"), aes(Time_median, Method,  fill= Meta_presence_absence)) + 
  geom_tile() + 
  facet_grid(Period ~ Species, space = "free", scales = "free_y", labeller = plot_labeller) + 
  coord_flip() +
  scale_fill_manual(values = c( "grey20","grey90")) + 
  labs(fill = "Species Detection", subtitle = "Depocenter") +
  geom_hline(aes(yintercept = 1990), colour = "red") + 
  xlab("Year") +
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(face = "bold"),
    strip.text.x = element_text(face = 4),
    strip.text = element_text(face = "bold", size = 14),
    strip.text.y = element_text(size = 11),
    axis.title.y = element_text(vjust= 2.2),
    axis.title.x = element_blank(),
    axis.text = element_text(colour = "black", size = 13),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12),
    legend.position="right",
    legend.text = element_text(size = 12),  
    legend.box = "horizontal",
    legend.key.size = unit(2,"line")) + 
  (theme(plot.margin = unit(c(.35,.35,.35,.35), "cm")))

Depocenter_figure

Nearshore_figure <- ggplot(subset(data, Core == "Nearshore"), aes(Time_median, Method,  fill= Meta_presence_absence)) + 
  geom_tile() + 
  facet_grid(Period ~ Species, space = "free", scales = "free_y", labeller = plot_labeller) + 
  coord_flip() +
  scale_fill_manual(values = c( "grey20","grey90")) + 
  labs(fill = "Species Detection", subtitle = "Nearshore") +
  geom_vline(aes(xintercept = 1990), colour = "red") + 
  xlab(label="Year") +
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(face = "bold"),
    strip.text.x = element_text(face = 4),
    strip.text = element_text(face = "bold", size = 14),
    strip.text.y = element_text(size = 11),
    axis.title.y = element_text(vjust= 2.2),
    axis.title.x = element_blank(),
    axis.text = element_text(colour = "black", size = 13),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12),
    legend.position="right",
    legend.text = element_text(size = 12),  
    legend.box = "horizontal",
    legend.key.size = unit(2,"line")) + 
  (theme(plot.margin = unit(c(.35,.35,.35,.35), "cm")))

Nearshore_figure

##Combine plots for graphing
Figure6 <- ggarrange(Depocenter_figure, Nearshore_figure, ncol = 1, common.legend = T, labels = c("A", "B"))
Figure6

 tiff(
  'Figure 6.tiff',
  width = 22,
  height = 42,
  units = 'cm',
  res = 600,
  compression = 'lzw',
  family = 'cairo'
)

 Figure6

dev.off()
