rm(list = ls()) 

##Library packages
library(ggplot2)
library(dplyr)
library(ggpubr)

####read in data

data <- read.csv("Figure_8_raw_input_data.csv", header=TRUE)
names(data)

#####Characterise data structure
data$Species<- as.factor(data$Species)
data$Species <- factor(data$Species, levels = c("Perca fluviatilis", 
                                                "Anguilla australis", 
                                                "Echyridella menziesii"))
data$DNA.total.conc <- as.numeric(data$DNA.total.conc)

###log transform for filter step
data$log_DNA.total.conc <- log(data$DNA.total.conc)

####linear regression - both separated for species, firstly with core location data separated and then combined
separate_linear <- data %>%
  dplyr::filter(log_DNA.total.conc > min(data$DNA.total.conc)) %>%
  dplyr::filter(!(Period%in%c("Maori","Prehuman") & Species == "Perca fluviatilis")) %>% 
  ggplot(aes(x = DNA.total.conc+ 1e-2, y = Conc.copies.uL. + 1e-2, colour = Core)) +
  geom_point(aes(x = DNA.total.conc+ 1e-2, y = Conc.copies.uL. + 1e-2), 
             position = position_jitter(height = .1)) +
  facet_wrap( ~ Species, scales = "free_x") +
  geom_smooth(aes(colour = Core),
              method = lm, formula = 'y ~ x', alpha = .1) +
  labs(x = expression("DNA concentration (ng " *mu*"L"^"-1"*")"),  
       y = expression("Target DNA copies (copies " ~mu*"L"^"-1"*")")) +
  scale_x_log10() +
  scale_y_log10() +
  ggpubr::stat_cor(
    aes(label = paste(
      ..rr.label.., gsub("p", "P", ..p.label..), sep = "~`,`~"
    )),
    p.accuracy = 0.001,
    r.accuracy = 0.01,
    size = 3
  ) + 
  scale_colour_manual(values = c("#00D000", "#7171FF"))  +
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(face = 4),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y = element_text(vjust= 2.2),
    axis.title.x = element_text(face= "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12),
    legend.position="right",
    legend.text = element_text(size = 12),  
    legend.box = "horizontal",
    legend.key.size = unit(2,"line")) + 
  (theme(plot.margin = unit(c(.35,.35,.35,.35), "cm")))

separate_linear

total_linear <- data %>%
  dplyr::filter(log_DNA.total.conc > min(data$DNA.total.conc)) %>%
  dplyr::filter(!(Period%in%c("Maori","Prehuman") & Species == "Perca fluviatilis")) %>% 
  ggplot(aes(x = DNA.total.conc+ 1e-2, y = Conc.copies.uL. + 1e-2)) +
  geom_point(aes(x = DNA.total.conc+ 1e-2, y = Conc.copies.uL. + 1e-2,colour = Period), 
             position = position_jitter(height = .1)) +
  facet_wrap( ~ Species, scales = "free_x") +
  geom_smooth(method = lm, formula = 'y ~ x', alpha = .1, colour = "black") +
  labs(x = expression("DNA concentration (ng " *mu*"L"^"-1"*")"),  
       y = expression("Target DNA copies (copies " ~mu*"L"^"-1"*")")) +
  scale_x_log10(labels = scales::label_number(accuracy = NULL)) +
  scale_y_log10(labels = scales::label_number(digits = 1)) +
  ggpubr::stat_cor(
    aes(label = paste(
      ..rr.label.., gsub("p", "P", ..p.label..), sep = "~`,`~"
    )),
    p.accuracy = 0.001,
    r.accuracy = 0.01,
    size = 3
  ) + 
  scale_colour_manual(name="Time Era", 
                      values = c("#ca0044", "#19b235", "#4841c4"),
                      labels = c("Post-European settlement ", 
                                 "M\u101ori settlement", "Prehuman")) + 
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(face = 4),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y = element_text(vjust= 2.2),
    axis.title.x = element_text(face= "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12),
    legend.position="right",
    legend.text = element_text(size = 12),  
    legend.box = "horizontal",
    legend.key.size = unit(2,"line")) + 
  (theme(plot.margin = unit(c(.35,.35,.35,.35), "cm")))


total_linear

##Combing figures for graphing
Figure8 <- ggarrange(separate_linear + rremove("x.title"), total_linear, labels = c("A", "B"),
                               ncol =1, nrow = 2,
                               align = "v")
Figure8

tiff(
  'Figure 8.tiff',
  width = 28,
  height = 23,
  units = 'cm',
  res = 600,
  compression = 'lzw',
  family = 'cairo'
)

Figure8

dev.off()

