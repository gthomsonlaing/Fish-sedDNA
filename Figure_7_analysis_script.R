rm(list = ls()) 

##Library packages
library(ggplot2)
library(mgcv)

####read data
data <- read.csv("Figure_7_raw_input_data.csv", header=TRUE)

###data characterisation
data$Time_median <- as.numeric(data$Time_median)

###run separatet gam by core
summary(gam(DNA.total.conc ~ s(Time_median, by = factor(Core), k = 4, bs = "cr"), data = data))

##plot gam output
Figure7 <- ggplot(data, aes(y = DNA.total.conc, 
                    x = Time_median)) + 
  geom_hline(yintercept=0.001, color = "red", lty = 3) +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, k = 4, bs = "cr"),
    se = T, 
    colour = "black", 
    alpha = 0.2) +
  geom_point(aes(y = DNA.total.conc, 
                 x = Time_median, 
                 colour = Period),  size = 2) +  
  ylab(expression("DNA concentration (ng " *mu*"L"^"-1"*")")) + 
  scale_colour_manual(name="Time Era", 
                      values = c("#ca0044", "#19b235", "#4841c4"),
                      labels = c("Post-European settlement ", 
                                 "M\u101ori settlement", "Prehuman")) +
  xlab ("Year") +
  facet_wrap(~Core) + 
  coord_flip() + 
  scale_x_continuous(breaks = c(2000, 1800, 1600, 1400, 1200, 1000, 800)) +
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
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

Figure7

###Saving figure file

tiff(
  'Figure 7.tiff',
  width = 25,
  height = 17,
  units = 'cm',
  res = 600,
  compression = 'lzw',
  family = 'cairo'
)

Figure7
dev.off()
