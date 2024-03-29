rm(list = ls()) 

##Library packages

library(mgcv)
library(ggplot2)

###read data

data <- read.csv("Figure_5_raw_input_data.csv", header=TRUE)

##characterise data structure
data$Time_median <- as.numeric(data$Time_median)
data$Core <- as.factor(data$Core)
data$Species <- as.factor(data$Species)

data$Species <- factor(data$Species, levels = c("Perca fluviatilis",
                                                "Anguilla australis", 
                                                "Echyridella menziesii"))

##Log-transform DNA copy data
data$log_Copies.DNA.ng <- log(data$Copies.DNA.ng + 0.01)

###run separate gamms by core and species
mod1 <- gamm(log_Copies.DNA.ng ~ s(Time_median, k = 28, by = Core:Species) + Species + Core, 
             data = data, correlation = corCAR1(form = ~Time_median | Core:Species), method = "REML") 

##model output
summary(mod1$gam)

###plotting gamms output
Sample.Data <- with(data, data.frame(Time_median, Species, Core, Period))
plot_gamms <- cbind(Sample.Data,
                 data.frame(predict(mod1$gam, Sample.Data , se.fit = TRUE)))

###confidence interval
crit.t <- qt(0.975, df = df.residual(mod1$gam))
plot_gamms <- transform(plot_gamms,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
##final figure
Figure_5 <- ggplot(plot_gamms, aes(x = Time_median, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Time_median), alpha = 0.1,
              inherit.aes = FALSE, fill = "black") +
  geom_hline(yintercept = -4.6051702, lty = 3, colour = "red") +
  geom_point(data = data, mapping = aes(x = Time_median, y = log_Copies.DNA.ng, colour = Period), inherit.aes = FALSE) + 
  geom_line()+
  facet_grid(Core~Species, scales = "free_y") + 
  coord_flip() +
  scale_colour_manual(name="Time Era", 
                      values = c("#ca0044", "#19b235", "#4841c4"),
                      labels = c("Post-European settlement ", 
                                 "M\u101ori settlement", "Prehuman")) + 
  xlab ("Year") +  
  scale_x_continuous(breaks = c(2000, 1800, 1600, 1400, 1200, 1000, 800)) +
  ylab(expression("log_target DNA copies / total DNA concentration (copies ng"^-1*")")) + 
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold.italic", size = 12),
    axis.title.y = element_text(vjust= 2.2),
    axis.title.x = element_text(face= "plain"),
    axis.text = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12),
    legend.position="right",
    legend.text = element_text(size = 12),  
    legend.box = "horizontal",
    legend.key.size = unit(2,"line")) + 
  (theme(plot.margin = unit(c(.35,.35,.35,.35), "cm")))
  
Figure_5

##saving figure

tiff(
  'Figure 5.tiff',
  width = 25,
  height = 22,
  units = 'cm',
  res = 400,
  compression = 'lzw',
  family = 'cairo'
)

Figure_5
dev.off()
