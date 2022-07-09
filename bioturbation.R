library(tidyverse)

bioturb <- read.csv("bioturbation2.csv")
head(bioturb)

####____3B____####
#total number of pellets over 3 hr survey period for both spp
#Pellet egestion (pellets/individual)"
#pellets <- ggplot(data=bioturb, aes(x = species, y = pellets_per_hr, fill = species)) +
 # geom_pointrange() + 
 # geom_jitter(shape=16, position=position_jitter(0.2)) +
 # scale_fill_manual(values=c("indianred3", "slateblue3")) +
 # labs(y = "Egestion (pellets" ~h^-1~ ")") +
 # theme( 
   # panel.background = element_rect(fill = "transparent",colour = NA),
   # panel.grid.minor = element_blank(),
   # panel.grid.major = element_blank(),
   # axis.line.x = element_line(colour = "black"),
   # axis.line.y = element_line(colour = "black"),
   # axis.title.x = element_blank(),
   # axis.title.y = element_text(size=16),
   # axis.text.y = element_text(size=18),
   # axis.text.x = element_text(size=18),
   # legend.position="none") 
#pellets

fig3b

fig3b <- ggplot(data=bioturb, aes(x = species, y = pellets_per_hr, fill = species)) +
geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
stat_summary(aes(color = species),
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.8)+
scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Egestion (pellets" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
fig3b

defenseB <- fig3b + theme(panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.border = element_blank(),
            axis.line.x = element_line(colour = "white"),
            axis.line.y = element_line(colour = "white"),
            axis.title.x = element_text(size=18, colour = "white"),
            axis.title.y = element_text(size=18, colour = "white"),
            axis.text.y = element_text(size=18, colour = "white"),
            axis.text.x = element_text(size=18, colour = "white")) 
defenseB

# fun data, one of ['mean_cl_boot', 'mean_cl_normal', 
                   #'mean_sdl', 'median_hilow'] or any function that takes a array and returns a 
                   #'dataframe with three columns named y, ymin and ymax. Defaults to 'mean_cl_boot'.


?stat_summary
?mean_sdl


#is there a difference btwn the mean number of pellets egested by each spp over a 3hr period?
mod1 <- lm(pellets_per_hr ~ species, data = bioturb)
mod1
# p val is 0.005785
summary(mod1)
#visreg and anova won't work unless you us lm rather than t.test
library(visreg)
visreg(mod1)
anova(mod1)


####____3C____####
#is there a difference between the average pellet weight between species?
#p_weight <- ggplot(data=bioturb, aes(x = species, y = weight_per_pellet, fill = species)) +
#  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
#  scale_fill_manual(values=c("indianred3", "slateblue3")) +
#  #Weight per pellet
#  labs(x = "Species", y = "Pellet weight (g)") +
#  theme( 
#    panel.background = element_rect(fill = "transparent",colour = NA),
#    panel.grid.minor = element_blank(),
#    panel.grid.major = element_blank(),
#    axis.line.x = element_line(colour = "black"),
#    axis.line.y = element_line(colour = "black"),
#    axis.title.x = element_blank(),
#    axis.title.y = element_text(size=16),
#    axis.text.y = element_text(size=18),
#    axis.text.x = element_text(size=18),
#    legend.position="none")

mod3 <- lm(weight_per_pellet ~ species, data = bioturb)
mod3
summary(mod3)

fig3c <- ggplot(data=bioturb, aes(x = species, y = weight_per_pellet, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Pellet weight (g)") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
fig3c

defenseC <- fig3c + theme(panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.border = element_blank(),
              axis.line.x = element_line(colour = "white"),
              axis.line.y = element_line(colour = "white"),
              axis.title.x = element_text(size=18, colour = "white"),
              axis.title.y = element_text(size=18, colour = "white"),
              axis.text.y = element_text(size=18, colour = "white"),
              axis.text.x = element_text(size=18, colour = "white")) 

####____3A____####
#speed (total distance/3hrs) for both spp
#speed <- ggplot(data=bioturb, aes(x = species, y = speed, fill = species)) +
#  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
#  scale_fill_manual(values=c("indianred3", "slateblue3")) +
#  scale_y_continuous(limits = c(0,0.61)) +
#  labs(x = "Species", y = "Speed (m" ~h^-1~ ")") +
#  theme( 
#    panel.background = element_rect(fill = "transparent",colour = NA),
#    panel.grid.minor = element_blank(),
#    panel.grid.major = element_blank(),
#    axis.line.x = element_line(colour = "black"),
#    axis.line.y = element_line(colour = "black"),
#    axis.title.x = element_blank(),
#    axis.title.y = element_text(size=16),
#    axis.text.y = element_text(size=18),
#    axis.text.x = element_text(size=18),
#    legend.position="none")

#is there a differene between the mean movement speed of each species?
mod2 <- lm(speed ~ species, data = bioturb)
mod2

?lm
# p val is 0.1649
summary(mod2)

fig3a <- ggplot(data=bioturb, aes(x = species, y = speed, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Speed (m" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
  #scale_y_continuous(limits = c(0,0.61)) 

fig3a



defenseA <- fig3a + theme(panel.background = element_rect(fill = "transparent",colour = NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                    panel.border = element_blank(),
                    axis.line.x = element_line(colour = "white"),
                    axis.line.y = element_line(colour = "white"),
                    axis.title.x = element_text(size=18, colour = "white"),
                    axis.title.y = element_text(size=18, colour = "white"),
                    axis.text.y = element_text(size=18, colour = "white"),
                    axis.text.x = element_text(size=18, colour = "white")) 

####____3D____####

fig3d <- ggplot(data=bioturb, aes(x = species, y = g_per_hr, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Bioturbation (g" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
fig3d

defenseD <-fig3d + theme(panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.border = element_blank(),
              axis.line.x = element_line(colour = "white"),
              axis.line.y = element_line(colour = "white"),
              axis.title.x = element_text(size=18, colour = "white"),
              axis.title.y = element_text(size=18, colour = "white"),
              axis.text.y = element_text(size=18, colour = "white"),
              axis.text.x = element_text(size=18, colour = "white")) 

bioturb %>% 
  group_by(species) %>% 
  summarise(mean(speed), sd(speed))



mod3d <- t.test(g_per_hr ~ species, data = bioturb)
mod3d


####____OM____#### 
#is there a difference between the organic matter content in fecal pellets between species?
##  % Organic Matter ((AFDW/DW)*100)
#om_content <- ggplot(data=bioturb, aes(x = species, y = organic_cont, fill = species)) +
#  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
#  scale_fill_manual(values=c("indianred3", "slateblue3")) +
#  labs(x = "Species", y = "% Organic Matter") +
#  theme( 
#    panel.background = element_rect(fill = "transparent",colour = NA),
#    panel.grid.minor = element_blank(),
#    panel.grid.major = element_blank(),
#    axis.line.x = element_line(colour = "black"),
#    axis.line.y = element_line(colour = "black"),
#    axis.title.x = element_blank(),
#    axis.title.y = element_text(size=18),
#    axis.text.y = element_text(size=18),
#    axis.text.x = element_text(size=18),
#    legend.position = "none")

mod4 <- lm(organic_cont ~ species, data = bioturb)
mod4
summary(mod4)


####____CH1 FIG3 A, B, C, & D  - 4x panel of bioturbation functions____####
library(cowplot)
citation('cowplot')
bioturbx4 <- plot_grid(fig3b, fig3c, fig3d, fig_om, fig3a, nrow = 1)
bioturbx4

bioturb_def <- plot_grid(defenseB, defenseC, defenseD, nrow = 1)
bioturb_def

##### CH1 FIG4 - OM content w ambient sand ####
bioturb_sand <- read.csv("3bioturb.csv")

#spp_sand <- ggplot(data=bioturb_sand, aes(x = species, y = om, fill = species)) +
#  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
#  scale_fill_manual(values=c("indianred3", "slateblue3", "darkseagreen")) +
#  labs(x = "Species", y = "% Organic Matter") +
#  theme( 
#    panel.background = element_rect(fill = "transparent",colour = NA),
#    panel.grid.minor = element_blank(),
#    panel.grid.major = element_blank(),
#    axis.line.x = element_line(colour = "black"),
#    axis.line.y = element_line(colour = "black"),
#    axis.title.x = element_blank(),
#    axis.title.y = element_text(size=16),
#    axis.text.y = element_text(size=18),
#    axis.text.x = element_text(size=18),
#    legend.position = "none")

bioturb_sand2 <- bioturb_sand %>% 
  filter(species != "sand")

fig_om <- ggplot(data=bioturb_sand2, aes(x = species, y = om, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.15), size = 1.6) +
  stat_summary(aes(color = species),
               fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.9)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "% Organic Matter") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    legend.position="none") 
fig_om

defenseOM <-fig_om + theme(panel.background = element_rect(fill = "transparent",colour = NA),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                         panel.border = element_blank(),
                         axis.line.x = element_line(colour = "white"),
                         axis.line.y = element_line(colour = "white"),
                         axis.title.x = element_text(size=18, colour = "white"),
                         axis.title.y = element_text(size=18, colour = "white"),
                         axis.text.y = element_text(size=18, colour = "white"),
                         axis.text.x = element_text(size=18, colour = "white")) 
defenseOM

bioturb_sand2 <- na.omit(bioturb_sand) 

bioturb_sand2 %>% 
  group_by(species) %>% 
  summarise(mean(om))

#Anova test
library(ggpubr)
library(rstatix)

levels(bioturb_sand$species)

#reorder levels
bioturb_sand <- bioturb_sand %>%
  reorder_levels(species, order = c("dd", "ft", "sand"))

#summary statistics
bioturb_sand %>%
  group_by(species) %>%
  get_summary_stats(om, type = "mean_sd")

#identify outliers
bioturb_sand %>% 
  group_by(species) %>%
  identify_outliers(om)

# Kruskal Wallis test between three grousp (2 species and sediment)

kruskal.test(om ~ species, data = bioturb_sand)


pairwise.wilcox.test(bioturb_sand$om, bioturb_sand$species,
                     p.adjust.method = "BH")

# Create a QQ plot of residuals
ggqqplot(residuals(model))

#shapiro test of normality
shapiro_test(residuals(model))
# Not normal data!  p = 0.0000000316

# Need to do Kruskal Wallis test
kruskal.test(om ~ species, data = bioturb_sand)

bioturb_sand$om = factor(bioturb_sand$om,
                     levels=c("ft", "dd", "sand"))

#multiple pairwise comparison between groups  -Dunn's test-
install.packages("dunn.test")
citation('dunn.test')

dunnTest(om ~ species,
              data=bioturb_sand,
              method="bh") 

## FIG 
#is there a difference between organic matter content PER GRAM of dry weight in fecal pellets between species?
om_per_g <- ggplot(data=bioturb, aes(x = species, y = om_per_g_dry, fill = species)) +
  geom_boxplot(names=c(expression(italic("H. mexicana"),italic("A. agassizii")))) + 
                 geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "% Organic Matter / gram dry weight") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_per_g

mod5 <- t.test(om_per_g_dry ~ species, data = bioturb)
mod5

## FIG 
#is there a difference between organic matter content PER GRAM of ash weight in fecal pellets between species?
om_ash <- ggplot(data=bioturb, aes(x = species, y = om_per_g_ash, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "% Organic Matter / gram AFDW") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_ash

mod5 <- t.test(om_per_g_ash ~ species, data = bioturb)
mod5

##7 UMMM THIS IS KIND OF THE SAME AS FIGURE 8B
#is there a difference between the sand egestion rate (dry weight/pellets per hr) between species?
vol_eg <- ggplot(data=bioturb, aes(x = species, y = sand_vol_egestion_rate, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "dry weight/pellets/hr") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
vol_eg

mod6 <- t.test(sand_vol_egestion_rate ~ species, data = bioturb)
mod6

##8 UMMM THIS IS KIND OF THE SAME AS FIGURE 8A
om_rate <- ggplot(data=bioturb, aes(x = species, y = om_rate, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "OM/g dry weight/hr") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_rate

mod7 <- t.test(om_rate ~ species, data = bioturb)
mod7

bioturb %>% 
  group_by(species) %>% 
  filter(weight_per_pellet!='NA') %>% 
  summarize(mean(weight_per_pellet), sd(weight_per_pellet), min(length), max(length), mean(girth), 
            sd(girth), min(girth), max(girth))


##### S4 Grams per hour Oct 9####
#grams per hour as a function of length
g_per_hour <- ggplot(bioturb, aes((length), (g_per_hr),  group = species, colour = species)) +
  geom_point(size = 2.5) +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "Bioturbation (grams" ~hour^-1~")", x = "Length (cm)") +
    guides (col = FALSE)



g_per_hour + scale_color_manual(values=c("deepskyblue3", "orange")) +
  scale_fill_manual(values=c("deepskyblue3", "orange"), 
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme (legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.title.x = element_text(size=18, colour = "black"),
        axis.title.y = element_text(size=18, colour = "black"),
        axis.text.y = element_text(size=18, colour = "black"),
        axis.text.x = element_text(size=18, colour = "black")) 

#for defense presentation
g_per_hour + scale_color_manual(values=c("deepskyblue3", "orange")) +
  scale_fill_manual(values=c("deepskyblue3", "orange"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
theme(panel.background = element_rect(fill = "transparent",colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "white"),
      axis.line.y = element_line(colour = "white"),
      axis.title.x = element_text(size=18, colour = "white"),
      axis.title.y = element_text(size=18, colour = "white"),
      axis.text.y = element_text(size=18, colour = "white"),
      axis.text.x = element_text(size=18, colour = "white")) 


#ggsave(biotm, filename = "biotm.png", bg = "transparent", height = 7, 
#width = 9)

mod3 <- lm(g_per_hr ~ species, data = bioturb)
summary(mod3)

library(car)
Anova(mod3)


bioturb %>%
  group_by(species) %>%
  summarise(mean(g_per_hr), sd(g_per_hr)/sqrt(20))

#just exploring, prob don't use this
#speed as a function of sea cucumber size index
biots <- ggplot(bioturb, aes((sizeindex), (speed),  group = species, colour = species)) +
  geom_point(size = 3) +
  #geom_line(aes(y = lwr, colour = species), linetype = "blank") +
  #geom_line(aes(y = upr, colour = species), linetype = "blank") +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "Speed (metres/hour)", x = "Size Index") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
biots

#ggsave(biots, filename = "biots.png", bg = "transparent", height = 7, 
#width = 9)

mod4 <- lm(speed ~ sizeindex * species, data = bioturb)
summary(mod4)
visreg(mod4)
anova(mod4)
drop1(mod4)

#ggsave(biots, filename = "biots.png", bg = "transparent", height = 7, 
#width = 9)


##### Egestion ESTIMATES #####
egestion_pot2 <- read.csv("egestion_pot2.csv")

#each point is a site (points are paired) and each site average egestion rate multiplied by 
#the number of individuals at each site (natural density before manipulation)
eg <- ggplot(data=egestion_pot2, aes(x = species, y = kg_m2yr, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("indianred3", "slateblue3")) +
  labs(x = "Species", y = "Sediment turnover (kg/m2/year)") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
eg



fit <- read.csv("ammonium_bioturbation.csv")

#Oct 16
##needs to be paired ttest, not welchs two sample 
mod5 <- lm(kg_m2yr ~ species, data = egestion_pot2)
mod5

dd <- fit$kgm2yr_dd
ft <- fit$kgm2yr_ft

t.test(ft, dd, paired = TRUE)


##need this for excretion too
dung <- fit$umolhm2_dd
tooth <- fit$umolhm2_ft

t.test(tooth, dung, paired = TRUE)

fit %>%
  summarise(mean(umolhm2_dd), sd (umolhm2_dd/sqrt(35)), mean(umolhm2_ft), sd(umolhm2_ft/sqrt(35)))


summary(mod5)
visreg(mod5)
anova(mod5)
drop1(mod5)


# Stacked barplot with multiple groups
#stacked bar of kg_m2_yr at each site, divided by species
# tweak units on y-axis here
ggplot(data=egestion_pot2, aes(x=reorder(patch, -kg_m2_yr), y = kg_m2_yr, fill=species)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#stacked bar of total egestion at each site, divided by species
#tweak units on the y-axis here
ggplot(data=egestion_pot2, aes(x=reorder(patch, -egestion_per_site), y = egestion_per_site, fill=species)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

### EGESTION heat maps ####

ddb <- filter(egestion_pot2, species == "dd")
#view(ddb)

ftb <- filter(egestion_pot2, species == "ft")
#view(ftb)

#squares with 4x7 fake rows for DD
ddb2 <- ggplot(data = ddb, aes(x = fake_g2, y = fake_g)) +
  geom_tile(aes(fill = kg_m2yr)) + 
   theme(axis.text.y = element_blank(),
         axis.text.x = element_blank()) +
         labs(x = " ", y = " ") +
  scale_fill_distiller(palette = "PuRd", direction = 1,
                       limits = c(min(egestion_pot2$kg_m2yr), max(egestion_pot2$kg_m2yr)),
                       name="kg" ~m^-2~ " " ~year^-1~ " ") 


#squares with 4x7 fake rows for FT
library(shades)

ftb2 <- ggplot(data = ftb, aes(x = fake_g2, y = fake_g)) +
  geom_tile(aes(fill = kg_m2yr)) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
        labs(x = " ", y = " ") +
 scale_fill_distiller(palette = "PuRd", direction = 1,
                       limits = c(min(egestion_pot2$kg_m2yr), max(egestion_pot2$kg_m2yr)), 
                       name="kg" ~m^-2~ " " ~year^-1~ " ") 

ddb2
ftb2
library(cowplot)
theme_set(theme_cowplot())
plot_grid(ddb2, ftb2)


### EXCRETION heat maps ####
#squares with 4x7 fake rows for DD
dde2 <- ggplot(data = ddb, aes(x = fake_g2, y = fake_g)) +
  geom_tile(aes(fill = umolh_m2)) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = " ", y = " ") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = c(min(egestion_pot2$umolh_m2), max(egestion_pot2$umolh_m2)),
                       name="umol" ~m^-2~ " " ~hour^-1~ " ") 


#squares with 4x7 fake rows for FT
library(shades)

fte2 <- ggplot(data = ftb, aes(x = fake_g2, y = fake_g)) +
  geom_tile(aes(fill = umolh_m2)) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  labs(x = " ", y = " ") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = c(min(egestion_pot2$umolh_m2), max(egestion_pot2$umolh_m2)), 
                       name="umol" ~m^-2~ " " ~hour^-1~ " ") 


dde2
fte2
