# DD and FT Excretion Rates - started first on February 17 2020
library(tidyverse)
library(ggplot2)
library(readr)
library(Rcpp)
library(backports)
theme_set(theme_cowplot()) #plot figures next to one another, example: plot_grid(plot1, plot2)

#### EXCRETION DAY 1 of 2, July 26 2019 ####
#read in standard curve and excretion data
standard <- read_csv("2019_07_26_standardcurve.csv")
excretion <- read_csv("2019_07_26_excretion.csv") %>% 
  filter(species!='NA')

standard_f <- standard %>% 
  mutate(nh4_added_umol = nh4_vol_uL/1e6 * nh4_conc_og_umol, #amount of NH4
         total_vol_L = nh4_vol_uL/1e6 + og_vol_L, #new volume of sample + NH4
         nh4_conc_final_umol_L = nh4_added_umol / total_vol_L, #concentration
         #of NH4 in seawater sample
         mean_FLU = rowMeans(cbind(FLU1, FLU2, FLU3), na.rm = TRUE)) #mean FLU

#reading linear mod between the fluorometer reading and actual concentration of NH4
sc_mod <- lm(nh4_conc_final_umol_L ~ mean_FLU, data = standard_f)
summary(sc_mod)

#extract intercept and slope to use later to determine sample concentrations
#based on fluorometry readings
int <- coef(sc_mod)[1]
slope <- coef(sc_mod)[2]

#visualize curve to make sure it looks right 
ggplot(standard_f, aes(mean_FLU, nh4_conc_final_umol_L)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

##actual samples:
excretion_f <- excretion %>% 
  mutate(inc_time_h = as.numeric(incubation.end - incubation.start, 
                                 units = "hours")) %>% #incubation time
  mutate(bag_volume_L = vol.measured/1000 + vol.syringed/1000)  %>% #volume of water in
  #bag at the time the sample was taken in litres
  mutate(mean_preFLU = rowMeans(cbind(pre.FLU1, pre.FLU2, pre.FLU3), 
                                na.rm = TRUE), mean_postFLU = rowMeans(cbind(post.FLU1, post.FLU2, post.FLU3), 
                                 na.rm = TRUE)) %>% #mean FLU readings
  mutate(int = int, #include values for the int and slope in for every column
         slope = slope) %>% #to calculate the conversion to NH4 conc
  mutate(nh4_conc_pre = (int + slope * mean_preFLU), #this gives us the initial
         #concentration of nh4 based on our standard curve in umol/L
         nh4_umol_pre = nh4_conc_pre * bag_volume_L + 0.060, #amount of NH4 
         #in umol in the whole bag based on concentration and volume
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio, 
         #concentration of nh4 after cuke was in the bag based on standard curve
         nh4_umol_post = nh4_conc_post * bag_volume_L,#amount of NH4 based on
         #concentration and volume
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre, #amount of NH4 cuke 
         #added to bag in total
         nh4_rate = nh4_umol_diff / inc_time_h, #the rate at which the cuke
         #added NH4 to the bag
         nh4_rate_per_g = nh4_rate/wetweight, #the rate at which the cuke
         #added NH4 to the bag for every gram of body weight
         inc_time_min = inc_time_h * 60,
         #we don't need season or ID here anymore because we only have 1 
         #sampling period
         mass_g = wetweight, 
         sizeindex = sqrt(length*breadth),
         sizeindex2 = length*breadth*0.01,
         cyl_vol = pi * ((breadth/(2*pi))^2) * length,
         cuke_id = ziploc.number)


#### EXCRETION DAY 2 of 2, August 1 2019 ####
standard1 <- read_csv("2019_08_01_standardcurve.csv")
excretion1 <- read_csv("2019_08_01_excretion.csv") %>% 
  filter(species!='NA')

standard_f1 <- standard1 %>% 
  mutate(nh4_added_umol = nh4_vol_uL/1e6 * nh4_conc_og_umol, #amount of NH4
         total_vol_L = nh4_vol_uL/1e6 + og_vol_L, #new volume of sample + NH4
         nh4_conc_final_umol_L = nh4_added_umol / total_vol_L, #concentration
         #of NH4 in seawater sample
         mean_FLU = rowMeans(cbind(FLU1, FLU2, FLU3), na.rm = TRUE)) #mean FLU

#reading linear model between the fluorometer reading and actual concentration of NH4
sc_mod2 <- lm(nh4_conc_final_umol_L ~ mean_FLU, data = standard_f1)
summary(sc_mod2)

#extract intercept and slope to use later to determine sample concentrations
#based on fluorometry readings
int2 <- coef(sc_mod2)[1]
slope2 <- coef(sc_mod2)[2]
#visualize curve to make sure it looks right
ggplot(standard_f1, aes(mean_FLU, nh4_conc_final_umol_L)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

##actual samples:
excretion_f1 <- excretion1 %>% 
  mutate(inc_time_h = as.numeric(incubation.end - incubation.start, 
                                 units = "hours")) %>% #incubation time
  mutate(bag_volume_L = vol.measured/1000 + vol.syringed/1000)  %>% #volume of water in
  #bag at the time the sample was taken
  mutate(mean_preFLU = rowMeans(cbind(pre.FLU1, pre.FLU2, pre.FLU3), 
                                na.rm = TRUE), mean_postFLU = rowMeans(cbind(post.FLU1, post.FLU2, post.FLU3), 
                                 na.rm = TRUE)) %>% 
  mutate(int = int2, #include values for the int and slope in for every column
         slope = slope2) %>% #to calculate the coversion to NH4 conc
  mutate(nh4_conc_pre = (int + slope * mean_preFLU),
         nh4_umol_pre = nh4_conc_pre * bag_volume_L + 0.060,
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio,
         nh4_umol_post = nh4_conc_post * bag_volume_L,
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre,
         nh4_rate = nh4_umol_diff / inc_time_h,
         nh4_rate_per_g = nh4_rate/wetweight,
         inc_time_min = inc_time_h * 60,
         mass_g = wetweight, 
         sizeindex = sqrt(length*breadth),
         sizeindex2 = length*breadth*0.01,
         cyl_vol = 3.1415926535897932384626433832795028841971 * ((breadth/(2*3.1415926535897932384626433832795028841971))^2) * length,
         cuke_id = ziploc.number) 





##### Cucumber corrections by RM and HW in January & February 2020 #######
  # Join excretion_f1 and excretion_f2 into one df for plotting together
  excretion_bound_df <- bind_rows(excretion_f, excretion_f1) %>% 
  #calclate water that was originally in bag (slightly less than 2L) and
  #how much went missing (i.e. likely sucked up into cuke) and another
  #60mL were collected during the pre - sampling)
  mutate(missing_water = case_when(cuke_id == "FT18" ~ 1.2 - bag_volume_L,
                                   cuke_id == "FT19" ~ 1.2 - bag_volume_L,
                                   cuke_id == "FT20" ~ 1.2 - bag_volume_L,
                                   cuke_id == "DD20" ~ 1.5 - bag_volume_L,
                                   TRUE ~ 1.84 - bag_volume_L),
         true_mass = mass_g - missing_water*1020)

  #manually change the true mass for the 4 cukes that were in a different
  #volume of water
  write.csv(excretion_bound_df, "C:\\Users\\rachelmunger\\Documents\\SFU\\MSc\\MSc_Data\\cuke_msc
                                  \\chapter1\\excretion_data_cleaned_2020_02_11.csv")
  
  
  
  
####___Prelim exploratory FIGURES####
#____Fig 1
#previous (and incorrectly) measuerd_mass
bahamas_excretion_measured_mass <- ggplot(excretion_bound_df, aes((mass_g), (nh4_rate), 
                                                                    group = species, colour = species)) +
   geom_point() +
   geom_smooth(method = 'lm',  aes(fill = species)) +
   labs(y = "Ammonium Excretion (umol/h)", x = " True Mass (g)")

   #looks bad - this is looking at it with uncorrected mass on x axis
   bahamas_excretion_measured_mass


#____Fig 2
## Test plot of logged excretion days  
logged_bahamas<- ggplot(excretion_bound_df, aes(log(true_mass), log(nh4_rate), 
                               group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Log Ammonium Excretion (umol/h)", x = "Log Mass (g)") 

  #also looks bad
  logged_bahamas

#____Fig 3
#non-logged plotting of both excretion days using true_mass
bahamas_excretion_true_mass <- ggplot(excretion_bound_df, aes((true_mass), (nh4_rate), 
                                                    group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Ammonium Excretion (umol/h)", x = " True Mass (g)") 

  #looks more intuitive when not logged
  bahamas_excretion_true_mass

  #Save plot to desktop
  #ggsave(bahamas_excretion_true_mass, filename = "excretion_true_mass.png", bg = "transparent", height = 7, width = 9) 




  
  
####___Correcting for total amount of water in bag------------####
#if we assume the cukes have sucked up that missing water, we should
#include that water in our total volume since we multiply the total volume by
#the concentration to get the excretion rate

corrected_df <- excretion_bound_df %>%  
  mutate(cor_vol_L = bag_volume_L + missing_water,
         nh4_conc_pre = (int + slope * mean_preFLU),
         nh4_umol_pre = nh4_conc_pre * cor_vol_L + 0.060,
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio,
         nh4_umol_post = nh4_conc_post * cor_vol_L,
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre,
         nh4_rate = nh4_umol_diff / inc_time_h,
         nh4_rate_per_g = nh4_rate/wetweight,
         log_nh4_rate = log(nh4_rate))

#check to see if this changes the relationship
#removing 1 outlier (donkey dung with negative mass, row 20)
outlier_removed <- corrected_df[-c(20), ]
outlier_removed

outlier_removed2 <- outlier_removed %>% 
  mutate(log_true_mass = log(true_mass),
         log_length = log(length), 
         nh4_rate_per_g2 = nh4_rate/true_mass)

#____Fig 4A
# plotting with outlier_removed, 39 sea cucumbers 
corrected_total_water2 <- ggplot(outlier_removed, aes(x = true_mass, y = nh4_rate, group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Ammonium Excretion (umol/h)", x = " Mass (g)") 

  corrected_total_water2

##Just need to add a '+' to join the black background theme
  theme( # This part removes all the background and makes the plot transparent
    panel.background = element_rect(fill = "transparent",colour = NA), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "black",colour = NA)) +
  theme(legend.position = "none",
    legend.title = element_text(colour = "black", size = 11),
        axis.title = element_text(colour = "white", size = 14),
        legend.text = element_text(colour = "black", size = 8),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))

# Plot by day as well so that we can see if there's an "effect" of day, just looking visually 
#_____Fig 5____________________________________________________________________
corrected_df_bydate <- ggplot(outlier_removed, aes(x = true_mass, y = nh4_rate, 
                                                group = species, colour = date)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = "Ammonium Excretion (umol/h)", x = " Mass (g)") 

  corrected_df_bydate

  
  
####___BASIC MODEL_____________####

#June 12 ANCOVA 2020 from this site https://biol607.github.io/lab/11_glm_aic.html
#lm of excretion rate for two species
mod_1 <- lm(nh4_rate ~ true_mass + species,
            data=outlier_removed)
coef(mod_1)
summary(mod_1)
Anova(mod_1)

#testing asumptions of ANCOVA
par(mfrow=c(2,2))
plot(mod_1, which=c(1,2,5))
par(mfrow=c(1,1))
#histogram of residuals
hist(resid(mod_1))
#OR you can do: and it will say "Hit <Return> to see next plot:
plot(mod_1)

#And now look at residuals by group/predictors
library(car)
residualPlots(mod_1, tests=FALSE)

#test parallel presumption?
mod_2 <- lm(nh4_rate ~ true_mass * species, 
            data=outlier_removed)
summary(mod_2)
Anova(mod_2, type = "III")
drop1(mod_2, test='F')
#can see that there is no interaction so we are OK to proceed with parallel lines model (mod_1)

#F-tests using type III sum of squares
Anova(mod_1, type = "III")
#anova(mod_1)
anova(mod_1, mod_2)

mod_1$coefficients

library(emmeans)
adj_means <- emmeans(mod_1, ~species)
#adjusted means
adj_means

avg_sp_means <- emmeans(mod_1, ~species|true_mass)
avg_sp_means

#comparisons
contrast(avg_sp_means, method="tukey", adjust="none")

#no interaction
emtrends(mod_1, ~species, var = "true_mass")

##model outputs
drop1(mod_1, test='F')
summary(mod_1)
confint(mod_1, level = 0.95)
visreg(mod_1)




####_AIC - not using at this time_####
 #null model mod_0
 mod_00 <- lm(nh4_rate ~ 1, data=outlier_removed)

 mod_11 <- lm(nh4_rate ~ true_mass, data=outlier_removed)  

 mod_22 <- lm(nh4_rate ~ sizeindex, data=outlier_removed)  

 mod_33 <- lm(nh4_rate ~ true_mass + species, data=outlier_removed)
  
 mod_44 <- lm(nh4_rate ~ true_mass * species, data=outlier_removed)

 mod_55 <- lm(nh4_rate ~ sizeindex + species, data=outlier_removed)

 mod_66 <- lm(nh4_rate ~ sizeindex * species, data=outlier_removed)

  library(survival)
  library(kimisc) # has the nlist function to create a named list
  library(AICcmodavg) # has the aictab function
  library(dplyr)
  library(ggplot2)
  library(reshape2)

  #smaller AIC value indicates better fit of several potential candidate models 
  #AIC acts as a guard against overfitting. 
  AICc(mod_00)
  AICc(mod_11)
  AICc(mod_22)
  AICc(mod_33)
  AICc(mod_44)
  AICc(mod_55)
  AICc(mod_66)

  # Put the models all together in a named list using nlist function from kimisc package
  model_list <- lst(mod_00, mod_11, mod_22, mod_33, mod_44, mod_55, mod_66)
 
  # Compare models with AIC table
  aic_table <- aictab(model_list)
  aic_table
  
  #top model was mod_3
  summary(mod_33)
  
  
####___Model + Plot with Confidence Intervals_####
  # change interval to "prediction" to create prediction intervals later if you need, 
  # and change linetype from "blank" to "dashed" in geom_line
  # using mod_1 from line 257
  cuke_predict <- predict(modelFeb24, interval="confidence")
  
  head(cuke_predict)
  
  cuke_ci <- cbind(outlier_removed, cuke_predict)
  
  
  #nh4_rate_per_g = nh4_rate/wetweight, the rate at which the cuke
  #added NH4 to the bag for every gram of body weight
  per_g <- ggplot(outlier_removed2, aes(x = true_mass, y = nh4_rate_per_g,  group = species, colour= species)) + 
    geom_point(size = 2.6) +
    geom_smooth(method = 'lm', aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol per gram)", x = " Wet Mass (g)") +
    guides (col = FALSE) 
  per_g
  
  outlier_removed %>% 
    group_by(species) %>% 
    summarise(mean(nh4_rate_per_g))
  
  ####NH4 hr ~ true_mass with Confidence Intervals ####
  # use new cuke_ci
  
  p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) + 
    geom_point(size = 2.6) +
    geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
    geom_line(aes(y = upr, colour = species), linetype = "dashed") +
    geom_smooth(method = 'lm', aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
    guides (col = FALSE) 
  p0
  
  #### CH1 FIG 6 Feb 2021 - NH4 hr ~ true_mass with Confidence Intervals ####
  # use new cuke_ci
  ######### CORRECTED FIG FEB 25 2021 ############
 p2 <-  ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate, group = species, colour = species)) + 
    geom_point(size = 2.6) +
    geom_ribbon(aes(ymin=lwr, ymax=upr, fill = species), linetype = 0, alpha=0.6) +
    geom_smooth(method = 'lm', se= F) +
    scale_fill_manual(values = c("dd"="deepskyblue3", "ft"="darkorange")) +
    guides (col = FALSE) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") 
  
  p2 + scale_color_manual(values=c("deepskyblue3", "darkorange")) +
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
  
  ## FIG for WSN 2020
   p0 + scale_color_manual(values=c("blue", "darkorange2")) +
    scale_fill_manual(values=c("blue", "darkorange2"),
                      name="Species",
                      breaks=c("dd", "ft"),
                      labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 
   
   
   p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) +
     geom_point(size = 3) +
     geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
     geom_line(aes(y = upr, colour = species), linetype = "dashed") +
     geom_smooth(method=lm,  aes(fill = species)) +
     labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
     guides (col = FALSE) 
   
   #### Figs for WSN 2020 ####
   #### nh4 rate - mass 
   p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) +
   geom_point(size = 3) +
     #geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
     #geom_line(aes(y = upr, colour = species), linetype = "dashed") +
     geom_smooth(method=lm,  aes(fill = species)) +
     labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
     guides (col = FALSE) 
   
   
   p0 + scale_color_manual(values=c("blue", "deepskyblue")) +
     scale_fill_manual(values=c("blue", "deepskyblue"),
                       name="Species",
                       breaks=c("dd", "ft"),
                       labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 
   
   #### wet mass - length
   p0 <- ggplot(outlier_removed, aes(x = length, y = true_mass,  group = species, colour= species)) +
     geom_point(size = 3) +
     geom_smooth(method=lm,  aes(fill = species), alpha = 0.7) +
     labs(y = "Wet Mass (g)", x = " Length (cm)") 
   p0
   
   
   p0 + scale_color_manual(values=c("deepskyblue", "chocolate1")) +
     scale_fill_manual(values=c("deepskyblue", "chocolate1"),
                       name="Species",
                       breaks=c("dd", "ft"),
                       labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 

   
   ### for appendix july 26 2021
   mod1 <- lm(true_mass ~ length + species, 
              data=outlier_removed)
   summary(mod1)
   anova(mod1)
   cuke_predict2 <- predict(mod1, interval="confidence")
   head(cuke_predict2)
   
   cuke_ci2 <- cbind(outlier_removed, cuke_predict2)
   
   p0 <- ggplot(cuke_ci2, aes(x = length, y = true_mass, group = species, colour = species)) + 
     geom_point(size = 2.6) +
     geom_ribbon(aes(ymin=lwr, ymax=upr, fill = species), linetype=3, alpha=0.3) +
     geom_smooth(method = 'lm', se= F) +
     scale_fill_manual(values = c("dd"="blue", "ft"="darkorange2")) +
     guides (col = FALSE) +
     labs(y = "Wet Mass (g)", x = "Length (cm)") +
     scale_x_continuous(breaks=(seq(15, 50, 10))) +
     scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) 
   
   
   
   p0 + scale_color_manual(values=c("blue", "darkorange2")) +
     theme(panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           panel.border = element_blank(),
           axis.line.x = element_line(colour = "black"),
           axis.line.y = element_line(colour = "black"),
           axis.title.x = element_text(size=18, colour = "black"),
           axis.title.y = element_text(size=18, colour = "black"),
           axis.text.y = element_text(size=14, colour = "black"),
           axis.text.x = element_text(size=14, colour = "black")) 
   

  #ggsave(p0, filename = "excretion.png", bg = "transparent", height = 7, width = 9) 
  
  ###looking at logged excretion and logged mass
  p02 <- ggplot(outlier_removed, aes(log(true_mass), log(nh4_rate),  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p02
  
  ###looking at excretion and length
  p03 <- ggplot(outlier_removed, aes(length, nh4_rate,  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p03
  
  ###looking at logged excretion and log length
  p04 <- ggplot(outlier_removed, aes(log(length), log(nh4_rate),  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p04
  
  
  
####___Model + Plot with Prediction Intervals_####
  # 0. Build linear model (code from Jess)
  
  #center body size
  library(tidyverse)
  outlier_removed <- outlier_removed %>% mutate(true_mass.c = true_mass - (mean(true_mass)))
  outlier_removed <- outlier_removed %>% mutate(nh4_rate_per_g.c = nh4_rate_per_g - (mean(nh4_rate_per_g)))
  
  library(ggplot2)
  modelFeb24 <- lm(nh4_rate ~ true_mass.c + species, data = outlier_removed)
  summary(modelFeb24)


  # Print summary
  summary(modelFeb24)$coefficients
  
  outlier_removed %>%
    group_by(species) %>%
    summarise(max(true_mass), min(true_mass))
  
  # 1. Add predictions 
  ##prediciton intervals are around a single point, whereas a confidence interval is based around the mean
  pred.int <- predict(modelFeb24, interval = "prediction")
  rm_data <- cbind(outlier_removed, pred.int)
  
  # 2. Regression line + intervals
  pi <-  ggplot(rm_data, aes(x = true_mass.c, y  = nh4_rate,  group = species, colour = species)) +
    geom_point() +
    #geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
    #geom_line(aes(y = upr, colour = species), linetype = "dashed") +
    geom_smooth(method=lm,  aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Mass (g)") +
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
  pi
  
  
### feb 24 2021 size index  from hannahs comment
  sizei <- lm(nh4_rate ~ sizeindex + species, data = outlier_removed)
  summary(sizei)
  
  ggplot(outlier_removed, aes(x = sizeindex2, y = nh4_rate,  group = species, colour= species)) + 
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species), level = 0.95) 
  
  
  
####___Separate models for each species June23_####
  library(gapminder)
  library(tidyverse)
citation("car")
  
# donkey dung cukes
dd <- filter(outlier_removed, species == "dd")

dd_mod0 <- lm(nh4_rate ~ true_mass, data=dd)
summary(dd_mod0)

dd_mod00 <- lm(true_mass ~ length, data=dd)
summary(dd_mod00)

dd_mod1 <- lm(nh4_rate ~ 1, data=dd) 

dd_mod2 <- lm(nh4_rate ~ cyl_vol, data=dd)

dd_mod3 <- lm(nh4_rate ~ sizeindex, data=dd)  

dd_mod4 <- lm(nh4_rate ~ sizeindex2, data=dd)  

dd_mod4 <- lm(nh4_rate ~ breadth, data=dd)  


# five tooth cukes
ft <- filter(outlier_removed, species == "ft")

ft_mod0 <- lm(nh4_rate ~ length, data=ft)
summary(ft_0)

ft_mod00 <- lm(true_mass ~ length, data=ft)
summary(ft_mod00)


ft_mod1 <- lm(nh4_rate ~ 1, data=ft) 

ft_mod2 <- lm(nh4_rate ~ cyl_vol, data=ft)

ft_mod3 <- lm(nh4_rate ~ sizeindex, data=ft)  

ft_mod4 <- lm(nh4_rate ~ sizeindex2, data=ft)    

ft_mod4 <- lm(nh4_rate ~ breadth, data=ft) 

  # Put the models all together in a named list using nlist function from kimisc package
  #dds
  model_list2 <- lst(dd_mod0, dd_mod1, dd_mod2, dd_mod3, dd_mod4, dd_mod5)
  
  #fts
  model_list3 <- lst(ft_mod0, ft_mod1, ft_mod2, ft_mod3, ft_mod4, ft_mod5)
  
  # Compare models with AIC table
  #dds
  aic_table2 <- aictab(model_list2)
  aic_table2
  
  #fts
  aic_table3 <- aictab(model_list3)
  aic_table3

  
  ## Just looking to see if log vs unlogged model is more supported. Really low AICc for model2 vs model1
  model1 <- lm(nh4_rate ~ true_mass * species, data = outlier_removed)
  
  model2 <- lm(log(nh4_rate) ~ log(true_mass) * species, data = outlier_removed)
  
  Anova(model1)
  Anova(model2)
  
  AICc(model1)
  AICc(model2)
  
## JULY 2 LOG length weight graphs  
  #log mass ~ log length for DD
  ggplot(dd, aes(log(length), log(true_mass))) +
    geom_point(size = 2) +
    geom_smooth(method=lm)
  
  #log mass ~ log length for FT
  ggplot(ft, aes(log(length), log(true_mass))) +
    geom_point(size = 2) +
    geom_smooth(method=lm)
  
  dd_other2 <- lm(true_mass ~ sizeindex2, data=dd)
  dd_other3 <- lm(true_mass ~ length, data=dd)
  dd_other4 <- lm(true_mass ~ breadth, data=dd)
  dd_other5 <- lm(true_mass ~ cyl_vol, data=dd)
  
  ft_other2 <- lm(true_mass ~ sizeindex2, data=ft)
  ft_other3 <- lm(true_mass ~ length, data=ft)
  ft_other4 <- lm(true_mass ~ breadth, data=ft)
  ft_other5 <- lm(true_mass ~ cyl_vol, data=ft)


  
  
#### JULY 7-10 TRYING TO BOOTSTRAP the mass data from lab cukes ####
 
 ##just bootstrapping mass to start
 dd <- filter(outlier_removed, species == "dd")

 hist(dd$nh4_rate, las=1, xlab='NH4',
      col='red') 
 
 mean(dd$true_mass)
 median(dd$true_mass)
 
 xboot <- sample(dd$nh4_rate, replace=TRUE)
 hist(xboot)
 mean(xboot)
 
 bootstrap.mean2 <- function(){
   mean(sample(dd$nh4_rate, replace=TRUE))
   }

 ## call function once to test it
 bootstrap.mean2()
 ## replicate it 10^4 times (the 'simplify' argument in replicate
 ## just tells replicate to return a vector instead of a list)
 # Resample our sample 10^4 times, and calculate the mean each time.
 boot.vals <- replicate(1e4, bootstrap.mean2(), simplify=TRUE)
 
 hist(boot.vals, xlab='Mean', col='red', breaks=10)
 sd(boot.vals)
 
 #compare to standard error of the mean
 sd(dd$true_mass/sqrt(length(dd$true_mass)))
 
 mean(boot.vals)

 #confidence intervals
 quantile(boot.vals, probs=c(0.025, 0.975))
 
 #using boot package
 library(boot)
 
 calc.stat <- function(x,i)
   median(x$nh4_rate[i])
 
 z <- boot(data=dd,
           statistic=calc.stat,
           R=1e4)
 boot.ci(z, type = 'bca')
 boot.ci(z, type = 'perc')
 
 ##trying to bootstrap linear regression model
 dd <- filter(outlier_removed, species == "dd")
 
 dd$log.length <- log(dd$length)
 dd$log.true_mass <- log(dd$true_mass)
 
 dd_mod3 <- lm(log.true_mass ~ log.length, data=dd)
 summary(dd_mod3)
 slope.actual <- coef(dd_mod3)['log.length']
 
 #residual plot
 plot(x=fitted(dd_mod3), y=resid(dd_mod3),
      xlab='Fitted Values', ylab='Residuals', las=1, pch=16)
 abline(h=0, col='red')
 
 # qq-plot
 qqnorm(resid(dd_mod3), pch=16)
 abline(a=0, b=1, col='red')
 
 #write function to fit model to permuted data and get slope
 permute.and.get.slope <- function() {
   #permute just the response (permuting just the predictor, or
   #both would be equivalent)
   rr <- sample(dd$log.true_mass, replace=FALSE)
   pp <- dd$log.length
   out <- lm(rr ~ pp)
   coef(out)['pp']
 }
 
 ## test function once
 permute.and.get.slope()
 
 ## now calculate 10^4 slopes
 slopes.perm <-replicate(1e4, permute.and.get.slope(), simplify=TRUE)
 
 hist(slopes.perm, xlab='Permuted slopes', las=1)
 abline(v=slope.actual, col='red', lwd=2)
 
 frac.greater <- sum(slopes.perm>slope.actual) / length(slopes.perm)
 frac.greater
 2*frac.greater
 #predict from the model, also get SE
 p <- predict(dd_mod3, se=TRUE)
 
 #inspect estimated SEs
 p$se.fit
 
 library(car)
 # Fit the model 1000 times, apply a function each time. In this case, we predict from the model.
 bootfit1 <- boot(dd_mod3, function(x)predict(x), B=1000)
 
 bs <- function(dd,inds,formula=dd_mod3) 
 {
   d <- dd[inds,] 
   fit <- lm(formula=formula, data=d)
   return(coef(fit)) 
 } 
 
 results <- boot(data=dd, statistic=bs, R=1000, formula=dd_mod3)
 results

 
 
#### POLYNOMIAL/least sqaures REGRESSION JULY 10 #########  
 # from this site: https://www.theanalysisfactor.com/r-tutorial-4/ 
 
all_cukes <- lm(true_mass ~ length, data=outlier_removed)
 summary(all_cukes)
 
 mod3.2 <- lm(true_mass ~ poly(length, degree=2, raw=T), data=outlier_removed)
 summary(mod3.2)
 library(visreg)
 visreg(mod3.2)
 
 lengthvalues <- seq(16, 47, 1)
 predictedmass <- predict(mod3.2,list(length=lengthvalues, length2=lengthvalues^2))

 plot(outlier_removed$length, outlier_removed$true_mass, pch=16, xlab = "Length (cm)", ylab = "Mass", cex.lab = 1.3, col = "blue")
 lines(lengthvalues, predictedmass, col = "blue", lwd = 3)
 
####__AUGUST 4-10, making basic total estimates of pee contribution ####
outlier_removed %>% 
   group_by(species) %>% 
   summarize(mean(nh4_rate), sd(nh4_rate), min(nh4_rate), 
             max(nh4_rate), mean(true_mass), sd(true_mass), min(true_mass), max(true_mass), min(length), max(length))

cuke_est <- read.csv("rocksound_coords.csv")

##density of cukes pre, multiplied by average rate, multiplied by active hrs feeding/day
cuke_est2 <- cuke_est %>%
mutate(pee_ft2 = (ft_density_patch_grass_pre * 12.0 * 10)) %>%
mutate(pee_dd2 = (dd_density_patch_grass_pre * 15.6 * 12)) %>%
  mutate(pee_ft_upper = (ft_density_patch_grass_pre * 13.02 * 10)) %>%
  mutate(pee_dd_upper = (dd_density_patch_grass_pre * 16.69 * 12)) %>%
  mutate(pee_ft_lower = (ft_density_patch_grass_pre * 10.98 * 10)) %>%
  mutate(pee_dd_lower = (dd_density_patch_grass_pre * 14.51 * 12)) %>%
  mutate(ftp_error = (ft_original * 12.0 * 10)) %>%
  mutate(ddp_error = (dd_original * 15.6 * 12))
## You get the SAME values if you do this calculation:
##pee_ft = (ft_original * 12.0 * 10)/(patch_area + seagrass_area)) %>%
##pee_dd = (dd_original * 15.6 * 12)/(patch_area + seagrass_area))  

write.csv(cuke_est2, "C:\\Users\\rachelmunger\\Documents\\SFU\\MSc\\MSc_Data\\cuke_msc
                                  \\chapter1\\cuke_est2.csv")
#move mutated data to egestion_pot2
#.
#.
#.
egestion_pot2 <- read.csv("egestion_pot2.csv")

ggplot(data=egestion_pot2, aes(x=reorder(patch, -total_pee), y = total_pee, fill=species)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
          labs(y = "Estimated Ammonium Contribution (umol m2 day)", x = "Site") 

fig6 <- ggplot(data=egestion_pot2, aes(x=reorder(patch, -total_pee_sept), y = total_pee_sept, fill = species)) +
geom_bar(stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  ## mighta ctaully need to be units of umol day-1, because its rate*cukes at a site * active hours
  labs(y = "Estimated Ammonium Contribution (umol hour-1)", x = "Site") +
  geom_errorbar(aes(ymin=total_pee_sept-pee_error_sept, ymax=total_pee_sept+pee_error_sept), size=0.5,   
                width=.25,position=position_dodge(.9))

fig6


##Looking at total pee estimates separately for each species, 
#then making false axes (fake_g and fake_g2) to view them in a block format
dde <- filter(egestion_pot2, species == "dd")

ddee <- ggplot(data = dde, aes(x = fake_g, y = fake_g2)) +
  geom_tile(aes(fill = total_pee)) + 
  #theme(legend.position = "none") +
  scale_fill_distiller(palette = "YlOrBr",
                       limits = c(min(0), max(100)))


fte <- filter(egestion_pot2, species == "ft")

ftee <- ggplot(data = fte, aes(x = fake_g, y = fake_g2)) +
  geom_tile(aes(fill = total_pee)) + 
  #theme(legend.position = "none") +
  scale_fill_distiller(palette = "YlOrBr",
                       limits = c(min(0), max(100)))

library(cowplot)
theme_set(theme_cowplot())
plot_grid(ddee, ftee)

as.numeric(cuke_est2$ftp_error)


#### Trying to add error generated from resampling to basic calc ####
cuke_est2 <- read.csv("cuke_est2.csv")

ggplot(data=cuke_est2, aes(x=reorder(patch, -ftp_error), y = ftp_error)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(y = "Estimated Ammonium Contribution (umol hour-1)", x = "Site") +
  geom_errorbar(aes(ymax = ftp_error+ft_error, ymin = ftp_error-ft_error), width = 0.6)

ggplot(data=cuke_est2, aes(x=reorder(patch, -ddp_error), y = ddp_error)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(y = "Estimated Ammonium Contribution (umol hour-1)", x = "Site") +
  geom_errorbar(aes(ymax = ddp_error+dd_error, ymin = ddp_error-dd_error), width = 0.6)


mod99 <- lm(nh4_rate ~ true_mass + species, data = outlier_removed)

mod100 <- lm(log(nh4_rate) ~ log(true_mass) + species, data = outlier_removed)

summary(mod99)
Anova(mod99, type = "III")
outlier_removed$predicted <- predict(mod99)
outlier_removed$residuals <- residuals(mod99)

outlier_removed %>% select(nh4_rate, predicted, residuals) %>% head()

ggplot(outlier_removed, aes(x = length, y = nh4_rate)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)




hist(outlier_removed$true_mass)
hist(outlier_removed$length)

plot(nh4_rate ~ true_mass, data=outlier_removed)
plot(nh4_rate ~ length, data=outlier_removed)




## BOXplot of average excretion rates
for_LM <- ggplot(data=outlier_removed, aes(x = species, y = nh4_rate, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon3", "goldenrod3")) +
  labs(x = "Species", y = "NH4+ rate (umol/hr)") +
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
for_LM

#Cohen's d of average excretion rates between species
library(effsize)
cohen.d(nh4_rate ~ species,
        data = outlier_removed)

nh4_av <- lm( nh4_rate ~ species, data = outlier_removed)
summary(nh4_av)




 
