library(tidyverse)

bioturb <- read.csv("bioturbation2.csv")
head(bioturb)

individual_cukes <- read_csv("individual_cukes.csv") %>%
  ## 4 treatemnts (dd, ft, natural, zero) - but zero is not in this df for obvious reasons
  filter(treatment!='NA') %>%
  # 12 individuals we don't have data for, so remove those
  filter(species!='NA') %>%
  # only want to consider the cucumbers "kept" or "removed", ignore the ones that were "added"
  filter(remove_add_keep %in% c("keep", "remove")) 

##### Grams per hour Oct 9####
#grams per hour btwn species

whole_mod <- lm(g_per_hr ~ length, data = bioturb)
summary(whole_mod)

ggplot(bioturb, aes((species), (g_per_hr),  group = species, colour = species)) +
  geom_point(size = 3) +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "grams per hour", x = "Species") 


#model we want to use
bioturb_mod <- lm(g_per_hr ~ species, data = bioturb)
summary(bioturb_mod)

#no significant interaction between species
drop1(bioturb_mod, test='F')


#We have:
bt_rate_df <- 
  bioturb %>% 
  select(species, g_per_hr)

reef_data <- individual_cukes %>% 
  select(patch, species, cuke_num) %>%
  filter(species == "ft")

ft_rate_mean <- mean(filter(bt_rate_df, species == 'ft')$g_per_hr)

ft_rate_sd   <- sd(filter(bt_rate_df, species == 'ft')$g_per_hr)

ft_rate_dist <- rnorm(1000, mean = ft_rate_mean, sd = ft_rate_sd)

hist(ft_rate_dist)


reef_data2 <- reef_data %>% 
  filter(species == "ft") %>%
  group_by(patch, species) %>% 
  summarise(count = n())



#Write a function to let you sample however many cukes you want from the 
#ft rate sampling distribution
get_ft_rate <- function(nsamps) {
  rnorm(nsamps, mean = ft_rate_mean, sd = ft_rate_sd)
}

get_ft_rate(10) 

#Bootstrap samples from the sampling distribution nsamps times. 
#In this case, 10 ft are at NEW2, and we'll do this 100x.
nsamps = 10
nboot = 5000

sample_set <- 
  replicate(nboot, get_ft_rate(nsamps), simplify = FALSE) %>% 
  enframe(name = "samp", value = "rate") %>% 
  unnest(cols = c(rate))


# Get the total excretion rate for each re-sampling.
reef_rates <- 
  sample_set %>% 
  group_by(samp) %>% 
  summarise(reef_rate = sum(rate)) %>% 
  ungroup()

#Estimate for NEW2 ft rate of NH4 with SE
# Note: it is the with SE part that is the 'bonus' we get from doing the bootstrapping

mean(reef_rates$reef_rate)
sd(reef_rates$reef_rate) 
#se
sd(reef_rates$reef_rate) / sqrt(nsamps)

hist(reef_rates$reef_rate)


## Write a for() loop - start with just 1 spp of cuke (5tooths) at all sites

#can use this vector in for loop
count_vec <- reef_data2$count

#view(count_vec)

#constant to run before looping
nboot = 5000
nsamps = 1000


## ---- FUNCTION ---- ##

get_ft_rate <- function(nsamps) {
  rnorm(nsamps, mean = ft_rate_mean, sd = ft_rate_sd)
}

reef_rate_function <- function(df) {
  # Change the for loop to iterate over a row in the reef dataframe, that way 
  # you get to keep row id.
  reef_row <- 1:nrow(df)
  # My b, you did need somewhere to stash stuff, in this case I'm going to use a 
  # list, I find them easier to use for stuff like this.
  reef_rate_out_list <- list()
  # Loop!
  for (i in reef_row) {
    # Get reef specific information
    # indexing in R is hard, I always trial and error it
    #str(reef_data2[i, "count"])  # look at what this does compared to:
    #str(reef_data2[[i, "count"]])
    cuke_count = df[[i, "count"]]
    patch = df[[i, "patch"]]
    
    sample_set <- 
      replicate(nboot, get_ft_rate(nsamps = cuke_count), simplify = FALSE) %>% 
      enframe(name = "samp", value = "rate") %>% 
      unnest(cols = c(rate))
    
    reef_rates <- sample_set %>% 
      group_by(samp) %>% 
      summarise(reef_rate = sum(rate)) %>% 
      ungroup()
    #browser()  # let me peer into the for loop - my fav function after str()
    #Join each loop output with the previous loop outputs 
    reef_rates$cuke_count <- cuke_count
    reef_rates$patch <- patch
    reef_rate_out_list[[i]] <- reef_rates
  }
  # Output the final df with counts and output from each loop
  return(reef_rate_out_list)
  
  # Join each loop output with the previous loop outputs 
  reef_rates$cuke_count <- cuke_count
  reef_rates$patch <- patch
  reef_rate_out_list[[i]] <- reef_rates
}
# Output the final df with counts and output from each loop
return(reef_rate_out_list)


# Test the function
test_v2_m3 <- 
  reef_rate_function(df = reef_data2) %>% 
  bind_rows

# Add a new column to ft data frame w ft species column
test_v2_m3$species <- c("ft")

############ END OF THIS PART ###########################################################


test33_march4 <- bind_rows(test_v2_m3, test_v2_ddb_m4)


hist(test33_march4$reef_rate)

#dont really need this
testing_v2 <- test_v2 %>%
  group_by(patch) %>%
  summarize(mean(test$reef_rate), median(test$reef_rate), sd(test$reef_rate), sd(test$reef_rate) / sqrt(cuke_count))



get_sd_BT <- test_v2 %>%
  group_by(patch) %>%
  summarize(mean(reef_rate), sd(reef_rate))



#write csv of get_sd_BT
write.csv(get_sd_BT, "get_sd_BT_march2.csv")


#####cross plot ######

##load new data set of combined species simulations
#OLD AMMONIUM EXCRETION ESTIMATES  
#STILL NEED
bioturb_est_area <- read_csv("bioturb_cross_plot.csv")

#NEW bioturbation estimates
bioturb_est_area2 <- read_csv("get_sd_BT_march2.csv")

####Double error scatter plot - grams per hour, scales different
ggplot(data = bioturb_est_area2,aes(x = dd_gm2hr,y = ft_gm2hr, group=patch)) + 
  geom_point() +
  geom_errorbar(aes(ymin = ft_gm2hr - ft_gm2hr_sd,ymax = ft_gm2hr + ft_gm2hr_sd)) + 
  geom_errorbarh(aes(xmin = dd_gm2hr - dd_gm2hr_sd,xmax = dd_gm2hr + dd_gm2hr_sd)) +
  geom_abline(intercept = 0, slope = 5)


#grams per hour - same x and y scales
ggplot(data = bioturb_est_area,aes(x = ghr_m2_hmex,y = ghr_m2_aaga, group=patch)) + 
  geom_point() +
  geom_errorbar(aes(ymin = ghr_m2_aaga - ghr_m2_sd_aaga,ymax = ghr_m2_aaga + ghr_m2_sd_aaga)) + 
  geom_errorbarh(aes(xmin = ghr_m2_hmex - ghr_m2_sd_hmex,xmax = ghr_m2_hmex + ghr_m2_sd_hmex)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 1.08)+ ylim(0, 1.08)

## CH1 FIG 5 PT1####
#kg per m2 per year - same x and y scales
bioturb_march3 <- crosspp_bioturb <- ggplot(data = bioturb_est_area2,aes(x = dd_kgm2yr,y = ft_kgm2yr, group=patch)) + 
  geom_abline(intercept = 0, slope = 1, colour = "dark grey", linetype = "dashed") +
  geom_errorbar(aes(ymin = ft_kgm2yr - ft_kgm2yr_sd,ymax = ft_kgm2yr + ft_kgm2yr_sd), colour = "black", width = 0.2) + 
  geom_errorbarh(aes(xmin = dd_kgm2yr - dd_kgm2yr_sd,xmax = dd_kgm2yr + dd_kgm2yr_sd), colour = "black", height = 0.2) +
  geom_point(colour = "darkmagenta", size = 2) +
  xlim(0, 9.4)+ ylim(0, 9.4) +
  xlab ("H. mexicana bioturbation (kg" ~m^-2~ " " ~yr^-1~ ") ") + ylab("A. agassizii bioturbation (kg" ~m^-2~ " " ~yr^-1~ ") ") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.title.x = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) 

bioturb_march3

#the pee_plot graph is from weight_rate_simulation.R on line 320
panel <- plot_grid(bioturb_march3, pee_plot, nrow = 1)
panel

#don't need for thesis or MS
#march 3 2021
#deleted this csv but can be made v quickly using "get_sd_BT_march2"
march3_bioturb<- read_csv("march3_bioturb.csv")

march3 <- t.test(bioturb ~ species, data = march3_bioturb, paired=T)
march3

######### Histogram of faceted patches separated by species ####################
bioturb_test33 <- ggplot(test33_march4, aes(x= reef_rate, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Sediment bioturbation (grams" ~hour^-1~ ")") +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("blue", "darkorange2"))+
  scale_fill_manual(values=c("blue", "darkorange2"))+
  theme_classic() +
  facet_wrap(~patch, scales = "free_x") +
  theme(axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 16)) +
  guides (col = FALSE) 

#adding aesthetic stuff
bioturb_test33 + 
  scale_color_manual(values=c("blue", "darkorange2")) +
  scale_fill_manual(values=c("blue", "darkorange2"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) 


######### Histogram of pooled data separated by species ####################
graph_test333 <- ggplot(test33_march4, aes(x= reef_rate, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Sediment bioturbation (grams" ~hour^-1~ ")") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("indianred3", "slateblue3"))+
  scale_fill_manual(values=c("indianred3", "slateblue3"))+
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  guides (col = FALSE) 

#adding aesthetic stuff
graph_test3331 <- graph_test333 + scale_color_manual(values=c("blue", "darkorange2")) +
  scale_fill_manual(values=c("blue", "darkorange2"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.position = "none")

graph_test3331

### JAN 5 IMC suggested changes made to Ch 1 for bioturbation ###
# load csv with sites where only both species contributed (24 of 35 sites)

get_se_WR_jan5 <- read_csv(here("Documents/SFU/MSc/MSc_data/cuke_msc/Master_Data/chapter1/get_se_WR_jan5.csv"))

get_se_WR_jan5 %>% 
  group_by(species) %>% 
  summarize(mean(kgm2yr), sd(kgm2yr))

modjan5 <- t.test(kgm2yr ~ species, data = get_se_WR_jan5)
modjan5

#jan 13
ggplot(data=get_se_WR_jan5, aes(x = species, y = kgm2yr, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("indianred3", "slateblue3")) 


