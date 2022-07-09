## Excretion rate simulations at patches given predicted weight from length
library(tidyverse)
library(here)

#cukes in lab
#here::here("Master_Data/chapter1/
excretion_bound_df <- read_csv("excretion_data_cleaned_2020_02_11.csv")

#cukes in the field
individual_cukes <- read_csv("individual_cukes.csv") %>%
  ## 4 treatemnts (dd, ft, natural, zero) - but zero is not in this df for obvious reasons
  filter(treatment!='NA') %>%
  # 12 individuals we don't have data for, so remove those
  filter(species!='NA') %>%
  # only want to consider the cucumbers "kept" or "added", ignore the ones that were "added"
  filter(remove_add_keep %in% c("keep", "add")) %>%
  filter(treatment!='half') 


#predicted weights of cukes in the field
pred_weights <- read_csv("pred_weights2.csv") %>%
  ## 4 treatemnts (dd, ft, natural, zero) - but zero is not in this df for obvious reasons
  filter(treatment!='NA') %>%
  # 12 individuals we don't have data for, so remove those
  filter(species!='NA') %>%
  # only want to consider the cucumbers "kept" or "removed", ignore the ones that were "added"
  filter(remove_add_keep %in% c("keep", "add"))  %>%
  filter(treatment!='half') 

###___Correcting for total amount of water in bag------------###
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

#removing 1 outlier in row 20
outlier_removed <- corrected_df[-c(20), ]
outlier_removed

#relationships we have:
# - length weight relationship with predicted weights (with lowr and upr p intervals) at 
#   all field sites already generated (df pred_weights)
# - weight excretion rate relationship from lab data

#We are trying to obtain:
# -For a given cuke at a site with cuke length x, what is its excretion rate in relation to it's predicted mass?

#We have:
nh4_weight <- 
  outlier_removed %>% 
  select(species, length, true_mass, nh4_rate)

reef_cukes_ft <- pred_weights %>% 
  select(patch, species, cuke_num, length, true_mass, lwr, upr) %>%
  filter(species == "ft")


# SLM we are ultimately interested in using, but need to deal with weight~length error first by resampling
mod_11 <- lm(nh4_rate ~ true_mass + species,
            data=outlier_removed)
library(car)
Anova(mod_11, type = "III")
anova(mod_11)
summary(mod_11)

# _________________________________________________________________________________
# 3a) for each cuke length sampled on a reef use the weight ~ length model to get 
# a weight estimate for each length (done)

# Can find predicted weights in pred_excr df
# _________________________________________________________________________________


# _________________________________________________________________________________
# 3b) repeat this 10x to start (then 1000 or whatever), this gets you a population 
# of weights, I would probably dump each of these dataframes into a list, 
# so the list will be 10 long, with a df with nrow = n reefs * n cukes
# _________________________________________________________________________________

# I hadn't realized before that there is really no difference between species
# for the weight model is there? If so then species identity doesn't matter for 
# this first part
wt_mod <- lm(true_mass ~ length, data=outlier_removed)
summary(wt_mod)
# Regression coefficients:
coef(wt_mod)
coef(wt_mod)[[1]]  # Intercept
coef(wt_mod)[[2]]   # Length coefficient

#get weight for a particular length. 
#So get_weight(27) is drawing a weight for a cuke of length 27 within the conf int range..?
get_weight <- function(cuke_length, nsamps) {
  #y = mx + b aka  weight_mu = length_coeff(length value) + intercept
  nsamps = 1 
  weight_mu <- coef(wt_mod)[[1]] + coef(wt_mod)[[2]] * cuke_length
  weight_sd <- sigma(wt_mod)   # see ?sigma; residual error
  weight <- rnorm(n = 1, mean = weight_mu, sd = weight_sd)
  return(weight)
}

get_weight(27)

# Run one test set to make sure everything is working
# ------------------------------------------------------------------------------
# This way I don't have to rerun the individual_cukes load in code every time I 
# test stuff, it's more contained
cuke_weights <- individual_cukes  
cuke_weights$est_weight <- NA  # make a place to stash weight estimates

for (i in 1:nrow(cuke_weights)) {
  cuke_length <- cuke_weights$length[[i]]
  
  est_weight <- get_weight(cuke_length = cuke_length)
  cuke_weights$est_weight[i] <- est_weight[[1]]
}

cuke_weights  # take a look
# ------------------------------------------------------------------------------
# Let's just make the raw dataframe bigger so that we just iterate over more 
# rows to get more samples for each length
#the only place the number of samples are coming into play, set up the resampling here and data frame here
nsamps <- 5000
cuke_weights <- 
  individual_cukes %>% 
  slice(rep(1:n(), each = nsamps))  # repeat rows in df

cuke_weights$est_weight <- NA  # make a place to stash weight estimates
# This is pretty slow
for (i in 1:nrow(cuke_weights)) {
  cuke_length <- cuke_weights$length[[i]]
  
  est_weight <- get_weight(cuke_length = cuke_length)
  cuke_weights$est_weight[i] <- est_weight[[1]]
}

# a faster way: apply the function on every row using rowwise() and mutate()
# Note I didn't save this to an object yet, I just ran it to see if it was faster

#not sure if I can identify the change being made in 'cuke weights' when I increase nsamps here.
#get rid of nsamps in get_weight function
#nsamps = 100

cuke_weights %>% 
  rowwise() %>% 
  mutate(est_weight2 = get_weight(nsamps = 100, length))

cuke_weights  # take a look


# Now that you have a population of estimated cuke_weights, you can do the same
# thing to look at the excretion rates for each weight
# ------------------------------------------------------------------------------
rate_mod <- lm(nh4_rate ~ true_mass + species, data=outlier_removed)

# make your life easier using dummy variables and one rate function instead of 
# having to do it in two
rate_df <- 
  cuke_weights %>% 
  mutate(spp_code = ifelse(species == "dd", 0, 1))

get_rate <- function(mass, species) {
  # note, when species == 0 it is dd and the last term drops out since the 
  # intercept is the value for dd; when species == 1, it's ft so we add the 
  # difference from the dd intercept.
  excretion_mu <- coef(rate_mod)[[1]] + coef(rate_mod)[[2]] * species
  excretion_sd <- sigma(rate_mod)
  rate <- rnorm(n = 1, mean = excretion_mu, sd = excretion_sd)
  return(rate)
}

# excretion estimates for each weight estimate
test <- 
rate_df %>% 
  rowwise() %>% 
  mutate(rate = get_rate(mass = true_mass, species = spp_code))

view(test)

test2 <- 
test %>% 
  group_by(patch, species, cuke_num) %>% 
  # to get a bunch of reef rate totals, we need some kind of sample id,
  # I googled "unique id row within group dplyr" to track down this method
  # I scrolled down to the bottom of this QA: https://community.rstudio.com/t/how-to-add-a-counter-to-each-group-in-dplyr/12986/2
  mutate(samp_id = row_number()) %>% 
  #slice(1:100) %>% view
  ungroup()

# Double check that the grouping gives us one 'resampling' of a reef
filter(test2, samp_id == 1) %>% arrange(patch, species, cuke_num) %>% view

individual_cukes %>% filter(patch == 119)

# Get a population of simulated excretions at the reef level. 
test3 <- test2 %>% 
  group_by(patch, species, samp_id) %>% 
  summarise(total_excr = sum(rate))

view(test3)

## Rough look at Test 3 using paired boxplots at each site
ggplot(data=test3, aes(x = patch, y = total_excr, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("indianred3", "slateblue3"))

######### Histogram of pooled data separated by species ####################
graph_test3 <- ggplot(test3, aes(x= total_excr, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Ammonium excretion (umol" ~hour^-1~ ")") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("blue", "darkorange2"))+
  scale_fill_manual(values=c("blue", "darkorange2"))+
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  guides (col = FALSE) 

#adding aesthetic stuff
graph_test3excr <- graph_test3 + scale_color_manual(values=c("blue", "darkorange2")) +
  scale_fill_manual(values=c("blue", "darkorange2"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.position = "none")
graph_test3excr

##### CH1 FIG S1 panel x2 March 4 #####
library(cowplot)
plot_grid(graph_test3excr, graph_test3331)


######### Histogram of faceted patches separated by species ####################
# Why is density so much lower than nh4_rate_simulation that doesn't take weight into consideration?
patches_test3 <- ggplot(test3, aes(x= total_excr, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Ammonium Excretion (umol" ~hour^-1~ ")") +
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
patches_test3 + scale_color_manual(values=c("blue", "darkorange2")) +
  scale_fill_manual(values=c("blue", "darkorange2"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) 


###############################################################

# summarize by taking mean, sd, and se by taking reef level total at each site from every re-sampling.
# se calculations are different when you call (mean(test3$total_excr)) vs just (mean(total_excr)).
# did calcs in excel and it seems to me that (mean(total_excr)) calculates the correct se but not sure.
get_se_WR_march12 <- test3 %>%
  group_by(patch, species) %>%
  summarize(mean(total_excr), sd(total_excr), sd(total_excr) / sqrt(10)) 


# couldn't rename columns for some reason, tried using both 'mutate' and 'rename' in different ways, 
# but didn't work, so ended up writing 'get_se_WR' to csv so I could rename columns 
write.csv(get_se_WR_march12, "C:\\Users\\rachelmunger\\Documents\\SFU\\MSc\\MSc_Data\\msc_chapter1-2\\Master_Data\\chapter2\\get_se_WR_march12.csv")

library(here)
get_se_WR2 <- read_csv(here("Documents/SFU/MSc/MSc_data/cuke_msc/Master_Data/chapter1/get_se_WR_v2.csv"))


## Calculate average and se umol/h/m2
get_se_WR2 %>% 
  group_by(species) %>% 
  summarize(mean(umol_h_m2), sd(umol_h_m2))

get_se_WR3 <- get_se_WR2 %>% 
  summarize(mean(umol_h_m2), sd(umol_h_m2))


####Double error scatter plot?
library(here)
dbl_error <- read_csv(here("Documents/SFU/MSc/MSc_Data/msc-chapter1-2/Master_Data/chapter1/nh4_cross_plot.csv"))
dbl_error <- read_csv("nh4_cross_plot.csv")

ggplot(data = dbl_error,aes(x = umolh_m2_hmex,y = umolh_m2_aaga, group=patch)) + 
  geom_point() +
  geom_errorbar(aes(ymin = umolh_m2_aaga - umolh_m2_sd_aaga,ymax = umolh_m2_aaga + umolh_m2_sd_aaga)) + 
  geom_errorbarh(aes(xmin = umolh_m2_hmex - umolh_m2_sd_hmex,xmax = umolh_m2_hmex + umolh_m2_sd_hmex)) +
  geom_abline(intercept = 0, slope = 5)


#same x and y scales
ggplot(data = dbl_error,aes(x = umolh_m2_hmex,y = umolh_m2_aaga, group=patch)) +
  geom_abline(intercept = 0, slope = 1, colour = "dark grey", size = 1.5) + 
  geom_point() +
  geom_errorbar(aes(ymin = umolh_m2_aaga - umolh_m2_sd_aaga,ymax = umolh_m2_aaga + umolh_m2_sd_aaga)) + 
  geom_errorbarh(aes(xmin = umolh_m2_hmex - umolh_m2_sd_hmex,xmax = umolh_m2_hmex + umolh_m2_sd_hmex)) +
  xlim(0, 15.3)+ ylim(0, 15.3)

hmex <- dbl_error$umolh_m2_hmex
aaga <- dbl_error$umolh_m2_aaga


t.test(hmex, aaga, paired = TRUE)

####  CH1 FIG 5 PT2 ####
pee_plot <- ggplot(data = dbl_error, aes(x = umolh_m2_hmex,y = umolh_m2_aaga, group=patch)) +
  geom_abline(intercept = 0, slope = 1, colour = "dark grey", linetype = "dashed") + 
  geom_errorbar(aes(ymin = umolh_m2_aaga - umolh_m2_sd_aaga,ymax = umolh_m2_aaga + umolh_m2_sd_aaga), colour = "black", width = 0.2) + 
  geom_errorbarh(aes(xmin = umolh_m2_hmex - umolh_m2_sd_hmex,xmax = umolh_m2_hmex + umolh_m2_sd_hmex), colour = "black", height = 0.2) +
  geom_point(colour = "darkmagenta", size = 2) +
  xlim(0, 15.3)+ ylim(0, 15.3) +
  xlab ("H. mexicana excretion (umol" ~m^-2~ " " ~h^-1~ ") ") + ylab ("A. agassizii excretion (umol" ~m^-2~ " " ~h^-1~ ") ") +
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



######### Figure of average ammonium contribution to each patch, separated by species, ###########
#with std error generated from all the reef totals for a site

#Does se look correct on estimates as calculated above in get_se_WR? 
#Is there a better way to graph this (will make figure of estimates on a map)?
#Wanted to do stacked barplot so there were fewer bars along x axis,
#but didn't know if it's weird to try and put two error bars on a stacked barplot?
resamp_error <- ggplot(complete(get_se_WR2, patch, species, fill = list(mean = 0)), 
                    aes(x=reorder(patch, -mean), y = mean, fill = species)) +
  geom_bar(stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(y = "Estimated Ammonium Contribution (umol hour-1)", x = "Site") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), size=0.5,   
                width=.25,position=position_dodge(.9))

#adding aesthetic stuff
resamp_error + scale_color_manual(values=c("indianred3", "slateblue3")) +
  scale_fill_manual(values=c("indianred3", "slateblue3"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=18)) 





######## scale by patch area - not relevant rn ###############
excretion_per_h_m2 <- ggplot(complete(get_se_WR2, patch, species, fill = list(mean = 0)), 
                      aes(x=reorder(patch, -excr_perh_m2), y = excr_perh_m2, fill = species)) +
  geom_bar(stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(y = "Estimated Ammonium Contribution (umol hour-1 metre-2)", x = "Site") 

#adding aesthetic stuff
excretion_per_h_m2 + scale_color_manual(values=c("indianred3", "slateblue3")) +
  scale_fill_manual(values=c("indianred3", "slateblue3"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=18)) 


### JAN 5 IMC suggested changes made to Ch 1 ###
# load csv with sites where only both species contributed (24 of 35 sites)

get_se_WR_jan5 <- read_csv(here("Documents/SFU/MSc/MSc_data/cuke_msc/Master_Data/chapter1/get_se_WR_jan5.csv"))

get_se_WR_jan5 %>% 
  group_by(species) %>% 
  summarize(mean(umol_h_m2), sd(umol_h_m2))

modjan5 <- t.test(umol_h_m2 ~ species, data = get_se_WR_jan5)
modjan5
