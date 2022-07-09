#looking at size distributions of two species
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(rmarkdown)
library(dplyr)  
library(visreg)


#read in cuke size data
cuke_size <- read.csv("cuke_size.csv")

#adding extra column for size index, calling it cuke_sizes (regular dataf is just cuke_size)
cuke_sizes <- cuke_size %>%
  mutate(sizeindex = sqrt(dist_length_cm*dist_girth))
#looking at the new DF
cuke_sizes

#looking at summary data
summary(cuke_sizes)

#______________________HISTOGRAMS of Length, Girth, and Size Index__________________________
# Basic histogram of disturbed length
length <- ggplot(cuke_sizes, aes(x=dist_length_cm)) + geom_histogram(binwidth=1, 
                                                          colour="black", fill="white")
# Basic histrogram of disturbed girth
girth <- ggplot(cuke_sizes, aes(x=dist_girth)) + geom_histogram(binwidth=1, 
                                                          colour="black", fill="white")


sizei <- ggplot(cuke_sizes, aes(x=sizeindex)) + geom_histogram(binwidth=1, 
                                                               colour="black", fill="white")

# Add mean line to length histograms
length2 <- length+ geom_vline(aes(xintercept=mean(dist_length_cm)),
              color="blue", linetype="dashed", size=1)

# Add mean line to girth histogram
girth2 <- length+ geom_vline(aes(xintercept=mean(dist_girth)),
                   color="blue", linetype="dashed", size=1)

sizei2 <- sizei+ geom_vline(aes(xintercept=mean(sizeindex)),
                   color="blue", linetype="dashed", size=1)

plot_grid(length2, girth2, sizei2, nrow=1)

# Histogram of length data with density plot
ggplot(cuke_size, aes(x=dist_length_cm)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Histogram of girth data with density plot
ggplot(cuke_size, aes(x=dist_girth)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

# Change histogram plot line colors to cuke species (DENSITY FUNCTION) (length)
# remove legend because it's going in a cowplot with another legend
lengths <- ggplot(cuke_sizes) +
  geom_histogram(aes(x = dist_length_cm, y=..density.., 
                     fill = species, colour = species), binwidth = 1, 
                 position="identity", alpha = 0.5) + theme(legend.position="none")
lengths

# Change histogram plot line colors to cuke species (DENSITY FUNCTION) (girth)
# remove legend because it's going in a cowplot with another legend
girths <- ggplot(cuke_sizes) +
  geom_histogram(aes(x = dist_girth, y=..density.., 
                     fill = species, colour = species), binwidth = 1, 
                 position="identity", alpha = 0.5) + theme(legend.position="none")
girths

#####__Cuke species size index density function FIGURE __######
si_index1 <- ggplot(cuke_sizes) +
  geom_histogram(aes(x = dist_length_cm, y=..density.., 
                      fill = species, colour = species), binwidth = 1.0, position = "identity",
                     alpha = 0.6) +
  labs(y = "Density", x = "Length")  +
  scale_colour_manual(values=c("firebrick4", "blueviolet")) +
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
si_index1

#### CH1 Fig 2 - Length histogram instead of size index - Sept 24 2020 ####
pt_1 <- ggplot(cuke_sizes, aes(x=dist_length_cm, fill = species, color=species), binwidth = 0.5) +
       labs(y = "Density", x = "Length (cm)") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.7)+
  geom_density(alpha=0.6)+
  theme_classic() +
  guides (col = FALSE) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))

 pt_2 <- pt_1 +  scale_color_manual(values=c("blue", "darkorange2"))+
  scale_fill_manual(values=c("blue", "darkorange2"),
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
   theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) 

 #for defense
 pt_2 +
  theme(  
   panel.background = element_rect(fill = "transparent",colour = NA),
   panel.grid.minor = element_blank(),
   panel.grid.major = element_blank(),
   plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
   panel.border = element_blank(),
   axis.ticks = element_line(size = 0.5, colour = "white"),
   axis.line.x = element_line(colour = "white"),
   axis.line.y = element_line(colour = "white"),
   axis.title.x = element_text(size=16, colour = "white"),
   axis.title.y = element_text(size=16, colour = "white"),
   axis.text.y = element_text(size=16, colour = "white"),
   axis.text.x = element_text(size=16, colour = "white"),
   legend.title = element_text(size = 14),
   legend.text = element_text(size = 10))


 setwd("/Users/rachelmunger/Documents/SFU/MSc/MSc_Data/cuke_msc/Figures")
 ggsave("length_histo.pdf", pt_2, height = 8, width = 12)

 
### summary data super imposed onto Fig 2 ####
 
 cuke_size %>% 
   group_by(species) %>% 
   summarize(mean(dist_length_cm), sd(dist_length_cm), min(dist_length_cm), max(dist_length_cm))

#cow plot of three histograms next to each other 
histos <- plot_grid(lengths, girths, si_index, nrow=1, labels=c('A', 'B', 'C'))
histos

##_____________  Kolmogorov-Smirnov test function ______________________________________

cuke_dd <- cuke_sizes %>%
  filter(., species == "dd") 
cuke_dd

cuke_ft <- cuke_sizes %>%
  filter(., species == "ft") 
cuke_ft

ks1 <- ks.test (cuke_ft$sizeindex, cuke_dd$sizeindex)
ks1

ks2 <- ks.test (cuke_ft$dist_length_cm, cuke_dd$dist_length_cm)
ks2

#ks3 <- ks.test (cuke_ft$dist_girth, cuke_dd$dist_girth)
#ks3
t.test(dist_length_cm ~ species, data = cuke_size)

#Kolmogrov plotting for size index----------------------Kolmogrov FIG 1
par(bty="l") 
plot(ecdf(cuke_ft$sizeindex), xlim=range(c(cuke_ft$sizeindex, cuke_dd$sizeindex)), 
     col="cyan3", main=NULL, 
     xlab="Size Index",
     ylab="Cumulative Probability (Fn(x))") 
legend(27, 0.8, legend=c("A. agassizi","H. mexicana"), col=c("cyan3", "salmon"), lty=1:2, box.lty=0) 
plot(ecdf(cuke_dd$sizeindex), add=TRUE, lty="dashed", col="salmon") +
geom_density(aes(color = sex), size =1)


#plotting Kolmogrov of lengths----------------------Kolmogrov FIG 2
plot(ecdf(cuke_ft$dist_length_cm), xlim=range(c(cuke_ft$dist_length_cm, cuke_dd$dist_length_cm)), col="dodgerblue")
plot(ecdf(cuke_dd$dist_length_cm), add=TRUE, lty="dashed", col="purple")


#plotting Kolmogrov of girths----------------------Kolmogrov FIG 3
plot(ecdf(cuke_ft$dist_girth), xlim=range(c(cuke_ft$dist_girth, cuke_dd$dist_girth)), col="dodgerblue")
plot(ecdf(cuke_dd$dist_girth), add=TRUE, lty="dashed", col="purple")


##_____________________ T-test between sizes of both species __________________

#model w size index and dist_lengh_cm return the same p-vals
size_diff_t1 <- lm(sizeindex ~ species,
                      data = cuke_sizes)

summary(size_diff_t1)
visreg(size_diff_t1)
drop1(size_diff_t1, test='F')
anova(size_diff_t1)

#size_diff_t2 <- lm(dist_length_cm ~ species,
                   #data = cuke_sizes)
#summary(size_diff_t2)
#visreg(size_diff_t2)
#drop1(size_diff_t2, test='F')
#anova(size_diff_t2)

size_diff_t <- t.test(sizeindex ~ species,
       data = cuke_sizes)
size_diff_t

size_diff_box <- boxplot(sizeindex ~ species,
        data = cuke_sizes)
size_diff_box


#_____________BOX PLOTS of Length, Girth, and Size Index for both species________________
#boxplot of the two species length
bplength <- ggplot(data=cuke_sizes, aes(x = species, y = dist_length_cm)) +
  geom_boxplot()+ theme_bw() + geom_jitter(shape=16, position=position_jitter(0.2)) 
bplength

#boxplot of the two species girth
bpgirth <- ggplot(data=cuke_sizes, aes(x = species, y = dist_girth)) +
  geom_boxplot()+ theme_bw() + geom_jitter(shape=16, position=position_jitter(0.2)) 

#boxplot of the two species size index
bpsizeindex <- ggplot(data=cuke_sizes, aes(x = species, y = sizeindex)) +
  geom_boxplot()+ theme_bw() + geom_jitter(shape=16, position=position_jitter(0.2)) 
bpsizeindex

#cow plot of three boxplots next to each other 
plot_grid(bplength, bpgirth, bpsizeindex, nrow=1)

