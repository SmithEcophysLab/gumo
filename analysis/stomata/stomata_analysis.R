# Stomata analysis for Zach Bailey's GUMO project 2019

library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)

#Define the raw data table, tab delimited with headers.
#Get the data from: "https://texastechuniversity-my.sharepoint.com/:t:/g/personal/matt_johnson_ttu_edu/EUylw9GCTWlKvxgcO0pQxOgBwDJQnthjJ0IUWVAVcQ-wrA?e=Xd43zm"
stomata_fn = "../../data/stomata/stomata_july2019.txt"



#Read in the data frame
stomata = read.csv(stomata_fn,sep="\t",header=T)

#Use dplyr to group the data by specimen
byTTCNum = group_by(stomata,TTCNum)

image_size = 302.8*227.1

#Create a table summarizing all the data by specimen
stomata.by.ttcNUM=summarize(byTTCNum,
                            count=n(), # number of stomata per specimen
                            Species=Species[1],
                            LifeHistory=LifeHistory[1],
                            images=length(unique(ImageFN)), # images per specimen
                            area.mean=mean(Area), # mean stomatal area per specimen
                            area.sd=sd(Area),
                            density=1e6*count/(images*image_size), # num. stomata per image size per specimen
                            prop.stomata = sum(Area)/image_size) # area occupied per specimen
# p = ggplot(stomata.by.ttcNUM,
#           aes(x=density,y=area.mean)) +
#   geom_point(aes(shape=Species,col=LifeHistory),size=3.5,stroke=1.5)+
#   scale_shape_manual(values = 0:17)+
#   labs(y=expression("Mean Stomatal Area" ~µm^2),
#        x=expression("Stomatal Density per"~ mm^2))+
#   theme_gray()
# p

# b = ggplot(stomata.by.ttcNUM,aes(x=Species,
#                                  y=prop.stomata,
#                                  col=LifeHistory)) +
#   geom_point(size=3) + 
#   labs(y="Stomatal Proportion of Leaf Area",
#        x="Species")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   
# b

### add in CN data
cn_data_raw = read.csv('../../data/cn/jose_gumo_results_clean.csv')
cn_data = cn_data_raw[cn_data_raw$Bad_Load == 0 & cn_data_raw$Low_Response == 0,]
species_info = read.csv('../../data/cassidy_data/CarbonNitrogenSampling_GUMO.csv', fileEncoding = 'Latin1')

## look at data
head(cn_data)
head(species_info)
head(stomata.by.ttcNUM)

## merge dataasets
cn_species_data = left_join(cn_data, species_info, by = c("Sample_ID" = "Sample"))
head(cn_species_data)

all_data = left_join(cn_species_data, stomata.by.ttcNUM, by = c("TTCNumber" = "TTCNum"))
head(all_data)
colnames(all_data)
all_data[,c(15, 22)]

### make new column describing whether species is woody or not
all_data$woody[all_data$LifeHistory.x == 'Forb' | all_data$LifeHistory.x == 'Grass'] <- 'no'
all_data$woody[all_data$LifeHistory.x == 'Shrub' | all_data$LifeHistory.x == 'Tree'] <- 'yes'

## statistical analyses
stomata_lmer <- lmer((prop.stomata) ~ woody * Elevation.m. + (1|Family), 
                  data = all_data)
plot(resid(stomata_lmer) ~ fitted(stomata_lmer))
summary(stomata_lmer)
Anova(stomata_lmer)
cld(emmeans(stomata_lmer, ~woody))
test(emtrends(stomata_lmer, ~1, var = 'Elevation.m.'))

stomata_n_lmer <- lmer(log(Nitrogen_Percent) ~ prop.stomata * Elevation.m. * woody + (1|Family), 
                     data = all_data)
plot(resid(stomata_n_lmer) ~ fitted(stomata_n_lmer))
summary(stomata_n_lmer)
Anova(stomata_n_lmer)
test(emtrends(stomata_n_lmer, ~1, var = 'Elevation.m.'))
test(emtrends(stomata_n_lmer, ~1, var = 'prop.stomata'))

all_data$cn = all_data$Carbon_Percent / all_data$Nitrogen_Percent
stomata_cn_lmer <- lmer(log(cn) ~ prop.stomata * Elevation.m. + (1|Family), 
                        data = all_data)
plot(resid(stomata_cn_lmer) ~ fitted(stomata_cn_lmer))
summary(stomata_cn_lmer)
Anova(stomata_cn_lmer)
test(emtrends(stomata_cn_lmer, ~1, var = 'Elevation.m.'))
test(emtrends(stomata_cn_lmer, ~1, var = 'prop.stomata'))

plot(Nitrogen_Percent ~ prop.stomata, data = all_data)

all_data$n_per_stomata = all_data$Nitrogen_Percent / all_data$prop.stomata
n_per_stomata_lmer <- lmer(log(n_per_stomata) ~ Elevation.m. + (1|Family), 
                       data = all_data)
plot(resid(n_per_stomata_lmer) ~ fitted(n_per_stomata_lmer))
summary(n_per_stomata_lmer)
Anova(n_per_stomata_lmer)
test(emtrends(n_per_stomata_lmer, ~1, var = 'Elevation.m.'))


