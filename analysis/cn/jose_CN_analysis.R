# script for analyzing CN data from GUMO samples
# for Jose Villeda's Honors project

## load packages
library(dplyr)
# install.packages('lme4')
library(lme4)
library(car)
library(emmeans)
# install.packages('ggplot2')
library(ggplot2)

## load (i.e., read) the data
cn_data = read.csv('../../data/cn/jose_gumo_1_results.csv')
species_info = read.csv('../../data/cn/species_information.csv', fileEncoding = 'Latin1')

## look at data
head(cn_data)
head(species_info)

## merge dataasets
cn_species_data = left_join(cn_data, species_info, by = c("Sample_ID" = "sample_id"))
head(cn_species_data)

## calculate variables
cn_species_data$cn = cn_species_data$Carbon_Percent / cn_species_data$Nitrogen_Percent
cn_species_data$lma = cn_species_data$mass / cn_species_data$area

## make some simple graphs
### histogram of the percent nitrogen data
hist(cn_species_data$Nitrogen_Percent)
hist(cn_species_data$cn)
hist(cn_species_data$lma)
plot(cn_species_data$Sample_Weight, cn_species_data$Carbon_Weight)

## test hypotheses about herbaceous/woody
### make new column describing whether species is woody or not
cn_species_data$woody <- NA
cn_species_data$woody[cn_species_data$life_history == 'Forb' | cn_species_data$life_history == 'Grass'] <- 'no'
cn_species_data$woody[cn_species_data$life_history == 'Shrub' | cn_species_data$life_history == 'Tree'] <- 'yes'

cn_species_data$PFT[cn_species_data$life_history == 'Grass' & 
                      cn_species_data$photosynthesis_type == 'C3'] <- 'C3 grass'
cn_species_data$PFT[cn_species_data$life_history == 'Grass' & 
                      cn_species_data$photosynthesis_type == 'C4'] <- 'C4 grass'
cn_species_data$PFT[cn_species_data$life_history == 'Forb' & 
                      cn_species_data$photosynthesis_type == 'C3'] <- 'C3 forb'
cn_species_data$PFT[cn_species_data$life_history == 'Shrub' & 
                      cn_species_data$photosynthesis_type == 'C3'] <- 'C3 shrub'
cn_species_data$PFT[cn_species_data$life_history == 'Tree' & 
                      cn_species_data$photosynthesis_type == 'C3'] <- 'C3 tree'

### test whether C/N ratios differ across plant type and environment
cn_lmer <- lmer(log(cn) ~ PFT * elevation_m + (1|family), 
                data = cn_species_data)
plot(resid(cn_lmer) ~ fitted(cn_lmer))
summary(cn_lmer)
Anova(cn_lmer)
cld(emmeans(cn_lmer, ~PFT))
emtrends(cn_lmer, ~1, var = 'elevation_m')

### test whether percent N differ across plant type and environment
nper_lmer <- lmer(log(Nitrogen_Percent) ~ PFT * elevation_m + (1|family), 
                data = cn_species_data)
plot(resid(nper_lmer) ~ fitted(nper_lmer))
summary(nper_lmer)
Anova(nper_lmer)
cld(emmeans(nper_lmer, ~PFT))
emtrends(nper_lmer, ~1, var = 'elevation_m')

### test whether percent lma differs across plant type and environment
lma_lmer <- lmer(log(lma) ~ PFT * elevation_m + (1|family), 
                  data = cn_species_data)
plot(resid(lma_lmer) ~ fitted(lma_lmer))
summary(lma_lmer)
Anova(lma_lmer)
cld(emmeans(lma_lmer, ~PFT))
emtrends(lma_lmer, ~1, var = 'elevation_m')

### results summary
# biggest effect is elevation, where there is great N at higher elevation
# follows predictions from Wang et al. (2017), which suggests
# that plants increase leaf N at high elevation because of high water stress
# more N allows them to maintain photosynthesis at lower stomatal conductance
# could test this with the stomatal data from Zach

### make plots
#### quick plots
plot(log(Nitrogen_Percent) ~ elevation_m, data = cn_species_data)
plot(log(cn) ~ elevation_m, data = cn_species_data)

#### nice plots
##### percent nitrogen x elevation
elevation_vector = seq(min(cn_species_data$elevation_m, na.rm = T), 
                       max(cn_species_data$elevation_m, na.rm = T), 1)
nper_elevation_slope = summary(emtrends(nper_lmer, ~1, var = 'elevation_m'))[1, 2]
nper_elevation_intercept = summary(emmeans(nper_lmer, ~1, at = list(elevation_m = 0)))[1, 2]
nper_modeled = nper_elevation_slope * elevation_vector + nper_elevation_intercept
nper_model_df = data.frame(elevation = elevation_vector, nper = nper_modeled)

ggplot(data = cn_species_data, aes(x = elevation_m, y = Nitrogen_Percent, shape = PFT)) +
  theme(legend.position = "right", 
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_point(colour = rgb(0, 0, 0, 0.5), size = 4) +
  geom_line(data = nper_model_df, aes(x = elevation, y = exp(nper), shape = NULL), 
            size = 4, col = rgb(0, 0, 0, 0.7)) +
  ylab('Leaf N (%)') +
  xlab('Elevation (m)') +
  xlim(c(500, 2500)) +
  ylim(c(0, 5))
