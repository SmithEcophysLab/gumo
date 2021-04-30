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
library(scales)

## load (i.e., read) the data
cn_data_raw = read.csv('../../data/cn/jose_gumo_results_clean.csv')
cn_data = cn_data_raw[cn_data_raw$Bad_Load == 0 & cn_data_raw$Low_Response == 0,]
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
plot(cn_species_data$elevation_m, cn_species_data$Nitrogen_Percent)

## test hypotheses about herbaceous/woody
### make new column describing whether species is woody or not
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

### check variable type
class(cn_species_data$PFT)
class(cn_species_data$Nitrogen_Percent)

### practice graph
plot(as.factor(cn_species_data$PFT), cn_species_data$Nitrogen_Percent)

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

### lma-nper correlation
nperlma_lmer <- lmer(log(Nitrogen_Percent) ~ PFT * log(lma) + (1|family), 
                  data = cn_species_data)
plot(resid(nperlma_lmer) ~ fitted(nperlma_lmer))
summary(nperlma_lmer)
Anova(nperlma_lmer)
cld(emmeans(nperlma_lmer, ~PFT))
emtrends(nperlma_lmer, ~1, var = 'lma')

### results summary
# biggest effect is elevation, where there is great N at higher elevation
# follows predictions from Wang et al. (2017), which suggests
# that plants increase leaf N at high elevation because of high water stress
# more N allows them to maintain photosynthesis at lower stomatal conductance
# could test this with the stomatal data from Zach
# light also goes up so plants invest in more enzymes to use it for photosynthesis
# temperature goes down as you increase elevation, so plants invest in more
# enzymes because they are working more slowly

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

nper_plot = ggplot(data = cn_species_data, aes(x = elevation_m, y = Nitrogen_Percent, 
                                   shape = PFT, colour = PFT)) +
  theme(legend.position = "right", 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_point(alpha = 0.9, size = 4) +
  geom_line(data = nper_model_df, aes(x = elevation, y = exp(nper), 
                                      shape = NULL, colour = NULL), 
            size = 4, col = rgb(0, 0, 0, 0.7)) +
  ylab('Leaf N (%)') +
  xlab('Elevation (m)') +
  xlim(c(500, 2500)) +
  ylim(c(0, 5))

jpeg('plots/nper_plot.jpeg',
     width = 8, height = 8, units = 'in', res = 600)
plot(nper_plot)
dev.off()

nper_plot_woody = ggplot(data = cn_species_data, 
                       aes(x = PFT, y = Nitrogen_Percent, colour = woody)) +
  theme(legend.position = "right", 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_boxplot(outlier.size = NA, size = 1) +
  geom_jitter(size = 4, alpha = 0.9, width = 0.1) +
  guides(colour = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c('Herbaceous', 'Woody')) +
  ylab('Leaf N (%)') +
  xlab('Plant Functional Type')

jpeg('plots/nper_plot_woody.jpeg',
     width = 8, height = 8, units = 'in', res = 600)
plot(nper_plot_woody)
dev.off()

##### cn x elevation
cn_elevation_slope = summary(emtrends(cn_lmer, ~1, var = 'elevation_m'))[1, 2]
cn_elevation_intercept = summary(emmeans(cn_lmer, ~1, at = list(elevation_m = 0)))[1, 2]
cn_modeled = cn_elevation_slope * elevation_vector + cn_elevation_intercept
cn_model_df = data.frame(elevation = elevation_vector, cn = cn_modeled)

cn_plot = ggplot(data = cn_species_data, aes(x = elevation_m, y = cn, 
                                   shape = PFT, colour = PFT)) +
  theme(legend.position = "right", 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15), 
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_point(alpha = 0.9, size = 4) +
  geom_line(data = cn_model_df, aes(x = elevation, y = exp(cn), 
                                      shape = NULL, colour = NULL), 
            size = 4, col = rgb(0, 0, 0, 0.7)) +
  ylab('Leaf C:N') +
  xlab('Elevation (m)') +
  xlim(c(500, 2500)) +
  ylim(c(0, 60))

jpeg('plots/cn_plot.jpeg',
     width = 8, height = 8, units = 'in', res = 600)
plot(cn_plot)
dev.off()

cn_plot_woody = ggplot(data = cn_species_data, 
                         aes(x = PFT, y = cn, colour = woody)) +
  theme(legend.position = "right", 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_boxplot(outlier.size = NA, size = 1) +
  geom_jitter(size = 4, alpha = 0.9, width = 0.1) +
  guides(colour = guide_legend(title = NULL)) +
  scale_color_discrete(labels = c('Herbaceous', 'Woody')) +
  ylab('Leaf C:N') +
  xlab('Plant Functional Type')

jpeg('plots/cn_plot_woody.jpeg',
     width = 8, height = 8, units = 'in', res = 600)
plot(cn_plot_woody)
dev.off()

##### nper-lma trend (to show fast-slow tradeoff)
# lma_vector = seq(min(cn_species_data$lma, na.rm = T), 
#                        max(cn_species_data$lma, na.rm = T), 0.01)
# nperlma_slope = summary(emtrends(nperlma_lmer, ~1, var = 'log(lma)'))[1, 2]
# nperlma_intercept = summary(emmeans(nperlma_lmer, ~1, at = list(lma = 0)))[1, 2]
# nperlma_modeled = nperlma_slope * lma_vector + nperlma_intercept
# nperlma_model_df = data.frame(lma = lma_vector, nper = nperlma_modeled)

nperlma_plot = ggplot(data = cn_species_data, aes(x = log(lma), y = log(Nitrogen_Percent), 
                                             shape = PFT, colour = PFT)) +
  theme(legend.position = "right", 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15), 
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) +
  geom_point(alpha = 0.9, size = 4) +
  # geom_line(data = cn_model_df, aes(x = lma, y = exp(cn), 
  #                                   shape = NULL, colour = NULL), 
  #           size = 4, col = rgb(0, 0, 0, 0.7)) +
  ylab('Log Leaf N (%)') +
  xlab(expression('Log Leaf Mass per Area (' * 'g mm'^'-2' * ')')) +
  xlim(c(-3.5, -1.5)) +
  ylim(c(0, 2))



