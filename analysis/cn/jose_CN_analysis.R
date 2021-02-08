# script for analyzing CN data from GUMO samples
# for Jose Villeda's Honors project

## load packages
library(dplyr)
# install.packages('lme4')
library(lme4)
library(car)
library(emmeans)

## load (i.e., read) the data
cn_data = read.csv('../../data/cn/jose_gumo_1_results.csv')
species_info = read.csv('../../data/cn/species_information.csv')

## look at data
head(cn_data)
head(species_info)

## merge dataasets
cn_species_data = left_join(cn_data, species_info, by = c("Sample_ID" = "Sample_ID"))
head(cn_species_data)

## calculate variables
cn_species_data$cn = cn_species_data$Carbon_Percent / cn_species_data$Nitrogen_Percent

## make some simple graphs
### histogram of the percent nitrogen data
hist(cn_species_data$Nitrogen_Percent)
hist(cn_species_data$cn)
plot(cn_species_data$Sample_Weight, cn_species_data$Carbon_Weight)

## test hypotheses about herbaceous/woody
### make new column describing whether species is woody or not
cn_species_data$woody <- NA
cn_species_data$woody[cn_species_data$Life.History == 'Forb' | cn_species_data$Life.History == 'Grass'] <- 'no'
cn_species_data$woody[cn_species_data$Life.History == 'Shrub' | cn_species_data$Life.History == 'Tree'] <- 'yes'

### test whether C/N ratios differ for wooody and non-woody species
cn_lmer <- lmer(cn ~ woody + (1|Species), data = cn_species_data)
plot(resid(cn_lmer) ~ fitted(cn_lmer))
summary(cn_lmer)
Anova(cn_lmer)
emmeans(cn_lmer, ~woody)

