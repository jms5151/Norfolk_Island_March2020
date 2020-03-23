# thermal stress barplots ------------------------------
rm(list = ls()) # remove everything stored in environment

# load libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# standard error function
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

# load survey data
surveys <- read.csv("Data/NI_March2020_coral_health_survey_data.csv", head=T, stringsAsFactors=F) 

# format data
surveys$Percent_partial_mortality <- as.numeric(surveys$Percent_partial_mortality)

# quantify thermal stress across transects -------------
surveys_site_prevalence <- surveys %>%
  group_by(GPS_Name) %>%
  summarize(Bleached = (sum(!is.na(Percent_bleaching))/length(GPS_Name))*100,
            Pale = (sum(!is.na(Percent_pale))/length(GPS_Name))*100,
            Fluorescent = (sum(!is.na(Percent_fluorescence))/length(GPS_Name))*100, 
            Partial_mortality = (sum(!is.na(Percent_partial_mortality))/length(GPS_Name))*100) %>% 
  gather(key="Condition", value='Prevalence', -GPS_Name)

surveys_site_prevalence_SE <- surveys %>%
  group_by(GPS_Name) %>%
  summarize(Bleached = se(!is.na(Percent_bleaching))*100,
            Pale = se(!is.na(Percent_pale))*100,
            Fluorescent = se(!is.na(Percent_fluorescence))*100,
            Partial_mortality = se(!is.na(Percent_partial_mortality))*100) %>% 
  gather(key="Condition", value='SE', -GPS_Name) %>%
  left_join(surveys_site_prevalence, by=c("GPS_Name", "Condition"))

# bar plots
ggplot(surveys_site_prevalence_SE, aes(GPS_Name, Prevalence)) + 
  facet_wrap(~Condition) +
  geom_col(fill='deepskyblue4') +
  geom_errorbar(aes(ymin = Prevalence - SE, ymax = Prevalence + SE), width=0.2) +
  ylab("Transect level prevalence") +
  xlab("") +
  theme_bw() +
  ylim(0,100) 
#1007 x 576

# quantify thermal stress within colonies -------------------
surveys_colony_prevalence <- surveys %>%
  group_by(GPS_Name) %>%
  summarize(Bleached = mean(Percent_bleaching, na.rm=T), 
            Pale = mean(Percent_pale, na.rm=T),
            Fluorescent = mean(Percent_fluorescence, na.rm=T), 
            Partial_mortality = mean(Percent_partial_mortality, na.rm=T)) %>% 
  gather(key="Condition", value='Prevalence', -GPS_Name)

surveys_colony_prevalence_SE <- surveys %>%
  group_by(GPS_Name) %>%
  summarize(Bleached = se(Percent_bleaching),
            Pale = se(Percent_pale),
            Fluorescent = se(Percent_fluorescence),
            Partial_mortality = se(Percent_partial_mortality)) %>% 
  gather(key="Condition", value='SE', -GPS_Name) %>%
  left_join(surveys_colony_prevalence, by=c("GPS_Name", "Condition"))

# bar plots
ggplot(surveys_colony_prevalence_SE, aes(GPS_Name, Prevalence)) + 
  facet_wrap(~Condition) +
  geom_col(fill='deepskyblue3') +
  geom_errorbar(aes(ymin = Prevalence - SE, ymax = Prevalence + SE), width=0.2) +
  ylab("Severity on colonies") +
  xlab("") +
  theme_bw() +
  ylim(0,100) 
#1007 x 576

# quantify thermal stress by genus ---------------------------
surveys_genus_prevalence <- surveys %>%
  group_by(Genus) %>%
  summarize(Bleached = mean(Percent_bleaching, na.rm=T), 
            Pale = mean(Percent_pale, na.rm=T),
            Fluorescent = mean(Percent_fluorescence, na.rm=T), 
            Partial_mortality = mean(Percent_partial_mortality, na.rm=T)) %>% 
  gather(key="Condition", value='Prevalence', -Genus)

surveys_genus_prevalence_SE <- surveys %>%
  group_by(Genus) %>%
  summarize(Bleached = se(Percent_bleaching),
            Pale = se(Percent_pale),
            Fluorescent = se(Percent_fluorescence),
            Partial_mortality = se(Percent_partial_mortality)) %>% 
  gather(key="Condition", value='SE', -Genus) %>%
  left_join(surveys_genus_prevalence, by=c("Genus", "Condition"))

# bar plots
ggplot(surveys_genus_prevalence_SE, aes(Genus, Prevalence)) + 
  facet_wrap(~Condition) +
  geom_col(fill='deepskyblue3') +
  geom_errorbar(aes(ymin = Prevalence - SE, ymax = Prevalence + SE), width=0.2) +
  ylab("Prevalence") +
  theme_bw() +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#1007 x 576