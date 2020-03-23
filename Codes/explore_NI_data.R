# load libraries
library(tidyverse)
library(ggplot2)

# standard error function
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

NI <- read.csv("C:/Users/Jeremy/Box Sync/Norfolk_Island/Data/NI_March2020_coral_health_survey_data.csv", head=T, stringsAsFactors = F)

# unique.genera <- sort(unique(NI$Genus))
# 
# hist(NI$Size_bin, xlab='Size bin', main='', col='deepskyblue4')
# hist(NI$Percent_bleaching, xlab='Bleaching (%)', main='', col='deepskyblue4')
# mean(NI$Percent_bleaching, na.rm=T)
# boxplot(NI$Percent_bleaching~NI$Genus, ylab='Bleached (%)', xlab='Genus', col='deepskyblue4')
# boxplot(NI$Percent_pale~NI$Genus, ylab='Pale (%)', xlab='Genus', col='deepskyblue4')
# 
# NI$Percent_bleaching <- as.numeric(NI$Percent_bleaching)

# by bay
NI$Bay <- ifelse(substr(NI$Site,1,1)=="S", "Slaughter Bay", "Emily Bay")

bay.bleaching <- NI %>%
  group_by(Bay) %>%
  summarize(bleaching_prevalence = (sum(!is.na(Percent_bleaching))/length(Site))*100, 
            se_bleaching_prevalence = se(!is.na(Percent_bleaching))*100,
            paling_prevalence = (sum(!is.na(Percent_pale))/length(Site))*100,
            se_paling_prevalence = se(!is.na(Percent_pale))*100)

# source("C:/Users/Jeremy/Box Sync/R_functions/multiplot.R")
# multiplot(site.bleaching, site.paling, colony.bleaching, colony.paling, ncol=2)

# density -------------------------------------------
NI_density <- NI %>%
  group_by(Site) %>%
  summarize(density = length(Genus)/20)

# water quality -------------------------------------
WQ <- read.csv("water_quality_data_merged.csv", head=T, stringsAsFactors=F)

# format date
WQ$Date <- as.Date(WQ$Date, "%m/%d/%Y")

# format locations
WQ$Location[WQ$Location=="EB outlet"] <- "EB Outlet"
WQ$Location[WQ$Location=="Middle EB"] <- "EB Middle"
WQ$Location[WQ$Location=="Lone Pine"] <- "EB Pine End"
WQ$Location[WQ$Location=="Bridge"] <- "Bounty Bridge"

# make data long format for plotting
WQlong <- gather(data=WQ, key=Measurement, value, ph, Temp, Enterobacter, Enteroccus..CFU.100mL, 
                E.Coli, Coliforms, TVC, Thermo.Coliforms)

# format measurement to numeric
WQlong$Measurement <- gsub(">|<", "", WQlong$Measurement)
WQlong$Measurement <- as.numeric(WQlong$Measurement) 

# e. coli data
ggplot(data = WQ, aes(x = Date, y = TVC)) +
  geom_line(color = "#00AFBB") #+
  facet_wrap(~Location, ncol=3)

# plot
ggplot(data = WQlong, aes(x = Date, y = Measurement)) +
  geom_line(color = "#00AFBB") +
  facet_wrap(~Location, ncol=3)
