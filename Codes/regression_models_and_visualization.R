# regression models for thermal stress on colonies ---------------------------
rm(list = ls()) # remove everything stored in environment

# load libraries
library(lme4)
library(MASS)
library(dotwhisker)

# load survey data
surveys <- read.csv("Data/NI_March2020_coral_health_survey_data.csv", head=T, stringsAsFactors=F) 

# remove surveys without genera information
surveys <- subset(surveys, Genus!="") 

# add zeroes for colonies unaffected by thermal stress
surveys$Percent_bleaching[is.na(surveys$Percent_bleaching)] <- 0
surveys$Percent_pale[is.na(surveys$Percent_pale)] <- 0
surveys$Percent_fluorescence[is.na(surveys$Percent_fluorescence)] <- 0

# bleaching severity on colony 
BL_mod <- glm(Percent_bleaching ~ Genus + Morphology + Size_bin + GPS_Name + Site, data=surveys)
BL_mod_selection <- stepAIC(BL_mod, direction = 'both')
BL_mod_final <- lm(Percent_bleaching ~ Genus + GPS_Name, data=surveys)
summary(BL_mod_final)

# paling severity on colony 
PA_mod <- glm(Percent_pale ~ Genus + Morphology + Size_bin + GPS_Name + Site, data=surveys)
PA_mod_selection <- stepAIC(PA_mod, direction = 'both')
PA_mod_final <- lm(Percent_pale ~ Genus + Morphology + Size_bin + GPS_Name, data=surveys)
summary(PA_mod_final)

# fluorescing severity on colony 
FL_mod <- glm(Percent_fluorescence ~ Genus + Morphology + Size_bin + GPS_Name + Site, data=surveys)
FL_mod_selection <- stepAIC(FL_mod, direction = 'both')
FL_mod_final <- lm(Percent_fluorescence ~ Genus + Morphology + GPS_Name, data=surveys)
summary(FL_mod_final)

# save model coefficient values
PA_coeffs <- data.frame(summary(PA_mod_final)$coefficients)
colnames(PA_coeffs) <- c("Effect_size", "Standard_error", "t-value", "p-value")
write.csv(PA_coeffs, "Data/Colony_pale_model.csv")

BL_coeffs <- data.frame(summary(BL_mod_final)$coefficients)
colnames(BL_coeffs) <- c("Effect_size", "Standard_error", "t-value", "p-value")
write.csv(BL_coeffs, "Data/Colony_bleaching_model.csv")

FL_coeffs <- data.frame(summary(FL_mod_final)$coefficients)
colnames(FL_coeffs) <- c("Effect_size", "Standard_error", "t-value", "p-value")
write.csv(FL_coeffs, "Data/Colony_fluorescing_model.csv")

# save model summary stats
colony_models <- data.frame(matrix(ncol=3, nrow=0))
colnames(colony_models) <- c("Adj_R2", "df", "F-statistic")

colony_models[1,] <- c(round(summary(BL_mod_final)$adj.r.squared,4), summary(BL_mod_final)$df[1], unname(round(summary(BL_mod_final)$fstatistic[1],4)))
colony_models[2,] <- c(round(summary(PA_mod_final)$adj.r.squared,4), summary(PA_mod_final)$df[1], unname(round(summary(PA_mod_final)$fstatistic[1],4)))
colony_models[3,] <- c(round(summary(FL_mod_final)$adj.r.squared,4), summary(FL_mod_final)$df[1], unname(round(summary(FL_mod_final)$fstatistic[1],4)))

write.csv(colony_models, "Data/Colony_models_summary_stats.csv")

# plot model coeffients results
dwplot(BL_mod_final,  whisker_args = list(color = "darkblue")
       , dot_args = list(size = 2, colour=I('darkblue'))
       , vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + theme_bw() +
  theme(legend.position="none") + xlab("Coefficient Estimate") +
  ggtitle("Colony level bleaching")
  # if we want to remove factor naming
  # + scale_y_discrete(labels=gsub("Genus|GPS_Name", "", names(BL_mod_final$coefficients)[2:length(BL_mod_final$coefficients)]))


dwplot(PA_mod_final,  whisker_args = list(color = "darkblue")
       , dot_args = list(size = 2, colour=I('darkblue'))
       , vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + theme_bw() +
  theme(legend.position="none") + xlab("Coefficient Estimate") +
  ggtitle("Colony level paling") 

dwplot(FL_mod_final,  whisker_args = list(color = "darkblue")
       , dot_args = list(size = 2, colour=I('darkblue'))
       , vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + theme_bw() +
  theme(legend.position="none") + xlab("Coefficient Estimate") +
  ggtitle("Colony level fluorescence")
