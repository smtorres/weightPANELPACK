rm(list=ls())
library(foreign); library(car)
ANES<-read.dta("~/Documents/Revised_Weights/DataANES/2008-2010/anes2010_merged.dta")
load("~/Documents/Revised_Weights/DataANES/pop_margins_ANES.Rda")


# Keep only variables needed for weighting (DER01=SEX, der02=AGE, der04=RACE_REC, der05=EDUCATION, wgtregio=REGION, WGTBASE=bw ) 
ANES2 <- ANES[,c("caseid", "der01", "der02", "der04", "der05", "wgtregio", "wgtbase")]
rm(ANES)

#Gender
ANES2$sex <- ANES2$der01
ANES2$sex <- droplevels(ANES2$sex)
levels(ANES2$sex) <- c("Male", "Female")

#Age
table(ANES2$der02, useNA="ifany")
ANES2$age <- ifelse(ANES2$der02<=29,1,
                    ifelse(ANES2$der02<=39,2,
                           ifelse(ANES2$der02<=49,3,
                                  ifelse(ANES2$der02<=59,4,
                                         ifelse(ANES2$der02<=69,5,
                                                ifelse(ANES2$der02>=70,6,NA))))))
ANES2$age <- as.factor(ANES2$age)
levels(ANES2$age) <- levels(pop.margins.ANES[[1]][[2]][,1])

# Region
ANES2$region <- ANES2$wgtregio
ANES2$region <- droplevels(ANES2$region)
ANES2$region[as.numeric(ANES2$region)==1] <- NA
ANES2$region <- as.factor(droplevels(ANES2$region))
levels(ANES2$region) <- as.character(pop.margins.ANES[[1]][[3]][,1])

#Race
ANES2$race <- ANES2$der04
ANES2$race <- as.factor(droplevels(ANES2$race))
levels(ANES2$race) <- pop.margins.ANES[[1]][[4]][,1]


#Education
ANES2$education <- ANES2$der05
ANES2$education <- droplevels(ANES2$education)
ANES2$education[as.numeric(ANES2$education)==1] <- NA
ANES2$education[as.numeric(ANES2$education)==2] <- NA
ANES2$education <- droplevels(ANES2$education)
levels(ANES2$education) <- pop.margins.ANES[[1]][[5]][,1]


ANES2 <- ANES2[,-(2:6)]
colnames(ANES2)[2] <- "basewt" 
colnames(ANES2)[1] <- "caseid" 

#Impute region and education
library(VIM)
ANES3 <- hotdeck(ANES2, c("region", "education"), "caseid")
ANES3 <- ANES3[,-(8:9)]

save(ANES3, file="/Users/michelletorres/Dropbox/WEIGHTS_PROJECT/Revised_Weights/DataANES/ANESdemographics.Rda")
