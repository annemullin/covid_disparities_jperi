# ---

# Variable names need to be in numeric format for models. 0=no, 1=yes
# EPOCH
blrc$epoch_mod <- ifelse(blrc$epoch == "covid", 1, 0)

# PTB
blrc$ptb_mod <- ifelse(blrc$ptb_deriv == "yes", 1, 0)
blrc$ptb_mod <- ifelse((is.na(blrc$ptb_mod)), 0, blrc$ptb_mod)

# sPTB
blrc$sptb_mod <- ifelse(blrc$rc.sptb == "yes", 1, 0)
blrc$sptb_mod <- ifelse((is.na(blrc$rc.sptb)), 0, blrc$sptb_mod)

# mPTB
blrc$mptb_mod <- ifelse(blrc$rc.sptb == "no", 1, 0)
blrc$mptb_mod <- ifelse((is.na(blrc$rc.sptb)), 0, blrc$mptb_mod)

# IUFD
blrc$iufd_mod <- ifelse(blrc$IUFD == "yes", 1, 0)

table(blrc$epoch_mod)
table(blrc$ptb_mod)
table(blrc$sptb_mod)
table(blrc$mptb_mod)
table(blrc$iufd_mod)

table(is.na(blrc$epoch_mod))
table(is.na(blrc$ptb_mod))
table(is.na(blrc$sptb_mod))
table(is.na(blrc$mptb_mod))
table(is.na(blrc$iufd_mod))

# ---

# Create exposure variables
# any exposure to covid during the first trimester [and second/third]  

blrc$covid_tri_1 <- ifelse((blrc$LMP_calc >= "2019-10-22" &
                              blrc$LMP_calc < "2019-12-04"), 0, 1)
blrc$covid_tri_1 <- ifelse((blrc$LMP_calc >= "2018-10-22" &
                              blrc$LMP_calc < "2018-12-04"), 0, blrc$covid_tri_1)
blrc$covid_tri_1 <- ifelse((blrc$LMP_calc >= "2017-10-22" &
                              blrc$LMP_calc < "2017-12-04"), 0, blrc$covid_tri_1)
table(blrc$covid_tri_1)

# Separate data frames
covid_tri_123 <- blrc[(blrc$covid_tri_1 == 1),]
covid_tri_23 <- blrc[(blrc$covid_tri_1 == 0),]

# Look at data
table1(~ ptb_deriv + rc.sptb + IUFD| epoch, data = blrc)
table1(~ ptb_deriv + rc.sptb + IUFD| epoch, data = covid_tri_123)
table1(~ ptb_deriv + rc.sptb + IUFD| epoch, data = covid_tri_23)

# ---

# Organize covariates
## BMI
summary(blrc$BMI)
class(blrc$BMI)

blrc$BMILT25 <- ifelse(blrc$BMIcat == "1", 1, 0)
blrc$BMI25LT30 <- ifelse(blrc$BMIcat == "2", 1, 0)
blrc$BMIGTE30 <- ifelse(blrc$BMIcat == "3", 1, 0)
blrc$BMIMissing <- ifelse(blrc$BMIcat == "99", 1, 0)

## MEDICAID
table(blrc$medicaid)
blrc$medicaid_mod <- as.numeric(blrc$medicaid)
table(blrc$medicaid_mod)

## SMOKING
table(blrc$smoker3)
blrc$Smoker <- ifelse(blrc$smoker3 == "Smoker", 1, 0)
blrc$Smoker <- ifelse(is.na(blrc$smoker3), 0, blrc$Smoker)
blrc$NonSmoker <- ifelse((blrc$smoker3 == "Non-Smoker" |
                            blrc$smoker3 == "Former Smoker"), 1, 0)
blrc$NonSmoker <- ifelse(is.na(blrc$smoker3), 0, blrc$NonSmoker)
blrc$SmokerMissing <- ifelse(is.na(blrc$smoker3), 1, 0)

## MARITAL STAT
table(blrc$bl.marital_stat)
blrc$marital_stat_mod <- ifelse(blrc$bl.marital_stat == "Married [2]", 1, 0)

## PARITY
table(blrc$nullip)
blrc$nullip_mod <- as.numeric(blrc$nullip)

## MATERNAL AGE
blrc$matage_cat <- ifelse(blrc$matage < 25, "1", NA)
blrc$matage_cat <- ifelse(blrc$matage >= 25 & blrc$matage < 35, "2", blrc$matage_cat)
blrc$matage_cat <- ifelse(blrc$matage >= 35, "3", blrc$matage_cat)
blrc$matage_cat <- ifelse(is.na(blrc$matage), "99", blrc$matage_cat)

blrc$AgeLT25 <- ifelse(blrc$matage_cat == "1", 1, 0)
blrc$Age25LT35 <- ifelse(blrc$matage_cat == "2", 1, 0)
blrc$AgeGTE35 <- ifelse(blrc$matage_cat == "3", 1, 0)
blrc$AgeMissing <- ifelse((is.na(blrc$matage_cat)), 1, 0)

## RACE/ETHNICITY
table(blrc$race2)
blrc$RaceHispanic <- ifelse(blrc$race2 == "Hispanic", 1, 0)
blrc$RaceNonHispanicW <- ifelse(blrc$race2 == "Non-Hispanic White", 1, 0)
blrc$RaceNonHispanicB <- ifelse(blrc$race2 == "Non-Hispanic Black", 1, 0)
blrc$RaceOther <- ifelse(blrc$race2 == "Other/unknown", 1, 0)
blrc$RaceMissing <- ifelse((is.na(blrc$race2)), 1, 0)


# GA cats
# 1= extreme, 2= very, 3= moderate, 4= late
blrc$preterm_cat <- ifelse(blrc$GA < 28, "extreme", NA)
blrc$preterm_cat <- ifelse((blrc$GA >= 28 &
                              blrc$GA < 32), "very", blrc$preterm_cat)
blrc$preterm_cat <- ifelse((blrc$GA >= 32 &
                              blrc$GA < 34), "moderate", blrc$preterm_cat)
blrc$preterm_cat <- ifelse((blrc$GA >= 34 &
                              blrc$GA < 37), "late", blrc$preterm_cat)
blrc$preterm_cat <- ifelse((blrc$GA >= 37), "term", blrc$preterm_cat)
blrc$preterm_cat <- ifelse((blrc$IUFD == "yes"), NA, blrc$preterm_cat)

table(blrc$preterm_cat)


# extreme
blrc$extreme_mod <- ifelse(blrc$preterm_cat == "extreme", 1, 0)
blrc$extreme_mod <- ifelse((is.na(blrc$preterm_cat)), 0, blrc$extreme_mod)

# very
blrc$very_mod <- ifelse(blrc$preterm_cat == "very", 1, 0)
blrc$very_mod <- ifelse((is.na(blrc$preterm_cat)), 0, blrc$very_mod)

# moderate
blrc$moderate_mod <- ifelse(blrc$preterm_cat == "moderate", 1, 0)
blrc$moderate_mod <- ifelse((is.na(blrc$preterm_cat)), 0, blrc$moderate_mod)

# late
blrc$late_mod <- ifelse(blrc$preterm_cat == "late", 1, 0)
blrc$late_mod <- ifelse((is.na(blrc$preterm_cat)), 0, blrc$late_mod)

# ---

# Table 1
blrc$medicaid_fac <- as.factor(blrc$medicaid)
blrc$BMI_fac <- as.factor(blrc$BMIcat)
blrc$marital_stat_fac <- as.factor(blrc$marital_stat_mod)
blrc$nullip_fac <- as.factor(blrc$nullip)
blrc$smoker_fac <- ifelse(blrc$Smoker == 1, "1", "0")
blrc$smoker_fac <- ifelse(blrc$SmokerMissing == 1, "missing", blrc$smoker_fac)

blrc$BWrace <- blrc$race2
blrc$BWrace <- ifelse(blrc$race2 == "Hispanic", "Other/unknown", blrc$BWrace)

blrc$tab1col <- ifelse(blrc$epoch == "precovid", "precovid", NA)
blrc$tab1col <- ifelse((is.na(blrc$tab1col) & 
                          blrc$covid_tri_1 == 1), "covid 0- <14 wks", blrc$tab1col)
blrc$tab1col <- ifelse((is.na(blrc$tab1col) & 
                          blrc$covid_tri_1 == 0), "covid 14- 20wks", blrc$tab1col)


table1(~ matage + BMI_fac + nullip_fac + BWrace + smoker_fac + medicaid_fac + marital_stat_fac | tab1col, data = blrc)
table1(~ matage + BMI_fac + nullip_fac + BWrace + smoker_fac + medicaid_fac + marital_stat_fac | epoch, data = blrc)

# check rounding
x <- covid_tri_23[covid_tri_23$year == "2020",]
sd(x$matage)

# p values

table(blrc$covid_tri_1)
#1= < 14 weeks, 0= 14-20 weeks
# ttest for age
t.test(blrc$matage~blrc$epoch)
lapply(split(blrc, blrc$covid_tri_1), function(x) {t.test(x$matage~x$epoch)$p.value})

# Check function
t.test(covid_tri_123$matage~covid_tri_123$epoch)
t.test(covid_tri_23$matage~covid_tri_23$epoch)


# Chi-square
chisq.test(blrc$epoch, blrc$BMI_fac)
chisq.test(blrc$epoch, blrc$medicaid)
chisq.test(blrc$epoch, blrc$race2)
chisq.test(blrc$epoch, blrc$nullip)
chisq.test(blrc$epoch, blrc$marital_stat_mod)
chisq.test(blrc$epoch, blrc$smoker3)

lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$BMI_fac, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$medicaid, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$race2, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$nullip, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$marital_stat_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$smoker3, x$epoch)$p.value})


# ----

# Checking rates of c-sections for reviewers
table(blrc$bl.method_of_delivery)

blrc$csection <- ifelse(blrc$bl.method_of_delivery == "C-Section, Classical" |
                          blrc$bl.method_of_delivery == "C-Section, Low Transverse" |
                          blrc$bl.method_of_delivery == "C-Section, Low Vertical" |
                          blrc$bl.method_of_delivery == "C-Section, Unspecified", "c-section", "vaginal")

table1(~ preterm_cat + ptb_deriv + rc.sptb |  epoch+csection , data = blrc)

# row %s

covid <- blrc[blrc$epoch == "covid",]
precovid <- blrc[blrc$epoch == "precovid",]

#precovid
tabdatprecovid <- precovid %>%
  mutate(PTB_cat_NY = factor(preterm_cat,
                             levels = c("late", "moderate", "very", "extreme", "term"),
                             labels = c("late", "moderate", "very", "extreme", "term")),
         PTB_NY = factor(ptb_deriv,
                         levels = c("no","yes"),
                         labels = c("Not PTB","PTB")),
         sPTB_NY = factor(rc.sptb,
                          levels = c("no","yes"),
                          labels = c("mPTB","sPTB")))


getTable1Stats <- function(x, digits = 1,...){
  getDescriptionStatsBy(x = x, 
                        by = tabdatprecovid$csection,
                        digits = digits,
                        header_count = TRUE,
                        ...)
}

t1 <- list()
t1[["PTB"]] <- getTable1Stats(tabdatprecovid$PTB_NY, hrzl_prop = TRUE, add_total_col = "last")
t1[["PTBcat"]] <- getTable1Stats(tabdatprecovid$PTB_cat_NY, hrzl_prop = TRUE, add_total_col = "last")
t1[["sPTB"]] <- getTable1Stats(tabdatprecovid$sPTB_NY, hrzl_prop = TRUE, add_total_col = "last")

tab1 <- mergeDesc(t1)
tab1


#covid
tabdatcovid <- covid %>%
  mutate(PTB_cat_NY = factor(preterm_cat,
                             levels = c("late", "moderate", "very", "extreme", "term"),
                             labels = c("late", "moderate", "very", "extreme", "term")),
         PTB_NY = factor(ptb_deriv,
                         levels = c("no","yes"),
                         labels = c("Not PTB","PTB")),
         sPTB_NY = factor(rc.sptb,
                          levels = c("no","yes"),
                          labels = c("mPTB","sPTB")))


getTable1Stats <- function(x, digits = 1,...){
  getDescriptionStatsBy(x = x, 
                        by = tabdatcovid$csection,
                        digits = digits,
                        header_count = TRUE,
                        ...)
}

t1 <- list()
t1[["PTB"]] <- getTable1Stats(tabdatcovid$PTB_NY, hrzl_prop = TRUE, add_total_col = "last")
t1[["PTBcat"]] <- getTable1Stats(tabdatcovid$PTB_cat_NY, hrzl_prop = TRUE, add_total_col = "last")
t1[["sPTB"]] <- getTable1Stats(tabdatcovid$sPTB_NY, hrzl_prop = TRUE, add_total_col = "last")

tab1 <- mergeDesc(t1)
tab1

# ----

# Table 2

table1(~ preterm_cat + ptb_deriv + rc.sptb + IUFD| tab1col, data = blrc)
table1(~ preterm_cat + ptb_deriv + rc.sptb + IUFD| epoch, data = blrc)


chisq.test(blrc$ptb_mod, blrc$epoch_mod)
chisq.test(blrc$extreme_mod, blrc$epoch_mod)
chisq.test(blrc$very_mod, blrc$epoch_mod)
chisq.test(blrc$moderate_mod, blrc$epoch_mod)
chisq.test(blrc$late_mod, blrc$epoch_mod)
chisq.test(blrc$sptb_mod, blrc$epoch_mod)
chisq.test(blrc$mptb_mod, blrc$epoch_mod)
chisq.test(blrc$iufd_mod, blrc$epoch_mod)

lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$ptb_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$extreme_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$very_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$moderate_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$late_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$sptb_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$mptb_mod, x$epoch)$p.value})
lapply(split(blrc, blrc$covid_tri_1), function(x) {chisq.test(x$iufd_mod, x$epoch)$p.value})


# Table 3
table(blrc$race2)
blrc$race3 <- ifelse(blrc$race2 == "Non-Hispanic White", "Non-Hispanic White", NA)
blrc$race3 <- ifelse(blrc$race2 == "Non-Hispanic Black", "Non-Hispanic Black", blrc$race3)
blrc$race3 <- ifelse((blrc$race2 == "Hispanic" |
                        blrc$race2 == "Other/unknown"), "Other/Hispanic/unknown", blrc$race3)
table(blrc$race3)

table(blrc$preterm_cat)
blrc$preterm_cat2 <- ifelse((blrc$preterm_cat == "very" |
                               blrc$preterm_cat == "extreme"), "extreme/very", blrc$preterm_cat)
table(blrc$preterm_cat2)

blrc$extr_very_mod <- ifelse(blrc$preterm_cat2 == "extreme/very", 1, 0)


table1(~ preterm_cat + ptb_deriv + rc.sptb + IUFD| race3 + epoch, data = blrc)
table1(~ preterm_cat2 + ptb_deriv + rc.sptb + IUFD| race3 + epoch, data = blrc)

lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$extreme_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$very_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$extr_very_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$moderate_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$late_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$ptb_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$sptb_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$mptb_mod, x$epoch)$p.value })
lapply(split(blrc, blrc$race3), function(x) { chisq.test(x$iufd_mod, x$epoch)$p.value })




### Combined Final models


# PTB

# m0 everybody
m0a_ptb <- glm(ptb_mod ~ epoch_mod, data = subset(blrc, iufd_mod == 0), family = binomial(log))
summary(m0a_ptb)
exp(m0a_ptb[[1]][2])
exp(confint(m0a_ptb))

# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_ptb <- glm(ptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, iufd_mod == 0), family = binomial(log))
summary(m1a_ptb) 
exp(m1a_ptb[[1]][2])
exp(confint(m1a_ptb))


# ----

### sPTB and mPTB in binomial models

# sPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_sptb <- glm(sptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m1a_sptb) 
exp(m1a_sptb[[1]][2])
exp(confint(m1a_sptb))

# mPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_mptb <- glm(mptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m1a_mptb) 
exp(m1a_mptb[[1]][2])
exp(confint(m1a_mptb))

# ----

### extreme, very, moderate, late in binomial models

# Check independent variables
table(blrc$extreme_mod)
table(blrc$very_mod)
table(blrc$moderate_mod)
table(blrc$late_mod)


# Extreme
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_extreme <- glm(extreme_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & very_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m1a_extreme) 
exp(m1a_extreme[[1]][2])
exp(confint(m1a_extreme))

# Very
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_very <- glm(very_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m1a_very) 
exp(m1a_very[[1]][2])
exp(confint(m1a_very))

# Moderate
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_moderate <- glm(moderate_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m1a_moderate) 
exp(m1a_moderate[[1]][2])
exp(confint(m1a_moderate))

# Late
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_late <- glm(late_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & moderate_mod == 0)), family = binomial(log))
summary(m1a_late) 
exp(m1a_late[[1]][2])
exp(confint(m1a_late))

### IUFD

# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_iufd <- glm(iufd_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = blrc, family = binomial(log))
summary(m1a_iufd) 
exp(m1a_iufd[[1]][2])
exp(confint(m1a_iufd))


# ----

## Duration of exposure

table1(~ ptb_deriv + rc.sptb + IUFD| epoch, data = subset(blrc, covid_tri_1 == 1))
table1(~ ptb_deriv + rc.sptb + IUFD| epoch, data = subset(blrc, covid_tri_1 == 0))

### Starting <14 weeks GA


#### PTB
# m0 everybody
m0a_ptb_tri1 <- glm(ptb_mod ~ epoch_mod, data = subset(blrc, iufd_mod == 0 & covid_tri_1 == 1), family = binomial(log))
summary(m0a_ptb_tri1)
exp(m0a_ptb_tri1[[1]][2])
exp(confint(m0a_ptb_tri1))

# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_ptb_tri1 <- glm(ptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, iufd_mod == 0 & covid_tri_1 == 1), family = binomial(log))
summary(m1a_ptb_tri1) 
exp(m1a_ptb_tri1[[1]][2])
exp(confint(m1a_ptb_tri1))

### sPTB and mPTB in binomial models

# sPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_sptb_tri1 <- glm(sptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & mptb_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_sptb_tri1) 
exp(m1a_sptb_tri1[[1]][2])
exp(confint(m1a_sptb_tri1))

# mPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_mptb_tri1 <- glm(mptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & sptb_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_mptb_tri1) 
exp(m1a_mptb_tri1[[1]][2])
exp(confint(m1a_mptb_tri1))

### Extreme, very, moderate, late in binomial models

# Extreme
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_extreme_tri1 <- glm(extreme_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & very_mod == 0 & moderate_mod == 0 & late_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_extreme_tri1) 
exp(m1a_extreme_tri1[[1]][2])
exp(confint(m1a_extreme_tri1))

# Very
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_very_tri1 <- glm(very_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & moderate_mod == 0 & late_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_very_tri1) 
exp(m1a_very_tri1[[1]][2])
exp(confint(m1a_very_tri1))

# Moderate
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_moderate_tri1 <- glm(moderate_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & late_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_moderate_tri1) 
exp(m1a_moderate_tri1[[1]][2])
exp(confint(m1a_moderate_tri1))


# Late
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_late_tri1 <- glm(late_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & moderate_mod == 0 & covid_tri_1 == 1)), family = binomial(log))
summary(m1a_late_tri1) 
exp(m1a_late_tri1[[1]][2])
exp(confint(m1a_late_tri1))


### IUFD

# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)

m1a_iufd_tri1 <- glm(iufd_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, covid_tri_1 == 1), family = binomial(log))
summary(m1a_iufd_tri1)
exp(m1a_iufd_tri1[[1]][2])
# exp(confint(m1a_iufd_tri1))
exp(-0.14139 - (1.96*0.35476))
exp(-0.14139 + (1.96*0.35476))


### Starting 14- <20 weeks GA

table(blrc$covid_tri_1)

#### PTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_ptb_tri23 <- glm(ptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, iufd_mod == 0 & covid_tri_1 == 0), family = binomial(log))
summary(m1a_ptb_tri23)
exp(m1a_ptb_tri23[[1]][2])
exp(confint(m1a_ptb_tri23))


### sPTB and mPTB in binomial models

# sPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_sptb_tri23 <- glm(sptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & mptb_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_sptb_tri23) 
exp(m1a_sptb_tri23[[1]][2])
exp(confint(m1a_sptb_tri23))

# mPTB
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_mptb_tri23 <- glm(mptb_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & sptb_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_mptb_tri23) 
exp(m1a_mptb_tri23[[1]][2])
exp(confint(m1a_mptb_tri23))


### Extreme, very, moderate, late in binomial models

# Extreme
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_extreme_tri23 <- glm(extreme_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & very_mod == 0 & moderate_mod == 0 & late_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_extreme_tri23) 
exp(m1a_extreme_tri23[[1]][2])
exp(confint(m1a_extreme_tri23))

# Very
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_very_tri23 <- glm(very_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & moderate_mod == 0 & late_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_very_tri23) 
exp(m1a_very_tri23[[1]][2])
exp(confint(m1a_very_tri23))

# Moderate
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_moderate_tri23 <- glm(moderate_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & late_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_moderate_tri23) 
exp(m1a_moderate_tri23[[1]][2])
# exp(confint(m1a_moderate_tri23))
exp(-0.42130 - (1.96*0.43120))
exp(-0.42130 + (1.96*0.43120))

# Late
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_late_tri23 <- glm(late_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, (iufd_mod == 0 & extreme_mod == 0 & very_mod == 0 & moderate_mod == 0 & covid_tri_1 == 0)), family = binomial(log))
summary(m1a_late_tri23) 
exp(m1a_late_tri23[[1]][2])
exp(confint(m1a_late_tri23))


### IUFD
# m1a, everyone with a missing indicator (age, bmi, smoking, parity, insurance)
m1a_iufd_tri23 <- glm(iufd_mod ~ epoch_mod + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(blrc, covid_tri_1 == 0), family = poisson(log))
summary(m1a_iufd_tri23)
exp(m1a_iufd_tri23[[1]][2])
exp(confint(m1a_iufd_tri23))


# RRR
RRR(m1a_ptb_tri23, m1a_ptb_tri1)

RRR(m1a_sptb_tri23, m1a_sptb_tri1)
RRR(m1a_mptb_tri23, m1a_mptb_tri1)

RRR(m1a_extreme_tri23, m1a_extreme_tri1)
RRR(m1a_very_tri23, m1a_very_tri1)
RRR(m1a_moderate_tri23, m1a_moderate_tri1)
RRR(m1a_late_tri23, m1a_late_tri1)

RRR(m1a_iufd_tri23, m1a_iufd_tri1)

# ----

## Black-White disparity    
#### All
# restrict to pandemic, outcome = preterm birth, exposure = black race


table(blrc$race2)
BW <- blrc[(blrc$race2 == "Non-Hispanic Black" |
              blrc$race2 == "Non-Hispanic White"),]
BW$BW_race <- ifelse(BW$race2 == "Non-Hispanic Black", 1, 0)

table(BW$BW_race)
table(BW$ptb_mod)
table(BW$sptb_mod)
table(BW$mptb_mod)
table(BW$iufd_mod)

table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = BW)
table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = subset(BW, covid_tri_1 == 1))
table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = subset(BW, covid_tri_1 == 0))


### Pre-pandemic   

#### PTB 
m_disp_pre_ptb_0a <- glm(ptb_mod ~ BW_race, data = subset(BW, epoch == "precovid" & iufd_mod == 0), family = binomial(log))
summary(m_disp_pre_ptb_0a)
exp(m_disp_pre_ptb_0a[[1]][2])
exp(confint(m_disp_pre_ptb_0a))


m_disp_pre_ptb_1a <- glm(ptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, epoch == "precovid" & iufd_mod == 0), family = binomial(log))
summary(m_disp_pre_ptb_1a) 
exp(m_disp_pre_ptb_1a[[1]][2])
exp(confint(m_disp_pre_ptb_1a))


#### sPTB 
m_disp_pre_sptb_0a <- glm(sptb_mod ~ BW_race, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m_disp_pre_sptb_0a)
exp(m_disp_pre_sptb_0a[[1]][2])
exp(confint(m_disp_pre_sptb_0a))

m_disp_pre_sptb_1a <- glm(sptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m_disp_pre_sptb_1a)
exp(m_disp_pre_sptb_1a[[1]][2])
exp(confint(m_disp_pre_sptb_1a))

#### mptb 
m_disp_pre_mptb_0a <- glm(mptb_mod ~ BW_race, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m_disp_pre_mptb_0a)
exp(m_disp_pre_mptb_0a[[1]][2])
exp(confint(m_disp_pre_mptb_0a))

m_disp_pre_mptb_1a <- glm(mptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m_disp_pre_mptb_1a)
exp(m_disp_pre_mptb_1a[[1]][2])
exp(confint(m_disp_pre_mptb_1a))



#### late 
m_disp_pre_late_1a <- glm(late_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & extr_very_mod == 0 & moderate_mod == 0)), family = binomial(log))
summary(m_disp_pre_late_1a) 
exp(m_disp_pre_late_1a[[1]][2])
exp(confint(m_disp_pre_late_1a))


#### moderate 
m_disp_pre_moderate_1a <- glm(moderate_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & extr_very_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_pre_moderate_1a) 
exp(m_disp_pre_moderate_1a[[1]][2])
exp(confint(m_disp_pre_moderate_1a))


#### very/ extreme 
table(BW$extr_very_mod)
table(BW$BW_race)

m_disp_pre_extr_very_1a <- glm(extr_very_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "precovid" & iufd_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_pre_extr_very_1a) 
exp(m_disp_pre_extr_very_1a[[1]][2])
exp(confint(m_disp_pre_extr_very_1a))


#### IUFD
m_disp_pre_iufd_0a <- glm(iufd_mod ~ BW_race, data = subset(BW, epoch == "precovid"), family = binomial(log))
summary(m_disp_pre_iufd_0a)    
exp(m_disp_pre_iufd_0a[[1]][2])
exp(confint(m_disp_pre_iufd_0a))

m_disp_pre_iufd_1a <- glm(iufd_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, epoch == "precovid"), family = binomial(log))
summary(m_disp_pre_iufd_1a)   
exp(m_disp_pre_iufd_1a[[1]][2])
exp(confint(m_disp_pre_iufd_1a))


### Pandemic   

m_disp_covid_ptb_1a <- glm(ptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, epoch == "covid" & iufd_mod == 0), family = binomial(log))
summary(m_disp_covid_ptb_1a) 
exp(m_disp_covid_ptb_1a[[1]][2])
exp(confint(m_disp_covid_ptb_1a))


#### sPTB 
m_disp_covid_sptb_1a <- glm(sptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "covid" & iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m_disp_covid_sptb_1a)
exp(m_disp_covid_sptb_1a[[1]][2])
exp(confint(m_disp_covid_sptb_1a))


m_disp_covid_mptb_1a <- glm(mptb_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "covid" & iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m_disp_covid_mptb_1a)
exp(m_disp_covid_mptb_1a[[1]][2])
exp(confint(m_disp_covid_mptb_1a))



#### late 
m_disp_covid_late_1a <- glm(late_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "covid" & iufd_mod == 0 & extr_very_mod == 0 & moderate_mod == 0)), family = binomial(log))
summary(m_disp_covid_late_1a) 
exp(m_disp_covid_late_1a[[1]][2])
exp(confint(m_disp_covid_late_1a))


#### moderate 
m_disp_covid_moderate_1a <- glm(moderate_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "covid" & iufd_mod == 0 & extr_very_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_covid_moderate_1a) 
exp(m_disp_covid_moderate_1a[[1]][2])
# exp(confint(m_disp_covid_moderate_1a))
exp(0.16553 - (1.96*0.58108))
exp(0.16553 + (1.96*0.58108))

#### very/ extreme 
m_disp_covid_extr_very_1a <- glm(extr_very_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, (epoch == "covid" & iufd_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_covid_extr_very_1a) 
exp(m_disp_covid_extr_very_1a[[1]][2])
exp(confint(m_disp_covid_extr_very_1a))


#### IUFD
m_disp_covid_iufd_1a <- glm(iufd_mod ~ BW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(BW, epoch == "covid"), family = binomial(log))
summary(m_disp_covid_iufd_1a)   
exp(m_disp_covid_iufd_1a[[1]][2])
# exp(confint(m_disp_covid_iufd_1a))
exp(0.44732 - (1.96*0.92117))
exp(0.44732 + (1.96*0.92117))


### RRR   
# (Scroll down to function and run first)


RRR(m_disp_covid_ptb_1a, m_disp_pre_ptb_1a)
RRR(m_disp_covid_sptb_1a, m_disp_pre_sptb_1a)
RRR(m_disp_covid_mptb_1a, m_disp_pre_mptb_1a)
RRR(m_disp_covid_late_1a, m_disp_pre_late_1a)
RRR(m_disp_covid_moderate_1a, m_disp_pre_moderate_1a)
RRR(m_disp_covid_extr_very_1a, m_disp_pre_extr_very_1a)
RRR(m_disp_covid_iufd_1a, m_disp_pre_iufd_1a)



# ----

## Hispanic/other-White disparity    
#### All
# restrict to pandemic, outcome = preterm birth, exposure = hispanic/other race


table(blrc$race3)
OthW <- blrc[(blrc$race3 == "Other/Hispanic/unknown" |
                blrc$race3 == "Non-Hispanic White"),]
OthW$OthW_race <- ifelse(OthW$race3 == "Other/Hispanic/unknown", 1, 0)

table(OthW$OthW_race)
table(OthW$ptb_mod)
table(OthW$sptb_mod)
table(OthW$mptb_mod)
table(OthW$iufd_mod)

table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = OthW)
table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = subset(OthW, covid_tri_1 == 1))
table1(~ rc.sptb + ptb_deriv + IUFD| epoch, data = subset(OthW, covid_tri_1 == 0))



### Pre-pandemic   

#### PTB 
m_disp_HW_pre_ptb_1a <- glm(ptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, epoch == "precovid" & iufd_mod == 0), family = binomial(log))
summary(m_disp_HW_pre_ptb_1a) 
exp(m_disp_HW_pre_ptb_1a[[1]][2])
exp(confint(m_disp_HW_pre_ptb_1a))


#### sPTB 

m_disp_HW_pre_sptb_1a <- glm(sptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "precovid" & iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m_disp_HW_pre_sptb_1a)
exp(m_disp_HW_pre_sptb_1a[[1]][2])
exp(confint(m_disp_HW_pre_sptb_1a))

#### mptb 

m_disp_HW_pre_mptb_1a <- glm(mptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "precovid" & iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m_disp_HW_pre_mptb_1a)
exp(m_disp_HW_pre_mptb_1a[[1]][2])
exp(confint(m_disp_HW_pre_mptb_1a))

#### late 
m_disp_HW_pre_late_1a <- glm(late_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "precovid" & iufd_mod == 0 & extr_very_mod == 0 & moderate_mod == 0)), family = binomial(log))
summary(m_disp_HW_pre_late_1a) 
exp(m_disp_HW_pre_late_1a[[1]][2])
exp(confint(m_disp_HW_pre_late_1a))

#### moderate 
m_disp_HW_pre_moderate_1a <- glm(moderate_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "precovid" & iufd_mod == 0 & extr_very_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_HW_pre_moderate_1a) 
exp(m_disp_HW_pre_moderate_1a[[1]][2])
exp(confint(m_disp_HW_pre_moderate_1a))

#### very/ extreme 
m_disp_HW_pre_extr_very_1a <- glm(extr_very_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "precovid" & iufd_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_HW_pre_extr_very_1a) 
exp(m_disp_HW_pre_extr_very_1a[[1]][2])
exp(confint(m_disp_HW_pre_extr_very_1a))


#### IUFD

m_disp_HW_pre_iufd_1a <- glm(iufd_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, epoch == "precovid"), family = binomial(log))
summary(m_disp_HW_pre_iufd_1a)   
exp(m_disp_HW_pre_iufd_1a[[1]][2])
# exp(confint(m_disp_HW_pre_iufd_1a))
exp(-1.32799  - (1.96*0.94269))
exp(-1.32799  + (1.96*0.94269))


### Pandemic   

#### PTB 

m_disp_HW_covid_ptb_1a <- glm(ptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, epoch == "covid" & iufd_mod == 0), family = binomial(log))
summary(m_disp_HW_covid_ptb_1a) 
exp(m_disp_HW_covid_ptb_1a[[1]][2])
exp(confint(m_disp_HW_covid_ptb_1a))


#### sPTB 

m_disp_HW_covid_sptb_1a <- glm(sptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "covid" & iufd_mod == 0 & mptb_mod == 0)), family = binomial(log))
summary(m_disp_HW_covid_sptb_1a)
exp(m_disp_HW_covid_sptb_1a[[1]][2])
exp(confint(m_disp_HW_covid_sptb_1a))

#### mptb 
m_disp_HW_covid_mptb_1a <- glm(mptb_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "covid" & iufd_mod == 0 & sptb_mod == 0)), family = binomial(log))
summary(m_disp_HW_covid_mptb_1a)
exp(m_disp_HW_covid_mptb_1a[[1]][2])
exp(confint(m_disp_HW_covid_mptb_1a))


#### late 
m_disp_HW_covid_late_1a <- glm(late_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "covid" & iufd_mod == 0 & extr_very_mod == 0 & moderate_mod == 0)), family = binomial(log))
summary(m_disp_HW_covid_late_1a) 
exp(m_disp_HW_covid_late_1a[[1]][2])
exp(confint(m_disp_HW_covid_late_1a))


#### moderate 
m_disp_HW_covid_moderate_1a <- glm(moderate_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "covid" & iufd_mod == 0 & extr_very_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_HW_covid_moderate_1a) 
exp(m_disp_HW_covid_moderate_1a[[1]][2])
# exp(confint(m_disp_HW_covid_moderate_1a))
exp(0.64204 - (1.96*0.92117))
exp(0.64204 + (1.96*0.92117))


#### very/ extreme 
m_disp_HW_covid_extr_very_1a <- glm(extr_very_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, (epoch == "covid" & iufd_mod == 0 & moderate_mod == 0 & late_mod == 0)), family = binomial(log))
summary(m_disp_HW_covid_extr_very_1a) 
exp(m_disp_HW_covid_extr_very_1a[[1]][2])
exp(confint(m_disp_HW_covid_extr_very_1a))


#### IUFD

m_disp_HW_covid_iufd_1a <- glm(iufd_mod ~ OthW_race + matage + BMILT25 + BMIGTE30 + BMIMissing + Smoker + SmokerMissing + nullip_mod + medicaid_mod, data = subset(OthW, epoch == "covid"), family = binomial(log))
summary(m_disp_HW_covid_iufd_1a)   
exp(m_disp_HW_covid_iufd_1a[[1]][2])
# exp(confint(m_disp_HW_covid_iufd_1a))
exp(0.90384  - (1.96*0.83379))
exp(0.90384  + (1.96*0.83379))

### RRR   
# (Scroll down to function and run first)


RRR(m_disp_HW_covid_ptb_1a, m_disp_HW_pre_ptb_1a)
RRR(m_disp_HW_covid_sptb_1a, m_disp_HW_pre_sptb_1a)
RRR(m_disp_HW_covid_mptb_1a, m_disp_HW_pre_mptb_1a)
RRR(m_disp_HW_covid_late_1a, m_disp_HW_pre_late_1a)
RRR(m_disp_HW_covid_moderate_1a, m_disp_HW_pre_moderate_1a)
RRR(m_disp_HW_covid_extr_very_1a, m_disp_HW_pre_extr_very_1a)
RRR(m_disp_HW_covid_iufd_1a, m_disp_HW_pre_iufd_1a)



# Manually entering RR and CI into RRR

#1 Relative risks of each model, exponentiated
RR1 <- exp(1.56)
RR2 <- exp(1.86)

#2 log of relative risks
E1 <- log(RR1)
E2 <- log(RR2)

#3&4 95% CIs
L1 <- 0.25                                #lower 95%CI
U1 <- 9.51                                #upper 95%CI
logL1 <- log(L1)                          #log of lower 95%CI
logU1 <- log(U1)                          #log of upper 95%CI

L2 <- 0.68                                #lower 95%CI
U2 <- 5.79                                #upper 95%CI
logL2 <- log(L2)                          #log of lower 95%CI
logU2 <- log(U2)                          #log of upper 95%CI

#5 Width of CIs
W1 <- abs(logU1 - logL1)
W2 <- abs(logU2 - logL2)

#6 Standard Errors
SE1 <- W1 / (2*1.96)
SE2 <- W2 / (2*1.96)

#7 Difference between log RRs
d <- E1 - E2

#8 Standard Error of d
SE <- sqrt(((SE1)^2) + ((SE2)^2))

#9 Confidence Interval
Lower <- (d - (1.96*SE))
Upper <- (d + (1.96*SE))

#10 test for interaction
z <- abs(d / SE)
p <- 2*pnorm(q=z, lower.tail=FALSE)        #p value of z score

#11 Ratio of Relative Risks
RRR <- exp(d)

#12 CI RRR 
LowerRRR <- exp(Lower)
UpperRRR <- exp(Upper)

#List output
newlist <- list(
  "d" = d, 
  "SE" = SE, 
  "95% CI" = c(Lower,Upper), 
  "test for interaction z score and p value (two tailed)" = c(z, p), 
  "RRR" = RRR, 
  "95% CI of RRR" = c(LowerRRR, UpperRRR))

print(newlist)









## RRR

# _Altman & Bland function_

RRR <- function(x,y){                       # x and y are log binomial regression models (see above)
  #1 Relative risks of each model, exponentiated
  RR1 <- exp(x[[1]][2])
  RR2 <- exp(y[[1]][2])
  
  #2 log of relative risks
  E1 <- log(RR1)
  E2 <- log(RR2)
  
  #3&4 95% CIs
  CI1 <- as.data.frame(exp(confint(x)))     #exponentiate CIs
  CI1.2 <- CI1[2,]                          #choose only second element
  L1 <- CI1.2$`2.5 %`                       #lower 95%CI
  U1 <- CI1.2$`97.5 %`                      #upper 95%CI
  logL1 <- log(L1)                          #log of lower 95%CI
  logU1 <- log(U1)                          #log of upper 95%CI
  
  CI2 <- as.data.frame(exp(confint(y)))     #exponentiate CIs
  CI2.2 <- CI2[2,]                          #choose only second element
  L2 <- CI2.2$`2.5 %`                       #lower 95%CI
  U2 <- CI2.2$`97.5 %`                      #upper 95%CI
  logL2 <- log(L2)                          #log of lower 95%CI
  logU2 <- log(U2)                          #log of upper 95%CI
  
  #5 Width of CIs
  W1 <- abs(logU1 - logL1)
  W2 <- abs(logU2 - logL2)
  
  #6 Standard Errors
  SE1 <- W1 / (2*1.96)
  SE2 <- W2 / (2*1.96)
  
  #7 Difference between log RRs
  d <- E1 - E2
  
  #8 Standard Error of d
  SE <- sqrt(((SE1)^2) + ((SE2)^2))
  
  #9 Confidence Interval
  Lower <- (d - (1.96*SE))
  Upper <- (d + (1.96*SE))
  
  #10 test for interaction
  z <- abs(d / SE)
  p <- 2*pnorm(q=z, lower.tail=FALSE)        #p value of z score
  
  #11 Ratio of Relative Risks
  RRR <- exp(d)
  
  #12 CI RRR 
  LowerRRR <- exp(Lower)
  UpperRRR <- exp(Upper)
  
  #List output
  newlist <- list(
    "d" = d, 
    "SE" = SE, 
    "95% CI" = c(Lower,Upper), 
    "test for interaction z score and p value (two tailed)" = c(z, p), 
    "RRR" = RRR, 
    "95% CI of RRR" = c(LowerRRR, UpperRRR))
  
  return(newlist)
}
