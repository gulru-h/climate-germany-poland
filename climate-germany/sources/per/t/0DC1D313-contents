getwd()

renv::init()
renv::snapshot()

#usethis::use_github()
usethis::gh_token_help()

library(careless)
library(ggplot2)
library(car)
library(psych)
library(dplyr)
library(gtsummary)
library(haven)
library(corrplot)
library(corrtable)
library(effectsize)
library(apaTables)

aa <- read_sav("corrected_dataset_preliminary_cleaned.sav")
raw <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";", dec = ".")

clean <- raw[,c(23:214 )]
names(clean)

########data cleaning######
#combine variables
#finance manipulation: single item
man_finance <- subset(clean[,c(60,65)])
clean$man_finance <- apply(man_finance, 1, sum, na.rm=T)

#efficacy: single item
class(clean$effectiveness_exp)
clean$effectiveness_exp <- as.numeric(clean$effectiveness_exp)
man_eff <- subset(clean[,c(61,66)])
clean$man_eff <- apply(sub, 1, sum, na.rm=T)
mean(clean$man_eff, na.rm=T)

#acceptability
man_acc <- subset(clean[,c(63,68)])
clean$man_acc <- apply(sub, 1, sum, na.rm=T)
mean(clean$man_acc, na.rm=T)


####build means####

clean[,c(1:163)] <- clean[,c(1:163)] %>% mutate_if(is.character, as.integer)

class(clean$financial_sit_3)
finan <- subset(clean[,c(31:33)])
clean$finan <- apply(finan, 1, mean, na.rm=T)
alpha(finan)

tsec <- subset(clean[,c(34:36)])
clean$tsec <- apply(tsec, 1, mean, na.rm=T)
alpha(tsec)

#last three security items are actually freedom
tfree <- subset(clean[,c(37:39)])
clean$tfree <- apply(tfree, 1, mean, na.rm=T)
alpha(tfree)

ssec <- subset(clean[,c(52:55)])
clean$ssec <- apply(ssec, 1, mean, na.rm=T)
alpha(ssec)
class(clean$security_freedom_4)

sfree <- subset(clean[,c(56:58)])
clean$sfree <- apply(sfree, 1, mean, na.rm=T)
alpha(sfree)

clean$finan[clean$finan == "1888"]<- NA

mand <- clean

##narratives####

#subsetting and building means 


micronarratives <- subset(mand[,c(69,71,73,75)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)
alpha(micronarratives)

mainstream <- subset(mand[,c(77,79,81,83)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)
alpha(mainstream)

micronarratives_ag <- subset(mand[,c(70,72,74,76)])
mand$micronarratives_ag <- apply(micronarratives_ag, 1, mean, na.rm=T)
alpha(micronarratives_ag)

mainstream_ag <- subset(mand[,c(78,80,82,84)])
mand$mainstream_ag <- apply(mainstream_ag, 1, mean, na.rm=T)
alpha(mainstream_ag)

#acceptability assessed with one item

#save manipulation indicator as a factor
class(clean$gr)
clean$gr <- as.factor(clean$gr)
#1 control
#2 voluntary
#3 obligatory
table(mand$gr)


#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==1|mand$gr ==2 , ]

contr <- mand[mand$gr ==3, ]
vol<- mand[mand$gr ==2, ]
mandat<- mand[mand$gr ==1, ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))

#install.packages("fastDummies")

# Load the library
library(fastDummies)

# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

#creating sums of social media variables ####
microlikes <- subset(forsem[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])
microlikes1 <- subset(mand[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])

forsem$microlikes <- apply(microlikes, 1, sum, na.rm=T)
mand$microlikes <- apply(microlikes1, 1, sum, na.rm=T)
alpha(microlikes1)

mainstreamlikes <- subset(forsem[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
mainstreamlikes1 <- subset(mand[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
alpha(mainstreamlikes1)

forsem$mainstreamlikes <- apply(mainstreamlikes, 1, sum, na.rm=T)
mand$mainstreamlikes <- apply(mainstreamlikes1, 1, sum, na.rm=T)

microdislikes <- subset(forsem[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
forsem$microdislikes <- apply(microdislikes, 1, sum, na.rm=T)
microdislikes1 <- subset(mand[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
mand$microdislikes <- apply(microdislikes1, 1, sum, na.rm=T)
alpha(microdislikes1)

mainstreamdislikes <- subset(forsem[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
forsem$mainstreamdislikes <- apply(mainstreamdislikes, 1, sum, na.rm=T)
mainstreamdislikes1 <- subset(mand[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
mand$mainstreamdislikes <- apply(mainstreamdislikes1, 1, sum, na.rm=T)
alpha(mainstreamdislikes1)

microshares <- subset(forsem[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
microshares1 <- subset(mand[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
forsem$microshares <- apply(microshares, 1, sum, na.rm=T)
mand$microshares <- apply(microshares1, 1, sum, na.rm=T)

mainstreamshares <- subset(forsem[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])
mainstreamshares1 <- subset(mand[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])

forsem$mainstreamshares <- apply(mainstreamshares, 1, sum, na.rm=T)
mand$mainstreamshares <- apply(mainstreamshares1, 1, sum, na.rm=T)

microcomment <- subset(forsem[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])
microcomment1 <- subset(mand[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])

forsem$microcomment <- apply(microcomment, 1, sum, na.rm=T)
mand$microcomment <- apply(microcomment1, 1, sum, na.rm=T)

mainstreamcomment <- subset(forsem[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])
mainstreamcomment1 <- subset(mand[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])

forsem$mainstreamcomment <- apply(mainstreamcomment, 1, sum, na.rm=T)
mand$mainstreamcomment <- apply(mainstreamcomment1, 1, sum, na.rm=T)

microhash <- subset(forsem[,c("q1_sm_5","q2_sm_5","q3_sm_5","q4_sm_5")])
forsem$microhash <- apply(microhash, 1, sum, na.rm=T)

mainstreamhash <- subset(forsem[,c("q5_sm_5","q6_sm_5","q7_sm_5","q8_sm_5")])
forsem$mainstreamhash <- apply(mainstreamhash, 1, sum, na.rm=T)

######

cors <- subset(mand[,c(199, 200,197, 198,149, 28,196, 201, 202, 203,204,
                       210, 209, 212, 211, 195, 194)])
cors1 <- cors[mand$gr==1, ]
cors2 <- cors[mand$gr==2, ]
cors3 <- cors[mand$gr==3, ]

save_correlation_matrix(cors1,
                        filename = "cors1.csv",
                        digits= 2, use="lower") #mandatory

save_correlation_matrix(cors2,
                        filename = "cors2.csv",
                        digits= 2, use="lower") #voluntary

save_correlation_matrix(cors3,
                        filename = "cors3.csv",
                        digits= 2, use="lower") #control

meancors1 <- data.frame(round(sapply(cors1, mean, na.rm=T), digits= 1)) #mandatory
meancors2 <- data.frame(round(sapply(cors2, mean, na.rm=T), digits= 1)) #voluntary
meancors3 <- data.frame(round(sapply(cors3, mean, na.rm=T), digits= 1)) #control


sdcors1 <- data.frame(round(sapply(cors1, sd, na.rm=T), digits= 1)) #mandatory
sdcors2 <- data.frame(round(sapply(cors2, sd, na.rm=T), digits= 1)) #voluntary
sdcors3 <- data.frame(round(sapply(cors3, sd, na.rm=T), digits= 1)) #control




#H1: Participants in the mandatory condition will perceive a higher threat for 
#financial security in comparison to those in the voluntary and control conditions. 

#3 group
mand$gr <- as.factor(mand$gr)
library(knitr)
a <- aov(ssec ~ gr+tsec+finan, data=mand)
summary(a)
eta_squared(a)

aa <- aov(ssec ~ gr, data=mand)
summary(aa)
TukeyHSD(aa)
aatab <- apa.aov.table(aa) 


#only manipulation
b <- aov(ssec ~ gr+tsec+finan, data=onlymanipulation)
summary(b)
bb <- aov(ssec ~ gr, data=onlymanipulation)
TukeyHSD(bb)

#comparisons with control and between the experimental conditions
#are significant

#3 group
c <- aov(sfree ~ gr+tfree+finan, data=mand)
summary(c)
eta_squared(c)
ctab <- apa.aov.table(c) 

cc <- aov(sfree ~ gr, data=mand)
TukeyHSD(cc)
cctab <- apa.aov.table(cc) 

#only manipulation
d <- aov(sfree ~ gr, data=onlymanipulation)
summary(d)
TukeyHSD(d)


#trust
trustsec <- lm(institution_trust_5 ~ ssec, data= onlymanipulation)
summary(trustsec)

standardise(trustsec)

install.packages("sjPlot")
library(sjPlot)

tab_model(trustsec,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")



#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m1 <- lm(micronarratives~ ssec*active_soc_media+tsec, data=onlymanipulation)
m1.a <- summary(m1)
#ggpredict(m1, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig


tab_model(m1.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,
          digits = 2,           
          p.style = "numeric")

m2 <- lm(mainstream~ ssec*active_soc_media+tsec, data=onlymanipulation)
m2.a <- summary(m2)
#ggpredict(m2, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m2.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m3 <- lm(mainstream_ag~ ssec*active_soc_media+tsec, data=onlymanipulation)
m3.a <- summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m3.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m4 <- lm(micronarratives_ag~ ssec*active_soc_media+tsec, data=onlymanipulation)
m4.a <- summary(m4)
ggpredict(m4, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig
tab_model(m4.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")


m5 <- lm(micronarratives~ sfree*active_soc_media+tfree, data=onlymanipulation)
m5.a <- summary(m5)
ggpredict(m5, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig
tab_model(m5.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m6 <- lm(mainstream~ sfree*active_soc_media+tfree, data=onlymanipulation)
m6.a <- summary(m6)
ggpredict(m6, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m6.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m7 <- lm(mainstream_ag~ sfree*active_soc_media+tfree, data=onlymanipulation)
m7.a <- summary(m7)
ggpredict(m7, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m7.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m8 <- lm(micronarratives_ag~ sfree*active_soc_media+tfree, data=onlymanipulation)
m8.a <- summary(m8)
ggpredict(m8, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig

tab_model(m8.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")


#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
eff <- aov(man_eff~gr, data=onlymanipulation)
summary(eff)
eta_squared(eff)
TukeyHSD(eff)
#mandatory policy seen as more effective


#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.
acc <- aov(man_acc~gr, data=onlymanipulation)
summary(acc)
eta_squared(acc)
TukeyHSD(acc)
apa.aov.table(acc, "acc.doc")



mean(mand$age_5, na.rm=T)
sd(mand$age_5, na.rm=T)



###SEM Time#####
library(lavaan)
library(semptools)
library(semPlot)
library(tidySEM)

library(future)
plan(multisession) 
set.seed(545)


#install.packages("fastDummies")

# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

forsem <- onlymanipulation

which(is.na(forsem$gr))
which(is.na(mand$gr))
forsem <- forsem[-c(123:125, 187, 188, 262,263), ]
mand <- mand[-c(191, 192, 193 ,284 ,285, 400 ,401), ] 


#cfa for narratives
# interest
narint.model <- '
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  Mainstream~~Micronarratives

'

narint.fit <- cfa(narint.model, data = forsem, estimator = "ML", missing = "FIML")
summary(narint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#agreement
narag.model <- '
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Mainstream~~Micronarratives
    
'

narag.fit <- cfa(narag.model, data = forsem, estimator = "ML", missing = "FIML")
summary(narag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

#base model interest
set.seed(545)
nint.model <- '
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream~~Micronarratives

'

nint.fit <- sem(nint.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


nintestimates <- parameterestimates(nint.fit)
standardisednint <- standardizedSolution(nint.fit)
nintcovariancematrix <- data.frame(fitted(nint.fit))
nintresiduals <- data.frame(resid(nint.fit))
fitnint <- data.frame(fitMeasures(nint.fit))

library(openxlsx)
nintdatabases <- list("parameter estimates" = nintestimates, 
                      "standardised pe" = standardisednint,
                      "covariance matrix" = nintcovariancematrix,
                      "residuals" = nintresiduals,
                      "fit estimates" = fitnint)

write.xlsx(nintdatabases, file = "nintfit.xlsx", colNames = T, rowNames = T)



#base model agreement

set.seed(545)

nag.model <- '
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Micronarratives ~ gr_1
  Mainstream ~gr_1
  
  Mainstream~~Micronarratives
    
'

nag.fit <- sem(nag.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


nagestimates <- parameterestimates(nag.fit)
standardisednag <- standardizedSolution(nag.fit)
nagcovariancematrix <- data.frame(fitted(nag.fit))
nagresiduals <- data.frame(resid(nag.fit))
fitnag <- data.frame(fitMeasures(nag.fit))

library(openxlsx)
nagdatabases <- list("parameter estimates" = nagestimates, 
                      "standardised pe" = standardisednag,
                      "covariance matrix" = nagcovariancematrix,
                      "residuals" = nagresiduals,
                      "fit estimates" = fitnag)

write.xlsx(nagdatabases, file = "nagfit.xlsx", colNames = T, rowNames = T)


#SECURITY

#Interest security base

medint.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  
  Micronarratives ~b1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance
  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2
 
'


medint.fit <- sem(medint.model, data = forsem, estimator = "ML",
                  missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
                  parallel ="multicore", verbose= T)
summary(medint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)


medintestimates <- parameterestimates(medint.fit)
standardisedmedint <- standardizedSolution(medint.fit)
medintcovariancematrix <- data.frame(fitted(medint.fit))
medintresiduals <- data.frame(resid(medint.fit))
fitmedint <- data.frame(fitMeasures(medint.fit))

library(openxlsx)
medintdatabases <- list("parameter estimates" = medintestimates, 
                     "standardised pe" = standardisedmedint,
                     "covariance matrix" = medintcovariancematrix,
                     "residuals" = medintresiduals,
                     "fit estimates" = fitmedint)

write.xlsx(medintdatabases, file = "medintfit.xlsx", colNames = T, rowNames = T)






#Security - agreement base

med.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3  
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  Micronarratives ~b1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance

  Mainstream~~Micronarratives
    
  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", 
               missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
               parallel ="multicore", verbose= T)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

medfitestimates <- parameterestimates(med.fit)
standardisedmedfit <- standardizedSolution(med.fit)
medfitcovariancematrix <- data.frame(fitted(med.fit))
medfitresiduals <- data.frame(resid(med.fit))
fitmedfit <- data.frame(fitMeasures(med.fit))
library(openxlsx)
medfitdatabases <- list("parameter estimates" = medfitestimates, 
                        "standardised pe" = standardisedmedfit,
                        "covariance matrix" = medfitcovariancematrix,
                        "residuals" = medfitresiduals,
                        "fit estimates" = fitmedfit)

write.xlsx(medfitdatabases, file = "medfit.xlsx", colNames = T, rowNames = T)



#security, interest, trust

int_talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  intMainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  intMicronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  institution_trust_5 ~ traitneedsecurity+b1*intMicronarratives + b2*intMainstream +gr_1 +needsecurity+finance
  intMicronarratives ~a1*needsecurity+gr_1+traitneedsecurity+finance
  intMainstream ~ a2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2
  
  intMicronarratives~~intMainstream


'

int_talt.fit <- sem(int_talt.model, data = forsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                    parallel ="multicore", verbose= T)
summary(int_talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)


int_taltfitestimates <- parameterestimates(int_talt.fit)
standardisedint_taltfit <- standardizedSolution(int_talt.fit)
int_taltfitcovariancematrix <- data.frame(fitted(int_talt.fit))
int_taltfitresiduals <- data.frame(resid(int_talt.fit))
fitint_taltfit <- data.frame(fitMeasures(int_talt.fit))
library(openxlsx)
int_taltfitdatabases <- list("parameter estimates" = int_taltfitestimates, 
                        "standardised pe" = standardisedint_taltfit,
                        "covariance matrix" = int_taltfitcovariancematrix,
                        "residuals" = int_taltfitresiduals,
                        "fit estimates" = fitint_taltfit)

write.xlsx(int_taltfitdatabases, file = "int_taltfit.xlsx", colNames = T, rowNames = T)



#Trust - agreement security

talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
 

  institution_trust_5 ~ b1*Micronarratives + b2*Mainstream +gr_1 +needsecurity+traitneedsecurity+finance
  Micronarratives ~a1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~ a2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2

  Micronarratives~~Mainstream

'


talt.fit <- sem(talt.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


taltfitestimates <- parameterestimates(talt.fit)
standardisedtaltfit <- standardizedSolution(talt.fit)
taltfitcovariancematrix <- data.frame(fitted(talt.fit))
taltfitresiduals <- data.frame(resid(talt.fit))
fittaltfit <- data.frame(fitMeasures(talt.fit))
library(openxlsx)
taltfitdatabases <- list("parameter estimates" = taltfitestimates, 
                             "standardised pe" = standardisedtaltfit,
                             "covariance matrix" = taltfitcovariancematrix,
                             "residuals" = taltfitresiduals,
                             "fit estimates" = fittaltfit)

write.xlsx(taltfitdatabases, file = "taltfit.xlsx", colNames = T, rowNames = T)



#active media use approach security 

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 


amedint.model <- '
  stateneedsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  interaction=~ security_freedom_1.active_soc_media +security_freedom_2.active_soc_media + security_freedom_3.active_soc_media 
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 

  institution_trust_5 ~ Micronarratives+traitneedsecurity+Mainstream+stateneedsecurity+gr_1 
  stateneedsecurity ~ gr_1+active_soc_media+traitneedsecurity

  Micronarratives~~Mainstream
'

amedint.fit <- sem(amedint.model, data = dmcforsem, estimator = "MLM")
summary(amedint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


amedintfitestimates <- parameterestimates(amedint.fit)
standardisedamedintfit <- standardizedSolution(amedint.fit)
amedintfitcovariancematrix <- data.frame(fitted(amedint.fit))
amedintfitresiduals <- data.frame(resid(amedint.fit))
fitamedintfit <- data.frame(fitMeasures(amedint.fit))
library(openxlsx)
amedintfitdatabases <- list("parameter estimates" = amedintfitestimates, 
                         "standardised pe" = standardisedamedintfit,
                         "covariance matrix" = amedintfitcovariancematrix,
                         "residuals" = amedintfitresiduals,
                         "fit estimates" = fitamedintfit)

write.xlsx(amedintfitdatabases, file = "amedintfit.xlsx", colNames = T, rowNames = T)



#active media use agreement security 
dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
amed.model <- '
stateneedsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  interaction=~ security_freedom_1.active_soc_media +security_freedom_2.active_soc_media + security_freedom_3.active_soc_media 
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 

  institution_trust_5 ~ Micronarratives+traitneedsecurity+Mainstream+stateneedsecurity+gr_1 
  stateneedsecurity ~ gr_1+active_soc_media+traitneedsecurity

  Micronarratives~~Mainstream
'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


amedfitestimates <- parameterestimates(amed.fit)
standardisedamedfit <- standardizedSolution(amed.fit)
amedfitcovariancematrix <- data.frame(fitted(amed.fit))
amedfitresiduals <- data.frame(resid(amed.fit))
fitamedfit <- data.frame(fitMeasures(amed.fit))
library(openxlsx)
amedfitdatabases <- list("parameter estimates" = amedfitestimates, 
                            "standardised pe" = standardisedamedfit,
                            "covariance matrix" = amedfitcovariancematrix,
                            "residuals" = amedfitresiduals,
                            "fit estimates" = fitamedfit)

write.xlsx(amedfitdatabases, file = "amedfit.xlsx", colNames = T, rowNames = T)



#FREEDOM

#freedom approach base

frintmed.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  Micronarratives ~b1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2

'

frintmed.fit <- sem(frintmed.model, data = forsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",
                    bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frintmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



frintmedfitestimates <- parameterestimates(frintmed.fit)
standardisedfrintmedfit <- standardizedSolution(frintmed.fit)
frintmedfitcovariancematrix <- data.frame(fitted(frintmed.fit))
frintmedfitresiduals <- data.frame(resid(frintmed.fit))
fitfrintmedfit <- data.frame(fitMeasures(frintmed.fit))
library(openxlsx)
frintmedfitdatabases <- list("parameter estimates" = frintmedfitestimates, 
                         "standardised pe" = standardisedfrintmedfit,
                         "covariance matrix" = frintmedfitcovariancematrix,
                         "residuals" = frintmedfitresiduals,
                         "fit estimates" = fitfrintmedfit)

write.xlsx(frintmedfitdatabases, file = "frintmedfit.xlsx", colNames = T, rowNames = T)


#freedom -- agreement base model


frmed.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  Micronarratives ~b1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+gr_1+traitneedfreedom+finance

  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

frmed.fit <- sem(frmed.model, data = forsem, estimator = "ML", 
                 missing = "FIML", se = "bootstrap",
                 bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


frmedfitestimates <- parameterestimates(frmed.fit)
standardisedfrmedfit <- standardizedSolution(frmed.fit)
frmedfitcovariancematrix <- data.frame(fitted(frmed.fit))
frmedfitresiduals <- data.frame(resid(frmed.fit))
fitfrmedfit <- data.frame(fitMeasures(frmed.fit))
library(openxlsx)
frmedfitdatabases <- list("parameter estimates" = frmedfitestimates, 
                             "standardised pe" = standardisedfrmedfit,
                             "covariance matrix" = frmedfitcovariancematrix,
                             "residuals" = frmedfitresiduals,
                             "fit estimates" = fitfrmedfit)

write.xlsx(frmedfitdatabases, file = "frmedfit.xlsx", colNames = T, rowNames = T)


#freedom approach trust

frtaltint.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance
  institution_trust_5~traitneedfreedom+finance

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  Mainstream~~Micronarratives

'

frtaltint.fit <- sem(frtaltint.model, data = forsem, estimator = "ML", 
                     missing = "FIML", se = "bootstrap",
                     bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtaltint.fit, fit.measures=T, standardized = T, rsquare=TRUE,
        ci=T)


frtaltintfitestimates <- parameterestimates(frtaltint.fit)
standardisedfrtaltintfit <- standardizedSolution(frtaltint.fit)
frtaltintfitcovariancematrix <- data.frame(fitted(frtaltint.fit))
frtaltintfitresiduals <- data.frame(resid(frtaltint.fit))
fitfrtaltintfit <- data.frame(fitMeasures(frtaltint.fit))
library(openxlsx)
frtaltintfitdatabases <- list("parameter estimates" = frtaltintfitestimates, 
                          "standardised pe" = standardisedfrtaltintfit,
                          "covariance matrix" = frtaltintfitcovariancematrix,
                          "residuals" = frtaltintfitresiduals,
                          "fit estimates" = fitfrtaltintfit)

write.xlsx(frtaltintfitdatabases, file = "frtaltintfit.xlsx", colNames = T, rowNames = T)


#freedom agreement trust



frtalt.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  Mainstream~~Micronarratives

'


frtalt.fit <- sem(frtalt.model, data = forsem, estimator = "ML", 
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtalt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


frtaltfitestimates <- parameterestimates(frtalt.fit)
standardisedfrtaltfit <- standardizedSolution(frtalt.fit)
frtaltfitcovariancematrix <- data.frame(fitted(frtalt.fit))
frtaltfitresiduals <- data.frame(resid(frtalt.fit))
fitfrtaltfit <- data.frame(fitMeasures(frtalt.fit))
library(openxlsx)
frtaltfitdatabases <- list("parameter estimates" = frtaltfitestimates, 
                              "standardised pe" = standardisedfrtaltfit,
                              "covariance matrix" = frtaltfitcovariancematrix,
                              "residuals" = frtaltfitresiduals,
                              "fit estimates" = fitfrtaltfit)

write.xlsx(frtaltfitdatabases, file = "frtaltfit.xlsx", colNames = T, rowNames = T)

#freedom approach trust active media use

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
frintamed.model <- '
  stateneedfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
  Micronarratives ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  Mainstream ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  active_soc_media ~ traitneedfreedom

  institution_trust_5 ~ Micronarratives+Mainstream+stateneedfreedom+traitneedfreedom+gr_1
  stateneedfreedom ~ gr_1+traitneedfreedom+active_soc_media
  
  Mainstream~~Micronarratives

'

frintamed.fit <- sem(frintamed.model, data = dmcforsem, estimator = "MLM")
summary(frintamed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


frintamedfitestimates <- parameterestimates(frintamed.fit)
standardisedfrintamedfit <- standardizedSolution(frintamed.fit)
frintamedfitcovariancematrix <- data.frame(fitted(frintamed.fit))
frintamedfitresiduals <- data.frame(resid(frintamed.fit))
fitfrintamedfit <- data.frame(fitMeasures(frintamed.fit))
library(openxlsx)
frintamedfitdatabases <- list("parameter estimates" = frintamedfitestimates, 
                           "standardised pe" = standardisedfrintamedfit,
                           "covariance matrix" = frintamedfitcovariancematrix,
                           "residuals" = frintamedfitresiduals,
                           "fit estimates" = fitfrintamedfit)

write.xlsx(frintamedfitdatabases, file = "frintamedfit.xlsx", colNames = T, rowNames = T)

#freedom agreement trust active media use
dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
fragamed.model <- '
  stateneedfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
   
  Micronarratives ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  Mainstream ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  active_soc_media ~ traitneedfreedom

  institution_trust_5 ~ Micronarratives+Mainstream+stateneedfreedom+traitneedfreedom+gr_1
  stateneedfreedom ~ gr_1+traitneedfreedom+active_soc_media
  
  Mainstream~~Micronarratives


'

fragamed.fit <- sem(fragamed.model, data = dmcforsem, estimator = "MLM")
summary(fragamed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


fragamedfitestimates <- parameterestimates(fragamed.fit)
standardisedfragamedfit <- standardizedSolution(fragamed.fit)
fragamedfitcovariancematrix <- data.frame(fitted(fragamed.fit))
fragamedfitresiduals <- data.frame(resid(fragamed.fit))
fitfragamedfit <- data.frame(fitMeasures(fragamed.fit))
library(openxlsx)
fragamedfitdatabases <- list("parameter estimates" = fragamedfitestimates, 
                           "standardised pe" = standardisedfragamedfit,
                           "covariance matrix" = fragamedfitcovariancematrix,
                           "residuals" = fragamedfitresiduals,
                           "fit estimates" = fitfragamedfit)

write.xlsx(fragamedfitdatabases, file = "fragamedfit.xlsx", colNames = T, rowNames = T)


#MEDIA VARIABLES
#Number of Likes and Dislikes, need for security, active media use

ldla.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  
  likeMicronarratives ~a1*needsecurity+gr_1+c1*active_soc_media+traitneedsecurity+finance
  likeMainstream ~a2*needsecurity+gr_1+c2*active_soc_media+traitneedsecurity+finance

  dislikeMicronarratives ~a3*needsecurity+gr_1+c3*active_soc_media+traitneedsecurity+finance
  dislikeMainstream ~a4*needsecurity+gr_1+c4*active_soc_media+traitneedsecurity+finance
  
  needsecurity ~ gr_1 +traitneedsecurity+finance
  institution_trust_5 ~ needsecurity +active_soc_media+gr_1+traitneedsecurity
  +b1*likeMicronarratives+b2*likeMainstream+b3*dislikeMicronarratives+
  b4*dislikeMainstream + finance

  active_soc_media~traitneedsecurity+finance
  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*b3
  ind4 := a4*b4
  
  ind5 := c1*b1
  ind6 := c2*b2
  ind7 := c3*b3
  ind8 := c4*b4
  
  #q3_sm_2~~q3_sm_6
  #q1_sm_2~~q1_sm_6
  #q7_sm_2~~q7_sm_6
  #q6_sm_2~~q6_sm_6
  #q5_sm_2~~q5_sm_6
  #q2_sm_2~~q2_sm_6
  #q4_sm_2~~q4_sm_6
  #q8_sm_2~~q8_sm_6
  
  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needsecurity


'

ldla.fit <- sem(ldla.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(ldla.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

ldlafitestimates <- parameterestimates(ldla.fit)
standardisedldlafit <- standardizedSolution(ldla.fit)
ldlafitcovariancematrix <- data.frame(fitted(ldla.fit))
ldlafitresiduals <- data.frame(resid(ldla.fit))
fitldlafit <- data.frame(fitMeasures(ldla.fit))
library(openxlsx)
ldlafitdatabases <- list("parameter estimates" = ldlafitestimates, 
                           "standardised pe" = standardisedldlafit,
                           "covariance matrix" = ldlafitcovariancematrix,
                           "residuals" = ldlafitresiduals,
                           "fit estimates" = fitldlafit)

write.xlsx(ldlafitdatabases, file = "ldlafit.xlsx", colNames = T, rowNames = T)


#Number of Likes and Dislikes, need for freedom, active media use

#both
fldl.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  
    
  likeMicronarratives ~a1*needfreedom+gr_1+c1*active_soc_media+traitneedfreedom+finance
  likeMainstream ~a2*needfreedom+gr_1+c2*active_soc_media+traitneedfreedom+finance

  dislikeMicronarratives ~a3*needfreedom+gr_1+c3*active_soc_media+traitneedfreedom+finance
  dislikeMainstream ~a4*needfreedom+gr_1+c4*active_soc_media+traitneedfreedom+finance
  
  needfreedom ~ gr_1+traitneedfreedom+finance
  institution_trust_5 ~ gr_1+active_soc_media+gr_1+traitneedfreedom
  +b1*likeMicronarratives+b2*likeMainstream+b3*dislikeMicronarratives+
  b4*dislikeMainstream + finance
    
  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*b3
  ind4 := a4*b4
  
  ind5 := c1*b1
  ind6 := c2*b2
  ind7 := c3*b3
  ind8 := c4*b4
  
  
  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needfreedom
'

fldl.fit <- sem(fldl.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(fldl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

fldlfitestimates <- parameterestimates(fldl.fit)
standardisedfldlfit <- standardizedSolution(fldl.fit)
fldlfitcovariancematrix <- data.frame(fitted(fldl.fit))
fldlfitresiduals <- data.frame(resid(fldl.fit))
fitfldlfit <- data.frame(fitMeasures(fldl.fit))
library(openxlsx)
fldlfitdatabases <- list("parameter estimates" = fldlfitestimates, 
                         "standardised pe" = standardisedfldlfit,
                         "covariance matrix" = fldlfitcovariancematrix,
                         "residuals" = fldlfitresiduals,
                         "fit estimates" = fitfldlfit)

write.xlsx(fldlfitdatabases, file = "fldlfit.xlsx", colNames = T, rowNames = T)


