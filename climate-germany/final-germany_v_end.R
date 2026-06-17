getwd()
renv::init()
renv::snapshot()

usethis::create_github_token(description = "climate-germany")
gitcreds::gitcreds_set() 
usethis::use_github()
usethis::gh_token_help()

library(careless)
library(ggplot2)
library(car)
library(psych)
library(dplyr)
library(gtsummary)
library(lavaan)
library(tidySEM)
library(haven)
library(semTools)
library(corrplot)
library(lavaan)
library(ggplot2)
library(ggeffects)
library(openxlsx)
library(relaimpo)
library(effectsize)
library(apaTables)
library(emmeans)

##########DO NOT TOUCH################
########data cleaning######

#raw <- read.csv("data8.csv", sep = ";", na.strings = "-99")  #only the completes

#raw1 <- read.csv("data3.csv", sep = ";", na.strings = "-99")  #only the completes

#raw[c(1:189), ]$c_0001[raw[c(1:189), ]$c_0001==1 | raw[c(1:189), ]$c_0001==3] <- 1
#raw[c(189:236), ]$c_0001 <- 1

#raw1[c(519:711), ]$c_0001[raw1[c(519:711), ]$c_0001==1 | raw1[c(519:711), ]$c_0001==3] <- 1
#raw1 <- raw1[c(13:711),]

#raw1$c_0001

#colnames(raw1) <- colnames(raw)    

#raw <- rbind(raw1, raw)
#935
#write.csv(raw, "raw.csv")
#########
#use the raw.csv file!
raw2 <- read.csv("raw.csv")
raw <- raw2[, 2:146]

clean <- raw

table(clean$c_0001)
#homeowner check 
which(clean$v_114 == 0) #check successful -- all own a house/apartment

#group sizes
table(clean$c_0001)
#   1   2   3 
#470 295 170 

#number of completes
nrow(clean)
#935 complete cases

#remove empty columns
clean <- clean[,-c(18:29)]

#combine variables

#finance manipulation
sub <- subset(clean[,c(40:41)])
clean$man_finance <- apply(sub, 1, sum, na.rm=T)

#efficacy
sub <- subset(clean[,c(42:43)])
clean$man_eff <- apply(sub, 1, sum, na.rm=T)

#mancheck1
sub <- subset(clean[,c(54, 56)])
clean$man_check1 <- apply(sub, 1, sum, na.rm=T)

#mancheck2
sub <- subset(clean[,c(55, 57)])
clean$man_check2 <- apply(sub, 1, sum, na.rm=T)


#remove more irrelevant columns 
clean <- clean[,-c(74:133)]
clean <- clean[,-c(41,43,56,57)]
clean <- clean[,-c(1:6)]
table(clean$c_0001)

#checking ranges

#age
range(clean$age)  
which(clean$age == 722)  #correct age
clean[266,]$age <- 72
mean(clean$age, na.rm = T)


####build means####

finan <- subset(clean[,c(13:15)])
clean$finan <- apply(finan, 1, mean, na.rm=T)
psych::alpha(finan) #.95

tsec <- subset(clean[,c(21:23)])
clean$tsec <- apply(tsec, 1, mean, na.rm=T)
psych::alpha(tsec) #.74 

tfree <- subset(clean[,c(24:26)])
clean$tfree <- apply(tfree, 1, mean, na.rm=T)
psych::alpha(tfree) #.81 

ssec <- subset(clean[,c(27:29)])
clean$ssec <- apply(ssec, 1, mean, na.rm=T)
psych::alpha(ssec)#.96

sfree <- subset(clean[,c(30:32)])
clean$sfree <- apply(sfree, 1, mean, na.rm=T)
psych::alpha(sfree)#.97

man_acc <- subset(clean[,c(36,37)])
clean$man_acc <- apply(man_acc, 1, mean, na.rm=T)
psych::alpha(man_acc) #.94

#save manipulation indicator as a factor
class(clean$c_0001)
clean$c_0001 <- as.factor(clean$c_0001)


#attention check
clean$v_113[clean$att==2 & clean$c_0001==1] <- 1
clean$v_113[clean$att==1 & clean$c_0001==2] <- 2
clean$v_113[clean$att==3]<- 3

clean1 <- subset(clean, clean$v_113 == 0)


table(clean$v_113)

table(clean1$c_0001)
#1   2   3 
#223 233 169 

#check if manipulation worked
which(clean1[clean1$c_0001 == 1:2, ]$man_check1> 1)

table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check2 > 1)
table(clean1[clean1$c_0001 !=3, ]$man_check1 > 1 & clean1[clean1$c_0001 !=3, ]$man_check2 > 1)


clean1[clean1$c_0001 ==3, ]$man_check1> 1

table(clean1$man_check1==1 & clean1$c_0001 != 3) 
table(clean1$man_check2==1 & clean1$c_0001 != 3) 

cond1 = clean1$c_0001!=3
cond2 = clean1$man_check1 > 1
cond3 = clean1$man_check2 > 1

cond = (cond1 & cond2 & cond3) | !cond1

mand <- clean1[cond,]
table(mand[mand$c_0001 !=3, ]$man_check2 > 1)

table(mand$c_0001)

#  1   2   3 
#188 167 169 

###FROM HERE ON OUT USE mand FOR CALCULATIONS #####

#detecting careless answers 

#checking duration
mand$duration_m <- mand$duration/60
summary(mand$duration_m)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.767   7.867  10.900  13.092  14.704 119.400 
10.900/2

mand <- mand[mand$duration_m >  10.900/2  , ]
nrow(mand)# n = 482

summary(mand$duration_m)
ggplot(mand, aes(y = duration_m)) + geom_boxplot()


#creating a manipulation only group 

table(mand$c_0001)
#table(mand$gr) <- after renaming, ignore for now
#  1   2   3 
#174 156 152 
##########DO NOT TOUCH#######


#subsetting and building means 

micronarratives <- subset(mand[,c(38,40,42,44)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)
psych::alpha(micronarratives) #.76

micronarratives <- subset(clean1[,c(38,40,42,44)])
clean1$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)
psych::alpha(micronarratives) #.76

mainstream <- subset(mand[,c(39,41,43,45)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)
alpha(mainstream) #.81

#renaming things
mand<- rename(mand, gr = c_0001)
clean1<- rename(clean1, gr = c_0001)
#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==1 |mand$gr ==2 , ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))


library(fastDummies)
# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- fastDummies::dummy_cols(mand, select_columns = "gr")

#to make comparisons easier with Poland
onlymanipulation$gr_1 <- onlymanipulation$gr_2 # obligatory: group 1 is mandatory
#onlymanipulation$gr_2 <- onlymanipulation$gr_1 #voluntary: group 2 is voluntary

onlymanipulation$gr_a[onlymanipulation$gr == 1] <- 2
onlymanipulation$gr_a[onlymanipulation$gr == 2] <- 1

onlymanipulation$gr_a <- as.factor(onlymanipulation$gr_a)

mand$gr_a[mand$gr == 1] <- 2
mand$gr_a[mand$gr == 2] <- 1
mand$gr_a[mand$gr == 3] <- 3


clean1$gr_a <- as.factor(clean1$gr)

clean1$gr_a[clean1$gr == 1] <- 2
clean1$gr_a[clean1$gr == 2] <- 1
clean1$gr_a[clean1$gr == 3] <- 3

#save(mand, file="mand.RData")

#done with cleaning plus data wrangling 

#gender
table(mand$gender)
#167 women 313 men

#ses
range(mand$SES, na.rm = T)
hist(mand$SES)#normal-ish
mean(mand$SES)

#pol
hist(mand$pol)
table(mand$pol)
#1CDU/CSU
#2SPD
#3Bündnis 90/Die Grünen
#4FDP
#5Die Linke
#6Bündnis Sahra Wagenknecht (BSW)
#7AfD
#8Andere Partei
#9Ich würde nicht wählen
#10Ich möchte dazu keine Angabe machen"


####correlations
#cors <- subset(mand[,c(68, 69,66, 67,72,73, 35, 70)])
cors <- subset(mand[,c(64:68, 71,72)])
cors<- subset(mand[,c("ssec", "tsec", "sfree", "tfree",
                      "finan", "SES", "man_finance",
                      "trust5", 
                      "med_act", 
                      "micronarratives", "mainstream")])


cors1 <- cors[mand$gr_a==1, ] #mandatory 
cors2 <- cors[mand$gr_a==2, ] #voluntary
cors3 <- cors[mand$gr_a==3, ] #control

library(corrtable)
save_correlation_matrix(cors1,
                        filename = "cors1.csv",
                        digits= 2, use="lower")

save_correlation_matrix(cors2,
                        filename = "cors2.csv",
                        digits= 2, use="lower")

save_correlation_matrix(cors3,
                        filename = "cors3.csv",
                        digits= 2, use="lower")

meancors1 <- data.frame(round(sapply(cors1, mean, na.rm=T), digits=1))
meancors2 <- data.frame(round(sapply(cors2, mean, na.rm=T), digits=1))
meancors3 <- data.frame(round(sapply(cors3, mean, na.rm=T), digits=1))


sdcors1 <- data.frame(round(sapply(cors1, sd, na.rm=T), digits=1))
sdcors2 <- data.frame(round(sapply(cors2, sd, na.rm=T), digits=1))
sdcors3 <- data.frame(round(sapply(cors3, sd, na.rm=T), digits=1))

# Combine into one data frame with M and SD columns
desc_table <- data.frame(
  Variable = rownames(meancors1),
  M  = paste0(meancors1[,1], " / ", meancors2[,1], " / ", meancors3[,1]),
  SD = paste0(sdcors1[,1],   " / ", sdcors2[,1],   " / ", sdcors3[,1])
)

# As flextable
library(flextable)
desc_table |>
  flextable() |>
  set_header_labels(M = "M (G1 / G2 / G3)", SD = "SD (G1 / G2 / G3)") |>
  bold(part = "header") |>
  autofit()

#in the manuscript the mandatory group is presented first for better 
#comparability with poland 

######

####analyses

#1- mandatory
#2- voluntary
#3 - control

mand$gr_a <- as.factor(mand$gr_a)
mand$gender <- as.factor(mand$gender)

#use gr_a when calculating with mand and onlymanipulation

#CAN WE TELL THE NEEDS APART

needs <- 'SEC=~ ssec1+ssec2+ssec3
          FREE=~ sfree1+sfree2+sfree3
         # TSEC=~ tsec1+tsec2+tsec3
          #TFREE=~ tfree1+tfree2+tfree3
          SEC~~FREE
          #TFREE~~TSEC
         # TSEC~~SEC
         # TSEC~~FREE
         # TFREE~~ SEC
         # TFREE~~ FREE

'

needs.fit<- cfa(needs, data = mand, estimator = "ML")
summary(needs.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

model_constrained <- gsub("SEC~~FREE", "SEC ~~ 1*FREE", needs)
fit_constrained <- cfa(model_constrained, data = mand)
summary(fit_constrained, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
  anova(needs.fit, fit_constrained)
library(semTools)
AVE(needs.fit)
htmt(needs, data=mand)
sqrt(0.920)
sqrt(0.904)
needs1 <- 'COMBO=~ ssec1+ssec2+ssec3+sfree1+sfree2+sfree3
           # TSEC=~ tsec1+tsec2+tsec3
           # TFREE=~tfree1+tfree2+tfree3
         #  COMBO~~TSEC
          # COMBO~~TFREE

'

needs1.fit<- cfa(needs1, data = mand, estimator = "ML")
summary(needs1.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
#the fit of the combination model is awful 
mi<- modificationindices(needs1.fit)

anova(needs.fit, needs1.fit)

standardizedSolution(needs.fit, type = "std.all", se = TRUE, ci = TRUE) %>%
  filter(op == "~~", lhs != rhs)
needs_bi <- 'SEC=~ ssec1+ssec2+ssec3
          FREE=~ sfree1+sfree2+sfree3
          COMBO=~ SEC+FREE
         # TSEC=~ tsec1+tsec2+tsec3
          #TFREE=~ tfree1+tfree2+tfree3
          SEC~~FREE
          #TFREE~~TSEC
         # TSEC~~SEC
         # TSEC~~FREE
         # TFREE~~ SEC
         # TFREE~~ FREE

'
needsbi.fit<- cfa(needs_bi, data = mand, estimator = "ML")
summary(needsbi.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

needs_bifactor <- '
  # General factor — loads on ALL items directly
  COMBO =~ ssec1 + ssec2 + ssec3 + sfree1 + sfree2 + sfree3
  
  # Specific factors — load only on their own items
  SEC  =~ ssec1 + ssec2 + ssec3
  FREE =~ sfree1 + sfree2 + sfree3
  
  # Bifactor models require orthogonality constraints
  COMBO ~~ 0*SEC
  COMBO ~~ 0*FREE
  SEC   ~~ 0*FREE
'

fit_bifactor <- cfa(needs_bifactor, data = mand, orthogonal = TRUE)
summary(fit_bifactor, fit.measures = TRUE, standardized = TRUE)

#new analyses

mand$ssec_c <- scale(mand$ssec, center=TRUE, scale=FALSE)[,1]
mand$tsec_c <- scale(mand$tsec, center=TRUE, scale=FALSE)[,1]
mand$sfree_c <- scale(mand$sfree, center=TRUE, scale=FALSE)[,1]
mand$trust5_c <- scale(mand$trust5, center=TRUE, scale=FALSE)[,1]
mand$finan_c <- scale(mand$finan, center=TRUE, scale=FALSE)[,1]


mand$ssec_s <- scale(mand$ssec, center=TRUE, scale=T)[,1]
mand$tsec_s <- scale(mand$tsec, center=TRUE, scale=T)[,1]
mand$sfree_s <- scale(mand$sfree, center=TRUE, scale=T)[,1]
mand$tfree_s <- scale(mand$tfree, center=TRUE, scale=T)[,1]

mand$trust5_s <- scale(mand$trust5, center=TRUE, scale=T)[,1]
mand$finan_s <- scale(mand$finan, center=TRUE, scale=T)[,1]
mand$med_act_s <- scale(mand$med_act, center=TRUE, scale=T)[,1]
mand$medtime_1_s <- scale(mand$medtime_1, center=TRUE, scale=T)[,1]
mand$SES_s <- scale(mand$SES, center=TRUE, scale=T)[,1]
mand$man_finance_s <- scale(mand$man_finance, center=TRUE, scale=T)[,1]
mand$micronarratives_s <- scale(mand$micronarratives, center=TRUE, scale=T)[,1]
mand$mainstream_s <- scale(mand$mainstream, center=TRUE, scale=T)[,1]


#FINANCES 
mand_com <- mand[complete.cases(mand[, c("sfree", "gr_a", "tsec", "ssec", "finan", 
                                               "tfree", "man_finance", "SES", "gender", "age")]), ]

#SECURITY 

n1 <- lm(ssec~ gr_a, data=mand)
summary(n1)

pw <- pairs(emmeans(n1, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")

Anova(n1)
effectsize(Anova(n1))


n666 <- lm(ssec~ gr_a*(sfree_s+finan_s)+micronarratives_s+mainstream_s+tfree_s+tsec_s+SES_s+gender+age, data=mand)
summary(n666)
anova(n666, n6) # no change
ggpredict(n66, terms = c("finan_s","gr_a")) |> plot()
effectsize(Anova(n666))

pw <- pairs(emmeans(n666, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")

sd_fin <- sd(mandpl$micronarratives_ag_c, na.rm = TRUE)

library(emmeans)
emtrends(n666fr, 
         pairwise ~ gr_a, 
         var = "finan_s",
        # at = list(micronarratives_ag_c = c(-sd_mna, 0, sd_mna))
         )



contrast(emtrends(n666, ~ gr_a, var="finan_s"), method= "pairwise", adjust="BH")

ggpredict(n6, terms = c("finan_s","gr_a")) |> plot()



#FREEDOM

n1fr <- lm(sfree~ gr_a, data=mand)
summary(n1fr)
Anova(n1fr)
effectsize(Anova(n1fr))

pwfr <- pairs(emmeans(n1fr, ~gr_a), adjust = "BH")
summary(pwfr, infer = TRUE, adjust ="tukey")


n666fr <- lm(sfree~ gr_a*(ssec_s+finan_s)+mainstream_s+micronarratives_s+tfree_s+tsec_s+SES_s+gender+age, data=mand)
summary(n666fr)
anova(n666, n6) # no change
ggpredict(n66fr, terms = c("finan_s","gr_a")) |> plot()
##use that one



n66fr <- lm(sfree~ gr_a*(ssec_s+finan_s+man_finance_s)+tfree_s+tsec_s++SES_s+gender+age, data=mand)
summary(n66fr)
anova(n666fr, n66fr) # no change




library(modelsummary)
library(flextable)

modelsummary(
  list("Security" = n666, "Freedom" = n666fr, "Anti-Mainstream" = milmend, "Mainstream" = maend),
  stars = c("*" = .05, "**" = .01, "***" = .001), 
#  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  fmt = 2,             # decimal places
  estimate = "{estimate}{stars}",
  statistic = "[{conf.low}, {conf.high}]",
  coef_rename = c("gr_a" = "Group",
                  "gr_a2"= "Voluntary",
                  "gr_a3" = "Control",
                  "finan_s" = "Financial Situation",
                  "sfree_s" = "State Need for Freedom",
                  "tfree_s" = "Trait Need for Freedom",
                  "tsec_s" = "Trait Need for Security",
                  "age" = "Age", 
                  "gender2" = "Gender (Male)",
                  "SES_s" = "SES",
                  "ssec_s" = "State Need for Security",
                  "micronarratives_s" = "Anti-Mainstream Narratives",
                  "mainstream_s" = "Mainstream Narratives"),
  output = "flextable"
  #shape = term ~ model + statistic
) |>
  save_as_docx(path = "table3.docx")



#H2


#### Predicting micronarratives with needs####
m00<- lm(micronarratives~ gr_a,
           data=mand) 
summary(m00)
mic <- pairs(emmeans(m00, ~gr_a), adjust = "BH")
summary(mic, infer = TRUE, adjust ="tukey")


Anova(m00)
#1-2, 2-3 sign
ma00<- lm(mainstream~ gr_a,
         data=mand) 
summary(ma00)
Anova(ma00)
ma <- pairs(emmeans(ma00, ~gr_a), adjust = "BH")
summary(ma, infer = TRUE, adjust ="tukey")



milmend<- lm(micronarratives~
             gr_a*(sfree_s+ssec_s+finan_s)+mainstream_s+SES_s+tsec_s+tfree_s+age+gender,
           data=mand)

summary(milmend)
car::vif(milmend, type= "predictor")
anova(milm1, milm3)
car::Anova(milmend)

maend<- lm(mainstream~
               gr_a*(sfree_s+ssec_s+finan_s)+micronarratives_s+SES_s+tsec_s+tfree_s+age+gender,
             data=mand)
summary(maend)
anova(milm1, milm3)
car::Anova(maend)
car::vif(maend, type= "predictor")

#no differences 
+micronarratives_s
emtrends(microlmp_end, ~ gr_a, var="finan_c")


ggpredict(microlmp_end, terms = c("sfree_c","finan_c", "gr_a")) |> plot()
modelsummary(
  list("Anti-Mainstream" = milmend, "Mainstream" = maend),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  fmt = 2,
  estimate = "{estimate}{stars}",
  statistic = "[{conf.low}, {conf.high}]"
)





###SEM Time ####
library(lavaan)
onlymanipulation <- mand[mand$gr ==1 |mand$gr ==2 , ]
forsem <- onlymanipulation

which(is.na(forsem$c_0001))
#no missings
which(is.na(mand$tsec))


#check if anyone in the sample already had exclusively climate friendly heaters 
table(mand[ , mand$heiz1==1 & mand[, c(40:45)] == 0]$heiz1)
mand$heiz1[mand$heiz1==2 & mand[, c(40:44)] == 0]

#checking the factor structure of narratives 

nar.model <- '
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 

  Mainstream ~~ Micronarratives
'

nar.fit <- cfa(nar.model, data = forsem, estimator = "ML")
summary(nar.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


#security
secu <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 

  stateneedsecurity ~ a_m3d*gr_1
  Micronarratives ~bM1d*stateneedsecurity+b1d*gr_1
  Mainstream ~bM2d*stateneedsecurity+b4d*gr_1


  ind1d := a_m3d*bM1d
  ind2d := a_m3d*bM2d
  total1d := ind1d+b1d
  total2d := ind2d+b4d

'


secu.fit <- sem(secu, data = de, estimator = "ML",missing = "FIML",
                se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(secu.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
lay <- get_layout("","","","Mainstream",
                  "gr_1","","stateneedsecurity","",
                  "","","","Micronarratives", rows = 3)
graph_sem(secu.fit, layout=lay)


#freedom
free <- '
  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  stateneedfreedom=~sfree1+sfree2+sfree3
  
  stateneedfreedom ~ a_m3d*gr_1
  Micronarratives ~bM1d*stateneedfreedom+b1d*gr_1
  Mainstream ~bM2d*stateneedfreedom+b4d*gr_1


  ind1d := a_m3d*bM1d
  ind2d := a_m3d*bM2d
  total1d := ind1d+b1d
  total2d := ind2d+b4d


'


free.fit <- sem(free, data = de, estimator = "ML"
,
missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
parallel ="multicore", verbose= F)

summary(free.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

lay <- get_layout("","","","Mainstream",
                  "gr_1","","stateneedfreedom","",
                  "","","","Micronarratives", rows = 3)
graph_sem(free.fit, layout=lay)


finance=~ finan1+finan2+finan3
tsecu=~ tsec1+tsec2+tsec3
tfreed=~tfree1+tfree2+tfree3

mand$gr_n <- as.numeric(mand$gr_a)
mand_com$gr_1 <- as.factor(mand_com$gr_1)
mand_com$gr_2 <- as.factor(mand_com$gr_2)


mand_com$C1 <- ifelse(mand_com$gr_a == 1, 1, ifelse(mand_com$gr_a == 2, -1, 0))
mand_com$C2 <- ifelse(mand_com$gr_a == 1, 1, ifelse(mand_com$gr_a == 3, -1, 0))
#source for contrast coding: Cohen, Cohen, West, and Aiken (2003) page 338 specifically 


#NEW ADDITIONS

#old analyses redone

####SEM with two groups and security####


secu2 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity +a1*gr_2
  Mainstream      ~ c3*stateneedsecurity +a3*gr_2
  

  stateneedsecurity ~ b1*gr_2

  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  total_1v2_micro := a1 + (b1*c1) 
  total_1v2_main  := a3 + (b1*c3)

  
'


secu2.fit <- lavaan::sem(secu2, data = onlymanipulation, estimator = "ML", 
                        se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(secu2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
secu2.ml.fit <- lavaan::sem(secu2, data = mand_com, estimator = "ML")


#######

####SEM with 2 groups and freedom####

free2 <- '
  stateneedfreedom=~sfree1+sfree2+sfree3
  

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~  c2*stateneedfreedom + a1*gr_2
  Mainstream      ~  c4*stateneedfreedom + a3*gr_2
  
  stateneedfreedom~ b3*gr_2
  
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  total_1v2_micro := a1 +  (b3*c2)
  total_1v2_main  := a3 +  (b3*c4)
 
'


free2.fit <- lavaan::sem(free2, data = onlymanipulation, estimator = "ML", 
                        se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(free2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
free2.ml.fit <- lavaan::sem(free2, data = mand_com, estimator = "ML")


#######

####SEM with 2 groups and both needs####

secufree2 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*gr_2
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*gr_2
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*gr_2
  stateneedfreedom~ b3*gr_2
  
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  # --- Total indirect effects (both mediators) ---
  total_ind_1v2_micro := (b1*c1) + (b3*c2)
  total_ind_1v2_main  := (b1*c3) + (b3*c4)
 

  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)

  
'


secufree2.fit <- lavaan::sem(secufree2, data = onlymanipulation, estimator = "ML", 
                            se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

secufree2.ml.fit <- lavaan::sem(secufree2, data = mand_com, estimator = "ML")


summary(secufree2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

library(dplyr)

est <- parameterEstimates(secufree2.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()


#######

vuongtest(free2.ml.fit,secufree2.ml.fit)
vuongtest(secu2.ml.fit,secufree2.ml.fit)


##higher order factor####
secufreehigh <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  
  StateNeeds =~ stateneedsecurity + stateneedfreedom

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*StateNeeds + a1*gr_2
  Mainstream      ~ c3*StateNeeds + a3*gr_2
  
  stateneedfreedom~~stateneedsecurity
  
  StateNeeds ~ b1*gr_2

  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3



  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1)
  total_1v2_main  := a3 + (b1*c3)

  
'


secufreehigh.fit <- lavaan::sem(secufreehigh, data = onlymanipulation, estimator = "ML", 
                             se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)


summary(secufreehigh.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

###



####SEM with all three groups and security####


secu <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3


  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + a1*C1 + a2*C2
  Mainstream      ~ c3*stateneedsecurity + a3*C1 + a4*C2
  

  stateneedsecurity ~ b1*C1 + b2*C2

  secdiff_1v2  := b1
  secdiff_1v3  := b2
  secdiff_2v3  := b2 - b1

    
  midiff_1v2   := a1
  midiff_1v3   := a2
  midiff_2v3   := a2 - a1
  
  madiff_1v2   := a3
  madiff_1v3   := a4
  madiff_2v3   := a4 - a3
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3


  # C2 (1v3) -> security -> outcomes
  ind_1v3_sec_micro := b2 * c1
  ind_1v3_sec_main  := b2 * c3


  # 2v3 derived -> security -> outcomes
  ind_2v3_sec_micro := (b2 - b1) * c1
  ind_2v3_sec_main  := (b2 - b1) * c3


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) 
  total_1v2_main  := a3 + (b1*c3)
  total_1v3_micro := a2 + (b2*c1) 
  total_1v3_main  := a4 + (b2*c3) 
  total_2v3_micro := (a2-a1) + ((b2-b1)*c1) 
  total_2v3_main  := (a4-a3) + ((b2-b1)*c3) 
  
'


secu.fit <- lavaan::sem(secu, data = mand_com, estimator = "ML", 
                            se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(secu.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


library(dplyr)

est <- parameterEstimates(secu.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######



####SEM with all three groups and freedom####



free <- '
  stateneedfreedom=~sfree1+sfree2+sfree3
  

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~  c2*stateneedfreedom + a1*C1 + a2*C2
  Mainstream      ~  c4*stateneedfreedom + a3*C1 + a4*C2
  
  stateneedfreedom~ b3*C1 + b4*C2
  
 
  freediff_1v2 := b3
  freediff_1v3 := b4
  freediff_2v3 := b4 - b3
  
    
  midiff_1v2   := a1
  midiff_1v3   := a2
  midiff_2v3   := a2 - a1
  
  madiff_1v2   := a3
  madiff_1v3   := a4
  madiff_2v3   := a4 - a3
  
  # --- Indirect effects ---

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4


  # C2 (1v3) -> freedom -> outcomes
  ind_1v3_free_micro := b4 * c2
  ind_1v3_free_main  := b4 * c4

  # 2v3 derived -> freedom -> outcomes
  ind_2v3_free_micro := (b4 - b3) * c2
  ind_2v3_free_main  := (b4 - b3) * c4


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 +  (b3*c2)
  total_1v2_main  := a3 +  (b3*c4)
  total_1v3_micro := a2 +  (b4*c2)
  total_1v3_main  := a4 +  (b4*c4)
  total_2v3_micro := (a2-a1) + ((b4-b3)*c2)
  total_2v3_main  := (a4-a3)  + ((b4-b3)*c4)
  
'


free.fit <- lavaan::sem(free, data = mand_com, estimator = "ML", 
                            se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(free.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
lay <- get_layout("stateneedfreedom","","","Mainstream",
                  "stateneedsecurity","","","",
                  "","","","",
                  "tfreed","","","",
                  "tsecu","","","",
                  "","","","",
                  "","","","",
                  "finance","","","Micronarratives", rows = 8)
graph_sem(secu.fit, layout=lay)


library(dplyr)

est <- parameterEstimates(free.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######






####SEM with all three groups and both needs####

secufree <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  

  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*C1 + b2*C2
  stateneedfreedom~ b3*C1 + b4*C2
  
  secdiff_1v2  := b1
  secdiff_1v3  := b2
  secdiff_2v3  := b2 - b1
  
  freediff_1v2 := b3
  freediff_1v3 := b4
  freediff_2v3 := b4 - b3
  
    
  midiff_1v2   := a1
  midiff_1v3   := a2
  midiff_2v3   := a2 - a1
  
  madiff_1v2   := a3
  madiff_1v3   := a4
  madiff_2v3   := a4 - a3
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  # C2 (1v3) -> security -> outcomes
  ind_1v3_sec_micro := b2 * c1
  ind_1v3_sec_main  := b2 * c3

  # C2 (1v3) -> freedom -> outcomes
  ind_1v3_free_micro := b4 * c2
  ind_1v3_free_main  := b4 * c4

  # 2v3 derived -> security -> outcomes
  ind_2v3_sec_micro := (b2 - b1) * c1
  ind_2v3_sec_main  := (b2 - b1) * c3

  # 2v3 derived -> freedom -> outcomes
  ind_2v3_free_micro := (b4 - b3) * c2
  ind_2v3_free_main  := (b4 - b3) * c4


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)
  total_1v3_micro := a2 + (b2*c1) + (b4*c2)
  total_1v3_main  := a4 + (b2*c3) + (b4*c4)
  total_2v3_micro := (a2-a1) + ((b2-b1)*c1) + ((b4-b3)*c2)
  total_2v3_main  := (a4-a3) + ((b2-b1)*c3) + ((b4-b3)*c4)
  
'


secufree.fit <- lavaan::sem(secufree, data = mand_com, estimator = "ML", 
                se = "bootstrap",bootstrap = 5000L,parallel ="multicore", verbose=F)

secufree.ml.fit <- lavaan::sem(secufree, data = mand_com, estimator = "ML")



summary(secufree.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
lay <- get_layout("stateneedfreedom","","","Mainstream",
                  "stateneedsecurity","","","",
                  "","","","",
                  "tfreed","","","",
                  "tsecu","","","",
                  "","","","",
                  "","","","",
                  "finance","","","Micronarratives", rows = 8)
graph_sem(secu.fit, layout=lay)


library(dplyr)

est <- parameterEstimates(secufree.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()


params <- parameterEstimates(secufree.fit, ci = TRUE, standardized = T)|>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

params |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(lhs,op, rhs, est, se, CI, pvalue, std.all) |>
  dplyr::rename(Parameter = lhs, op= , Predictor =rhs, B = est, SE = se, `95% CI` = CI, p = pvalue, 
                `Std. B`= std.all) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######



vuongtest(free.fit, secufree.fit)
vuongtest(secu.fit, secufree.fit)
# model 1 (needs separated fits better for all (disable bootsrapping to check again)
#differences in fit are however minimal


####SEM with all three groups and trait ####


addon <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3


  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*C1 + b2*C2+tsecu+tfreed
  stateneedfreedom~ b3*C1 + b4*C2+tsecu+tfreed
  
  secdiff_1v2  := b1
  secdiff_1v3  := b2
  secdiff_2v3  := b2 - b1
  
  freediff_1v2 := b3
  freediff_1v3 := b4
  freediff_2v3 := b4 - b3
  
    
  midiff_1v2   := a1
  midiff_1v3   := a2
  midiff_2v3   := a2 - a1
  
  madiff_1v2   := a3
  madiff_1v3   := a4
  madiff_2v3   := a4 - a3
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  # C2 (1v3) -> security -> outcomes
  ind_1v3_sec_micro := b2 * c1
  ind_1v3_sec_main  := b2 * c3

  # C2 (1v3) -> freedom -> outcomes
  ind_1v3_free_micro := b4 * c2
  ind_1v3_free_main  := b4 * c4

  # 2v3 derived -> security -> outcomes
  ind_2v3_sec_micro := (b2 - b1) * c1
  ind_2v3_sec_main  := (b2 - b1) * c3

  # 2v3 derived -> freedom -> outcomes
  ind_2v3_free_micro := (b4 - b3) * c2
  ind_2v3_free_main  := (b4 - b3) * c4

  # --- Total indirect effects (both mediators) ---
  total_ind_1v2_micro := (b1*c1) + (b3*c2)
  total_ind_1v2_main  := (b1*c3) + (b3*c4)
  total_ind_1v3_micro := (b2*c1) + (b4*c2)
  total_ind_1v3_main  := (b2*c3) + (b4*c4)
  total_ind_2v3_micro := ((b2-b1)*c1) + ((b4-b3)*c2)
  total_ind_2v3_main  := ((b2-b1)*c3) + ((b4-b3)*c4)

  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)
  total_1v3_micro := a2 + (b2*c1) + (b4*c2)
  total_1v3_main  := a4 + (b2*c3) + (b4*c4)
  total_2v3_micro := (a2-a1) + ((b2-b1)*c1) + ((b4-b3)*c2)
  total_2v3_main  := (a4-a3) + ((b2-b1)*c3) + ((b4-b3)*c4)
  
'


addon.fit <- lavaan::sem(addon, data = mand_com, estimator = "ML", 
                            se = "bootstrap",bootstrap = 50L,
                         parallel ="multicore", verbose=F)

addon.ml.fit <- lavaan::sem(addon, data = mand_com, estimator = "ML")

summary(addon.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


library(dplyr)

est <- parameterEstimates(addon.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######


####SEM with all three groups and trait plus finance ####


addon1 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3
  finance=~ finan1+finan2+finan3


  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed+finance
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed+finance
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*C1 + b2*C2+tsecu+tfreed+finance
  stateneedfreedom~ b3*C1 + b4*C2+tsecu+tfreed+finance
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  # C2 (1v3) -> security -> outcomes
  ind_1v3_sec_micro := b2 * c1
  ind_1v3_sec_main  := b2 * c3

  # C2 (1v3) -> freedom -> outcomes
  ind_1v3_free_micro := b4 * c2
  ind_1v3_free_main  := b4 * c4

  # 2v3 derived -> security -> outcomes
  ind_2v3_sec_micro := (b2 - b1) * c1
  ind_2v3_sec_main  := (b2 - b1) * c3

  # 2v3 derived -> freedom -> outcomes
  ind_2v3_free_micro := (b4 - b3) * c2
  ind_2v3_free_main  := (b4 - b3) * c4


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)
  total_1v3_micro := a2 + (b2*c1) + (b4*c2)
  total_1v3_main  := a4 + (b2*c3) + (b4*c4)
  total_2v3_micro := (a2-a1) + ((b2-b1)*c1) + ((b4-b3)*c2)
  total_2v3_main  := (a4-a3) + ((b2-b1)*c3) + ((b4-b3)*c4)
  
'

addon1.fit <- lavaan::sem(addon1, data = mand_com, estimator = "ML", 
                         se = "bootstrap",bootstrap = 5000L,
                         parallel ="multicore", verbose=F
                         )
addon1.ml.fit <- lavaan::sem(addon1, data = mand_com, estimator = "ML")

summary(addon1.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


library(dplyr)

est <- parameterEstimates(addon1.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()


params <- parameterEstimates(addon1.fit, ci = TRUE, standardized = T)|>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

params |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(lhs,op, rhs, est, se, CI, pvalue, std.all) |>
  dplyr::rename(Parameter = lhs, op= , Predictor =rhs, B = est, SE = se, `95% CI` = CI, p = pvalue, 
                `Std. B`= std.all) |>
  flextable() |>
  bold(part = "header") |>
  autofit()











#######

vuongtest(secufree.ml.fit, addon.ml.fit)
vuongtest(secufree.ml.fit, addon1.ml.fit)




####SEM with 2 groups, both needs and trait ####


addon2 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3


  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*gr_2 +tsecu+tfreed
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*gr_2 +tsecu+tfreed
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*gr_2+tsecu+tfreed
  stateneedfreedom~ b3*gr_2+tsecu+tfreed
  
 
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4


  # --- Total indirect effects (both mediators) ---
  total_ind_1v2_micro := (b1*c1) + (b3*c2)
  total_ind_1v2_main  := (b1*c3) + (b3*c4)


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)
  
'


addon2.fit <- lavaan::sem(addon2, data = mand_com, estimator = "ML", 
                         se = "bootstrap",bootstrap = 50L,
                         parallel ="multicore", verbose=F)

addon2.ml.fit <- lavaan::sem(addon2, data = mand_com, estimator = "ML")

summary(addon2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


library(dplyr)

est <- parameterEstimates(addon2.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######


####SEM with 2 groups, both needs and traits finance ####


addon12 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3
  finance=~ finan1+finan2+finan3


  Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*gr_2+tsecu+tfreed+finance
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*gr_2+tsecu+tfreed+finance
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*gr_2+tsecu+tfreed+finance
  stateneedfreedom~ b3*gr_2+tsecu+tfreed+finance
  
  
  # --- Indirect effects ---
  # C1 (1v2) -> security -> outcomes
  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  # C1 (1v2) -> freedom -> outcomes
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

 
  # --- Total indirect effects (both mediators) ---
  total_ind_1v2_micro := (b1*c1) + (b3*c2)
  total_ind_1v2_main  := (b1*c3) + (b3*c4)


  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)

  
'

addon12.fit <- lavaan::sem(addon12, data = mand_com, estimator = "ML", 
                          se = "bootstrap",bootstrap = 5000L,
                          parallel ="multicore", verbose=F
)
addon12.ml.fit <- lavaan::sem(addon12, data = mand_com, estimator = "ML")

summary(addon12.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


library(dplyr)

est <- parameterEstimates(addon12.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


library(flextable)

est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######









# Create dummies (group 1 = reference)
mand$gr_a_d2 <- as.numeric(mand$gr_a == 2)
mand$gr_a_d3 <- as.numeric(mand$gr_a == 3)
mand$gender <- as.numeric(mand$gender)
# 1 vs 2
d_1v2 <- mand[mand$gr_a %in% c(1, 2), ]

process(data = d_1v2,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        
        center= 2,
        boot = 5000,
        seed = 123)



process(data = d_1v2 ,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
        model = 4,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)

# 1 vs 3 
d_1v3 <- mand[mand$gr_a %in% c(1, 3), ]

process(data = d_1v3,
        y = "micronarratives",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c( "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,

        total=1,
        center= 2,
        boot = 5000,
        seed = 123)

process(data = d_1v3 ,
        y = "micronarratives",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)

#2 vs 3
d_2v3 <- mand[mand$gr_a %in% c(2, 3), ]

process(data = d_2v3,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)


process(data = d_2v3,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c( "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)




#mainstream

#1v2

process(data = d_1v2 ,
               y = "mainstream",
               x = "gr_a_d2",
               m = c("sfree", "ssec"),
               cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
               model = 4,
               total=1,
               center= 2,
               boot = 5000,
               seed = 123)


process(data = d_1v2,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        total=1,
        
        model = 7,
        center= 2,
        boot = 5000,
        seed = 123
        )


#1v3
process(data = d_1v3 ,
        y = "mainstream",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)


process(data = d_1v3,
        y = "mainstream",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c( "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        total=1,
        
        model = 7,
        center= 2,
        boot = 5000,
        seed = 123)





#2v3
process(data = d_2v3,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)




process(data = d_2v3,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c( "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)





Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
Micronarratives=~narrative_1+narrative_3+narrative_5 

narintde.model <- '

Mainstream=~narrative_2+narrative_4 +narrative_6  +narrative_8  
Micronarratives=~narrative_5+narrative_3+narrative_7+narrative_1
Mainstream~~Micronarratives
	
narrative_4~~narrative_7
narrative_4~~narrative_5
narrative_2~~narrative_5
'

narintde.fit <- cfa(narintde.model, data = mand,  estimator = "ML", missing = "FIML")
summary(narintde.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
mi <- modificationindices(narintde.fit)
View(mi)















