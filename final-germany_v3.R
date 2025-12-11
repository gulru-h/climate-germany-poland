getwd()
renv::init()
renv::snapshot()

usethis::create_github_token(description = "climate-germany")
gitcreds::gitcreds_set() 
usethis::use_github()
usethis::gh_token_help()

#"X:/LS-KESSELS/ALLGEMEIN/Gülru/digipatch/klima/klima"
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
alpha(finan) #.95

tsec <- subset(clean[,c(21:23)])
clean$tsec <- apply(tsec, 1, mean, na.rm=T)
alpha(tsec) #.74 

tfree <- subset(clean[,c(24:26)])
clean$tfree <- apply(tfree, 1, mean, na.rm=T)
alpha(tfree) #.81 

ssec <- subset(clean[,c(27:29)])
clean$ssec <- apply(ssec, 1, mean, na.rm=T)
alpha(ssec)#.96

sfree <- subset(clean[,c(30:32)])
clean$sfree <- apply(sfree, 1, mean, na.rm=T)
alpha(sfree)#.97

man_acc <- subset(clean[,c(36,37)])
clean$man_acc <- apply(man_acc, 1, mean, na.rm=T)
alpha(man_acc) #.94

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

sd(mand$duration_m)
# 9.72077

#due to the age of the participants we decided to only remove the too quick 
#answers 
#sd and mean without outliers

fast <- mand[mand$duration_m <45, ]

summary(fast$duration_m)
sd(fast$duration_m)

low <- mean(fast$duration_m)-sd(fast$duration_m)

which(mand$duration_m <   5.593478)
mand <- mand[mand$duration_m >  5.593478, ]
# n = 480

summary(mand$duration_m)
ggplot(mand, aes(y = duration_m)) + geom_boxplot()


#creating a manipulation only group 

table(mand$c_0001)
#table(mand$gr) <- after renaming, ignore for now
#  1   2   3 
#173 156 151 
##########DO NOT TOUCH#######


#subsetting and building means 

micronarratives <- subset(mand[,c(38,40,42,44)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)
alpha(micronarratives) #.76

mainstream <- subset(mand[,c(39,41,43,45)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)
alpha(mainstream) #.81

#renaming things
mand<- rename(mand, gr = c_0001)
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

save(mand, file="mand.RData")

#done with cleaning plus data wrangling 

#gender
table(mand$gender)
#167 women 313 men

#ses
range(mand$SES, na.rm = T)
hist(mand$SES)
#normal-ish

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
cors <- subset(mand[,c(68, 69,66, 67,72,73, 35, 70)])

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

#in the manuscript the mandatory group is presented first for better 
#comparability with poland 

######

####analyses
##anova
#1- mandatory
#2- voluntary
#3 - control

mand$gr_a <- as.factor(mand$gr_a)

#use gr_a when calculating with mand and onlymanipulation

a <- aov(ssec ~ gr_a+tsec, data=mand)
summary(a)
aa <- aov(ssec ~ gr_a, data=mand)
summary(aa)

library(effectsize)
eta_squared(a)
eta_squared(aa)
TukeyHSD(aa)

library(apaTables)
apa.aov.table(a, "a.doc",
              conf.level = 0.95)

e <- aov(tsec ~ gr_a, data=mand)
summary(e)
eta_squared(e)

#only manipulation

b <- aov(ssec ~ gr_a+tsec, data=onlymanipulation)
summary(b)
bb <- aov(ssec ~ gr_a, data=onlymanipulation)
TukeyHSD(bb)
eta_squared(b)
cohens_d(ssec ~ gr_a, data=onlymanipulation) #-.13

apa.aov.table(b, "b.doc",,
              conf.level = 0.95,
              type=3)


#comparisons with control are significant

c <- aov(sfree ~ gr_a + tfree , data=mand)
summary(c)
eta_squared(c)
TukeyHSD(c)

cc <- aov(sfree ~ gr_a , data=mand)
summary(cc)
TukeyHSD(cc)

#only manipulation
d <- aov(sfree ~ gr_a+ tfree +finan, data=mand)
summary(d)
TukeyHSD(d)
dfree<- cohen.d(sfree ~ gr_a, data=onlymanipulation)
summary(dfree)

#make the grouping variable ordered
mand$c_0001 <- ordered(mand$c_0001, levels = c("1", "2", "3"))


#H2
microlm<- lm(micronarratives~gr_a, data=onlymanipulation)
summary(microlm)

tab_model(microlm,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,
          digits = 2,           
          p.style = "numeric")
effect_plot(microlm, pred = gr_a, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)
microaov<- aov(micronarratives~gr_a, data=mand)
summary(microaov)
TukeyHSD(microaov)
effectsize(microaov)


###SEM Time ####
library(lavaan)
forsem <- onlymanipulation

which(is.na(forsem$c_0001))
#no missings

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
                se = "bootstrap",bootstrap = 5000L,parallel ="multicore", verbose=F)

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





################ ignore
####(exploratory) 
#H3:The strength of the relationship between trust in 
#institutions and perceived threat to security will vary by experimental group.######

tma<- lm(mainstream~trust5, data=onlymanipulation)
summary(tma)
standardise(tma)

library(sjPlot)
tab_model(tma,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,
          digits = 3,           
          p.style = "numeric")

tmi<- lm(micronarratives~trust5, data=onlymanipulation)
summary(tmi)
standardise(tmi)

tab_model(tmi,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")

#will be added to the SEM


#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m3 <- lm(micronarratives~ ssec*med_act+tsec+finan, data=onlymanipulation)
summary(m3)
standardise(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()
tab_model(m3,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")

m3.1 <- lm(mainstream~ ssec*med_act+tsec+finan, data=onlymanipulation)
summary(m3.1)
ggpredict(m3.1, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

tab_model(m3.1,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")


m4 <- lm(micronarratives~ sfree*med_act+tfree+finan, data=onlymanipulation)
summary(m4)
standardise(m4)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()

tab_model(m4,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")

m4.1 <- lm(mainstream~ sfree*med_act+tfree+finan, data=onlymanipulation)
summary(m4.1)
standardise(m4.1)
ggpredict(m3.1, terms = c("ssec[1:7 by=0.1]", "med_act[1:6 by=2]")) |> plot()
tab_model(m4.1,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 3,           
          p.style = "numeric")
#no active media use effects 

#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr_a, data=onlymanipulation)
eta_squared(a)
summary(a)
TukeyHSD(a)
#similar

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr_a, data=onlymanipulation)
summary(b)
TukeyHSD(b)
eta_squared(b)

#mandatory less effective
#############
o <- aov(micronarratives ~ tsec , data=mand)
summary(o)
