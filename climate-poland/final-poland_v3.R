getwd()

renv::init()
renv::snapshot()

#usethis::use_github()
usethis::gh_token_help()
gitcreds::gitcreds_set()

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

aapl <- read_sav("corrected_dataset_preliminary_cleaned.sav")
rawpl <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";", dec = ".")

cleanpl <- rawpl[,c(23:214 )]
names(cleanpl)

########data cleaning######
#combine variables
#finance manipulation: single item
man_financepl <- subset(cleanpl[,c(60,65)])
cleanpl$man_finance <- apply(man_financepl, 1, sum, na.rm=T)

#efficacy: single item
class(cleanpl$effectiveness_exp)
cleanpl$effectiveness_exp <- as.numeric(cleanpl$effectiveness_exp)
man_effpl <- subset(cleanpl[,c(61,66)])
cleanpl$man_eff <- apply(man_effpl, 1, sum, na.rm=T)
mean(cleanpl$man_eff, na.rm=T)

#acceptability
man_accpl <- subset(cleanpl[,c(63,68)])
cleanpl$man_acc <- apply(man_accpl, 1, sum, na.rm=T)
mean(cleanpl$man_acc, na.rm=T)



#attention check
attpl <- subset(cleanpl[,c(59,64)])
cleanpl$att <- apply(attpl, 1, sum, na.rm=T)

cleanpl$att_check_exp<- as.numeric(cleanpl$att_check_exp)
table(cleanpl$att_check_exp)
table(cleanpl$attention_control) # all g


table(cleanpl$realist_merged< 2)
table(cleanpl$credib_merged< 2) # all credible and believable 



####build means####
cleanpl <- cleanpl %>%
  mutate(across(where(is.character), ~ {
    numeric_vals <- suppressWarnings(as.numeric(.))
    if (mean(!is.na(numeric_vals), na.rm = TRUE) > 0.9) numeric_vals else .
  }))

class(cleanpl$financial_sit_3)
finanpl <- subset(cleanpl[,c(31:33)])
cleanpl$finan <- apply(finanpl, 1, mean, na.rm=T)
alpha(finanpl)

tsecpl <- subset(cleanpl[,c(34:36)])
cleanpl$tsec <- apply(tsecpl, 1, mean, na.rm=T)
alpha(tsecpl)

#last three security items are actually freedom
tfreepl <- subset(cleanpl[,c(37:39)])
cleanpl$tfree <- apply(tfreepl, 1, mean, na.rm=T)
alpha(tfreepl)

ssecpl <- subset(cleanpl[,c(52:55)])
cleanpl$ssec <- apply(ssecpl, 1, mean, na.rm=T)
alpha(ssecpl)
class(cleanpl$security_freedom_4)

sfreepl <- subset(cleanpl[,c(56:58)])
cleanpl$sfree <- apply(sfreepl, 1, mean, na.rm=T)
alpha(sfreepl)



cleanpl$finan[cleanpl$finan == "1888"]<- NA

mandpl <- cleanpl


##narratives####

#subsetting and building means 

micronarrativespl <- subset(mandpl[,c(69,71,73,75)])
mandpl$micronarratives <- apply(micronarrativespl, 1, mean, na.rm=T)
alpha(micronarratives)

mainstreampl <- subset(mandpl[,c(188:191)])
mandpl$mainstream <- apply(mainstreampl, 1, mean, na.rm=T)
alpha(mainstream)

micronarratives_agpl <- subset(mandpl[,c(70,72,74,76)])
mandpl$micronarratives_ag <- apply(micronarratives_agpl, 1, mean, na.rm=T)
alpha(micronarratives_agpl)

mainstream_agpl <- subset(mandpl[,c(78,80,82,84)])
mandpl$mainstream_ag <- apply(mainstream_agpl, 1, mean, na.rm=T)
alpha(mainstream_agpl)

narrativespl <- data.frame(micronarrativespl,micronarratives_agpl, mainstreampl, mainstream_agpl)
narcors<-cor(narrativespl, use= "pairwise.complete.obs")
#acceptability assessed with one item

#save manipulation indicator as a factor
class(cleanpl$gr)
cleanpl$gr <- as.factor(cleanpl$gr)
#1 control
#2 voluntary
#3 obligatory
table(mandpl$gr)


#create a manipulation only group without the control group
mandpl <- mandpl[,(c(2:16, 28:39, 52:58, 69:75, 78,80,82,84,
                     143, 145:150, 159,188:191, 193:204))]
mandpl$active_soc_media[mandpl$active_soc_media == "-2"]<- NA

rm(onlymanipulationpl)
onlymanipulationpl <- mandpl[mandpl$gr ==1|mandpl$gr ==2 , ]

contr <- mandpl[mandpl$gr ==3, ]
vol<- mandpl[mandpl$gr ==2, ]
mandat<- mandpl[mandpl$gr ==1, ]

#create the two levels 
onlymanipulationpl$gr <- ordered(onlymanipulationpl$gr, levels = c("1", "2"))

#install.packages("fastDummies")

# Load the library
library(fastDummies)

# Create dummy variable
onlymanipulationpl <- fastDummies::dummy_cols(onlymanipulationpl, 
                                              select_columns = "gr")
mandpl <- dummy_cols(mandpl, select_columns = "gr")

#
#NOT USED IN THE MANUSCRIPT: creating sums of social media variables ####
microlikes <- subset(forsem[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])
microlikes1 <- subset(mandpl[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])

forsem$microlikes <- apply(microlikes, 1, sum, na.rm=T)
mandpl$microlikes <- apply(microlikes1, 1, sum, na.rm=T)
alpha(microlikes1)

mainstreamlikes <- subset(forsem[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
mainstreamlikes1 <- subset(mandpl[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
alpha(mainstreamlikes1)

forsem$mainstreamlikes <- apply(mainstreamlikes, 1, sum, na.rm=T)
mandpl$mainstreamlikes <- apply(mainstreamlikes1, 1, sum, na.rm=T)

microdislikes <- subset(forsem[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
forsem$microdislikes <- apply(microdislikes, 1, sum, na.rm=T)
microdislikes1 <- subset(mandpl[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
mandpl$microdislikes <- apply(microdislikes1, 1, sum, na.rm=T)
alpha(microdislikes1)

mainstreamdislikes <- subset(forsem[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
forsem$mainstreamdislikes <- apply(mainstreamdislikes, 1, sum, na.rm=T)
mainstreamdislikes1 <- subset(mandpl[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
mandpl$mainstreamdislikes <- apply(mainstreamdislikes1, 1, sum, na.rm=T)
alpha(mainstreamdislikes1)

microshares <- subset(forsem[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
microshares1 <- subset(mandpl[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
forsem$microshares <- apply(microshares, 1, sum, na.rm=T)
mandpl$microshares <- apply(microshares1, 1, sum, na.rm=T)

mainstreamshares <- subset(forsem[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])
mainstreamshares1 <- subset(mandpl[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])

forsem$mainstreamshares <- apply(mainstreamshares, 1, sum, na.rm=T)
mandpl$mainstreamshares <- apply(mainstreamshares1, 1, sum, na.rm=T)

microcomment <- subset(forsem[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])
microcomment1 <- subset(mandpl[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])

forsem$microcomment <- apply(microcomment, 1, sum, na.rm=T)
mandpl$microcomment <- apply(microcomment1, 1, sum, na.rm=T)

mainstreamcomment <- subset(forsem[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])
mainstreamcomment1 <- subset(mandpl[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])

forsem$mainstreamcomment <- apply(mainstreamcomment, 1, sum, na.rm=T)
mandpl$mainstreamcomment <- apply(mainstreamcomment1, 1, sum, na.rm=T)

microhash <- subset(forsem[,c("q1_sm_5","q2_sm_5","q3_sm_5","q4_sm_5")])
forsem$microhash <- apply(microhash, 1, sum, na.rm=T)

mainstreamhash <- subset(forsem[,c("q5_sm_5","q6_sm_5","q7_sm_5","q8_sm_5")])
forsem$mainstreamhash <- apply(mainstreamhash, 1, sum, na.rm=T)

######


mandpl <- mandpl %>% rename(
  med_act = active_soc_media,
  med_pas =passive_soc_media,
  trust1 = institution_trust_1,
  trust2 = institution_trust_2,
  trust3 = institution_trust_3,
  trust4 = institution_trust_4,
  trust5 = institution_trust_5,
  SES = sss_1,
  age = age_5,
  EU= q5_eu_attitudes_1,
  size_region=mzam,
  region=woj,
  ssec1=security_freedom_1,
  ssec2=security_freedom_2,
  ssec3=security_freedom_3,
  ssec4=security_freedom_4,
  sfree1= security_freedom_5,
  sfree2= security_freedom_6,
  sfree3= security_freedom_7,
  tsec1 =security_1,
  tsec2 =security_2,
  tsec3 =security_3,
  tfree1 = security_4,
  tfree2 = security_5,
  tfree3 = security_6
)

corspl <- subset(mandpl[,c("ssec", "tsec", "sfree", "tfree",
                           "finan", "SES", "man_finance","EU",
                           "trust5", 
                           "med_act", "med_pas",
                           "micronarratives", "mainstream",
                           "micronarratives_ag", "mainstream_ag")])
cors1pl <- corspl[mandpl$gr==1, ]
cors2pl <- corspl[mandpl$gr==2, ]
cors3pl <- corspl[mandpl$gr==3, ]

save_correlation_matrix(cors1pl,
                        filename = "cors1.csv",
                        digits= 2, use="lower") #mandatory

save_correlation_matrix(cors2pl,
                        filename = "cors2.csv",
                        digits= 2, use="lower") #voluntary

save_correlation_matrix(cors3pl,
                        filename = "cors3.csv",
                        digits= 2, use="lower") #control

meancors1pl <- data.frame(round(sapply(cors1pl, mean, na.rm=T), digits= 1)) #mandatory
meancors2pl <- data.frame(round(sapply(cors2pl, mean, na.rm=T), digits= 1)) #voluntary
meancors3pl <- data.frame(round(sapply(cors3pl, mean, na.rm=T), digits= 1)) #control


sdcors1pl <- data.frame(round(sapply(cors1pl, sd, na.rm=T), digits= 1)) #mandatory
sdcors2pl <- data.frame(round(sapply(cors2pl, sd, na.rm=T), digits= 1)) #voluntary
sdcors3pl <- data.frame(round(sapply(cors3pl, sd, na.rm=T), digits= 1)) #control


#CAN WE TELL THE NEEDS APART

needs <- 'SEC=~ ssec1+ssec2+ssec3
          FREE=~ sfree1+sfree2+sfree3
          TSEC=~ tsec1+tsec2+tsec3
          TFREE=~ tfree1+tfree2+tfree3
          SEC~~FREE
          TFREE~~TSEC
          TSEC~~SEC
          TSEC~~FREE
          TFREE~~ SEC
          TFREE~~ FREE

'

needspl.fit<- cfa(needs, data = mandpl, estimator = "ML")
summary(needspl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


needs1 <- 'COMBO=~ ssec1+ssec2+ssec3+sfree1+sfree2+sfree3
            TSEC=~ tsec1+tsec2+tsec3
            TFREE=~tfree1+tfree2+tfree3
           COMBO~~TSEC
           COMBO~~TFREE

'

needs1pl.fit<- cfa(needs1, data = mandpl, estimator = "ML")
summary(needs1pl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
#the fit of the combination model is awful also for poland 
mi<- modificationindices(needs1.fit)

anova(needspl.fit, needs1pl.fit)


mandpl$ssec_c <- scale(mandpl$ssec, center=TRUE, scale=FALSE)[,1]
mandpl$tsec_c <- scale(mandpl$tsec, center=TRUE, scale=FALSE)[,1]
mandpl$sfree_c <- scale(mandpl$sfree, center=TRUE, scale=FALSE)[,1]
mandpl$tfree_c <- scale(mandpl$tfree, center=TRUE, scale=FALSE)[,1]
mandpl$trust5_c <- scale(mandpl$trust5, center=TRUE, scale=FALSE)[,1]
mandpl$finan_c <- scale(mandpl$finan, center=TRUE, scale=FALSE)[,1]
mandpl$ssec_c <- scale(mandpl$ssec, center=TRUE, scale=FALSE)[,1]
mandpl$ssec_c <- scale(mandpl$ssec, center=TRUE, scale=FALSE)[,1]
mandpl$micronarratives_c <- scale(mandpl$micronarratives, center=TRUE, scale=FALSE)[,1]
mandpl$micronarratives_ag_c <- scale(mandpl$micronarratives_ag, center=TRUE, scale=FALSE)[,1]

mandpl$ssec_s <- scale(mandpl$ssec, center=TRUE, scale=T)[,1]
mandpl$tsec_s <- scale(mandpl$tsec, center=TRUE, scale=T)[,1]
mandpl$sfree_s <- scale(mandpl$sfree, center=TRUE, scale=T)[,1]
mandpl$tfree_s <- scale(mandpl$tfree, center=TRUE, scale=T)[,1]

mandpl$trust5_s <- scale(mandpl$trust5, center=TRUE, scale=T)[,1]
mandpl$finan_s <- scale(mandpl$finan, center=TRUE, scale=T)[,1]
mandpl$med_act_s <- scale(mandpl$med_act, center=TRUE, scale=T)[,1]
mandpl$med_pas_s <- scale(mandpl$med_pas, center=TRUE, scale=T)[,1]
mandpl$media_daily_1_s <- scale(mandpl$media_daily_1, center=TRUE, scale=T)[,1]


#H1: Participants in the mandatory condition will perceive a higher threat for 
#financial security in comparison to those in the voluntary and control conditions. 

#3 group
mandpl$gr_a <- as.factor(mandpl$gr)
mandpl$gender <- as.factor(mandpl$gender)

onlymanipulationpl$gr <- as.factor(onlymanipulationpl$gr)

library(knitr)
a <- aov(ssec ~ gr+tsec, data=mandpl)
summary(a)
library(effectsize)
eta_squared(a)

aa <- aov(ssec ~ gr, data=mandpl)
summary(aa)
TukeyHSD(aa)
aatab <- apa.aov.table(aa) 


#only manipulation
b <- aov(ssec ~ gr+tsec+finan, data=onlymanipulationpl)
summary(b)
bb <- aov(ssec ~ gr, data=onlymanipulationpl)
TukeyHSD(bb)

o<-aov(tsec~gr, data=mandpl)
summary(o)

#comparisons with control and between the experimental conditions
#are significant

#3 group
c <- aov(sfree ~ gr+tfree, data=mandpl)
summary(c)
eta_squared(c)
ctab <- apa.aov.table(c) 

cc <- aov(sfree ~ gr, data=mandpl)
TukeyHSD(cc)
cctab <- apa.aov.table(cc) 

#only manipulation
d <- aov(sfree ~ gr, data=onlymanipulationpl)
summary(d)
TukeyHSD(d)

#NEW: FINAN




n1p <- lm(ssec~ gr_a, data=mandpl)
summary(n1p)
pairs(emmeans(n1p, ~gr_a), adjust= "BH")
car::Anova(n1p)



n3p<- lm(ssec~ gr_a*(sfree+tsec+finan+tfree)+trust5+med_act+meduse_1+meduse_2+meduse_3
         +gender+age, data=mandpl)
summary(n3p)
ggpredict(n3p, terms = c("finan","gr_a")) |> plot()

n4p<- lm(sfree~ gr_a*(ssec+tsec+finan+tfree)+trust5+med_act+meduse_1+meduse_2+meduse_3
         +gender+age, data=mand)
summary(n4)
#come back to those two later

n5p <- lm(ssec~ gr_a+tsec+sfree+finan+tfree+man_finance+SES+gender+age, data=mandpl)
summary(n5p)
#group belonging does not predict much...
n55p <- lm(ssec~ gr_a+tsec+sfree+finan+tfree+gender+age, data=mandpl)
summary(n55p)


n6p <- lm(ssec~ gr_a*(tsec+sfree+finan+tfree+man_finance+SES)+gender+age, data=mandpl)
summary(n6p)
vif(n6p, type = "predictor")


n666p <- lm(ssec~ gr_a*(sfree_s+finan_s)+tfree_s+tsec_s+man_finance_s+SES_s+gender+age, data=mandpl)
summary(n666p)
anova(n666p, n6p) # no change
ggpredict(n666p, terms = c("finan_s","gr_a")) |> plot()
##use that one

ggpredict(n6p, terms = c("finan","gr_a")) |> plot()
ggpredict(n6, terms = c("man_finan","gr_a")) |> plot()

n7p <- lm(ssec~ gr_a*(tsec+sfree+finan+tfree+SES)+gender+age, data=mandpl)
summary(n7p)
anova(n666p, n7p) # no change

n8p <- lm(ssec~ gr_a*(tsec_c+sfree_c+finan_c+tfree_c)+gender+age, data=mandpl)
summary(n8p)
ggpredict(n8p, terms = c("finan_c","gr_a")) |> plot()


n9p <- lm(ssec~ gr_a*(tsec_c+finan_c)+gender+age, data=mandpl)
summary(n9p)
ggpredict(n9p, terms = c("finan_c","gr_a")) |> plot()



n10p <- lm(sfree~ gr_a*(tsec+ssec+finan+tfree+man_finance+SES)+gender+age, data=mandpl)
summary(n10)
ggpredict(n10p, terms = c("finan","gr_a")) |> plot()
vif(n10p, type = "predictor")
anova(n10p)




n11p <- lm(sfree~ gr_a*(tsec_c+ssec_c+finan_c+tfree_c)+gender+age, data=mandpl)
summary(n11p)
ggpredict(n11p, terms = c("finan_c","gr_a")) |> plot()

vif(n11p, type = "predictor")

n12p <- lm(sfree~ gr_a*(tsec_c+finan_c+tfree_c)+gender+age, data=mandpl)
summary(n12p)
ggpredict(n12p, terms = c("finan_c","gr_a")) |> plot()

n111p <- lm(sfree~ gr_a*(ssec_s+finan_s)+tfree_s+tsec_s+man_finance_s+SES_s+gender+age, data=mandpl)
summary(n111p)



n1frp <- lm(sfree~ gr_a, data=mandpl)
summary(n1frp)
pairs(emmeans(n1frp, ~gr_a), adjust= "BH")
car::Anova(n1frp)



#FUNKY NEW ANALYSES


f1p<- lm(ssec~ gr_a*(sfree+finan+trust5+med_act+media_daily_1)+tsec+tfree+gender+age, data=mandpl)
summary(f1p)
ggpredict(f1, terms = c("finan","gr_a")) |> plot()
anova(f1,n8) # no added value

f11p<- lm(ssec~ gr_a*(sfree_s+finan_s+trust5_s+med_act_s+media_daily_1_s)+tsec_s+tfree_s+gender+age, 
          data=mandpl)
summary(f11p)
ggpredict(f1p, terms = c("finan","gr_a")) |> plot()
anova(f11,n8p) # no added value

f2p<- lm(sfree~gr_a*(ssec+finan+trust5+med_act+media_daily_1)+tsec+tfree+gender+age,
         data=mandpl)
summary(f2p)
ggpredict(f2, terms = c("finan","gr_a")) |> plot()
anova(f2,n11p) 

f22p<- lm(sfree~gr_a*(ssec_s+finan_s+trust5_s+med_act_s+media_daily_1_s)+tsec_s+tfree_s+gender+age,
          data=mandpl)
summary(f22p)
anova(f22p,n11p) 
ggpredict(f22p, terms = c("finan_s","gr_a")) |> plot()






#### Predicting micronarratives with needs####

#APPROACH
m00pc<- lm(micronarratives~ age+gender+
             gr_a,
           data=mandpl) 
summary(m00pc)
pairs(emmeans(m00pc, ~gr_a))
#no differences in approach by group


m1pc<- lm(micronarratives~ age+gender+
             gr_a*(ssec_c+sfree_c)+tsec_c+tfree_c,
           data=mandpl) 
summary(m1pc) #BEST MODEL SO FAR
pairs(emmeans(m1pc, ~gr_a))
anova(m0pc, m1pc)

m11pc<- lm(micronarratives~ age+gender+gr_a+
            ssec_c+sfree_c+tsec_c+tfree_c,
          data=mandpl) 
summary(m11pc)
anova(m1pc, m11pc) #no difference

#let needs interact

m2pc<- lm(micronarratives~ age+gender+
            gr_a*(ssec_c*sfree_c)
            +tsec_c+tfree_c
          ,data=mandpl)
summary(m2pc)
anova(m1pc, m2pc) #no difference

m22pc<- lm(micronarratives~ age+gender+
            gr_a+(ssec_c*sfree_c)
          +tsec_c+tfree_c
          ,data=mandpl)
summary(m22pc)
anova(m11pc, m22pc)
#no difference 

#interact with trait needs


m3pc<- lm(micronarratives~ age+gender+
            (ssec_c+sfree_c)*(gr_a+tsec_c+tfree_c)
          ,data=mandpl)
summary(m3pc)

anova(m1pc,m3pc)
anova(m2pc,m3pc)#no difference

##ALL MODELS EQUALLY MEDIOCRE
#use m1pc 



mimi<- lm(micronarratives~ age+gender+micronarratives_ag_c+
            gr_a*(ssec_c+sfree_c)+tsec_c+tfree_c,
          data=mandpl) 
summary(mimi) #agreement is the best predictor of approach



#AGREEMENT
miag0pc<- lm(micronarratives_ag~ age+gender+
             gr_a,
           data=mandpl) 
summary(miag0pc)
pairs(emmeans(miag0pc, ~gr_a))
#no differences in agreement by group

miag11pc<- lm(micronarratives_ag~ age+gender+gr_a+
             ssec_c+sfree_c+tsec_c+tfree_c,
           data=mandpl) 
summary(miag11pc)
anova(miag0pc, miag11pc)# better fit

miag1pc<- lm(micronarratives_ag~ age+gender+
            gr_a*(ssec_c+sfree_c)+tsec_c+tfree_c,
          data=mandpl) 
summary(miag1pc) 
pairs(emmeans(miag1pc, ~gr_a))
anova(miag11pc, miag1pc) # better fit

miag2pc<- lm(micronarratives_ag~ age+gender+
            gr_a*(ssec_c*sfree_c)
          +tsec_c+tfree_c
          ,data=mandpl)
summary(miag2pc)
anova(miag1pc, miag2pc) #no difference


#interact with trait needs
miag3pc<- lm(micronarratives_ag~ age+gender+
            (ssec_c+sfree_c)*(gr_a+tsec_c+tfree_c)
          ,data=mandpl)
summary(miag3pc)

anova(miag1pc,miag3pc)#no difference

#use miag1pc 






####Adding in some new variables#####

microlm0p<- lm(micronarratives~age+gender+
                 gr_a+ssec_c+sfree_c+finan_c+tsec_c+tfree_c,
               data=mandpl)
summary(microlm0p)
vif(microlm0p) # it looks okay

microlm1p<- lm(micronarratives~age+gender+
                 gr_a*(ssec_c+sfree_c)+
                 finan_c+tsec_c+tfree_c,
               data=mandpl)
summary(microlm1p) #finan, tfree, gender significant
car::Anova(microlm1p)
anova(microlm0p, microlm1p) #no difference
anova(m1pc, microlm1p) #finan adds value overall



microlm22p<- lm(micronarratives~
                  gr_a*finan_c+sfree_c+ssec_c+tsec_c+tfree_c+age+gender,
                data=mandpl)
summary(microlm22p)
anova(microlm0p, microlm22p) #BETTER FIT 



microlm2p<- lm(micronarratives~
                 gr_a*finan_c+(ssec_c+sfree_c)*finan_c+tsec_c+tfree_c+age+gender,
               data=mandpl)
summary(microlm2p) #tfree, gender significant

anova(microlm22p, microlm2p)# no difference
#gr interaction remains sufficient, interaction with needs adds no value

microlm20p<- lm(micronarratives~
                 gr_a+(ssec_c+sfree_c)*finan_c+tsec_c+tfree_c+age+gender,
               data=mandpl)
summary(microlm20p)
anova(microlm2p,microlm20p)
#gr interaction remains necessary
#microlm22p remains the best model 

microlmp_end<- lm(micronarratives~
                  gr_a*(finan_c+sfree_c+ssec_c)+tsec_c+tfree_c+age+gender,
                data=mandpl)
summary(microlmp_end)

anova(microlmp_end, microlm22p) #no difference
anova(microlm1p,microlmp_end) #end fits best

#no differences 

emtrends(microlmp_end, ~ gr_a, var="finan_c")


ggpredict(microlmp_end, terms = c("finan_c", "gr_a")) |> plot()
#groups 1 and 3: the better the financial situation the more they tended 
#towards the micronarratives
#use lmp_end

mimifinan1<- lm(micronarratives~
                    gr_a*(finan_c+sfree_c+ssec_c)+micronarratives_ag_c+tsec_c+tfree_c+age+gender,
                  data=mandpl)

summary(mimifinan1)
anova(mimi, mimifinan1) # finan is still useful!
ggpredict(mimifinan1, terms = c("finan_c","gr_a")) |> plot()
ggpredict(mimifinan1, terms = c("finan_c","micronarratives_ag_c","gr_a")) |> plot()

mimifinan2<- lm(micronarratives~
                  gr_a*(sfree_c+ssec_c+finan_c*micronarratives_ag_c)+tsec_c+tfree_c+age+gender,
                data=mandpl)

summary(mimifinan2)
anova(mimifinan1, mimifinan2) #no diff

ggpredict(mimifinan2, terms = c("finan_c", "micronarratives_ag_c","gr_a")) |> plot()

sd_mna <- sd(mandpl$micronarratives_ag_c, na.rm = TRUE)


emtrends(mimifinan2, 
                   pairwise ~ gr_a, 
                   var = "finan_c",
                   at = list(micronarratives_ag_c = c(-sd_mna, 0, sd_mna)))

emtrends(mimifinan2, 
         ~ gr_a | micronarratives_ag_c, 
         var = "finan_c",
         at = list(micronarratives_ag_c = c(-sd_mna, 0, sd_mna)))
#if agreement is already high, they approach (regardless of financial situation)
#average or low agreement leads to a more pronounced relationship between financial situation and micronarrative approach


##agreement
miag0<- lm(micronarratives_ag~age+gender+
                 gr_a+ssec_c+sfree_c+finan_c+tsec_c+tfree_c,
               data=mandpl)
summary(microlm0p)
vif(microlm0p) # it looks okay




car::vif(microlmp_end, type="predictor")
car::Anova(microlmp_end)

##ACTIVE MEDIA USE

me1<- lm(micronarratives~
           gr_a*(ssec_s+sfree_s+finan_c)+med_act_s+
           finan_s+tsec_s+tfree_s
         +age+gender,
         data=mandpl)
summary(me1) 

ggpredict(me1, terms = c("med_act_s[meansd]", "gr_a")) |> plot()

anova(microlmp_end, me1) #sign

me2<- lm(micronarratives~
                gr_a*(ssec_s+sfree_s)+(ssec_s+sfree_s)*med_act_s+finan_s+tsec_s+tfree_s
              +age+gender,
              data=mandpl)
summary(me2) 
anova(microlmp_end, me2)# not sign

me2<- lm(micronarratives~
           gr_a*(ssec_s+sfree_s+finan_c)+(ssec_s+sfree_s)*med_act_s+tsec_s+tfree_s
         +age+gender,
         data=mandpl)
summary(me2) 
anova(me1,me2)# non sign, interaction not necessary

me3<- lm(micronarratives~
           gr_a*(ssec_s+sfree_s+finan_c+med_act_s)+tsec_s+tfree_s
         +age+gender,
         data=mandpl)
anova(me1,me2) # non sign


#ADDING TRUST ####
microtr1p<- lm(micronarratives~
                  gr_a+ssec+sfree+finan_c+trust5+tsec_c+tfree_c+age+gender,
                data=mandpl)
summary(microtr1p)
#no effect


micro4p<- lm(micronarratives~
                 gr_a*(ssec_c+sfree_c+finan_c+trust5_c)+tsec_c+tfree_c
               +age+gender,
               data=mandpl)
summary(micro4p) 
anova(microtr1p, micro4p) # no diff
#####




#TRUST AS OUTCOME ####

t0<- lm(trust5 ~ gr_a+age+gender, data=mandpl)
summary(t0)
pairs(emmeans(t0, ~gr_a))
car::Anova(t0)
#no diff

t1<- lm(trust5 ~ gr_a+ssec_c+sfree_c+age+gender, data=mandpl)
summary(t1)
car::Anova(t1)
pairs(emmeans(t1, ~gr_a))
anova(t0,t1) #better
#needs at mean: contrast with control significant
t11<- lm(trust5 ~ age+gender+
           gr_a+ssec_c+sfree_c+tfree_c+tsec_c+finan_c, data=mandpl)
summary(t11)


t2<- lm(trust5 ~ gr_a*(ssec_c+sfree_c)+tfree_c+tsec_c+finan_c+age+gender, data=mandpl) # best fit
summary(t2)
car::Anova(t2) # group, age and sfree sign.
ggpredict(t2, terms = c("ssec_c", "gr_a")) |> plot()
anova(t11,t2) # very small difference

t22<- lm(trust5 ~ gr_a*(ssec_c+sfree_c+finan_c)+tfree_c+tsec_c+age+gender, data=mandpl) # best fit
summary(t22)
anova(t2,t22)#no difference
AIC(t2, t22)
BIC(t2, t22) # use t2

t3<- lm(trust5 ~ (gr_a+tsec_c+tfree_c)*(ssec_c+sfree_c)+finan_c+age+gender, data=mandpl) 
summary(t3)
car::Anova(t3)
anova(t2,t3) # t3 best
AIC(t2,t3)
BIC(t2,t3) #t2 better
pairs(emmeans(t4, ~gr_a))


#adding in narratives to predict trust 
#approach

t4 <- lm(trust5 ~ gr_a*(ssec_c+sfree_c)+finan_c+tsec_c+tfree_c+micronarratives+age+gender, data=mandpl) 
summary(t4) # no diff
anova(t2,t4) # no diff
car::vif(t4, type= "predictor")

t5 <- lm(trust5 ~ gr_a*(ssec_c+sfree_c+micronarratives_c)+finan_c+tsec_c+tfree_c+age+gender, data=mandpl) 
summary(t5) # no diff
anova(t4,t5) # no diff

t6 <- lm(trust5 ~ gr_a*(ssec_c+sfree_c)+micronarratives_c*(ssec_c+sfree_c)+tfree_c+tsec_c+finan_c+age+gender, data=mandpl) 
summary(t6)
anova(t2,t6) #no diff


t7 <- lm(trust5 ~ gr_a*(ssec_c+sfree_c)+finan_c+tsec_c+tfree_c+micronarratives_ag+age+gender, data=mandpl) 
summary(t7)
anova(t2,t7)#best one so far


        
#trait
tr1<- lm(tsec~ gr_a + age + gender, data=mandpl)
car::Anova(tr1)
tr2<- lm(tfree~ gr_a + age + gender, data=mandpl)
car::Anova(tr2)
#no group differences
#####
           
######



mfp<- lm(man_finance~ gr_a, data=mandpl)
summary(mfp)
pairs(emmeans(mfp, ~gr_a), adjust="BH")











#approach towards narratives by group

microint<- lm(micronarratives~gr, data=onlymanipulationpl)
summary(microint)

microag<- lm(micronarratives_ag~gr, data=onlymanipulationpl)
summary(microag)






mf<- lm(man_finance~ gr_a, data=mand)
summary(mf)
pairs(emmeans(mf, ~gr_a), adjust="BH")











#recovered






'# --- Indirect effects ---
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
#total_ind_1v2_micro := (b1*c1) + (b3*c2)
#total_ind_1v2_main  := (b1*c3) + (b3*c4)
#total_ind_1v3_micro := (b2*c1) + (b4*c2)
#total_ind_1v3_main  := (b2*c3) + (b4*c4)
#total_ind_2v3_micro := ((b2-b1)*c1) + ((b4-b3)*c2)
#total_ind_2v3_main  := ((b2-b1)*c3) + ((b4-b3)*c4)
# --- Total effects (direct + total indirect) ---
total_1v2_micro := a1 + (b1*c1) + (b3*c2)
total_1v2_main  := a3 + (b1*c3) + (b3*c4)
total_1v3_micro := a2 + (b2*c1) + (b4*c2)
total_1v3_main  := a4 + (b2*c3) + (b4*c4)
total_2v3_micro := (a2-a1) + ((b2-b1)*c1) + ((b4-b3)*c2)
total_2v3_main  := (a4-a3) + ((b2-b1)*c3) + ((b4-b3)*c4)
'
psecufreeag.fit <- lavaan::sem(psecufreeag, data = mandpl_com, estimator = "ML",
                               se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)
summary(psecufreeag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



psecufreeag1 <- '
stateneedsecurity =~ ssec1+ssec2+ssec3
stateneedfreedom=~sfree1+sfree2+sfree3
AGMainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end
AGMicronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end
AGMicronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2
AGMainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2
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
psecufreeag1.fit <- lavaan::sem(psecufreeag1, data = mandpl_com, estimator = "ML",
                                se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)
summary(psecufreeag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
est <- parameterEstimates(psecufreeag1.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()
paddon <- '
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
paddon.fit <- lavaan::sem(paddon, data = mandpl_com, estimator = "ML",
                          se = "bootstrap",bootstrap = 50L,
                          parallel ="multicore", verbose=F)
paddon <- '
stateneedsecurity =~ ssec1+ssec2+ssec3
stateneedfreedom=~sfree1+sfree2+sfree3
tsecu=~ tsec1+tsec2+tsec3
tfreed=~tfree1+tfree2+tfree3
Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4
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
paddon.fit <- lavaan::sem(paddon, data = mandpl_com, estimator = "ML",
                          se = "bootstrap",bootstrap = 50L,
                          parallel ="multicore", verbose=F)
summary(paddon.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
est <- parameterEstimates(paddon.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()
paddon1 <- '
stateneedsecurity =~ ssec1+ssec2+ssec3
stateneedfreedom=~sfree1+sfree2+sfree3
tsecu=~ tsec1+tsec2+tsec3
tfreed=~tfree1+tfree2+tfree3
finance=~ finan1+finan2+finan3
Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4
Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed+finance
Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed+finance
stateneedfreedom~~stateneedsecurity
stateneedsecurity ~ b1*C1 + b2*C2+tsecu+tfreed+finance
stateneedfreedom~ b3*C1 + b4*C2+tsecu+tfreed+finance
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
paddon1.fit <- lavaan::sem(paddon1, data = mand_com, estimator = "ML",
                           se = "bootstrap",bootstrap = 50L,
                           parallel ="multicore", verbose=F
)
paddon1.fit <- lavaan::sem(paddon1, data = mandpl_com, estimator = "ML",
                           se = "bootstrap",bootstrap = 50L,
                           parallel ="multicore", verbose=F
)
names(mandpl_com)
paddon1 <- '
stateneedsecurity =~ ssec1+ssec2+ssec3
stateneedfreedom=~sfree1+sfree2+sfree3
tsecu=~ tsec1+tsec2+tsec3
tfreed=~tfree1+tfree2+tfree3
finance=~ financial_sit_1+financial_sit_2+financial_sit_3
Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4
Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed+finance
Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed+finance
stateneedfreedom~~stateneedsecurity
stateneedsecurity ~ b1*C1 + b2*C2+tsecu+tfreed+finance
stateneedfreedom~ b3*C1 + b4*C2+tsecu+tfreed+finance
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
paddon1.fit <- lavaan::sem(paddon1, data = mandpl_com, estimator = "ML",
                           se = "bootstrap",bootstrap = 50L,
                           parallel ="multicore", verbose=F
)
summary(paddon1.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
est <- parameterEstimates(paddon1.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
est |>
  mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  select(label, est, se, CI, pvalue) |>
  rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()














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
onlymanipulationpl <- fastDummies::dummy_cols(onlymanipulationpl, 
                                              select_columns = "gr")
mandpl <- dummy_cols(mandpl, select_columns = "gr")

forsem <- onlymanipulationpl

which(is.na(forsem$gr))
which(is.na(mandpl$gr))
forsem <- forsem[-c(123:125, 187, 188, 262,263), ]
mandpl <- mandpl[-c(191, 192, 193 ,284 ,285, 400 ,401), ] 


#cfa for narratives
# interest
narint.model <- '
  Mainstream=~nar_5  +nar_6  +nar_8 +nar_7
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  Mainstream~~Micronarratives

'

narint.fit <- cfa(narint.model, data = mandpl, estimator = "ML", missing = "FIML")
summary(narint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#agreement
narag.model <- '
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Mainstream~~Micronarratives
    
'

narag.fit <- cfa(narag.model, data = forsem, estimator = "ML", missing = "FIML")
summary(narag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



#new dataset for combination
pl<- subset(forsem[, c(52:58, 149, 205, 218:225)])
write.csv(pl, "pl.csv") 


#separate for needs and approach and agg
secuint.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4

  Micronarrativesint ~b3*needsecurity+d3*gr_1
  Mainstreamint ~b4*needsecurity+d4*gr_1

  needsecurity ~ a1*gr_1


  ind3 := a1*b3
  ind4 := a1*b4
    
  total3 := ind3 + d3
  total4 := ind4 + d4

  
  
  Mainstreamint =~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarrativesint=~nar_1+nar_2+nar_3+ nar_4 

  Mainstreamint~~Micronarrativesint

'

secuint.fit <- sem(secuint.model, data = forsem, estimator = "ML",
                   missing = "FIML", se = "bootstrap",
                   bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(secuint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

lay <- get_layout("","","","Mainstreamint",
                  "gr_1","","needsecurity","",
                  "","","","Micronarrativesint", rows = 3)
graph_sem(secuint.fit, layout=lay)



secuag.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4

  Micronarrativesag ~b1*needsecurity+d1*gr_1
  Mainstreamag ~b2*needsecurity+d2*gr_1

  needsecurity ~ a1*gr_1

  ind1 := a1*b1
  ind2 := a1*b2
    
  total1 := ind1 + d1
  total2 := ind2 + d2

  
  
  Mainstreamag=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarrativesag=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Mainstreamag~~Micronarrativesag


'

secuag.fit <- sem(secuag.model, data = forsem, estimator = "ML",
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(secuag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

lay <- get_layout("","","","Mainstreamag",
                  "gr_1","","needsecurity","",
                  "","","","Micronarrativesag", rows = 3)
graph_sem(secuag.fit, layout=lay)


#free
freeint.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7

  Micronarrativesint ~b3*needfreedom+d3*gr_1
  Mainstreamint ~b4*needfreedom+d4*gr_1

  needfreedom ~ a1*gr_1


  ind3 := a1*b3
  ind4 := a1*b4
    
  total3 := ind3 + d3
  total4 := ind4 + d4

  
  
  Mainstreamint =~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarrativesint=~nar_1+nar_2+nar_3+ nar_4 

  Mainstreamint~~Micronarrativesint

'

freeint.fit <- sem(freeint.model, data = forsem, estimator = "ML",
                   missing = "FIML", se = "bootstrap",
                   bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(freeint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

lay <- get_layout("","","","Mainstreamint",
                  "gr_1","","needsecurity","",
                  "","","","Micronarrativesint", rows = 3)
graph_sem(secuint.fit, layout=lay)
vuongtest(secu.fit, secuint.fit)



freeag.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7

  Micronarrativesag ~b1*needfreedom+d1*gr_1
  Mainstreamag ~b2*needfreedom+d2*gr_1

  needfreedom ~ a1*gr_1

  ind1 := a1*b1
  ind2 := a1*b2
    
  total1 := ind1 + d1
  total2 := ind2 + d2

  
  
  Mainstreamag=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarrativesag=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Mainstreamag~~Micronarrativesag


'

freeag.fit <- sem(freeag.model, data = forsem, estimator = "ML",
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(freeag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

lay <- get_layout("","","","Mainstreamag",
                  "gr_1","","needsecurity","",
                  "","","","Micronarrativesag", rows = 3)
graph_sem(secuag.fit, layout=lay)


vuongtest(secu.fit, secuag.fit)
vuongtest(secu.fit, secuint.fit)



#I put analyses that did not make it into the manuscript here. Some of the results
#are presented on the OSF page separately##########

#trust
trustsec <- lm(institution_trust_5 ~ ssec, data= onlymanipulationpl)
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

#no longer part of paper, some of the analyses are reported on the OSF page


#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m1 <- lm(micronarratives~ ssec*active_soc_media+tsec, data=onlymanipulationpl)
m1.a <- summary(m1)
#ggpredict(m1, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig


tab_model(m1.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,
          digits = 2,           
          p.style = "numeric")

m2 <- lm(mainstream~ ssec*active_soc_media+tsec, data=onlymanipulationpl)
m2.a <- summary(m2)
#ggpredict(m2, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m2.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m3 <- lm(mainstream_ag~ ssec*active_soc_media+tsec, data=onlymanipulationpl)
m3.a <- summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m3.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m4 <- lm(micronarratives_ag~ ssec*active_soc_media+tsec, data=onlymanipulationpl)
m4.a <- summary(m4)
ggpredict(m4, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig
tab_model(m4.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")


m5 <- lm(micronarratives~ sfree*active_soc_media+tfree, data=onlymanipulationpl)
m5.a <- summary(m5)
ggpredict(m5, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig
tab_model(m5.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m6 <- lm(mainstream~ sfree*active_soc_media+tfree, data=onlymanipulationpl)
m6.a <- summary(m6)
ggpredict(m6, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m6.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m7 <- lm(mainstream_ag~ sfree*active_soc_media+tfree, data=onlymanipulationpl)
m7.a <- summary(m7)
ggpredict(m7, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

tab_model(m7.a,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")

m8 <- lm(micronarratives_ag~ sfree*active_soc_media+tfree, data=onlymanipulationpl)
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
eff <- aov(man_eff~gr, data=onlymanipulationpl)
summary(eff)
eta_squared(eff)
TukeyHSD(eff)
#mandatory policy seen as more effective


#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.
acc <- aov(man_acc~gr, data=onlymanipulationpl)
summary(acc)
eta_squared(acc)
TukeyHSD(acc)
apa.aov.table(acc, "acc.doc")



mean(mandpl$age_5, na.rm=T)
sd(mandpl$age_5, na.rm=T)





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
set.seed(545)
medint.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  
  Micronarratives ~b1*needsecurity+d1*gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+d2*gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance
  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2
  total1 := ind1 + d1
  total2 := ind2 + d2
 
'


medint.fit <- sem(medint.model, data = forsem, estimator = "ML"
                  #,
                  #missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
                  #parallel ="multicore", verbose= T
)
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


  Micronarratives ~b1*needsecurity+d1*gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+d2*gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance

  Mainstream~~Micronarratives
    
  ind1 := a1*b1
  ind2 := a1*b2
  
  total1 := ind1 + d1
  total2 := ind2 + d2


'

med.fit <- sem(med.model, data = forsem, estimator = "ML"
               #   , 
               #  missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
               # parallel ="multicore", verbose= T
)
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


  institution_trust_5 ~ traitneedsecurity+b1*intMicronarratives + b2*intMainstream +gr_1 +d3*needsecurity+finance
  intMicronarratives ~a1*needsecurity+d1*gr_1+traitneedsecurity+finance
  intMainstream ~ a2*needsecurity+d2*gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

 ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  ind5:= b1*a1*a3
  
  total1 := ind1 + d3
  total2 := ind2 + d3
  total3 := ind3 + d1
  total4 := ind4 + d2
  total5 := ind5 + d3


  
  intMicronarratives~~intMainstream
'

int_talt.fit <- sem(int_talt.model, data = forsem, estimator = "ML"
                    #, 
                    #missing = "FIML", se = "bootstrap",bootstrap = 500L, 
                    #parallel ="multicore", verbose= T
)
summary(int_talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)
lavaan::parameterEstimates(int_talt.fit, boot.ci.type = "bca.simple")

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
 

  institution_trust_5 ~ b1*Micronarratives + b2*Mainstream +gr_1 +d3*needsecurity+traitneedsecurity+finance
  Micronarratives ~a1*needsecurity+d1*gr_1+traitneedsecurity+finance
  Mainstream ~ a2*needsecurity+d2*gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  ind5:= b1*a1*a3
  
  total1 := ind1 + d3
  total2 := ind2 + d3
  total3 := ind3 + d1
  total4 := ind4 + d2
  total5 := ind5 + d3


  Micronarratives~~Mainstream

'


talt.fit <- sem(talt.model, data = forsem, estimator = "ML"
                #, 
                #missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                #parallel ="multicore", verbose= T
)
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

  Micronarratives ~b1*needfreedom+d1*gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+d2*gr_1+traitneedfreedom+finance
  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2
  
    
  total1 := ind1 + d1
  total2 := ind2 + d2
'

frintmed.fit <- sem(frintmed.model, data = forsem, estimator = "ML"
                    #, 
                    #missing = "FIML", se = "bootstrap",
                    # bootstrap = 5000L,parallel ="multicore", verbose= T
)
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

  Micronarratives ~b1*needfreedom+d1*gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+d2*gr_1+traitneedfreedom+finance

  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2
  
    
  total1 := ind1 + d1
  total2 := ind2 + d2
'

frmed.fit <- sem(frmed.model, data = forsem, estimator = "ML"
                 #, 
                 #missing = "FIML", se = "bootstrap",
                 #bootstrap = 5000L,parallel ="multicore", verbose= T
)
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

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+d3*needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+d1*gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+d2*gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance
  institution_trust_5~traitneedfreedom+finance
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  total1 := ind1 + d3
  total2 := ind2 + d3
  total3 := ind3 + d1
  total4 := ind4 + d2


  Mainstream~~Micronarratives

'

frtaltint.fit <- sem(frtaltint.model, data = forsem, estimator = "ML"
                     , 
                     missing = "FIML", se = "bootstrap",
                     bootstrap = 5000L,parallel ="multicore", verbose= T
)
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

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+d3*needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+d1*gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+d2*gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  total1 := ind1 + d3
  total2 := ind2 + d3
  total3 := ind3 + d1
  total4 := ind4 + d2

  Mainstream~~Micronarratives

'


frtalt.fit <- sem(frtalt.model, data = forsem, estimator = "ML"
                  , 
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T
)
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
  institution_trust_5 ~ d1*needsecurity +d2*active_soc_media+gr_1+traitneedsecurity
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

  
  total1 := ind1 + d1
  total2 := ind2 + d1
  total3 := ind3 + d1
  total4 := ind4 + d1

  total5 := ind5 + d2
  total6 := ind6 + d2
  total7 := ind7 + d2
  total8 := ind8 + d2



  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needsecurity


'

ldla.fit <- sem(ldla.model, data = forsem, estimator = "ML"
                #, 
                #missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                #parallel ="multicore", verbose= T
)
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
  institution_trust_5 ~ gr_1+d2*active_soc_media+gr_1+traitneedfreedom
  +b1*likeMicronarratives+b2*likeMainstream+b3*dislikeMicronarratives+
  b4*dislikeMainstream + finance+d1*needfreedom
    
  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*b3
  ind4 := a4*b4
  
  ind5 := c1*b1
  ind6 := c2*b2
  ind7 := c3*b3
  ind8 := c4*b4
  
  total1 := ind1 + d1
  total2 := ind2 + d1
  total3 := ind3 + d1
  total4 := ind4 + d1

  total5 := ind5 + d2
  total6 := ind6 + d2
  total7 := ind7 + d2
  total8 := ind8 + d2
  
  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needfreedom
'

fldl.fit <- sem(fldl.model, data = forsem, estimator = "ML"
                #, 
                #        missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                #       parallel ="multicore", verbose= T
)
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


vuongtest(frtalt.fit, frtalt.fit)










both.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3  
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  traitneedfreedom=~ security_4+security_5+security_6
  
  needs =~ needsecurity+needfreedom
  traitneeds =~ traitneedsecurity+ traitneedfreedom

  Micronarratives ~b1*needs+gr_1+traitneeds+finance
  Mainstream ~b2*needs+gr_1+traitneeds+finance
  needs ~ a1*gr_1+traitneeds+finance


  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2

'

both.fit <- sem(both.model, data = forsem, estimator = "ML"
                #, 
                #missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                #parallel ="multicore", verbose= T
)
summary(both.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


library(simsem)


vuongtest(medint.fit, frintmed.fit)
vuongtest(med.fit, frmed.fit)
vuongtest(int_talt.fit, frtaltint.fit)
vuongtest(int_talt.fit, frtaltint.fit)
vuongtest(talt.fit, frtalt.fit)




nars <- subset(forsem[, c("nar1_end", "nar_1", "nar2_end", "nar_2","nar_3", "nar3_end",
                          "nar4_end","nar_4", "Nar_5correct", "nar5_end", "nar_6correct", 
                          "nar6_end","nar7_end","nar_7correct", "nar_8correct", "nar8_end")])
forsem$nar1_t <- apply(nars[, 1:2], 1, mean)
forsem$nar2_t <- apply(nars[, 3:4], 1, mean)
forsem$nar3_t <- apply(nars[, 5:6], 1, mean)
forsem$nar4_t <- apply(nars[, 7:8], 1, mean)
forsem$nar5_t <- apply(nars[, 9:10], 1, mean)
forsem$nar6_t <- apply(nars[, 11:12], 1, mean)
forsem$nar7_t <- apply(nars[, 13:14], 1, mean)
forsem$nar8_t <- apply(nars[, 15:16], 1, mean)
#no longer part of paper####

