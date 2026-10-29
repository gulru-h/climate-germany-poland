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
uncleaned <- read_sav("uncleaned.sav")
rawpl <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";", dec = ".")

#removed ip adresses of participants and user ids
anon <- uncleaned[, -c(4, 179)]
write.csv(anon, "anon.csv", row.names = FALSE, fileEncoding = "UTF-8")
#keep in mind that this data file has 2 less columns so the subsetting actions
#below might be referring to the wrong columns

cleanpl <- rawpl[,c(23:214 )]
uncleaned <- uncleaned[,c(23:210 )]

names(cleanpl)

########data cleaning######
#combine variables
#finance manipulation: single item
man_financepl <- subset(cleanpl[,c(60,65)])
cleanpl$man_finance <- apply(man_financepl, 1, sum, na.rm=T)

#efficacy: single item
class(uncleaned$effectiveness_exp)
uncleaned$effectiveness_exp <- as.numeric(uncleaned$effectiveness_exp)

#man_effpl <- subset(cleanpl[,c(61,66)])
#cleanpl$man_eff <- apply(man_effpl, 1, sum, na.rm=T)
#mean(cleanpl$man_eff, na.rm=T)

#acceptability
man_accpl <- subset(cleanpl[,c(63,68)])
cleanpl$man_acc <- apply(man_accpl, 1, sum, na.rm=T)
mean(cleanpl$man_acc, na.rm=T)

uncleaned$gr <- as.factor(uncleaned$gr)
uncleaned$q_terminate_flag<- as.numeric(uncleaned$q_terminate_flag)

#attention check
uncleaned$q_terminate_flag[uncleaned$gr== 2 & uncleaned$att_merged==1] <- 1
uncleaned$q_terminate_flag[uncleaned$gr== 1 & uncleaned$att_merged==2] <- 2
uncleaned$q_terminate_flag[uncleaned$gr== 3 & uncleaned$att_merged==3]<- 3

table(uncleaned$q_terminate_flag)
table(uncleaned$gr)
table(uncleaned1$gr)
uncleaned1 <- subset(uncleaned, uncleaned$q_terminate_flag!= 0)
nrow(uncleaned1)


table(uncleaned1[uncleaned1$gr !=3, ]$realist_merged > 1)
table(uncleaned1[uncleaned1$gr !=3, ]$credib_merged > 1)

cond1 = uncleaned1$gr!=3
cond2 = uncleaned1$realist_merged > 1
cond3 = uncleaned1$credib_merged > 1

cond = (cond1 & cond2 & cond3) | !cond1

uncleaned2 <- uncleaned1[cond,]

table(uncleaned2[uncleaned2$gr !=3, ]$realist_merged > 1)
table(uncleaned2$gr)

uncleaned2$q_total_duration<- as.numeric(uncleaned2$q_total_duration)
uncleaned2$q_total_duration<- uncleaned2$q_total_duration/60
summary(uncleaned2$q_total_duration)
18.175/2
9.0875

uncleaned3 <- uncleaned2[uncleaned2$q_total_duration >  9.0875, ]
table(uncleaned3$gr)

uncleaned4<- uncleaned3 %>% filter_all(any_vars(!is.na(.)))
table(uncleaned4$gr)

slow <-  uncleaned2[uncleaned2$q_total_duration >  120, ]
table(slow$gr)

uncleaned5 <- uncleaned4[uncleaned4$q_total_duration <  120, ]
uncleaned6 <- uncleaned5[uncleaned5$q_straightlining_count != 7, ]

cleanpl<-uncleaned6
summary(uncleaned6$q_total_duration)
summary(mand$duration_m)

####build means####
cleanpl <- cleanpl %>%
  mutate(across(where(is.character), ~ {
    numeric_vals <- suppressWarnings(as.numeric(.))
    if (mean(!is.na(numeric_vals), na.rm = TRUE) > 0.9) numeric_vals else .
  }))

class(cleanpl$financial_sit_3)
finanpl <- subset(cleanpl[,c(31:33)])
cleanpl$finan <- apply(finanpl, 1, mean, na.rm=T)
psych::alpha(finanpl)
omega(finanpl, nfactors= 1)

tsecpl <- subset(cleanpl[,c(34:36)])
cleanpl$tsec <- apply(tsecpl, 1, mean, na.rm=T)
psych::alpha(tsecpl)
omega(tsecpl, nfactors= 1)

#last three security items are actually freedom
tfreepl <- subset(cleanpl[,c(37:39)])
cleanpl$tfree <- apply(tfreepl, 1, mean, na.rm=T)
psych::alpha(tfreepl)
omega(tfreepl, nfactors= 1)

ssecpl <- subset(cleanpl[,c(52:55)])
cleanpl$ssec <- apply(ssecpl, 1, mean, na.rm=T)
psych::alpha(ssecpl)
class(cleanpl$security_freedom_4)
omega(ssecpl, nfactors= 1)

sfreepl <- subset(cleanpl[,c(56:58)])
cleanpl$sfree <- apply(sfreepl, 1, mean, na.rm=T)
psych::alpha(sfreepl)
omega(sfreepl, nfactors= 1)



cleanpl$finan[cleanpl$finan == "1888"]<- NA

cleanpl[, c(77, 79, 81, 83)] <- lapply(cleanpl[, c(77, 79, 81, 83)], as_factor)

cleanpl[, c(77, 79, 81, 83)] <- lapply(cleanpl[, c(77, 79, 81, 83)], function(x) {
  factor(gsub(" \\(.*\\)", "", as.character(x)))
})
cleanpl[, c(77, 79, 81, 83)] <- lapply(cleanpl[, c(77, 79, 81, 83)], as.numeric)


mandpl <- cleanpl
cleanpl

##narratives####

#subsetting and building means 

micronarrativespl <- subset(cleanpl[,c(69,71,73,75)])
mandpl$micronarratives <- apply(micronarrativespl, 1, mean, na.rm=T)
psych::alpha(micronarratives)
omega(micronarrativespl, nfactors= 1)

mainstreampl <- subset(cleanpl[,c(77,79,81,83)])
mandpl$mainstream <- apply(mainstreampl, 1, mean, na.rm=T)
psych::alpha(mainstream)
omega(mainstream, nfactors= 1)

library(haven)

micronarratives_agpl <- subset(mandpl[,c(70,72,74,76)])
mandpl$micronarratives_ag <- apply(micronarratives_agpl, 1, mean, na.rm=T)
psych::alpha(micronarratives_agpl)
omega(micronarratives_agpl, nfactors= 1)

mainstream_agpl <- subset(mandpl[,c(78,80,82,84)])
mandpl$mainstream_ag <- apply(mainstream_agpl, 1, mean, na.rm=T)
psych::alpha(mainstream_agpl)
omega(mainstream_agpl, nfactors= 1)

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

effpl<- lm(mandpl$effec_merged ~ gr_a + age + gender, data= mandpl)
summary(effpl)

car::Anova(effpl)
effectsize(Anova(effpl))

pw <- pairs(emmeans(effpl, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")


#create a manipulation only group without the control group
mandpl <- mandpl[,(c(2:16, 28:39, 52:58, 69:84,
                     143, 145:150, 159,189:197,172))]
#mandpl$active_soc_media[mandpl$active_soc_media == "-2"]<- NA

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
                           "finan", "SES", "EU",
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

# Combine into one data frame with M and SD columns
desc_table <- data.frame(
  Variable = rownames(meancors1pl),
  M  = paste0(meancors1pl[,1], "/", meancors2pl[,1], "/", meancors3pl[,1]),
  SD = paste0(sdcors1pl[,1],   "/", sdcors2pl[,1],   "/", sdcors3pl[,1])
)

# As flextable
library(flextable)
desc_table |>
  flextable() |>
  set_header_labels(M = "M (G1 / G2 / G3)", SD = "SD (G1 / G2 / G3)") |>
  bold(part = "header") |>
  autofit()


mean(mandpl$age, na.rm=T)
SD(mandpl$age, na.rm=T)

table(mandpl$gender)

table(mandpl$edu)


mandpl$age_group <- cut(mandpl$age, breaks = c(17, 34, 49, 64, Inf),
                      labels = c("18-34", "35-49", "50-64", "65+"))
table(mandpl$age_group)
prop.table(table(mandpl$age_group, mandpl$gr), margin = 2)
summary(mandpl$age)   # min, median, max





#CAN WE TELL THE NEEDS APART

needs <- 'SEC=~ ssec1+ssec2+ssec3
          FREE=~ sfree1+sfree2+sfree3
         # TSEC=~ tsec1+tsec2+tsec3
          #TFREE=~ tfree1+tfree2+tfree3
          SEC~~FREE
          #TFREE~~ rtgtSEC
          #TSEC~~SEC
          #TSEC~~FREE
          #TFREE~~ SEC
          #TFREE~~ FREE

'

needspl.fit<- cfa(needs, data = mandpl, estimator = "ML")
summary(needspl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

standardizedSolution(needspl.fit, type = "std.all", se = TRUE, ci = TRUE) %>%
  filter(op == "~~", lhs != rhs)
model_constrained <- gsub("SEC~~FREE", "SEC ~~ 1*FREE", needs)
fit_constrained <- cfa(model_constrained, data = mandpl)
summary(fit_constrained, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
anova(needspl.fit, fit_constrained)

library(semTools)
AVE(needspl.fit)
htmt(needs, data=mandpl)
sqrt(0.920)
sqrt(0.904)
needs1 <- 'COMBO=~ ssec1+ssec2+ssec3+sfree1+sfree2+sfree3
            #TSEC=~ tsec1+tsec2+tsec3
           # TFREE=~tfree1+tfree2+tfree3
        #   COMBO~~TSEC
        #   COMBO~~TFREE

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


mandpl$micronarratives_ag_s <- scale(mandpl$micronarratives_ag, center=TRUE, scale=T)[,1]
mandpl$mainstream_ag_s <- scale(mandpl$mainstream_ag, center=TRUE, scale=T)[,1]

mandpl$ssec_s <- scale(mandpl$ssec, center=TRUE, scale=T)[,1]
mandpl$tsec_s <- scale(mandpl$tsec, center=TRUE, scale=T)[,1]
mandpl$sfree_s <- scale(mandpl$sfree, center=TRUE, scale=T)[,1]
mandpl$tfree_s <- scale(mandpl$tfree, center=TRUE, scale=T)[,1]

mandpl$trust5_s <- scale(mandpl$trust5, center=TRUE, scale=T)[,1]
mandpl$finan_s <- scale(mandpl$finan, center=TRUE, scale=T)[,1]
mandpl$med_act_s <- scale(mandpl$med_act, center=TRUE, scale=T)[,1]
mandpl$med_pas_s <- scale(mandpl$med_pas, center=TRUE, scale=T)[,1]
mandpl$media_daily_1_s <- scale(mandpl$media_daily_1, center=TRUE, scale=T)[,1]
mandpl$SES_s <- scale(mandpl$SES, center=TRUE, scale=T)[,1]
mandpl$mainstream_s <- scale(mandpl$mainstream, center=TRUE, scale=T)[,1]
mandpl$micronarratives_s <- scale(mandpl$micronarratives, center=TRUE, scale=T)[,1]


#H1: Participants in the mandatory condition will perceive a higher threat for 
#financial security in comparison to those in the voluntary and control conditions. 

#3 group
mandpl$gr_a <- as.factor(mandpl$gr)
mandpl$gender <- as.factor(mandpl$gender)
#mandpl$gender <- ifelse(mandpl$gender == 1, 2, 1)

onlymanipulationpl$gr <- as.factor(onlymanipulationpl$gr)


n1p <- lm(ssec~ gr_a+gender+age, data=mandpl)
summary(n1p)
car::Anova(n1p)
effectsize(Anova(n1p))

pw <- pairs(emmeans(n1p, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")

n1frp <- lm(sfree~ gr_a+gender+age, data=mandpl)
summary(n1frp)
car::Anova(n1frp)
effectsize(Anova(n1frp))

pw <- pairs(emmeans(n1frp, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")




n666p <- lm(ssec~ gr_a*(sfree_s+finan_s)+micronarratives_s+mainstream_s+tfree_s+tsec_s+SES_s+gender+age, data=mandpl)
summary(n666p)
ggpredict(n666p, terms = c("sfree_s","gr_a")) |> plot()
##use that one

emtrends(n666frp, 
         pairwise ~ gr_a, 
         var = "ssec_s")


n666frp <- lm(sfree~ gr_a*(ssec_s+finan_s)+micronarratives_s+mainstream_s+tfree_s+tsec_s+SES_s+gender+age, data=mandpl)
summary(n666frp)

ggpredict(n66frp, terms = c("finan_s","gr_a")) |> plot()
##use that one




#micronarrative approach
mip <- lm(micronarratives~ gr_a+gender+age, data=mandpl)
summary(mip)
car::Anova(mip)
effectsize(Anova(n1p))

pw <- pairs(emmeans(n1p, ~gr_a), adjust = "BH")
summary(pw, infer = TRUE, adjust ="tukey")

map <- lm(mainstream~ gr_a+gender+age, data=mandpl)
summary(map)
car::Anova(map)


milmendagp<- lm(micronarratives~
                gr_a*(ssec_s+sfree_s+finan_s+micronarratives_ag_s)+SES_s+tsec_s+tfree_s+age+gender,
              data=mandpl)
summary(milmendagp)
car::vif(milmendagp, type= "predictor")
car::Anova(milmendagp)
ggpredict(milmendagp, terms = c("finan_s","gr_a")) |> plot()

emtrends(milmendagp, 
         pairwise ~ gr_a, 
         var = "finan_s")


milmendagapp<- lm(micronarratives~
                  gr_a*(ssec_s+sfree_s+finan_s+micronarratives_ag_s+mainstream_s)+SES_s+tsec_s+tfree_s+age+gender,
                data=mandpl)
summary(milmendagapp)
emtrends(milmendagapp, 
         pairwise ~ gr_a, 
         var = "finan_s")

maendagp<- lm(mainstream~
              gr_a*(sfree_s+ssec_s+finan_s+mainstream_ag_s+micronarratives_s)+SES_s+tsec_s+tfree_s+age+gender,
            data=mandpl)
summary(maendagp)
car::Anova(maendagp)
car::vif(maendagp, type= "predictor")




maendagapp<- lm(mainstream~
                gr_a*(sfree_s+ssec_s+finan_s+mainstream_ag_s+micronarratives_s)+SES_s+tsec_s+tfree_s+age+gender,
              data=mandpl)
summary(maendagapp)



library(modelsummary)
library(flextable)

modelsummary(
  list("Security" = n666p, "Freedom" = n666frp, "Anti-Mainstream" = milmendagapp, "Mainstream" = maendagapp),
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
                  "mainstream_s" = "Mainstream Narratives",
                  "micronarratives_ag_s" = "Agreement Anti-Mainstream",
                  "mainstream_ag_s" ="Agreement Mainstream"),
  output = "flextable"
  #shape = term ~ model + statistic
) |>
  save_as_docx(path = "table.docx")

summary(n666p)
######



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

ggpredict(mimifinan2, terms = c("finan_c", "age")) |> plot()

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



        
#trait
tr1<- lm(tsec~ gr_a + age + gender, data=mandpl)
car::Anova(tr1)
tr2<- lm(tfree~ gr_a + age + gender, data=mandpl)
car::Anova(tr2)
#no group differences
#####
           
######





mf<- lm(man_finance~ gr_a, data=mand)
summary(mf)
pairs(emmeans(mf, ~gr_a), adjust="BH")

mandpl_com <- mandpl[complete.cases(mandpl[, c("sfree", "gr_a", "tsec", "ssec", "finan", 
                                         "tfree", "SES", "gender", "age")]), ]

mandpl_com$C1 <- ifelse(mandpl_com$gr_a == 1, 1, ifelse(mandpl_com$gr_a == 2, -1, 0))
mandpl_com$C2 <- ifelse(mandpl_com$gr_a == 1, 1, ifelse(mandpl_com$gr_a == 3, -1, 0))
mandpl_com$gender<- as.numeric(mandpl_com$gender)

onlymanipulationpl <- mandpl[mandpl$gr_a ==1 |mandpl$gr_a ==2 , ]


####SEM with two groups and security####


psecu2 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3

Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4

  Micronarratives ~ c1*stateneedsecurity +a1*gr_2
  Mainstream      ~ c3*stateneedsecurity +a3*gr_2
  

  stateneedsecurity ~ b1*gr_2

  ind_1v2_sec_micro := b1 * c1
  ind_1v2_sec_main  := b1 * c3

  total_1v2_micro := a1 + (b1*c1) 
  total_1v2_main  := a3 + (b1*c3)

  
'


psecu2.fit <- lavaan::sem(psecu2, data = onlymanipulationpl, estimator = "ML", 
                         se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(psecu2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
psecu2.ml.fit <- lavaan::sem(psecu2, data = onlymanipulationpl, estimator = "ML")


#######

####SEM with 2 groups and freedom####

pfree2 <- '
  stateneedfreedom=~sfree1+sfree2+sfree3
  

Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4


  Micronarratives ~  c2*stateneedfreedom + a1*gr_2
  Mainstream      ~  c4*stateneedfreedom + a3*gr_2
  
  stateneedfreedom~ b3*gr_2
  
  ind_1v2_free_micro := b3 * c2
  ind_1v2_free_main  := b3 * c4

  total_1v2_micro := a1 +  (b3*c2)
  total_1v2_main  := a3 +  (b3*c4)
 
'


pfree2.fit <- lavaan::sem(pfree2, data = onlymanipulationpl, estimator = "ML", 
                         se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(pfree2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
pfree2.ml.fit <- lavaan::sem(pfree2, data = mand_com, estimator = "ML")


#######

####SEM with 2 groups and both needs####

psecufree2 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  

Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4


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


psecufree2.fit <- lavaan::sem(psecufree2, data = onlymanipulationpl, estimator = "ML", 
                             se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

psecufree2.ml.fit <- lavaan::sem(psecufree2, data = onlymanipulationpl, estimator = "ML")


summary(psecufree2.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

est <- parameterEstimates(psecufree2.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

#######

vuongtest(free2.ml.fit,secufree2.ml.fit)
vuongtest(secu2.ml.fit,secufree2.ml.fit)



####SEM with all three groups and security####


psecu <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3

Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4


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


psecu.fit <- lavaan::sem(psecu, data = mandpl_com, estimator = "ML", 
                        se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(psecu.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


est <- parameterEstimates(psecu.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()


#######



####SEM with all three groups and freedom####



pfree <- '
  stateneedfreedom=~sfree1+sfree2+sfree3
  
Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4


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


pfree.fit <- lavaan::sem(pfree, data = mandpl_com, estimator = "ML", 
                        se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

summary(pfree.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
lay <- get_layout("stateneedfreedom","","","Mainstream",
                  "stateneedsecurity","","","",
                  "","","","",
                  "tfreed","","","",
                  "tsecu","","","",
                  "","","","",
                  "","","","",
                  "finance","","","Micronarratives", rows = 8)
graph_sem(psecu.fit, layout=lay)




est <- parameterEstimates(pfree.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()

params <- parameterEstimates(pfree.fit, ci = TRUE, standardized = T)|>
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






####SEM with all three groups and both needs####

psecufree <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3


Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct
Micronarratives=~nar_1+nar_2+nar_3+ nar_4



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


psecufree.fit <- lavaan::sem(psecufree, data = mandpl_com, estimator = "ML", 
                            se = "bootstrap",bootstrap = 50L,parallel ="multicore", verbose=F)

psecufree.ml.fit <- lavaan::sem(psecufree, data = mandpl_com, estimator = "ML")



summary(psecufree.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



est <- parameterEstimates(psecufree.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
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

paddon1.fit <- lavaan::sem(addon1, data = mandpl_com, estimator = "ML", 
                          se = "bootstrap",bootstrap = 50L,
                          parallel ="multicore", verbose=F
)
paddon1.ml.fit <- lavaan::sem(paddon1, data = mandpl_com, estimator = "ML")

summary(paddon1.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

est <- parameterEstimates(paddon1.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
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
                           se = "bootstrap",bootstrap = 50L,
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








####SEM with all three groups and trait plus finance+micronarratives ####
#used in the manuscript

paddon3 <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3
  finance=~ financial_sit_1+financial_sit_2+financial_sit_3



  Mainstream=~nar_5  +nar_6  +nar_7  +nar_8
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed+finance+micronarratives_ag+mainstream_ag
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed+finance+micronarratives_ag+mainstream_ag
  
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

  # --- Total effects (direct + total indirect) ---
  total_1v2_micro := a1 + (b1*c1) + (b3*c2)
  total_1v2_main  := a3 + (b1*c3) + (b3*c4)
  total_1v3_micro := a2 + (b2*c1) + (b4*c2)
  total_1v3_main  := a4 + (b2*c3) + (b4*c4)
  total_2v3_micro := (a2-a1) + ((b2-b1)*c1) + ((b4-b3)*c2)
  total_2v3_main  := (a4-a3) + ((b2-b1)*c3) + ((b4-b3)*c4)
  
'

paddon3.fit <- lavaan::sem(paddon3, data = mandpl_com, estimator = "ML", 
                           se = "bootstrap",bootstrap = 5000L,
                           parallel ="multicore", verbose=F
)
paddon3.ml.fit <- lavaan::sem(paddon3, data = mandpl_com, estimator = "ML")

summary(paddon3.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

est <- parameterEstimates(paddon1.fit, ci = TRUE) |>
  subset(op == ":=", select = c(label, est, se, pvalue, ci.lower, ci.upper)) |>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

est |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(label, est, se, CI, pvalue) |>
  dplyr::rename(Parameter = label, B = est, SE = se, `95% CI` = CI, p = pvalue) |>
  flextable() |>
  bold(part = "header") |>
  autofit()



params <- parameterEstimates(paddon3.fit, ci = TRUE, standardized = T)|>
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))

params |>
  dplyr::mutate(CI = paste0("[", ci.lower, ", ", ci.upper, "]")) |>
  dplyr::select(lhs,op, rhs, est, se, CI, pvalue, std.all) |>
  dplyr::rename(Parameter = lhs, op= , Predictor =rhs, B = est, SE = se, `95% CI` = CI, p = pvalue, 
                `Std. B`= std.all) |>
  flextable() |>
  bold(part = "header") |>
  autofit()





paddon3_cov <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  stateneedfreedom=~sfree1+sfree2+sfree3
  tsecu=~ tsec1+tsec2+tsec3
  tfreed=~tfree1+tfree2+tfree3
  finance=~ financial_sit_1+financial_sit_2+financial_sit_3



  Mainstream=~nar_5  +nar_6  +nar_7  +nar_8
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4


  Micronarratives ~ c1*stateneedsecurity + c2*stateneedfreedom + a1*C1 + a2*C2 +tsecu+tfreed+finance+micronarratives_ag+mainstream_ag+age+gender
  Mainstream      ~ c3*stateneedsecurity + c4*stateneedfreedom + a3*C1 + a4*C2+tsecu+tfreed+finance+micronarratives_ag+mainstream_ag+age+gender
  
  stateneedfreedom~~stateneedsecurity
  
  stateneedsecurity ~ b1*C1 + b2*C2+tsecu+tfreed+finance+age+gender
  stateneedfreedom~ b3*C1 + b4*C2+tsecu+tfreed+finance+age+gender
  
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

paddon3_cov.ml.fit <- lavaan::sem(paddon3_cov, data = mandpl_com, estimator = "ML")
summary(paddon3_cov.ml.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



table(mandpl_com$gr)

#######


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







##mediation with process####




# Create dummies (group 1 = reference)
mandpl$gr_a_d2 <- as.numeric(mandpl$gr_a == 2)
mandpl$gr_a_d3 <- as.numeric(mandpl$gr_a == 3)
mandpl$gender <- as.numeric(mandpl$gender)
# 1 vs 2
d_1v2pl <- mandpl[mandpl$gr_a %in% c(1, 2), ]

process(data = d_1v2pl,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("micronarratives_ag", "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        
        center= 2,
        boot = 5000,
        seed = 123)



process(data = d_1v2pl ,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("micronarratives_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        model = 4,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)

# 1 vs 3 
d_1v3pl <- mandpl[mandpl$gr_a %in% c(1, 3), ]

process(data = d_1v3pl,
        y = "micronarratives",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c("micronarratives_ag", "SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)

process(data = d_1v3pl ,
        y = "micronarratives",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c("micronarratives_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)

#2 vs 3
d_2v3pl <- mandpl[mandpl$gr_a %in% c(2, 3), ]

process(data = d_2v3pl,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("micronarratives_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)


process(data = d_2v3pl,
        y = "micronarratives",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c( "micronarratives_ag","SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)




#mainstream

#1v2

process(data = d_1v2pl ,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("mainstream_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        model = 4,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)


process(data = d_1v2pl,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("mainstream_ag","SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        total=1,
        
        model = 7,
        center= 2,
        boot = 5000,
        seed = 123
)


#1v3
process(data = d_1v3pl ,
        y = "mainstream",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c("mainstream_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)


process(data = d_1v3pl,
        y = "mainstream",
        x = "gr_a_d3",
        m = c("sfree", "ssec"),
        cov = c( "mainstream_ag","SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        total=1,
        
        model = 7,
        center= 2,
        boot = 5000,
        seed = 123)





#2v3
process(data = d_2v3pl,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c("mainstream_ag","finan", "SES", "tsec", "tfree", "age", "gender"),
        total= 1,
        model = 4,
        center= 2,
        boot = 5000,
        seed = 123)




process(data = d_2v3pl,
        y = "mainstream",
        x = "gr_a_d2",
        m = c("sfree", "ssec"),
        cov = c( "mainstream_ag","SES", "tsec", "tfree", "age", "gender"),
        w= "finan",
        model = 7,
        total=1,
        center= 2,
        boot = 5000,
        seed = 123)














library(dplyr)
library(Hmisc)

vars <- c("ssec", "tsec", "sfree", "tfree", "finan", "SES",
          "micronarratives", "mainstream", "micronarratives_ag", "mainstream_ag")

# Variables available in country 1
vars_c1 <- c("ssec", "tsec", "sfree", "tfree", "finan", "SES",
             "micronarratives", "mainstream")

# Helper: format correlation with significance stars
format_cor <- function(r, p) {
  if (is.na(r)) return("")
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  r_fmt <- sub("0\\.", ".", sprintf("%.2f", r))
  paste0(r_fmt, stars)
}

# Helper: get correlation between two variables for a given dataframe and group
get_cor <- function(df, var1, var2, group) {
  d <- df[df$gr == group, c(var1, var2)]
  d <- d[complete.cases(d), ]
  if (nrow(d) < 3) return(c(NA, NA))
  ct <- cor.test(d[[var1]], d[[var2]])
  c(ct$estimate, ct$p.value)
}

# Build the matrix (as character)
n <- length(vars)
mat <- matrix("", nrow = n, ncol = n, dimnames = list(vars, vars))

for (i in 1:n) {
  for (j in 1:n) {
    if (i == j) {
      mat[i, j] <- "-"
      next
    }
    
    var_i <- vars[i]
    var_j <- vars[j]
    
    if (i > j) {
      # Below diagonal: Country 1
      # Skip if either variable not in country 1
      if (!var_i %in% vars_c1 || !var_j %in% vars_c1) {
        mat[i, j] <- ""
        next
      }
      r1 <- get_cor(mand, var_i, var_j, 1)
      r2 <- get_cor(mand, var_i, var_j, 2)
      r3 <- get_cor(mand, var_i, var_j, 3)
      mat[i, j] <- paste0(
        format_cor(r1[1], r1[2]), "/
        ",
        format_cor(r2[1], r2[2]), "/
        ",
        format_cor(r3[1], r3[2])
      )
    } else {
      # Above diagonal: Country 2
      r1 <- get_cor(mandpl, var_i, var_j, 1)
      r2 <- get_cor(mandpl, var_i, var_j, 2)
      r3 <- get_cor(mandpl, var_i, var_j, 3)
      mat[i, j] <- paste0(
        format_cor(r1[1], r1[2]), "/
        ",
        format_cor(r2[1], r2[2]), "/
        ",
        format_cor(r3[1], r3[2])
      )
    }
  }
}

# View
as.data.frame(mat)



library(flextable)
library(officer)

mat_df <- as.data.frame(mat)
mat_df <- cbind(Variable = rownames(mat_df), mat_df)

ft <- flextable(mat_df) |>
  theme_vanilla() |>
  autofit() |>
  add_header_lines("Correlation matrix: Country 1 below diagonal / Country 2 above diagonal") |>
  add_footer_lines("Note. Values = Group 1 / Group 2 / Group 3. * p < .05, ** p < .01, *** p < .001")

save_as_docx(ft, path = "correlation_matrix1.docx")








