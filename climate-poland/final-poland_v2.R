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
a <- aov(ssec ~ gr+tsec+finan, data=mand)
summary(a)
eta_squared(a)

aa <- aov(ssec ~ gr, data=mand)
summary(aa)
TukeyHSD(aa)
aatab <- apa.aov.table(aa) 


library(ggplot2)
library(ggsignif)
library(dplyr)

# Compute group means & 95% CI for positioning
ci_data_aa <- mand %>%
  group_by(gr) %>%
  summarise(
    mean = mean(ssec, na.rm = TRUE),
    se = sd(ssec, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    ci_upper = mean + qt(0.975, df = nrow(mand)-1) * se
  )

# Highest point for placing stars
y_pos_max <- max(ci_data_aa$ci_upper) + 0.3

# Tukey results: choose comparisons
comparisons <- list(
  c("1", "2"), # p = 0.0200038 
  c("1", "3"), # p < .0001
  c("2", "3")  # p < .0001
)

# Manual stars: "*" for sig, "ns" for non-sig
annotations <- c("*", "***", "***")

# Plot
ggplot(mand, aes(x = gr, y = ssec)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6, color = "#58756A", fill = "#58756A") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_signif(
    comparisons = comparisons,
    annotations = annotations,
    y_position = c(
      max(ci_data_aa$ci_upper[1:2]) + 0,
      max(ci_data_aa$ci_upper[c(1,3)]) + 0.4,
      max(ci_data_aa$ci_upper[2:3]) + 1.6
    ),
    tip_length = 0.01
  ) +
  scale_x_discrete(labels = c(" Mandatory", "Voluntary", "Control")) + # Custom names if desired
  labs(
    x = "Group",
    y = "NFS",
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#only manipulation
b <- aov(ssec ~ gr+tsec+finan, data=onlymanipulation)
summary(b)
bb <- aov(ssec ~ gr, data=onlymanipulation)
TukeyHSD(bb)


aaa <- aov(institution_trust_5~gr, data=mand)
summary(aaa)
TukeyHSD(aaa)

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

#approach

microint<- lm(micronarratives~gr, data=onlymanipulation)
summary(microint)
standardise(microint)


tab_model(microint,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")


microag<- lm(micronarratives_ag~gr, data=onlymanipulation)
summary(microag)
standardise(microag)


tab_model(microag,
          show.std = TRUE,     
          show.se = TRUE,       
          show.fstat = TRUE,    
          digits = 2,           
          p.style = "numeric")


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


#H1.1: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
eff <- aov(man_eff~gr, data=onlymanipulation)
summary(eff)
eta_squared(eff)
TukeyHSD(eff)
#mandatory policy seen as more effective


#H1.2: Participants in the mandatory condition will perceive the policy to be less 
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


set.seed(545)
#INTEREST
#security, interest, trust

#measurement model
mint_talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  intMainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  intMicronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  trust =~1*institution_trust_5

'

mint_talt.fit <- sem(mint_talt.model, data = forsem, estimator = "ML", 
                    missing = "FIML"
                  #  , se = "bootstrap",bootstrap = 5000L, 
                  # parallel ="multicore", verbose= T
                    )
summary(mint_talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)



int_talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  intMainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  intMicronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  trust =~1*institution_trust_5


  trust ~ traitneedsecurity+b1*intMicronarratives + b2*intMainstream +gr_1 +needsecurity+finance
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
                    missing = "FIML"
                   # , se = "bootstrap",bootstrap = 5000L, 
                  #  parallel ="multicore", verbose= T
                    )
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


#AGREEMENT
#Measurement model

mtalt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  trust =~1*institution_trust_5

'

mtalt.fit <- sem(mtalt.model, data = forsem, estimator = "ML", 
                missing = "FIML"
                #, se = "bootstrap",bootstrap = 5000L, 
               # parallel ="multicore", verbose= T
               )
summary(mtalt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

#structural model
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
                missing = "FIML"
                , se = "bootstrap",bootstrap = 5000L, 
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

#APPROACH
#measurement model


mfrtaltint.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

'

mfrtaltint.fit <- sem(mfrtaltint.model, data = forsem, estimator = "ML", 
                     missing = "FIML", se = "bootstrap",
                     bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(mfrtaltint.fit, fit.measures=T, standardized = T, rsquare=TRUE,
        ci=T)


#structural model
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

#AGREEMENT
#freedom 
#measurement model

mfrtalt.model <- '
  needfreedom =~ security_freedom_5+security_freedom_6+security_freedom_7
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

'

mfrtalt.fit <- sem(mfrtalt.model, data = forsem, estimator = "ML", 
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(mfrtalt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



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


rsquareCalc(frtaltint.fit, "institution_trust_5", "Micronarratives")
rsquareCalc(frtaltint.fit, "institution_trust_5", "Mainstream")

library(nonnest2)
vuongtest(int_talt.fit, medint.fit)





simple <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3+security_freedom_4
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  institution_trust_5 ~ b1*Micronarratives + b2*Mainstream +gr_1 +d3*needsecurity
  Micronarratives ~a1*needsecurity+d1*gr_1
  Mainstream ~ a2*needsecurity+d2*gr_1
  needsecurity ~ a3*gr_1
  
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


simple.fit <- sem(simple, data = forsem, estimator = "ML"
                #, 
               # missing = "FIML"
                #, se = "bootstrap",bootstrap = 5000L, 
                #parallel ="multicore", verbose= T
                )
summary(simple.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)




rsquareCalc <- function (model, y, x, adj = FALSE, effN = FALSE, silent =
                           FALSE) {
  #model is a model fit by lavaan using e.g., the sem() or lavaan ()function.
  #Y is a character vector of length 1 specifying the name of the (single) structural outcome of interest.
  #x is a vector of one or more character strings specifying the name (s) of the target predictor(s) of interest, to be omitted from the reduced model.
  #adj: do you want to calculate adjusted rather than unadjusted R2 and R2 change? Defaults to FALSE.
  #effN: if TRUE, N in the adjusted R-square calculation is set to the lowest effective N in the structural regression. Defaults to FALSE.
  #silent: if TRUE, output does not automatically print (but is returned as invisible). Defaults to FALSE.
  #This argument is invoked when using rsquareCalc.Boot in order to turn off default printing while taking bootstrap resamples.
  require ("lavaan")
  if(!is.character (y) |length (y) != 1) stop("y must be a character vector
of length 1, specifying the name of the DV in the (manifest or latent variable) regression of interest!")
  #parameter estimates
  pe <- parameterEstimates (model, standardized = TRUE, rsquare = TRUE)
  #correlation matrix of all variables
  Rmat <- lavInspect (model, what = "cor.all")
  
  #regression coefficients
  Gamma <- pe[pe$lhs == y & pe$op == "~", ]
  
  #names of X variables NOT specified in x 
  otherXnames <- Gamma [! (Gamma$rhs %in% x), "rhs"]
  
  #Grab correlation matrix of other Xs.
  Rxx <- Rmat[otherXnames, otherXnames, drop = FALSE]
  
  #Inverse X cor mat.
  RxxInv <- solve (Rxx)
  #vector of xy correlations.
  Rxy <- Rmat [otherXnames, y, drop = FALSE] 
  #this way preserves the correct order of the other x names
  #compute new gammas as they would have been without the variables in x included in the model.
  #gamma = RxxInv%*Rxy
  GammaNew <- RxxInv%*%Rxy
  #R square of the submodel
  RsqReduced <- t (GammaNew) %*%Rxy
  #R square from Full model
  RsqFull <- pe[pe$lhs == y & pe$op == "r2", "est"]
  if (adj) {
    #If adjusted R-square is requested
    #Retrieve number of observations used in the analysis.
    n <- lavInspect (model, what = "nobs")
    #Number of predictors in the full model. 
    pFull <- nrow (pe[pe$lhs == y & pe$op == "~",])
    #Number of predictors contributing to increment in R-squared.
    pInc <- length (x)
    #Reducted model p = pFull - pInc.
    pRed <- pFull - pInc
    if (effN) {
      #If effective N is requested, first check that the fmi is calculable.
      #To do this, the following code borrows from lavaan'sinternal code.
      ###Code taken from parameterEstimates () function:
      PT <- parTable (model)
      EM.cov <- lavInspect (model, "sampstat.h1")$cov
      EM.mean <- lavInspect (model, "sampstat.h1")$mean 
      this.options <- model@Options 
      this.options$optim.method <- "none"
      this.options$sample.cov.rescale <- FALSE
      this.options$check.gradient <- FALSE
      this.options
      this.options$baseline <- FALSE
      this.options$h1 <- FALSE 
      this.options$test <- FALSE
      fit.complete <- lavaan (model = PT, sample.cov = EM.cov,
                              sample.mean = EM.mean, sample.nobs = n, slotOptions = this.options) 
      ###
      #Check that the complete model is identified:
      if (any (eigen (lavInspect (fit.complete, what = "vcov") ) $values
               <0)) {
        #If the model used to estimate the fmi is non-identified, everything is NA.
        res <- rep (NA, 2)
        names (res)<- c (paste ("Rsquare Without ", paste0 (x,
                                                            collapse = " "), collapse = ""), "RsquareChange")
      }else{
        #Otherwise, the calculations proceed
        #peFMI = parameter estimates with fmi
        peFMI <- parameterEstimates (model, standardized = TRUE,
                                     rsquare = TRUE, fmi = TRUE)
        #Flag regression relationships with proper dv: 
        regressionflag <- peFMI$lhs == y & peFMI$op == "~"
        #Flag the residual variance as well
        residvarflag <- peFMI$lhs == y & peFMI$op == "~~" &peFMI$rhs == y
        
        #Subset the parameter estimates object to include all contributors to predicted and residual variance. 
        peFMI_sub <- peFMI[regressionflag|residvarflag,]
        #Retrieve max fmi from structural model.
        fmi < peFMI_sub[which.max(peFMI_sub$fmi), "fmi"]
        #Calculate the effective n
        effN <- n* (1-fmi)
        #Overwrite the original n for use in subsequent calculations.
        n <- effN
        #Adjusted R-square calculations.
        multiplierFull <- (n-1) /(n - pFull - 1) 
        multiplierRed <- (n-1) / (n - pRed - 1)
        RsqReduced <- 1 - multiplierRed* (1 - RsqReduced)
        RsqFull <- 1 - multiplierFull*(1 - RsqFull)
        #R square change is difference between overall Rsquare and reduced R square.
        RsgChange <- RsqFull - RsqReduced
        res < - c(RsqReduced, RsqChange)
        names (res) <- c(paste ("Rsquare Without ", paste0 (x,
                                                            collapse = " "), collapse = ""),
                         "RsquareChange")
      }
    }else{        
      #If effN == FALSE, we proceed with the overall n.
      #Adjusted R-square calculations.
      multiplierFull <- (n-1)/(n - pFull - 1) 
      multiplierRed <- (n-1) / (n - pRed - 1)
      RsqReduced <- 1 - multiplierRed* (1 - RsqReduced)
      RsqFull <- 1 - multiplierFull*(1 - RsqFull)
      #R square change is difference between overall R square and reduced R square.
      RsqChange <- RsqFull - RsqReduced
      res <- c (RsqReduced, RsqChange)
      names (res) <- c (paste ("Rsquare Without ", paste0 (x,
                                                           collapse = " "), collapse = ""), "RsquareChange")
    }
  }else{
    #Otherwise, simply calculate R-square change without the adjustment terms.
    RsqChange <- RsqFull - RsqReduced
    res <- c (RsqReduced, RsqChange)
    names (res) <- c (paste("Rsquare Without ", paste0(x, collapse = "
                "), collapse = ""),"RsquareChange")
  }
  #if silent printing is not requested, print the result.
  if(!silent) print (round(res, 2))
  #And return the object.
  invisible (res)
}                      


