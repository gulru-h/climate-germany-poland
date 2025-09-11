
med.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3
  finance =~finan1+finan2+finan3
  
  Micronarratives ~b1*stateneedsecurity+traitneedsecurity+gr_1+finance
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+gr_1+finance
  stateneedsecurity ~ a1*gr_1+traitneedsecurity+finance
  
  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", missing = "FIML"
               # ,se = "bootstrap",bootstrap = 5000L, 
               # parallel ="multicore", verbose= T
)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)




simple.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitneedsecurity=~ tsec1+tsec2+tsec3

  Micronarratives ~b1*stateneedsecurity+traitneedsecurity+gr_1
  Mainstream ~b2*stateneedsecurity+traitneedsecurity+gr_1
  stateneedsecurity ~ a1*gr_1+traitneedsecurity
  
  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

simple.fit <- sem(simple.model, data = forsem, estimator = "ML", missing = "FIML"
               # ,se = "bootstrap",bootstrap = 5000L, 
               # parallel ="multicore", verbose= T
)
summary(simple.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)





simpler.model <- '
  stateneedsecurity =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2+narrative_4+narrative_6+narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 

  Micronarratives ~b1*stateneedsecurity+gr_1
  Mainstream ~b2*stateneedsecurity+gr_1
  stateneedsecurity ~ a1*gr_1
  
  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

simpler.fit <- sem(simpler.model, data = forsem, estimator = "ML", missing = "FIML"
                  # ,se = "bootstrap",bootstrap = 5000L, 
                  # parallel ="multicore", verbose= T
)
summary(simpler.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



vuongtest(med.fit, simple.fit)

vuongtest(simple.fit, simpler.fit)

lavInspect(med.fit, "rsquare")
lavInspect(simple.fit, "rsquare")
lavInspect(simpler.fit, "rsquare")
lavInspect(tmod.fit, "rsquare")

anova(med.fit, simple.fit)




rsquareCalc ‹- function (model, y, x, adj = FALSE, effN = FALSE, silent =
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
pe ‹- parameterEstimates (model, standardized = TRUE, square = TRUE)
#correlation matrix of all variables
Rmat <- lavInspect (model, what = "cor.all")
#regression coefficients
Gamma ‹- pe[pe$lhs == y & pe$op == "~", ]
#names of X variables NOT specified in x 
otherXnames ‹- Gamma [! (Gamma$rhs %in% x), "rhs"]
#Grab correlation matrix of other Xs.
Rxx ‹- Rmat (otherXnames, otherXnames, drop = FALSE]
#Inverse X cor mat.
RxxInv <- solve (Rxx)
#vector of xy correlations.
Rxy <- Rmat (otherXnames, y, drop = FALSE] 
#this way preserves the correct order of the other x names
#compute new gammas as they would have been without the variables in x included in the model.
#gamma = RxxInv%*Rxy
GammaNew <- RxxInv%*%Rxy
#R square of the submodel
RsqReduced <- t (GammaNew) %*%Rxy
#R square from Full model
RsqFull ‹- pe[pe$lhs == y & pe$op == "r2", "est"]
if (adj) (
  #If adjusted R-square is requested
  #Retrieve number of observations used in the analysis.
  n <- lavInspect (model, what = "nobs")
  #Number of predictors in the full model. pFull <- nrow (pe[pe$lhs == y & pe$op == "~",])
  #Number of predictors contributing to increment in R-squared.
  pInc <- length (x)
  #Reducted model p = pFull - pInc.
  pRed <- pFull - pInc
  if (effN) {
    #If effective N is requested, first check that the fmi is calculable.
    #To do this, the following code borrows from lavaan'sinternal code.
    ###Code taken from parameterEstimates () function:
    PT ‹- parTable (model)
    EM.cov <- lavInspect (model, "sampstat.h1")$cov
    EM. mean <- lavInspect (model, "sampstat.h1")$mean 
    this.options ‹- model@Options 
    this.options$optim.method <- "none"
    this.options$sample.cov.rescale <- FALSE
    this.options$check.gradient <- FALSE
    this.options
    this.options$baseline <- FALSE
    this.options$h1 ‹- FALSE 
    this.options$test <- FALSE
        fit.complete ‹- lavaan (model = PT, sample.cov = EM.cov,
                        sample.mean = EM.mean, sample.nobs = n, slotOptions = this.options) 
                          ###
                          #Check that the complete model is identified:
                          if (any (eigen (lavInspect (fit.complete, what = "vcov") ) $values
                                   <0)) {
                            #If the model used to estimate the fmi is non-identified, everything is NA.
                            res <- rep (NA, 2)
                            names (res)
                            <- c (paste ("Rsquare Without ", paste0 (x,
                                                                     collapse = " "), collapse = ""), "RsquareChange")
                            }else{
                            #Otherwise, the calculations proceed
                            #peFMI = parameter estimates with fmi
                            peFMI <- parameterEstimates (model, standardized = TRUE,
                                                         rsquare = TRUE, fmi = TRUE)
                            #Flag regression relationships with proper dv: 
                            regressionflag ‹- peFMI$lhs == y & peFMI$op == "~"
                            #Flag the residual variance as well
                            residvarflag ‹- peFMI$lhs == y & peFMI$op == "~~" &
                              peFMI$rhs == y
                            
                            #Subset the parameter estimates object to include all contributors to predicted and residual variance. 
                            peFMI_sub <- peFMI[regressionflag|residvarflag,]
                            #Retrieve max fmi from structural model.
                            fmi < peFMI_sub[which.max(peFMI_sub$fmi), "fmi"']
                            #Calculate the effective n
                            effN <- n* (1-fmi)
                            #Overwrite the original n for use in subsequent calculations.
                            n <- effN
                            #Adjusted R-square calculations.
                            multiplierFull <- (n-1) /(n - pFull - 1) 
                            multiplierRed ‹- (n-1) / (n - pRed - 1)
                            RsqReduced <- 1 - multiplierRed* (1 - RsqReduced)
                            RsqFull <- 1 - multiplierFull*(1 - RsqFull)
                            #R square change is difference between overall Rsquare and reduced R square.
                            RsgChange <- RsqFull - RsgReduced
                            res < - c(RsgReduced, RsgChange)
                            
                            
                                
