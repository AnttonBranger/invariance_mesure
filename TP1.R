options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
install.packages("lavaan")

library(lavaan)
library(semTools)

# Exercice 1

data("HolzingerSwineford1939")

formula <- '## Definition of relationships between items and common factors
              #! NA* allows the first loading of a common factor to freely estimated, otherwise lavaan would set it to 1
              #! as an identifiability constraint (ant in that case factor variances would be estimated 
              #! using the metric of the first item)
            commonf1 =~ NA*x1 + x2 + x3 
            commonf2 =~ NA*x4 + x5 + x6
            commonf3 =~ NA*x7 + x8 + x9

            ## Estimation of common factor covariances
            commonf1 ~~ commonf2
            commonf1 ~~ commonf3
            commonf2 ~~ commonf3
            
            ## Estimation of residual variances
            x1 ~~ x1
            x2 ~~ x2
            x3 ~~ x3
            x4 ~~ x4
            x5 ~~ x5
            x6 ~~ x6
            x7 ~~ x7
            x8 ~~ x8
            x9 ~~ x9

            ## Estimation of intercepts
            x1 ~ 1
            x2 ~ 1
            x3 ~ 1
            x4 ~ 1
            x5 ~ 1
            x6 ~ 1
            x7 ~ 1
            x8 ~ 1
            x9 ~ 1
            
            ## Identifiability constraints
              # Factor variances set to 1
            commonf1 ~~ 1*commonf1
            commonf2 ~~ 1*commonf2
            commonf3 ~~ 1*commonf3
              
              # Factor means set to 0
            commonf1 ~ 0*1
            commonf2 ~ 0*1
            commonf3 ~ 0*1'


fit <- cfa(model=formula, data=HolzingerSwineford1939)

summary(fit, fit.measures=TRUE, standardized=TRUE)


formula2 <- '## Definition of relationships between items and common factors
              #! NA* allows the first loading of a common factor to freely estimated, otherwise lavaan would set it to 1
              #! as an identifiability constraint (ant in that case factor variances would be estimated 
              #! using the metric of the first item)
            commonf1 =~ NA*x1 + x2 + x3 
            commonf2 =~ NA*x4 + x5 + x6
            commonf3 =~ NA*x7 + x8 + x9

            ## Estimation of common factor covariances
            commonf1 ~~ commonf2
            commonf1 ~~ commonf3
            commonf2 ~~ commonf3
            
            ## Estimation of residual variances
            x1 ~~ x1
            x2 ~~ x2
            x3 ~~ x3
            x4 ~~ x4
            x5 ~~ x5
            x6 ~~ x6
            x7 ~~ x7
            x8 ~~ x8
            x9 ~~ x9

            ## Estimation of intercepts
            x1 ~ 1
            x2 ~ 1
            x3 ~ 1
            x4 ~ 1
            x5 ~ 1
            x6 ~ 1
            x7 ~ 1
            x8 ~ 1
            x9 ~ 1
            
            ## Identifiability constraints
              # Factor variances set to 1
            commonf1 ~~ 1*commonf1
            commonf2 ~~ 1*commonf2
            commonf3 ~~ 1*commonf3'

fit3 <- cfa(model=formula2, data=HolzingerSwineford1939, std.lv=TRUE, meanstructure=TRUE, 
            orthogonal=FALSE, estimator="ML")
summary(fit3, fit.measures=TRUE, standardized=TRUE)

modindices(fit3, sort = T)


## Exercice 2

library(readr)

data <- read_delim("M2SDDP_QOL.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

formula <- '## Definition of relationships between items and common factors
              #! NA* allows the first loading of a common factor to freely estimated, otherwise lavaan would set it to 1
              #! as an identifiability constraint (ant in that case factor variances would be estimated 
              #! using the metric of the first item)
            commonf1 =~ NA*x1 + x2 + x3 
            commonf2 =~ NA*x4 + x5 + x6
            commonf3 =~ NA*x7 + x8 + x9

            ## Estimation of common factor covariances
            commonf1 ~~ commonf2
            commonf1 ~~ commonf3
            commonf2 ~~ commonf3
            
            ## Estimation of residual variances
            x1 ~~ x1
            x2 ~~ x2
            x3 ~~ x3
            x4 ~~ x4
            x5 ~~ x5
            x6 ~~ x6
            x7 ~~ x7
            x8 ~~ x8
            x9 ~~ x9

            ## Estimation of intercepts
            x1 ~ 1
            x2 ~ 1
            x3 ~ 1
            x4 ~ 1
            x5 ~ 1
            x6 ~ 1
            x7 ~ 1
            x8 ~ 1
            x9 ~ 1
            
            ## Identifiability constraints
              # Factor variances set to 1
            commonf1 ~~ 1*commonf1
            commonf2 ~~ 1*commonf2
            commonf3 ~~ 1*commonf3
              
              # Factor means set to 0
            commonf1 ~ 0*1
            commonf2 ~ 0*1
            commonf3 ~ 0*1'


fit <- cfa(model=formula, data=HolzingerSwineford1939)

summary(fit, fit.measures=TRUE, standardized=TRUE)

# Exercice 2

formula <- '## Definition of relationships between items and common factors
              #! NA* allows the first loading of a common factor to freely estimated, otherwise lavaan would set it to 1
              #! as an identifiability constraint (ant in that case factor variances would be estimated 
              #! using the metric of the first item)
            commonf1 =~ NA*eqol_1 + eqol_2 + eqol_3 + eqol_4 + eqol_5 + eqol_6
            commonf2 =~ NA*eqol_7 + eqol_8 + eqol_9
            commonf3 =~ NA*eqol_10 + eqol_11 + eqol_12 + eqol_13
            commonf4 =~ NA*eqol_14 + eqol_15 + eqol_16 + eqol_17 + eqol_18 + eqol_19 + eqol_20

            ## Estimation of common factor covariances
            commonf1 ~~ commonf2
            commonf1 ~~ commonf3
            commonf1 ~~ commonf4
            commonf2 ~~ commonf3
            commonf2 ~~ commonf4
            commonf4 ~~ commonf3
            
            ## Estimation of residual variances
            eqol_1 ~~ eqol_1
            eqol_2 ~~ eqol_2
            eqol_3 ~~ eqol_3
            eqol_4 ~~ eqol_4
            eqol_5 ~~ eqol_5
            eqol_6 ~~ eqol_6
            eqol_7 ~~ eqol_7
            eqol_8 ~~ eqol_8
            eqol_9 ~~ eqol_9
            eqol_10 ~~ eqol_10
            eqol_11 ~~ eqol_11
            eqol_12 ~~ eqol_12
            eqol_13 ~~ eqol_13
            eqol_14 ~~ eqol_14
            eqol_15 ~~ eqol_15
            eqol_16 ~~ eqol_16
            eqol_17 ~~ eqol_17
            eqol_18 ~~ eqol_18
            eqol_19 ~~ eqol_19
            eqol_20 ~~ eqol_20

            ## Estimation of intercepts
            eqol_1 ~ 1
            eqol_2 ~ 1
            eqol_3 ~ 1
            eqol_4 ~ 1
            eqol_5 ~ 1
            eqol_6 ~ 1
            eqol_7 ~ 1
            eqol_8 ~ 1
            eqol_9 ~ 1
            eqol_10 ~ 1
            eqol_11 ~ 1
            eqol_12 ~ 1
            eqol_13 ~ 1
            eqol_14 ~ 1
            eqol_15 ~ 1
            eqol_16 ~ 1
            eqol_17 ~ 1
            eqol_18 ~ 1
            eqol_19 ~ 1
            eqol_20 ~ 1
            
            ## Identifiability constraints
              # Factor variances set to 1
            commonf1 ~~ 1*commonf1
            commonf2 ~~ 1*commonf2
            commonf3 ~~ 1*commonf3
            commonf4 ~~ 1*commonf4'

fit3 <- cfa(model=formula, data=data, std.lv=TRUE, meanstructure=TRUE, 
            orthogonal=FALSE, estimator="ML")
summary(fit3, fit.measures=TRUE, standardized=TRUE)

##########################################
### 3rd Exercice :  Intergroup ivnvariance- 1H15
# Fitting a multiCFA model (two groups, two factors per group) 
#! Groups are defined by age=1 (<= 29 years old) and age=4 (between 55 and 64 years old)

datasati <- read.csv2("M2SDDP_MultiGroups_Satisfaction.csv", na.string="NA")

dim(datasati)
head(datasati)

## 3.1 Defining and testing the model
formula <- 
  'sati_social =~ s_rela + s_relf
  sati_autre =~ s_trav + s_logt + s_lois'

fit <- cfa(model=formula, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML")
summary(fit, fit.measures=TRUE, standardized=TRUE)



##########################################
### Testing for between-groups measurement invariance


formula <- 'sati_social =~ s_rela + s_relf
            sati_autre =~ s_trav + s_logt + s_lois'

## 3.3 Testing configfural invariance = Fitting a multi-group unconstrained CFA model (same than 3)
fitmulti1 <- cfa(model=formula, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML",
                 group="agecl")
summary(fitmulti1, fit.measure = T)

## 3.4 Testing metric invariance : constraints on factor loadings
# 3.4.1 Defining a formula with equality constraints on the factor loadings

#! In lavaan there is the possibility to specify a 'label" for a parameter by naming it
#! before a variable using the multiplier convention. E.g., wrinting myLabel*x1 leads
#! to name the loading corresponding to the variable x1 "myLabel"
#! This can be used to impose equality constraints. If two parameters have the same label
#! they are constrained to be estimated at the same value
#! Here by specifying using a vector that one factor loading has the same label e.g., c(s1, s1)
#! the loading will be constrained to be equal in each groups
formula1 <- '# Constraints on loadings to be equal across groups for each items
             sati_social =~ c(s1, s1)*s_rela + c(s2, s2)*s_relf
             sati_autre =~ c(s3, s3)*s_trav + c(s4, s4)*s_logt + c(s5, s5)*s_lois

             # Common factor variances can be estimated in the second group because of equality
             # constraints on factor loadings c(1, NA) multiplier leads to fix factor variance in the first group
             # and free estimation in the second one
             sati_social ~~ c(1, NA)*sati_social
             sati_autre ~~ c(1, NA)*sati_autre'

# 3.4.2 Fitting the model with equality constraints


fitmulti2 <- cfa(model=formula1, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML",
                 group="agecl")




# Atlernatively, this can also be achieved by adding a group.equal argument in the CFA function. 

formula <- '# We use the simple notation for cfa models
            sati_social =~ s_rela + s_relf
            sati_autre =~ s_trav + s_logt + s_lois'
fitmulti2alt <- cfa(model=formula, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML",
                    group="agecl", group.equal=c("loadings"))

# 3.4.3 Assessing fit and comparing fit to the previous model
#! Compared to the previous model, Chi square has increased (expected, the second model is simpler), RMSEA has decreased,
#! CFI and TLI has increased
#! LRT is non significant
#! Metric invariance can be considered achieved
summary(fitmulti2alt, fit.measures=TRUE, standardized=TRUE)
summary(compareFit(fitmulti1,fitmulti2alt))



## 3.5 Testing scalar invariance :  adding constraints on intercepts
formula2 <- '# Constraints on loadings to be equal across groups for each items
             sati_social =~ c(s1, s1)*s_rela + c(s2, s2)*s_relf
             sati_autre =~ c(s3, s3)*s_trav + c(s4, s4)*s_logt + c(s5, s5)*s_lois
             
             # Constraints on intercepts to be equal across groups for each items
             s_rela ~ c(i1, i1)*1
             s_relf ~ c(i2, i2)*1
             s_trav ~ c(i3, i3)*1
             s_logt ~ c(i4, i4)*1
             s_lois ~ c(i5, i5)*1

             # Common factor variances can be estimated in the second group because of equality constraints
             sati_social ~~ c(1, NA)*sati_social
             sati_autre ~~ c(1, NA)*sati_autre

             # Common factor means can be estimated in the second group because of equality constraints
             # on intercepts
             sati_social ~ c(0, NA)*1
             sati_autre ~ c(0, NA) *1'

fitmulti3 <- cfa(model=formula2, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML",
                 group="agecl")

#! Compared to metric invariance model, chi-square has increased a lot, RMSEA has increased a lot
#! CFI has decreased, SRMR has increased
#! LRT is significant
#! Scalar invariance can not be considered achieved
summary(fitmulti3, fit.measures=TRUE, standardized=TRUE)
summary(compareFit(fitmulti2,fitmulti3))

#! Fitting the model could also be done this way
formula2alt <- '# Constraints on loadings to be equal across groups for each items
                sati_social =~ s_rela + s_relf
                sati_autre =~ s_trav + s_logt + s_lois

                # Common factor variances can be estimated in the second group because of equality constraints
                sati_social ~~ c(1, NA)*sati_social
                sati_autre ~~ c(1, NA)*sati_autre

                # Common factor means can be estimated in the second group because of equality constraints
                # on intercepts
                sati_social ~ c(0, NA)*1
                sati_autre ~ c(0, NA) *1'

fitmulti3alt <- cfa(model=formula2, data=datasati, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML",
                    group="agecl", group.equal=c("loadings", "intercepts"))
summary(fitmulti3alt, fit.measures=TRUE, standardized=TRUE)

modi <- lavTestScore(fitmulti3)
x <- order(modi[[2]]$X2, decreasing=TRUE)
modi <- modi[[2]][x, ]
modi #! parameters are named after the plabel column of the parameter table of the model
#! To identify to which parameters the modification indices table refers, there is a need to consult the
#! parameter table of the model
parTable(fitmulti3)


# Exercice 4 

dataQRCP <- read.csv2("M2SDDP_QRCP.csv",sep = ";")


modelQRCP <-'
#latent variable
RYt  =~ QRCP_Rythme_1 + QRCP_Rythme_2 + QRCP_Rythme_3 + QRCP_Rythme_4 
ChangTache =~ QRCP_ChangTache_1_REV + QRCP_ChangTache_2 + QRCP_ChangTache_3 + QRCP_ChangTache_4 
Com  =~ QRCP_Com_1 + QRCP_Com_2 + QRCP_Com_3 + QRCP_Com_4
Indep =~ QRCP_indep_1 + QRCP_indep_2 + QRCP_indep_3 + QRCP_indep_4
'

fitQRCP<-cfa(modelQRCP,data=dataQRCP,ordered=F,std.lv=T)
summary(fitQRCP, fit.measures=TRUE, standardized=TRUE)

## The fit is pretty good, except RMSEA which is slightly over the threshold...Let's try adding residual covariances

modindices(fitQRCP,sort=T)

modelQRCP2 <-'
#latent variable
RYt  =~ QRCP_Rythme_1 + QRCP_Rythme_2 + QRCP_Rythme_3 + QRCP_Rythme_4 
ChangTache =~ QRCP_ChangTache_1_REV + QRCP_ChangTache_2 + QRCP_ChangTache_3 + QRCP_ChangTache_4 
Com  =~ QRCP_Com_1 + QRCP_Com_2 + QRCP_Com_3 + QRCP_Com_4
Indep =~ QRCP_indep_1 + QRCP_indep_2 + QRCP_indep_3 + QRCP_indep_4

QRCP_indep_3 ~~ QRCP_indep_4
'

fitQRCP2<-cfa(modelQRCP2,data=dataQRCP,ordered=F,std.lv=T)
summary(fitQRCP2, fit.measures=TRUE, standardized=TRUE)

# Adding one residual is enough to make the CFA well fitted to the data :) 

# Let's try to investigate configural invariance 

fitQRCP2.configural<-cfa(modelQRCP2,data=dataQRCP,ordered=F,std.lv=T,group ="Genre")
summary(fitQRCP2.configural, fit.measures=TRUE, standardized=TRUE)

# It seems the configural invariance is achieved - hence we can move to metric invariance 


fitQRCP2.metric<-cfa(modelQRCP2,data=dataQRCP,ordered=F,std.lv=T,group ="Genre",group.equal=c("loadings"))
summary(fitQRCP2.metric, fit.measures=TRUE, standardized=TRUE)
summary(compareFit(fitQRCP2.configural,fitQRCP2.metric))

# Metric invariance is also achieved :) - Go for Scalar invariance 

fitQRCP2.scalar<-cfa(modelQRCP2,data=dataQRCP,ordered=F,std.lv=T,group ="Genre",group.equal=c("intercepts","loadings"))
summary(fitQRCP2.scalar, fit.measures=TRUE, standardized=TRUE)
summary(compareFit(fitQRCP2.configural,fitQRCP2.metric,fitQRCP2.scalar))



# This scale is found to be invariant at the three levels ! 

# Exercice 5


formula1 <-   '# Common factors at time 1 
               phys1 =~ pf1 + rp1 + bp1 + gh1
               ment1 =~ vt1 + sf1 + re1 + mh1 
                
               # Common factors at time 2 
               phys2 =~ pf2 + rp2 + bp2 + gh2
               ment2 =~ vt2 + sf2 + re2 + mh2 

               # Covariances between the specific items of each item over time                
               pf1 ~~ pf2
               rp1 ~~ rp2
               bp1 ~~ bp2
               gh1 ~~ gh2
               vt1 ~~ vt2
               sf1 ~~ sf2
               re1 ~~ re2
               mh1 ~~ mh2'

fitlongi1 <- cfa(model=formula1, data=datapaces, orthogonal=FALSE, std.lv=TRUE, meanstructure=TRUE, estimator="ML")

summary(fitlongi1, fit.measures=TRUE, standardized=TRUE)


formula2 <-   '# Common factors at time 1 + equality constraints on loadings over time
               phys1 =~ pf1 + rp1 + bp1 + gh1
               ment1 =~ vt1 + sf1 + re1 + mh1 
                
               # Common factors at time 2 + equality constraints on loadings over time
               #! using the equal() notation as a multiplier is another way of specifying equality constraints
               phys2 =~ equal("phys1=~pf1")*pf2 + equal("phys1=~rp1")*rp2 + equal("phys1=~bp1")*bp2 + equal("phys1=~gh1")*gh2
               ment2 =~ equal("ment1=~vt1")*vt2 + equal("ment1=~sf1")*sf2 + equal("ment1=~re1")*re2 + equal("ment1=~mh1")*mh2
               
               # Equality constraints on intercepts over time
               pf2 ~ equal("pf1~1")*1
               rp2 ~ equal("rp1~1")*1
               bp2 ~ equal("bp1~1")*1
               gh2 ~ equal("gh1~1")*1
               vt2 ~ equal("vt1~1")*1
               sf2 ~ equal("sf1~1")*1
               re2 ~ equal("re1~1")*1
               mh2 ~ equal("mh1~1")*1
               
               # Equality constraints on specific factor variances over time
               pf2 ~~ equal("pf1~~pf1")*pf2
               rp2 ~~ equal("rp1~~rp1")*rp2
               bp2 ~~ equal("bp1~~bp1")*bp2
               gh2 ~~ equal("gh1~~gh1")*gh2
               vt2 ~~ equal("vt1~~vt1")*vt2
               sf2 ~~ equal("sf1~~sf1")*sf2
               re2 ~~ equal("re1~~re1")*re2
               mh2 ~~ equal("mh1~~mh1")*mh2

               # Covariances between the specific factors of each item over time                
               pf1 ~~ pf2
               rp1 ~~ rp2
               bp1 ~~ bp2
               gh1 ~~ gh2
               vt1 ~~ vt2
               sf1 ~~ sf2
               re1 ~~ re2
               mh1 ~~ mh2

               # Because of equality constraints common factor means and variances can be estimated at the second time
               phys1 ~ 0*1
               ment1 ~ 0*1
               phys2 ~ 1
               ment2 ~ 1

               phys1 ~~ 1*phys1
               ment1 ~~ 1*ment1
               phys2 ~~ phys2
               ment2 ~~ ment2'

#! The use of the generic function 'lavaan" which allows the estimation of all types of structural equation modeling models
# (CFA is a special case) is here needed for overriding some default behaviors to fit the model as needed for this step
#std.lv=FALSE avoids standardization of common factor variance at the second time of measurement but must be used in conjunction with
#auto.fix.first=FALSE to avoid the first loading of each common factor to be set to 1 by default
#because of std.lv=FALSE by default lavaan would set one intercept per common factor to be equal to 0, therefore
#int.ov.free=TRUE is needed to override the default behavior and allowing all intercepts to be estimated
#auto.var=TRUE auto.cov.lv.x=TRUE must also be specified to allow estimation of specific factor variances and
#common factor covariances

fitlongi2 <-  lavaan(model=formula2, model.type="cfa", data=datapaces, meanstructure=TRUE, 
                     int.ov.free=TRUE, orthogonal=FALSE, std.lv=FALSE, auto.fix.first=FALSE, 
                     auto.var=TRUE, auto.cov.lv.x=TRUE, estimator="ML")
