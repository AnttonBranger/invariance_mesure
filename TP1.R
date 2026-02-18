options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
install.packages("lavaan")

library(lavaan)


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
