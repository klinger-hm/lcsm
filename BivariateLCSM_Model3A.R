library(lavaan)

Model3A <- '#### PACC MODEL SETUP

               ## modeling true scores
               lpacc1 =~ 1*PACC_1
               lpacc2 =~ 1*PACC_2
               lpacc3 =~ 1*PACC_3
               lpacc4 =~ 1*PACC_4
               lpacc5 =~ 1*PACC_5
               lpacc6 =~ 1*PACC_6
               lpacc7 =~ 1*PACC_7
               lpacc8 =~ 1*PACC_8
               lpacc9 =~ 1*PACC_9

               ## auto regressions - 1 loadings
               lpacc2 ~ 1*lpacc1
               lpacc3 ~ 1*lpacc2
               lpacc4 ~ 1*lpacc3
               lpacc5 ~ 1*lpacc4
               lpacc6 ~ 1*lpacc5
               lpacc7 ~ 1*lpacc6
               lpacc8 ~ 1*lpacc7
               lpacc9 ~ 1*lpacc8

               ## difference scores
               dpacc12 =~ 1*lpacc2
               dpacc23 =~ 1*lpacc3
               dpacc34 =~ 1*lpacc4
               dpacc45 =~ 1*lpacc5
               dpacc56 =~ 1*lpacc6
               dpacc67 =~ 1*lpacc7
               dpacc78 =~ 1*lpacc8
               dpacc89 =~ 1*lpacc9

               ## latent intercept
               pacc_i =~ 1*lpacc1
               
               ## set means to 0
               PACC_1 ~ 0*1
               PACC_2 ~ 0*1
               PACC_3 ~ 0*1
               PACC_4 ~ 0*1
               PACC_5 ~ 0*1
               PACC_6 ~ 0*1
               PACC_7 ~ 0*1
               PACC_8 ~ 0*1
               PACC_9 ~ 0*1
               
               lpacc1 ~ 0*1
               lpacc2 ~ 0*1
               lpacc3 ~ 0*1
               lpacc4 ~ 0*1
               lpacc5 ~ 0*1
               lpacc6 ~ 0*1
               lpacc7 ~ 0*1
               lpacc8 ~ 0*1
               lpacc9 ~ 0*1
               
               dpacc12 ~ 0*1
               dpacc23 ~ 0*1
               dpacc34 ~ 0*1
               dpacc45 ~ 0*1
               dpacc56 ~ 0*1
               dpacc67 ~ 0*1
               dpacc78 ~ 0*1
               dpacc89 ~ 0*1

               ## set variances to 0
               dpacc12 ~~ 0*dpacc12
               dpacc23 ~~ 0*dpacc23
               dpacc34 ~~ 0*dpacc34
               dpacc45 ~~ 0*dpacc45
               dpacc56 ~~ 0*dpacc56
               dpacc67 ~~ 0*dpacc67
               dpacc78 ~~ 0*dpacc78
               dpacc89 ~~ 0*dpacc89
               
               lpacc1 ~~ 0*lpacc1
               lpacc2 ~~ 0*lpacc2
               lpacc3 ~~ 0*lpacc3
               lpacc4 ~~ 0*lpacc4
               lpacc5 ~~ 0*lpacc5
               lpacc6 ~~ 0*lpacc6
               lpacc7 ~~ 0*lpacc7
               lpacc8 ~~ 0*lpacc8
               lpacc9 ~~ 0*lpacc9

               ## constrain residuals
               PACC_1 ~~ r1*PACC_1
               PACC_2 ~~ r1*PACC_2
               PACC_3 ~~ r1*PACC_3
               PACC_4 ~~ r1*PACC_4
               PACC_5 ~~ r1*PACC_5
               PACC_6 ~~ r1*PACC_6
               PACC_7 ~~ r1*PACC_7
               PACC_8 ~~ r1*PACC_8
               PACC_9 ~~ r1*PACC_9

               ## proportional change
               dpacc12 ~ p1*lpacc1
               dpacc23 ~ p1*lpacc2
               dpacc34 ~ p1*lpacc3
               dpacc45 ~ p1*lpacc4
               dpacc56 ~ p1*lpacc5
               dpacc67 ~ p1*lpacc6
               dpacc78 ~ p1*lpacc7
               dpacc89 ~ p1*lpacc8
               
               ## dynamic change
               dpacc23 ~ d1*dpacc12
               dpacc34 ~ d1*dpacc23
               dpacc45 ~ d1*dpacc34
               dpacc56 ~ d1*dpacc45
               dpacc67 ~ d1*dpacc56
               dpacc78 ~ d1*dpacc67
               dpacc89 ~ d1*dpacc78

               ## latent slope
               pacc_s =~ 1*dpacc12 + 1*dpacc23 + 1*dpacc34 + 1*dpacc45 + 1*dpacc56 + 1*dpacc67 + 1*dpacc78 + 1*dpacc89 + 1*dpacc89
               
               ## correlation b/w intercept and slope
               pacc_i ~~ pacc_s
               
               ## estimate intercept for pacc intercept & slope
               pacc_i ~ 1
               pacc_s ~ 1

               #### PIB MODEL SET UP
               
               ## modeling true scores
               lpib1 =~ 1*PIB_1
               lpib4 =~ 1*PIB_2
               lpib6 =~ 1*PIB_3
               lpib9 =~ 1*PIB_4
               
               ## auto regressions - 1 loadings
               lpib4 ~ 1*lpib1
               lpib6 ~ 1*lpib4
               lpib9 ~ 1*lpib6
               
               ## difference scores
               dpib14 =~ 1*lpib4
               dpib46 =~ 1*lpib6
               dpib69 =~ 1*lpib9
               
               ## latent intercept
               pib_i =~ 1*lpib1
               
               ## set means to 0
               lpib1 ~ 0*1
               lpib4 ~ 0*1
               lpib6 ~ 0*1
               lpib9 ~ 0*1

               dpib14 ~ 0*1
               dpib46 ~ 0*1
               dpib69 ~ 0*1

               PIB_1 ~ 0*1
               PIB_2 ~ 0*1
               PIB_3 ~ 0*1
               PIB_4 ~ 0*1

               ## set variances to 0
               lpib1 ~~ 0*lpib1
               lpib4 ~~ 0*lpib4
               lpib6 ~~ 0*lpib6
               lpib9 ~~ 0*lpib9

               dpib14 ~~ 0*dpib14
               dpib46 ~~ 0*dpib46
               dpib69 ~~ 0*dpib69

               ## constrain residuals
               PIB_1 ~~ r2*PIB_1
               PIB_2 ~~ r2*PIB_2
               PIB_3 ~~ r2*PIB_3
               PIB_4 ~~ r2*PIB_4
               
               ## proportional change
               dpib14 ~ p2*lpib1
               dpib46 ~ p2*lpib4
               dpib69 ~ p2*lpib6

               ## dynamic change
               dpib46 ~ d2*dpib14
               dpib69 ~ d2*dpib46

               ## latent slope
               pib_s =~ 1*dpib14+ 1*dpib46 + 1*dpib69
               
               ## correlation b/w intercept and slope
               pib_i ~~ pib_s
               
               ## estimate intercept for pib intercept & slope
               pib_i ~ 1
               pib_s ~ 1

               #### BIVARIATE MODEL
              
               ## correlations b/w slopes and intercepts
               pacc_i ~~ pib_i
               pacc_i ~~ pib_s
               pacc_s ~~ pib_i
               pacc_s ~~ pib_s

               ## correlations b/w data points - constrained
               PACC_1 ~~ c*PIB_1
               PACC_4 ~~ c*PIB_2
               PACC_6 ~~ c*PIB_3
               PACC_9 ~~ c*PIB_4
               
               ## changes in pacc on pib levels
               dpacc12 ~ beta*lpib1
               dpacc45 ~ beta*lpib4
               dpacc67 ~ beta*lpib6

               ## changes in pacc on pib changes
               dpacc45 ~ beta_delta*dpib14
               dpacc67 ~ beta_delta*dpib46
               
               ## Covariates

              pacc_i ~ PACCAge_1
              pib_i ~ PACCAge_1
              pacc_s ~ PACCAge_1
              pib_s ~ PACCAge_1
              
              pacc_i ~ SEX
              pib_i ~ SEX
              pacc_s ~ SEX
              pib_s ~ SEX
              
              pacc_i ~ YrsEd
              pib_i ~ YrsEd
              pacc_s ~ YrsEd
              pib_s ~ YrsEd
              
              pacc_i ~ E4_Status
              pib_i ~ E4_Status
              pacc_s ~ E4_Status
              pib_s ~ E4_Status

              pacc_i ~ MTL
              pib_i ~ MTL
              pacc_s ~ MTL
              pib_s ~ MTL

              pacc_i ~ FDGcomp
              pib_i ~ FDGcomp
              pacc_s ~ FDGcomp
              pib_s ~ FDGcomp'

Model3A_sem <- lavaan::sem(Model3A, data = my_data, missing = "fiml", fixed.x = FALSE)

summary(Model3A_sem, fit.measures = TRUE)