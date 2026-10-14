## ============================================================
##  Bifactor predictive models: classic vs. augmented vs. SAM
##  Teaching demo for the Penn State I/O written by Jinsoo Choi
## ============================================================

library(lavaan)

## ------------------------------------------------------------
## 0. Read Data (simulated data)
## ------------------------------------------------------------

dat <- read.csv("bifactor_demo_data.csv")

## ------------------------------------------------------------
## 1. Classic bifactor predictive model
## ------------------------------------------------------------
m_classic <- '
  # measurement
  G  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  S1 =~ x1 + x2 + x3 + x4
  S2 =~ x5 + x6 + x7 + x8
  S3 =~ x9 + x10 + x11 + x12

  #G, S1-S3 uncorrelated
  G ~~ 0*S1 + 0*S2 + 0*S3 
  S1 ~~ 0*S2 + 0*S3
  S2 ~~ 0*S3

  # structural
  js ~ G + S1 + S2 + S3
'
fit_classic <- sem(m_classic, data = dat,
                   std.lv = TRUE)       # factor variances fixed to 1
summary(fit_classic, fit.measures = TRUE, standardized = TRUE)

## ------------------------------------------------------------
## 2. Augmented bifactor predictive model (Zhang et al., 2021)
##    One extra item that loads on G ONLY
## ------------------------------------------------------------
m_aug <- '
  # measurement
  G  =~ aug + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  S1 =~ x1 + x2 + x3 + x4
  S2 =~ x5 + x6 + x7 + x8
  S3 =~ x9 + x10 + x11 + x12

  #G, S1-S3 uncorrelated
  G ~~ 0*S1 + 0*S2 + 0*S3 
  S1 ~~ 0*S2 + 0*S3
  S2 ~~ 0*S3
  
  # structural
  js ~ G + S1 + S2 + S3
'
fit_aug <- sem(m_aug, data = dat, std.lv = TRUE, orthogonal = TRUE)
summary(fit_aug,fit.measures = TRUE, standardized = TRUE)

## ------------------------------------------------------------
## 3. Structural After Measurement (SAM; Rosseel & Loh, 2022;
##    Choi et al., 2025) 
##    Step 1: measurement block (G, S1-S3) estimated alone
##    Step 2: js ~ G + S1 + S2 + S3 with Step-1 results held fixed
## ------------------------------------------------------------

fit_sam <- sam(m_classic, data=dat, std.lv=T, 
               mm.list=list(IV=c("G", "S1", "S2", "S3")), sam.method = "global") 

summary(fit_sam, standardized = TRUE)

## ------------------------------------------------------------
## 4. Compare the structural coefficients side by side
## ------------------------------------------------------------
get_paths <- function(fit, label) {
  pe <- parameterEstimates(fit)
  pe <- pe[pe$lhs == "js" & pe$op == "~", c("rhs", "est", "se", "pvalue")]
  data.frame(model = label, pe, row.names = NULL)
}
comparison <- rbind(get_paths(fit_classic, "Classic"),
                    get_paths(fit_aug,     "Augmented"),
                    get_paths(fit_sam,     "SAM"))
comparison[, 3:5] <- round(comparison[, 3:5], 3)
print(comparison)

## Population values: G = .40, S1 = .25, S2 = .00, S3 = .10
