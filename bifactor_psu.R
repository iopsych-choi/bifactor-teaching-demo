# =====================================================================
# Teaching Demo: Hierarchical Modeling in SEM (Bifactor Predictive Model)
# Jinsoo Choi, Ph.D.
#
# Data: bifactor_demo_data.csv (simulated, N = 300)
#   x1  - x12 : 12 conscientiousness items
#               S1 (Industriousness) = x1-x4, S2 (Self-control) = x5-x8,
#               S3 (Responsibility)  = x9-x12; G loads on all 12
#   aug       : augmenting indicator (loads on G only)
#   js        : job satisfaction (outcome)
#
# True standardized effects on js: G = .40, S1 = .25, S2 = .00, S3 = .10
# =====================================================================

library(lavaan)

dat <- read.csv("bifactor_data.csv")
head(dat)

# ---------------------------------------------------------------------
# Code 0: Single Factor Model
# ---------------------------------------------------------------------
m_one <- '
  # measurement: one factor for all 12 items
  Consc =~ x1 + x2 + x3 + x4 + x5 + x6 +
          x7 + x8 + x9 + x10 + x11 + x12
          
  # structural
  js ~ Consc
'

fit_one <- sem(m_one, data =dat)
summary(fit_one, fit.measures = TRUE, standardized = TRUE)


# ---------------------------------------------------------------------
# Code 1: Classic Bifactor Predictive Model
# ---------------------------------------------------------------------
m_classic <- '
  # measurement
  G  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  S1 =~ x1 + x2 + x3 + x4
  S2 =~ x5 + x6 + x7 + x8
  S3 =~ x9 + x10 + x11 + x12

  # G, S1-S3 uncorrelated
  G  ~~ 0*S1 + 0*S2 + 0*S3
  S1 ~~ 0*S2 + 0*S3
  S2 ~~ 0*S3

  # structural
  js ~ G + S1 + S2 + S3
'
fit_classic <- sem(m_classic, data = dat)
# -> Warning: some estimated ov variances are negative
summary(fit_classic, fit.measures = TRUE, standardized = TRUE)


# ---------------------------------------------------------------------
# Code 2: Augmented Bifactor Model
# ---------------------------------------------------------------------
m_aug <- '
  # measurement
  G  =~ aug + x1 + x2 + x3 + x4 + x5 + x6
        + x7 + x8 + x9 + x10 + x11 + x12
  S1 =~ x1 + x2 + x3 + x4
  S2 =~ x5 + x6 + x7 + x8
  S3 =~ x9 + x10 + x11 + x12

  # G, S1-S3 uncorrelated
  G  ~~ 0*S1 + 0*S2 + 0*S3
  S1 ~~ 0*S2 + 0*S3
  S2 ~~ 0*S3

  # structural
  js ~ G + S1 + S2 + S3
'
fit_aug <- sem(m_aug, data = dat)
summary(fit_aug, fit.measures = TRUE, standardized = TRUE)


# ---------------------------------------------------------------------
# Code 3: Structural After Measurement (SAM)
# Preview only ??? not covered in this teaching demo
# ---------------------------------------------------------------------
fit_sam <- sam(m_classic,          # same syntax as Code 1
               data       = dat,
               std.lv     = TRUE,
               mm.list    = list(IV = c("G", "S1", "S2", "S3")),
               sam.method = "global")

summary(fit_sam, standardized = TRUE)


# ---------------------------------------------------------------------
# Results table (slide 30): standardized estimates (SE)
# ---------------------------------------------------------------------
get_beta <- function(fit) {
  s <- standardizedSolution(fit)
  s <- s[s$lhs == "js" & s$op == "~", c("rhs", "est.std", "se")]
  sprintf("%.2f (%.2f)", s$est.std, s$se)
}

results <- data.frame(
  Predictor = c("G", "S1", "S2", "S3"),
  True      = c(.40, .25, .00, .10),
  Classic   = get_beta(fit_classic),
  Augmented = get_beta(fit_aug),
  SAM       = get_beta(fit_sam)
)
print(results, row.names = FALSE)
