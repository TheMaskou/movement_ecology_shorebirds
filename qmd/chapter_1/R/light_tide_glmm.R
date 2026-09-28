###############################################################################
# Which explains shorebird visits better: natural light or tide?
#   (1) number of visits per hour     (table: grid, one row per bird x hour)
#   (2) duration of each visit        (table: vd,   one row per visit)
# All species and receivers pooled.
# Random effects: bird (Band.ID) and local day of visit (date).
###############################################################################

library(glmmTMB)
library(dplyr)

TZ <- "Australia/Sydney"

# -----------------------------------------------------------------------------
# 0a. Build 'vd': one row per visit (for the DURATION model)
# -----------------------------------------------------------------------------
# Needs: visit_duration (with ambiant_lux_log and tideHeight already linked)
vd <- visit_duration %>%
  mutate(
    # local calendar day of the visit -> random effect
    date  = factor(as.Date(visitStart, tz = TZ)),
    # exact duration in hours (duration_h is rounded to 0.01 h and has zeros)
    dur_h = as.numeric(difftime(visitEnd, visitStart, units = "hours")),
    dur_h = pmax(dur_h, 1 / 3600)          # at least 1 second (Gamma needs > 0)
  )

# -----------------------------------------------------------------------------
# 0b. Build 'grid': one row per bird x hour (for the NUMBER OF VISITS model)
# -----------------------------------------------------------------------------
# For every bird, take every day it was detected at least once, and create all
# 24 hours of that day. Hours without a visit get n_visits = 0 (these zeros
# are essential: they tell the model when birds did NOT come).

# helper: convert any date-time to UTC (avoids daylight-saving problems)
to_utc <- function(x) as.POSIXct(format(x, tz = "UTC"), tz = "UTC")

# all bird-days
bird_days <- distinct(vd, Band.ID, date)

# all 24 local hours of one day
make_hours <- function(d) {
  d <- as.Date(as.character(d))
  seq(as.POSIXct(paste(d,     "00:00"), tz = TZ),
      as.POSIXct(paste(d + 1, "00:00"), tz = TZ) - 3600, by = "hour")
}

# one row per bird x hour
grid <- do.call(rbind, lapply(seq_len(nrow(bird_days)), function(i)
  data.frame(Band.ID    = bird_days$Band.ID[i],
             date       = bird_days$date[i],
             hour_local = make_hours(bird_days$date[i]))))
grid$hour_utc <- to_utc(grid$hour_local)

# number of visits STARTING in each bird-hour (0 if none)
vd$hour_utc <- as.POSIXct(trunc(to_utc(vd$visitStart), "hours"))
visits_per_hour <- count(vd, Band.ID, hour_utc, name = "n_visits")
grid <- left_join(grid, visits_per_hour, by = c("Band.ID", "hour_utc"))
grid$n_visits[is.na(grid$n_visits)] <- 0

# light for each hour (from light_h: one row per hour)
light_start_utc <- as.POSIXct(light_h$hour_end_utc, format = "%Y-%m-%d %H:%M",
                              tz = "UTC") - 3600
grid$ambiant_lux_log <- light_h$ambiant_lux_log[match(as.numeric(grid$hour_utc),
                                                      as.numeric(light_start_utc))]

# tide height at the middle of each hour (linear interpolation of tide_data)
tide_time_col <- "tideDateTimeAus"
tide_data <- tide_data[order(tide_data[[tide_time_col]]), ]
grid$tideHeight <- approx(x = as.numeric(tide_data[[tide_time_col]]),
                          y = tide_data$tideHeight,
                          xout = as.numeric(grid$hour_utc) + 1800,
                          ties = mean)$y

# quick checks
nrow(grid)                       # number of bird-hours
table(grid$n_visits)             # mostly 0, some 1, few 2+
sum(is.na(grid$ambiant_lux_log)) # hours without light data (Oct 2024 gap)
sum(is.na(grid$tideHeight))      # hours outside the tide record

# -----------------------------------------------------------------------------
# 0c. Final model data
# -----------------------------------------------------------------------------
# Keep only rows with both predictors, so all models use exactly the same rows
# (needed to compare models with AIC / likelihood-ratio tests).
grid_m <- subset(grid, !is.na(ambiant_lux_log) & !is.na(tideHeight))
vd_m   <- subset(vd,   !is.na(ambiant_lux_log) & !is.na(tideHeight))

# Standardise both predictors (mean 0, SD 1). Their estimates are then on the
# same scale, so the larger absolute estimate = the stronger effect.
grid_m$light_z <- as.numeric(scale(grid_m$ambiant_lux_log))
grid_m$tide_z  <- as.numeric(scale(grid_m$tideHeight))
vd_m$light_z   <- as.numeric(scale(vd_m$ambiant_lux_log))
vd_m$tide_z    <- as.numeric(scale(vd_m$tideHeight))

# -----------------------------------------------------------------------------
# 1. NUMBER OF VISITS per hour (count -> negative binomial)
# -----------------------------------------------------------------------------
# Four models: nothing, light only, tide only, light + tide
c_null  <- glmmTMB(n_visits ~ 1                + (1 | date) + (1 | Band.ID),
                   family = nbinom2, data = grid_m)

c_light <- glmmTMB(n_visits ~ light_z          + (1 | date) + (1 | Band.ID),
                   family = nbinom2, data = grid_m)

c_tide  <- glmmTMB(n_visits ~ tide_z           + (1 | date) + (1 | Band.ID),
                   family = nbinom2, data = grid_m)

c_both  <- glmmTMB(n_visits ~ light_z + tide_z + (1 | date) + (1 | Band.ID),
                   family = nbinom2, data = grid_m)

# Effect sizes: compare |estimate| of light_z and tide_z
summary(c_both)

# Model comparison: lowest AIC = best model (a difference > 2 is meaningful)
AIC(c_null, c_light, c_tide, c_both)

# Does each predictor add something once the other is in the model?
anova(c_tide,  c_both)   # test of LIGHT (tide already included)
anova(c_light, c_both)   # test of TIDE  (light already included)

# -----------------------------------------------------------------------------
# 2. DURATION of each visit (positive, skewed -> Gamma with log link)
# -----------------------------------------------------------------------------
# dur_h = exact duration in hours (duration_h is rounded and contains zeros,
# which a Gamma model cannot use)
d_null  <- glmmTMB(dur_h ~ 1                + (1 | date) + (1 | Band.ID),
                   family = Gamma(link = "log"), data = vd_m)
d_light <- glmmTMB(dur_h ~ light_z          + (1 | date) + (1 | Band.ID),
                   family = Gamma(link = "log"), data = vd_m)
d_tide  <- glmmTMB(dur_h ~ tide_z           + (1 | date) + (1 | Band.ID),
                   family = Gamma(link = "log"), data = vd_m)
d_both  <- glmmTMB(dur_h ~ light_z + tide_z + (1 | date) + (1 | Band.ID),
                   family = Gamma(link = "log"), data = vd_m)

summary(d_both)
AIC(d_null, d_light, d_tide, d_both)
anova(d_tide,  d_both)   # test of LIGHT
anova(d_light, d_both)   # test of TIDE

# -----------------------------------------------------------------------------
# 3. Reading the estimates
# -----------------------------------------------------------------------------
# exp(estimate) = multiplicative change for +1 SD of the predictor, e.g.
#   0.80 -> 20% fewer visits (or 20% shorter visits) per +1 SD
#   1.50 -> 50% more visits (or 50% longer visits) per +1 SD
exp(fixef(c_both)$cond)
exp(fixef(d_both)$cond)

# Size of 1 SD in real units (to report alongside the estimates)
sd(grid_m$ambiant_lux_log); sd(grid_m$tideHeight)   # count model
sd(vd_m$ambiant_lux_log);   sd(vd_m$tideHeight)     # duration model













###############################################################################
# Plots for the light vs tide models (run after light_vs_tide_simple.R)
#   Fig 1: standardised effects of light and tide (which matters more?)
#   Fig 2: predicted number of visits per hour vs light and tide
#   Fig 3: predicted visit duration vs light and tide
###############################################################################

library(ggplot2)
library(patchwork)   # to combine panels (install.packages("patchwork"))

theme_set(theme_classic(base_size = 12))
col_light <- "darkorange3"
col_tide  <- "steelblue4"

# Nice labels for the light axis: log10 lux -> lux
lux_breaks <- c(-4, -2, 0, 2, 4)
lux_labels <- c("0.0001", "0.01", "1", "100", "10,000")

# -----------------------------------------------------------------------------
# Helper: predicted curve (population level, no random effects) with 95% CI
#   model : fitted glmmTMB model with light_z + tide_z
#   dat   : data used to fit it (grid_m or vd_m), to undo the standardisation
#   var   : "light" or "tide" (the other predictor is held at its mean)
# -----------------------------------------------------------------------------
pred_curve <- function(model, dat, var) {
  raw  <- if (var == "light") dat$ambiant_lux_log else dat$tideHeight
  x    <- seq(min(raw), max(raw), length.out = 200)
  nd   <- data.frame(light_z = 0, tide_z = 0, date = NA, Band.ID = NA)[rep(1, 200), ]
  if (var == "light") nd$light_z <- (x - mean(raw)) / sd(raw)
  if (var == "tide")  nd$tide_z  <- (x - mean(raw)) / sd(raw)
  p <- predict(model, newdata = nd, re.form = NA, type = "link", se.fit = TRUE)
  data.frame(x   = x,
             fit = exp(p$fit),
             lo  = exp(p$fit - 1.96 * p$se.fit),
             hi  = exp(p$fit + 1.96 * p$se.fit))
}

plot_curve <- function(pc, raw_x, col, xlab, ylab, light_axis = FALSE) {
  g <- ggplot(pc, aes(x, fit)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = col, alpha = 0.2) +
    geom_line(colour = col, linewidth = 1) +
    geom_rug(data = data.frame(x = raw_x), aes(x = x), inherit.aes = FALSE,
             alpha = 0.05, length = unit(0.02, "npc")) +
    labs(x = xlab, y = ylab)
  if (light_axis) g <- g + scale_x_continuous(breaks = lux_breaks, labels = lux_labels)
  g
}

# -----------------------------------------------------------------------------
# Fig 1: standardised effects (+1 SD) with 95% CI
# -----------------------------------------------------------------------------
eff <- function(model, response) {
  s  <- summary(model)$coefficients$cond[c("light_z", "tide_z"), ]
  data.frame(response = response,
             term     = c("Light (ambiant_lux_log)", "Tide height"),
             est      = exp(s[, "Estimate"]),
             lo       = exp(s[, "Estimate"] - 1.96 * s[, "Std. Error"]),
             hi       = exp(s[, "Estimate"] + 1.96 * s[, "Std. Error"]))
}
eff_all <- rbind(eff(c_both, "Number of visits per hour"),
                 eff(d_both, "Visit duration"))

fig1 <- ggplot(eff_all, aes(x = est, y = term, colour = term)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.6) +
  facet_wrap(~ response) +
  scale_colour_manual(values = c(col_light, col_tide), guide = "none") +
  labs(x = "Multiplicative effect of +1 SD (1 = no effect)", y = NULL,
       title = "Effect of light and tide on shorebird visits")
fig1

# -----------------------------------------------------------------------------
# Fig 2: number of visits per hour
# -----------------------------------------------------------------------------
f2a <- plot_curve(pred_curve(c_both, grid_m, "light"), grid_m$ambiant_lux_log,
                  col_light, "Natural light (lux, log scale)",
                  "Visits starting per bird-hour", light_axis = TRUE)
f2b <- plot_curve(pred_curve(c_both, grid_m, "tide"), grid_m$tideHeight,
                  col_tide, "Tide height (m)", NULL)
fig2 <- (f2a | f2b) + plot_layout(axes = "collect") &
  coord_cartesian(ylim = c(0, NA))
fig2 + plot_annotation(title = "Number of visits: light matters, tide does not")

# -----------------------------------------------------------------------------
# Fig 3: visit duration
# -----------------------------------------------------------------------------
f3a <- plot_curve(pred_curve(d_both, vd_m, "light"), vd_m$ambiant_lux_log,
                  col_light, "Natural light at visit start (lux, log scale)",
                  "Predicted visit duration (h)", light_axis = TRUE)
f3b <- plot_curve(pred_curve(d_both, vd_m, "tide"), vd_m$tideHeight,
                  col_tide, "Tide height at visit start (m)", NULL)
fig3 <- (f3a | f3b) & coord_cartesian(ylim = c(0, NA))
fig3 + plot_annotation(title = "Visit duration: both light and tide matter")

# -----------------------------------------------------------------------------
# Residuals
# -----------------------------------------------------------------------------

plot(simulateResiduals(c_both))
plot(simulateResiduals(d_both))

hist(log10(vd_m$dur_h), breaks = 60,
     xlab = "log10(visit duration, h)", main = "Visit durations")

# histogram shows two humps (short "pass-by" visits and long stays), split the question in two: where the two humps separate on the histogram, 0.18 h (11min) as seen before
vd_m$long_visit <- vd_m$dur_h > 0.18

# (a) Is a visit long rather than short?
d_long <- glmmTMB(long_visit ~ light_z + tide_z + (1 | date) + (1 | Band.ID),
                  family = binomial, data = vd_m)

# (b) For long visits only: how long do they last?
d_len  <- glmmTMB(log(dur_h) ~ light_z + tide_z + (1 | date) + (1 | Band.ID),
                  family = gaussian, data = subset(vd_m, long_visit))

summary(d_long); summary(d_len)
plot(simulateResiduals(d_long)); plot(simulateResiduals(d_len))

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------
# ggsave("fig1_effects.png",  fig1, width = 8, height = 3.5, dpi = 300)
# ggsave("fig2_visits.png",   fig2, width = 8, height = 3.5, dpi = 300)
# ggsave("fig3_duration.png", fig3, width = 8, height = 3.5, dpi = 300)
