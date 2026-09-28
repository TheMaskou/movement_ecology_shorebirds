

# Packages
# pkgs <- c("moonlit", "ecmwfr", "terra", "ncdf4")
# for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(moonlit)
library(ecmwfr)
library(terra)
library(ncdf4)

# Settings 
SITE_LAT   <- -32.93
SITE_LON   <- 151.78
TZ         <- "Australia/Sydney"
START_DATE <- as.Date("2023-01-01")
END_DATE   <- min(as.Date("2026-09-27"), Sys.Date() - 7)   # ERA5 lags ~5 days
EXTINCTION <- 0.28          # moonlit atmospheric extinction, sea level

# Luminous efficacy (lumens per watt of solar radiation) - Littlefair (1985)
EFF_DIRECT  <- 95           # direct beam sunlight
EFF_DIFFUSE <- 120          # diffuse skylight (blue sky / cloud)

# Night-time cloud dimming: factor = 1 - A * C^B (Kasten & Czeplak 1980)
KC_A <- 0.75
KC_B <- 3.4

ERA5_DIR <- here::here("qmd", "chapter_1", "data", "solar_radiance", "era5_tcc")          # folder where ERA5 files are cached

# Run once with your token from https://cds.climate.copernicus.eu (profile page)
ecmwfr::wf_set_key(key = "YOUR OWN TOKEN")

# Hour grid
hour_start <- seq(as.POSIXct(paste(START_DATE, "00:00"), tz = TZ),
                  as.POSIXct(paste(END_DATE,   "23:00"), tz = TZ),
                  by = "hour")
hour_mid   <- hour_start + 1800
hour_end_utc <- as.POSIXct(format(hour_start + 3600, tz = "UTC"), tz = "UTC")
message("Hours: ", length(hour_start))

# Moonlight + twilight (clear sky) at the middle of each hour
ml <- tryCatch(
  calculateMoonlightIntensity(lat = SITE_LAT, lon = SITE_LON,
                              date = hour_mid, e = EXTINCTION),
  error = function(err) NULL)
if (is.null(ml) || nrow(ml) != length(hour_mid)) {
  message("Vectorised moonlit call failed - computing hour by hour...")
  ml <- do.call(rbind, lapply(seq_along(hour_mid), function(i)
    calculateMoonlightIntensity(lat = SITE_LAT, lon = SITE_LON,
                                date = hour_mid[i], e = EXTINCTION)))
}

base <- data.frame(
  hour_start_local = hour_start,
  time_utc         = hour_end_utc,       # key to match ERA5 (end of hour)
  sun_alt_deg      = ml$sunAltDegrees,
  moon_phase       = ml$moonPhase,
  moonlight_rel    = ml$moonlightModel,
  night_clear_lux  = ml$illumination     # moon + twilight, clear sky
)

# Download ERA5 
dir.create(ERA5_DIR, showWarnings = FALSE)
area <- c(ceiling(SITE_LAT * 4) / 4 + 0.25, floor(SITE_LON * 4) / 4 - 0.25,
          floor(SITE_LAT * 4) / 4 - 0.25,   ceiling(SITE_LON * 4) / 4 + 0.25)

utc_days <- seq(START_DATE - 1, END_DATE + 1, by = "day")
utc_days <- utc_days[utc_days <= Sys.Date() - 6]
months   <- unique(format(utc_days, "%Y-%m"))

for (ym in months) {
  f <- file.path(ERA5_DIR, paste0("era5_", ym, ".nc"))
  if (file.exists(f)) next
  d <- utc_days[format(utc_days, "%Y-%m") == ym]
  message("Requesting ERA5 for ", ym, " ...")
  req <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type       = "reanalysis",
    variable           = c("surface_solar_radiation_downwards",
                           "total_sky_direct_solar_radiation_at_surface",
                           "total_cloud_cover"),
    year  = format(d[1], "%Y"), month = format(d[1], "%m"),
    day   = format(d, "%d"),    time  = sprintf("%02d:00", 0:23),
    area  = area,
    data_format = "netcdf", download_format = "unarchived",
    target = basename(f))
  tryCatch(wf_request(request = req, path = ERA5_DIR, transfer = TRUE),
           error = function(err) message("  FAILED ", ym, ": ", conditionMessage(err)))
}

# Extract nearest grid cell for each variable
# Convert a NetCDF time axis ("seconds since 1970-01-01", etc.) to POSIXct UTC
parse_nc_time <- function(x, units) {
  u <- strsplit(units, " since ")[[1]]
  origin <- as.POSIXct(substr(u[2], 1, 19), tz = "UTC",
                       tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d"))
  mult <- switch(trimws(u[1]), seconds = 1, minutes = 60, hours = 3600, days = 86400)
  origin + x * mult
}

# Read one variable at the grid cell nearest the site (ncdf4 reads the
# new-CDS 'valid_time' axis reliably; scale/offset applied automatically)
read_var <- function(f, var) {
  nc <- nc_open(f); on.exit(nc_close(nc))
  if (!var %in% names(nc$var)) return(NULL)
  dn    <- sapply(nc$var[[var]]$dim, `[[`, "name")
  lon   <- ncvar_get(nc, "longitude"); lat <- ncvar_get(nc, "latitude")
  tname <- dn[dn %in% c("valid_time", "time")][1]
  start <- rep(1, length(dn)); count <- rep(-1, length(dn))
  start[dn == "longitude"] <- which.min(abs(lon - SITE_LON)); count[dn == "longitude"] <- 1
  start[dn == "latitude"]  <- which.min(abs(lat - SITE_LAT)); count[dn == "latitude"]  <- 1
  v  <- as.vector(ncvar_get(nc, var, start = start, count = count))
  tt <- parse_nc_time(ncvar_get(nc, tname), ncatt_get(nc, tname, "units")$value)
  data.frame(time_utc = tt, value = v)
}

# The CDS returns a ZIP (one .nc for accumulated vars, one for instantaneous)
# Unzip each month into its own folder, then collect all inner .nc files
for (z in list.files(ERA5_DIR, pattern = "^era5_.*\\.zip$", full.names = TRUE)) {
  exdir <- sub("\\.zip$", "", z)
  if (!dir.exists(exdir)) unzip(z, exdir = exdir)
}
files <- sort(c(
  list.files(ERA5_DIR, pattern = "^era5_.*\\.nc$", full.names = TRUE),
  list.files(ERA5_DIR, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
))
files <- unique(files)
if (!length(files)) stop("No ERA5 files - check CDS token and licence acceptance.")

get_series <- function(var) {
  s <- do.call(rbind, lapply(files, read_var, var = var))
  s <- s[!duplicated(s$time_utc), ]
  names(s)[2] <- var
  s
}
era5 <- Reduce(function(a, b) merge(a, b, by = "time_utc", all = TRUE),
               list(get_series("ssrd"), get_series("fdir"), get_series("tcc")))

# Compute lux
out <- merge(base, era5, by = "time_utc", all.x = TRUE)
out <- out[order(out$time_utc), ]

# Hour-mean irradiance (W m-2) from hourly accumulations (J m-2)
out$ghi_Wm2     <- pmax(out$ssrd, 0) / 3600
out$direct_Wm2  <- pmin(pmax(out$fdir, 0) / 3600, out$ghi_Wm2)
out$diffuse_Wm2 <- out$ghi_Wm2 - out$direct_Wm2

out$day_lux <- out$direct_Wm2 * EFF_DIRECT + out$diffuse_Wm2 * EFF_DIFFUSE

out$tcc          <- pmin(pmax(out$tcc, 0), 1)
out$cloud_factor <- 1 - KC_A * out$tcc^KC_B
out$night_lux    <- out$night_clear_lux * out$cloud_factor

# Sun above horizon at mid-hour -> ERA5 daylight; otherwise moon + twilight
out$is_day      <- out$sun_alt_deg > 0
out$ambient_lux <- ifelse(out$is_day, out$day_lux, out$night_lux)
out$source      <- ifelse(out$is_day, "ERA5_sun", "moonlit_moon_twilight")

# moonlit's twilight formula is not valid with the sun up - blank it by day
out$night_clear_lux[out$is_day] <- NA
out$night_lux[out$is_day]       <- NA

# Save
out$hour_start_local <- format(out$hour_start_local, "%Y-%m-%d %H:%M %Z", tz = TZ)
out$hour_end_utc     <- format(out$time_utc, "%Y-%m-%d %H:%M", tz = "UTC")
out$ambiant_lux_log  <- log10(out$ambient_lux + 0.0001)
write.csv(out, here::here("qmd", "chapter_1", "data", "solar_radiance", "newcastle_natural_light_hourly.csv"), row.names = FALSE)
message("Saved ", nrow(out), " rows to ", OUT_CSV,
        " | hours missing ERA5: ", sum(is.na(out$tcc)))

# Quick check: one week, log scale
wk <- out[1:(24 * 7), ]
t  <- as.POSIXct(wk$hour_start_local, format = "%Y-%m-%d %H:%M", tz = TZ)
plot(t, pmax(wk$ambient_lux, 1e-4), type = "l", log = "y", col = "darkorange3",
     xlab = "", ylab = "Ambient natural light (lux, log)",
     main = "Newcastle NSW - hourly natural illuminance")
abline(h = c(0.001, 0.3, 1000, 1e5), lty = 3, col = "grey70")

wk <- out[1:(24 * 7), ]
t  <- as.POSIXct(wk$hour_start_local, format = "%Y-%m-%d %H:%M", tz = TZ)
y  <- log10(wk$ambient_lux + 1e-4)
plot(t, y, type = "n", ylim = c(-4, 5.5), xlab = "",
     ylab = expression(log[10]*"(lux + 0.0001)"),
     main = "Newcastle NSW - hourly natural light")
rect(t[!wk$is_day], -5, t[!wk$is_day] + 3600, 6, col = "grey92", border = NA)
refs <- c(0.0001, 0.001, 0.3, 1000, 1e5)
labs <- c("moon + overcast night", "moonless", "full moon",
          "overcast day", "full sun")
abline(h = log10(refs + 1e-4), lty = 3, col = "grey60")
text(t[10], log10(refs + 1e-4), labs, pos = 3, cex = 0.7, col = "grey40")
lines(t, y, col = "darkorange3", lwd = 1.5)

