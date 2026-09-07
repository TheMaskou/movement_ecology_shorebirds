# ch1_4_antenna_table.R — Generate antenna deployment table from sql.motus
#
# Seeds the manually-maintained antenna record-keeping spreadsheet (kept on
# SharePoint) with the antenna configurations already entered into Motus, so
# past deployments don't need to be re-typed by hand from
# Motus > Manage Stations. Future deployments get added to the spreadsheet
# manually, mirroring what's entered on the Motus web form.
#
# This is a seed for a hand-maintained sheet, not an analysis artifact: it's
# a plain, faithful dump of recvDeps x antDeps at the Motus grain (one row
# per deployment x port). No collapsing, deduplication, or derived
# continuity columns — station uptime / deployment-period continuity is
# tracked elsewhere, not here.
#
# DEPENDENCIES:
#   - qmd/chapter_1/R/globals.R (motus_proj_num, station_rename, path_motus_database,
#     path_antenna_table_generated)
#   - data/motus/project-294.motus (SQLite database; recvDeps, antDeps tables)
# PRODUCES:
#   - data/motus/array_maintenance/antenna_table_generated.csv (df.antennas)

# ==== Setup ====

library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(lubridate)
library(readr)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

sql.motus <- dbConnect(SQLite(), path_motus_database)

path_antenna_table_generated <- here::here("data", "motus", "array_maintenance", "antenna_table_generated.csv")

# ==== Load Tables ====

tbl.recvDeps <- sql.motus |> tbl("recvDeps") |> collect()
tbl.antDeps  <- sql.motus |> tbl("antDeps")  |> collect()

# ==== Join and Filter ====

# recvDeps is the "left" table (not antDeps) so that deployments with no
# antenna record in Motus still appear as a row with blank antenna columns,
# instead of silently disappearing. antDeps also holds rows for other
# projects sharing this .motus file, so filter to ours (motus_proj_num)
# before joining.
df.antennas <- tbl.recvDeps |>
  filter(projectID == motus_proj_num) |>
  left_join(tbl.antDeps, by = join_by(deployID))

# ==== Derive Columns ====

df.antennas <- df.antennas |>
  mutate(
    # NOTE: kept in UTC deliberately, NOT converted to Australia/Sydney (unlike
    # most of chapter 1, e.g. ch1_1_load_format.R). The Motus "Manage Stations"
    # webpage displays install/removal dates in UTC, and this table exists to
    # be cross-checked against that page by eye — converting to local time
    # would shift dates and break that comparison for anything installed after
    # ~1pm AEDT / ~2pm AEST.
    install_datetime_utc = as_datetime(tsStart, tz = "UTC"),
    removal_datetime_utc = as_datetime(tsEnd,   tz = "UTC"),
    install_date = as_date(install_datetime_utc),
    removal_date = as_date(removal_datetime_utc),

    motus_name = name,
    site = recode(motus_name, !!!station_rename),

    # Motus stores a single "bearing" field that the handbook itself describes
    # as "magnetic OR true bearing" with no record of which was used. Split
    # into two columns rather than guess:
    #   - bearing_true_motus: whatever Motus has on file (unverified re:
    #     convention — some values here look computed from the map tool
    #     (many decimal places), others look like typed compass readings).
    #   - bearing_magnetic: blank here; filled in by hand from a compass
    #     reading taken on site. Deliberately NOT derived from
    #     bearing_true_motus — that conversion needs a magnetic declination
    #     value for the specific site and date (declination drifts over
    #     time), which nothing in this script assumes or looks up.
    bearing_true_motus = bearing,
    bearing_magnetic = NA_real_,

    port_num = as.integer(port)
  )

# ==== Assemble Output ====

df.antennas <- df.antennas |>
  select(
    # Site
    stationID, site, motus_name, latitude, longitude, elevation, fixtureType,
    # Deployment
    deployID, serno, status,
    # Dates
    install_date, removal_date, install_datetime_utc, removal_datetime_utc,
    # Antenna
    port, antFreq, antennaType, bearing_magnetic, bearing_true_motus, heightMeters,
    # Future use — present in antDeps but empty for project 294; kept as
    # blank columns in case they're useful to record for future installs.
    cableLengthMeters, cableType, mountDistanceMeters, mountBearing,
    # sort helper only
    port_num
  ) |>
  arrange(site, install_datetime_utc, port_num) |>
  select(-port_num)

df.antennas |> glimpse()

# ==== Save ====

# write_excel_csv (not write_csv) emits a UTF-8 BOM so Excel opens the file
# correctly when synced back from SharePoint. Dates are written ISO
# (YYYY-MM-DD), which Excel parses unambiguously regardless of locale.
write_excel_csv(df.antennas, path_antenna_table_generated)

dbDisconnect(sql.motus)
