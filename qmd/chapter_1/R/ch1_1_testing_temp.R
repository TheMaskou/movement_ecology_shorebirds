# Testing how long these operations will take
# Note needs df.new to exist from ch1_1.R
library(tictoc)
library(default)

# Check if metadata is project or network-level ====
tbl(sql.motus, "projs")    %>% count() %>% collect()   # 1-3 = project-only; hundreds = all-network
tbl(sql.motus, "recvDeps") %>% count() %>% collect()   # tells you receiver scope
tbl(sql.motus, "tagDeps")  %>% count() %>% collect()   # tells you tag 

# Entire df.alltags ====

library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats)
library(lubridate)
library(bioRad)
library(purrr)
library(ggplot2)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

sql.motus <- dbConnect(SQLite(), path_motus_database)

default(toc) <- list(log = TRUE)

tbl.alltags <- tbl(sql.motus, "alltags")

tic("Entire alltags table 'collect()'")
df.alltags <- tbl.alltags |> 
  collect()
toc()

# From full table ====
## 100 rows
tic("100 rows, all fields")

tbl_alltags |> 
  head(100) |> 
  collect()

toc()

## 10000 rows
tic("10000 rows, all fields")

tbl_alltags |> 
  head(10000) |> 
  collect()

toc()

## 100000 rows
tic("100000 rows, all fields")

tbl_alltags |> 
  head(100000) |> 
  collect()

toc()

# Anti-Join Table ====
## 100 rows ====
tic("100 rows, all fields")

df.new |> head(100) |> collect()

toc()

## 100 rows ====
tic("100 rows, only hitID")

df.new |> 
  select(hitID) |> 
  head(100) |> 
  collect()

toc()

## 500 rows ====
tic("500 rows, all fields")

df.new |> head(500) |> collect()

toc()

## 500 rows ====
tic("500 rows, only hitID")

df.new |> 
  select(hitID) |> 
  head(500) |> 
  collect()

toc()

## 1000 rows ====
tic()

df.new |> head(1000) |> collect()

toc()
