# ---------------------------------------
# Assignment 1 — Data Import (FAST, local CAMELS Daymet)
# Student: Zoe Kindt (xkinz003)
# ---------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readr); library(dplyr)
  library(lubridate); library(stringr)
})


base_dir <- "C:/Users/zkind/OneDrive/Documents/DMAVR/basin_timeseries_v1p2_modelOutput_daymet (1)"

# Output CSV to my DMVAR folder
csv_out  <- "C:/Users/zkind/OneDrive/Documents/DMAVR/runoff_data.csv"

# Fast mode reads only a subset I can test quickly 
FAST_MODE       <- TRUE     
MAX_FILES       <- 400      
PROGRESS_EVERY  <- 50       # print a progress message every N files

# restrict to specific basin IDs to be even faster (character vector)
BASIN_WHITELIST <- NULL     # e.g., c("1013500","06888500")

# ==== CHECKS ================================================================
if (!dir.exists(base_dir)) stop("Folder not found: ", base_dir)

# ==== FILE DISCOVERY (fast and focused) =====================================
all_files <- list.files(
  base_dir,
  pattern    = "\\.(txt|csv)$",
  recursive  = TRUE,
  full.names = TRUE
)

# Prefer files that look like actual time series (Daymet/flow)
cand <- all_files[grepl("model_output|timeseries|daymet|flow|runoff|discharge",
                        tolower(all_files))]

# keep only files matching chosen basins
if (!is.null(BASIN_WHITELIST)) {
  pat <- paste0("(", paste(BASIN_WHITELIST, collapse="|"), ")")
  cand <- cand[grepl(pat, cand)]
}

if (!length(cand)) stop("No time-series files found under: ", base_dir)

if (FAST_MODE) cand <- head(cand, MAX_FILES)
cat("Files to read:", length(cand), "from", base_dir, "\n")

# ==== HELPERS =====================================================================
read_guess <- function(f) {
  dt <- tryCatch(fread(f, showProgress = FALSE), error = function(e) NULL)
  if (is.null(dt)) {
    dt <- tryCatch(readr::read_table(f, show_col_types = FALSE) |> as.data.table(),
                   error = function(e) NULL)
  }
  dt
}

normalize_dates <- function(DT) {
  setDT(DT); cn <- names(DT); lcn <- tolower(cn)
  if ("date" %in% lcn) {
    dc <- cn[match("date", lcn)]
    DT[, (dc) := as.Date(get(dc))]
    if (dc != "date") setnames(DT, dc, "date")
  } else if (all(c("year","month","day") %in% lcn)) {
    y <- cn[match("year",lcn)]; m <- cn[match("month",lcn)]; d <- cn[match("day",lcn)]
    set(DT, j="date", value = as.Date(sprintf("%04d-%02d-%02d", DT[[y]], DT[[m]], DT[[d]])))
  } else if ("t" %in% lcn) {
    tcol <- cn[match("t",lcn)]
    set(DT, j="date", value = as.Date(DT[[tcol]]))
  } else {
    set(DT, j="date", value = as.Date("2000-01-01") + seq_len(nrow(DT)) - 1L)
  }
  DT
}

rename_common <- function(DT) {
  col_map <- list(
    PRCP = c("prcp","precip","precipitation","ppt","p","rain","pr"),
    TMIN = c("tmin","temp_min","t_min"),
    TMAX = c("tmax","temp_max","t_max"),
    SWE  = c("swe","snow","snow_water_equiv","snow_water_equivalent"),
    PET  = c("pet","et","etr","eto","potential_et","evaporation","pet_mm"),
    Flow = c("qobs","runoff","flow","discharge","q_mm","obs_run","q","qout","streamflow")
  )
  lcn <- tolower(names(DT))
  for (nm in names(col_map)) {
    idx <- which(lcn %in% col_map[[nm]])
    if (length(idx)) setnames(DT, idx[1], nm)
  }
  # Basin ID from filename if missing
  if (!("ID" %in% names(DT))) {
    src <- tryCatch(attr(DT, "src_file", exact = TRUE), error = function(e) NA_character_)
    bid <- if (!is.null(src)) stringr::str_extract(basename(src), "\\d{6,}") else NA_character_
    DT[, ID := ifelse(is.na(bid),
                      if (!is.null(src)) tools::file_path_sans_ext(basename(src)) else "unknown",
                      bid)]
  }
  DT
}

# ==== READ & COMBINE ===========================================================================
needed <- c("date","ID","PRCP","TMIN","TMAX","SWE","PET","Flow")
tables <- vector("list", length(cand)); k <- 0L

for (i in seq_along(cand)) {
  f  <- cand[i]
  dt <- read_guess(f)
  if (is.null(dt) || !nrow(dt)) next
  
  attr(dt, "src_file") <- f
  dt <- normalize_dates(dt)
  dt <- rename_common(dt)
  
  keep <- union(c("date","ID"), intersect(needed, names(dt)))
  keep <- intersect(keep, names(dt))
  if (length(keep) >= 2L) {
    k <- k + 1L
    tables[[k]] <- dt[, ..keep]
  }
  
  if (i %% PROGRESS_EVERY == 0) cat("Read", i, "files\n")
}

tables <- tables[seq_len(k)]
if (!length(tables)) stop("No readable tables with required columns under: ", base_dir)

all_dt <- rbindlist(tables, fill = TRUE, use.names = TRUE) |>
  mutate(
    year  = year(date),
    month = month(date),
    doy   = yday(date)
  ) |>
  as.data.table()

#drop impossible negatives
if ("PRCP" %in% names(all_dt)) all_dt[PRCP < 0, PRCP := NA_real_]
if ("Flow" %in% names(all_dt)) all_dt[Flow < 0,  Flow  := NA_real_]

# ==== SAVE ==================================================================
fwrite(all_dt, csv_out)
cat("Saved:", csv_out, "\n")


