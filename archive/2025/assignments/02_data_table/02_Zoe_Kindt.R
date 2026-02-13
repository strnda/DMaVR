# ------------------------------------------------------------
# Assignment 2 — Hydrologic summaries (monthly & annual)
# Student: Zoe Kindt (xkinz003)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# === paths ===
base <- "C:/Users/zkind/OneDrive/Documents/DMAVR"
infile <- file.path(base, "runoff_data.csv")

stopifnot(file.exists(infile))

# === load & basic cleaning ===
DT <- fread(infile)
setDT(DT)
DT[, date := as.Date(date)]

# standardize to 8-digit strings
pad_id <- function(x) sprintf("%08d", as.integer(as.character(x)))
DT[, ID := pad_id(ID)]

# make sure numeric variables are numeric
num_cols <- intersect(c("PRCP","PET","Flow","SWE","TMIN","TMAX"), names(DT))
for (v in num_cols) set(DT, j = v, value = suppressWarnings(as.numeric(DT[[v]])))

# basic QC
DT[PRCP < 0, PRCP := NA_real_]
DT[Flow < 0,  Flow  := NA_real_]
DT[, OBS_RUN := Flow]

# === hydrological calendar: Oct–Sep ===
DT[, HYR  := year(date + months(3))]
DT[, MNTH := month(date)]

# === monthly summaries ===
mon <- DT[, .(
  PRCP    = sum(PRCP,    na.rm = TRUE),
  PET     = if ("PET" %in% names(DT)) sum(PET,  na.rm = TRUE) else NA_real_,
  OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
  SWE_mean = if ("SWE" %in% names(DT)) mean(SWE, na.rm = TRUE) else NA_real_
), by = .(ID, HYR, MNTH)]

# snowmelt = positive drop in SWE from previous month
setorder(mon, ID, HYR, MNTH)
if ("SWE_mean" %in% names(mon)) {
  mon[, SWE_prev := shift(SWE_mean), by = .(ID, HYR)]
  mon[, Snowmelt_month := pmax(SWE_prev - SWE_mean, 0, na.rm = TRUE)]
} else {
  mon[, `:=`(SWE_prev = NA_real_, Snowmelt_month = NA_real_)]
}

# water balance
mon[, WB := PRCP - (fifelse(is.finite(PET), PET, 0) + OBS_RUN)]

fwrite(mon[, .(ID, HYR, MNTH, PRCP, PET, OBS_RUN, WB, SWE_mean, Snowmelt_month)],
       file.path(base, "monthly_summary.csv"))

# === annual summaries ===
ann <- mon[, .(
  PRCP    = sum(PRCP,    na.rm = TRUE),
  PET     = sum(PET,     na.rm = TRUE),
  OBS_RUN = sum(OBS_RUN, na.rm = TRUE),
  WB      = sum(WB,      na.rm = TRUE),
  SpringMelt   = sum(Snowmelt_month[MNTH %in% 3:5], na.rm = TRUE),
  SpringRunoff = sum(OBS_RUN[MNTH %in% 3:5],        na.rm = TRUE)
), by = .(ID, HYR)]

# yearly RC
ann[, RC_year := fifelse(PRCP > 0, OBS_RUN / PRCP, NA_real_)]

fwrite(ann, file.path(base, "annual_summary.csv"))

# === overall RC per catchment + 5 classes ===
rc_overall <- ann[, .(
  PRCP_total    = sum(PRCP,    na.rm = TRUE),
  OBS_RUN_total = sum(OBS_RUN, na.rm = TRUE)
), by = ID][, RC_overall := fifelse(PRCP_total > 0, OBS_RUN_total / PRCP_total, NA_real_)]

# classify into quintiles
rc_valid <- rc_overall[is.finite(RC_overall)]
labs5 <- c("Very Low","Low","Medium","High","Very High")
if (nrow(rc_valid) >= 5) {
  cuts <- quantile(rc_valid$RC_overall, probs = seq(0,1,0.2), na.rm = TRUE)
  rc_overall[, RC_class_overall := cut(RC_overall, cuts, include.lowest = TRUE, labels = labs5)]
} else {
  rc_overall[, RC_class_overall := NA_character_]
}

fwrite(rc_overall[, .(ID, PRCP_total, OBS_RUN_total, RC_overall)],
       file.path(base, "rc_overall.csv"))
fwrite(rc_overall[, .(ID, RC_overall, RC_class_overall)],
       file.path(base, "rc_overall_classified.csv"))

# pick one basin per class
five <- rc_overall[!is.na(RC_class_overall),
                   .SD[which.min(abs(RC_overall - median(RC_overall, na.rm = TRUE)))],
                   by = RC_class_overall][order(RC_class_overall)]
setnames(five, "RC_class_overall", "RC_class")
fwrite(five, file.path(base, "five_catchments.csv"))

# === correlation: spring melt vs spring runoff (per basin) ===
corr <- ann[, .(r = suppressWarnings(cor(SpringMelt, SpringRunoff, use = "complete.obs"))),
            by = ID][order(-r)]
fwrite(corr, file.path(base, "snowmelt_correlation.csv"))

# === brief interpretation requirement ===
interp <- sprintf(
  "Spring melt–runoff correlation: median r = %.2f; most basins positive, meaning higher melt usually matches higher runoff.",
  median(corr$r, na.rm = TRUE)
)
writeLines(interp, file.path(base, "A2_methodology_summary.txt"))

cat("A2 done. Files written to:", base, "\n")




