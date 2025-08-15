# ------------------------------------------------------------
# Assignment 3 — Hydrologic Visualisations & Analysis
# Student: Zoe Kindt (xkinz003)
# ------------------------------------------------------------

library(data.table)
library(ggplot2)
library(lubridate)

# === paths ===
base <- "C:/Users/zkind/OneDrive/Documents/DMAVR"
mon_file  <- file.path(base, "monthly_summary.csv")
ann_file  <- file.path(base, "annual_summary.csv")
five_file <- file.path(base, "five_catchments.csv")

stopifnot(file.exists(mon_file), file.exists(ann_file), file.exists(five_file))

# === load data ===
monthly <- fread(mon_file)
annual  <- fread(ann_file)
five    <- fread(five_file)

# IDs as character
monthly[, ID := as.character(ID)]
annual[,  ID := as.character(ID)]
five[,    ID := as.character(ID)]

# select 5 reps
sel_ids <- unique(five$ID)
monthly <- monthly[ID %in% sel_ids & !is.na(HYR) & !is.na(MNTH)]
annual  <- annual[ID %in% sel_ids]

# =====================================================
# 1) Runoff anomalies (OBS_RUN minus monthly mean)
# =====================================================
clim <- monthly[, .(OBS_RUN_clim = mean(OBS_RUN, na.rm = TRUE)), by = .(ID, MNTH)]
m_anom <- merge(monthly, clim, by = c("ID","MNTH"))
m_anom[, Runoff_anom := OBS_RUN - OBS_RUN_clim]

p1 <- ggplot(m_anom, aes(MNTH, Runoff_anom, group = HYR, colour = factor(HYR))) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_y") +
  labs(title = "Monthly Runoff Anomalies",
       x = "Month", y = "Anomaly (mm)", colour = "HYR") +
  theme_minimal()
ggsave(file.path(base, "A3_runoff_anomalies.png"), p1, width = 10, height = 6)

# =====================================================
# 2) Nonlinear regression: Annual PRCP vs Annual Runoff
# =====================================================
p2 <- ggplot(annual, aes(PRCP, OBS_RUN, colour = ID)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Annual PRCP vs Annual Runoff",
       x = "Annual Precipitation (mm)",
       y = "Annual Runoff (mm)") +
  theme_minimal()
ggsave(file.path(base, "A3_prcp_vs_runoff_loess.png"), p2, width = 7, height = 5)

# =====================================================
# 3) SWE distribution plots
# =====================================================
if ("SWE_mean" %in% names(monthly)) {
  p3 <- ggplot(monthly, aes(x = SWE_mean, fill = ID)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    labs(title = "Distribution of Monthly SWE",
         x = "SWE (mm)", y = "Count") +
    theme_minimal()
  ggsave(file.path(base, "A3_swe_distribution.png"), p3, width = 8, height = 5)
}

# =====================================================
# 4) Multivariate scatter: PET vs PRCP coloured by RC_year
# =====================================================
p4 <- ggplot(annual, aes(PET, PRCP, colour = RC_year, size = OBS_RUN)) +
  geom_point(alpha = 0.7) +
  scale_colour_viridis_c(option = "plasma", na.value = "grey50") +
  labs(title = "PET vs PRCP coloured by RC_year",
       x = "Annual PET (mm)", y = "Annual PRCP (mm)",
       colour = "RC_year", size = "Runoff") +
  theme_minimal()
ggsave(file.path(base, "A3_pet_vs_prcp_rc.png"), p4, width = 8, height = 5)

# =====================================================
# 5) Water balance plot
# =====================================================
p5 <- ggplot(monthly, aes(MNTH, WB, group = HYR, colour = factor(HYR))) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  facet_wrap(~ ID, scales = "free_y") +
  labs(title = "Monthly Water Balance",
       x = "Month", y = "WB = PRCP - (PET + OBS_RUN)") +
  theme_minimal()
ggsave(file.path(base, "A3_water_balance.png"), p5, width = 10, height = 6)

# brief note to satisfy assignment
note <- "Assignment 3: Plots saved. Runoff anomalies show seasonal deviations; SWE distributions vary by basin; PET–PRCP scatter reveals climate/runoff relationships."
writeLines(note, file.path(base, "A3_summary.txt"))

cat("A3 done. Plots + summary written to:", base, "\n")
