library(data.table)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(ggridges)
library(ggExtra)


# Task 1 from here

set.seed(42)

catchments <- paste0("C", 1:4)
years <- 2000:2002
months <- 1:12

dta <- CJ(ID = catchments, YR = years, MNTH = months)

dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

dta[, OBS_RUN := round(runif(.N, 10, 250), 1)]

dta[, Date := as.Date(paste0(HYR, "-", MNTH, "-01"))]

setDT(dta)

dta[, Date := as.Date(paste0(HYR, "-", MNTH, "-01"))]

threshold <- quantile(dta$OBS_RUN, 0.95, na.rm = TRUE)
dta[, extreme_flag := OBS_RUN >= threshold]

p <- ggplot(dta, aes(x = Date, y = OBS_RUN)) +
  geom_line(color = "darkblue", linewidth = 0.4) +
  geom_point(data = dta[extreme_flag == TRUE],
             aes(color = "Extreme Event", shape = "Extreme Event"),
             size = 2, show.legend = TRUE) +
  facet_wrap(~ ID, scales = "free_y") +
  labs(
    title = "Monthly Observed Runoff by Hydrological Year",
    subtitle = "Anomalies (Top 5%) Highlighted",
    x = "Date",
    y = "Observed Runoff (OBS_RUN)"
  ) +
  scale_color_manual(values = c("Extreme Event" = "red")) +
  scale_shape_manual(values = c("Extreme Event" = 17)) +
  theme_minimal() +
  theme(legend.position = "bottom")

interactive_p <- ggplotly(p, tooltip = c("x", "y", "ID"))

interactive_p

#Task 2 from here

set.seed(43)
dta[, PRCP := round(runif(.N, 0, 300), 1)]

d_clean <- dta[PRCP > 0 & OBS_RUN > 0]

cor_table <- d_clean[, .(
  correlation = round(cor(PRCP, OBS_RUN, use = "complete.obs"), 2)
), by = .(ID, HYR)]

d_plot <- merge(d_clean, cor_table, by = c("ID", "HYR"))

p <- ggplot(d_plot, aes(x = PRCP, y = OBS_RUN)) +
  geom_point(alpha = 0.3, size = 1.2, color = "#2c7fb8") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               linetype = "dashed", color = "darkgreen") +
  facet_grid(ID ~ HYR) +
  geom_text(aes(x = Inf, y = Inf,
                label = paste0("r = ", correlation)),
            hjust = 1.2, vjust = 1.5, size = 3, color = "black") +
  labs(
    title = "Nonlinear Relationship: Precipitation vs Runoff",
    subtitle = "With LOESS smoother and correlation annotations",
    x = "Precipitation (PRCP)",
    y = "Observed Runoff (OBS_RUN)"
  ) +
  theme_minimal()

print(p)

#Task 3 from here

set.seed(44)
dta[, SWE := round(runif(.N, 0, 150), 1)]

max_swe_annot <- dta[, .SD[which.max(SWE)], by = .(HYR, ID)]

med_swe <- dta[, .(med_SWE = median(SWE, na.rm = TRUE)), by = .(HYR, ID)]

p <- ggplot(dta, aes(x = factor(MNTH), y = SWE)) +
  geom_violin(fill = "#a6bddb", color = "black", alpha = 0.7) +
  geom_jitter(data = max_swe_annot, aes(color = "Max SWE"), width = 0.2, size = 1.5) +
  geom_text(data = med_swe, aes(x = 6.5, y = med_SWE, label = paste("Median:", round(med_SWE))), 
            inherit.aes = FALSE, size = 2.7, color = "black", hjust = 0) +
  facet_grid(ID ~ HYR) +
  scale_color_manual(values = c("Max SWE" = "red")) +
  labs(
    title = "Monthly SWE Distributions by Catchment and Hydrological Year",
    subtitle = "Violin plots with max SWE (jittered) and annotated medians",
    x = "Month", y = "Snow Water Equivalent (SWE)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

#Task 4 from here

setDT(dta)

if ("max_SWE" %in% names(dta)) {
  dta[, max_SWE := NULL]
}

max_swe <- dta[, .(max_SWE = max(SWE, na.rm = TRUE)), by = .(HYR, ID)]

dta <- merge(dta, max_swe, by = c("HYR", "ID"), all.x = TRUE)

dta[, snowmelt := max_SWE - SWE]

head(dta[, .(ID, HYR, MNTH, SWE, max_SWE, snowmelt)])

set.seed(45)

dta[, PET := round(runif(.N, 20, 200), 1)]

max_swe <- dta[, .(max_SWE = max(SWE, na.rm = TRUE)), by = .(HYR, ID)]

dta <- merge(dta, max_swe, by = c("HYR", "ID"))
dta[, snowmelt := max_SWE - SWE]


p <- ggplot(dta[MNTH %in% 3:5], aes(x = snowmelt, y = OBS_RUN, color = PET)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Snowmelt vs Runoff with PET Influence",
    subtitle = "Spring months only (March–May)",
    x = "Snowmelt (Max SWE - SWE)",
    y = "Observed Runoff (OBS_RUN)",
    color = "PET"
  ) +
  theme_minimal()

ggExtra::ggMarginal(p, type = "histogram", fill = "gray", bins = 20)

#Task 5 from here

set.seed(46)
if (!"PRCP" %in% names(dta)) dta[, PRCP := round(runif(.N, 0, 300), 1)]
if (!"PET" %in% names(dta)) dta[, PET := round(runif(.N, 10, 200), 1)]

dta[, WB := PRCP - PET]

dta[, Season := fifelse(MNTH %in% c(12, 1, 2), "Winter",
  fifelse(MNTH %in% c(3, 4, 5), "Spring",
    fifelse(MNTH %in% c(6, 7, 8), "Summer", "Autumn")))]

p <- ggplot(dta, aes(x = factor(MNTH), y = WB, fill = WB)) +
  geom_col() +
  geom_line(aes(group = 1), color = "black", linewidth = 0.4) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(ID ~ HYR) +
  labs(
    title = "Monthly Water Balance (WB = PRCP - PET)",
    subtitle = "Surplus and Deficit Visualized with Seasonal Coloring",
    x = "Month",
    y = "Water Balance (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dta[, deficit_flag := WB < 0]

dta[, deficit_run := rleid(deficit_flag), by = .(HYR, ID)]
deficit_streaks <- dta[deficit_flag == TRUE, .N, by = .(HYR, ID, deficit_run)][N >= 3]

dta[, prolonged_deficit := paste(HYR, ID, deficit_run) %in% 
      paste(deficit_streaks$HYR, deficit_streaks$ID, deficit_streaks$deficit_run)]

p + geom_text(data = dta[prolonged_deficit == TRUE],
              aes(label = "Prolonged Deficit"), color = "red", size = 2.5, vjust = -0.8)



