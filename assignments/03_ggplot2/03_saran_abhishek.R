# Load required libraries
library(data.table)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(ggridges)
library(ggExtra)

# ---- Task 1 ----

set.seed(42)

<<catchments>> <- paste0("C", 1:4)
<<years>> <- 2000:2002
<<months>> <- 1:12

<<dta>> <- CJ(ID = catchments, YR = years, MNTH = months)

# Assign hydrological year
dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Generate synthetic observed runoff
dta[, OBS_RUN := round(runif(.N, 10, 250), 1)]

# Create a Date column
dta[, Date := as.Date(paste0(HYR, "-", MNTH, "-01"))]

setDT(dta)

# Repeat Date assignment (redundant but safe)
dta[, Date := as.Date(paste0(HYR, "-", MNTH, "-01"))]

# Identify extreme runoff events
<<threshold>> <- quantile(dta$OBS_RUN, 0.95, na.rm = TRUE)
dta[, extreme_flag := OBS_RUN >= threshold]

<<p>> <- ggplot(dta, aes(x = Date, y = OBS_RUN)) +
  geom_line(color = "darkblue", linewidth = 0.4) +
  geom_point(data = dta[extreme_flag == TRUE],
             aes(color = "Extreme Event", shape = "Extreme Event"),
             size = 2, show.legend = TRUE) +
  facet_wrap(~ ID, scales = "free_y") +
  labs(
    title = "Monthly Observed
