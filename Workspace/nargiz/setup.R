# ---- Load data ----
library(tidyverse)
library(readxl)
library(gt)

mydata <- read_excel("data/main_dataset.xlsx", sheet = "Clean data")
names(mydata) <- c("country", "year", "region", "region_2groups",
                   "leave_age_total", "leave_age_men", "leave_age_women",
                   "gender_gap", "price_income_ratio", "owners_pct",
                   "gdp_per_capita_eur", "employment_rate_20_64")

dir.create("Workspace/nargiz/tables", showWarnings = FALSE)
dir.create("Workspace/nargiz/charts", showWarnings = FALSE)


# ---- Style ----
deep_purple <- "#3E2C52"
deep_brown  <- "#3B2A20"
region_colours <- c(Nordic   = "#CDBFDD",
                    Western  = "#8C76A3",
                    Southern = "#B9BC6E",
                    Eastern  = "#6F7342")

my_theme <- theme_minimal(base_size = 14) +
  theme(text             = element_text(colour = deep_brown),
        axis.text        = element_text(colour = deep_brown),
        plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank())