source("Workspace/nargiz/setup.R")

# ---- 1.1 Overall mean ----
mean_overall <- mydata %>%
  summarise(mean_age = mean(leave_age_total))

tbl_overall <- mean_overall %>%
  gt() %>%
  tab_header(title = "Overall average leaving age, EU-27, 2016-2024") %>%
  cols_label(mean_age = "Mean (years)") %>%
  fmt_number(decimals = 1)

tbl_overall
gtsave(tbl_overall, "Workspace/nargiz/tables/mean_overall.png", expand = 20)


# ---- 1.2 Mean by year ----
mean_by_year <- mydata %>%
  group_by(year) %>%
  summarise(mean_age = mean(leave_age_total))

tbl_year <- mean_by_year %>%
  gt() %>%
  tab_header(title = "Average leaving age by year, EU-27") %>%
  cols_label(year = "Year", mean_age = "Mean (years)") %>%
  fmt_number(columns = mean_age, decimals = 1)

tbl_year
gtsave(tbl_year, "Workspace/nargiz/tables/mean_by_year.png", expand = 20)


# ---- 1.3 Mean by country ----
mean_by_country <- mydata %>%
  group_by(country, region) %>%
  summarise(mean_age = mean(leave_age_total), .groups = "drop") %>%
  arrange(mean_age)

tbl_country <- mean_by_country %>%
  gt() %>%
  tab_header(title = "Average leaving age by country, 2016-2024") %>%
  cols_label(country = "Country", region = "Region", mean_age = "Mean (years)") %>%
  fmt_number(columns = mean_age, decimals = 1)

tbl_country
gtsave(tbl_country, "Workspace/nargiz/tables/mean_by_country.png", expand = 20)


# ---- 1.4 Mean by region ----
mean_by_region <- mydata %>%
  group_by(region) %>%
  summarise(mean_age = mean(leave_age_total)) %>%
  arrange(mean_age)

tbl_region <- mean_by_region %>%
  gt() %>%
  tab_header(title = "Average leaving age by region, 2016-2024") %>%
  cols_label(region = "Region", mean_age = "Mean (years)") %>%
  fmt_number(columns = mean_age, decimals = 1)

tbl_region
gtsave(tbl_region, "Workspace/nargiz/tables/mean_by_region.png", expand = 20)


# ---- 1.5 Mean by region and year ----
mean_region_year <- mydata %>%
  group_by(region, year) %>%
  summarise(mean_age = mean(leave_age_total), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = mean_age)

tbl_region_year <- mean_region_year %>%
  gt() %>%
  tab_header(title = "Average leaving age by region and year") %>%
  cols_label(region = "Region") %>%
  fmt_number(columns = -region, decimals = 1)

tbl_region_year
gtsave(tbl_region_year, "Workspace/nargiz/tables/mean_region_year.png", expand = 20)

# ---- Chart 1: Mean by region and year ----
region_year_long <- mydata %>%
  group_by(region, year) %>%
  summarise(mean_age = mean(leave_age_total), .groups = "drop")

chart1_data <- bind_rows(region_year_long,
                         mean_by_year %>% mutate(region = "EU-27 average")) %>%
  mutate(region = factor(region, levels = c("Southern", "Eastern", "EU-27 average",
                                            "Western", "Nordic")))

line_colours <- c(region_colours, "EU-27 average" = deep_brown)
line_types   <- c(Southern = "solid", Eastern = "solid", "EU-27 average" = "longdash",
                  Western = "solid", Nordic = "solid")

chart_region_year <- ggplot(chart1_data,
                            aes(x = year, y = mean_age, colour = region, linetype = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = filter(chart1_data, region != "EU-27 average"), size = 2.5) +
  scale_colour_manual(values = line_colours, name = NULL) +
  scale_linetype_manual(values = line_types, name = NULL) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, 16, NA, 16, 16)))) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(x = "Year", y = "Average leaving age (years)") +
  my_theme +
  theme(axis.title.x      = element_text(margin = margin(t = 12)),
        axis.title.y      = element_text(margin = margin(r = 12)),
        legend.key.width  = unit(1.5, "cm"))

chart_region_year
ggsave("Workspace/nargiz/charts/mean_region_year.png", chart_region_year,
       width = 9, height = 5.5, dpi = 300, bg = "white",
       device = ragg::agg_png)

#

# ---- Chart 2: Mean by country ----
chart_country <- mean_by_country %>%
  mutate(region = factor(region, levels = c("Southern", "Eastern", "Western", "Nordic"))) %>%
  ggplot(aes(x = mean_age, y = reorder(country, mean_age), fill = region)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.1f", mean_age)),
            hjust = -0.2, size = 3.8, colour = deep_brown) +
  scale_fill_manual(values = region_colours, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Average leaving age (years)", y = NULL) +
  my_theme +
  theme(axis.title.x       = element_text(margin = margin(t = 12)),
        panel.grid.major.y = element_blank())

chart_country
ggsave("Workspace/nargiz/charts/mean_by_country.png", chart_country,
       width = 9, height = 8, dpi = 300, bg = "white",
       device = ragg::agg_png)
#

# ---- Chart 3: Mean by region ----
chart_region <- mean_by_region %>%
  mutate(region = factor(region, levels = c("Nordic", "Western", "Eastern", "Southern"))) %>%
  ggplot(aes(x = region, y = mean_age, fill = region)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", mean_age)),
            vjust = -0.6, size = 5, colour = deep_brown) +
  scale_fill_manual(values = region_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Average leaving age (years)") +
  my_theme +
  theme(legend.position    = "none",
        axis.title.y       = element_text(margin = margin(r = 12)),
        panel.grid.major.x = element_blank())

chart_region
ggsave("Workspace/nargiz/charts/mean_by_region.png", chart_region,
       width = 7, height = 5.5, dpi = 300, bg = "white",
       device = ragg::agg_png)
#

# ---- 2.1 Summary table by region (one value per country) ----
country_avg <- mydata %>%
  group_by(country, region) %>%
  summarise(age = mean(leave_age_total), .groups = "drop")

summary_region <- country_avg %>%
  group_by(region) %>%
  summarise(countries = n(),
            mean      = mean(age),
            median    = median(age),
            variance  = var(age),
            sd        = sd(age)) %>%
  arrange(mean)

summary_overall <- country_avg %>%
  summarise(region    = "EU-27 (all)",
            countries = n(),
            mean      = mean(age),
            median    = median(age),
            variance  = var(age),
            sd        = sd(age))

tbl_summary_region <- bind_rows(summary_region, summary_overall) %>%
  gt() %>%
  cols_label(region = "Region", countries = "Countries", mean = "Mean",
             median = "Median", variance = "Variance", sd = "SD") %>%
  fmt_number(columns = c(mean, median, variance, sd), decimals = 1)

tbl_summary_region
gtsave(tbl_summary_region, "Workspace/nargiz/tables/summary_region.png", expand = 20)


# ---- 2.2 Boxplot by region (one value per country) ----
chart_boxplot <- country_avg %>%
  mutate(region = factor(region, levels = c("Nordic", "Western", "Eastern", "Southern"))) %>%
  ggplot(aes(x = region, y = age, fill = region)) +
  geom_boxplot(width = 0.6, colour = deep_brown, outlier.shape = NA, alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 1),
             size = 2.5, colour = deep_brown) +
  scale_fill_manual(values = region_colours) +
  labs(x = NULL, y = "Average leaving age, 2016-2024 (years)") +
  my_theme +
  theme(legend.position    = "none",
        axis.title.y       = element_text(margin = margin(r = 12)),
        panel.grid.major.x = element_blank())

chart_boxplot
ggsave("Workspace/nargiz/charts/boxplot_region.png", chart_boxplot,
       width = 7, height = 5.5, dpi = 300, bg = "white",
       device = ragg::agg_png)
#

# ---- 2.3 Maps of Europe ----
library(sf)
library(rnaturalearth)

sf_use_s2(FALSE)

europe <- ne_countries(scale = 50, returnclass = "sf") %>%
  st_crop(xmin = -30, xmax = 60, ymin = 30, ymax = 75) %>%
  st_transform(3035) %>%
  left_join(country_avg, by = c("name" = "country"))


# ---- Map A: average leaving age by country ----
map_age <- ggplot(europe) +
  geom_sf(aes(fill = age), colour = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#E9E2F1", high = deep_purple,
                      na.value = "grey90", name = "Average\nleaving age\n(years)") +
  coord_sf(xlim = c(2500000, 6700000), ylim = c(1250000, 5400000), expand = FALSE) +
  theme_void(base_size = 14) +
  theme(text            = element_text(colour = deep_brown),
        legend.position = "inside",
        legend.position.inside = c(0.12, 0.75))

map_age
ggsave("Workspace/nargiz/charts/map_leaving_age.png", map_age,
       width = 8, height = 8, dpi = 300, bg = "white",
       device = ragg::agg_png)