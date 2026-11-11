source("Workspace/nargiz/setup.R")

gender_colours <- c(Men = deep_purple, Women = "#B9BC6E")


# ---- 2.1 One value per country (2016-2024 average) ----
gender_country <- mydata %>%
  group_by(country, region) %>%
  summarise(men   = mean(leave_age_men),
            women = mean(leave_age_women),
            gap   = mean(gender_gap), .groups = "drop")


# ---- 2.2 Summary table: men, women, gap ----
gender_summary <- gender_country %>%
  pivot_longer(c(men, women, gap), names_to = "group", values_to = "age") %>%
  group_by(group) %>%
  summarise(mean     = mean(age),
            median   = median(age),
            variance = var(age),
            sd       = sd(age)) %>%
  mutate(group = recode(group, men = "Men", women = "Women", gap = "Gap (men - women)"),
         group = factor(group, levels = c("Men", "Women", "Gap (men - women)"))) %>%
  arrange(group)

tbl_gender_summary <- gender_summary %>%
  gt() %>%
  cols_label(group = "", mean = "Mean", median = "Median",
             variance = "Variance", sd = "SD") %>%
  fmt_number(columns = -group, decimals = 1)

tbl_gender_summary
gtsave(tbl_gender_summary, "Workspace/nargiz/tables/gender_summary.png", expand = 20)


# ---- 2.3 Table: gap by region ----
gender_region <- gender_country %>%
  group_by(region) %>%
  summarise(men   = mean(men),
            women = mean(women),
            gap   = mean(gap)) %>%
  arrange(gap)

tbl_gender_region <- gender_region %>%
  gt() %>%
  cols_label(region = "Region", men = "Men", women = "Women", gap = "Gap") %>%
  fmt_number(columns = -region, decimals = 1)

tbl_gender_region
gtsave(tbl_gender_region, "Workspace/nargiz/tables/gender_region.png", expand = 20)


# ---- 2.4 Chart: men vs women by country ----
gender_country_plot <- gender_country %>%
  mutate(country = reorder(country, men))

gender_long <- gender_country_plot %>%
  pivot_longer(c(men, women), names_to = "sex", values_to = "age") %>%
  mutate(sex = ifelse(sex == "men", "Men", "Women"))

chart_gender_country <- ggplot(gender_country_plot, aes(y = country)) +
  geom_segment(aes(x = women, xend = men, yend = country),
               colour = "grey75", linewidth = 1.5) +
  geom_point(data = gender_long, aes(x = age, colour = sex), size = 3) +
  scale_colour_manual(values = gender_colours, name = NULL) +
  labs(x = "Average leaving age, 2016-2024 (years)", y = NULL) +
  my_theme +
  theme(axis.title.x       = element_text(margin = margin(t = 12)),
        panel.grid.major.y = element_blank(),
        legend.position    = "top")

chart_gender_country
ggsave("Workspace/nargiz/charts/gender_by_country.png", chart_gender_country,
       width = 9, height = 8, dpi = 300, bg = "white",
       device = ragg::agg_png)


# ---- 2.5 Chart: gender gap by region over time ----
gap_region_year <- mydata %>%
  group_by(region, year) %>%
  summarise(gap = mean(gender_gap), .groups = "drop")

gap_eu_year <- mydata %>%
  group_by(year) %>%
  summarise(gap = mean(gender_gap)) %>%
  mutate(region = "EU-27 average")

gap_data <- bind_rows(gap_region_year, gap_eu_year) %>%
  mutate(region = factor(region, levels = c("Eastern", "EU-27 average", "Southern",
                                            "Western", "Nordic")))

gap_colours <- c(region_colours, "EU-27 average" = deep_brown)
gap_types   <- c(Eastern = "solid", "EU-27 average" = "longdash", Southern = "solid",
                 Western = "solid", Nordic = "solid")

chart_gap_year <- ggplot(gap_data, aes(x = year, y = gap, colour = region, linetype = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = filter(gap_data, region != "EU-27 average"), size = 2.5) +
  scale_colour_manual(values = gap_colours, name = NULL) +
  scale_linetype_manual(values = gap_types, name = NULL) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, NA, 16, 16, 16)))) +
  scale_x_continuous(breaks = 2016:2024) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Year", y = "Gender gap (men - women, years)") +
  my_theme +
  theme(axis.title.x     = element_text(margin = margin(t = 12)),
        axis.title.y     = element_text(margin = margin(r = 12)),
        legend.key.width = unit(1.5, "cm"))

chart_gap_year
ggsave("Workspace/nargiz/charts/gender_gap_by_year.png", chart_gap_year,
       width = 9, height = 5.5, dpi = 300, bg = "white",
       device = ragg::agg_png)