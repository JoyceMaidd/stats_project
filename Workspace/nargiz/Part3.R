source("Workspace/nargiz/setup.R")


# ---- 3.1 Ranking: 2016-2018 vs 2022-2024 ----
rank_periods <- mydata %>%
  filter(year <= 2018 | year >= 2022) %>%
  mutate(period = ifelse(year <= 2018, "2016-2018", "2022-2024")) %>%
  group_by(country, region, period) %>%
  summarise(age = mean(leave_age_total), .groups = "drop") %>%
  group_by(period) %>%
  mutate(rank = rank(-age, ties.method = "first")) %>%
  ungroup()

rank_table <- rank_periods %>%
  pivot_wider(names_from = period, values_from = c(age, rank)) %>%
  rename(age_start  = `age_2016-2018`, age_end  = `age_2022-2024`,
         rank_start = `rank_2016-2018`, rank_end = `rank_2022-2024`) %>%
  mutate(change = rank_start - rank_end) %>%
  arrange(rank_end)


# ---- 3.2 Table: ranking with change ----
tbl_rank <- rank_table %>%
  select(rank_end, country, region, age_end, rank_start, change) %>%
  gt() %>%
  cols_label(rank_end = "Rank 2022-24", country = "Country", region = "Region",
             age_end = "Age 2022-24", rank_start = "Rank 2016-18", change = "Change") %>%
  fmt_number(columns = age_end, decimals = 1)

tbl_rank
gtsave(tbl_rank, "Workspace/nargiz/tables/ranking.png", expand = 20)


# ---- 3.3 Chart: ranking, big movers highlighted ----
movers <- rank_table %>% filter(abs(change) >= 3) %>% pull(country)
rp <- rank_periods %>% mutate(highlight = country %in% movers)
chart_rank_hl <- ggplot(rp, aes(x = period, y = rank, group = country)) +
  geom_line(data = filter(rp, !highlight), colour = "grey85", linewidth = 0.8) +
  geom_point(data = filter(rp, !highlight), colour = "grey80", size = 2) +
  geom_line(data = filter(rp, highlight), aes(colour = region), linewidth = 1.6) +
  geom_point(data = filter(rp, highlight), aes(colour = region), size = 3.5) +
  geom_text(data = filter(rp, period == "2016-2018"),
            aes(label = paste0(country, "  ", rank), alpha = highlight),
            hjust = 1.1, size = 3.5, colour = deep_brown) +
  geom_text(data = filter(rp, period == "2022-2024"),
            aes(label = paste0(rank, "  ", country), alpha = highlight),
            hjust = -0.1, size = 3.5, colour = deep_brown) +
  scale_alpha_manual(values = c(`FALSE` = 0.35, `TRUE` = 1), guide = "none") +
  scale_y_reverse() +
  scale_colour_manual(values = region_colours, name = NULL) +
  scale_x_discrete(expand = expansion(add = 1)) +
  labs(x = NULL, y = NULL) +
  my_theme +
  theme(panel.grid      = element_blank(),
        axis.text.y     = element_blank(),
        axis.text.x     = element_text(size = 14, face = "bold"),
        legend.position = "bottom")
chart_rank_hl
ggsave("Workspace/nargiz/charts/ranking_highlight.png", chart_rank_hl,
       width = 8, height = 9, dpi = 300, bg = "white",
       device = ragg::agg_png)

# ---- 3.4 Chart: places gained or lost ----
chart_rank_change <- rank_table %>%
  ggplot(aes(x = change, y = reorder(country, change), fill = region)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, colour = deep_brown) +
  geom_text(aes(label = ifelse(change == 0, "0", sprintf("%+d", change)),
                hjust = ifelse(change >= 0, -0.3, 1.3)),
            size = 3.5, colour = deep_brown) +
  scale_fill_manual(values = region_colours, name = NULL,
                    breaks = c("Southern", "Eastern", "Western", "Nordic")) +
  scale_x_continuous(breaks = seq(-6, 4, 2), expand = expansion(add = 1)) +
  labs(x = "Places gained (+) or lost (-) in the ranking, 2016-18 to 2022-24", y = NULL) +
  my_theme +
  theme(axis.title.x       = element_text(margin = margin(t = 12)),
        panel.grid.major.y = element_blank(),
        legend.position    = "top")
chart_rank_change
ggsave("Workspace/nargiz/charts/ranking_change.png", chart_rank_change,
       width = 9, height = 8, dpi = 300, bg = "white",
       device = ragg::agg_png)