source("Workspace/nargiz/setup.R")


# ---- 4.1 Chart: EU trend with spread between countries ----
eu_trend <- mydata %>%
  group_by(year) %>%
  summarise(mean = mean(leave_age_total),
            p25  = quantile(leave_age_total, 0.25),
            p75  = quantile(leave_age_total, 0.75))

chart_eu_trend <- ggplot(eu_trend, aes(x = year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#CDBFDD", alpha = 0.6) +
  geom_line(aes(y = mean), colour = deep_purple, linewidth = 1.3) +
  geom_point(aes(y = mean), colour = deep_purple, size = 2.8) +
  geom_text(aes(y = mean, label = sprintf("%.1f", mean)),
            vjust = -1.2, size = 3.8, colour = deep_brown) +
  scale_x_continuous(breaks = 2016:2024) +
  scale_y_continuous(limits = c(20, 32), breaks = seq(20, 32, 2)) +
  labs(x = "Year", y = "Average leaving age (years)") +
  my_theme +
  theme(axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

chart_eu_trend
ggsave("Workspace/nargiz/charts/eu_trend.png", chart_eu_trend,
       width = 9, height = 5.5, dpi = 300, bg = "white",
       device = ragg::agg_png)