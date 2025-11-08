## ----echo=FALSE------------------------------------------------------------------------------------------

gee_1 <- geeglm(
  sarco ~ mobility,
  data = survey_data,
  family = binomial(),
  corstr = "exchangeable",
  weights = survey_weights,
  id = id
)

gee_2 <- geeglm(
  sarco ~ mobility + age + gender,
  data = survey_data,
  family = binomial(),
  corstr = "exchangeable",
  weights = survey_weights,
  id = id
)

gee_3 <- geeglm(
  sarco ~ mobility + age + gender + Race + marry_baseline + rural_baseline + child_adver,
  data = survey_data,
  family = binomial(),
  corstr = "exchangeable",
  weights = survey_weights,
  id = id
)


## ----echo=FALSE------------------------------------------------------------------------------------------
# results
tab_model(gee_1, gee_2, gee_3,
          dv.labels = c("Model 1", "Model 2", "Model 3"), digits = 3)


## ----echo=FALSE------------------------------------------------------------------------------------------

# Create tidy data
tidy_gee <- broom::tidy(gee_3, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "mobilitydown" ~ "Downwardly mobile",
    term == "mobilitymiddle" ~ "Stably middle",
    term == "mobilityup" ~ "Upwardly mobile",
    term == "mobilityhigh" ~ "Stably high",
    TRUE ~ term
  )) %>%
  mutate(group = case_when(
    grepl("Downwardly mobile|Upwardly mobile|Stably", term) ~ "Mobility",
    TRUE ~ NA_character_
  )) %>%
  filter(term %in% custom_order & !is.na(group)) %>%
  mutate(term = factor(term, levels = rev(custom_order)))

# Add the reference group
ref_row <- tibble(
  term = factor("Stably low", levels = levels(tidy_gee$term)),
  estimate = 1,
  conf.low = 1,
  conf.high = 1,
  group = "Mobility"
)

tidy_gee <- bind_rows(ref_row, tidy_gee)

# Plot
geeforestplot <- ggplot(tidy_gee, aes(x = estimate, y = term, color = group)) +
  geom_point(shape = 18, size = 6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray72") +
  scale_color_manual(values = c("Mobility" = "#005f96"), breaks = c("Mobility")) +
  scale_x_log10(
    limits = c(0.25, 1.25),
    breaks = c(0.25, 0.5, 0.75, 1, 1.25),
    labels = c("0.25", "0.50", "0.75", "1", "1.25")
    ) +
  xlab("Odds Ratio (95% CI)") +
  ylab("") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  ggtitle("")
print(geeforestplot)

dev.off()

