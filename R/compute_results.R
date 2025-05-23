# load libraries
library(tidyverse)
library(patchwork)
library(binom)
library(ggtext)
library(directlabels)
library(scales)
library(openxlsx)
library(geomtextpath)
library(marginaleffects)
library(mgcv)

# load fonts and define colors
extrafont::loadfonts(quiet = TRUE)
mycols <- c("coral4", "steelblue")

# import data
df <- read_csv2(file = "data/data_indicators.csv")

# define factors and additional variables
df$group     <- factor(df$group, levels = c("Program", "Control"))
df$indic_id  <- factor(df$indic_id)
df$year_fct  <- factor(df$year)
df$post      <- df$year >= 2017

##################################
#### table: indicator summary ####
##################################
tab_summary <- df %>%
  group_by(ID = indic_id) %>%
  summarise(
    `Group`                           = unique(group),
    `Clinical area`                   = unique(clin_area),
    `Description`                     = unique(indic_descr),
    `Population size (2013-2020)`     = sum(n),
    `Adverse care events (2013-2020)` = sum(o_adverse),
    `Indicator type` = case_when(
      unique(indic_type) == "I" ~ "Indication",
      unique(indic_type) == "P" ~ "Process"
    ),
    prop = sum(o_adverse) / sum(n),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      .cols = c(`Population size (2013-2020)`, `Adverse care events (2013-2020)`),
      ~ format(.x, big.mark = ",")
    ),
    `Adverse care events (2013-2020)` = paste0(`Adverse care events (2013-2020)`, " (", trimws(format(
      round(prop * 100, 2), nsmall = 2
    )), "%)"
    )
  ) %>% 
  arrange(`Group`, `Clinical area`, as.character(ID)) %>% 
  select(-prop)

write.xlsx(tab_summary, file = "results/table_summary.xlsx")

###########################################
#### figure: indicator-specific trends ####
###########################################
p_indic_trends <- df %>% 
  mutate(result = o_adverse/n,
         lwr    = binom.wilson(o_adverse, n, conf.level = 0.95)$lower,
         upr    = binom.wilson(o_adverse, n, conf.level = 0.95)$upper) %>% 
  rowwise %>% 
  mutate(indic_descr_split = paste(strwrap(indic_descr, width = 50), collapse = "<br>")) %>% 
  ungroup() %>% 
  
  mutate(
    ID = fct_reorder(factor(paste0("<b>ID ", indic_id, "</b><br>", clin_area,
                                   "<br><span style = 'font-size:7pt'>", 
                                   indic_descr_split
                                   , "</span>")), group != "Program")
  ) %>% 
  ggplot() +
  geom_point(aes(x = year, y = result, col = group), size = 2) + 
  geom_line(aes(x = year, y = result, col = group, group = ID)) + 
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr, group = ID, fill = group), 
              alpha = 0.2, show.legend = FALSE) + 
  theme_minimal(13) + 
  theme(text = element_text(family = "Segoe UI"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_markdown(),
        axis.text.x = element_text(angle = 45)) + 
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0.25,0.25))) + 
  scale_x_continuous(breaks = 2013:2020) + 
  scale_fill_manual(values = mycols) + 
  scale_color_manual(values = mycols) + 
  facet_wrap(~ID, scales = "free_y", ncol = 3) + 
  labs(x = "Year", y = "Estimated event probability (95% CI)",
       col = "Group") + 
  guides(col = guide_legend(ncol = 1))

png("results/figure_indic_trends.png", width = 3400, height = 3900, res = 310)
print(p_indic_trends)
dev.off()

#################################
#### fit logistic regression ####
#################################
formula_mod <-  "cbind(o_adverse, n - o_adverse) ~ indic_id * year_fct"
m <- glm(as.formula(formula_mod), data = df, family = binomial(link = "logit"))

#################################
#### obtain effect estimates ####
#################################
# create empty grid for all year-QI combinations
grid <- expand_grid(
  year_fct = unique(df$year_fct),
  indic_id = unique(df$indic_id)
) %>%
  left_join(
    df %>% select(-o, -o_adverse),
    by = c("year_fct", "indic_id")
  )

# estimate delta with confidence interval
delta_dm <- hypotheses(
  m,
  hypothesis = function(x){
    p <- predict(x, type = "response", newdata = grid)
    delta <- grid %>%
      mutate(p = p) %>%
      group_by(post, group=="Program") %>%
      summarise(mean_p = mean(p), .groups = "drop_last") %>%
      summarise(diff = diff(qlogis(mean_p)), .groups = "drop") %>%
      summarise(delta = diff(diff)) %>%
      pull(delta)
    return(delta)
  }
)

delta_se <- list(estimate = as.numeric(delta_dm$estimate),
                 ci95 = c(delta_dm$conf.low, delta_dm$conf.high))
writeLines(capture.output(delta_se), "results/estimated_delta.txt")

# estimate theta_att with confidence interval
theta_att_dm <- hypotheses(
  m,
  hypothesis = function(x) {
    p <- predict(x, type = "response", newdata = grid)
    
    delta <- grid %>%
      mutate(p = p) %>%
      group_by(post, group == "Program") %>%
      summarise(mean_p = mean(p), .groups = "drop_last") %>%
      summarise(diff = diff(qlogis(mean_p)), .groups = "drop") %>%
      summarise(delta = diff(diff)) %>%
      pull(delta)
    
    theta_att <- grid %>%
      mutate(p = p) %>%
      mutate(p_cf = plogis(qlogis(p) - delta)) %>%
      filter(post, group == "Program") %>%
      summarise(theta_att = weighted.mean(p, n) / weighted.mean(p_cf, n)) %>%
      pull(theta_att)
    
    return(theta_att)
  }
)

theta_att_se <- list(estimate = as.numeric(theta_att_dm$estimate),
                     ci95 = c(theta_att_dm$conf.low, theta_att_dm$conf.high))
writeLines(capture.output(theta_att_se), "results/estimated_theta_att.txt")

# estimate profited patients with confidence interval (aggregated)
o_att_dm <- hypotheses(
  m,
  hypothesis = function(x) {
    p <- predict(x, type = "response", newdata = grid)
    
    delta <- grid %>%
      mutate(p = p) %>%
      group_by(post, group == "Program") %>%
      summarise(mean_p = mean(p), .groups = "drop_last") %>%
      summarise(diff = diff(qlogis(mean_p)), .groups = "drop") %>%
      summarise(delta = diff(diff)) %>%
      pull(delta)
    
    o_att <- grid %>%
      mutate(p = p) %>%
      mutate(p_cf = plogis(qlogis(p) - delta)) %>%
      filter(post, group == "Program") %>%
      summarise(o_att = sum(p_cf * n) - sum(p * n)) %>%
      pull(o_att)
    
    return(o_att)
  }
)

o_att_se <- list(estimate = as.numeric(o_att_dm$estimate),
                 ci95 = c(o_att_dm$conf.low, o_att_dm$conf.high))
writeLines(capture.output(o_att_se), "results/estimated_o_att.txt")

# estimate profited patients with confidence interval (per program indicator)
os_att_dm <- hypotheses(
  m,
  hypothesis = function(x) {
    p <- predict(x, type = "response", newdata = grid)
    
    delta <- grid %>%
      mutate(p = p) %>%
      group_by(post, group == "Program") %>%
      summarise(mean_p = mean(p), .groups = "drop_last") %>%
      summarise(diff = diff(qlogis(mean_p)), .groups = "drop") %>%
      summarise(delta = diff(diff)) %>%
      pull(delta)
    
    os_att <- grid %>%
      mutate(p = p) %>%
      mutate(p_cf = plogis(qlogis(p) - delta)) %>%
      filter(post, group == "Program") %>%
      group_by(indic_id) %>% 
      summarise(os_att = sum(p_cf * n) - sum(p * n),
                .groups = "drop") %>%
      pull(os_att)
    
    return(os_att)
  }
)

os_att_se <- data.frame(indicator = sort(unique(grid$indic_id[grid$group == "Program"])),
                        estimate = as.numeric(os_att_dm$estimate),
                        ci95_lwr = os_att_dm$conf.low,
                        ci95_upr = os_att_dm$conf.high)
writeLines(capture.output(os_att_se), "results/estimated_os_att.txt")

###################################################
#### figure: difference-in-differences results ####
###################################################
# pre- and post-trends for average indicator
grp_year_means_dm <- hypotheses(
  m,
  hypothesis = function(x) {
    p <- predict(x, type = "response", newdata = grid)
    
    grp_year_means <- grid %>%
      mutate(p = p) %>%
      group_by(group, year) %>%
      summarise(estimate = qlogis(mean(p)),
                term = paste(unique(group), unique(year), sep = "; "),
                .groups = "drop")
    
    return(grp_year_means)
  }
)

grp_year_means_se <- as_tibble(grp_year_means_dm) %>%
  mutate(group = factor(
    ifelse(grepl("Program", term), "Program", "Control"),
    levels = c("Program", "Control")
  ),
  year = parse_number(term))

p_trends <- grp_year_means_se %>% 
  ggplot() + 
  geom_point(aes(x = year, y = estimate, col = group), size = 2) + 
  geom_line(aes(x = year,  y = estimate, group = group, col = group)) + 
  geom_ribbon(aes(x = year, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, show.legend = FALSE) +
  geom_errorbar(aes(x = year, ymin = conf.low, ymax = conf.high, col = group), 
                width = 0,
                show.legend = FALSE) +
  geom_dl(aes(label= group, x = year,  y = estimate, col = group,
              group = group), position = position_nudge(x = -0.125, y = + 0.015),
          method = list("first.points", cex = 0.7)) +
  geom_vline(xintercept = 2016.5,  col = "grey30", linetype = 3) +
  geom_text(x = 2016.6, y = -2.6, label = "Program\nintroduction", check_overlap = TRUE,
            col = "grey30", hjust = 0, size = 3, family = "Segoe UI") +
  theme_minimal(11) + 
  scale_x_continuous(limits = c(2012.5,2020.5), breaks = 2013:2020) +
  scale_y_continuous(limits = c(-4,-2.5), breaks = seq(-4,-2.5,0.5)) +
  theme(text = element_text(family = "Segoe UI"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = "none") + 
  labs(y = "Estimated<br>event probability (logit)<br><span style = 'font-size:8pt'>for average quality indicator",
       x = "Year",
       col = "Group") + 
  guides(col=guide_legend(nrow = 1)) + 
  scale_fill_manual(values = mycols) +
  scale_color_manual(values = mycols)

# event study for average indicator
year_diff_means_dm <- hypotheses(
  m,
  hypothesis = function(x) {
    p <- predict(x, type = "response", newdata = grid)
    
    year_diff_means <- grid %>%
      mutate(p = p) %>%
      group_by(group, year) %>%
      summarise(logit_mean_p = qlogis(mean(p)),
                .groups = "drop") %>%
      group_by(year) %>%
      summarise(diff = -diff(logit_mean_p)) %>%
      mutate(estimate = diff - diff[year == 2016], 
             term = unique(year))
    
    return(year_diff_means)
  }
)

year_diff_means_se <- as_tibble(year_diff_means_dm) %>% rename(year = term)

p_event <- year_diff_means_se %>% 
  ggplot() + 
  geom_point(aes(x = year, y = estimate), size = 2) + 
  geom_line(aes(x = year, y = estimate)) + 
  geom_errorbar(aes(x = year, ymin = conf.low, ymax = conf.high),
                col = "black", width = 0) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_vline(xintercept = 2016.5,  col = "grey30", linetype = 3) +
  geom_text(x = 2016.7, y = 0.15, label = "Program\nintroduction", check_overlap = TRUE,
            col = "grey30", hjust = 0, size = 3, family = "Segoe UI") +
  theme_minimal(11) + 
  scale_x_continuous(limits = c(2012.5, 2020.5), breaks = 2013:2020) +
  scale_y_continuous(limits = c(-0.725,0.225), breaks = seq(-0.8,0.2,0.2)) +
  theme(text = element_text(family = "Segoe UI"),
        axis.title.y = element_markdown(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(y = "Estimated<br>DiD (Reference year: 2016) (logit)<br><span style = 'font-size:8pt'>for average quality indicator",
       x = "Year")

# causal risk ratio for average patient
p_riskratio <- grid %>% 
  mutate(
    pred = predict(m, type = "response", newdata = grid),
    pred_cf = case_when(
      group=="Program" & post  ~ plogis(qlogis(pred)-delta_se$estimate),
      group=="Program" & !post ~ pred,
      TRUE ~ NA_real_)) %>% 
  group_by(group, post) %>% 
  summarise(mean_pred    = weighted.mean(pred, n),
            mean_pred_cf = weighted.mean(pred_cf, n),
            .groups = "drop") %>% 
  mutate(Period = factor(ifelse(post, "Post", "Pre"),
                         levels = c("Pre", "Post"), ordered = TRUE)) %>% 
  pivot_longer(cols = contains("pred"), names_to = "world", values_to = "pred") %>% 
  mutate(world = factor(ifelse(grepl("cf", world), "Counterfactual", "Observable"),
                        levels = c("Observable", "Counterfactual"), ordered = TRUE)) %>% 
  filter(!is.na(pred)) %>% 
  ggplot() + 
  geom_point(aes(x = Period,  y= pred, col = group), size = 2) + 
  geom_textline(aes(x = Period,  y= pred,  col = group, 
                    label = world,
                    lty = world, group = paste0(world, group)),
                size = 3) +
  scale_y_continuous(breaks = seq(0.01,0.05,0.01), limits = c(0,0.052), labels = percent_format(), expand = c(0,0)) + 
  labs(y = "Estimated event probability<br><span style = 'font-size:8pt'>for average patient",
       lty = "Scenario",
       col = "Group") + 
  theme_minimal(11) + 
  theme(legend.position = "none",
        text = element_text(family = "Segoe UI"),
        axis.title.y = element_markdown(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  guides(linetype = guide_legend(nrow = 2),
         col = guide_legend(nrow = 2)) + 
  scale_linetype_manual(values = c(1,2)) +
  scale_color_manual(values = mycols) + 
  scale_x_discrete(expand = c(0.2,0.2))+
  geom_text(
    data = . %>% filter(Period == "Post" & group == "Program"),
    aes(x = Period, y = pred, label = paste0(format(round(pred*100, 2), nsmall = 2), "%"), col = group),
    position = position_nudge(y = 0.0035), check_overlap = TRUE,
    size = 3,
    family = "Segoe UI"
  ) +
  geom_dl(aes(label= group, x = Period,  y = pred, col = group,
              group = group), position = position_nudge(x = -0.07, y = 0.0005),
          method = list("first.points", cex = 0.7))

png("results/figure_did_results.png", width = 4200, height = 2900, res = 485)
print(
  p_trends / (p_event + p_riskratio) + plot_annotation(tag_levels = "A") + 
    plot_layout(heights = c(1,1.25)) &
    theme(plot.tag = element_text(face = 'bold'))
)
dev.off()

##################################
#### figure: population sizes ####
##################################
p_popsizes <- df %>%
  mutate(
    ID = fct_reorder(factor(paste0("ID ", indic_id)), group != "Program"),
    n_beautified = format(n, big.mark=",")
  ) %>%
  ggplot() +
  geom_bar(aes(x = ID, y = n, fill = group), stat = "identity", alpha = 0.5) +
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Quality indicator",
       y = "Population size") +
  theme_minimal(13) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        text = element_text(family = "Segoe UI"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = label_comma(), trans = "log10") + 
  geom_text(aes(x = ID, y = n, label = n_beautified), family = "Segoe UI", vjust = 1.5, size= 2) +
  scale_fill_manual(values = mycols) + 
  labs(fill = "Group")

png("results/figure_popsizes.png", width = 2700, height = 3750, res = 350)
print(p_popsizes)
dev.off()

#####################################
#### figure: number of hospitals ####
#####################################
p_hospitals <- df %>% 
  mutate(ID = fct_reorder(factor(paste0("ID ", indic_id)), group != "Program")) %>% 
  filter(year > 2013) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = n_hospitals, col = group)) +
  scale_color_manual(values = mycols) +
  facet_wrap(~ ID, ncol = 2) +
  theme_minimal(13) +
  theme(legend.position = "bottom",
        text = element_text(family = "Segoe UI"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(
    col = "Group",
    y = "Number of hospitals",
    x = "Year"
  ) +
  scale_x_continuous(breaks = 2014:2020)

png("results/figure_hospitals.png", width = 2700, height = 3750, res = 350)
print(p_hospitals)
dev.off()

#######################################
#### alternative data and analysis ####
#######################################
# import data
df2 <- read_csv2(file = "data/data_hospitalreports.csv")

# define factors and additional variables
df2$group    <- factor(df2$group, levels = c("Program", "Control"))
df2$indic_id <- factor(df2$indic_id)
df2$year_fct <- factor(df2$year)
df2$hospital <- factor(df2$hospital)

# mixed logistic regression model
m2 <- gam(cbind(o_adverse, n - o_adverse) ~ indic_id * year_fct + s(hospital, bs = "re"),
          data = df2, family = binomial(link = "logit"), method = "REML",
          control = list(nthreads = 15))

# extract outputs
gam.vcomp(m2) # SD: 0.776
random_eff <- m2$coefficients[grepl("hospital", names(m2$coefficients))]
length(unique(m2$model$hospital)) # 1,257
length(random_eff) # 1,257

# create grid
grid <- expand_grid(
  year_fct = df2 %>% pull(year_fct) %>% unique,
  indic_id = df2 %>% pull(indic_id) %>% unique) %>%
  left_join(df2 %>% group_by(year, year_fct, indic_id, group) %>% summarise(n = sum(n), .groups = "drop"), by = c("year_fct", "indic_id")) %>%
  mutate(hospital = factor(1))

# event plot
year_diff_means_dm <- hypotheses(
  predictions(m2, newdata = grid, type = "response", exclude = "s(hospital)"),
  hypothesis = function(x) {
    year_diff_means <- grid %>%
      mutate(p = x$estimate) %>%
      group_by(group, year) %>%
      summarise(logit_mean_p = qlogis(mean(p)),
                .groups = "drop") %>%
      group_by(year) %>%
      summarise(diff = -diff(logit_mean_p)) %>%
      mutate(estimate = diff - diff[year == 2016], 
             term = unique(year))
    
    return(year_diff_means)
  }
)

year_diff_means_se <- as_tibble(year_diff_means_dm) %>% rename(year = term)

p_event_sqb <- year_diff_means_se %>% 
  ggplot() + 
  geom_point(aes(x = year, y = estimate), size = 2) + 
  geom_line(aes(x = year, y = estimate)) + 
  geom_errorbar(aes(x = year, ymin = conf.low, ymax = conf.high),
                col = "black", width = 0) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_vline(xintercept = 2016.5,  col = "grey30", linetype = 3) +
  geom_text(x = 2016.7, y = 0.15, label = "Program\nintroduction", check_overlap = TRUE,
            col = "grey30", hjust = 0, size = 3, family = "Segoe UI") +
  theme_minimal(11) + 
  scale_x_continuous(limits = c(2014.5, 2020.5), breaks = 2015:2020) +
  scale_y_continuous(breaks = seq(-1,0.2,0.2), limits = c(-0.95, 0.2)) +
  theme(text = element_text(family = "Segoe UI"),
        axis.title.y = element_markdown(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(y = "Estimated<br>DiD (Reference year: 2016) (logit)<br><span style = 'font-size:8pt'>for average quality indicator and hospital",
       x = "Year")

# distribution of random effects
p_re_sqb <- tibble(random_eff = random_eff) %>% 
  ggplot() + 
  geom_histogram(aes(x = random_eff), col = "white", fill = "grey30", bins = 25, linewidth = 0.25) + 
  theme_minimal(11) + 
  theme(text = element_text(family = "Segoe UI")) + 
  labs(y = "Frequency", x = "Estimated hospital effect")

png("results/figure_alternative_analysis.png", width = 3500, height = 1300, res = 390)
p_re_sqb + p_event_sqb + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = 'bold'))
dev.off()

#############################
#### export session info ####
#############################
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
