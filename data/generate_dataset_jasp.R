# Script to generate dataset in wide format for JASP
library(tidyverse)

# Stroop data
# # ---------------------------------------------------------------------
# read in & filter data
d_stroop <- read.csv("data/clean/dataset_stroop_clean.csv") |>
    filter(rt > 0.099 & rt < 8)|>

    mutate(condition = case_when(congruent == 1 ~ "congruent",
                                 congruent == 0 ~ "incongruent")) |>
    filter(id != "sub-009") |> # Ausschluss wegen zu tiefer Accuracy (< 0.33)
    filter(id != "sub-180") |> # Ausschluss wegen zu tiefer Accuracy (< 0.33)
    filter(id != "sub-201") # Ausschluss wegen zu tiefer Accuracy (< 0.33)

# change format to wide (by conditions)
d_stroop_wide_rt <- d_stroop |>
    pivot_wider(id_cols = c(id),
                names_from = condition,
                values_from = rt,
                values_fn = mean,
                names_prefix = "rt_") # take mean for id/conditions

d_stroop_wide_acc <- d_stroop |>
    pivot_wider(id_cols = c(id),
                names_from = condition,
                values_from = corr,
                values_fn = mean,
                names_prefix = "corr_") # take mean for id/conditions

# merge dataframes
d_stroop_wide <- merge(d_stroop_wide_rt, d_stroop_wide_acc,
                    by = "id")
# save new file
write.csv(d_stroop_wide, "data/data_stroop_wide.csv", row.names = FALSE)

# Random Dot data
# ---------------------------------------------------------------------
# read in & filter data
d_rdk<- read.csv("data/dataset_random_dot_clean.csv") |>
    filter(rt > 0.1 & rt < 6)

## Preprocessing

# a. missings?
naniar::vis_miss(d_rdk) #ok

# b. summarise
acc_rt_individual <- d_rdk |>
group_by(id, condition) |>
    summarise(
        N = n(),
        ncorrect = sum(corr),
        accuracy = mean(corr),
        median_rt = median(rt)
    )
acc_rt_individual

# Plot: Anzahl Trials pro Bedingung für jede Versuchsperson
acc_rt_individual |>
    ggplot(aes(x = id, y = N)) +
    geom_point() +
    facet_wrap(~ condition) +
    geom_hline(yintercept = 40) +
    theme_minimal()

# Datensatz mit allen Ids, welche zuwenig Trials hatten
n_exclusions <- acc_rt_individual |>
    filter(N < 40)

# Aus dem Hauptdatensatz diese Ids ausschliessen
d_rdk <- d_rdk |>
    filter(!id %in% n_exclusions$id)

# Check
d_acc_rt_individual <- d_rdk |>
    group_by(id, condition) |>
    summarise(
        N = n(),
        ncorrect = sum(corr),
        accuracy = mean(corr),
        median_rt = median(rt)
    )

d_acc_rt_individual |>
    ggplot(aes(x = id, y = N)) +
    geom_point() +
    facet_wrap(~ condition) +
    geom_hline(yintercept = 40) + # Horizontale Linie einfügen
    theme_minimal()
#
# Trials nach accuracy einteilen
d_acc_rt_individual_grouped <- d_acc_rt_individual %>%
    mutate(
        performance = case_when(
            accuracy > 0.85 ~ "good",
            accuracy < 0.55 ~ "bad",
            TRUE ~ "ok") %>%
            factor(levels = c("good", "ok", "bad")))

# Datensatz mit allen Ids, welche zu schlechte accuracy hatten

d_acc_rt_individual <- d_rdk |>
    group_by(id) |>
    summarise(
        N = n(),
        ncorrect = sum(corr),
        accuracy = mean(corr),
        median_rt = median(rt)
    )
n_exclusions <- d_acc_rt_individual |>
    filter(accuracy < 0.55)

# Aus dem Hauptdatensatz diese Ids ausschliessen
d_rdk <- d_rdk |>
    filter(!id %in% n_exclusions$id)

d_acc_rt_individual <- d_rdk |>
    group_by(id, condition) |>
    summarise(
        N = n(),
        ncorrect = sum(corr),
        accuracy = mean(corr),
        median_rt = median(rt)
    )

# Outlier visualisieren

# Trials nach accuracy einteilen
d_acc_rt_individual_grouped <- d_acc_rt_individual %>%
    mutate(
        performance = case_when(
            accuracy > 0.85 ~ "good",
            accuracy < 0.55 ~ "bad",
            TRUE ~ "ok") %>%
            factor(levels = c("good", "ok", "bad")))
d_acc_rt_individual_grouped %>%
    ggplot(aes(x = id, y = accuracy, color = performance, shape = performance)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_point(data = filter(d_acc_rt_individual_grouped, performance != "OK"),
               alpha = 0.9) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray40", "steelblue", "red")) +
    geom_hline(yintercept = 0.5, linetype='dotted', col = 'black')+
    annotate("text", x = "sub-100", y = 0.33, label = "chance level", vjust = -1, size = 3) +
    theme_minimal(base_size = 12)

# Verlaufseffekte
d_acc_rt_trial <- d_rdk |>
    group_by(condition, trial) |>
    summarise(
        accuracy = mean(corr),
        median_rt = median(rt)
    )

d_acc_rt_trial |>
    ggplot(aes(x = trial, y = accuracy, color = condition)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line() +
    scale_color_manual(values = c(accuracy = "tomato3",
                                  speed = "skyblue3")) +
    facet_wrap(~ condition) +
    theme_minimal(base_size = 12)

d_acc_rt_trial |>
    ggplot(aes(x = trial, y = median_rt, color = condition)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line() +
    scale_color_manual(values = c(accuracy = "tomato3",
                                  speed = "skyblue3")) +
    facet_wrap(~ condition) +
    theme_minimal(base_size = 12)

# Plot accuracy per person and condition
d_acc_rt_individual |>
    ggplot(aes(x = condition, y = accuracy, color = condition)) +
    geom_jitter(size = 3, alpha = 0.4,
                width = 0.2, height = 0) +
    geom_boxplot(width = 0.1, alpha = 0, color = "black") +
    scale_color_manual(values = c(accuracy = "tomato2",
                                  speed = "skyblue3")) +
    labs(x = "Instruction",
         y = "Proportion correct",
         title = "Accuracy per Person and Condition") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

d_acc_rt_individual |>
    ggplot(aes(x = condition, y = median_rt, color = condition)) +
    geom_jitter(size = 3, alpha = 0.4,
                width = 0.2, height = 0) +
    geom_boxplot(width = 0.1, alpha = 0, color = "black") +
    scale_color_manual(values = c(accuracy = "tomato2",
                                  speed = "skyblue3")) +
    labs(x = "Instruction",
         y = "Median Response Time [s]",
         title = "Median Response Time per Person and Condition") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

p3 <- d_acc_rt_individual |>
    ggplot(aes(x = condition, y = accuracy, color = condition, group = id)) +
    geom_line(color = "grey40", alpha = 0.5) +
    geom_jitter(size = 3, alpha = 0.8,
                width = 0, height = 0) +
    scale_color_manual(values = c(accuracy = "tomato2",
                                  speed = "skyblue3")) +
    labs(x = "Instruction",
         y = "Proportion correct",
         title = "Accuracy per Person and Condition") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

p4 <- d_acc_rt_individual |>
    ggplot(aes(x = condition, y = median_rt, color = condition, group = id)) +
    geom_line(color = "grey40", alpha = 0.5) +
    geom_jitter(size = 3, alpha = 0.8,
                width = 0, height = 0) +
    scale_color_manual(values = c(accuracy = "tomato2",
                                  speed = "skyblue3")) +
    labs(x = "Instruction",
         y = "Median Response Time [s]",
         title = "Median Response Time per Person and Condition") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

p3 + p4


# create new dataset for uebung-04

d_rdk_long <- write.csv(d_acc_rt_individual, "data/data_random_dot_long.csv")

# change format to wide (by conditions)
d_rdk_wide_condition <- d_rdk |>
    pivot_wider(id_cols = c(id),
                names_from = condition,
                values_from = c(rt, corr),
                values_fn = mean) # take mean for id/conditions

# change format to wide (by conditions and directions)
d_rdk_wide_direction <- d_rdk |>
    pivot_wider(id_cols = c(id),
                names_from = c(condition, direction),
                values_from = c(rt, corr),
                values_fn = mean) # take mean for id/conditions

# merge dataframes
d_rdk_wide <- merge(d_rdk_wide_condition, d_rdk_wide_direction,
                       by = "id")

# save new file
write.csv(d_rdk_wide, "data/data_random_dot_wide.csv", row.names = FALSE)
