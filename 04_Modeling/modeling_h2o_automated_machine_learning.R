# H2O MODELLING ----

# 1. Setup ----

# 1.1 Load Libraries ----
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(glue)
library(cowplot)
library(fs)

# 1.2 Load Data ----
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(
  path_data_definitions,
  sheet = 1,
  col_names = FALSE
)

# 1.3 Processing Pipeline ----
source("00_functions/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(
  train_raw_tbl,
  definitions_raw_tbl
)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# 1.4 ML Preprocessing ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
  prep()

train_tbl <- recipe_obj %>% bake(new_data = train_readable_tbl)
test_tbl <- recipe_obj %>% bake(new_data = test_readable_tbl)

train_tbl %>% glimpse()

# 2. Modelling ----

h2o.init() # start Java and connect to R

split_h2o <- h2o.splitFrame(
  train_tbl %>% as.h2o(),
  ratios = c(0.85),
  seed = 1234
)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

# INFOGRAM (not in course) ----
# infogram for feature selection: core infogram
cig <- h2o.infogram(y = y, training_frame = train_h2o)
plot(cig)
casf <- cig@admissible_score
casf %>% as_tibble() %>% filter(admissible == 1)
# only OverTime and JobRole are relevant and unique

# infogram for admissible machine learning: fair infogram
# Protected columns
pcols <- c("Age", "Gender")
fig <- h2o.infogram(
  y = y,
  training_frame = train_h2o,
  protected_columns = pcols
)
plot(fig)
fasf <- fig@admissible_score
fasf %>% as_tibble() %>% filter(admissible == 0)

# Get the names of columns to keep
cols_to_keep <- setdiff(names(train_h2o), pcols)
ftrain_h2o <- train_h2o[, cols_to_keep]
fvalid_h2o <- valid_h2o[, cols_to_keep]
ftest_h2o <- test_h2o[, cols_to_keep]

y <- "Attrition"
x <- setdiff(names(ftrain_h2o), y)

# 2.1 build model ----
fautoml_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = ftrain_h2o,
  validation_frame = fvalid_h2o,
  leaderboard_frame = ftest_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

typeof(fautoml_models_h2o)
slotNames(fautoml_models_h2o)
fautoml_models_h2o@leaderboard
fautoml_models_h2o@leader

# leaderboard_frame is not needed so:
fautoml_models_h2o_2 <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_tbl %>% select(-pcols) %>% as.h2o(),
  leaderboard_frame = ftest_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)
fautoml_models_h2o_2@leaderboard
fautoml_models_h2o_2@leader # best model on ....

# h2o.getModel("GLM_1_AutoML_20211015_155027")
# h2o.getModel("DeepLearning_grid__2_AutoML_20211015_155027_model_2")

# save model(s)
# Convert the H2O leaderboard to an R data.frame, extract the top model_id as a string,
# then load and save the model.
leaderboard_df <- as.data.frame(fautoml_models_h2o@leaderboard)
top_model_id <- leaderboard_df$model_id[1]

top_model <- h2o.getModel(top_model_id)
# Save (overwrite if exists) and then load using the exact returned path
saved_model_path <- h2o.saveModel(
  top_model,
  path = "04_Modeling/h2o_models/",
  force = TRUE
)
loaded_model <- h2o.loadModel(saved_model_path)


# 3. Predicting ----
stacked_ensemble_h2o <- loaded_model
stacked_ensemble_h2o

prediction_h2o <- h2o.predict(object = stacked_ensemble_h2o, newdata = test_h2o)
predictions_tbl <- prediction_h2o %>% as_tibble()
predictions_tbl

# get parameters of a model
stacked_ensemble_h2o@allparameters

# 4. Visualizing the Leaderboard ----
fautoml_models_h2o_2@leaderboard
data_transformed_tbl <- fautoml_models_h2o_2@leaderboard %>%
  as.tibble() %>%
  select(model_id, auc, logloss, rmse) %>%
  mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[, 1]) %>%
  rownames_to_column(var = "rowname") %>%
  mutate(
    model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor()
  ) %>%
  slice(1:11) %>%
  mutate(
    model_id = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>%
  gather(
    key = key,
    value = value,
    -c(model_id, model_type, rowname),
    factor_key = T
  )

data_transformed_tbl %>%
  ggplot(aes(x = value, y = model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~key, scales = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Leaderboard Metrics",
    subtitle = paste0("Ordered by: ", toupper("auc")),
    y = "Model Postion, Model ID",
    x = ""
  )

# 5. Assessing Performance ----
# load previous models
deeplearning_h2o <- h2o.loadModel(
  "04_Modeling/h2o_models/DeepLearning_grid__1_AutoML_20211015_153946_model_3"
)
stacked_ensemble_h2o <- h2o.loadModel(
  "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20211015_153946"
)
glm_h2o <- h2o.loadModel("04_Modeling/h2o_models/GLM_1_AutoML_20211015_153946")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = test_h2o)
typeof(performance_h2o)
performance_h2o %>% slotNames()
performance_h2o@metrics

# Classifier summary Metrics
# AUC
h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)

h2o.auc(performance_h2o)
performance_h2o@metrics$AUC

# Gini
h2o.giniCoef(performance_h2o)
performance_h2o@metrics$Gini

# Logloss
h2o.logloss(performance_h2o)
performance_h2o@metrics$logloss

# Confusion Matrix
h2o.confusionMatrix(stacked_ensemble_h2o)
h2o.confusionMatrix(performance_h2o)
performance_h2o@metrics$cm$table # does not show max f1 threshold

# h2o metrics
performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as_tibble() %>%
  glimpse()
performance_tbl

performance_tbl %>%
  filter(f1 == max(f1))
performance_tbl %>%
  filter(f2 == max(f2)) # leaning toward recall

# visualization precision vs recall
performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  geom_vline(
    aes(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")),
    color = "orange",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f2")),
    color = "green",
    size = 1
  ) +
  theme_tq() +
  labs(title = "Precision vs Recall", y = "value")

# Performance of multiple models ----

load_model_performance_metrics <- function(path, test_h2o) {
  model_h2o <- h2o.loadModel(path)
  perform_h2o <- h2o.performance(model_h2o, newdata = test_h2o)

  perform_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perform_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
}

# for full directory
model_metrics_tbl <-
  fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_h2o)) %>%
  unnest(cols = c(metrics))

# * plot ROC-curve ----
model_metrics_tbl %>%
  mutate(
    path = str_split(string = path, pattern = "/", simplify = TRUE, )[, 3] %>%
      as_factor
  ) %>%
  mutate(auc = auc %>% round(digits = 3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(x = fpr, y = tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(title = "ROC Plot", subtitle = "Performance of 3 Top Performing Models")

# * Precision vs Recall ----
model_metrics_tbl %>%
  mutate(
    path = str_split(string = path, pattern = "/", simplify = TRUE, )[, 3] %>%
      as_factor
  ) %>%
  mutate(auc = auc %>% round(digits = 3) %>% as.character() %>% as_factor()) %>%
  ggplot(aes(x = recall, y = precision, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )

# * Gain & Lift ----
ranked_predictions_tbl <-
  predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, Attrition) %>%
  arrange(desc(Yes))

calculated_gain_lift_tbl <-
  ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarize(
    cases = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  mutate(group = row_number()) %>%
  select(group, cases, responses) %>%
  mutate(
    cum_responses = cumsum(responses),
    pct_responses = responses / sum(responses),
    gain = cumsum(pct_responses),
    cum_pct_cases = cumsum(cases) / sum(cases),
    lift = gain / cum_pct_cases,
    gain_baseline = cum_pct_cases,
    lift_baseline = gain_baseline / cum_pct_cases
  )
calculated_gain_lift_tbl

gain_lift_tbl <-
  performance_h2o %>%
  h2o.gainsLift() %>%
  as_tibble()

gain_transformed_tbl <- gain_lift_tbl %>%
  select(
    group,
    cumulative_data_fraction,
    cumulative_capture_rate,
    cumulative_lift
  ) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain = cumulative_capture_rate) %>%
  pivot_longer(cols = gain:baseline, names_to = "key")

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Gain Chart", x = "cumulative Data Fraction", y = "Gain")

lift_transformed_tbl <- gain_lift_tbl %>%
  select(
    group,
    cumulative_data_fraction,
    cumulative_capture_rate,
    cumulative_lift
  ) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  pivot_longer(cols = lift:baseline, names_to = "key")

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Lift Chart", x = "cumulative Data Fraction", y = "Gain")

# 6. Performance Visualization ----
h2o_leaderboard <- fautoml_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "auc"
max_models <- 4
size <- 1

plot_h2o_performance <- function(
  h2o_leaderboard,
  newdata,
  order_by = c("auc", "logloss"),
  max_models = 3,
  size = 1.5
) {
  # Inputs

  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(1:max_models)

  newdata_tbl <- newdata %>%
    as.tibble()

  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)

  h2o.no_progress()

  # 1. Model metrics

  get_model_performance_metrics <- function(model_id, test_tbl) {
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))

    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
  }

  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(
      metrics = map(model_id, get_model_performance_metrics, newdata_tbl)
    ) %>%
    unnest(cols = c(metrics)) %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(
          !!order_by_expr,
          .desc = ifelse(order_by == "auc", TRUE, FALSE)
        ),
      auc = auc %>%
        round(3) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>%
        round(4) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id))
    )

  # 1A. ROC Plot

  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical")

  # 1B. Precision vs Recall

  p2 <- model_metrics_tbl %>%
    ggplot(aes_string(
      "recall",
      "precision",
      color = "model_id",
      linetype = order_by
    )) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none")

  # 2. Gain / Lift

  get_gain_lift <- function(model_id, test_tbl) {
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))

    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(
        group,
        cumulative_data_fraction,
        cumulative_capture_rate,
        cumulative_lift
      )
  }

  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest(cols = c(metrics)) %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(
          !!order_by_expr,
          .desc = ifelse(order_by == "auc", TRUE, FALSE)
        ),
      auc = auc %>%
        round(3) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>%
        round(4) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    )

  # 2A. Gain Plot

  p3 <- gain_lift_tbl %>%
    ggplot(aes_string(
      "cumulative_data_fraction",
      "gain",
      color = "model_id",
      linetype = order_by
    )) +
    geom_line(size = size) +
    geom_segment(
      x = 0,
      y = 0,
      xend = 1,
      yend = 1,
      color = "black",
      size = size
    ) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain", x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")

  # 2B. Lift Plot

  p4 <- gain_lift_tbl %>%
    ggplot(aes_string(
      "cumulative_data_fraction",
      "lift",
      color = "model_id",
      linetype = order_by
    )) +
    geom_line(size = size) +
    geom_segment(
      x = 0,
      y = 1,
      xend = 1,
      yend = 1,
      color = "black",
      size = size
    ) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift", x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none")

  # Combine using cowplot
  extract_legend <- function(plot) {
    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
    guide_pos <- which(sapply(gt$grobs, function(x) x$name) == "guide-box")
    if (length(guide_pos) == 0) {
      return(NULL)
    }
    gt$grobs[[guide_pos]]
  }

  p_legend <- extract_legend(p1)
  p1 <- p1 + theme(legend.position = "none")

  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)

  p_title <- ggdraw() +
    draw_label(
      "H2O Model Metrics",
      size = 18,
      fontface = "bold",
      colour = palette_light()[[1]]
    )

  p_subtitle <- ggdraw() +
    draw_label(
      glue("Ordered by {toupper(order_by)}"),
      size = 10,
      colour = palette_light()[[1]]
    )

  ret <- plot_grid(
    p_title,
    p_subtitle,
    p,
    p_legend,
    ncol = 1,
    rel_heights = c(0.05, 0.05, 1, 0.05 * max_models)
  )

  h2o.show_progress()

  return(ret)
}

fautoml_models_h2o@leaderboard %>%
  plot_h2o_performance(
    newdata = test_tbl,
    order_by = "auc",
    max_models = 5
  )
