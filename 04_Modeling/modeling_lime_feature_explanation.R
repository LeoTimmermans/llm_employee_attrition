# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
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

# Processing Pipeline
source("00_functions/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(
  train_raw_tbl,
  definitions_raw_tbl
)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(JobLevel, StockOptionLevel, fn = factor) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel(
  path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_4_AutoML_1_20251017_150519"
)

automl_leader

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as_tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )
predictions_tbl


test_tbl %>%
  slice(5) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model = automl_leader,
    bin_continous = TRUE,
    n_bins = 4,
    quantile_bins = TRUE
  )

explainer

explanation <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width = 1 # vary to get best model_r2
  )

explanation %>%
  as_tibble() %>%
  select(feature:prediction)

plot_features(explanation = explanation)


# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width = 1 # vary to get best model_r2
  )

explanation %>%
  as_tibble()

plot_features(explanation = explanation, ncol = 4)

plot_explanations(explanation = explanation)

# global try (not in course)
tictoc::tic()
global_explanation <- test_tbl %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width = 1 # vary to get best model_r2
  )
tictoc::toc()


color_pos = palette_light()[[1]]
color_neg = palette_light()[[2]]

global_explanation %>%
  filter(label == "Yes") |>
  as_tibble() %>%
  group_by(label, feature, feature_desc) %>%
  summarise(feature_weight = mean(feature_weight)) %>%
  ungroup() %>%
  mutate(sign = ifelse(feature_weight >= 0, "Supports", "Contradicts")) %>%
  # arrange(desc(abs(feature_weight))) %>%
  mutate(
    feature_desc = as.factor(feature_desc) %>% fct_reorder(abs(feature_weight))
  ) %>%
  ggplot(aes(x = feature_weight, y = feature_desc, fill = sign)) +
  geom_col() +
  theme_tq() +
  scale_fill_manual(values = c(color_neg, color_pos))

global_explanation %>%
  filter(label == "No") |>
  as_tibble() %>%
  group_by(label, feature, feature_desc) %>%
  summarise(feature_weight = mean(feature_weight)) %>%
  ungroup() %>%
  mutate(sign = ifelse(feature_weight >= 0, "Supports", "Contradicts")) %>%
  # arrange(desc(abs(feature_weight))) %>%
  mutate(
    feature_desc = as.factor(feature_desc) %>% fct_reorder(abs(feature_weight))
  ) %>%
  ggplot(aes(x = feature_weight, y = feature_desc, fill = sign)) +
  geom_col() +
  theme_tq() +
  scale_fill_manual(values = c(color_neg, color_pos))

tidy_global_explanation <- global_explanation |>
  unnest(prediction) |>
  unnest(prediction) |>
  mutate(
    # ensure each element is a tibble, then coerce every column to character
    data = map(
      data,
      ~ as_tibble(.x) |> mutate(across(everything(), as.character))
    )
  ) |>
  unnest(data)

tidy_global_explanation |> glimpse()

# reference feature types from training data (used when building the explainer)
ref_features <- train_tbl %>% select(-Attrition)

convert_one <- function(dat, ref) {
  dat <- as_tibble(dat)
  common <- intersect(names(dat), names(ref))
  for (nm in common) {
    ref_col <- ref[[nm]]
    cls <- class(ref_col)
    if ("factor" %in% cls) {
      # strict: only levels seen in training will be valid (unseen -> NA)
      dat[[nm]] <- factor(
        as.character(dat[[nm]]),
        levels = levels(ref_col),
        ordered = is.ordered(ref_col)
      )
      # alternative (preserve unseen values by adding levels):
      # dat[[nm]] <- factor(as.character(dat[[nm]]),
      #                     levels = union(levels(ref_col), unique(as.character(dat[[nm]]))),
      #                     ordered = is.ordered(ref_col))
    } else if ("integer" %in% cls) {
      dat[[nm]] <- as.integer(dat[[nm]])
    } else if ("numeric" %in% cls) {
      dat[[nm]] <- as.numeric(dat[[nm]])
    } else if ("logical" %in% cls) {
      dat[[nm]] <- as.logical(dat[[nm]])
    } else if ("character" %in% cls) {
      dat[[nm]] <- as.character(dat[[nm]])
    } else {
      # leave other types unchanged
    }
  }
  dat
}

tidy_global_explanation <- global_explanation |>
  unnest(prediction) |>
  unnest(prediction) |>
  mutate(
    data = map(data, ~ convert_one(.x, ref_features))
  ) |>
  unnest(data)

tidy_global_explanation
tidy_global_explanation |> saveRDS("00_data/tidy_global_explanation.rds")
readRDS("00_data/tidy_global_explanation.rds")
