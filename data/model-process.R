source("data/dependencies.R")

ghg_data_merged <- read_csv("/data.csv")

# Modeling ----------------------------------------------------------------

mod_tbl <- ghg_data_merged |> 
  summarize(
    .by = c(year, gas),
    emission = sum(emission)
  ) |> 
  mutate(
    year = make_date(year = year, month = 12, day = 31),
    gas = factor(gas)
  )

mod_tbl_nested <- mod_tbl |> 
  extend_timeseries(
    .id_var = gas,
    .date_var = year,
    .length_future = 20
  ) |> 
  nest_timeseries(
    .id_var = gas,
    .length_future = 20,
    .length_actual =  32
  ) |> 
  split_nested_timeseries(
    .length_test = 6
  )

## Feature engineering ----------------------------------------------------

mod_rec <- recipe(emission ~ ., data = extract_nested_train_split(mod_tbl_nested))  |> 
  step_timeseries_signature(year) |> 
  step_rm(matches("(_month)|(_wday)|(week)|(year_)")) |> 
  step_zv()

mod_rec2 <- mod_rec |> 
  update_role(year, new_role = "ID")

## Model Specification --------------------------------------------------

est_model <- exp_smoothing() |> 
  set_mode("regression") |> 
  set_engine("ets") 

prop_mod <- prophet_reg() |> 
  set_mode("regression") |> 
  set_engine("prophet")

rand_mod <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

prop_boost_mod <- prophet_boost() |> 
  set_mode("regression") |> 
  set_engine("prophet_xgboost")

lm_mod <- linear_reg() |> 
  set_mode("regression") |> 
  set_engine("lm")

## Model Workflow --------------------------------

est_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(est_model)

prop_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(prop_mod)

rand_wf <- workflow() |> 
  add_recipe(mod_rec2) |> 
  add_model(rand_mod)

prop_bst_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(prop_boost_mod)

lm_wf <- workflow() |> 
  add_model(lm_mod) |> 
  add_recipe(mod_rec)


mod_fit <- modeltime_nested_fit(
  nested_data = mod_tbl_nested,
  est_wf, 
  prop_wf,
  rand_wf,
  prop_bst_wf,
  lm_wf
)

mod_fit |> 
  extract_nested_test_accuracy() |> 
  pivot_longer(
    cols = mae:rsq,
    names_to = "eval_metric",
    values_to = "error"
  ) |> 
  filter(!is.na(error)) |> 
  ggplot(aes(fct_reorder(gas, error), error, fill = eval_metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~eval_metric, scales = "free_x") +
  coord_flip()


## Extract best model ------------------------------------------------------


mod_best <- mod_fit |> 
  modeltime_nested_select_best(metric = "mae")

mod_best |> 
  extract_nested_best_model_report()

forecast <- mod_best |> 
  extract_nested_test_forecast()


ggplotly(forecast |> 
  ggplot(aes(.index, .value, col = .model_desc)) +
  geom_line() +
  facet_wrap(~gas))
  

mod_best |>
  extract_nested_test_forecast() |> 
  group_by(gas) |>
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .legend_show = FALSE,
    .conf_interval_show = FALSE
  )

mod_best

mod_refit |> 
  extract_nested_future_forecast() |> 
  print(n =5100)

# model refit -------------------------------------------------------------

mod_refit <- mod_best |> 
  modeltime_nested_refit()

saveRDS(mod_refit, "data/forecast_mod.rds")
