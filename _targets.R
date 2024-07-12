# ====== Setup =================================================================
### Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(tidyverse)
library(qs)
library(formulaic)

# Set target options:
tar_option_set(format = "qs")

### Source R code containing functions used in the pipeline:
sapply(list.files(here::here("R"), full.names = TRUE), source)
# could use `tar_source` but this works

# ====== Adjustable Parameters =================================================
### Influential Case Multiple of Mean
param_influential <- 5

var_apriori <- c(
  "rec_type",
  "std_smp_cty_weird",
  "es_mth_escalc",
  "std_mth_smp_setting",
  "es_mth_msr_source",
  "es_cnd_noncm_desig",
  "std_qlt_int",
  "std_smp_subject",
  "std_smp_grade",
  # "es_cnd_cm_collab",
  "es_cnd_cm_train",
  # "es_cnd_cm_med_int",
  "es_cnd_cm_use"
  # "es_cnd_duration"
)

form_apriori <- create.formula(outcome.name = "yi", input.names = var_apriori)

var_ml <- c(
  "rec_yrpub",
  "rec_type",
  "std_smp_cty_weird",
  "es_mth_escalc",
  "std_mth_smp_setting",
  # "std_mth_smp_breadth",
  "es_mth_msr_source",
  "es_cnd_noncm_desig",
  "std_qlt_int",
  "es_qlt_measure",
  "std_smp_subject",
  "std_smp_grade",
  # "es_cnd_cm_collab",
  "es_cnd_cm_train",
  # "es_cnd_cm_med_int",
  "es_cnd_cm_use",
  # "es_cnd_cm_type",
  # "es_cnd_duration",
  "es_msr_timing"
)

form_ml <- create.formula(outcome.name = "yi", input.names = var_ml)

var_ml_post <- c(
  "rec_yrpub",
  "rec_type",
  "std_smp_cty_weird",
  # "es_mth_escalc",
  # "std_mth_smp_setting",
  # "std_mth_smp_breadth",
  # "es_mth_msr_source",
  "es_cnd_noncm_desig",
  "std_qlt_int",
  "es_qlt_measure",
  # "std_smp_subject",
  "std_smp_grade"
  # "es_cnd_cm_collab",
  # "es_cnd_cm_train",
  # "es_cnd_cm_med_int",
  # "es_cnd_cm_use",
  # "es_cnd_cm_type",
  # "es_cnd_duration",
  # "es_msr_timing"
)

form_ml_post <- create.formula(outcome.name = "yi", input.names = var_ml_post)

# ====== Pipeline ==============================================================
list(
  ### Data Handling targets: see `data_processing.r` script for functions
  tar_target(
    data_es_stats_raw,
    calc_missing_stats(
      data = read_csv(here("data", "es_stats-2.csv")),
      assumed_rpp = 0.6
    ),
  ),
  tar_target(
    data_es_stats_full,
    calc_es(data = data_es_stats_raw)
  ),
  tar_target(
    data_es_stats_proc,
    algorithm_es(data = data_es_stats_full)
  ),
  tar_target(
    data_es_review,
    # processing for review table (selecting, renaming, and algorithms)
    # .$full          | for systematic review description
    # .$reduced       | for missingness analysis
    proc_review_coding(
      coding_data = read_csv(
        here("data", "meta_reviewer_results", "FT_coding_results.csv")
        ),
      screening_data = qread(
        here("data", "meta_reviewer_results", "phase1_FTstd_results")
        ),
      es_data = data_es_stats_proc
    )
  ),
  # # target for descriptive analysis of missing data
  # tar_target(
  #   missing_analysis,
  #   missing_data_function()
  # ),
  # tar_quarto(
  #   report_missing_data,
  #   path = here("reports", "report_missing_data.qmd")
  # ),
  # # target for descriptive analysis of variables of potential interest
  # descriptives may include frequency tables and Kruskal-Wallis (sp?) plots
  # tar_target(
  #   description_analysis,
  #   description_analysis_function()
  # ),
  tar_target(
    data_es_meta,
    # processing for meta table (selecting, centering, and re-classing)
    # .$transformed   | for
    # .$regression    | for
    # .$imputation    | for
    # .$tbl_missing   | for
    proc_meta_coding(
      data = data_es_review$full,
      threshold_miss = 50
    )
  ),
  tar_target(
    data_vcv_matrix,
    calc_matrix(
      data = data_es_meta$regression,
      between_time_r = 0.6,
      between_obs_occ_r = c(0.8, 0.4)
    )
  ),
  tar_target(
    model_base,
    rma_basemodel(
      plot_tag = "base",
      profile_plot = TRUE,
      es_plot = TRUE,
      data = data_es_meta$regression,
      vcv = data_vcv_matrix,
    )
  ),
  tar_target(
    influential_cases,
    # identification of influential cases
    # .$index         | for identification of influential cases
    # .$distances     | cooks distances values
    diagnostic_cooks(
      plot_tag = "base",
      data_model = model_base,
      threshold_influential = param_influential
    )
  ),
  tar_target(
    model_base_out,
    rma_basemodel(
      plot_tag = "base_out",
      profile_plot = TRUE,
      es_plot = TRUE,
      data = data_es_meta$regression,
      vcv = data_vcv_matrix,
      prior_model = model_base,
      influential = influential_cases$index
    )
  ),

### Primary Analyses #################################################
  tar_target(
    model_apriori_out,
    rma_multi(
      data = data_es_meta$regression,
      vcv = data_vcv_matrix,
      formula = form_apriori$formula,
      basemodel = model_base_out,
      influential = influential_cases$index
    )
  ),
  tar_target(
    check_ml_convergence,
    ml_converge(
      n_trees = 10000,
      data = data_es_meta$regression,
      form = form_ml$formula,
      influential = influential_cases$index
    )
  ),
  tar_target(
    preselected_mods,
    ml_preselect(
      n_trees = 5000,
      reps = 100,
      data = data_es_meta$regression,
      form = form_ml$formula,
      influential = influential_cases$index
    )
  # ),
  # tar_target(
  #   trained_model,
  #   ml_train(
  #     n_trees = 5000,
  #     data = data_es_meta$regression,
  #     mods = preselected_mods,
  #     influential = influential_cases$index
  #   )
  ),
  tar_target(
    model_ml_post_out,
    rma_multi(
      data = data_es_meta$regression,
      vcv = data_vcv_matrix,
      formula = form_ml_post$formula,
      basemodel = model_base_out,
      influential = influential_cases$index
    )
  )
  # ),
  # tar_target(
  #   model_rand_forest_out,
  #   rma_ml()

### Sensitivity and Exploratory Analyses ############################
  # tar_target(
  #   model_univar_out,
    # rma_formula(
    #   formula = list(something),
    #   data = data_es_meta,
    #   vcv = data_vcv_matrix,
    #   index_study = index_influential
    # )
  # ),
  )
)



#===== Pipeline Associated with Lit Search, Handling, and Analysis =====
# Database Search Results
  # tar_target(
  #   compiled_db_search,
  #   compile_db_search(here("data", "database_search_results"))
  # ),
  # tar_target(
  #   deduped_db_search_auto,
  #   ASySD::dedup_citations(
  #     compiled_db_search,
  #     extra_merge_fields = "database"
  #   )
  # ),
  # tar_target(
  #   output_db_possible_dupes,
  #   write_csv(
  #     deduped_db_search_auto[["manual_dedup"]],
  #     here("data", "compiled_search_results", "db_possible_dupes.csv")
  #   )
  # ),
  # tar_target(
  #   deduped_db_search_man,
  #   ASySD::dedup_citations_add_manual(
  #     deduped_db_search_auto[["unique"]],
  #     additional_pairs = read_csv(
  #       here("data", "compiled_search_results", "db_true_dupes.csv")
  #     ),
  #     extra_merge_fields = "database"
  #   )
  # ),
  # tar_target(
  #   output_db_abstrackr,
  #   rename(id = duplicate_id, authors = author) |>
  #     write_tsv(
#   #       here("data", "compiled_search_results", "db_abstrackr_export.tsv")
#       )
#   # ),
#   # tar_target(
#   #   results_db_abstrackr,
#   #   read_csv(here("data", "abstrackr_results", "db_abstrackr_results.tsv"))
#   # ),
#   # tar_target(
#   #   output_db_metarev,
#   #   results_db_abstrackr |>
#   #     semi_join(
#   #       abstrackr_results1 |> filter(`Aaron Wenger` == 1),
#   #       join_by(id == `(source) id`)
#   #     )
#   )
#
# #   tar_target(
# #     man_dedup_search,
# #
# #   )
# )


