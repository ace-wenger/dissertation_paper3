# ====== Setup =================================================================
### Load packages required to define the pipeline:
library(targets)
library(here)
library(tidyverse)
library(qs)

# Set target options:
tar_option_set(format = "qs")

### Source R code containing functions used in the pipeline:
sapply(list.files(here::here("R"), full.names = TRUE), source)
# could use `tar_source` but this works

# ====== Pipeline ==============================================================
list(
  ### Data Handling targets: see `data_processing.r` script for functions
  tar_target(
    data_es_stats_raw,
    calc_missing_stats(
      data = read_csv(here("es_stats-2.csv")),
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
  # processing for review table (selecting, renaming, and algorithms)
  tar_target(
    data_es_review,
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
  # processing for meta table (selecting, centering, and re-classing)
  tar_target(
    data_es_meta,
    proc_meta_coding(
      data = data_es_review,
      threshold_miss = 35
    )
  # ),
  # # target for descriptive analysis of missing data
  # tar_target(
  #   missing_analysis,
  #   missing_data_function()
  # ),
  # # target for descriptive analysis of variables of potential interest
  # descriptives may include frequency tables and Kruskal-Wallis (sp?) plots
  # tar_target(
  #   description_analysis,
  #   description_analysis_function()
  ),
  tar_target(
    data_vcv_matrix,
    calc_matrix(
      data = data_es_meta,
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
      data = data_es_meta,
      vcv = data_vcv_matrix,
    )
  ),
  #
  tar_target(
    index_influential,
    diagnostic_cooks(
      plot_tag = "base",
      data_model = model_base,
      threshold_influential = 5
    )
  ),
  ## need to change `rma_basemodel` to accept `index_study` argument
  tar_target(
    model_base_out,
    rma_basemodel(
      plot_tag = "base_out",
      profile_plot = TRUE,
      es_plot = TRUE,
      data = data_es_meta,
      vcv = data_vcv_matrix,
      prior_model = model_base,
      influential = index_influential
    )
  # ),

### Primary Analyses #################################################
  # tar_target(
  #   model_apriori_out,
    # rma_formula(
    #   formula = something,
    #   data = data_es_meta,
    #   vcv = data_vcv_matrix,
    #   index_study = index_influential
    # )
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


