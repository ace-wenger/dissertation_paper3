#' Compile database search results
#'
#' A wrapper for [ASySD::load_multi_search()]. Requires that all database search
#' results are formatted as `.ris` files and are contained within one folder.
#'
#' @param data_subfolder_path The file path to the `.ris` files to be compiled.
#'
#' @return Returns a dataframe containing all citations in the `.ris`
#'   files present in `data_subfolder_path`.
compile_db_search <- function(subfolder_path){

  # Make list of .ris files in data_subfolder_path
  citation_files <- list.files(
    path = data_subfolder_path,
    pattern = "\\.ris",
    full.names = TRUE
  )

  # Extract file names from .ris files
  source_labels <- str_extract(citation_files, "(?<=\\/)[^\\/]*?(?=(\\.ris))")

  # Compile contents
  citations <- ASySD::load_multi_search(
    citation_files,
    source_labels,
    method = "ris"
  )

  # WOS search results are associated with NA values for column "databases".
  # This fixes that issue so that the deduplication keeps database source also
  citations <- mutate(citations,
    # perhaps refactor this part to process somewhat more
    database = if_else(str_detect(file_name, "wos+"), "wos", database),
    # phase = phase
  )

  citations
}


#' Load object into R environment or generate it and save to disk
#'
#' This is a helper function that supports an interactive, iterative, but also
#' reproducible workflow. Solves the problem that some R objects are
#' computationally intensive and is inefficient to regenerate in every work
#' session.
#'
#' @param load_path The file path of an object saved to disk using `qs::qsave`,
#'   or, if the saved object does not exist, the file path where that object
#'   should be saved to after generating it using function(s) called in `expr`
#' @param expr Function(s) for generating object
#'
#' @return Loads object into R environment
load_or_run <- function(load_path, expr) {

  # Extract an object name from the file path
  object_name <- str_extract(load_path, "[^/]*$")

  if (file.exists(load_path)) {
    # Load the object if file exists
    assign(object_name, qread(load_path), envir = globalenv())
  } else {
    # Evaluate the expression and assign the result if file does not exist
    result <- eval(substitute(expr))
    assign(object_name, result, envir = globalenv())

    # Save the result to the specified path
    qsave(
      result,
      load_path,
      preset = "archive"
    )
  }

}


#' Save object to disk as a delimited file if it doesn't already exist
#'
#' This is a helper function that supports an interactive, iterative, but also reproducible workflow. Provides safety by preventing the
#'
#' @param save_path
#' @param data
#'
#' @return
#' @export
#'
#' @examples
write_file <- function(save_path, data) {

  if (!file.exists(save_path)) {
    file_type <- str_extract(save_path, "(?<=\\.).+")

    switch(file_type,
           csv = write_csv(data, save_path),
           tsv = write_tsv(data, save_path),
           stop("Unsupported `file_type`")
    )

    cat("File written to:", save_path)
  }
  else {
    file_meta <- file.info(save_path)

    cat("File already written at:", save_path, "\n")
    cat("Last modified (ctime):"); print(file_meta$ctime)
  }

}


#' Title
#'
#' @param data
#' @param id_start_value
#' @param upload_name
#'
#' @return
format_abstrackr_to_mr <- function(
    ab_export_path,
    ab_results_path,
    id_start_value = 10000
) {

  eligible <- read_csv(ab_results_path) |> filter(`Aaron Wenger` == 1)

  n_eligible <- nrow(eligible)

  read_tsv(ab_export_path) |>
    semi_join(eligible, join_by(id == `(source) id`)
    ) |> mutate(
      pages = if_else(pages == "N.PAG", NA, pages),
      PDF_ID = NA,
      Study_ID = str_c(
        "S",
        id_start_value:(id_start_value + n_eligible - 1)
      ),
      Library_Catalog = NA,
      Editor = NA,
      Conference_Name = NA,
      isMainCitation = NA
    ) |> select(
      CIT_ID = id,
      PDF_ID,
      Study_ID,
      Item_Type = source_type,
      Publication_Year = year,
      Author_List = authors,
      Title = title,
      Publication_Title = journal,
      DOI = doi,
      Url = url,
      Abstract = abstract,
      Pages = pages,
      Issue = issue,
      Volume = volume,
      Publisher = publisher,
      Place = place_published,
      Library_Catalog,
      Editor,
      Conference_Name,
      isMainCitation
    )
}


#' Title
#'
#' @param conf_rating
#'
#' @return
recode_conf_scores <- function(conf_rating) {
  case_when(
    std_mth_dsg_conf == "Certain or almost certain" ~ 2,
    std_mth_dsg_conf == "More likely than not" ~ 1,
    std_mth_dsg_conf == "Guess" ~ 0
  )
}


#' Title
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
process_mr_study_results <- function(file_path) {

  read_csv(file_path) |>
    select(
      std_id = study_id,
      rec_id = citation_id,
      std_eligible = eligibility_status,
      std_smp_subject,
      std_smp_subject_author,
      std_smp_grade,
      std_smp_grade_author_unit,
      std_smp_grade_author_value,
      std_cnd_cm = std_cmlearn,
      std_cnd_cm_conf = std_cmlearn_conf,
      std_cnd_cm_n = std_cmlearn_n,
      std_cnd_noncm = std_comp,
      std_cnd_noncm_conf = std_comp_conf,
      std_cnd_noncm_n = std_comp_n,
      std_cnd_diff = std_compdiff,
      std_cnd_diff_conf = std_compdiff_conf,
      std_cnd_diff_author = std_compdiff_author,
      std_msr_cog = std_outcome,
      std_msr_cog_conf = std_outcome_conf,
      std_mth_dsg = std_qlt_design,
      std_mth_dsg_conf = std_qlt_design_conf,
      std_mth_stats = std_es_stats,
      std_mth_stats_conf = std_es_stats_conf
    ) |>
    mutate(
      std_eligible = if_else(std_eligible == "Eligible", 1, 0),
      std_smp_subject = case_when(
        std_smp_subject == "Yes, physics" ~ "physics",
        std_smp_subject == "Yes, chemistry" ~ "chemistry",
        std_smp_subject == "Yes, biology" ~ "biology",
        std_smp_subject == "Yes, other" ~ "other",
        std_smp_subject == "Yes, unsure how to categorize" ~ "unsure",
        std_smp_subject == "No, teaching/learning of a science subject is not reported" ~ "no",
      ),
      std_smp_grade = case_when(
        std_smp_grade == "Yes, primarily undergraduate / post-secondary students (grades 13-16, ages ~18-22)" ~ "undergraduate",
        std_smp_grade == "Yes, primarily high school / secondary students (grades 9-12, ages ~14-18)" ~ "high school",
        std_smp_grade == "Yes, primarily middle school students (grades 5-8, ages ~10-14)" ~ "middle school",
        std_smp_grade == "Yes, primarily elementary students (grades K-4, ages ~5-10)" ~ "elementary",
        std_smp_grade == "Yes, unsure how to categorize" ~ "unsure",
        std_smp_grade == "No, participants were not K-16 students (exclude)" ~ "no"
      ),
      std_cnd_cm = if_else(std_cnd_cm, 1, 0),
      # std_cnd_cm_conf = recode_conf_scores(std_cnd_cm_conf),
      std_cnd_noncm = case_when(
        std_cnd_noncm == "Yes, includes one or more instructional reform condition(s)" ~ "reform",
        std_cnd_noncm == "Yes, includes both types of condition" ~ "both",
        std_cnd_noncm == "Yes, includes one or more instructional BAU condition(s)" ~ "bau",
        std_cnd_noncm == "No (exclude)" ~ "no"
      ),
      std_cnd_diff = if_else(str_detect(std_cnd_diff, "Yes"), 1, 0),
      # std_cnd_diff_conf = recode_conf_scores(std_cnd_diff_conf),
      std_msr_cog = if_else(str_detect(std_msr_cog, "Yes"), 1, 0),
      # std_msr_cog_conf = recode_conf_scores(std_msr_cog_conf),
      std_mth_dsg = case_when(
        std_mth_dsg == "Yes, true experiment (students are randomized to groups)" ~ "experiment",
        std_mth_dsg == "Yes, QED pretest-posttest design" ~ "qed pre post",
        std_mth_dsg == "Yes, QED crossover group design" ~ "qed crossover",
        std_mth_dsg == "Yes, QED measure with baseline equivalency (no pretest)" ~ "qed baseline",
        std_mth_dsg == "Yes, individually-assigned counter-balanced/crossover" ~ "individual crossover",
        std_mth_dsg == "Yes, regression discontinuity design" ~ "rdd",
        std_mth_dsg == "Yes, matched subjects" ~ "matched",
        std_mth_dsg == "No, study design does not control for pre-existing differences (exclude)" ~ "no"
      ),
      # std_mth_dsg_conf = recode_conf_scores(std_mth_dsg_conf),
      std_mth_stats = if_else(
        std_mth_stats == "Yes" | std_mth_stats == "Possibly/Nonstandard", 1, 0
      ),
      # std_mth_stats_conf = recode_conf_scores(std_mth_stats_conf),
      std_tag = case_when(
        std_smp_subject == "no" ~ "Study, not science subject",
        std_smp_grade == "no" ~ "Study, not K-12 students",
        std_cnd_cm == 0 ~ "Study, no CM for learning",
        std_cnd_noncm == "no" ~ "Study, no non-CM comparison",
        std_cnd_diff == 0 ~ "Study, groups differ otherwise",
        std_msr_cog == 0 ~ "Study, no cognitive outcome",
        std_mth_dsg == "no" ~ "Study, no control for pre-existing",
        std_mth_stats == 0 ~ "Study, no stats for ES",
        .default = "Eligible"
      )
    ) |> left_join(
      y = read_csv(
        here("data", "meta_reviewer_results", "phase1_FTretrieval_results.csv")
        ) |> select(
          rec_id = citation_id,
          rec_cty = `Country affiliation`
        ),
      by = "rec_id"
      )

}


process_mr_record_results <- function(file_path, study_results) {

  study_results <- qread(here("data", "meta_reviewer_results", "phase1_FTstd_results"))
  # Retrieve the number of eligible studies per record, reduce tag(s) to one
  rec_std_eligible <- study_results |>
    group_by(rec_id) |>
    summarize(
      rec_std_n_eligible = sum(std_eligible),
      rec_std_tag = paste(std_tag, collapse = "; ")
    ) |>
    mutate(
      rec_std_tag = case_when(
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "subject") ~ "subject",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "K-12") ~ "grade",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "no CM") ~ "CM",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "non-CM") ~ "nonCM",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "differ") ~ "differ",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "cogn") ~ "cognitive",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "control") ~ "control",
        rec_std_n_eligible == 0 & str_detect(rec_std_tag, "stats") ~ "stats"
      )
    )

  # Rename, process MR record results
  rec_results <- read_csv(file_path) |>
    select(
      rec_id = citation_id,
      rec_author = authors,
      rec_title = citation_name,
      rec_yrpub = publication_year,
      rec_type = item_type,
      rec_duplicate = mark_as_duplicate,
      rec_retrieved = fulltext_status,
      rec_retrieval_query_eligible = `Author Query Eligibility`,
      rec_english = `Full-text is in English`,
      rec_cty = `Country affiliation`,
      rec_std_n = `Number of studies reported`,
    ) |>
    mutate(
      rec_retrieved = if_else(str_detect(rec_retrieved, "Not"), 0, 1),
      # rec_ft_english = if_else,
      rec_duplicate = if_else(!is.na(rec_duplicate), 1, 0),
      rec_ft_ret_tag = case_when(
        rec_retrieved == 0 ~ "FT not retrieved",
        rec_english == 0 ~ "FT not in english",
        rec_std_n == 0 ~ "FT reports no primary study",
        rec_duplicate == 1 ~ "FT is a duplicate",
        .default = "Eligible"
      )
    )

  # Join from study table, study eligibility count
  rec_results |> left_join(rec_std_eligible)
}


join_rec_results <- function(rec_deduped, rec_ab_screen, rec_ft_screen) {

  joined_results <- rec_deduped |>
    select(
      rec_id = duplicate_id,
      rec_all_id = record_ids,
      rec_author = author,
      rec_title = title,
      rec_yrpub = year,
      rec_type_bib1 = source_type,
      rec_type_bib2 = document_type,
      # perhaps refactor
      rec_method = 1,
      # perhaps refactor
      rec_source = database
    ) |>
    mutate(
      rec_id = as.numeric(rec_id),
      rec_yrpub = as.numeric(str_extract(rec_yrpub, "[:digit:]{4}"))
    ) |>
    full_join(rec_ab_screen) |>
    left_join(rec_ft_screen) |>
    mutate(
      rec_ab_eligible = if_else(rec_ab_eligible == 1, 1, 0)
      # rec_eligible = case_when(
      #   rec_ab_eligible == 1 ~ 1,
      #   rec_ft_ret_tag == "Eligible" ~ 1,
      #   rec_std_n_eligible > 0 ~ 1
      # )
    )

}
