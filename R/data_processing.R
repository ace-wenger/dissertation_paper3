# ===== Required Packages ======================================================
# library(tidyverse)
# library(metafor)
# library(naniar)
# library(here)

# ===== Pipeline Functions =====================================================
calc_missing_stats <- function(data, assumed_rpp) {
  data |> expand_missing_stats() |> assume_rpp(r = assumed_rpp)
}

calc_es <- function(data) {
  data |>
    mutate(zdat = 0) |>  # vector of zeros for escalc functions
    escalc( # reported effect size and t-test values
      measure = "SMD",
      var.names = c("d_yi", "d_vi"),
      n1i = nT,
      n2i = nC,
      di = d,
      ti = ind_t,
      data = _
    ) |>
    escalc( # posttest
      measure = "SMD",
      var.names = c("posttest_yi", "posttest_vi"),
      n1i = nT,
      n2i = nC,
      m1i = pst_mT,
      m2i = pst_mC,
      sd1i = pst_sdT,
      sd2i = pst_sdC,
      replace = FALSE,
      data = _
    ) |>
    escalc( # adjusted posttest
      measure = "SMD",
      var.names = c("adj_posttest_yi", "adj_posttest_vi"),
      n1i = nT,
      n2i = nC,
      m1i = adj_pst_mT,
      m2i = adj_pst_mC,
      sd1i = pst_sdT,
      sd2i = pst_sdC,
      replace = FALSE,
      data = _
    ) |>
    # the following is conditioned on change score SD not posttest SD
    # I would like to calculate both for my own benefit but use posttest SD for the meta-analysis
    escalc( # pre-, post-test, treatment group
      measure = "SMCC",
      ni   = nT,
      m1i  = chg_mT,
      m2i  = zdat,
      sd1i = chg_sdT,
      sd2i = zdat,
      ri   = zdat,
      var.names = c("ppc_yi_T", "ppc_vi_T"),
      replace = FALSE,
      data = _
    ) |>
    escalc( # pre-, post-test, control
      measure = "SMCC",
      ni   = nC,
      m1i  = chg_mC,
      m2i  = zdat,
      sd1i = chg_sdC,
      sd2i = zdat,
      ri   = zdat,
      var.names = c("ppc_yi_C", "ppc_vi_C"),
      replace = FALSE,
      data = _
    ) |>
    mutate( # PPC design, combining within group ES for study ES
      prepost_yi = ppc_yi_T - ppc_yi_C,
      prepost_vi = ppc_vi_T + ppc_vi_C,
      anova_yi = calc_yi_anova(f = anova_f, n1 = nT, n2 = nC),
      anova_vi = calc_vi_anova(n1 = nT, n2 = nC, d = anova_yi),
      ancova_yi = calc_yi_ancova(f = ancova_f, n1 = nT, n2 = nC, r = (rppT + rppC) / 2),
      ancova_vi = calc_vi_ancova(n1 = nT, n2 = nC, d = ancova_yi, r = (rppT + rppC) / 2)
    )

}

algorithm_es <- function(data) {
  # choose which effect size calculation to include in meta-analysis data
  data |> mutate(
    yi = case_when(
      !is.na(adj_posttest_yi) ~ adj_posttest_yi,
      !is.na(prepost_yi) & rppC != 0.6 ~ prepost_yi,
      !is.na(posttest_yi) ~ posttest_yi,
      !is.na(d_yi) ~ d_yi,
      !is.na(prepost_yi) & rppC == 0.6 ~ prepost_yi,
      !is.na(anova_yi) ~ anova_yi,
      !is.na(ancova_yi) ~ ancova_yi,
      .default = NA
    ),
    vi = case_when(
      !is.na(adj_posttest_vi) ~ adj_posttest_vi,
      !is.na(prepost_vi) & rppC != 0.6 ~ prepost_vi,
      !is.na(posttest_vi) ~ posttest_vi,
      !is.na(d_vi) ~ d_vi,
      !is.na(prepost_vi) & rppC == 0.6 ~ prepost_vi,
      !is.na(anova_vi) ~ anova_vi,
      !is.na(ancova_yi) ~ ancova_vi,
      .default = NA
    ),
    es_mth_escalc = case_when(
      !is.na(adj_posttest_yi) ~ "posttest_adj",
      !is.na(prepost_yi) & rppC != 0.6 ~ "prepost",
      !is.na(posttest_yi) ~ "posttest", # to reduce unecessary levels
      !is.na(d_yi) ~ "posttest",        # to reduce unecessary levels
      !is.na(prepost_yi) & rppC == 0.6 ~ "prepost_imp",
      !is.na(anova_yi) ~ "posttest",    # to reduce unecessary levels
      !is.na(ancova_yi) ~ "prepost_imp",# to reduce unecessary levels
      .default = NA
    )
  ) |>
  filter(
    !is.na(yi),
    !is.na(nT),
    !is.na(nC)
  ) |>
  # !!! Temporary !!! to keep the calculated vcv matrix positive definite
  subset(!(std_id %in% c("S10057", "S10097", "S10361", "S20008", "S10265"))) |>
  select(
    rec_id:observation,
    nT,
    nC,
    pst_mT,
    pst_sdT,
    pst_mC,
    pst_sdC,
    yi,
    vi,
    es_mth_escalc,
    es_general_notes = notes
  )

}

proc_review_coding <- function(coding_data, screening_data, es_data) {

  coding_dat <- coding_data |>
    filter(
      eligibility_status == "Eligible" &
        coding_status == "Complete" &
        std_smp_subsample...65 == TRUE
    ) |>
    select( # also serves as a key to variables present in the `review` table
      rec_id                   = citation_id,
      rec_title                = citation_name,
      rec_author               = authors,
      rec_yrpub                = publication_year,
      rec_cty_affil            = country_affiliation,
      rec_type                 = item_type,
      std_id                   = study_id,
      # [x] std_id,                 FROM `screening_data`
      # [x] std_smp_subject,
      # [x] std_smp_subject_author,
      # [x] std_smp_grade,
      # [x] std_smp_grade_author_unit,
      # [x] std_smp_grade_author_value,
      # [x] std_qlt_design = std_mth_dsg
      std_mth_smp_setting      = std_mth_smp_setting...66,
      std_mth_smp_breadth      = std_mth_smp_breadth...67,
      std_smp_year             = std_smp_year...68,
      std_smp_cty              = std_smp_cty...69,
      std_smp_cty_region       = std_smp_cty_region...70,
      std_smp_eth              = std_smp_eth...71,
      std_smp_eth_notes        = std_smp_eth_notes...72,
      std_smp_gender           = std_smp_gender...73,
      std_smp_ses              = std_smp_ses...74,
      std_smp_ses_notes        = std_smp_ses_notes...75,
      std_smp_author_note      = std_smp_author_note...76,
      std_mth_assign_unit:std_qlt_tests,
      # [x] std_qlt_int_confound,   PROC variables
      # [x] std_qlt_int_threats,
      # [x] std_qlt_int,
      # [x] std_smp_cty_weird,
      # [x] std_qlt_reporting
      es_id                    = effect_size_id,
      # [x] es_name,                PROC variable
      es_cnd_noncm_name        = condition_id...118,
      es_cnd_noncm_desc        = std_cnd_notes...123,
      es_cnd_noncm_desig       = std_cnd_noncm_desig...121,
      es_cnd_noncm_duration    = std_cnd_duration...124,
      es_cnd_noncm_intensity   = std_cnd_intensity...125,
      es_cnd_noncm_structure   = std_cnd_structure...126,
      es_cnd_noncm_mode        = std_cnd_mode...127,
      es_cnd_noncm_collab      = std_cnd_collab...128,
      es_cnd_noncm_feedback    = std_cnd_feedback...129,
      es_mth_noncm_instr       = std_mth_instr...137,
      es_qlt_noncm_instr_train = std_qlt_cnd_train...138,
      es_cnd_noncm_note        = std_cnd_note...139,
      es_cnd_cm_name           = condition_id...95,
      es_cnd_cm_desc           = std_cnd_notes...100,
      es_cnd_cm_duration       = std_cnd_duration...101,
      es_cnd_cm_intensity      = std_cnd_intensity...102,
      es_cnd_cm_structure      = std_cnd_structure...103,
      es_cnd_cm_mode           = std_cnd_mode...104,
      es_cnd_cm_collab         = std_cnd_collab...105,
      es_cnd_cm_feedback       = std_cnd_feedback...106,
      es_cnd_cm_train          = std_cnd_cm_train...107,
      es_cnd_cm_medium         = std_cnd_cm_medium...108,
      es_cnd_cm_interact       = std_cnd_cm_interact...109,
      es_cnd_cm_use            = std_cnd_cm_use...110,
      es_cnd_cm_use_scaf       = std_cnd_cm_use_scaf...111,
      es_cnd_cm_use_rev        = std_cnd_cm_use_rev...112,
      es_cnd_cm_type           = std_cnd_cm_type...113,
      es_mth_cm_instr          = std_mth_instr...114,
      es_qlt_cm_instr_train    = std_qlt_cnd_train...115,
      es_cnd_cm_note           = std_cnd_note...116,
      es_msr_name              = measure_id...141,
      es_mth_msr_source        = std_mth_msr_source...144,
      es_qlt_msr_validity      = std_qlt_msr_validity...145,
      es_qlt_msr_align         = std_qlt_msr_align...146,
      es_qlt_msr_rel           = std_qlt_msr_rel...148,
      es_qlt_msr_rel_x         = std_qlt_msr_rel_x...149,
      es_msr_rng_x             = std_msr_rng_x...150,
      es_msr_timing            = std_msr_timing...151,
      es_msr_timing_delay      = std_msr_timing_delay...152,
      es_msr_timing_mid        = std_msr_timing_mid...153,
      es_msr_note              = std_msr_note...154
      # [x] es_qlt_msr_reliability, PROC variables
      # [x] es_qlt_msr_retriction,
      # [x] es_qlt_measure,
      # [x] es_cnd_duration,
      # [ ] std_cnd_difference,
      # [x] grpT,                   FROM es_data
      # [x] grpC,
      # [x] occasion,
      # [x] time,
      # [x] observation,
      # [x] nT,
      # [x] nC,
      # [x] pst_mT,
      # [x] pst_sdT,
      # [x] pst_mC,
      # [x] pst_sdC,
      # [x] yi,
      # [x] vi,
      # [x] es_mth_escalc,
      # [x] es_general_note
    )

  screen_dat <- screening_data |>
    select(
      std_id,
      std_smp_subject,
      std_smp_subject_author,
      std_smp_grade,
      std_smp_grade_author_unit,
      std_smp_grade_author_value,
      std_qlt_design = std_mth_dsg
    )

  left_join(coding_dat, screen_dat, by = "std_id") |>
    inner_join(es_data) |>
    relocate(std_smp_subject:std_qlt_design, .before = std_mth_smp_setting) |>
    # mutate(
    #   rec_decpub = case_when(
    #     rec_yrpub >= 2020 ~ 6,
    #     rec_yrpub >= 2010 ~ 5,
    #     rec_yrpub >= 2000 ~ 4,
    #     rec_yrpub >= 1990 ~ 3,
    #     rec_yrpub >= 1980 ~ 2,
    #     rec_yrpub >= 1970 ~ 1,
    #     .default = NA
    #   ),
    #   .before = std_id
    # ) |>
    mutate(
      std_qlt_att_threat = if_else(
        std_qlt_att_threat == "NR",
        "No, NR",
        std_qlt_att_threat
      ),
      std_qlt_int_confound = algorithm_n1_confound(
        n_assign = std_qlt_assign_x,
        n_condition = std_qlt_cnd_x,
        instr_multi = std_qlt_cnd_ins_mult,
        n1_confound = std_qlt_cnf
      ),
      std_qlt_int_threats = algorithm_int_threats(
        cm_instr = es_mth_cm_instr,
        noncm_instr = es_mth_noncm_instr,
        imp = std_qlt_cnd_imp,
        event = std_qlt_cnd_event,
        contam = std_qlt_cnd_contam
      ),
      std_qlt_int = algorithm_internal(
        confounds = std_qlt_int_confound,
        threats = std_qlt_int_threats,
        attrition = std_qlt_att_threat,
        design = std_qlt_design
      ),
      # std_smp_cty_us = if_else(str_detect(std_smp_cty, "United States"), "US", "nonUS"),
      std_smp_cty_weird = if_else(
        !is.na(std_smp_cty) | std_smp_cty == "NR",
        case_when(
          str_detect(rec_cty_affil, "United States") ~ "weird_US",
          rec_cty_affil %in% cty_weird ~ "weird_other",
          rec_cty_affil %in% cty_non_weird ~ "non-weird",
          .default = rec_cty_affil
        ),
        case_when(
          str_detect(std_smp_cty, "United States") ~ "weird_US",
          std_smp_cty %in% cty_weird ~ "weird_other",
          std_smp_cty %in% cty_non_weird ~ "non-weird",
          .default = std_smp_cty
        ),
      ),
      ### currently not working
      std_qlt_reporting = algorithm_report(
        cnd = std_qlt_cnd_report,
        smp = std_qlt_smp_report,
        msr_f = std_qlt_msr_report_full,
        msr_r = std_qlt_msr_report
      ),
      .before = es_id
    ) |>
    mutate(
      es_name = paste(es_cnd_cm_name, es_cnd_noncm_name, sep = "-"),
      .before = es_cnd_noncm_name
    ) |>
    mutate(
      es_cnd_noncm_desig = if_else(
        str_detect(es_cnd_noncm_desig, "Reform"),
        "Reform",
        "BAU"
      ),
      .before = es_cnd_noncm_duration
    ) |>
    mutate(
      es_cnd_noncm_mode = sapply(es_cnd_noncm_mode, function(x) cnd_modes[[x]]),
      .before = es_cnd_noncm_collab
    ) |>
    mutate(
      es_cnd_cm_mode = sapply(es_cnd_cm_mode, function(x) cnd_modes[[x]]),
      .before = es_cnd_noncm_collab
    ) |>
    mutate(
      es_cnd_cm_med_int = case_when(
        str_detect(es_cnd_cm_medium, "paper") ~ "static_physical",
        str_detect(es_cnd_cm_interact, "Static") ~ "static_digital",
        str_detect(es_cnd_cm_interact, "Dynamic") ~ "dynamic_digital",
        str_detect(es_cnd_cm_interact, "NR") | str_detect(es_cnd_cm_medium, "NR") ~ NA,
        is.na(es_cnd_cm_interact) | is.na(es_cnd_cm_medium) ~ NA
      ),
      es_qlt_msr_reliability = algorithm_msr_reliability(
        reliability = es_qlt_msr_rel,
        rel_value = es_qlt_msr_rel_x
      ),
      es_qlt_msr_restriction = algorithm_msr_restriction(
        msr_range = es_msr_rng_x,
        int_mean = pst_mT,
        int_sd = pst_sdT,
        comp_mean = pst_mC,
        comp_sd = pst_sdC
      ),
      es_qlt_measure = algorithm_measure(
        restriction = es_qlt_msr_restriction,
        reliability = es_qlt_msr_reliability,
        validity = es_qlt_msr_validity,
        alignment = es_qlt_msr_align
      ),
      es_cnd_duration = algorithm_duration(
        duration = es_cnd_cm_duration,
        measure_time = es_msr_timing,
        mid_time = es_msr_timing_mid
      ),
      # es_cnd_difference = ,
      .before = grpT
    ) |> select(!pst_mT:pst_sdC)

}

proc_meta_coding <- function(data, threshold_miss) {

  data_es_meta_all <- data |>
    select(
      rec_id,
      rec_author,
      std_id,
      es_id,
      grpT,
      grpC,
      occasion,
      time,
      observation,
      nT,
      nC,
      yi,
      vi,
      rec_yrpub,
      rec_type,
      std_smp_cty_weird,
      es_mth_escalc,
      std_mth_smp_setting,
      std_mth_smp_breadth,
      std_mth_assign_unit,
      # std_qlt_assign_equate,
      es_mth_msr_source,
      es_cnd_noncm_desig,
      std_qlt_int,
      es_qlt_measure,
      # es_cnd_difference,
      std_smp_subject,
      std_smp_grade,
      # std_smp_gender,
      # std_smp_ses,
      es_cnd_cm_collab,
      es_cnd_cm_train,
      es_cnd_cm_med_int,
      es_cnd_cm_use,
      es_cnd_cm_type,
      es_cnd_duration,
      es_msr_timing
    ) |>
    mutate(
      rec_yrpub = rec_yrpub - min(rec_yrpub),
      rec_type = as_factor(
        case_when(
          rec_type == "Journal Article" ~ "article",
          rec_type == "Dissertation/Thesis" ~ "thesis",
          rec_type == "Conference Paper" ~ "paper",
          rec_type == "Preprint" ~ "paper",
          rec_type == "other/unsure" ~ "paper",
          TRUE ~ rec_type
        )
      ),
      # var = sapply(var, function(x) look_up[[x]]),
      std_smp_cty_weird = as_factor(std_smp_cty_weird),
      es_mth_escalc = as_factor(es_mth_escalc),
      std_mth_smp_setting = as_factor(
        case_when(
          str_detect(std_mth_smp_setting, "Class") ~ "Class",
          str_detect(std_mth_smp_setting, "Lab") ~ "Lab",
          str_detect(std_mth_smp_setting, "unsure") ~ "Other",
          std_mth_smp_setting == "NR" ~ NA,
          is.na(std_mth_smp_setting) ~ NA,
          TRUE ~ std_mth_smp_setting
        )
      ),
      # var = sapply(var, function(x) look_up[[x]]),
      std_mth_smp_breadth = as_factor(
        case_when(
          std_mth_smp_breadth == "1 classroom/section" ~ "class_singular",
          std_mth_smp_breadth == "2+ classrooms/sections" ~ "class_plural",
          std_mth_smp_breadth == "1 school (all relevant classrooms)" ~ "school_singular",
          std_mth_smp_breadth == "2+ schools" ~ "school_plural",
          std_mth_smp_breadth == "Other, unsure how to categorize" ~ "other",
          std_mth_smp_breadth == "NR" ~ NA,
          is.na(std_mth_smp_breadth) ~ NA,
          TRUE ~ std_mth_smp_breadth
        )
      ),
      # var = sapply(var, function(x) look_up[[x]]),
      std_mth_assign_unit = as_factor(
        case_when(
          std_mth_assign_unit == "Individual" ~ "individual",
          std_mth_assign_unit == "Pairs/groups" ~ "other",
          std_mth_assign_unit == "Classes/teachers" ~ "class_teacher",
          std_mth_assign_unit == "Schools" ~ "other",
          std_mth_assign_unit == "NR" ~ NA,
          is.na(std_mth_assign_unit) ~ NA,
          TRUE ~ std_mth_assign_unit
        )
      ),
      # std_qlt_assign_equate = as_factor(std_qlt_assign_equate),
      es_mth_msr_source = as_factor(
        case_when(
          es_mth_msr_source == "Researcher-derived" ~ "researcher",
          es_mth_msr_source == "Teacher/curriculum-derived" ~ "teacher",
          es_mth_msr_source == "Published-academic" ~ "published",
          es_mth_msr_source == "Published-commercial" ~ "published",
          es_mth_msr_source == "Standardized" ~ "other",
          es_mth_msr_source == "Other/unsure" ~ "other",
          es_mth_msr_source == "NR" ~ NA,
          is.na(es_mth_msr_source) ~ NA,
          TRUE ~ es_mth_msr_source
        )
      ),
      # var = sapply(var, function(x) look_up[[x]]),
      std_smp_subject = as_factor(std_smp_subject),
      std_smp_grade = as_factor(std_smp_grade),
      es_cnd_cm_collab = as_factor(
        case_when(
          es_cnd_cm_collab == "Individual activity, no peer interaction" ~ "individual",
          es_cnd_cm_collab == "Individual activity, with peer interaction" ~ "individual",
          es_cnd_cm_collab == "Individual activity, other" ~ "individual",
          es_cnd_cm_collab == "Mixed activity, individual and group activity" ~ "mixed",
          es_cnd_cm_collab == "Mixed activity, other" ~ "mixed",
          es_cnd_cm_collab == "Group activity, individual assessment/feedback" ~ "group",
          es_cnd_cm_collab == "Group activity, group assessment/feedback" ~ "group",
          es_cnd_cm_collab == "Group activity, other" ~ "group",
          es_cnd_cm_collab == "Whole-class activity" ~ "class_other",
          es_cnd_cm_collab == "Other" ~ "class_other",
          es_cnd_cm_collab == "NR" ~ NA,
          is.na(es_mth_msr_source) ~ NA,
          TRUE ~ es_cnd_cm_collab
        )
      ),
      es_cnd_cm_train = as_factor(
        case_when(
          es_cnd_cm_train == "No" ~ "no",
          es_cnd_cm_train == "Minimal (1 session, <60 minutes)" ~ "minimal",
          es_cnd_cm_train == "Yes (2+ sessions, 60+ minutes)" ~ "yes",
          es_cnd_cm_train == "NR" ~ "no",
          is.na(es_mth_msr_source) ~ NA,
          TRUE ~ es_mth_msr_source
        )
      ),
      es_cnd_cm_use = as_factor(
        case_when(
          es_cnd_cm_use == "Not constructed by students" ~ "studied",
          es_cnd_cm_use == "Constructed by students" ~ "constructed",
          es_mth_msr_source == "NR" ~ NA,
          is.na(es_mth_msr_source) ~ NA,
          TRUE ~ es_mth_msr_source
        )
      ),
      es_cnd_cm_type = as_factor(
        case_when(
          es_cnd_cm_type == "Associative (no labels)" ~ "associative",
          es_cnd_cm_type == "Unstructured (labeled links)" ~ "unstructed",
          es_cnd_cm_type == "Novakian (hierarchy, labeled link)" ~ "novakian",
          es_cnd_cm_type == "NR" ~ NA,
          is.na(es_cnd_cm_type) ~ NA,
          TRUE ~ es_cnd_cm_type
        ),
      ),
      # var = sapply(var, function(x) look_up[[x]]),
      es_msr_timing = as_factor(es_msr_timing)
    )

  tab_miss_var <- mis_ma_var_summary(data_es_meta_all, "vi", truncate = TRUE)

  data_es_meta <- data_es_meta_all |>
    select(!filter(tab_miss_var, pct_miss > threshold_miss)$Variable)

  return(data_es_meta)
}

calc_matrix <- function(data, between_time_r, between_obs_occ_r) {

  vcalc(
    data = data,
    vi = vi,
    cluster = rec_id,
    subgroup = std_id, # don't know if the subgroup is necessary, may only
    # need to specify std_id as cluster variable
    rho = between_obs_occ_r, #obs and type both use rho, give a vector (0.8, 0.4)
    # same sample comparison, occasion, time, different measures, r = 0.8
    obs = observation,
    # same sample comparison, occasion, different time, same measure, r = 0.6
    time1 = time,
    phi = between_time_r,
    # same sample comparison, different occasion (and time, and measure), r = 0.4
    type = occasion,
    # different comparison pair
    grp1 = grpT,
    grp2 = grpC,
    w1 = nT,
    w2 = nC,
    checkpd = TRUE
  )

}

# This function accounts for four sources of dependency as I have used it
# (read == as "same" and <> as "different"):
# 1. measures    == study, == sample pair, == occasion, == time, <> measure (obs)
# 2. timepoints  == study, == sample pair, == occasion, <> time  == measure (time1)
# 3. occasion    == study, == sample pair, <> occasion  <> time  <> measure (type)
# 4. sample pair == study, <> sample pair (grp1/grp2 w1/w2)

# (4) Many studies have multiple intervention or comparison groups that yield
# multiple relevant comparisons to the meta-analysis. The errors of these
# comparisons are correlated by the sample size of the groups. (3) Sometimes
# studies have multiple rounds of intervention and measurement in which each
# round includes the same groups but are somewhat independent. For example, an
# instructional intervention applied to a genetics unit with immediate genetics
# posttest, and applied again to an ecology unit with immediate ecology
# posttest. (2) In repeated measures designs, the outcome is measured in the
# same group for the same round of intervention at two or more timepoints. (1)
# Lastly, Studies often report summary statistics for multiple measures for the
# same construct of interest collected at the same in the same intervention.

# ===== Helper `calc_missing_stats` and `calc_es` Functions ====================
# calculating missing mean differences, sdD, and r
expand_missing_stats <- function(data) {
  mutate(data,
         chg_mT  = if_else(
           is.na(chg_mT) & !is.na(pst_mT) & !is.na(pre_mT),
           pst_mT - pre_mT,
           chg_mT
         ),
         chg_mC  = if_else(
           is.na(chg_mC) & !is.na(pst_mC) & !is.na(pre_mC),
           pst_mC - pre_mC,
           chg_mC
         ),
         chg_sdT = if_else(
           is.na(pairtT),
           chg_sdT,
           calc_chg_sd_t(mc = chg_mT, pairt = pairtT, n1 = nT)
         ),
         chg_sdC = if_else(
           is.na(pairtC),
           chg_sdC,
           calc_chg_sd_t(mc = chg_mC, pairt = pairtC, n1 = nC)
         ),
         rppT = if_else(
           is.na(rppT),
           calc_rpp(sd1 = pre_sdT, sd2 = pst_sdT, sdD = chg_sdT),
           rppT
         ),
         rppC = if_else(
           is.na(rppC),
           calc_rpp(sd1 = pre_sdC, sd2 = pst_sdC, sdD = chg_sdC),
           rppC
         ),
  )
}

# calculate sdD from mean difference and paired t test value - Morris and
# Deshon (2002)
calc_chg_sd_t <- function(data, mc, pairt, n1) {
  sqrt({{ n1 }} * ({{ mc }})^2) / {{ pairt }}
}

# calculate r from pretest, posttest, and change score standard deviations -
# Morris and Deshon (2002)
calc_rpp <-  function(data, sd1, sd2, sdD) {
  abs(({{ sd1 }}^2 + {{ sd2 }}^2 - {{ sdD }}^2) / (2 * {{ sd1 }} * {{ sd2 }}))
}

# calculate pooled standard deviation (sdP) - Borenstein et al. (2009)
calc_sdP <- function (data, sd1, sd2 = sd1, n1, n2 = n1) {
  sqrt(((({{ n1 }} - 1) * {{ sd1 }}^2) + (({{ n2 }}-1) * {{ sd2 }}^2))
       / ({{ n1 }} + {{ n2 }} - 2))
}

# assume missing r values, estimate missing sdD values, calculate sdP
assume_rpp <- function(data, r){
  data |>
    replace_na(replace = list(rppT = r, rppC = r)) |>
    mutate(
      chg_sdT = case_when(
        !is.na(chg_sdT) ~ chg_sdT,
        !is.na(pre_sdT & pst_sdT) ~ calc_chg_sd_r(
          sd1 = pre_sdT, sd2 = pst_sdT, n1 = nT, r = rppT
        ),
        TRUE ~ calc_chg_sd_pr(sd1 = pst_sdT, r = rppT)
      ),
      chg_sdC = case_when(
        !is.na(chg_sdC) ~ chg_sdC,
        !is.na(pre_sdC & pst_sdC) ~ calc_chg_sd_r(
          sd1 = pre_sdC, sd2 = pst_sdC, n1 = nC, r = rppC
        ),
        TRUE ~ calc_chg_sd_pr(sd1 = pst_sdC, r = rppC)
      )
    )
}

# calculate sdD (change score standard deviation) from sdP (pooled pre/post
# standard deviation or only post if pre is not given) and r (pre/post
# correlation) - Morris and Deshon (2002)
calc_chg_sd_r <- function (data, sd1, sd2 = sd1, n1, n2 = n1, r) {
  calc_sdP(data, sd1, sd2, n1, n2) * sqrt(2 * (1 - {{ r }}))
}

# calculate sdD from posttest standard deviation and r - Morris and Deshon (2002)
calc_chg_sd_pr <- function(sd1, r) {
  {{ sd1 }} * sqrt(2 * (1 - {{ r }}))
}

# Calculate effect size and variance from one-way ANOVA - Cooper et al. (2019)
calc_yi_anova <- function(data, f, n1, n2) {
  sqrt((f * (n1 + n2)) / (n1 * n2))
}

calc_vi_anova <- function(data, n1, n2, d) {
  ((n1 + n2) / (n1 * n2)) + ({{ d }}^2 / (2 *(n1 + n2)))
}

# Calculate effect size and variance from one-way ANCOVA - Cooper et al. (2019)
calc_yi_ancova <- function(data, f, n1, n2, r) {
  sqrt((2 * {{ f }} * (1 - r)) / (n1 + n2))
}

calc_vi_ancova <- function(data, n1, n2, d, r) {
  ((1 / n1 + n2) + ({{ d }}^2 / (2* (n1 + n2)))) * 2 * (1 - r)
}

# ===== Constants for `mr_coding` ==============================================

cty_weird <- c("Austrailia", "Australia", "Belgium", "Canada", "Finland", "Germany", "Ireland", "Netherlands", "The Netherlands", "New Zealand", "Norway", "Spain", "Sweden", "United Kingdom", "United States")

cty_non_weird <- c("Algeria", "Bolivia", "Chile", "China", "Colombia", "Ethiopia", "Gaza", "Ghana", "Greece", "India", "Indonesia", "Iran", "Jamaica", "Japan", "Jordan", "Lebanon", "Lithuania", "Malaysia", "Mexico", "Nigeria", "Lagos", "Oman", "Pakistan", "Peru", "Poland", "Romania", "Rwanda", "Saudi Arabia", "Serbia", "Singapore", "Slovenia", "South Africa", "South Korea", "Taiwan", "Thailand", "Turkey", "United Arab Emirates", "Zambia")

cnd_modes <- list(
  "Item 1" = "Physical, classroom/lab",
  "Item 2" = "Physical, other/unsure",
  "Item 3" = "Digital, online/distance learning",
  "Item 4" = "Digital, technology-based learning",
  "Item 5" = "Digital, other/unsure",
  "Item 6" = "Hybrid, flipped classroom",
  "Item 7" = "Hybrid, other/unsure",
  "Item 8" = "NR")

cnd_cm_rev_types <- list(
  "Item 1" = "Review (after)",
  "Item 2" = "Advance organizer (before)",
  "Item 3" = "Computer interface/organizer (during)",
  "Item 4" = "Other (during)",
  "Item 5" = "NR"
)

# ===== Helper `proc_review_coding` Functions ==================================
# ALGORITHM: Does the study report sufficient detail for replication?
algorithm_report <- function(cnd, smp, msr_f, msr_r) {

  cnd <- recode_likely(cnd)
  smp <- recode_likely(smp)
  msr_r <- recode_likely(msr_r)

  result <- mapply(function(cnd, smp, msr_f, msr_r) {
    if (any(is.na(c(cnd, smp, msr_f, msr_r)))) {
      return(NA)
    } else if (cnd == 2 & smp == 2 & (msr_f == "Yes" | msr_r == 2)) {
      # "Yes" if yes for all
      return(3)
    } else if (cnd >= 1 & smp >= 1 & (msr_f == "Yes" | msr_r >= 1)) {
      # "Maybe yes" if some or better for all
      return(2)
    } else if (check_reporting_maybe_no(cnd, smp, msr_r)) {
      # "Maybe no" if no in only one
      return(1)
    } else if (check_reporting_no(cnd, smp, msr_r)) {
      # "No" if no in two or more
      return(0)
    } else {
      return("error")
    }
  }, cnd, smp, msr_f, msr_r)

  return(result)
}

recode_likely <- function(vector) {
  case_match(
    vector,
    "Yes (very likely)" ~ 2,
    "Some (more likely than not)" ~ 1,
    "Yes (more likely than not)" ~ 1,
    "No (probably not)" ~ 0
  )
}

check_reporting_maybe_no <- function(cnd, smp, msr_r) {
  msr_r <- if_else(is.na(msr_r), 2, msr_r)
  sum(c(cnd == 0, smp == 0, msr_r == 0)) == 1
}

check_reporting_no <- function(cnd, smp, msr_r) {
  msr_r <- if_else(is.na(msr_r), 2, msr_r)
  sum(c(cnd == 0, smp == 0, msr_r == 0)) > 1
}

# ALGORITHM: Is the study free of N = 1 confounders?
algorithm_n1_confound <- function(
    n_assign,
    n_condition,
    instr_multi,
    n1_confound) {

  # Is there one unit per condition?
  n1 <- (as.integer(n_assign) - n_condition) == 0
  # Are there other n = 1 threats?
  n1_evd <- case_when(
    instr_multi == "Yes" | n1_confound == "Yes" ~ "No",
    instr_multi == "NR" ~ "Maybe",
    instr_multi == "No" ~ "Yes"
  )

  case_when(
    n1 == FALSE & n1_evd == "No" ~ 3,
    n1 == FALSE & n1_evd == "Maybe" ~ 2,
    n1 == TRUE & n1_evd == "Yes" ~ 0,
    .default = 1
  )
}

# ALGORITHM: Is the study free of other threats?
algorithm_int_threats <- function(cm_instr, noncm_instr, imp, event, contam) {

  cntr_instr <- if_else(
    str_detect(cm_instr, "searcher|Technology") &
      str_detect(noncm_instr, "searcher|Technology"),
    TRUE,
    FALSE
  )

  imp_comb <-  if_else(cntr_instr | str_detect(imp, "Yes"), TRUE, FALSE)

  event <- recode_likely(event)
  contam <- recode_likely(contam)

  result <- mapply(function(imp_comb, event, contam) {
    if (imp_comb & event == 0 & contam == 0) {
      return(3)
    } else if (!imp_comb & event == 0 & contam == 0) {
      return(2)
    } else if (imp_comb & (event >= 1 | contam >= 1)) {
      return(1)
    } else if (!imp_comb & (event >= 1 | contam >= 1)) {
      return(0)
    } else {
      return(NA)
    }
  }, imp_comb, event, contam)

  return(result)
}

# ALGORITHM: Do the study results support a causal inference?
algorithm_internal <- function(design, attrition, confounds, threats){

  design <- transform_design(design)

  results <- mapply(function(design, attrition, confounds, threats) {
    if (is.na(design) | is.na(attrition) | is.na(confounds) | is.na(threats)) {
      return(NA)
    } else if (design == "exp" &
               str_detect(attrition, "No") &
               confounds >= 2 &
               threats >= 2) {
      return(3)
    } else if (design == "exp" &
               xor(!str_detect(attrition, "No"),
                   xor(confounds < 2, threats < 2))) {
      return(2)
    } else if (design == "qed" &
               str_detect(attrition, "No") &
               confounds >= 2 &
               threats >= 2) {
      return(2)
    } else if (design == "exp" &
               !str_detect(attrition, "No") &
               xor(confounds < 2, threats < 2)) {
      return(1)
    } else if (design == "qed" &
               xor(!str_detect(attrition, "No"),
                   (confounds < 2 | threats < 2))) {
      return(1)
    } else if (design == "exp" &
               !str_detect(attrition, "No") &
               confounds < 2 &
               threats < 2) {
      return(0)
    } else if (design == "qed" &
               !str_detect(attrition, "No") &
               (confounds < 2 | threats < 2)) {
      return(0)
    } else {
      return("error")
    }
  }, design, attrition, confounds, threats)

  return(results)
}

transform_design <- function(design) {
  results <- sapply(design, function(design) {
    if (is.na(design)) {
      return(NA)
    } else if (str_detect(design, "experiment|crossover")) {
      return("exp")
    } else if (str_detect(design, "qed")) {
      return("qed")
    } else {
      return(design)
    }
  })
  return(results)
}

# ALGORITHM: Is the outcome measure reliable?
algorithm_msr_reliability <- function(reliability, rel_value) {

  results <- mapply(function(reliability, rel_value) {
    if (reliability == "NR" | is.na(reliability)) {
      return("unknown")
    } else if (reliability == "Other") {
      return("unknown")
    } else if (reliability == "no") {
      return("no")
    } else if (rel_value == "NR" | is.na(rel_value)) {
      return("unknown")
    } else if (reliability == "Yes (internal consistency)") {
      if (rel_value < 0.6) {return("no")}
      else if (rel_value < 0.75) {return("possibly")}
      else {return("yes")}
    } else if (reliability == "Yes (test-retest reliability)") {
      if (rel_value < 0.4) {return("no")}
      else if (rel_value < 0.6) {return("possibly")}
      else {return("yes")}
    } else if (reliability == "Yes (inter-rater agreement)") {
      if (rel_value < 0.8) {return("no")}
      else if (rel_value < 0.9) {return("possibly")}
      else {return("yes")}
    } else if (reliability == "Yes (inter-rater reliability)") {
      if (rel_value < 0.5) {return("no")}
      else if (rel_value < 0.7) {return("possibly")}
      else {return("yes")}
    } else {return("error")}
  }, reliability, rel_value)

  return(results)

}

# ALGORITHM: Is the outcome measure free of bias from range restriction?
algorithm_msr_restriction <- function(
    msr_range,
    int_mean,
    int_sd,
    comp_mean,
    comp_sd) {

  results <- mapply(function(msr_range, int_mean, int_sd, comp_mean, comp_sd) {
    if (msr_range == "NR" |
        any(is.na(c(msr_range, int_mean, int_sd, comp_mean, comp_sd)))) {
      return("unknown")
    } else {

      range_x <- str_split_1(msr_range, ";")

      if (length(range_x) != 2) {
        return("unknown")
      } else {
        range_min <- str_trim(range_x[[1]])
        range_max <- str_trim(range_x[[2]])
      }

      if (
        (int_mean + int_sd > range_max) |
        (comp_mean + comp_sd > range_max) |
        (int_mean - int_sd < range_min) |
        (comp_mean - comp_sd < range_min)
      ) {
        return("no")
      } else if (
        (int_mean + 1.5 * int_sd > range_max) |
        (comp_mean + 1.5 * comp_sd > range_max) |
        (int_mean - 1.5 * int_sd < range_min) |
        (comp_mean - 1.5 * comp_sd < range_min)
      ) {
        return("possibly")
      } else return("yes")

    }
  }, msr_range, int_mean, int_sd, comp_mean, comp_sd)

  return(results)
}

# ALGORITHM: Does the outcome measure provide a good estimate of effect?
algorithm_measure <- function(restriction, reliability, validity, alignment) {

  validity <- case_when(
    validity == "Yes" ~ "yes",
    validity == "Unsure" ~ "yes",
    validity == "No" ~ "no",
    validity == "NR" ~ "no",
    is.na(validity) ~ NA
  )

  results <- mapply(function(restriction, reliability, validity, alignment) {
    if (is.na(restriction) |
        is.na(reliability) |
        is.na(validity) |
        is.na(alignment)
    ) {
      return(NA)
    } else if (restriction == "yes" &
               reliability == "yes" &
               validity == "yes" &
               alignment == "No"
    ) {
      return(3)
      # only one weak negative indicator
    } else if ((restriction == "possibly" | restriction == "unknown") &
               reliability == "yes" &
               validity == "yes" &
               alignment == "No"
    ) {
      return(2)
    } else if (restriction == "yes" &
               (reliability == "possibly" | reliability == "unknown") &
               validity == "yes" &
               alignment == "No"
    ) {
      return(2)
    } else if (restriction == "yes" &
               reliability == "yes" &
               xor(validity == "yes", alignment == "No")
    ) {
      return(2)
      # only one strong negative indicator
    } else if (restriction == "yes" &
               reliability == "no" &
               validity == "yes" &
               alignment == "No"
    ) {
      return(1)
    } else if (restriction == "no" &
               reliability == "yes" &
               validity == "yes" &
               alignment == "No"
    ) {
      return(1)
      # two weak negative indicators
    } else if (restriction == "yes" &
               reliability == "yes" &
               validity == "no" &
               alignment == "Possibly"
    ) {
      return(1)
    } else if (restriction == "yes" &
               (reliability == "possibly" | reliability == "unknown") &
               xor(validity == "yes", alignment == "No")
    ) {
      return(1)
    } else if ((restriction == "possibly" | restriction == "unknown") &
               reliability == "yes" &
               xor(validity == "yes", alignment == "No")
    ) {
      return(1)
    } else if ((restriction == "possibly" | restriction == "unknown") &
               (reliability == "possibly" | reliability == "unknown") &
               (validity == "yes" |
                alignment == "No")
    ) {
      return(1)
      # one strong and one weak indicator
    } else if (xor(restriction == "no",
                   reliability == "no") &
               xor(validity == "no", alignment == "Possibly")
    ) {
      return(1)
      # two weak and one or more strong indicators
    } else if ((restriction == "no" |
                reliability == "no") &
               (validity == "yes" | validity == "no") &
               (alignment == "No" | alignment == "Possibly")
    ) {
      return(0)
      # three or more weak indicators
    } else if ((restriction == "possibly" | restriction == "unknown") &
               (reliability == "possibly" | reliability == "unknown") &
               (validity == "yes" | validity == "no") &
               (alignment == "No" | alignment == "Possibly")
    ) {
      return(0)
    } else {
      return("error")
    }
  }, restriction, reliability, validity, alignment)

  return(results)
}

# ALGORITHM: Duration of condition adjusted for "midtest" timing
algorithm_duration <- function(duration, measure_time, mid_time) {

  results <- mapply(function(duration, measure_time, mid_time) {
    if (is.na(measure_time)) {
      return(duration)
    } else if (measure_time == "mid") {
      return(mid_time)
    } else {
      return(duration)
    }
  }, duration, measure_time, mid_time)

  return(results)
}

# ===== Helper `proc_meta_coding` functions ====================================
# From Jacob et al. (2022) tutorial code
###--------------------------------------------------------------------------###
# Function to compute missing data summaries by variable
#' @name mis_ma_var_summary
#' @param data: data set (tibble)
#' @param se_col: string indicating the standard error of the effect size column name
#' @param truncate: boolean; If TRUE, omits any variables with no missingness
#' @return forest plot of effect sizes colored by whether the covariate is missing
###-------------------------------------------------------------------------###
mis_ma_var_summary <- function(data, se_col, truncate = TRUE){

  # Quick compute functions:
  # no. of cases
  n_miss_fun <- function(x){
    return(sum(is.na(x)))
  }

  # pct of cases
  pct_miss_fun <- function(x){
    return(mean(is.na(x)) * 100)
  }

  # weighted pct of cases
  wt_miss_fun <- function(x, wt){
    return(sum(as.integer(is.na(x)) * wt)/sum(wt) * 100)
  }

  # pull standard erros
  se_vals <- data %>%
    select(all_of(se_col)) %>%
    as_vector()

  # build summar table
  ntab <- data %>%
    summarize_all(n_miss_fun)
  pctab <- data %>%
    summarize_all(pct_miss_fun)

  sums <- bind_rows(ntab, pctab)

  if(!is.null(se_col)){

    wpctab <- data %>%
      summarize_all(.funs = function(x) wt_miss_fun(x, (1/se_vals)^2))

    sums <- bind_rows(sums, wpctab)
    row.names(sums) <- c("n_miss", "pct_miss", "wtpct_miss")

  } else {

    row.names(sums) <- c("n_miss", "pct_miss")

  }

  sums_tab <- as_tibble(cbind(Variable = names(sums), t(sums))) %>%
    mutate_at(grep("miss", names(.), value = T), as.numeric) %>%
    arrange(desc(wtpct_miss))

  if(truncate){ sums_tab <- filter(sums_tab, n_miss > 0) }

  return(sums_tab)
}
