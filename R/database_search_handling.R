citation_files <- list.files(
  path = "data/database_search",
  pattern = "\\.ris",
  full.names = TRUE
)

source_labels <- str_extract(citation_files, "(?<=\\/)[^\\/]*?(?=\\.ris)")

citations <- read_citations(
  citation_files,
  cite_sources = source_labels,
  cite_labels = "search",
  tag_naming = "best_guess"
)

export_ris(citations = citations)
write_csv(citations, file = "citations.csv")

citations |>
  mutate(across(everything(), is.na)) |>
  summarize(across(everything(), sum))

# deduplication
unique_citations <- CiteSource::dedup_citations(citations, manual = TRUE)

export_csv(unique_citations[["flag_dedup"]], filename = "flag_dedup.csv")

export_csv(unique_citations[["unique"]], filename = "unique.csv")

reimport_csv(filename = "unique_manual.csv")

# final export
export_ris(unique_citations, filename = "unique_citations.ris", source_field = "DB", label_field = "N1")
export_csv(unique_citations, filename = "unique-by-source.csv", separate = "cite_source")

unique_citations_clean[["unique"]] |>
  rename(
    id = duplicate_id,
    authors = author
  ) |>
  write_tsv("unique_citations_x.tsv")

This houses the work for my second dissertation paper which consists of a second, larger meta-analysis of concept mapping in all science educatio
