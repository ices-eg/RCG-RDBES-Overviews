#prepare subset of data
cs |>
  select(DEhierarchy, SDctry, SDid) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> SDsummary

summarizedRow <- SDsummary |>
  ungroup() |>
  summarise(`Country/Hierarchy` = "Total",
            across(-`Country/Hierarchy`, sum, na.rm = TRUE))

SDsummary <- bind_rows(SDsummary, summarizedRow)

SDsummary <- SDsummary |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))

cs |>
  select(DEhierarchy, SDctry, SDid,DEsampSchemeType) |>
  filter(DEsampSchemeType %in% c('RegPilCF','RegPilIB','RegRouCF')) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> SDsummaryType

summarizedRow <- SDsummaryType |>
  ungroup() |>
  summarise(`Country/Hierarchy` = "Total",
            across(-`Country/Hierarchy`, sum, na.rm = TRUE))

SDsummaryType <- bind_rows(SDsummaryType, summarizedRow)

SDsummaryType <- SDsummaryType |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))

# merge tables with prefix. I want numbers of regional routine in brackets
combined <- SDsummary |>
  left_join(SDsummaryType, by = "Country/Hierarchy", suffix = c("", ".y"))

combined <- combined |>
  mutate(across(ends_with(".y"), ~ paste0("(", replace_na(as.character(.), "0"), ")")))

# merge cells from both tables
for(col in colnames(SDsummary)[-1]) {
  combined[[col]] <- paste0(combined[[col]], combined[[paste0(col, ".y")]])
}

# delete columns with ".y"
combined <- combined |>
  select(-ends_with(".y"))|>
  mutate(across(everything(), ~ gsub("\\(NA\\)", "", .)),
         across(everything(), ~ gsub("\\(0\\)", "", .)),
         across(everything(), ~ gsub("\\(\\)", "", .)))