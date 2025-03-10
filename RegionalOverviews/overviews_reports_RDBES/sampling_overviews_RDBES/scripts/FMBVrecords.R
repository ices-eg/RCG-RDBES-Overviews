cs |>
  select(DEhierarchy, SDctry, FMid) |>
  filter(FMid != "" & !is.na(FMid)) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> FMsummary

summarizedRow <- FMsummary |>
  ungroup() |>
  summarise(`Country/Hierarchy` = "Total",
            across(-`Country/Hierarchy`, sum, na.rm = TRUE))

FMsummary <- bind_rows(FMsummary, summarizedRow)

FMsummary <- FMsummary |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))

#only lower hierarchy B
cs |>
  select(DEhierarchy, SDctry, FMid, SAlowHierarchy) |>
  filter(SAlowHierarchy == 'B', FMid != "", !is.na(FMid)) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> FMsummaryB

summarizedRow <- FMsummaryB |>
  ungroup() |>
  summarise(`Country/Hierarchy` = "Total",
            across(-`Country/Hierarchy`, sum, na.rm = TRUE))

FMsummaryB <- bind_rows(FMsummaryB, summarizedRow)

FMsummaryB <- FMsummaryB |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))

# merge tables with prefix. I want numbers of regional routine in brackets
combined <- FMsummary |>
  left_join(FMsummaryB, by = "Country/Hierarchy", suffix = c("", ".y"))

combined <- combined |>
  mutate(across(ends_with(".y"), ~ paste0("(", replace_na(as.character(.), "0"), ")")))

# merge cells from both tables
for(col in colnames(FMsummary)[-1]) {
  combined[[col]] <- paste0(combined[[col]], combined[[paste0(col, ".y")]])
}

# delete columns with ".y"
combined <- combined |>
  select(-ends_with(".y"))|>
  mutate(across(everything(), ~ gsub("\\(NA\\)", "", .)),
         across(everything(), ~ gsub("\\(0\\)", "", .)),
         across(everything(), ~ gsub("\\(\\)", "", .)))