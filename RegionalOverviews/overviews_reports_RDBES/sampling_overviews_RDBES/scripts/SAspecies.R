cs |>
  select(DEhierarchy, SDctry, SAspeCode) |>
  filter(!is.na(SAspeCode))|>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) -> SAsummary

cs |>
  select(SDctry, SAspeCode) |>
  filter(!is.na(SAspeCode))|>
  distinct() |>
  group_by(SDctry) |>
  count() |>
  rename(`Country/Hierarchy` = SDctry,
         Total = n) -> summarizedCol

SAsummary <- SAsummary |>
  left_join(summarizedCol, by = "Country/Hierarchy")

summarizedRow <- cs |>
  select(DEhierarchy, SAspeCode) |>
  distinct() |>
  group_by(DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n)|>
  mutate(`Country/Hierarchy` = "Total")

summarizedRowTotal <- cs |>
  select(SAspeCode) |>
  distinct() |>
  count() |>
  rename(Total = n)

cbind(summarizedRow,summarizedRowTotal) -> summarizedRow
SAsummary <- bind_rows(SAsummary, summarizedRow)

SAsummary <- SAsummary |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))