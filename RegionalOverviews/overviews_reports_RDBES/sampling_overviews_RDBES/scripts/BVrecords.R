cs |>
  select(DEhierarchy, SDctry, BVid) |>
  filter(BVid != "", !is.na(BVid)) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> BVsummary

summarizedRow <- BVsummary |>
  ungroup() |>
  summarise(`Country/Hierarchy` = "Total",
            across(-`Country/Hierarchy`, sum, na.rm = TRUE))

BVsummary <- bind_rows(BVsummary, summarizedRow)

BVsummary <- BVsummary |>
  mutate(across(everything(), ~ replace_na(as.character(.), "")))

cs |>
  select(DEhierarchy, SDctry, BVid, SAlowHierarchy) |>
  filter(SAlowHierarchy == 'C',
         BVid != "",
         !is.na(BVid)) |>
  distinct() |>
  group_by(SDctry, DEhierarchy) |>
  count() |>
  arrange(DEhierarchy) |>
  pivot_wider(names_from = DEhierarchy, values_from = n) |>
  rename(`Country/Hierarchy` = SDctry) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> BVsummaryC

# merge tables with prefix. I want numbers of regional routine in brackets
combined <- BVsummary |>
  left_join(BVsummaryC, by = "Country/Hierarchy", suffix = c("", ".y"))

combined <- combined |>
  mutate(across(ends_with(".y"), ~ paste0("(", replace_na(as.character(.), "0"), ")")))

# merge cells from both tables
for(col in colnames(BVsummary)[-1]) {
  combined[[col]] <- paste0(combined[[col]], combined[[paste0(col, ".y")]])
}