cs|> 
  select(BVtypeMeas, BVid) |>
  filter(!is.na(BVid),
         BVid != "")|>
  distinct() |>
  group_by(BVtypeMeas) |>
  count() |>
  arrange(desc(n)) |>
  pivot_wider(names_from = BVtypeMeas, values_from = n) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) -> BVtypeMeasSummary