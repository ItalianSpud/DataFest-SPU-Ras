encounters <- encounters |>
  mutate(Date = as.Date(Date, "%m/%d/%y")) |>
  group_by(PatientDurableKey) |>
  arrange(Date) |>
  mutate(Encounter = row_number(), num_encounter = n()) |>
  filter(num_encounter > 1) |>
  arrange(PatientDurableKey) |>
  mutate(Day_gap = Date - lag(Date, default = NA)) |>
  mutate(Day_gap = as.double(sub("[^0-9].*", "", Day_gap)))
