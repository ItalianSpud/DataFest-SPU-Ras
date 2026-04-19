encounters <- encounters |>
  mutate(Date = as.Date(Date, "%m/%d/%y")) |>
  group_by(PatientDurableKey) |>
  arrange(Date) |>
  mutate(Encounter = row_number(), num_encounter = n()) |>
  filter(num_encounter > 1) |>
  arrange(PatientDurableKey)


encounters$Day_Gap <- encounters$Date - lag(encounters$Date, default = NA)

encounters <- pleasework |>
  group_by(PatientDurableKey) |>
  mutate(Day_Gap = ifelse(row_number() == 1, NA, Day_Gap))