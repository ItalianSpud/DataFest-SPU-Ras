# library(car)
# library(agricola)
# library(pastecs)

encounters_gap <- encounters |>
  mutate(Date = as.Date(Date, "%m/%d/%y")) |>
  group_by(PatientDurableKey) |>
  arrange(Date) |>
  mutate(Encounter = row_number(), num_encounter = n()) |>
  filter(num_encounter > 1) |>
  arrange(PatientDurableKey)


encounters_gap$Day_Gap <- encounters_gap$Date - lag(encounters_gap$Date, default = NA)

encounters_gap <- encounters_gap |>
  group_by(PatientDurableKey) |>
  mutate(Day_Gap = ifelse(row_number() == 1, NA, Day_Gap))

# encounters_diagnosis <- encounters_gap |>
  left_join(diagnosis, by = c("PrimaryDiagnosisKey" = "DiagnosisKey")) |>
  left_join(patients, by = c("PatientDurableKey" = "DurableKey")) |>
  select(-(CensusBlockGroupFipsCode:SexualOrientation))

# social <- social |>
  filter(is.na(Domain) == F)

# encounters_full <- encounters_diagnosis |>
#  left_join(social, by = c("EncounterKey" = "EncounterKey"))




### Code for the plot
social <- social |>
  filter(is.na(Domain) == F)

encounters_filtered <- encounters_gap |>
  left_join(social, by = c("EncounterKey" = "EncounterKey"))|>
  filter(Domain == "stress")

theplot <- encounters_filtered |>
  group_by(AnswerText) |>
  filter(Day_Gap > 0)

### Biostats Flexing 
# kruskal.test(Day_Gap ~ AnswerText, theplot)

theplot <- theplot |>
  summarize(mean = mean(Day_Gap, na.rm = T))


ggplot(theplot, aes(x = AnswerText, y = mean)) + 
         geom_bar(fill = "gray80", color = "black", width = 0.6, stat = "identity")  

  