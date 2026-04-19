library(tidyverse)
library(readxl)

departments <- read_csv("Data/departments.csv")
diagnosis <- read_csv("Data/diagnosis.csv")
encounters <- read_csv("Data/encounters.csv")
patients <- read_csv("Data/patients.csv")
providers <- read_csv("Data/providers.csv")
social <- read_csv("Data/social_determinants.csv")
tiger <- read_csv("Data/tigercensuscodes.csv")

social <- social |>
  filter(DisplayName != "*Unspecified")

plot1 <- diagnosis |>
  mutate(DiagnosisValue = sub("[0-9].*", "", x = DiagnosisValue)) |>
  filter(DiagnosisValue != "ERRENC") |>
  filter(DiagnosisValue != "EXECPXICD") |>
  filter(DiagnosisValue != "NOD.X") |>
  filter(DiagnosisValue != "WESTAR")
plot1 |>
  ggplot() +
  geom_bar(aes(x = DiagnosisValue))



encounters |>
  ggplot() +
  geom_bar(aes(x = Type))
encounters |>
  ggplot() +
  geom_bar(aes(x = VisitTypeDescription))


what <- encounters |>
  inner_join(patients, by = c("PatientDurableKey" = "DurableKey")) |>
  arrange(PatientDurableKey) |>
  filter(Type != "Patient Outreach")


funny <- patients |>
  filter(SmokingStatus != "*Unknown") |>
  filter(SmokingStatus != "Never Assessed") |>
  filter(SmokingStatus != "Unknown")

ggplot() +
geom_bar(data = filter(funny, VitalStatus == "Alive"), aes(x = SmokingStatus), fill = "blue") + 
geom_bar(data = filter(funny, VitalStatus == "Deceased"), aes(x = SmokingStatus), fill = "red")

patients |>
  filter(PatientBirthYearBin != "1850") |>
  ggplot() +
  geom_bar(aes(x = as.factor(PatientBirthYearBin)))

encounters |>
  group_by(PatientDurableKey) |>
  summarize()

patients |>
  group_by(DurableKey) |>
  summarize()

max(encounters$PatientDurableKey)
max(patients$DurableKey)


what2 <- what |>
  left_join(providers, by = c("AttendingProviderDurableKey" = "DurableKey")) |>
  filter(ClinicianTitle != "*Unspecified")


what <- what |>
  filter(Type != "Patient Outreach")





yess2 <- yess |>
  filter(Domain == "physical activity")  |>
  

yess2 |>
  ggplot() +
  geom_bar(aes(x = AnswerText, fill = DisplayName))


social_fit <- social |>
  filter(Domain == "physical activity") |>
  filter(DisplayName != "On average, how many minutes do you engage in exercise at this level?") |>
  filter(AnswerText != "Patient declined") |>
  filter(AnswerText != "Patient unable to answer") |>
  mutate(Exercise = as.numeric(sub("[^0-9].*", "", x = AnswerText, useBytes = FALSE))) |>
  select(Exercise, EncounterKey)


encounters_fit <- encounters |>
  arrange(PatientDurableKey) |>
  group_by(PatientDurableKey) |>
  mutate(Encounter = row_number(), num_encounter = n()) |>
  left_join(social_fit, by = c("EncounterKey" = "EncounterKey"))

encounters_fit <- yess |>
  filter(num_encounter < 1000)

lm <- lm(num_encounter ~ Exercise, encounters_fit)
summary(lm)
plot(num_encounter ~ Exercise, encounters_fit) 
abline(lm)
