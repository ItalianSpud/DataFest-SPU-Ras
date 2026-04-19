library(tidyverse)
library(readxl)

departments <- read_csv("departments.csv")
diagnosis <- read_csv("diagnosis.csv")
encounters <- read_csv("encounters.csv")
patients <- read_csv("patients.csv")
providers <- read_csv("providers.csv")
social <- read_csv("social_determinants.csv")
tiger <- read_csv("tigercensuscodes.csv")

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


what <- plot1 |>
  left_join(encounters, by = c("DiagnosisKey" = "PrimaryDiagnosisKey"), relationship = "many-to-many")


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


encounters <- encounters |>
  mutate(StayDuration = difftime(DischargeInstant, AdmissionInstant, units = "mins"))

encounters |> group_by(Type) |>
  summarize(avgTime = median(StayDuration, na.rm = TRUE)) |>
  ggplot() +
  geom_col(aes(x = Type, y = avgTime))

sum_by_type <- encounters |> group_by(Type) |>
  summarize(avgTime = median(StayDuration, na.rm = TRUE))

durables <- encounters |>
  left_join(patients, by=c("PatientDurableKey"="DurableKey"))


provider_encounters <- encounters |>
  left_join(providers, by=c("ProviderDurableKey"="DurableKey"))

encounters <- encounters |>
  mutate(isEncounter = (PrimaryDiagnosisKey != -1) + 1 - 1)

sum_encount_per_diag <- encounters |> 
  group_by(PatientDurableKey) |>
  summarize(Encounters = n())

social_enc <- encounters |>
  left_join(social, by=c("EncounterKey"="EncounterKey"))

social_enc_per_person <- social |>
  left_join(sum_encount_per_diag, by=c("PatientDurableKey"="PatientDurableKey"))

social_enc_per_person <- social_enc_per_person |>
  group_by(Domain, DisplayName, AnswerText, PatientDurableKey)

social_enc_by_dom <- social_enc |>
  group_by(Domain, DisplayName, AnswerText) |>
  summarize(Encounters = n())

social_enc_per_person <- social_enc_per_person |>
  arrange(desc(Encounters))

social_enc_dep <- social_enc |>
  filter(Domain == "Depression")

answers <- social_enc_dep |>
  group_by(DisplayName, AnswerText) |>
  summarize(Encounters = n())

problem_child <- social_enc |>
  filter(PatientDurableKey.x == 7136932)

social_enc_per_person <- social_enc_per_person |>
  left_join(select(encounters, Date, EncounterKey), by=c("EncounterKey"="EncounterKey"))

social_enc_per_person <- social_enc_per_person |>
  filter(DisplayName != "*Unspecified")

social_enc_per_person <- social_enc_per_person |>
  mutate(Date = as.Date(Date, "%m/%d/%y"))

social_enc_per_person <- social_enc_per_person |>
  arrange(Date)

stress <- social_enc_per_person |>
  filter(Domain == "stress")

