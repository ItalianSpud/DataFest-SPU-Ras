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



