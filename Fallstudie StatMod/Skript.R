# Benötigtes Paket laden
library(here)

# Rohdaten aus dem Unterordner data einlesen
daten <- read.table(here("data", "student-por.csv"),
                    sep = ";",
                    header = TRUE)

# Zusatzvariable für den Kurs erzeugen
daten$course <- "por"

# Nur die für die Fallstudie tatsächlich genutzten Variablen behalten
keep_vars <- c(
  "school", "sex", "Medu", "Fedu", "studytime", "failures", "paid",
  "higher", "internet", "famrel", "freetime", "goout", "Walc",
  "health", "absences", "G1", "G2", "G3", "course"
)

daten <- daten[, keep_vars, drop = FALSE]

# Numerische Variablen in numerisches Format umwandeln
daten$Medu <- as.numeric(daten$Medu)
daten$Fedu <- as.numeric(daten$Fedu)
daten$studytime <- as.numeric(daten$studytime)
daten$failures <- as.numeric(daten$failures)
daten$famrel <- as.numeric(daten$famrel)
daten$freetime <- as.numeric(daten$freetime)
daten$goout <- as.numeric(daten$goout)
daten$Walc <- as.numeric(daten$Walc)
daten$health <- as.numeric(daten$health)
daten$absences <- as.numeric(daten$absences)
daten$G1 <- as.numeric(daten$G1)
daten$G2 <- as.numeric(daten$G2)
daten$G3 <- as.numeric(daten$G3)

# Kategoriale Variablen als Faktoren festlegen
daten$school <- factor(daten$school)
daten$sex <- factor(daten$sex)
daten$paid <- factor(daten$paid, levels = c("no", "yes"))
daten$higher <- factor(daten$higher, levels = c("no", "yes"))
daten$internet <- factor(daten$internet, levels = c("no", "yes"))
daten$course <- factor(daten$course)

# Kontrollausgaben
str(daten)
head(daten)
nrow(daten)
ncol(daten)
names(daten)

# Bereinigten Datensatz als CSV speichern
write.csv(daten,
          file = here("data", "student-por-clean.csv"),
          row.names = FALSE)