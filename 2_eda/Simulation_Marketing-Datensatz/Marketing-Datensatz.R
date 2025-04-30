# Working Directory
setwd("C:/Users/celine.widera/Desktop")

# Für Reproduzierbarkeit
set.seed(5926)

# Einlesen der Daten
marketing_data <- read.csv2("rintro-chapter5.csv", sep= ",")

# Neue Kinder-Variable kodieren
marketing_data$haveKids <- ifelse(marketing_data$kids == 0, "kidsNo", "kidsYes")

# Skalenniveau metrischer Variablen formatieren
marketing_data$age <- as.integer(marketing_data$age)
marketing_data$income <- as.integer(marketing_data$income)

# Auswählen der Variablen von Interesse
marketing_data <- marketing_data[, c(1,2,3,8,4)]


### Subscription-Variable neu simulieren
# Anzahl der Beobachtungen
n <- nrow(marketing_data)

# Kodieren von haveKids und ownHome als numerische Variable
marketing_data$kids_num <- ifelse(marketing_data$haveKids == "kidsYes", 1, 0)

# Regressionskoeffizienten festlegen
beta_0 <- -1      # Intercept
beta_age <- -0.003  # negativer Einfluss mit steigendem Alter
beta_income <- 0.00005  # positiver Einfluss von Einkommen
beta_kids <- 0.7   # positiver Einfluss, wenn Kinder vorhanden


# Lineare Vorhersage + logistische Transformation
log_odds <- beta_0 + beta_age * marketing_data$age + beta_income * marketing_data$income + beta_kids * marketing_data$kids_num
prob_yes <- 1 / (1 + exp(-log_odds))

# Zufällige Ziehung basierend auf Wahrscheinlichkeit
set.seed(5926)
marketing_data$subscribe <- ifelse(runif(n) < prob_yes, "subYes", "subNo")
table(marketing_data$subscribe)


### Streaming-Budget Vaiable simulieren

for (i in 1:nrow(marketing_data)) {
  
  marketing_data$streaming_cost[i] <- round((marketing_data$income[i] * (1/5000) + ifelse(marketing_data$haveKids[i] == "kidsYes", 10, 5) + rnorm(1, mean = 0, sd = 3)), 2)
  
}

# Streaming-Budget gleich 0, wenn keine Subscription
marketing_data$streaming_cost[c(which(marketing_data$subscribe == "subNo"))] <- 0

# Entfernen von kids_num aus dem Datensatz
marketing_data <- marketing_data[, -which(names(marketing_data) == "kids_num")]


# Dtensatz speichern
write.csv2(marketing_data, file = "marketing_data.csv", sep = ",", row.names = F)
