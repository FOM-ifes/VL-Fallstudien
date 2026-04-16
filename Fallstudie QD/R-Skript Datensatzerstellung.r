# Datenbereinigung für den LEGO-Datensatz
# Grundlage: lego.population.csv

library(tidyverse)

# Pfad definieren
population_path <- "D:/Users/daniel.laudy/Desktop/Lego-Datensatz/Data/Data/lego.population.csv"

# Spaltennamen festlegen
lego_colnames <- c(
  "Item_Number", "Set_Name", "Theme", "Pieces", "Price", "Amazon_Price",
  "Year", "Ages", "Pages", "Minifigures", "Packaging", "Weight",
  "Unique_Pieces", "Availability", "Size"
)

# Datei einlesen 
lego_raw <- read.csv(
  file = population_path,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE,
  header = TRUE
)

if (!all(lego_colnames %in% names(lego_raw))) {
  lego_raw <- read.csv(
    file = population_path,
    fileEncoding = "latin1",
    stringsAsFactors = FALSE,
    header = FALSE,
    col.names = lego_colnames
  )
}

# Datensatz bereinigen 
lego_clean_all <- lego_raw |>
  mutate(
    Item_Number = as.character(Item_Number),
    Set_Name = trimws(Set_Name),
    
    Theme = trimws(Theme),
    Theme = gsub("®", "", Theme, fixed = TRUE),
    Theme = gsub("Æ", "", Theme, fixed = TRUE),
    Theme = gsub("ô", "", Theme, fixed = TRUE),
    Theme = gsub("", "", Theme, fixed = TRUE),
    Theme = gsub("\\s+", " ", Theme),
    Theme = trimws(Theme),
    
    Pieces = as.numeric(Pieces),
    Price = as.numeric(trimws(gsub("\\$", "", Price))),
    Amazon_Price = as.numeric(trimws(gsub("\\$", "", Amazon_Price))),
    Year = as.integer(Year),
    Ages = trimws(Ages),
    Pages = as.numeric(Pages),
    Minifigures = as.numeric(Minifigures),
    Packaging = trimws(Packaging),
    Weight = trimws(Weight),
    Unique_Pieces = as.numeric(Unique_Pieces),
    Availability = trimws(Availability),
    Size = trimws(Size)
  ) |>
  mutate(
    across(where(is.character), ~ na_if(., "NA")),
    across(where(is.character), ~ na_if(., "")),
    Theme = factor(Theme),
    Packaging = factor(Packaging),
    Availability = factor(Availability),
    Size = factor(Size, levels = c("Small", "Large"))
  )

# Bereinigten Datensatz mit 15 Variablen speichern (vollständig)
write.csv(
  lego_clean_all,
  file = "lego_population_clean_all.csv",
  row.names = FALSE,
  na = ""
)

# QMD-Datensatz erzeugen für die Fallstudie mit nur 6 Variablen und fehlende Werte entfernen
lego_qmd <- lego_clean_all |>
  select(Item_Number, Set_Name, Price, Pieces, Theme, Size) |>
  drop_na(Item_Number, Set_Name, Price, Pieces, Theme, Size) |>
  mutate(
    Theme = factor(Theme),
    Size = factor(Size, levels = c("Small", "Large"))
  )

write.csv(
  lego_qmd,
  file = "lego_population_qmd.csv",
  row.names = FALSE,
  na = ""
)

# Kontrollcheck
str(lego_clean_all)
summary(lego_clean_all)
head(lego_clean_all)

str(lego_qmd)
summary(lego_qmd)
head(lego_qmd)