### Fiktive Population aus Mieten-Datentabelle

# Daten einlesen
here("data", "Immo-Datensatz.csv") |>  read.csv2() -> immo
# kategoriale Variablen in Faktoren umwandeln
mieten <- immo |> mutate(
  balcony = factor(balcony, levels = c(0, 1), labels = c("nein", "ja")),
  haskitchen = factor(haskitchen, levels = c(0, 1), labels = c("nein", "ja"))
)
# Wohnungen ohne Bewertung interiorqual löschen
mieten |> filter(interiorqual != 0) -> mieteniq
# neue Datentabelle Population erzeugen
set.seed(123)
# trfac <- runif(1000, 0.9, 1.1)
# lsfac <- runif(1000, 0.9, 1.1)
trfac <- rnorm(1000, 1, sd = 0.06)
lsfac <- rnorm(1000, 1, sd = 0.04)
mietenpop <- mieteniq
for(i in 1:1000){
  add <- mieteniq
  add <- add |> mutate(
    totalrent = round(totalrent * trfac[i], 0),
    livingspace = round(livingspace * lsfac[i], 0)
  )
  mietenpop <- rbind(mietenpop, add)
}
mietenpop <- sample(mietenpop, 100000)
mietenpop$orig.id <- NULL
write.csv2(mietenpop, file = here("data", "MietenPopulation.csv"), row.names = FALSE)
