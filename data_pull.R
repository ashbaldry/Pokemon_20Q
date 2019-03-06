library(data.table)
# Pokemon Names
base_dat <- fread("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon.csv")
names_dat <- fread("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon_species_names.csv")

# Languages
lanuages_dat <- fread("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/language_names.csv")

# Basic Stats
species_dat <- fread("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon_species.csv")
