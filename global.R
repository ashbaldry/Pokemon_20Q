library(shiny)
library(data.table)
library(recoder)
library(DT)

####WWW Tables####
poketab <- data.table::fread('./www/pokeinfo.csv', na.strings = '')
pokeegg <- data.table::fread('./www/pokeeggs.csv', na.strings = '')
poketypeff <- data.table::fread('./www/pokeeff.csv', na.strings = '')
pokeguesstab <- rbindlist(
  list(
    data.table(Column = 'Weight', Criteria = 'Pokemon weight is higher or lower than median'),
    data.table(Column = 'Height', Criteria = 'Pokemon height is higher or lower than median'),
    data.table(Column = 'Mega', Criteria = 'Pokemon has a mega evolution'),
    data.table(Column = 'Generation', Criteria = 'Pokemon is from a certain generation or up to a certain one'),
    data.table(Column = 'Colour', Criteria = 'Pokemon is of a certain colour'),
    data.table(Column = 'Shape', Criteria = 'Pokemon is of a certain shape'),
    data.table(Column = 'Baby', Criteria = 'Pokemon is a baby pokemon'),
    data.table(Column = 'EvoStage', Criteria = 'Pokemon is of a certain evolution stage'),
    data.table(Column = 'HasEvolution', Criteria = 'Pokemon has an evolution'),
    data.table(Column = 'HasEvolved', Criteria = 'Pokemon has evolved from something'),
    data.table(Column = 'Type', Criteria = 'Pokemon is of a certain type'),
    data.table(Column = 'DualType', Criteria = 'Pokemon has dual type'),
    data.table(Column = 'Egg', Criteria = 'Pokemon is from certain egg family'),
    data.table(Column = 'TypeEff', Criteria = 'Type move is super effective/not effective against the Pokemon')
  )
)

# Reduce possibilities
#
# correct - TRUE if answered 'Yes', FALSE if answered 'No'
# qtype - Summary on what type of question was answered
# qinfo - Extra information that goes with the question class
# pokeref - Data.table of the remaining Pokémon left to guess from
# pokeegss - Data.table of the egg types left in the study
# poketypes - Data.table of the types combinations
PokeAnswer <- function(correct, qtype, qinfo, pokeref, pokeeggs, poketypes) {
  ####Weight####
  if(qtype == 'Weight') {
    medweight <- median(pokeref$weight)
    if(sum(qinfo$Lighter, correct) %% 2) return(pokeref[weight >= medweight]) else return(pokeref[weight <= medweight])
  }

  ####Height####
  if(qtype == 'Height') {
    medheight <- median(pokeref$height)
    if(sum(qinfo$Smaller, correct) %% 2) return(pokeref[height >= medheight]) else return(pokeref[height <= medheight])
  }

  ####Mega####
  if(qtype == 'Mega') {
    if(correct) return(pokeref[has_mega == 1]) else return(pokeref[has_mega == 0])
  }

  ####Generation####
  if(qtype == 'Generation') {
    if(correct) return(pokeref[generation_id %in% qinfo$Version]) else return(pokeref[!generation_id %in% qinfo$Version])
  }

  ####Colour####
  if(qtype == 'Colour') {
    if(correct) return(pokeref[colour == qinfo$Colour]) else return(pokeref[colour != qinfo$Colour])
  }

  ####Shape####
  if(qtype == 'Shape') {
    if(correct) return(pokeref[shape == qinfo$Shape]) else return(pokeref[shape != qinfo$Shape])
  }

  ####Baby####
  if(qtype == 'Baby') {
    if(correct) return(pokeref[is_baby == 1]) else return(pokeref[is_baby == 0])
  }

  ####EvoStage####
  if(qtype == 'EvoStage') {
    if(correct) return(pokeref[evo_stage == qinfo$Stage]) else return(pokeref[evo_stage != qinfo$Stage])
  }

  ####HasEvolution####
  if(qtype == 'HasEvolution') {
    if(correct) return(pokeref[has_evo == 1]) else return(pokeref[has_evo == 0])
  }

  ####HasEvolved####
  if(qtype == 'HasEvolved') {
    if(correct) return(pokeref[is_evolved == 1]) else return(pokeref[is_evolved == 0])
  }

  ####Type####
  if(qtype == 'Type') {
    if(correct) {
      return(pokeref[type_1 == qinfo$Type | type_2 == qinfo$Type])
    } else {
      return(pokeref[(type_1 != qinfo$Type & type_2 != qinfo$Type) | (type_1 != qinfo$Type & is.na(type_2))])
    }
  }

  ####DualType####
  if(qtype == 'DualType') {
    if(correct) return(pokeref[!is.na(type_2)]) else return(pokeref[is.na(type_2)])
  }

  ####Egg####
  if(qtype == 'Egg') {
    if(correct) {
      return(pokeref[species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]])
    } else {
      return(pokeref[!species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]])
    }
  }

  ####TypeEff####
  if(qtype == 'TypeEff') {
    goodtypes <- poketypes[damage_type == qinfo$Type]
    typ1match <- goodtypes[match(pokeref$type_1, target_type), damage_factor]
    typ2match <- goodtypes[match(pokeref$type_2, target_type), damage_factor]
    typ1match[typ1match == 200] <- 150
    typ2match[typ2match == 200] <- 150
    typ2means <- round(rowMeans(cbind(typ1match, typ2match), na.rm = TRUE), 0)
    typ2means[sapply(typ1match == 0 | typ2match == 0, any, na.rm = TRUE)] <- 0
    typ2means <- recoder::recoder(typ2means, '0:"No Effect";>0 & <100:"Not Very Effective";100:"Effective";>100:"Super Effective"')
    if(correct) return(pokeref[typ2means == qinfo$Effect]) else return(pokeref[typ2means != qinfo$Effect])
  }
}

# Reduce Guessing Questions
#
# pokeguess - Data.table of the guessing options
# pokeref - Data.table of the remaining Pokémon left to guess from
# pokeegss - Data.table of the egg types left in the study
# poketypes - Data.table of the types combinations
PokeGuess <- function(pokeguess, pokeref, pokeeggs, poketypes) {
  guessopts <- pokeguess$Column
  ####Weight####
  if(isTRUE('Weight' %in% guessopts)) if(isTRUE(sd(pokeref$weight) == 0)) pokeguess <- pokeguess[Column != 'Weight']

  ####Height####
  if(isTRUE('Height' %in% guessopts)) if(isTRUE(sd(pokeref$height) == 0)) pokeguess <- pokeguess[Column != 'Height']

  ####Mega####
  if(isTRUE('Mega' %in% guessopts)) if(isTRUE(sd(pokeref$has_mega) == 0)) pokeguess <- pokeguess[Column != 'Mega']

  ####Generation####
  if(isTRUE('Generation' %in% guessopts)) if(isTRUE(sd(pokeref$generation_id) == 0)) pokeguess <- pokeguess[Column != 'Generation']

  ####Colour####
  if(isTRUE('Colour' %in% guessopts)) if(length(unique(pokeref$colour)) == 1) pokeguess <- pokeguess[Column != 'Colour']

  ####Shape####
  if(isTRUE('Shape' %in% guessopts)) if(length(unique(pokeref$shape)) == 1) pokeguess <- pokeguess[Column != 'Shape']

  ####Baby####
  if(isTRUE('Baby' %in% guessopts)) if(isTRUE(sd(pokeref$is_baby) == 0)) pokeguess <- pokeguess[Column != 'Baby']

  ####EvoStage####
  if(isTRUE('EvoStage' %in% guessopts)) if(isTRUE(sd(pokeref$evo_stage) == 0)) pokeguess <- pokeguess[Column != 'EvoStage']

  ####HasEvolution####
  if(isTRUE('HasEvolution' %in% guessopts)) if(isTRUE(sd(pokeref$has_evo) == 0)) pokeguess <- pokeguess[Column != 'HasEvolution']

  ####HasEvolution####
  if(isTRUE('HasEvolved' %in% guessopts)) if(isTRUE(sd(pokeref$is_evolved) == 0)) pokeguess <- pokeguess[Column != 'HasEvolved']

  ####Type####
  if(isTRUE('Type' %in% guessopts)) {
    if(length(unique(c(pokeref$type_1, na.omit(pokeref$type_2)))) == 1) pokeguess <- pokeguess[Column != 'Type']
  }

  ####DualType####
  if(isTRUE('DualType' %in% guessopts)) {
    if(all(is.na(pokeref$type_2)) | all(!is.na(pokeref$type_2))) pokeguess <- pokeguess[Column != 'DualType']
  }

  ####Egg####
  if(isTRUE('Egg' %in% guessopts)) if(length(unique(pokeeggs$egg_group_name)) == 1) pokeguess <- pokeguess[Column != 'Egg']

  ####TypeEff####
  if(isTRUE('TypeEff' %in% guessopts)) if(length(unique(poketypes$target_type)) == 1) pokeguess <- pokeguess[Column != 'TypeEff']

  return(pokeguess)
}

# Create New Question
#
# pokeguess - Data.table of the guessing options
# pokeref - Data.table of the remaining Pokémon left to guess from
# pokeegss - Data.table of the egg types left in the study
# poketypes - Data.table of the types combinations
PokeQuestion <- function(pokeguess, pokeref, pokeeggs, poketypes) {
  if(nrow(pokeguess) == 0 | nrow(pokeref) == 0) return(list(QType = '', Question = list(Question = '')))

  qtype <- sample(setdiff(pokeguess$Column, 'Shape'), 1)
  pokeQ <- list(Question = '')

  ####Weight####
  if(qtype == 'Weight') {
    direc <- sample(c('lighter', 'heavier'), 1)
    pokeQ <- list(Question = paste0('Is the Pokémon ', direc, ' than ', median(pokeref$weight), 'kg?'),
                  Lighter = direc == 'lighter')
  }

  ####Height####
  if(qtype == 'Height') {
    direc <- sample(c('smaller', 'taller'), 1)
    pokeQ <- list(Question = paste0('Is the Pokémon ', direc, ' than ', median(pokeref$height), 'm?'),
                  Smaller = direc == 'smaller')
  }

  ####Mega####
  if(qtype == 'Mega') {
    pokeQ <- list(Question = 'Does the Pokémon have a mega evolution?')
  }

  ####Generation####
  if(qtype == 'Generation') {
    pokevers <- table(pokeref$generation_id)
    nhalfpoke <- nrow(pokeref) / 2
    minmeandiff <- min(abs(pokevers - nhalfpoke)) / nhalfpoke
    minvers <- names(pokevers)[which.min(abs(pokevers - nhalfpoke))]
    gencnt <- 1
    while(isTRUE(minmeandiff >= 0.15 & gencnt <= length(pokevers) / 2)) {
      gencnt <- gencnt + 1
      pokeverscombs <- combn(names(pokevers), gencnt)
      pokeverssums <- apply(pokeverscombs, 2, function(y) sum(pokevers[y]))
      minmeandiff <- min(abs(pokeverssums - nhalfpoke)) / nhalfpoke
      minvers <- pokeverscombs[, which.min(abs(pokeverssums - nhalfpoke))]
    }
    if(length(minvers) == 1) {
      pokeQ <- list(Question = paste0('Did the Pokémon first appear in generation ', minvers, '?'), Version = minvers)
    } else {
      pokeQ <- list(Question = paste0('Did the Pokémon first appear in any of the following generations: ',
                                      paste(minvers, collapse = ', '), '?'), Version = minvers)
    }
  }

  ####Colour####
  if(qtype == 'Colour') {
    col <- sample(pokeref$colour, 1)
    pokeQ <- list(Question = paste0('Is the colour of the Pokémon primarily ', tolower(col), '?'), Colour = col)
  }

  ####Shape####
  if(qtype == 'Shape') {
    shape <- sample(pokeref$shape, 1)
    pokeQ <- list(Question = paste0('Is the Pokémon a ', shape, '?'), Shape = shape)
  }

  ####Baby####
  if(qtype == 'Baby') {
    pokeQ <- list(Question = 'Is it a baby Pokémon?')
  }

  ####EvoStage####
  if(qtype == 'EvoStage') {
    evostage <- sample(pokeref$evo_stage, 1)
    pokeQ <- list(Question = paste('Is the Pokémon in it\'s',
                                   switch(evostage, '1' = 'first', '2' = 'second', '3' = 'third'),
                                   'stage of evolution?'), Stage = evostage)
  }

  ####HasEvolution####
  if(qtype == 'HasEvolution') {
    pokeQ <- list(Question = 'Does the Pokémon have an evolution (not including mega evolution)?')
  }

  ####HasEvolved####
  if(qtype == 'HasEvolved') {
    pokeQ <- list(Question = 'Does the Pokémon evolve from another Pokémon (not including mega evolution)?')
  }

  ####Type####
  if(qtype == 'Type') {
    type <- sample(c(pokeref$type_1, na.omit(pokeref$type_2)), 1)
    pokeQ <- list(Question = paste0('Is the Pokémon a', if(substr(type, 1, 1) %in% c('A', 'E', 'I', 'O', 'U')) 'n',
                                    ' ', tolower(type), ' type?'), Type = type)
  }

  ###DualType###
  if(qtype == 'DualType') {
    pokeQ <- list(Question = 'Is it a dual type Pokémon?')
  }

  ####Egg####
  if(qtype == 'Egg') {
    eggtype <- sample(pokeeggs$egg_group_name, 1)
    pokeQ <- list(Question = paste('Is the Pokémon come from the', tolower(eggtype), 'egg family?'), EggType = eggtype)
  }

  ####TypeEff####
  if(qtype == 'TypeEff') {
    type <- sample(poketypes$damage_type, 1)
    typeeff <- sample(poketypes[damage_type == type, damage_eff], 1)
    if(typeeff == 'No Effect') {
      pokeQ <- list(Question = paste0('Does a', if(substr(type, 1, 1) %in% c('A', 'E', 'I', 'O', 'U')) 'n', ' ',
                                      tolower(type), ' type attack have ', tolower(typeeff), ' on the Pokémon?'),
                    Type = type, Effect = typeeff)
    } else {
      pokeQ <- list(Question = paste0('Is a', if(substr(type, 1, 1) %in% c('A', 'E', 'I', 'O', 'U')) 'n', ' ',
                                      tolower(type), ' type attack ', tolower(typeeff), ' on the Pokémon?'),
                    Type = type, Effect = typeeff)
    }
  }

  return(list(QType = qtype, Question = pokeQ))
}