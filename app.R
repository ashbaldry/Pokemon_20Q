# This is a Shiny web application that will try and guess one of the pokemon within 20 yes/no questions.
library(shiny)
library(shinydashboard)
library(data.table)#
library(recoder)

####UI####
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(

    ####Title Page####
    shiny::fluidRow(
      div(img(src = 'logo.png', height = 250), style='text-align: center;')
    ),

    ####20 Questions####
    shiny::fluidRow(
      shiny::column(
        div(h3(strong('Computer')), style='text-align: center;'),
        div(h1(textOutput('compscore')), style='text-align: center;'),
        width = 3
      ),
      shinydashboard::box(
        div(h4(strong(textOutput('questinfo'))), style='text-align: right;'),
        uiOutput('pokeguess'),
        div(radioButtons('yncheck', '', c('Yes', 'No'), inline = TRUE), style='text-align: center;'),
        div(actionButton('ynenter', 'Enter', icon('play')), style='text-align: center;'),
        selectInput('nguesses', 'Number of Questions', seq(5, 20, 5), 20, width = 150)
      ),
      shiny::column(
        div(h3(strong('Player')), style='text-align: center;'),
        div(h1(textOutput('playscore')), style='text-align: center;'),
        width = 3
      )
    ),

    includeCSS('www/style.css')
  ),

  title = 'Pokemon 20 Questions'
)

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

####Server####
server <- function(input, output, session) {
  pokereact <- reactiveValues(Table = poketab, Eggs = pokeegg, Type = poketypeff, Guess = pokeguesstab,
                              CompScore = 0, PlayScore = 0, Question = 0,
                              QuestInfo = list(Question = list('Press Enter for First Question')))

  output$questinfo <- renderText(paste0(input$ynenter - pokereact$Question, ' / ', input$nguesses))
  output$compscore <- renderText(pokereact$CompScore)
  output$playscore <- renderText(pokereact$PlayScore)

  observeEvent(input$ynenter, {
    if(input$ynenter > 1 & input$ynenter - pokereact$Question > 1) {
      correct <- isTRUE(input$yncheck == 'Yes')
      question <- pokereact$QuestInfo$Question
      qtype <- pokereact$QuestInfo$QType
      if(qtype == 'Weight') {
        direcsign <- ifelse(correct, question$Direc, ifelse(question$Direc == '>=', '<=', '>='))
        expr <- paste('weight', direcsign, median(pokereact$Table$weight))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(isTRUE(sd(pokereact$Table$weight) == 0)) pokereact$Guess <- pokereact$Guess[Column != 'Weight']
      } else if(qtype == 'Height') {
        direcsign <- ifelse(correct, question$Direc, ifelse(question$Direc == '>=', '<=', '>='))
        expr <- paste('height', direcsign, median(pokereact$Table$height))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(isTRUE(sd(pokereact$Table$height) == 0)) pokereact$Guess <- pokereact$Guess[Column != 'Height']
      } else if(qtype == 'Mega') {
        expr <- paste('has_mega ==', ifelse(correct, 1, 0))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        pokereact$Guess <- pokereact$Guess[Column != 'Mega']
      } else if(qtype == 'Generation') {
        expr <- paste0(if(!correct) '!', 'generation_id %in% ', as.numeric(question$Version))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(length(unique(pokereact$Table$generation_id)) == 1) pokereact$Guess <- pokereact$Guess[Column != 'Generation']
      } else if(qtype == 'Colour') {
        expr <- paste0(if(!correct) '!', 'colour %in% "', question$Colour, '"')
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(length(unique(pokereact$Table$colour)) == 1) pokereact$Guess <- pokereact$Guess[Column != 'Colour']
      } else if(qtype == 'Shape') {
        expr <- paste0(if(!correct) '!', 'shape %in% "', question$Shape, '"')
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(length(unique(pokereact$Table$shape)) == 1) pokereact$Guess <- pokereact$Guess[Column != 'Shape']
      } else if(qtype == 'Baby') {
        expr <- paste('is_baby ==', ifelse(correct, 1, 0))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        pokereact$Guess <- pokereact$Guess[Column != 'Baby']
      } else if(qtype == 'EvoStage') {
        expr <- paste0(if(!correct) '!', 'evo_stage %in% ', question$Stage)
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        if(length(unique(pokereact$Table$evo_stage)) == 1) pokereact$Guess <- pokereact$Guess[Column != 'EvoStage']
      } else if(qtype == 'HasEvolution') {
        expr <- paste0('has_evo ==', ifelse(correct, 1, 0))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        pokereact$Guess <- pokereact$Guess[Column != 'HasEvolution']
        if(length(unique(pokereact$Table$evo_stage)) == 1) {
          pokereact$Guess <- pokereact$Guess[!Column %in% c('EvoStage', 'HasEvolved')]
        }
      } else if(qtype == 'HasEvolved') {
        expr <- paste0('is_evolved ==', ifelse(correct, 1, 0))
        pokereact$Table <- pokereact$Table[eval(parse(text = expr))]
        pokereact$Guess <- pokereact$Guess[Column != 'HasEvolved']
        if(length(unique(pokereact$Table$evo_stage)) == 1) {
          pokereact$Guess <- pokereact$Guess[!Column %in% c('EvoStage', 'HasEvolution')]
        }
      } else if(qtype == 'Type') {
        typerows <- sapply(pokereact$Table$type_2 == question$Type | pokereact$Table$type_1 == question$Type, any, na.rm = TRUE)
        if(!correct) typerows <- !typerows
        pokereact$Table <- pokereact$Table[typerows]
        if(!correct) {
          pokereact$Type <- pokereact$Type[target_type != question$Type]
        } else {
          pokereact$Guess <- pokereact$Guess[Column != 'Type']
          pokereact$Type <- pokereact$Type[target_type == question$Type]
        }
      } else if(qtype == 'DualType') {
        if(correct) pokereact$Table <- pokereact$Table[!is.na(type_2)] else pokereact$Table <- pokereact$Table[is.na(type_2)]
        pokereact$Guess <- pokereact$Guess[Column != 'DualType']
      } else if(qtype == 'Egg') {
        eggtype <- question$EggType
        if(correct) {
          pokeids <- pokereact$Eggs[egg_group_name == eggtype, species_id]
          pokereact$Eggs <- pokereact$Eggs[species_id %in% pokeids]
        } else {
          pokeids <- pokereact$Eggs[, all(egg_group_name != eggtype), by = 'species_id'][V1 == TRUE, species_id]
          pokereact$Eggs <- pokereact$Eggs[species_id %in% pokeids]
        }
        pokereact$Table <- pokereact$Table[species_id %in% pokeids]
        if(length(unique(pokereact$Eggs$egg_group_name)) == 1) pokereact$Guess <- pokereact$Guess[Column != 'Egg']
      } else if(qtype == 'TypeEff') {
        goodtypes <- pokereact$Type[damage_type == question$Type]
        typ1match <- goodtypes[match(pokereact$Table$type_1, target_type), damage_factor]
        typ2match <- goodtypes[match(pokereact$Table$type_2, target_type), damage_factor]
        typ2means <- rowMeans(cbind(typ1match, typ2match), na.rm = TRUE)
        typ2means[sapply(typ1match == 0 | typ2match == 0, any, na.rm = TRUE)] <- 0
        typ2means <- recoder::recoder(typ2means, '0:"No Effect";>0 & <100:"Not Very Effective";100:"Effective";>100:"Super Effective"')
        pokereact$Type <- pokereact$Type[damage_type != question$Type]
        if(correct) {
          pokereact$Table <- pokereact$Table[typ2means == question$Effect]
        } else {
          pokereact$Table <- pokereact$Table[typ2means != question$Effect]
        }
      }
    }
    pokereact$QuestInfo <- quest()
  })

  quest <- eventReactive(input$ynenter, {
    qtype <- sample(pokereact$Guess$Column, 1, FALSE)
    question <- switch(
      qtype,
      Weight = {
        direc <- sample(c('lighter', 'heavier'), 1, FALSE)
        direcsign <- ifelse(direc == 'lighter', '<=', '>=')
        list(Question = paste('Is the pokemon', direc, 'than', median(pokereact$Table$weight), 'kg?'), Direc = direcsign)
      },
      Height = {
        direc <- sample(c('smaller', 'taller'), 1, FALSE)
        direcsign <- ifelse(direc == 'smaller', '<=', '>=')
        list(Question = paste('Is the pokemon', direc, 'than', median(pokereact$Table$height), 'm?'), Direc = direcsign)
      },
      Mega = {
        list(Question = 'Can the pokemon mega evolve?')
      },
      Generation = {
        pokevers <- pokereact$Table$generation_id
        pokeverstab <- table(pokevers)
        halfvars <- length(pokevers) / 2
        minversdiff <- min(abs(halfvars - pokeverstab))
        minvers <- names(pokeverstab)[which.min(abs(halfvars - pokeverstab))]
        x <- 1
        while(isTRUE(minversdiff / halfvars >= 0.15 & x <= length(unique(pokevers)) / 2)) {
          x <- x + 1
          pokeverscombs <- combn(unique(pokevers), x)
          pokeversdiff <- apply(pokeverscombs, 2, function(y) sum(pokeverstab[y]))
          minversdiff <- min(abs(halfvars - pokeversdiff))
          minvers <- pokeverscombs[, which.min(abs(halfvars - pokeversdiff))]
        }
        list(Question = paste0('Does the pokemon originate from any of the following version(s): ',
                               paste(minvers, collapse = ', '), '?'), Version = minvers)
      },
      Colour = {
        col <- sample(sort(unique(pokereact$Table$colour)), 1, FALSE, table(pokereact$Table$colour))
        list(Question = paste0('Is the pokemon primarily the colour ', tolower(col), '?'), Colour = col)
      },
      Shape = {
        shape <- sample(sort(unique(pokereact$Table$shape)), 1, FALSE, table(pokereact$Table$shape))
        list(Question = paste0('Would you say that the shape is of a ', shape, '?'), Shape = shape)
      },
      Baby = {
        list(Question = paste0('Is the pokemon a baby pokemon?'))
      },
      EvoStage = {
        evostage <- sample(pokereact$Table$evo_stage, 1, FALSE)
        evostagestr <- switch(evostage, '1' = 'first', '2' = 'second', '3' = 'third')
        list(Question = paste('Is the pokemon the', evostagestr, 'stage of evolution?'), Stage = evostage)
      },
      HasEvolution = {
        list(Question = 'Does the pokemon have an evolution?')
      },
      HasEvolved = {
        list(Question = 'Has the pokemon evolved from another pokemon?')
      },
      Type = {
        typesvec <- c(pokereact$Table$type_1, na.omit(pokereact$Table$type_2))
        types <- sample(typesvec, 1, FALSE)
        list(Question = paste('Is the pokemon of type', tolower(types)), Type = types)
      },
      DualType = {
        list(Question = 'Is the pokemon of dual type?')
      },
      Egg = {
        eggfam <- pokereact$Eggs$egg_group_name
        eggtype <- sample(eggfam, 1, FALSE)
        list(Question = paste('Is the pokemon part of the', tolower(eggtype), 'egg family?'), EggType = eggtype)
      },
      TypeEff = {
        types <- pokereact$Type$damage_type
        typecho <- sample(types, 1, FALSE)
        noeff <- pokereact$Type[damage_eff == 'No Effect' & damage_type == typecho]
        effvec <- c('Effective', 'Super Effective', 'Not Effective', if(nrow(noeff) > 0) 'No Effect')
        effect <- sample(effvec, 1, FALSE)
        list(Question = paste('Is a', typecho, 'attack', effect, 'on the pokemon?'), Type = typecho, Effect = effect)
      },
      list(Question = 'Question not made yet.')
    )
    list(QType = qtype, Question = question)
  })

  output$pokeguess <- renderUI({
    nguesses <- as.numeric(input$nguesses)
    if(nrow(pokereact$Table) > 1 & input$ynenter - pokereact$Question <= nguesses) {
      h4(pokereact$QuestInfo$Question[[1]])
    } else if(nrow(pokereact$Table) == 1 & input$ynenter - pokereact$Question <= nguesses) {
      div(tagList(
        h4('Is this your Pokémon?'),
        img(src = paste0('pokemon-icons/', pokereact$Table$species_id[1], '.png'), height = 75),
        h5(strong(pokereact$Table$name)),
        fluidRow(
          actionButton('finaly', 'Yes', icon('tick')),
          actionButton('finaln', 'No', icon('cross'))
        )
      ), style = 'text-align: center;')
    } else {
      div(
        tagList(
          h4('Unable to guess Pokémon, you win! Press Restart to play another game.'),
          actionButton('restart', 'Restart')
        ), style = 'text-align: center;')
    }
  })

  observeEvent(input$restart, {
    pokereact$PlayScore <- pokereact$PlayScore + 1
    pokereact$Question <- input$ynenter
    pokereact$Table <- poketab
    pokereact$Eggs <- pokeegg
    pokereact$Type <- poketypeff
    pokereact$Guess <- pokeguesstab
    pokereact$QuestInfo <- list(Question = list('Press Enter for First Question'))
  })
  observeEvent(input$finaly, {
    pokereact$CompScore <- pokereact$CompScore + 1
    pokereact$Question <- input$ynenter
    pokereact$Table <- poketab
    pokereact$Eggs <- pokeegg
    pokereact$Type <- poketypeff
    pokereact$Guess <- pokeguesstab
    pokereact$QuestInfo <- list(Question = list('Press Enter for First Question'))
  })
  observeEvent(input$finaln, {
    pokereact$PlayScore <- pokereact$PlayScore + 1
    pokereact$Question <- input$ynenter
    pokereact$Table <- poketab
    pokereact$Eggs <- pokeegg
    pokereact$Type <- poketypeff
    pokereact$Guess <- pokeguesstab
    pokereact$QuestInfo <- list(Question = list('Press Enter for First Question'))
  })

}

shinyApp(ui = ui, server = server)