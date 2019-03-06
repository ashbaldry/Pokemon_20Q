library(shiny)
library(data.table)
library(recoder)
library(DT)

####Server####
server <- function(input, output, session) {
    pokereact <- reactiveValues(Table = poketab, Eggs = pokeegg, Type = poketypeff, Guess = pokeguesstab,
                                CompScore = 0, PlayScore = 0, Question = 0,
                                QuestInfo = list(Question = list('Press Enter to start new game')))

    output$questinfo <- renderText(paste0(input$ynenter - pokereact$Question, ' / ', input$nguesses))
    output$compscore <- renderText(pokereact$CompScore)
    output$playscore <- renderText(pokereact$PlayScore)

    observeEvent(input$ynenter, {
        #Load data locally
        pokeref <- pokereact$Table
        pokeeggs <- pokereact$Eggs
        pokeguess <- pokereact$Guess
        poketypes <- pokereact$Type

        if(input$ynenter > 1 & input$ynenter - pokereact$Question > 1 & input$yncheck != 'Not Sure' & nrow(pokeref) > 0) {
            correct <- isTRUE(input$yncheck == 'Yes')
            question <- pokereact$QuestInfo$Question
            qtype <- pokereact$QuestInfo$QType
            qinfo <- pokereact$QuestInfo$Question

            #Reduce the table down depending on type
            pokeref <- PokeAnswer(correct, qtype, qinfo, pokeref, pokeeggs, poketypes)
            pokeguess <- PokeGuess(pokeguess, pokeref, pokeeggs, poketypes)

            #Special ones for Types and Eggs
            if(qtype == 'Type') {
                if(correct) poketypes <- poketypes[target_type == qinfo$Type] else poketypes <- poketypes[target_type != qinfo$Type]
            }
            if(qtype == 'TypeEff') {
                goodtypes <- poketypes[damage_eff == qinfo$Effect & damage_type == qinfo$Type, target_type]
                if(correct) poketypes <- poketypes[target_type %in% goodtypes] else poketypes <- poketypes[!target_type %in% goodtypes]
            }
            poketypes <- poketypes[target_type %in% pokeref$type_1 | target_type %in% pokeref$type_2]

            if(qtype == 'Egg') {
                if(correct) {
                    pokeeggs <- pokeeggs[species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]]
                } else {
                    pokeeggs <- pokeeggs[!species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]]
                }
            }
            pokeeggs <- pokeeggs[species_id %in% pokeref$species_id]

            #Add back to reractive pokereact
            pokereact$Guess <- pokeguess
            pokereact$Table <- pokeref
            pokereact$Eggs <- pokeeggs
            pokereact$Type <- poketypes
        }
        pokereact$QuestInfo <- PokeQuestion(pokeguess, pokeref, pokeeggs, poketypes)
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
                    actionButton('finaly', 'Yes', icon('check')),
                    actionButton('finaln', 'No', icon('times'))
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
        pokereact$QuestInfo <- list(Question = list('Press Enter to start new game'))
    })
    observeEvent(input$finaly, {
        pokereact$CompScore <- pokereact$CompScore + 1
        pokereact$Question <- input$ynenter
        pokereact$Table <- poketab
        pokereact$Eggs <- pokeegg
        pokereact$Type <- poketypeff
        pokereact$Guess <- pokeguesstab
        pokereact$QuestInfo <- list(Question = list('Press Enter to start new game'))
    })
    observeEvent(input$finaln, {
        pokereact$PlayScore <- pokereact$PlayScore + 1
        pokereact$Question <- input$ynenter
        pokereact$Table <- poketab
        pokereact$Eggs <- pokeegg
        pokereact$Type <- poketypeff
        pokereact$Guess <- pokeguesstab
        pokereact$QuestInfo <- list(Question = list('Press Enter to start new game'))
    })

    output$IndexTable <- DT::renderDataTable({
        DT::datatable(
            data.table(`Pokemon ID` = poketab$species_id,
                       Name = poketab$name,
                       Sprite = paste0('<img src="pokemon-icons/', poketab$species_id, '.png", height=52></img>'),
                       `Height (m)` = poketab$height,
                       `Weight (kg)` = poketab$weight,
                       `Egg Types` = pokeegg[, paste(egg_group_name, collapse = ', '), by = 'species_id'][[2]],
                       `Type 1` = poketab$type_1,
                       `Type 2` = poketab$type_2
            ),
            escape = FALSE, rownames = FALSE, filter = 'top', options = list(scrollX = TRUE, dom = 'tip')
        )
    })

}