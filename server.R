library(shiny)
library(data.table)
library(recoder)
library(DT)

#### Server####
function(input, output, session) {
  pokereact <- reactiveValues(
    poke_dt = poketab, poke_egg_dt = pokeegg, poke_type_dt = poketypeff, poke_gss_dt = pokeguesstab,
    CompScore = 0, PlayScore = 0, Question = 0, QuestInfo = list()
  )

  output$questinfo <- renderText(paste(pokereact$Question))
  output$compscore <- renderText(pokereact$CompScore)
  output$playscore <- renderText(pokereact$PlayScore)

  observeEvent(pokereact$Question, {
    # Load data locally
    pokeref <- pokereact$poke_dt
    pokeeggs <- pokereact$poke_egg_dt
    pokeguess <- pokereact$poke_gss_dt
    poketypes <- pokereact$poke_type_dt

    if (pokereact$Question > 1 & isTRUE(input$yncheck != "Not Sure") & nrow(pokeref) > 0) {
      correct <- isTRUE(input$yncheck == "Yes")
      question <- pokereact$QuestInfo$Question
      qtype <- pokereact$QuestInfo$QType
      qinfo <- pokereact$QuestInfo$Question

      # Reduce the table down depending on type
      pokeref <- PokeAnswer(correct, qtype, qinfo, pokeref, pokeeggs, poketypes)
      pokeguess <- PokeGuess(pokeguess, pokeref, pokeeggs, poketypes)

      # Special ones for Types and Eggs
      if (qtype == "Type") {
        if (correct) poketypes <- poketypes[target_type == qinfo$Type] else poketypes <- poketypes[target_type != qinfo$Type]
      }
      if (qtype == "TypeEff") {
        goodtypes <- poketypes[damage_eff == qinfo$Effect & damage_type == qinfo$Type, target_type]
        if (correct) poketypes <- poketypes[target_type %in% goodtypes] else poketypes <- poketypes[!target_type %in% goodtypes]
      }
      poketypes <- poketypes[target_type %in% pokeref$type_1 | target_type %in% pokeref$type_2]

      if (qtype == "Egg") {
        if (correct) {
          pokeeggs <- pokeeggs[species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]]
        } else {
          pokeeggs <- pokeeggs[!species_id %in% pokeeggs[egg_group_name == qinfo$EggType, species_id]]
        }
      }
      pokeeggs <- pokeeggs[species_id %in% pokeref$species_id]

      # Add back to reractive pokereact
      pokereact$poke_gss_dt <- pokeguess
      pokereact$poke_dt <- pokeref
      pokereact$poke_egg_dt <- pokeeggs
      pokereact$poke_type_dt <- poketypes
    }
    pokereact$QuestInfo <- PokeQuestion(pokeguess, pokeref, pokeeggs, poketypes)
  })

  zero_guess_ui <- tagList(
    tags$button(
      id = "zero_guess_button",
      class = "ui massive circular icon button action-button",
      style = "font-size: 4em;",
      tags$i(class = "play icon")
    )
  )

  observeEvent(input$zero_guess_button, pokereact$Question <- 1)
  observeEvent(input$ynenter, pokereact$Question <- pokereact$Question + 1)

  output$pokeguess <- renderUI({
    if (pokereact$Question == 0) return(zero_guess_ui)

    nguesses <- as.numeric(input$nguesses)
    if (nrow(pokereact$poke_dt) > 1 & pokereact$Question <= nguesses) {
      tagList(
        h4(pokereact$QuestInfo$Question[[1]]),
        div(radioButtons("yncheck", "", c("Yes", "No", "Not Sure"), inline = TRUE), style = "text-align: center;"),
        tags$button(id = "ynenter", style = "text-align: center;", class = "ui button action-button", "Enter", tags$i(class = "play icon"))
      )
    } else if (nrow(pokereact$poke_dt) == 1 & pokereact$Question <= nguesses) {
      div(tagList(
        h4("Is this your Pokémon?"),
        img(src = paste0("pokemon-icons/", pokereact$poke_dt$species_id[1], ".png"), height = 100),
        h5(strong(pokereact$poke_dt$name)),
        fluidRow(
          tags$button(id = "finaly", class = "ui button action-button", "Yes", tags$i(class = "check icon")),
          tags$button(id = "finaln", class = "ui button action-button", "No", tags$i(class = "times icon"))
        )
      ), style = "text-align: center;")
    } else {
      div(
        tagList(
          h4("Unable to guess Pokémon, you win! Press Restart to play another game."),
          actionButton("restart", "Restart")
        ),
        style = "text-align: center;"
      )
    }
  })

  observeEvent(input$restart, {
    pokereact$PlayScore <- pokereact$PlayScore + 1
    pokereact$Question <- 0
    pokereact$poke_dt <- poketab
    pokereact$poke_egg_dt <- pokeegg
    pokereact$poke_type_dt <- poketypeff
    pokereact$poke_gss_dt <- pokeguesstab
  })
  observeEvent(input$finaly, {
    pokereact$CompScore <- pokereact$CompScore + 1
    pokereact$Question <- 0
    pokereact$poke_dt <- poketab
    pokereact$poke_egg_dt <- pokeegg
    pokereact$poke_type_dt <- poketypeff
    pokereact$poke_gss_dt <- pokeguesstab
  })
  observeEvent(input$finaln, {
    pokereact$PlayScore <- pokereact$PlayScore + 1
    pokereact$Question <- 0
    pokereact$poke_dt <- poketab
    pokereact$poke_egg_dt <- pokeegg
    pokereact$poke_type_dt <- poketypeff
    pokereact$poke_gss_dt <- pokeguesstab
  })

  output$IndexTable <- DT::renderDataTable({
    dt_names <- c("ID", "Name", "Sprite", "Height (m)", "Weight (kg)", "Egg Group", "Type 1", "Type 2")
    dt_tab <- poketab[, .(species_id, name, height, weight, type_1, type_2)]
    dt_tab[, sprite := paste0('<img src="pokemon-icons/', species_id, '.png", height=52></img>')]
    egg_tab <- pokeegg[, list(egg_group = paste(egg_group_name, collapse = ", ")), by = species_id]
    dt_tab <- dt_tab[egg_tab, on = "species_id"]
    setcolorder(dt_tab, c("species_id", "name", "sprite", "height", "weight", "egg_group", "type_1", "type_2"))

    DT::datatable(
      dt_tab, colnames = dt_names, escape = FALSE, rownames = FALSE, filter = "top",
      options = list(scrollX = TRUE, dom = "tip")
    )
  })
}
