library(shiny)
library(shiny.semantic)
library(data.table)
library(recoder)
library(DT)

#### Function ####
inline_dropdown <- function(name, choices, choices_value = choices, default_text = "Select", value = NULL) {
  unique_dropdown_class <- paste0("dropdown_name_", name)
  class <- paste("ui inline basic dropdown", unique_dropdown_class)
  shiny::tagList(
    shiny::span(
      class = class,
      shiny_text_input(name, shiny::tags$input(type = "hidden", name = name), value = value),
      shiny::div(class = "default text", default_text),
      uimenu(purrr::map2(choices, choices_value, ~menu_item(`data-value` = .y, .x))),
      uiicon("dropdown")
    ),
    shiny::tags$script(paste0("$('.ui.dropdown.", unique_dropdown_class, "').dropdown().dropdown('set selected', '", value, "');"))
  )
}

#### UI ####
semanticPage(
  title = "Pokémon 20 Questions",
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  suppressDependencies("bootstrap"),

  #### Header ####
  div(img(src = "logo.png", width = "50%", height = "15%"),
    style = "text-align: center;",

    #### Info ####
    span(
      tags$button(
        class = "right floated circular ui info icon button", style = "margin: 50px;",
        tags$i(class = "info icon")
      ),
      div(
        class = "ui info modal",
        div(class = "header", "Info"),
        div(
          class = "content",
          h5(
            "Welcome to the Pokémon 20 Question app. This works in the same way as a standard 20 Question but in the",
            "world of Pokémon. You can select any one of the 721 monsters from Generations 1-6 and go through each",
            "question, answering yes/no/don't know until there is either one Pokémon left\n and will show you the one it has guessed,",
            "or if it can't guess in time then you have won."
          ),
          h5(
            "If you are finding that it is guessing it correctly too much for your liking, then to",
            "make it easier for\n you, you can reduce the amount of questions the app can ask."
          ),
          h6(
            "Source code is available on",
            a("GitHub.", href = "https://github.com/ashbaldry/Pokemon_20Q", target = "_blank"),
            "All data and images have been retreived from",
            a("veekun.com", href = "https://veekun.com", target = "_blank")
          )
        )
      ),
      tags$script("$('.info.modal').modal('attach events', '.info.button', 'show');")
    )
  ),

  #### Main ####
  div(
    class = "ui center aligned grid",
    div(
      class = "row",

      #### User Score ####
      div(class = "one wide column"),
      div(
        class = "two wide column",
        div(
          class = "ui inverted circular segment", style = "padding: 50px 60px;",
          h2(class = "ui header", style = "text-align: center;", "Player"),
          h1(class = "sub header", style = "text-align: center;", textOutput("playscore"))
        )
      ),
      div(class = "one wide column"),

      #### Quiz ####
      div(
        class = "eight wide column",
        div(
          class = "ui clearing segment",
          h4(
            style = "text-align: right;",
            strong(textOutput("questinfo", inline = TRUE)), "/",
            inline_dropdown("nguesses", choices = seq(5, 20, 5), value = 20)
          ),
          uiOutput("pokeguess"),
          tags$br(),
          tags$button(class = "ui right floated reference button", "Reference Table"),
          div(
            class = "ui reference modal",
            div(class = "header", "Pokémon Reference"),
            div(class = "content", DT::dataTableOutput("IndexTable"))
          ),
          tags$script("$('.reference.modal').modal('attach events', '.reference.button', 'show');"),
          tags$script("$('.reference.button').on('click', function() {$('#IndexTable').show().trigger('shown');});")
        )
      ),

      #### Computer Score ####
      div(class = "one wide column"),
      div(
        class = "two wide column",
        div(
          class = "ui circular segment", style = "padding: 50px 40px;",
          h2(class = "ui header", style = "text-align: center;", "Computer"),
          h1(class = "sub header", style = "text-align: center;", textOutput("compscore"))
        )
      ),
      div(class = "one wide column")
    )
  )
)
