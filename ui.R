library(shiny)
library(shiny.semantic)
library(data.table)
library(recoder)
library(DT)

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
            "Here is an effort at making a 20 Question style app based on the world of Pokémon.",
            "Basically think of\n any Pokémon from generations 1 - 7 and the app will ask questions",
            "until there is either one Pokémon left\n and will show you the one it has guessed,",
            "or if it can't guess then you have won."
          ),
          h5(
            "If you are finding that it is guessing it correctly too much for your liking, then to",
            "make it easier for\n you, you can reduce the amount of questions the program can ask before it loses."
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
          class = "ui segment",
          h4(strong(textOutput("questinfo")), style = "text-align: right;"),
          uiOutput("pokeguess"),
          div(radioButtons("yncheck", "", c("Yes", "No", "Not Sure"), inline = TRUE), style = "text-align: center;"),
          div(actionButton("ynenter", "Enter", icon("play")), style = "text-align: center;"),
          selectInput("nguesses", "Number of Questions", seq(5, 20, 5), 20, width = 100),
          tags$button(class = "ui reference button", "Reference Table"),
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
