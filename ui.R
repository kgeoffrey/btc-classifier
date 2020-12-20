
library(shiny)

shinyUI(

fluidPage(
  mainPanel(
    fluidRow(
      align = "center",
      class = "myRow1",
      column(
        12,
        div(style = "height:10vh; margin-top:5vh;background-color: transparent;font-size:4vh;",
            "Should you buy Bitcoin NOW?"
        ),
        align = "center"
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "height:10vh; margin-top: 0px;background-color: transparent;font-size:6vh;",
            htmlOutput("pred")
        ),
        align = "center"

      )
    ),
    fluidRow(
      #column(2),
      column(
        12,
        align = "center",
        plotOutput("scatterplot", width = "85%", height = "40vh")
      ),
    ),

    width = 12,


    fluidRow(
      column(
        12,
        div(style = "height:5vh; margin-top: 0px;background-color: transparent;font-size:3vh;",
            "Prediction Accuracy"
        ),
        align = "center"
      )
    ),
    fluidRow(
      column(
        12,
        div(style = "height:3vh; margin-top: 0px;background-color: transparent;font-size:3vh;",
            htmlOutput("accuracy")
        ),
        align = "center"
      )
    ),
    fluidRow(
      align = "center",
      class = "myRow1",
      column(
        12,
        div(style = "height:9vh; margin-top: 5vh;background-color: transparent;font-size:3vh;",
            tags$a("Click for Source Code", href="https://github.com/kgeoffrey/btc-classifier")
        ),
        align = "bottom"
      )
    ),
    fluidRow(
      align = "center",
      class = "myRow1",
      column(
        12,
        div(style = "height:3vh;background-color: transparent;font-size:2vh;",
            "© Geoffrey Kasenbacher 2020"
        )
      )
    )
  )
)

)
