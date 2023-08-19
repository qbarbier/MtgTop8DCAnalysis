library("shiny")

fluidPage(
  titlePanel("Duel Commander MTG_TOP8 Tier List"),
  mainPanel(width = 12,
    fluidRow(column(12,
      column(8,
        column(2,dateInput("date1","Start Date", value=NA)),
        column(2,dateInput("date2","End Date", value=NA)),
        column(2,numericInput("players","Tourn. Size",value=12)),
        #column(1,tags$br(),checkboxInput("other","Others",value=FALSE)),
        column(3,sliderInput("minPercent","Percent Min",min=0.1, max=10, step=0.1, value=1))
      ),
      column(4, verbatimTextOutput("dataBaseSetting"))
    )),
    fluidRow(
      column(7,
        plotOutput("plotTierList")
      ),
      column(5,
        plotOutput("plotTierList2")
      )
    ),
    fluidRow(column(12,
      plotOutput("plotTierList3")
    ))
  )
)
