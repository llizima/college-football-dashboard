library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(scales)

dashboardPage(
  dashboardHeader(title = "College Football Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Teams", tabName = "teams", icon = icon("users")),
      menuItem("Game Insights", tabName = "gameinsights", icon = icon("star")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    ),
    sliderInput(
      inputId = "year",
      label = "Select Year",
      min = 2015,  # Change to your data's min year if needed
      max = 2024,  # Change to your data's max year if needed
      value = 2024,
      step = 1,
      sep = ""
    ),
    selectInput(
      inputId = "conference",
      label = "Select Conference",
      choices = NULL
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("mostWinsBox", width = 3),
          valueBoxOutput("mostPointsBox", width = 3),
          valueBoxOutput("topWinPctBox", width = 3),
          valueBoxOutput("topMarginBox", width = 3)
        ),
        fluidRow(
          box(width = 6, title = "Conference Points", status = "primary", solidHeader = TRUE,
              plotlyOutput("overviewBar")
          ),
          box(width = 6, title = "Top Teams by Wins", status = "primary", solidHeader = TRUE,
              DTOutput("summaryTable")
          )
        )
      ),
      tabItem(tabName = "teams",
        fluidRow(
          box(width = 12, title = "Stadium Capacity Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("capacityHist")
          )
        ),
        fluidRow(
          box(width = 12, title = "Stadium Elevation Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("elevationHist")
          )
        ),
        fluidRow(
          box(width = 12, title = "Teams by Time Zone", status = "primary", solidHeader = TRUE,
              plotlyOutput("timezoneBar")
          )
        )
      ),
      tabItem(tabName = "gameinsights",
        fluidRow(
          box(width = 12, title = "Home Win Percentage: Neutral vs Non-Neutral Sites", status = "primary", solidHeader = TRUE,
              plotlyOutput("homeWinPctPlot")
          )
        ),
        fluidRow(
          box(width = 12, title = "Completion Rate Over Time", status = "primary", solidHeader = TRUE,
              plotlyOutput("completionRatePlot")
          )
        )
      ),
      tabItem(tabName = "map",
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin-bottom: 20px;",
              uiOutput("noTeamsMsg")
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Map", status = "primary", solidHeader = TRUE,
              leafletOutput("mapPlot", height = 600)
          )
        ),
        fluidRow(
          column(
            width = 12,
            sliderInput("map_elevation", "Stadium Elevation (meters)",
                        min = 0, max = 2500, value = c(0, 2500), step = 10),
            sliderInput("map_winrate", "Minimum Win Percentage (%)",
                        min = 0, max = 100, value = 0, step = 1),
            sliderInput("map_capacity", "Stadium Capacity",
                        min = 0, max = 110000, value = c(0, 110000), step = 1000)
          )
        )
      )
    )
  )
)