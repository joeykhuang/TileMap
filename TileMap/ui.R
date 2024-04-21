library(shiny)
library(plotly)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboard)
library(shinyjqui)

# Define UI for application that draws a histogram
datasets = fs::path_ext_remove(unique(list.files("datasets/")))

button_color_css <- "
  #DivCompClear, #FinderClear, #EnterTimes{
  /* Change the background color of the update button
  to blue. */
  background: DodgerBlue;
  
  /* Change the text size to 15 pixels. */
  font-size: 15px;
}"

tweaks <-
  list(tags$head(tags$style(
    HTML(
      ".multicol {
         -webkit-column-count: 3; /* Chrome, Safari, Opera */
         -moz-column-count: 3;    /* Firefox */
         column-count: 3;
       }"
    )
  )))

ui <- fluidPage(tweaks,
                #Navbar structure for UI
                navbarPage(
                  "TileMap",
                  theme = shinytheme("journal"),
                  tabPanel(
                    "TileMap",
                    fluid = TRUE,
                    icon = icon("boxes-stacked"),
                    tags$style(button_color_css),
                    # Sidebar layout with a input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        titlePanel("Data Selection"),
                        # Select Event
                        selectInput(
                          inputId = "dataset",
                          label = "Choose Dataset",
                          choices = datasets,
                          selected = "Jorstad",
                          width = "220px"
                        ),
                        fluidRow(column(
                          width = 12,
                          tags$label("Select Condition(s):"),
                          actionButton('selectall','Select All', style='padding:6px; font-size:80%'),
                          actionButton('selectnone','Select None', style='padding:6px; font-size:80%'),
                          br(),
                          br(),
                          tags$div(
                            align = 'left',
                            class = "multicol",
                            uiOutput("conditionSelection")
                          )
                        )),
                        hr(),
                        titlePanel("Filtering"),
                        fluidRow(column(width = 9, uiOutput("filterBy"))),
                        fluidRow(column(width = 9, uiOutput("filterConds"))),
                        fluidRow(column(
                          width = 9,
                          actionButton(inputId = "regen", label = "Regenerate Plot!")
                        )),
                        hr(),
                        titlePanel("Plot Controls"),
                        fluidRow(column(
                          width = 9,
                          tags$div(
                            materialSwitch(
                              inputId = "plotByButton",
                              label = "By Condition",
                              inline = TRUE
                            ),
                            tags$span("By Gene")
                          )
                        )),
                        fluidRow(column(
                          width = 6,
                          materialSwitch(inputId = "plotOutline",
                                         label = "Remove Outline?")
                        ),
                        column(width = 4,
                               materialSwitch(inputId = "plotText",
                                              label = "Text?"))), 
                        fluidRow(column(width = 9, uiOutput("groupBy"))),
                        width = 3),
                      mainPanel(fluidRow(withSpinner(
                        plotlyOutput("tilePlot", height = "60vh")
                      )),
                      hr(),
                      fluidRow(withSpinner(
                        DT::dataTableOutput(outputId = "table")
                      ))
                      , width = 9)
                    )
                  ),
                  tabPanel(
                    "Heatmap",
                    icon = icon("grip-vertical"),
                    sidebarLayout(
                      sidebarPanel(
                        titlePanel("Data Selection"),
                        # Select Event
                        selectInput(
                          inputId = "HeatmapDataset",
                          label = "Choose Dataset",
                          choices = datasets,
                          selected = "Jorstad",
                          width = "220px"
                        ), width = 3),
                    mainPanel(fluidRow(plotOutput("heatmapPlot", height = "95vh")), width = 9))
                  ),
                  tabPanel(
                    "About",
                    icon = icon("info-circle"),
                    fluidRow(column(6,
                                    h4(
                                      p("About the Project")
                                    ),
                                    column(6,
                                           h4(
                                             p("About the Authors")
                                           ),
                                           br())),
                             br(),
                             hr(),
                             h5("Sources:"),
                             
                             h5(
                               "Built with",
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                               "."
                             )
                    )
                  ))
)