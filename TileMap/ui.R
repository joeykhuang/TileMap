library(shiny)
library(plotly)
library(shinythemes)
library(shinycssloaders)

# Define UI for application that draws a histogram
datasets = fs::path_ext_remove(unique(list.files("datasets/")))
print(datasets)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# 
# fluidPage(
# 
#     # Application title
#     titlePanel("TileMap"),
# 
#     # Sidebar with a slider input for number of bins
#     verticalLayout(
#         
#         # Show a plot of the generated distribution
#         selectInput("dataset", "Choose Dataset", choices=datasets, selected=datasets[0]),
#         
#         mainPanel(
#             plotlyOutput("tilePlot"),
#             width=10
#         ),
#         
#         actionButton("button", "Switch View"),
# 
#         # wellPanel(
#         #   actionLink("selectall","Select All"),
#         #   checkboxGroupInput("conds",
#         #               "Conditions:",
#         #               choices,
#         #               inline=TRUE,
#         #               selected=choices)
#         # ),
#         # 
#     )
# )

ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("TileMap", theme = shinytheme("journal"),
             tabPanel("Data", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Data Selection"),
                          # Select Event
                          selectInput(inputId = "dataset",
                                      label = "Choose Dataset",
                                      choices = datasets,
                                      selected = datasets[3],
                                      width = "220px"
                          ),
                          fluidRow(
                          column(9, offset = 0,
                                 # Select which Region(s) to plot
                                 checkboxGroupInput(inputId = "ConditionFilter",
                                                    label = "Select Condition(s):",
                                                    choices = c(),
                                                    selected = "")
                          )),
                          actionButton("button", "Switch View"),
                          hr(),
                          titlePanel("Filtering"),
                        ),
                        mainPanel(
                          # fluidRow(
                          #   column(3, offset = 9,
                          #          radioButtons(inputId = "show_NamesFinder",
                          #                       label = "Display:",
                          #                       choices = c("School Names", "City Names", "Neither"),
                          #                       selected = "School Names")
                          #   )),
                          # hr(),
                          withSpinner(plotlyOutput("tilePlot")),
                          hr(),
                          fluidRow(column(7,
                                          helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                                          #actionButton(inputId = "draw", label = "Input Event and Times")
                                          
                          ),
                          column(width = 2, offset = 2, conditionalPanel(
                            condition = "output.schoolstableFinder",
                            actionButton(inputId = "FinderClear", label = "Clear Table")))),
                          br(),
                          fluidRow(
                            withSpinner(DT::dataTableOutput(outputId = "table"))))
                      )
             ),
             
             tabPanel("Compare", fluid = TRUE, icon = icon("swimmer"),
                      titlePanel("Program Comparisons"),
                      fluidRow(
                        column(6,
                               selectizeInput(inputId = "SchoolSelectA",
                                              label = "Select Schools (Max 4)",
                                              choices = unique(conditions),
                                              multiple = TRUE,
                                              options = list(maxItems = 4, placeholder = 'Enter school name',
                                                             onInitialize = I('function() { this.setValue(""); }'))
                               ),
                               selectInput(inputId = "SchoolCompRace",
                                           label = "Select Event",
                                           choices = levels(conditions),
                                           selected = "50 Free"),
                               helpText("Select school and event to create plots")
                        ),
                        column(6,
                               checkboxGroupInput(inputId = "SchoolCompGender",
                                                  label = "Select Gender(s):",
                                                  choices = c("Male" = "M", "Female" = "F"),
                                                  selected = "M"),
                               radioButtons(inputId = "TuitionType",
                                            label = "Use In-State Tution?",
                                            choices = c("Yes", "No"),
                                            selected = "Yes"),
                               helpText("Note: In-state tuition will only apply to public schools")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(6,
                               withSpinner(plotOutput(outputId = "SchoolCompPlotEvent"
                                                      # brush = "brush_SchoolComp"
                               )),
                               br(),
                               dataTableOutput(outputId = "SchoolCompDT")
                        ),
                        column(6,
                               dataTableOutput(outputId = "SchoolCompStats"),
                               helpText("For more information on school types and US News rankings please see More > About > School Types & Rankings")
                        )
                      )
             ),
             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel("School Types & Rankings", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("School Types")),
                                          h5(p("US News and World Report uses four categories of schools for their rankings system:"),
                                             p("National universities are those that offer a “full range” of undergraduate majors, while also offering graduate programs, including at the doctoral level.  Intercollegiate sports, including swimming, are generally pursued by undergrads, or occasionally students in master’s degree programs, so a university having nor not having doctoral programs isn’t directly relevant.  That said, doctoral programs and faculty research go hand-in-hand, so faculty at national universities are nearly always active in research, in addition to their teaching duties.  National universities are usually, though not always, large.  Most state flagship universities would fall under this category."),
                                             p("Regional universities are similar to national universities in that they have a wide range of undergrad programs, and some master’s programs as well.  They generally do not have large doctoral programs, and correspondingly less faculty research."),
                                             p("National liberal arts colleges are undergraduate focused, with few graduate programs.  They award the majority of their degrees in arts and sciences, and may or may not have other undergraduate programs, like engineering or professional studies."),
                                             p("Regional colleges are also undergraduate focused institutions, but do not award the majority of their degrees in arts and/or sciences.  These colleges may have a particular focus, like technology or agriculture, or they may be primarily two year institutions that also grant some four year degrees.")
                                          )
                                   ),
                                   column(6,
                                          h4(p("US News Rankings")),
                                          h5(p("Every year the US News and World Report issues a set of rankings for US colleges and universities.  They are a used in this setting as a guideline, and a general comparative device, but can often be misinterpreted or overvalued.  The major component of a given school’s rankings are graduation and retention rates, academic reputation (basically name recognition), and faculty resources (class size, faculty salary etc.).  Each school is given a score, and then placed in order.  That said the scored differences between schools of different rank can be quite small, so take the rankings with a grain of salt.
                                    The full methodology for the US News and World report college rankings can be found ",
                                               a("here.",
                                                 href = "https://www.usnews.com/education/best-colleges/articles/ranking-criteria-and-weights"))
                                          )
                                   ))
                                 
                        ),
                        
                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("This project is intended to facilitate useful comparisons between colleges in the NCAA, based on swimming performance, location, and academic information.  Here a prospective student-athlete, or anyone else with an interest can find schools fitting a particular set of criterion relevant to them, for example, schools close to home, with times in a particular range, and of a specified academic profile.")),
                                          br(),
                                          h5(p("The project began as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below.")),
                                          br(),
                                          h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at gpilgrim2607@gmail.com"),
                                             p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/gpilgrim2670/SwimMap"), "."))
                                          
                                          #hr(),
                                          
                                   ),
                                   column(6,
                                          #br(),
                                          #             HTML('<img src="GregPicCrop.png", height="110px"
                                          # style="float:right"/>','<p style="color:black"></p>'),
                                          h4(p("About the Author")),
                                          h5(p("Greg is a former collegiate swimmer.  After completing his undergrad degree he joined USMS, earned a PhD in chemistry, and began officiating swimming at the high school level.  He now swims with his local USMS team and serves as an official in USA Swimming while also working as an engineer.  He is the author the", a("SwimmeR package", href = "https://github.com/gpilgrim2670/SwimmeR"), "for working with swimming results in the R environment."),
                                             p("For more work with swimming and R see Greg's articles at ", a("Swimming + Data Science", href = 'https://pilgrim.netlify.app/'), "."),
                                             
                                          ),
                                          HTML('<img src="GregPicCrop.png", height="200px"'),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Sources:"),
                                 h6(
                                   p("Swimming Information from ",
                                     a("USA Swimming",
                                       href = "https://www.usaswimming.org/Home/times/ncaa-information"))),
                                 h6(
                                   p("US News College Rankings from ",
                                     a("US News",
                                       href = "https://www.usnews.com/best-colleges/rankings"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    "by",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                    ".")
                        )
             )
  )
)
