library(shiny)
library(plotly)

# Define UI for application that draws a histogram
choices = unique(fc_sample_pivot$condition)

fluidPage(

    # Application title
    titlePanel("TrendMap"),

    # Sidebar with a slider input for number of bins
    verticalLayout(
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("tilePlot"),
            width=10
        ),
        
        actionButton("button", "Switch View"),

        # wellPanel(
        #   actionLink("selectall","Select All"),
        #   checkboxGroupInput("conds",
        #               "Conditions:",
        #               choices,
        #               inline=TRUE,
        #               selected=choices)
        # ),
        # 
    )
)
