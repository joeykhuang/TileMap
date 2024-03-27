library(shiny)
library(plotly)

# Define UI for application that draws a histogram
datasets = fs::path_ext_remove(unique(list.files("datasets/")))
print(datasets)

fluidPage(

    # Application title
    titlePanel("TileMap"),

    # Sidebar with a slider input for number of bins
    verticalLayout(
        
        # Show a plot of the generated distribution
        selectInput("dataset", "Choose Dataset", choices=datasets, selected=datasets[0]),
        
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
