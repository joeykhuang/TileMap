library(shiny)
library(tidyverse)
library(tibble)
library(superheat)
library(plotly)
library(forcats)
library(DT)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    fileName <- reactive({input$dataset})
    res <- reactive({
      if(!is.null(fileName)){
        read.csv(paste0("datasets/", fileName(), ".csv"))
      }})
    
    
    fold_cat_colors <- c("-2" = "#103D80",
                         "-1.5" = "#3A7EBD",
                         "-0.75" = "#6FA5CC",
                         "-0.25" = "#D7EBF9",
                         "0.25" = "#FFF8E0",
                         "0.75" = "#ffe396",
                         "1.5" = "#E16D54",
                         "2" = "#AA1B1B")
    
    fc_sample_pivot <- reactive({res() %>%
        mutate(val = as.integer(fold > 0) * 2 - 1) %>%
        mutate(fold_cat = case_when(
          fold < -2 ~ -2,
          fold <= -1 ~ -1.5,
          fold <= -0.25 ~ -0.75,
          fold <= 0 ~ -0.25,
          fold <= 0.25 ~ 0.25,
          fold <= 1 ~ 0.75,
          fold <= 2 ~ 1.5,
          fold > 2 ~ 2,
        )) %>%
        mutate(fold_cat = factor(fold_cat,  levels = c("-2", "-1.5", "-0.75", "-0.25", "2", "1.5", "0.75", "0.25")))})
    
    whichplot <- reactiveVal(TRUE)
    observeEvent(input$button, {
      whichplot(!whichplot())
    })
    
    output$tilePlot <- renderPlotly({   
      if (whichplot()) {
        highlight(ggplotly(ggplot(data = highlight_key(fc_sample_pivot(), ~gene), 
                                        aes(y=fct_reorder(condition, val), x=val, fill=fold_cat, text=gene)) +
                                   geom_col(position="stack",alpha=1,color='black',linewidth=0.2) +
                                   #geom_text(aes(label = gene), position = position_stack(vjust = .5), size=3) +
                                   geom_hline(yintercept=0, size=1) +
                                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(),
                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                                   scale_fill_manual(values=fold_cat_colors, name="Fold Change",
                                                     breaks=c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")), tooltip = "text"),
                        on = "plotly_hover", opacityDim=0.3)
      } else {
        highlight(ggplotly(ggplot(data = highlight_key(fc_sample_pivot(), ~condition), 
                                  aes(x=fct_reorder(gene, val), y=val, fill=fold_cat, text=condition)) +
                             geom_col(position="stack",alpha=1,color='black',linewidth=0.2) +
                             geom_hline(yintercept=0, size=1) +
                             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(),
                                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                             scale_fill_manual(values=fold_cat_colors, name="Fold Change",
                                               breaks=c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")), tooltip = "text"),
                  on = "plotly_hover", opacityDim=0.3)
      }
    })
    
    output$table <- DT::renderDataTable(res(),
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}
