library(shiny)
library(tidyverse)
library(tibble)
library(superheat)
library(plotly)
library(forcats)

# Define server logic required to draw a histogram
function(input, output, session) {
    fold_cat_colors <- c("-2" = "#103D80",
                         "-1.5" = "#3A7EBD",
                         "-0.75" = "#6FA5CC",
                         "-0.25" = "#D7EBF9",
                         "0.25" = "#FFF8E0",
                         "0.75" = "#ffe396",
                         "1.5" = "#E16D54",
                         "2" = "#AA1B1B")
    
    fc_sample_pivot$fold_cat <- factor(fc_sample_pivot$fold_cat, levels = c("-2", "-1.5", "-0.75", "-0.25", "2", "1.5", "0.75", "0.25"))
    fc_sample_pivot$highlight <- as.factor(ifelse(fc_sample_pivot$gene == "GUF1", 1, 0.9))
    print(fc_sample_pivot$highlight)
    fc_sample_pivot <- fc_sample_pivot[order(abs(fc_sample_pivot$fold),decreasing=F),]
    #fc_sample_pivot_plot <- fc_sample_pivot[fc_sample_pivot$condition %in% selected_conditions, ]
    #fc_sample_pivot_plot <- fc_sample_pivot
    
    fc_sample_pivot_plot <- highlight_key(fc_sample_pivot, ~gene)
    
    whichplot <- reactiveVal(FALSE)
    observeEvent(input$button, {
      whichplot(!whichplot())
    })
    
    conditionPlot <- highlight(ggplotly(ggplot(data = highlight_key(fc_sample_pivot, ~gene), 
                        aes(y=fct_reorder(condition, val), x=val, fill=fold_cat, text=gene)) +
                          geom_col(position="stack",alpha = 1, linewidth=0.2, color='black') + 
                          geom_vline(xintercept=0, size=1) +
                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank()) +
                          scale_fill_manual(values=fold_cat_colors, name="Fold Change", 
                                            breaks=c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")), tooltip = "text"), 
                        on = "plotly_hover", opacityDim=0.3)
    
    genePlot <- highlight(ggplotly(ggplot(data = highlight_key(fc_sample_pivot, ~condition), 
                                aes(x=fct_reorder(gene, val), y=val, fill=fold_cat, text=condition)) +
                           geom_col(position="stack",alpha=1,color='black',linewidth=0.2) + 
                           geom_hline(yintercept=0, size=1) +
                           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(),
                                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                           scale_fill_manual(values=fold_cat_colors, name="Fold Change", 
                                             breaks=c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")), tooltip = "text"),
                          on = "plotly_hover", opacityDim=0.3)
    
    which_graph <- reactive({
      if (whichplot()) {
        genePlot
      } else {
        conditionPlot
      }
    })
    
    output$tilePlot <- renderPlotly({   
      which_graph()
    }) 
}
