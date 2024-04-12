library(shiny)
library(tidyverse)
library(tibble)
library(superheat)
library(plotly)
library(forcats)
library(DT)
library(shinyjs)

# Define server logic required to draw a histogram
function(input, output, session) {
  fileName <- reactive({
    input$dataset
  })
  res <- reactive({
    if (!is.null(fileName)) {
      read.csv(paste0("datasets/", fileName(), ".csv"))
    }
  })
  
  res_selected <- eventReactive(input$regen,
                                {
                                  if (input$regen > 0) {
                                    res_ <- res() %>% filter(condition %in% input$condSelect)
                                    if (input$filterID != "") {
                                      if (class(input$filterCondID) %in% c("character", "logical")) {
                                        res_ %>% filter(!!rlang::sym(input$filterID) %in% input$filterCondID)
                                      } else if (class(input$filterCondID) == "numeric") {
                                        res_ %>% filter(between(
                                          !!rlang::sym(input$filterID),
                                          input$filterCondID[1],
                                          input$filterCondID[2]
                                        ))
                                      }
                                      
                                    } else {
                                      res_
                                    }
                                  } else {
                                    res()
                                  }
                                }, ignoreNULL = FALSE)
  
  reactive(print(res_selected()))
  
  fold_cat_colors <- c(
    "-2" = "#103D80",
    "-1.5" = "#3A7EBD",
    "-0.75" = "#6FA5CC",
    "-0.25" = "#D7EBF9",
    "0.25" = "#FFF8E0",
    "0.75" = "#ffe396",
    "1.5" = "#E16D54",
    "2" = "#AA1B1B"
  )
  
  fc_sample_pivot <- reactive({
    res_selected() %>%
      mutate(val = as.integer(fold > 0) * 2 - 1) %>%
      mutate(
        fold_cat = case_when(
          fold < -2 ~ -2,
          fold <= -1 ~ -1.5,
          fold <= -0.25 ~ -0.75,
          fold <= 0 ~ -0.25,
          fold <= 0.25 ~ 0.25,
          fold <= 1 ~ 0.75,
          fold <= 2 ~ 1.5,
          fold > 2 ~ 2,
        )
      ) %>%
      mutate(fold_cat = factor(
        fold_cat,
        levels = c("-2", "-1.5", "-0.75", "-0.25", "2", "1.5", "0.75", "0.25")
      ))
  })
  
  cdata <- session$clientData
  
  output$tilePlot <- renderPlotly({
    if (!input$button) {
      highlight(
        ggplotly(
          ggplot(
            data = highlight_key(fc_sample_pivot(), ~ gene),
            aes(
              y = fct_reorder(condition, val, .na_rm = TRUE),
              x = val,
              fill = fold_cat,
              text = gene
            )
          ) +
            geom_col(
              position = "stack",
              alpha = 1,
              color = 'black',
              linewidth = 0.2,
              na.rm = TRUE
            ) +
            #geom_text(aes(label = gene), position = position_stack(vjust = .5), size=3) +
            geom_vline(xintercept = 0, size = 1) +
            xlab("# of Changed Genes") +
            ylab("Conditions") +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()
            ) +
            scale_fill_manual(
              values = fold_cat_colors,
              name = "Fold Change",
              breaks = c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")
            ),
          tooltip = "text",
          width = cdata$output_pid_width,
          height = cdata$output_pid_height
        ) %>% config(displayModeBar = FALSE),
        on = "plotly_click",
        off = "plotly_doubleclick",
        opacityDim = 0.3,
        selected = attrs_selected(showlegend = FALSE)
      )
    } else {
      highlight(
        ggplotly(
          ggplot(
            data = highlight_key(fc_sample_pivot(), ~ condition),
            aes(
              x = fct_reorder(gene, val, .na_rm = TRUE),
              y = val,
              fill = fold_cat,
              text = condition
            )
          ) +
            geom_col(
              position = "stack",
              alpha = 1,
              color = 'black',
              linewidth = 0.2,
              na.rm = TRUE
            ) +
            geom_hline(yintercept = 0, size = 1) +
            xlab("Genes") +
            ylab("# of Changed Conditions") +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
              ),
              legend.position = 'none'
            ) +
            scale_fill_manual(
              values = fold_cat_colors,
              name = "Fold Change",
              breaks = c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")
            ),
          tooltip = "text",
          width = cdata$output_pid_width,
          height = cdata$output_pid_height
        ) %>% config(displayModeBar = FALSE),
        on = "plotly_click",
        off = "plotly_doubleclick",
        opacityDim = 0.3,
        selected = attrs_selected(showlegend = FALSE)
      )
    }
  })
  
  
  allChoices <- reactive(sort(unique(res()$condition)))
  
  observeEvent(input$selectnone, {
    updateCheckboxGroupInput(session,
                             'condSelect',
                             choices = allChoices(),
                             selected = NULL)
  })
  
  observeEvent(input$selectall, {
    updateCheckboxGroupInput(session,
                             'condSelect',
                             choices = allChoices(),
                             selected = allChoices())
  })
  
  output$table <- DT::renderDataTable(res_selected(),
                                      options = list(scrollX = TRUE),
                                      rownames = FALSE)
  
  output$conditionSelection <- renderUI({
    conditionChoices <- sort(unique(res()$condition))
    checkboxGroupInput(
      inputId = "condSelect",
      label = NULL,
      choices = conditionChoices,
      selected = conditionChoices,
    )
  })
  
  
  output$filterBy <- renderUI({
    filterChoices <- unique(colnames(res()))
    filterChoices <-
      c("", filterChoices[!filterChoices %in% c("X", "gene", "condition")])
    selectInput(
      inputId = "filterID",
      label = "Select Column:",
      choices = filterChoices,
      selected = ""
    )
  })
  
  output$filterConds <- renderUI({
    filterBy <- if (is.null(input$filterID))
      ""
    else
      input$filterID
    if (filterBy != "") {
      filterCondChoices <- unique(res()[, filterBy])
      print(class(filterCondChoices))
      if (class(filterCondChoices) %in% c("character", "logical")) {
        selectInput(
          inputId = "filterCondID",
          label = "Select Value(s):",
          choices = filterCondChoices,
          selected = NULL,
          multiple = TRUE
        )
      } else if (class(filterCondChoices) == "numeric") {
        minChoice <- floor(min(filterCondChoices) * 10) / 10
        maxChoice <- ceiling(max(filterCondChoices) * 10) / 10
        sliderInput(
          "filterCondID",
          "Range:",
          min = minChoice,
          max = maxChoice,
          value = c(minChoice, maxChoice)
        )
      }
    }
  })
}
