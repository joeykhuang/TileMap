library(shiny)
library(tidyverse)
library(tibble)
library(plotly)
library(forcats)
library(DT)
library(shinyjs)
library(ggh4x)

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
                                        if (input$filterID == "pvalue") {
                                          print("hi");
                                          res_ %>% filter(between(
                                            !!rlang::sym(input$filterID),
                                            0,
                                            input$filterCondID[1]
                                          ))
                                        } else {
                                          res_ %>% filter(between(
                                            !!rlang::sym(input$filterID),
                                            input$filterCondID[1],
                                            input$filterCondID[2]
                                          ))
                                        }
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
  
  res_levels <- reactive({
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
    add_facet <- !is.null(input$groupID) && input$groupID != ''
    
    if (add_facet) {
      unique_groups = unique(res()[, input$groupID])
      if (length(unique_groups) > 5) {
        show_alert("cannot plot more than 5 groups",
                   type = "error",
                   duration = 5)
        add_facet = FALSE
      }
    }
    
    if (!input$plotByButton) {
      plotBy <- "condition"
      valCol <- "gene"
    } else {
      plotBy <- "gene"
      valCol <- "condition"
    }
    
    outlineWidth <- if (input$plotOutline) 0 else 0.2
    
    sortBy <- input$sortBy
    
    if (sortBy == "Total Changed") {
      print("Total Changed is sorting function");
      sortFun <- function (x) sum(abs(x))
    } else if (sortBy == "Upregulated") {
      sortFun <- function (x) sum(x > 0)
    } else if (sortBy == "Downregulated") {
      sortFun <- function (x) sum(x < 0)
    } else {
      sortFun <- function (x) 0
    }
    
    p <- ggplotly(
      ggplot(
        data = highlight_key(res_levels(), ~ get(valCol)),
        aes(
          y = fct_reorder(get(plotBy), val, .fun = sortFun, .na_rm = TRUE),
          x = val,
          fill = fold_cat,
          text = paste0(gene, "\n", fold)
        )
      ) +
        geom_col(
          position = "stack",
          alpha = 1,
          color = 'black',
          linewidth = outlineWidth,
          na.rm = TRUE
        ) +
        geom_vline(xintercept = 0, size = 1) +
        xlab("# Changed") +
        ylab(plotBy) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_rect(
            fill = "#EFEFEF",
            color = "black",
            linewidth = 1
          ),
          strip.text = element_text(size = 10)
        ) +
        scale_fill_manual(
          values = fold_cat_colors,
          name = "Fold Change",
          breaks = c("-2", "-1.5", "-0.75", "-0.25", "0.25", "0.75", "1.5", "2")
        ) +
        {
          if (add_facet)
            facet_grid(
              ~ get(input$groupID) ~ .,
              space = "free_y",
              scales = "free_y",
              switch = "y",
              drop = TRUE
            )
        } +
        {
          if (add_facet)
            theme(panel.border = element_rect(fill = "transparent", linewidth = 1))
        } + 
        {
          if (input$plotText) geom_text(aes(label = get(valCol)), position = position_stack(vjust = .5), size=2)
        }
      ,
      tooltip = "text",
      width = cdata$output_pid_width,
      height = cdata$output_pid_height
    ) %>% config(displayModeBar = FALSE)
    
    if (add_facet) {
      group_sizes <-
        rev((res_levels() %>% group_by(!!sym(input$groupID)) %>% summarise(n = n_distinct(get(plotBy))))$n)
      total_size <- sum(group_sizes)
      group_end <- c(rev(cumsum(group_sizes / total_size)), 0)
      num_groups = length(group_sizes)
      #print(group_sizes)
      
      if (num_groups >= 2) {
        p$x$layout$yaxis$domain <- c(group_end[2], group_end[1])
        p$x$layout$yaxis2$domain <- c(group_end[3], group_end[2])
      }
      if (num_groups >= 3) {
        p$x$layout$yaxis3$domain <- c(group_end[4], group_end[3])
      }
      if (num_groups >= 4) {
        p$x$layout$yaxis4$domain <- c(group_end[5], group_end[4])
      }
      if (num_groups == 5) {
        p$x$layout$yaxis5$domain <- c(group_end[6], group_end[5])
      }
      
      lapply(3:(2 + num_groups), function(i) {
        p$x$layout$annotations[[i]]$y <<-
          (group_end[i - 1] + group_end[i - 2]) / 2
      })
      
      margin_size <- 0.003
      lapply(seq(2, 2 * num_groups, 2), function(i) {
        p$x$layout$shapes[[i]]$y0 <<- group_end[i / 2 + 1] + margin_size
        p$x$layout$shapes[[i]]$y1 <<- group_end[i / 2] - margin_size
      })
      
      lapply(seq(1, 2 * num_groups - 1, 2), function(i) {
        p$x$layout$shapes[[i]]$y0 <<- group_end[(i + 1) / 2 + 1] + margin_size
        p$x$layout$shapes[[i]]$y1 <<-
          group_end[(i + 1) / 2] - margin_size
      })
    }
    
    highlightedValues <- input$search
    
    highlight(
      p,
      on = "plotly_click",
      off = "plotly_doubleclick",
      opacityDim = 0.3,
      defaultValues = highlightedValues,
      selected = attrs_selected(showlegend = FALSE)
    )
  })
  
  
  fileNameHeatmap <- reactive({
    input$HeatmapDataset
  })
  resHeatMap <- reactive({
    if (!is.null(fileNameHeatmap)) {
      read.csv(paste0("datasets/", fileNameHeatmap(), ".csv"))
    }
  })
  
  output$heatmapPlot <- renderPlot(
    ggplot(resHeatMap(),
           aes(condition, gene, fill=fold)) 
    + geom_raster() 
    + scale_fill_gradientn(colors = fold_cat_colors, na.value = 'white') 
    + theme(
      axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5), 
      axis.text.y = element_text(angle = 30),
            panel.background = element_blank())
    )
  
  
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
  
  output$groupBy <- renderUI({
    groupChoices <- unique(colnames(res()))
    groupChoices <-
      c("", groupChoices[!groupChoices %in% c("X", "gene", "condition")])
    selectInput(
      inputId = "groupID",
      label = "Group By:",
      choices = groupChoices,
      selected = ""
    )
  })
  
  output$searchBar <- renderUI({
    if (!input$plotByButton) {
      valCol <- "gene"
    } else {
      valCol <- "condition"
    }
    selectizeInput(inputId = "search",
                   choices = unique(res_selected()[, valCol]),
                   selected = NULL,
                   multiple = TRUE,
                   label = "Search",
                   options = list(placeholder = 'e.g. TP53, TNF', create = TRUE))
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
        if (filterBy == "pvalue") {
          minChoice <- floor(min(filterCondChoices) * 10) / 10
          maxChoice <- ceiling(max(filterCondChoices) * 10) / 10
          sliderTextInput(
            "filterCondID",
            "Range:",
            choices = c(0, 0.0001, 0.001, 0.01, 0.1, 1, 10),
            grid = T
          )
        }
      }
    }
  })
}
