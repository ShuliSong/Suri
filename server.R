


library(shiny)
library(data.table)
library(DT)
library(dplyr)

shinyServer(function(input, output, session) {
  ###   OUTPUT HOT   ###
  output$test <- DT::renderDataTable({
    input$reset
    DF2 <<-
      DF[, c(
        "ItemDescription",
        "infMFC",
        "infBRD",
        "ProductLine",
        "CurrentRegPrice",
        "PropoRegPrice",
        "Freq",
        "Depth"
      )]
    DT::datatable(data.frame(DF2), editable = TRUE) %>%
      formatPercentage(c('Freq', 'Depth'), 2) %>%
      formatRound(c("CurrentRegPrice", "PropoRegPrice"), 1)
  })
  observeEvent(input$test_cell_edit, {
    proxy2 = dataTableProxy('test')
    info = input$test_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    DF2[i, j] <<- DT::coerceValue(v, DF2[i, j])
    replaceData(proxy2, DF2, resetPaging = FALSE)  # important
  })
  
  #### SUBMIT TABLE ####
  output$aggregate_table <- DT::renderDataTable({
    input$submit
    x <- DF2
    x1 <- fun(x$Depth, x$Freq, x$PropoRegPrice) #input
    names(x1) <- c("EstimatedVolume", "EstimatedValue")
    x1$EstimatedVolumeShare = x1[, 1] / sum(x1[, 1])
    x1$EstimatedValueShare = x1[, 2] / sum(x1[, 2])
    x2 <- fun(DF$Depth, DF$Freq, DF$CurrentRegPrice) #original
    names(x2) <- c("OriginalVolume", "OriginalValue")
    x2$OrignialVolumeShare = x2[, 1] / sum(x2[, 1])
    x2$OriginalValueShare = x2[, 2] / sum(x2[, 2])
    x5 <- cbind(x, x1, x2)

    
    input$levels
    ### AGGREGATE TABLE AT DIFFERENT LEVELS ###
      x3 <- select(x5,
        ItemDescription,
        infMFC,
        infBRD,
        ProductLine,
        OriginalVolume,
        EstimatedVolume,
        OriginalValue,
        EstimatedValue
      ) %>%
        group_by_(.dots = input$levels) %>%
        summarise_if(is.numeric, sum) %>%
        mutate(
          VolumeChg = EstimatedVolume / OriginalVolume - 1,
          ValueChg = EstimatedValue / OriginalValue - 1)
      x3$OriginalVolumeShare = x3$OriginalVolume / sum(x3$OriginalVolume)
      x3$EstimatedVolumeShare = x3$EstimatedVolume / sum(x3$EstimatedVolume)
      x3$OriginalValueShare = x3$OriginalValue / sum(x3$OriginalValue)
      x3$EstimatedValueShare = x3$EstimatedValue / sum(x3$EstimatedValue)
      
      DT::datatable(
        select(x3,-c(OriginalVolume,EstimatedVolume,
            OriginalValue,EstimatedValue)),
        extensions = 'Buttons',
        options = list(
          pageLength = 50,
          dom = 'Brtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      ) %>%
        formatStyle("OriginalVolumeShare",
                    background = styleColorBar(range(0, 1), 'lightgrey')) %>%
        formatStyle("EstimatedVolumeShare",
                    background = styleColorBar(range(0, 1), 'lightblue')) %>%
        formatStyle("OriginalValueShare", background = styleColorBar(range(0, 1), 'lightgrey')) %>%
        formatStyle("EstimatedValueShare", background = styleColorBar(range(0, 1), 'orange')) %>%
        formatStyle(c('VolumeChg', 'ValueChg'), color = styleInterval(c(-0.00001, 0.000001), c('green', NA, 'red'))) %>%
        formatPercentage(
          c(
            'VolumeChg',
            'ValueChg',
            'OriginalVolumeShare',
            'EstimatedVolumeShare',
            'OriginalValueShare',
            'EstimatedValueShare'
          ),
          2
        )
    })
  })
