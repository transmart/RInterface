
# Shows the tranSMART tree structure in shiny tree with clinical data visualizations
showTree <- function(study_as_list, ...) {

  app <- list(
    ui = fluidPage(
      sidebarLayout(
        
        mainPanel(
          br(),
          "Currently Selected:",
          verbatimTextOutput("selTxt"),
          verbatimTextOutput("conceptTxt"),
          hr(),
          shinyTree("tree", search = TRUE),
          
          conditionalPanel(
            condition = "output.conceptTxt=='Numerical variable'",
            plotOutput("distPlot"),
            plotOutput("boxPlot")
          ),
          
          conditionalPanel(
            condition = "output.conceptTxt=='Categorical variable'",
            plotOutput("barPlot")
          ),
          
          width = 8
        ),
        
        sidebarPanel(
          h3("Study concepts in tranSMART tree structure"),
          hr(),
          imageOutput("image", width = 325, height = 325),
          helpText(HTML("Select concepts for more information and data variable visualization. 
                        <hr>Created with Shiny and shinyTree.")),
          hr(),
          fluidRow(
            column(10,
                   h4("Graphical options"),
                   br(),
                   
                   conditionalPanel(
                     condition = "output.conceptTxt=='Numerical variable'",
                     sliderInput('bins', 'Number of bins', min=1, max=30, value=10, step=1, round=0),
                     br()),
                   
                   radioButtons("dist", "Color scheme:",
                                c("Default" = "norm",
                                  "Alternating" = "tomatoes",
                                  "Gradient" = "rainbow",
                                  "Grayscale" = "sorrow"))
                   
            )),
          width = 4),
          )
    ),
    
    
    server = function(input, output) {
      
      # show tranSMART logo
      output$image <- renderImage({
        return(list(src = system.file('img/circle.png', package='transmartRClient'), contentType = "image/png", width = 300, height = 300, alt = "tranSMART logo"))
      }, deleteFile = FALSE)
      
      output$tree <- renderTree({
        study_as_list
      })
      
      output$selTxt <- renderText({
        tree <- input$tree
        if (is.null(tree)){
          "None"
        } else{
          .html_to_txt(unlist(get_selected(tree)))
        }
      })
      
      output$conceptTxt <- renderText({
        concept_type = "None"
        tree <- input$tree
        if (is.null(tree)){
          "None"
        } else {
          var <- .html_to_txt(get_selected(tree)[[1]])
          ancestry <- attr(get_selected(tree)[[1]], "ancestry")
          concept_type <- attr(study_as_list[[c(ancestry, var)]], "concept_type")
          
          if (concept_type == "Numerical variable"){
            
            #Retrieve data only when variable is selected for the first time
            if (is.null(attr(study_as_list[[c(ancestry, var)]], "data_values"))){
              data_values <- as.numeric(getObservations(study, concept.links = attr(study_as_list[[c(ancestry, var)]], "apilink"))$observations[,2])
              attr(study_as_list[[c(ancestry, var)]], "data_values") <<- data_values
            }
            
            values <- attr(study_as_list[[c(ancestry, var)]], "data_values")
            values <- values[!is.na(values)]
            sample_size <- length(values)
            
            # draw the histogram with the specified number of bins
            output$distPlot <- renderPlot({
              
              par(mar=c(2.1,4.1,4.1,2.1))
              bins <- seq(min(values), max(values), length.out = input$bins + 1)
              if (min(bins) == max(bins)) bins <- 1
              main = paste(var, " (n = ", sample_size, ")", sep = "")
              
              args <- switch(input$dist,
                             "norm" = list(x = values, breaks = bins, border = "white", main = main, xlab = "", col = "steelblue"),
                             "tomatoes" = list(x = values, breaks = bins, border = "white", main = main, xlab = "", col = c('steelblue',"tomato")),
                             "rainbow" = list(x = values, breaks = bins, border = "white", main = main, xlab = "", col = rainbow(input$bins + 1)),
                             "sorrow" = list(x = values, breaks = bins, border = "white", main = main, xlab = "", col = "darkgrey"))
              
              do.call(hist, args)
            })
            
            # draw boxplot
            output$boxPlot <- renderPlot({
              par(mar=c(2.1,4.1,2.1,2.1))
              
              args <- switch(input$dist,
                             "norm" = list(x = values, xlab = "", col = "steelblue"),
                             "tomatoes" = list(x = values, xlab = "", col = "steelblue", medcol = "tomato"),
                             "rainbow" = list(x = values, xlab = "", col = sample(rainbow(10), 1)),
                             "sorrow" = list(x = values, xlab = "", col = "darkgrey"))
              
              do.call(boxplot, args)
            })
          }
          else if (concept_type == "Categorical variable"){
            
            #Retrieve data only when variable is selected for the first time
            if (is.null(attr(study_as_list[[c(ancestry, var)]], "data_values"))){
              data_values <- table(getObservations(study, concept.links = dirname(attr(study_as_list[[c(ancestry, var)]], "apilink")))$observations[,2])
              attr(study_as_list[[c(ancestry, var)]], "data_values") <<- data_values
            }
            
            # draw barplot of categorical options
            output$barPlot <- renderPlot({
              
              values <- attr(study_as_list[[c(ancestry, var)]], "data_values")
              left_inch <-  max(strwidth(names(values), "inch")+0.4, na.rm = TRUE)
              par(mai=c(1.02,left_inch,0.82,0.42))
              sample_size <- sum(values)
              main = paste(var, " (n = ", sample_size, ")", sep = "")
              
              args <- switch(input$dist,
                             "norm" = list(sort(values, descending = FALSE), horiz = TRUE, las = 2, col = "steelblue", main = main),
                             "tomatoes" = list(sort(values, descending = FALSE), horiz = TRUE, las = 2, col = c("steelblue", "tomato"), main = main),
                             "rainbow" = list(sort(values, descending = FALSE), horiz = TRUE, las = 2, col = rainbow(length(values)), main = main),
                             "sorrow" = list(sort(values, descending = FALSE), horiz = TRUE, las = 2, col = "darkgrey", main = main))
              
              do.call(barplot, args)
              grid(NULL,NA, col = "Black")
            })
          }
        }
        # show concept type in text field
        concept_type
      })
      
    }
    )
  runApp(app, ...)
}


# Converts html entities to normal text
.html_to_txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]] 
}

