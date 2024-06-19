#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(caret)
library(mlr)
library(xgboost)
library(shinythemes)
library(DT)

load("XGBOOST_regress_v1.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cerulean"),
    
    
    # Application title
    # Application title
    titlePanel(title=div(img(src="fraunhofer_ITMP-logo_900p.jpg",
                             height="20%", width="20%", align="right"),
                         "XGBoost Model from Czodorowski pKa dataset v2")),
    
    p("An attempt to model dataset in https://github.com/czodrowskilab/Machine-learning-meets-pKa"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Nrounds", "Number of rounds:", min = 300, max = 1000, value = 600),
            sliderInput("max_depth", "Tree depth:", min = 6, max = 12, value = 10),
            sliderInput("Eta", "Learning rate:", min = 0.01, max = 1, value = 0.025),
            sliderInput("Colsample", "Percentage of columns:", min = 0, max = 1, value = 0.75),
            fileInput("file", 
                      span("Choose File for predictions ",
                           tags$a(
                               "(example)",
                               href = "#",
                               onclick = "window.open('Capture.jpg', 
                               'newwindow', 'width=500, height=250'); return false;"
                           ), 
                      multiple = FALSE, accept = ".csv"))
            ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Training", plotOutput('trainingplot',width = "500px", height = "500px")),
                        tabPanel("Test", plotOutput('testplot',width = "500px", height = "500px")),
                        tabPanel("TOP10", plotOutput('top10features',width = "500px", height = "500px")),
                        tabPanel("Predictions", dataTableOutput('tables'))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    final_model <- reactive({
        
        req(input$Nrounds, input$max_depth, input$Eta, input$Colsample)
        
        xgboost(data = as.matrix(train_data[,-c(1,2)]),
                label = unname(unlist(train_data[,1])),
                booster = "gbtree",
                objective = "reg:squarederror",
                nrounds = input$Nrounds,
                max_depth= input$max_depth,
                colsample_bytree = input$Colsample,
                min_child_weight=bst$bestTune$min_child_weight,
                subsample=bst$bestTune$subsample,
                eta = input$Eta,
                gamma=bst$bestTune$gamma,
                scale_pos_weight = 0.5, # because our dataset is unbalanced
                verbose = 0)
        
        
    })
    
    y_pred_tr <- reactive({
        
        req(final_model())
        predict(final_model(), as.matrix(train_data[,-c(1,2)]))
        
    })
    
    y_pred <- reactive({
        
        req(final_model())
        predict(final_model(), as.matrix(test_data[,-c(1,2)]))
        
    })
    
    output$trainingplot <- renderPlot({
        
        plot(unname(unlist(train_data[,1])), y_pred_tr(), xlab='pKa values', ylab = 'predicted pKa in training')
        abline(lm(unname(unlist(train_data[,1]))~y_pred_tr()), col = 'green', lwd = 4)
        abline(0,1, col= 'red', lwd= 4, lty = 2)
        legend("bottomright", bty="n", legend=paste("R2 is", 
                                                    format(summary(lm(unname(unlist(train_data[,1]))~y_pred_tr()))$adj.r.squared, digits=4)))
        
    })
    
    output$testplot <- renderPlot({
        
        plot(unname(unlist(test_data[,1])), y_pred(),
             xlab='pKa values', ylab = 'predicted pKa in training')
        abline(lm(unname(unlist(test_data[,1]))~y_pred()), col = 'green', lwd = 4)
        abline(0,1, col= "red", lwd= 4, lty = 2)
        legend("bottomright", bty="n", legend=paste("R2 is", 
                                                    format(summary(lm(unname(unlist(test_data[,1]))~y_pred()))$adj.r.squared, digits=4)))
        
        
        #axis(1, labels = "pKa")
        
    })
    
    importance_matrix <- reactive({
        
        req(final_model())
        
        xgb.importance(colnames(train_data[,-c(1,2)]), model = final_model())
        
    })

    top10plot <- reactive({ 
    
        req(importance_matrix())
        
        xgb.ggplot.importance(importance_matrix()[1:10,], rel_to_first = TRUE, xlab = "Relative importance")+
            #ggtitle("Top10 most important features in xgboost model")+
            guides(fill = "none")+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=16,face="bold"))
    })    
    
    output$top10features <- renderPlot({
        
        top10plot()   
        
    })
    
    pKa_predicted <- reactive({
        req(input$file)
        
        #ext <- tools::file_ext(input$file$name)
        #switch(ext,
        #vroom::vroom(input$upload$datapath, delim = ";")
        read_delim(input$file$datapath, delim = ";")
        #validate("Invalid file; Please upload a .tsv file")
        
    })
    
    predictions <- reactive({
        
        req(input$file, final_model())
        
        pred <- round(predict(final_model(), as.matrix(pKa_predicted()[,-c(1:4)])),4)
        pred <- as.data.frame(pred, names('pKa_pred'))
        return(pred)
        
        
        
    })
    
    output$tables <- DT::renderDataTable({
        DT::datatable(predictions(),
                      extensions = c('Buttons'),
                      options = list(
                          dom = 'frtBip',
                          buttons = list(list(extend = 'excel', filename = paste0("pKa_pred-", Sys.Date())))
                      )
        )
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
