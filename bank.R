#Project

##Installing and loading packages (Shiny, ggplot2, gridExtra)
install.packages("shiny")
install.packages("ggplot2")
install.packages("gridExtra")
library(shiny)
library(ggplot2)
library(gridExtra)

##Loading of csv file
bank = read.csv("bank.csv")



##Creation of normal function to make data easier to compare
normal <- function(x) { return ((x-min(x))/(max(x)-min(x)))}

##Applying normal function to bank dataset
norbank <- as.data.frame(lapply(bank[1:4], normal))
norbank[5] <- bank[5]

##Creating Shiny user interface
ui <- fluidPage(
  h1("Banknote Test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Bank", "Normalized Bank"),
                  selected = "Bank"),
      selectInput("varone", "Choose variable one:",
                  choices = c("Variance","Skewness","Curtosis","Entropy","Result"),
                  selected = "Variance"),
      selectInput("vartwo", "Choose variable two:",
                  choices = c("Variance","Skewness","Curtosis","Entropy","Result"),
                  selected = "Skewness"),
      selectInput("comparison", "Make a selection:",
                  choices = c("Histogram: 1 variable", "Result: 2 variable", 
                              "Summary", 
                              "Test New Data"),
                  selected = "Result: 2 variable")
    ),
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("summary"),
      textOutput("test")
    )
  )
)

##Creating Shiny server
server <- function(input,output){
  datasetInput <- reactive ({
    switch(input$dataset,
           "Bank" = bank, "Normalized Bank" = norbank)
    })
  
  
  
  ##This prints text to screen 
  output$summary <- renderDataTable({
    dataset <- datasetInput()
    summary <- summary(dataset[0:4])
    ##Prints summary or test data based on selection
    data <- switch(input$comparison,
                   "Summary" = summary)
  })
  
  output$test <- renderPrint({
    dataset <- datasetInput()
    ##Function tests entered data to form prediction
    test <- function() 
    {
      model <- glm(formula = forged ~ variance + skewness, 
                   data=dataset, family = "binomial")
      variance <- as.numeric(readline("Variance: "))
      skewness <- as.numeric(readline("Skewness: "))
      if (is.na(variance) || is.na(skewness))
        {
        test()
        }
      else
        {
          new.norbank <- data.frame(variance = skewness, variance = skewness)
          p6<-predict(model,new.norbank, type="response")
          p6
          p7 <- round((p6) * 100, digits = 2)
          p7 <- as.numeric(p7)
          cat("There is a", p7, "% chance that this is forged banknote.")
        }
    }

    data <- switch(input$comparison,
                   "Test New Data" = test())
  })
  ##Output Plots
  output$plot <- renderPlot({
    dataset <- datasetInput()
    
    varoneInput <- reactive ({
      switch(input$varone,
             "Variance" = dataset$variance,
             "Skewness" = dataset$skewness,
             "Curtosis" = dataset$curtosis,
             "Entropy" = dataset$entropy,
             "Result" = dataset$forged)
    })
    vartwoInput <- reactive ({
      switch(input$vartwo,
             "Variance" = dataset$variance,
             "Skewness" = dataset$skewness,
             "Curtosis" = dataset$curtosis,
             "Entropy" = dataset$entropy,
             "Result" = dataset$forged)
    }) 
    varone <- varoneInput()
    vartwo <- vartwoInput()
    #Prints plot with variables chosen as x and y axis(Reference 2)
    g2<- ggplot(dataset, aes(x = varone, y = vartwo), 
                environment = environment())+ geom_point(aes(color=factor(bank$forged))) + geom_smooth(method=lm)    ##g2<- ggplot(dataset, aes(x = varone, y = vartwo), 
    p4 <- g2 + ggtitle("Result based on variables chosen") +
      labs(x = input$varone, y = input$vartwo) +
      scale_color_discrete(name ="Result", labels=c("Real", "Forged")) 

    data <- switch(input$comparison,
            "Result: 2 variable" = grid.arrange(p4),
            "Histogram: 1 variable" = hist(x = varone,
                                           main = "Histogram of Variable chosen", 
                                           xlab = input$varone,  col='red', freq=FALSE))  
  })
}
shinyApp(ui,server)


