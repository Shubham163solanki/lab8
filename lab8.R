library(shiny)
library(ggplot2)


data(iris)

ui <- fluidPage(
  

  tabsetPanel(
    

    tabPanel("Histogram",
 
             selectInput("col", "Select Column:",
                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                         selected = "Sepal.Length"),
             # Add dropdown menu for class selection
             selectInput("class", "Select Class:",
                         choices = c("setosa", "versicolor", "virginica"),
                         selected = "setosa"),
    
             plotOutput("histogram")),
    

    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "X-axis variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
                 selectInput("y_var", "Y-axis variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
                 selectInput("species", "Species:", choices = c("setosa", "versicolor", "virginica")),
               ),
               mainPanel(
                 plotOutput("iris_plot")
               )
             )
  ),
  tabPanel("vilon Plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("var", "Variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
             ),
             mainPanel(
               plotOutput("v_plot")
             )
           )
  )
)
)


server <- function(input, output) {

  output$histogram <- renderPlot({
    hist_data <- iris[iris$Species == input$class, input$col]
    feature_name <- sub("\\.", " ", input$col, fixed = TRUE)
    class_name <- input$class
    hist(hist_data, main = paste(class_name, "vs.", feature_name), xlab = feature_name)
  })
  

  output$scatterplot <- renderPlot({
    plot_data <- iris[, c("Species", input$col2)]
    feature_name <- sub("\\.", " ", input$col2, fixed = TRUE)
    x_axis_label <- feature_name
    y_axis_label <- "Species"
    plot(plot_data[,2], plot_data[,1], xlab = x_axis_label, ylab = y_axis_label, col = as.integer(iris$Species))
  })
  
  

  iris_data <- reactive({
    if (input$species == "setosa") {
      subset(iris, Species == "setosa")
    } else if (input$species == "versicolor") {
      subset(iris, Species == "versicolor")
    } else {
      subset(iris, Species == "virginica")
    }
  })
  

  output$iris_plot <- renderPlot({
    ggplot(iris_data(), aes_string(x = input$x_var, y = input$y_var, color = "Species")) +
      geom_point(position =  "identity") +
      labs(title = paste(input$x_var, "vs", input$y_var),
           x = input$x_var,
           y = input$y_var)
  })
  
  output$v_plot <- renderPlot({
    ggplot(data = iris, aes(x = Species, y = iris[[input$var]], fill = Species)) +
      geom_violin(trim = FALSE) +
      labs(x = "species", y = input$var) +
      theme_bw()
  })
}

# Run the app
shinyApp(ui, server)
