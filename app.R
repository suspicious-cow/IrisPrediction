

# set a seed in case we use any random items
set.seed(1337)


# Set the names of the packages and libraries you want to install
required_libraries <- c("shiny","class","shinythemes")

# Install missing packages and load all required libraries
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}


# Define the UI for our Iris Prediction application
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  tags$style(HTML("
    .prediction-result {
      font-size: 24px;
      font-weight: bold;
    }
  ")),
  
  tabsetPanel(
    tabPanel("App",
             titlePanel("Iris Prediction Application"),
             
             sidebarLayout(
               sidebarPanel(
                 headerPanel("Enter Iris Measurements"),
                 
                 numericInput("sepal_length",
                              label = "Sepal Length (cm)",
                              value = 5.1,
                              min = 4,
                              max = 8,
                              step = 0.1),
                 
                 numericInput("sepal_width",
                              label = "Sepal Width (cm)",
                              value = 3.5,
                              min = 2,
                              max = 5,
                              step = 0.1),
                 
                 numericInput("petal_length",
                              label = "Petal Length (cm)",
                              value = 1.4,
                              min = 1,
                              max = 7,
                              step = 0.1),
                 
                 numericInput("petal_width",
                              label = "Petal Width (cm)",
                              value = 0.2,
                              min = 0.1,
                              max = 2.5,
                              step = 0.1),
                 
                 actionButton("predict",
                              "Predict Iris Species")
               ),
               
               mainPanel(
                 headerPanel("Prediction Result"),
                 div(class = "prediction-result", textOutput("prediction_result")), 
                 uiOutput("iris_image")
               )
             )
    ), 
    tabPanel("Help",
             # Help content goes here
             h2("How to Use the Iris Prediction Application"),
             p("The Iris Prediction app allows you to predict the species of an iris flower based on its morphological measurements. The app uses a machine learning model trained on the famous Iris dataset to make predictions. The possible species are Iris setosa, Iris versicolor, and Iris virginica."),
             h3("Step 1: Provide Measurements"),
             p("To use the app, you need to provide four measurements of the iris flower. These measurements are the sepal length, sepal width, petal length, and petal width. All measurements should be entered in centimeters (cm)."),
             tags$ul(
               tags$li("Sepal Length: Enter the length of the sepal in centimeters. The sepal length typically ranges from 4 to 8 cm."),
               tags$li("Sepal Width: Enter the width of the sepal in centimeters. The sepal width typically ranges from 2 to 5 cm."),
               tags$li("Petal Length: Enter the length of the petal in centimeters. The petal length typically ranges from 1 to 7 cm."),
               tags$li("Petal Width: Enter the width of the petal in centimeters. The petal width typically ranges from 0.1 to 2.5 cm.")
             ),
             h3("Step 2: Prediction of the Species"),
             p("After entering the measurements, click the 'Predict Iris Species' button. The app will use the provided measurements to predict the species of the iris flower. The prediction result is displayed in the 'Prediction Result' section."),
             p("The prediction result will show the predicted species along with an image of the corresponding iris flower. The image is provided for reference and may not exactly match the specific iris flower you are analyzing."),
             h3("Additional Information"),
             p("The Iris Prediction app uses the k-nearest neighbors (k-NN) algorithm for classification. The model was trained on a dataset of iris flowers with known species and measurements. The app uses the trained model to classify new iris flowers based on their measurements."),
             p("Please note that the prediction accuracy may vary depending on the specific characteristics of the iris flower being analyzed. The app is intended for educational and illustrative purposes and should not be used for scientific research or critical decision-making.")
             
    )
  ) # Close the tabsetPanel here
) # Close the fluidPage here

  



# Define server logic for our Iris Prediction
server <- function(input, output) {
  
  # Prepare the Iris dataset
  iris_data <- datasets::iris
  
  # Train-test split
  set.seed(123)
  train_indices <- sample(1:nrow(iris_data), 0.7 * nrow(iris_data))
  train_data <- iris_data[train_indices, ]
  test_data <- iris_data[-train_indices, ]
  
  # Prediction function
  predict_iris <- function(sepal_length, sepal_width, petal_length, petal_width) {
    new_data <- data.frame(Sepal.Length = sepal_length,
                           Sepal.Width = sepal_width,
                           Petal.Length = petal_length,
                           Petal.Width = petal_width)
    
    # k-NN prediction
    prediction <- knn(train_data[, 1:4], rbind(test_data[, 1:4], new_data), train_data[, 5], k = 5)
    return(tail(prediction, 1)) # Return the prediction for the new data
  }
  
  # Initialize an empty prediction result
  output$prediction_result <- renderText({" "})
  
  # Update prediction result when the "Predict" button is clicked
  observeEvent(input$predict, {
    sepal_length <- input$sepal_length
    sepal_width <- input$sepal_width
    petal_length <- input$petal_length
    petal_width <- input$petal_width
    
    prediction <- isolate(predict_iris(sepal_length, sepal_width, petal_length, petal_width))
    output$prediction_result <- renderText({
      paste("Predicted Iris Species:", prediction)
    })
    
    # Image URLs for each iris species
    iris_images <- list(
      setosa = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/800px-Kosaciec_szczecinkowaty_Iris_setosa.jpg",
      versicolor = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/800px-Iris_versicolor_3.jpg",
      virginica = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/800px-Iris_virginica.jpg"
    )
    
    # Update image source based on the prediction
    output$iris_image <- renderUI({
      req(prediction) # Ensure the prediction variable is available
      if (prediction == "setosa") {
        img_src <- iris_images$setosa
      } else if (prediction == "versicolor") {
        img_src <- iris_images$versicolor
      } else if (prediction == "virginica") {
        img_src <- iris_images$virginica
      }
      div(class = "prediction-result", tags$img(src = img_src, height = 400, width = 400))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)


