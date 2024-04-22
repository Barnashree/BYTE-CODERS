# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Analyze student performance data to identify patterns and predict future performance"),
  h4("Presented by Byte Coders (Barnashree Mondal, G.Y.Manisha, and Priyanka Show)"),
  h5("College of Engineering, Kolaghat, Westbengal, India"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      
      conditionalPanel(
        'input.control === "Correlation"',
        selectInput("pmethod", "Select plot:",
                    list("corrplot_number"='corrplot_number', 
                         "corrplot_color_alphabet"='corrplot_color_alphabet', 
                         "corrplot_default"='corrplot_default', 
                         "corrplot_AOE"='corrplot_AOE', 
                         "corrplot_shade_AOE"='corrplot_shade_AOE', 
                         "corrplot_square_FPC_lower"='corrplot_square_FPC_lower', 
                         "corrplot_ellipse_AOE_upper"='corrplot_ellipse_AOE_upper', 
                         "corrplot_mixed_AOE"='corrplot_mixed_AOE', 
                         "corrplot_mixed_shade_pie_hclust"='corrplot_mixed_shade_pie_hclust',
                         "Heat map"='Heat map'
                         )),
        
        checkboxGroupInput("cor",
                           label = "Select items (for correlation):",
                           choices = c("schoolMS", "sexM", "age", "addressU", "famsizeLE3", "PstatusT", "Medu", "Fedu", "Mjobhealth", "Mjobother", "Mjobservices", "Mjobteacher", "Fjobhealth", "Fjobother", "Fjobservices", "Fjobteacher", "reasonhome", "reasonother", "reasonreputation", "guardianmother", "guardianother", "traveltime", "studytime", "failures", "schoolsupyes", "famsupyes", "paidyes", "activitiesyes", "nurseryyes", "higheryes", "internetyes", "romanticyes", "famrel", "freetime", "goout", "Dalc", "Walc", "health", "absences", "G1", "G2", "G3"),
                           selected = c("age", "famsizeLE3", "traveltime","studytime","failures", "absences", "G1", "G2", "G3")
        )  ),
      
      
      conditionalPanel(
        'input.control === "Regression"',
        selectInput("method", "Select Regression Model:",
                    list("Ordinary Least Squares Regression"='ols', "Significant at the p < 0.05"='p0.05', "Significant at the p < 0.15"='p0.15', "Stepwise regression (Akaike’s Information Criterion (AIC))"='step.aic', "Stepwise regression (Bayesian Information Criterion (BIC))"='step.bic', "Ridge Model Fitting"='ridge', "Lasso Model (Minimum value of the penalty parameter)"='lasso.min', "Lasso Model (Regularized model)"='lasso.1se', "Elastic Net Model (Minimum value of the penalty parameter)"='enet.min', "Elastic Net Model (Regularized model)"='enet.1se')),
        
        selectInput("subject", "Select Subject:",
                    list("Mathematics"='student-mat.csv', "Portuguese"='student-por.csv')),
        
        selectInput("grade", "Select Grade:",
                    list("G1"='G1', "G2"='G2', "G3"='G3')),
        
        sliderInput(inputId = "train",
                    label = "Training set (%):",
                    min = 50,
                    max = 80,
                    value = 80),
        sliderInput(inputId = "rep",
                    label = "Number of repetation:",
                    min = 3,
                    max = 500,
                    value = 3)
        
      )),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Panel for Correlation outputs ----
      tabsetPanel(id='control',type="tab",
        tabPanel("Correlation",
                 h4("Correlation plot:"),
                 plotOutput("corplot",width = "100%"),
                 h4("Correlation matrix:"),
                 tableOutput("cormat")
                 
        ),
        
        tabPanel("Regression",
                 h4("Dimension:"),
                 verbatimTextOutput(outputId = "dim"),
                 h4("Model and Coefficient:"),
                 verbatimTextOutput(outputId = "coefficient"),
                 h4("Average Mean-Squared Error:"),
                 textOutput(outputId = "mse"),
                 h4("Standard Deviation:"),
                 textOutput(outputId = "msesd"),
                 h4("Mean-Squared Errors:"),
                 verbatimTextOutput(outputId = "mses"),
                 h4("Boxplot:"),
                 plotOutput("mseplot",width = "100%"),
                 h4("Ridge trace:"),
                 plotOutput("plot1",width = "100%"),
                 h4("10-fold CV to estimate lambda:"),
                 plotOutput("plot2",width = "100%"),
                 h4("Plot alphaseq vs CV-MSE:"),
                 plotOutput("plot3",width = "100%"),
                 h4("Plot results for minimum:"),
                 plotOutput("plot4",width = "100%"),
                 h4("Unpenalized coefficients:"),
                 verbatimTextOutput(outputId = "utab"),
                 h4("Penalized coefficients:"),
                 verbatimTextOutput(outputId = "ptab"),
                 h4("Comparisons (for Mathematics):"),
                 plotOutput("plotmath",width = "100%"),
                 h4("Comparisons (for Portuguese):"),
                 plotOutput("plotpor",width = "100%"),
                 h4("Demographic Information:"),
                 plotOutput("dataset",width = "100%")
        )
      )
      
      # Panel for Regression outputs ----
      
    )
  )
)
