library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)
library(broom)
library(knitr)
library(kableExtra)

# Load datasets upfront
nhanes_data <- readRDS("nhanes_data.RDS")
nhanes_data_imputed <- readRDS("nhanes_data_imputed.RDS")

# Define the UI
ui <- fluidPage(
  
  # Title of the app
  titlePanel("BMI Dashboard"),
  
  # 1) Describe the data
  fluidRow(
    column(12, h3("Study Description"),
           p("This study will investigate how BMI changes with multiple variables described below. ",
             "The data was taken from CDC's NHANES dataset from 2017 - 2020 (pre-pandemic)."))
  ),
  
  # 2) Variable description table
  fluidRow(
    column(12, h3("Variable Descriptions"),
           tableOutput("variable_table"))
  ),
  
  # 3) Checkbox for imputing data
  fluidRow(
    column(12, checkboxInput("impute_data", "Check this box if you want to impute missing data", value = FALSE))
  ),
  
  # 4) BMI transformation section
  fluidRow(
    column(12, h3("Check normality of BMI distribution"),
           plotOutput("bmi_distribution")),
    
    column(12, h3("Check normality of log-transformed BMI distribution"),
           plotOutput("bmi_log_distribution"))
  ),
  
  # 5) Rationale for log transformed data
  fluidRow(
    column(12, h3("What should we use as our outcome variable?"),
           p("As we can see, the log-transformed data better aligns to the expected trend in the Q-Q plot. The histogram and violin plot also show more normally distributed data. Therefore, we will use log-transformed BMI values in our analysis."))
  ),
  
  # 6) Linear model coefficients
  fluidRow(
    column(12, h3("Linear Model Results"),
           p("A linear model (without interaction terms) is now created to determine how different variables impact BMI. Here are the coefficients of the fitted linear model:"),
           tableOutput("lm_coefficients"))
  ),
  
  # 7) Interpret linear coefficients
  fluidRow(
    column(12, h3("Interpret the Linear Model"),
           p("From the coefficients produced, we can see that BMI was most influenced by socioeconomic status, biological sex, and whether a person received emergency rations. This is interesting as other lifestyle factors, like how much moderate physical activity was received each day, did not impact the predicted BMI at all. This suggests that BMI is largely influenced by biological and socioeconomic factors rather than lifestyle."))
  ), 
  
  # 8) Visualize quantitative predictors
  fluidRow(
    column(12, 
           h3("Visualize Log BMI Distribution by Quantitative Predictor Variables"),
           selectInput("quant_predictor_variable", "Select a quantitative predictor variable:", 
                       choices = c(
                         "Time of moderate physical activity (min/day)" = "ModPhysicalActivity", 
                         "Time of sedentary activity (min/day)" = "SedentaryActivity", 
                         "Age (years)" = "Age"),
                       plotOutput("bmi_by_quant_predictor")))
  ),
  
  # 9) Visualize categorical predictors
  fluidRow(
    column(12, 
           h3("Visualize Log BMI Distribution by Categorical Predictor Variables"),
           selectInput("cat_predictor_variable", "Select a categorical predictor variable:", 
                       choices = c(
                         "Socioeconomic status" = "PovertyLevel", 
                         "Received emergency food rations" = "ReceivedEmergencyFood", 
                         "Biological sex" = "BiologicalSex"),
                       plotOutput("bmi_by_cat_predictor")))
  )
)

# Define the server
server <- function(input, output) {
  
  # Variable descriptions table
  output$variable_table <- renderTable({
    data.frame(
      Variable = c("ID", "Age", "BiologicalSex", "ModPhysicalActivity", "SedentaryActivity", "PovertyLevel", "ReceivedEmergencyFood", "BMI"),
      Type = c("ID", "Quantitative", "Binary", "Quantitative", "Quantitative", "Multi-categorical", "Binary", "Quantitative"),
      OldName = c("SEQN", "RIDAGEYR", "RIAGENDR", "PAD675", "PAD680", "INDFMMPC", "FSD151", "BMXBMI"),
      Description = c(
        "Respondent sequence number",
        "Age in years at screening",
        "Biological sex of the participant (Male/Female)",
        "Amount of moderate physical activity (minutes per day)",
        "Amount of sedentary activity (minutes per day)",
        "Family monthly poverty level index categories (Poor(\u22641.30)/Low-income(1.40 to 1.85)/Stable(>1.85))",
        "HH Emergency food received status in the last 12 months (TRUE/FALSE)",
        "BMI value in kg/m\u00b2"
      ),
      Years = "2017-2020"
    )
  })
  
  # Reactive dataset selection
  selected_data <- reactive({
    if (input$impute_data) {
      nhanes_data_imputed
    } else {
      nhanes_data
    }
  })
  
  # 4) Render BMI distribution plots
  output$bmi_distribution <- renderPlot({
    s2 <- selected_data()
    p1 <- ggplot(s2, aes(x = BMI)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of BMI Values", x = "BMI", y = "Count") +
      theme_minimal()
    
    p2 <- ggplot(s2, aes(x = BMI, y = 1)) +
      geom_violin(fill = "skyblue", alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "skyblue", color = "black", alpha = 0.9) +
      labs(title = "Violin and Boxplot of BMI Distribution", x = "BMI", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    p3 <- ggplot(s2, aes(sample = BMI)) +
      stat_qq() +
      stat_qq_line(color = "coral") +
      labs(title = "Q-Q Plot of BMI", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    
    (p1 / p2) | p3
  })
  
  output$bmi_log_distribution <- renderPlot({
    s2 <- selected_data()
    p1 <- ggplot(s2, aes(x = BMI_log)) +
      geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of Log-Transformed BMI", x = "Log(BMI)", y = "Count") +
      theme_minimal()
    
    p2 <- ggplot(s2, aes(x = BMI_log, y = 1)) +
      geom_violin(fill = "skyblue", alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "skyblue", color = "black", alpha = 0.9) +
      labs(title = "Violin and Boxplot of Log-Transformed BMI", x = "Log(BMI)", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    p3 <- ggplot(s2, aes(sample = BMI_log)) +
      stat_qq() +
      stat_qq_line(color = "coral") +
      labs(title = "Q-Q Plot of Log-Transformed BMI", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    
    (p1 / p2) | p3
  })
  
  # 6) Render linear model coefficients
  output$lm_coefficients <- renderTable({
    s2 <- selected_data()
    mod <- lm(BMI_log ~ ModPhysicalActivity + SedentaryActivity + Age + PovertyLevel + ReceivedEmergencyFood + BiologicalSex, data = s2)
    tidy(mod) %>% 
      select(term, estimate, std.error) %>% 
      kable(digits = 3, format = "html") %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
  }, sanitize.text.function = identity)
  
  # 8) Render BMI distribution by the chosen quantitative predictor
  output$bmi_by_quant_predictor <- renderPlot({
    s2 <- selected_data()
    
    predictor <- input$quant_predictor_variable
    predictor_titles <- list(
      ModPhysicalActivity = "Time of moderate physical activity (min/day)",
      SedentaryActivity = "Time of sedentary activity (min/day)",
      Age = "Age (years)"
    )
    
    p <- ggplot(s2, aes_string(x = predictor, y = "BMI_log")) +
      geom_point(color = "skyblue", alpha = 0.5) + 
      geom_smooth(method = "loess", color = "coral", se = FALSE) +
      geom_smooth(method = "lm", color = "black", linetype = "dashed") +
      labs(title = paste("BMI vs", predictor_titles[[predictor]]), 
           x = predictor_titles[[predictor]], y = "Log(BMI)") +
      theme_minimal()
    
    return(p)  
  })
  
  # 9) Render BMI distribution by the chosen categorical predictor
  output$bmi_by_cat_predictor <- renderPlot({
    s2 <- selected_data()
    
    predictor <- input$cat_predictor_variable
    predictor_titles <- list(
      PovertyLevel = "Socioeconomic status",
      ReceivedEmergencyFood = "Received emergency food rations",
      BiologicalSex = "Biological sex"
    )
    
    p <- ggplot(s2, aes_string(x = predictor, y = "BMI_log")) + 
      geom_violin(fill = "skyblue", color = "black", alpha = 0.3) +   
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("BMI Across", predictor_titles[[predictor]]), 
           x = predictor_titles[[predictor]], y = "Log(BMI)") +
      theme_minimal()
    
    return(p)  
  })
}

# Run the application
shinyApp(ui = ui, server = server)
