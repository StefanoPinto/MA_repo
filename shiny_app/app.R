library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)



setwd("C:/Users/Stef/Desktop/Uni/CU/semester_4/ma/shiny_app/stef_ma")

dataset <- read_csv("metrics.csv")
theme_set(theme_bw(base_size = 18))


header <- dashboardHeader(
  title = "Stefano MA (working title)",
  titleWidth = 400,
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Stef",
                 message = "To Do: PCA stuff"
               )))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main results", tabName = "main_country",
             menuSubItem("Overview", tabName = "main_charts")),
    menuItem("Secondary results", tabName = "secondary",
             menuSubItem("Secondary charts", tabName = "secondary_charts"))
  )
)

body <- dashboardBody(
  tags$style(HTML("
  .box.box-solid.box-info>.box-header {
    background-color: #00204d !important};
  
  .skin-blue .main-header .logo {
    background-color: #00204d };
    
  }
    
  ")),
  tabItems(
    tabItem(
      "main_charts",
      " ",
      fluidRow(
        box(
          width = "100%",
          plotOutput("p"),
        )
      ),
      fluidRow(switchInput(
        label = "Group by country",
        inputId = "switch",
        onStatus = "success", 
        offStatus = "danger",
        value = FALSE,
        size = "mini",
        labelWidth = "100px"
      )),
      fluidRow(
        uiOutput("countries")
      ),
      fluidRow(
        box(title = "Metric", status = "info", solidHeader = T,
          selectInput(
            "metrics",
            " ",
            choices = c(
              "non_neutrality",
              "avg_deviation_from_neutrality",
              "dispersion",
              "moderate_divergence",
              "moderate_group_consensus",
              "moderate_size_parity"),
            multiple = FALSE,
            selected = "non_neutrality"),
          
          uiOutput(outputId = "metric_description"),
          uiOutput("latex_output"),width = 8
        ),
        box(title = "Variable(s)", status = "info", solidHeader = T,
          width = 4,
          checkboxGroupButtons(
            inputId = "variable",
            label = " ",
            choices = unique(dataset$variable),
            selected = "imbgeco"),
          uiOutput(
            outputId = "variable_description")),
        
          box(title = "Year(s)", status = "info", solidHeader = T,
            width = 4,
            sliderInput(
            "year",
            " ",
            min = as.integer(min(dataset$essround)),
            max = as.integer(max(dataset$essround)),
            value = c(as.integer(min(dataset$essround)), as.integer(max(dataset$essround))),
            step = 2,
            sep = ""
          )
        )
      ),
      
    ),
    tabItem(
      "secondary_charts",
      "Secondary results go here",
      fluidRow(
        box("Dummy", width = 12, height = 400)
      ),
      fluidRow(
        box("Dummy", width = 4, height = 100),
        box("Dummy", width = 4, height = 100),
        box("Dummy", width = 4, height = 100)
      )
    )
  )
)

  
ui <- dashboardPage(header, sidebar, body)
server <- function(input, output){
  
  
  metrics_description <- reactive({
    
    case_when(
      input$metrics == "non_neutrality" ~ "**Non-neutrality**: The fraction of individuals who do not use the central (neutral) answer option (five on the eleven-point scale from zero to ten).",
      input$metrics == "avg_deviation_from_neutrality" ~ "**Average deviation from neutrality**: Shows resemblance to the concept of group polarization in psychology: How far is the average attitude away from the scale’s midpoint? This is operationalized by capturing the absolute value of the distance of the mean of the opinion distribution. For public opinion on immigration an increasing average deviation from neutrality means that individuals become either increasingly accepting or increasingly rejecting in their views. Most polarized in that sense would be a society where everyone has an extreme stance, be it 0 or 10. Also relates to the concept of group polarization from social psychology as it captures a shift of opinion positions “toward a more extreme point” (cf. Sunstein, 2003, p. 81). However, the most polarized society in this notion is also consensual in its extremity, and thus least polarized in the following notion.",
      input$metrics == "dispersion" ~ "**Dispersion**: Is measured by its mean absolute deviation. This is a good basic measure of polarization for typical survey response distributions on bounded scales (0-10 in the ESS). The maximum dispersion is when half of the individuals are on both extremes while the minimal dispersion appears for any consensus with all individuals having the same response. This measure is exactly as proposed by Bramson et al. (2016). For public opinion on immigration increasing dispersion means that individuals deviate more from the average attitude. Most polarized would be a society divided into two equally sized groups of people advocating total acceptance and total objection.",
      input$metrics == "moderate_divergence" ~ "**Moderate divergence**: Assessed by the absolute difference of group means of the moderate accepting group and the moderate opposing group, as described in Bramson et al. (2016). Lorenz (2017) analyzed the typical characteristics of ESS opinion distributions and pointed to the existence of five endogenous groups in ESS opinion distributions: The extreme left, the moderate left, the neutrals, the moderate right, and the extreme right. Per item, we operationalize similar groups, consisting of the 'full-on acceptors' (individuals with opinion 0), the 'moderate accepting group' (individuals with opinion 1-4), the 'neutrals' (individuals with opinion 5, the 'moderate opposing group' (individuals with opinion 6-9), and the 'full-on opponents' (individuals with opinion 10).",
      input$metrics == "moderate_group_consensus" ~ "**Moderate group conensus**: Based on the mean absolute deviation (MAD) of the two moderate groups. In contrast to dispersion, which we assess as MAD of the entire opinion distribution, the measurement of group consensus increases with decreasing dispersion in the two groups.",
      input$metrics == "moderate_size_parity" ~ "**Moderate size parity**: The relative size of the smaller group of moderates compared to the larger group. Hereby the mass of the smaller group is divided by the mass of the larger group. A size parity of 1 indicates equal size of moderate groups and thereby the maximum possible polarization in the sense of parity. This is a simplified version of the size parity measure proposed by Bramson et al. (2016)."
    )
  }
)
    
    
  
  observeEvent(input$switch, {
    if(input$switch) {
      output$countries <- renderUI({
        fluidRow(
          box(width = "100%",
            multiInput(
              inputId = "countries",
              label = " ",
              choices = sort(unique(dataset$country)),
              choiceNames = sort(unique(dataset$country)),
              choiceValues = sort(unique(dataset$country)),
              selected = "Germany"
          )))})
    }
    else {
      output$countries <- NULL
    }
  })
  
  variable_description <- reactive({
    case_when(
      input$variable == "imbgeco" ~ "**imbgeco**: Immigration bad (0) or good (10) for country's economy<br>",
      input$variable == "imueclt" ~ "**imueclt**: Country's cultural life undermined (0) or enriched (10) by immigrants<br>",
      input$variable == "imwbcnt" ~ "**imwbcnt**: Immigrants make country worse (0) or better (10) place to live \n"
    )
  })
  
  clean_string <- function(s) {
    str_to_title(str_replace_all(s, "_", " "))
  }
  
  plot_generator_countries <- reactive({
    dataset |> 
      pivot_longer(
        cols = non_neutrality:moderate_size_parity,
        names_to = "metric_name",
        values_to = "metric_value") |> 
      filter(
        metric_name %in% input$metrics,
        country %in% input$countries,
        variable %in% input$variable,
        between(essround, input$year[1], input$year[2])) |> 
    ggplot(aes(essround, metric_value, col = variable)) +
      facet_wrap(~country) +
      geom_point(size = 3) +
      geom_line(lwd = 1) +
      labs(x = "Year", y = glue("{clean_string(input$metrics)}"), title = "Country-level values") +
      scale_color_brewer(palette = "Set1") 
    }
  )
  
  
  
  plot_generator_overview <- reactive({
    dataset |> 
      select(-country) |> 
      pivot_longer(cols = non_neutrality:moderate_size_parity,
                   names_to = "metric", values_to = "value") |>
      filter(
        metric == input$metrics,
        variable %in% input$variable,
        between(essround, input$year[1], input$year[2])) |> 
      group_by(essround, variable, metric) |> 
      summarize(mean_value = mean(value, na.rm = T)) |> 
      ggplot(aes(essround, mean_value, col = variable)) +
      geom_point(size = 3) +
      geom_line(lwd = 1) +
      labs(x = "Year", y = glue("Mean {clean_string(input$metrics)}"), title = "ESS-wide mean values") +
      scale_color_brewer(palette = "Set1")
  })
  
  
  metrics_formula <- reactive({
    case_when(
      input$metrics == "non_neutrality" ~ "$$\\textrm{Non-neutrality} = 1-p$$",
      input$metrics == "avg_deviation_from_neutrality" ~ "$$\\textrm{Average deviation from neutrality} = \\frac{1}{5}|\\mu-5| \\\\ \\text{where } \\mu=\\sum_{i=0}^{10} i \\cdot p_i$$",
      input$metrics == "dispersion" ~ "$$\\textrm{Dispersion} = \\frac{1}{5} \\sum_{i=0}^{10} p_i \\cdot |i-\\mu|$$",
      input$metrics == "moderate_divergence" ~ "$$\\textrm{Moderate divergence} = \\frac{1}{10}|\\mu_{m[oppose]}-\\mu_{m[accept]}| \\\\
    \\textrm{with } \\\\
    p_{m[accept]} = p_1 + p_2 + p_3 + p_4 \\\\
    \\textrm{and } \\\\
    p_{m[oppose]} = p_6 + p_7 + p_8 + p_9 \\\\
    \\textrm{and } \\\\
    \\mu_{m[accept]} = \\frac{1 \\cdot p_1 + 2 \\cdot p_2 + 3 \\cdot p_3 + 4 \\cdot p_4}{p_{m[accept]}} \\\\
    \\textrm{and } \\\\
    \\mu_{m[oppose]} = \\frac{6 \\cdot p_6 + 7 \\cdot p_7 + 8 \\cdot p_8 + 9 \\cdot p_9}{p_{m[oppose]}}$$",
      input$metrics == "moderate_group_consensus" ~ "$$\\textrm{Moderate group consensus} = 1 - \\frac{1}{2}(MAD_{m[accept]} + MAD_{m[oppose]}) \\\\
    \\textrm{where} \\\\
    MAD_{m[accept]} = \\sum_{i=1}^{4} \\frac{p_j \\cdot |i-\\mu_{m[accept]}|}{\\sum_{j=1}^{4}p_j} \\\\
    \\textrm{and} \\\\
    MAD_{m[oppose]} = \\sum_{i=6}^{9} \\frac{p_j \\cdot |i-\\mu_{m[oppose]}|}{\\sum_{j=6}^{9}p_j}$$",
      input$metrics == "moderate_size_parity" ~ "$$\\textrm{Moderate size parity} = \\min \\lbrace \\frac{p_{m[accept]}}{p_{m[oppose]}}, \\frac{p_{m[oppose]}}{p_{m[accept]}} \\rbrace$$"
    )
  }
)
  
  
  observeEvent(input$switch, {
    if(input$switch) {
      output$p <- renderPlot({plot_generator_countries()})
    }
    else{
      output$p <- renderPlot({plot_generator_overview()})
    }
  })
  
  
  output$metric_description <- renderUI({markdown(metrics_description())})
  output$latex_output <- renderUI({withMathJax(metrics_formula())})
  output$variable_description <- renderUI({markdown(variable_description())})
}
shinyApp(ui, server)

