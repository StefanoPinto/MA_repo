library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(DT)
library(naniar)
library(countrycode)

#setwd("C:/Users/Stef/Desktop/Uni/CU/semester_4/ma/shiny_app/stef_ma")
theme_set(theme_bw(base_size = 20))

select_and_clean <- function(dataset) {
  dataset_clean <- dataset |> 
    select(essround, cntry, imbgeco, imueclt, imwbcnt, anweight, pspwght, pweight) |> 
    replace_with_na(replace = list(
      imbgeco = c(77, 88, 99),
      imueclt = c(77, 88, 99),
      imwbcnt = c(77, 88, 99))) |> 
    mutate(essround = 2000 + 2 * essround) |> 
    na.omit()
  
  return(dataset_clean)
}

original_data <- read_csv("data2.csv")
original_data <- select_and_clean(original_data)
original_data <- original_data |> 
  rename(country = cntry)
original_data <- original_data |> 
  mutate(country = countrycode(sourcevar = country,
                             origin = "iso2c",
                             destination = "country.name.en"),
         .keep = "unused")

original_data[which(is.na(original_data$country)), "country"] <- "Kosovo"

original_data <- original_data |> 
  mutate(anweight = pspwght * pweight)

weighted_ratios <- function(dataset, variable) {
  
  if(nrow(dataset) == 0) {
    return(NA)
  }
  # calculate weighted ratios
  weighted_counts <- dataset %>%
    group_by({{ variable }}) %>%
    summarise(weighted_n = sum(anweight)) 
  
  available_rounds <- pull(unique(weighted_counts[,1]))
  
  n_total_weighted <- sum(weighted_counts)
  
  weighted_ratios <- weighted_counts$weighted_n / n_total_weighted
  names(weighted_ratios) <- available_rounds
  
  # Returns a vector of n ratios / probabilities, one for each available round
  return(weighted_ratios)
}


metrics_dataset <- read_csv("metrics.csv")

pca_metric_dataset <- read_csv("pca_metric_loadings_variance.csv")
pca_slope_dataset <- read_csv("pca_metric_slope_variance.csv")


metrics_dataset <- metrics_dataset |> 
  left_join(pca_metric_dataset) |> 
  left_join(pca_slope_dataset) |> 
  select(-c(loading_pc1_imbgeco, loading_pc1_imueclt, loading_pc1_imwbcnt))


metrics_dataset <- metrics_dataset |> mutate(across(
  .cols = non_neutrality : slope_expl_var_pc1,
  .fns = \(var) round(var, 4)))


header <- dashboardHeader(
  title = "ESS Opinion Polarization Exploratory Tool",
  titleWidth = 500)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Exploratory tools", tabName = "main_country",
             menuSubItem("Overview + trend", tabName = "main_charts"),
             menuSubItem("Ranking", tabName = "ranking"),
             menuSubItem("Country-focus", tabName = "country_focus"))
  )
)

body <- dashboardBody(
  tags$style(HTML("
    .box.box-solid.box-info>.box-header {
      background-color: #00204d !important;
    }

    .skin-blue .main-header .logo {
      background-color: #00204d;
    }
  ")),
  tabItems(
    tabItem("country_focus",
            fluidRow(
              box(
                status = "info", solidHeader = TRUE, width = 4,
                selectInput(
                  inputId = "country_id_300",
                  label = "Select Country:",
                  choices = unique(metrics_dataset$country),
                  multiple = FALSE,
                  selected = "Germany"
                )
              ),
              box(
                status = "info", solidHeader = TRUE, width = 4,
                selectInput(
                  inputId = "year_id_300",
                  label = "Select Year:",
                  choices = unique(metrics_dataset$essround),
                  multiple = FALSE,
                  selected = "2020"
                )
              ),
              box(
                status = "info", solidHeader = TRUE, width = 4,
                selectInput(
                  inputId = "variable_id_300",
                  label = "Select variable:",
                  choices = unique(metrics_dataset$variable),
                  multiple = FALSE,
                  selected = "imbgeco"
                )
              )
            ),
            fluidRow(
              box(
                plotOutput("plot_country_specific"),
                width = 12
              ),
              box(
                plotOutput("opinions_hist"),
                width = 12)
            )),
    tabItem("ranking",
            fluidRow(
              box(
                pickerInput(
                  inputId = "Id088",
                  label = "Select year",
                  choices = unique(metrics_dataset$essround),
                  options = pickerOptions(container = "body",
                                          style = "btn-outline-primary"),
                  width = "100%"
                ),
                pickerInput(
                  inputId = "Id089",
                  label = "Select variable",
                  choices = unique(metrics_dataset$variable),
                  options = pickerOptions(container = "body",
                                          style = "btn-outline-primary"),
                  width = "100%"
                ),
                status = "info", solidHeader = TRUE
              )
            ),
            mainPanel(
              DT::dataTableOutput("mytable"), width = 12
            )
    ),
    tabItem(
      "main_charts",
      " ",
      fluidRow(
        box(
          width = "100%",
          plotOutput("p")
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
        box(title = "Metric", status = "info", solidHeader = TRUE,
            selectInput(
              "metrics",
              " ",
              choices = c(
                "non_neutrality",
                "avg_deviation_from_neutrality",
                "dispersion",
                "moderate_divergence",
                "moderate_group_consensus",
                "moderate_size_parity",
                "expl_var_pc1"),
              multiple = FALSE,
              selected = "non_neutrality"),
            
            uiOutput(outputId = "metric_description"),
            uiOutput("latex_output"),width = 8
        ),
        box(title = "Variable(s)", status = "info", solidHeader = TRUE,
            width = 4,
            checkboxGroupButtons(
              inputId = "variable",
              label = " ",
              choices = unique(metrics_dataset$variable),
              selected = "imbgeco"),
            uiOutput(
              outputId = "variable_description")),
        
        
        box(title = "Year(s)", status = "info", solidHeader = TRUE,
            width = 4,
            sliderInput(
              "year",
              " ",
              min = as.integer(min(metrics_dataset$essround)),
              max = as.integer(max(metrics_dataset$essround)),
              value = c(as.integer(min(metrics_dataset$essround)), as.integer(max(metrics_dataset$essround))),
              step = 2,
              sep = ""
            )
        )
      ),
      
    ),
    tabItem(
      "secondary_charts",
      "Static plots of the main results go here",
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
      input$metrics == "moderate_size_parity" ~ "**Moderate size parity**: The relative size of the smaller group of moderates compared to the larger group. Hereby the mass of the smaller group is divided by the mass of the larger group. A size parity of 1 indicates equal size of moderate groups and thereby the maximum possible polarization in the sense of parity. This is a simplified version of the size parity measure proposed by Bramson et al. (2016).",
      input$metrics == "expl_var_pc1" ~ "**Explained variance of PC1**: The explained variance of the first principal component derived from a Principal Component Analysis (PCA) using **imbgeco**, **imueclt** & **imwbcnt**"
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
              choices = sort(unique(metrics_dataset$country)),
              choiceNames = sort(unique(metrics_dataset$country)),
              choiceValues = sort(unique(metrics_dataset$country)),
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
    metrics_dataset |> 
      pivot_longer(
        cols = non_neutrality:expl_var_pc1,
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
      labs(
        x = "Year",
        y = glue("{clean_string(input$metrics)}"),
        title = "Country-level values",
        caption = "Note that if the explained variance of PC 1 is picked as metric, the values are perfectly overlapping. This make sense, as this metric is derived from all three variables in unison.") +
      scale_color_brewer(palette = "Set1") 
    }
  )
  
  plot_generator_overview <- reactive({
    metrics_dataset |> 
      select(-country) |> 
      pivot_longer(cols = non_neutrality:expl_var_pc1,
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
      labs(
        x = "Year",
        y = glue("Mean {clean_string(input$metrics)}"),
        title = "ESS-wide mean values",
        caption = "Note that if the explained variance of PC 1 is picked as metric, the values are perfectly overlapping. This make sense, as this metric is derived from all three variables in unison.") +
      scale_color_brewer(palette = "Set1")
  })
  
  
  plot_generator_country_specific <- reactive({
    
    country_df <- metrics_dataset |> 
      filter(
        country == input$country_id_300,
        essround == input$year_id_300,
        variable == input$variable_id_300) |> 
      select(non_neutrality : expl_var_pc1) |> 
      gather(metric, value) |> 
      mutate(scope = "country")
    
    
    europe_means <- metrics_dataset |> 
      filter(
        essround == input$year_id_300,
        variable == input$variable_id_300) |> 
      select(non_neutrality : expl_var_pc1) |> 
      gather(metric, value) |> 
      group_by(metric) |> 
      summarize(value = mean(value, na.rm = T)) |> 
      mutate(scope = "europe_wide_mean")
    
    merged_df <- bind_rows(country_df, europe_means) 
    
    ggplot(merged_df, aes(metric, value)) +
      geom_col(aes(fill = scope), position = "dodge") +
      coord_flip() +
      labs(
        x = "",
        y = "Value",
        title = glue("{input$country_id_300} - {input$year_id_300} - {input$variable_id_300}"),
        caption = "Note: If no country bars (green) are displayed, it means that for that particular year-country combination, no data is available\n Also, note that the value of the explained variance of PC1 does not change when the variable is changed. This make sense, as that value is derived from all three variables in unison.") +
      scale_x_discrete(labels = c(
        "Average deviation from neutrality",
        "Dispersion",
        "Explained variance PC1",
        "Moderate divergence",
        "Moderate group consensus",
        "Moderate size parity",
        "Non-neutrality")) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      scale_fill_brewer(palette = "Set2", "", labels = c("Country","Europe-wide average\n(for that year)"))
  })
  
  
  
  plot_generator_country_specific_hist <- reactive({
    
    original_data_sub <- original_data |> 
      filter(
        country == input$country_id_300,
        essround == input$year_id_300)
    
    ratios <- weighted_ratios(original_data_sub, !!sym(input$variable_id_300))
    df <- tibble(opinion_value = as.integer(names(ratios)), ratio = ratios)
    ggplot(df, aes(opinion_value, ratio)) +
      geom_col(fill = "steelblue") +
      scale_x_continuous(breaks = 0:10) +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Likert-scale value", y = "",
           caption = "Note: If no bars (steelblue) are displayed, it means that for that particular year-country combination, no data is available")
    }
  )
  
  
  output$opinions_hist <- renderPlot({plot_generator_country_specific_hist()})
  
  
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
      input$metrics == "moderate_size_parity" ~ "$$\\textrm{Moderate size parity} = \\min \\lbrace \\frac{p_{m[accept]}}{p_{m[oppose]}}, \\frac{p_{m[oppose]}}{p_{m[accept]}} \\rbrace$$",
      input$metrics == "expl_var_pc1" ~ ""
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
  
  
  output$plot_country_specific <- renderPlot({plot_generator_country_specific()})
  
  output$mytable <- DT::renderDataTable(metrics_dataset |> filter(essround == input$Id088, variable == input$Id089) |> select(c(country, non_neutrality:expl_var_pc1)) ,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
  
  output$metric_description <- renderUI({markdown(metrics_description())})
  output$latex_output <- renderUI({withMathJax(metrics_formula())})
  output$variable_description <- renderUI({markdown(variable_description())})
}
shinyApp(ui, server)





