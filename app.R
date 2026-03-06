# app.R

# 1. Coexistence probability heatmap (Pr_Coexist)
# 2. Extinction probability heatmaps (Pr_Sp1_Extinct, Pr_Sp2_Extinct)
# 3. Outcome category probability heatmap (P(Outcome_ft == selected))

#mean trait values change over time (which parameters? = fighting diff, frac refuge, resource overlap)
#average z (mu and sigma) value at each time point
#hope: diff simulations for each set of parameters follow a direction (if not pick specific examples)

#make comparison between original model, neural network model, same parameter values, how likely is it that species would D/C
#keep it to trait diff 1 or 2 to match og model simulations ; compare with eugene's

#width of 1 for gaussian function works for now in new model works well

# Data files needed:
# - Simulations_summary10.csv
# - Simulation_results_24.csv


library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(bslib)

summary_path <- "Simulations_summary10.csv"
runs_path    <- "Simulation_results_24.csv"

#helper functions
stop_if_missing <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path, call. = FALSE)
}

theme_midnight_plot <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key        = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

#loading data
stop_if_missing(summary_path)
stop_if_missing(runs_path)

summary_df <- read_csv(summary_path, show_col_types = FALSE)
runs_df    <- read_csv(runs_path,    show_col_types = FALSE)

#ensuring correct columns (from summary file)
req_summary <- c("InitialDiff", "FracRefuge", "ResourceOverlap", "FightingDiff",
                 "Pr_Coexist", "Pr_Sp1_Extinct", "Pr_Sp2_Extinct")
miss_summary <- setdiff(req_summary, names(summary_df))
if (length(miss_summary) > 0) {
  stop("Simulations_summary10.csv is missing columns: ",
       paste(miss_summary, collapse = ", "),
       call. = FALSE)
}
# same but for outcome heatmap
req_runs <- c("InitialDiff", "FracRefuge", "ResourceOverlap", "FightingDiff", "Outcome_ft")
miss_runs <- setdiff(req_runs, names(runs_df))
if (length(miss_runs) > 0) {
  stop("Simulation_results_24.csv is missing columns: ",
       paste(miss_runs, collapse = ", "),
       call. = FALSE)
}

#shared control values (refuge and InitialDiff)
fracs <- sort(unique(summary_df$FracRefuge))
initialdiff_choices <- sort(unique(summary_df$InitialDiff))

#fixed axis ordering for heatmap tiles
x_levels <- sort(unique(summary_df$FightingDiff))
y_levels <- sort(unique(summary_df$ResourceOverlap))

#precompute probabilities for Outcome_ft
#1.for each parameter combination (InitialDiff, ResourceOverlap, FracRefuge,
#FightingDiff) and each outcome categorycount how many simulation runs
# produced that outcome (n)
#2. within each parameter combination,  divide by the total number of runs
#for that combination (sum(n)) to get a probability
outcome_probs <- runs_df %>%
  count(InitialDiff, ResourceOverlap, FracRefuge, FightingDiff, Outcome_ft, name = "n") %>%
  group_by(InitialDiff, ResourceOverlap, FracRefuge, FightingDiff) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

outcome_choices <- sort(unique(outcome_probs$Outcome_ft))

#UI//clear defintions for ecology terms
definition_text <- list(
  FracRefuge = paste(
    "Refuge habitat proportion (FracRefuge): the fraction of habitat that serves as a refuge for Species 2.",
    "Interpretation: higher values mean Species 2 has more places where it is less affected by interference from Species 1."
  ),
  ResourceOverlap = paste(
    "Resource overlap (ResourceOverlap): how similar the two species are in their resource use (often interpreted as diet overlap).",
    "0 means no overlap; 1 means complete overlap."
  ),
  FightingDiff = paste(
    "Difference in fighting ability (FightingDiff): how large the advantage is in direct interference/fighting between the species.",
    "Larger magnitude means fights are more one-sided."
  ),
  InitialDiff = paste(
    "Scenario setting (InitialDiff): a discrete simulation setting controlling the initial difference used in the design.",
    "Use this as a scenario switch; keep it fixed while interpreting how the heatmap changes with other parameters."
  ),
  Pr_Coexist = "Probability of coexistence (Pr_Coexist): estimated probability (0 to 1) that both species persist together.",
  Pr_Sp1_Extinct = "Probability Species 1 goes extinct (Pr_Sp1_Extinct): estimated probability (0 to 1) that Species 1 is lost.",
  Pr_Sp2_Extinct = "Probability Species 2 goes extinct (Pr_Sp2_Extinct): estimated probability (0 to 1) that Species 2 is lost.",
  Outcome_ft = paste(
    "Outcome category (Outcome_ft): a categorical label summarizing the evolutionary/end-state outcome of a simulation run.",
    "In the heatmap, we show the probability (0 to 1) that Outcome_ft equals the selected category for each parameter combination."
  )
)

#UI
ui <- page_sidebar(
  title = "Coexistence and extinction heatmaps (Grether model results)",
  theme = bs_theme(
    version = 5,
    bootswatch = "cyborg",
    primary = "#7AA2F7",
    bg = "#0B1020",
    fg = "#E7ECF3"
  ),
  
  sidebar = sidebar(
    width = 420,
    
    h4("What you are controlling"),
    p("These settings apply to all three graph tabs."),
    
    #scenario
    tooltip(
      selectInput(
        "initialdiff",
        "Scenario setting: InitialDiff",
        choices = initialdiff_choices,
        selected = initialdiff_choices[1]
      ),
      definition_text$InitialDiff,
      placement = "right"
    ),
    
    #refuge slider (discrete)
    tooltip(
      sliderInput(
        "fr_idx",
        "Refuge habitat proportion: FracRefuge",
        min = 1, max = length(fracs), value = 1, step = 1,
        animate = animationOptions(interval = 900, loop = TRUE)
      ),
      definition_text$FracRefuge,
      placement = "right"
    ),
    textOutput("frac_label"),
    
    hr(),
    
    accordion(
      accordion_panel(
        "How to read the heatmaps",
        p(strong("Every tile"), " represents one parameter combination."),
        tags$ul(
          tags$li(strong("x-axis"), ": Difference in fighting ability (FightingDiff)."),
          tags$li(strong("y-axis"), ": Resource overlap (ResourceOverlap)."),
          tags$li(strong("color"), ": a probability from 0 to 1 (higher means more likely).")
        ),
        p("Use the FracRefuge slider to switch between slices of the parameter space.")
      ),
      accordion_panel(
        "Definitions of the axis variables",
        tags$ul(
          tags$li(strong("Difference in fighting ability (FightingDiff): "), definition_text$FightingDiff),
          tags$li(strong("Resource overlap (ResourceOverlap): "), definition_text$ResourceOverlap),
          tags$li(strong("Refuge habitat proportion (FracRefuge): "), definition_text$FracRefuge)
        )
      )
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Coexistence probability heatmap",
      div(style = "padding: 10px;",
          p(strong("Color meaning: "), definition_text$Pr_Coexist),
          p("Axes are fixed: x = FightingDiff, y = ResourceOverlap. Use FracRefuge to change the slice.")
      ),
      card(
        full_screen = TRUE,
        card_header("Probability that both species persist together (coexistence)"),
        plotOutput("heat_coexist", height = "720px")
      )
    ),
    
    nav_panel(
      "Extinction probability heatmaps",
      div(style = "padding: 10px;",
          p("These two heatmaps use the same axes and FracRefuge slider."),
          tags$ul(
            tags$li(definition_text$Pr_Sp1_Extinct),
            tags$li(definition_text$Pr_Sp2_Extinct)
          )
      ),
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          full_screen = TRUE,
          card_header("Probability Species 1 goes extinct"),
          plotOutput("heat_sp1", height = "650px")
        ),
        
        card(
          full_screen = TRUE,
          card_header("Probability Species 2 goes extinct"),
          plotOutput("heat_sp2", height = "650px")
        )
      )
    ),
    
    nav_panel(
      "Outcome category probability heatmap",
      div(style = "padding: 10px;",
          p("This tab uses run-level outcomes (Outcome_ft) and converts them into probabilities for each parameter combination."),
          p(definition_text$Outcome_ft)
      ),
      card(
        card_header("Choose an outcome category to map"),
        div(style = "padding: 10px;",
            tooltip(
              selectInput("outcome", "Outcome category: Outcome_ft", choices = outcome_choices, selected = outcome_choices[1]),
              definition_text$Outcome_ft,
              placement = "right"
            )
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Probability that the selected outcome occurs"),
        plotOutput("heat_outcome", height = "720px")
      )
    ),
    
    nav_panel(
      "About",
      div(style = "padding: 12px;",
          h4("Source"),
          p(
            "This app visualizes simulation results from the model described in the ",
            tags$a(
              "Grether & Okamoto (2022) paper",
              href = "https://sites.lifesci.ucla.edu/eeb-gretherlab/wp-content/uploads/sites/146/2022/09/Grether-Okamoto-2022.pdf",
              target = "_blank"
            ),
            "."
          ),
          h4("Interpretation"),
          p("All values shown in the heatmaps are probabilities between 0 and 1."),
          p("The app is intended for exploration and communication; interpretation should follow the assumptions and definitions in the paper.")
      )
    )
  )
)

#server
server <- function(input, output, session) {
  
  frac_val <- reactive(fracs[input$fr_idx])
  
  output$frac_label <- renderText({
    paste0("Currently showing FracRefuge = ", frac_val(), " (", definition_text$FracRefuge, ")")
  })
  
  #summary-based heatmaps
  summary_slice <- reactive({
    summary_df %>%
      filter(
        InitialDiff == as.numeric(input$initialdiff),
        FracRefuge == frac_val()
      ) %>%
      mutate(
        x = factor(FightingDiff, levels = x_levels),
        y = factor(ResourceOverlap, levels = y_levels)
      )
  })
  
  #coexistance heatmap
  output$heat_coexist <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Coexist)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      labs(
        x = "Difference in fighting ability (FightingDiff)",
        y = "Resource overlap (ResourceOverlap)",
        fill = "Probability (0 to 1)",
        title = paste0("Coexistence probability | FracRefuge = ", frac_val(), " | InitialDiff = ", input$initialdiff)
      ) +
      theme_midnight_plot()
  })
  
  #extinction heatmap for sp1
  output$heat_sp1 <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Sp1_Extinct)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      labs(
        x = "Difference in fighting ability (FightingDiff)",
        y = "Resource overlap (ResourceOverlap)",
        fill = "Probability (0 to 1)",
        title = paste0("Species 1 extinction probability | FracRefuge = ", frac_val(), " | InitialDiff = ", input$initialdiff)
      ) +
      theme_midnight_plot()
  })
  #extinction heatmap sp2
  output$heat_sp2 <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Sp2_Extinct)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      labs(
        x = "Difference in fighting ability (FightingDiff)",
        y = "Resource overlap (ResourceOverlap)",
        fill = "Probability (0 to 1)",
        title = paste0("Species 2 extinction probability | FracRefuge = ", frac_val(), " | InitialDiff = ", input$initialdiff)
      ) +
      theme_midnight_plot()
  })
  
  #outcome heatmap FIXXX THISSS
  #Blank tiles in the outcome heatmap happen because some outcome
  # categories never occur in some parameter combinations
  #(so you have missing rows, not zeros).
  #Fix by “filling missing outcomes with probability 0”
  output$heat_outcome <- renderPlot({
    req(input$outcome)
    
    dat <- outcome_probs %>%
      filter(
        InitialDiff == as.numeric(input$initialdiff),
        FracRefuge == frac_val(),
        Outcome_ft == input$outcome
      ) %>%
      mutate(
        x = factor(FightingDiff, levels = x_levels),
        y = factor(ResourceOverlap, levels = y_levels)
      )
    
    validate(need(nrow(dat) > 0, "No outcome data matches the current settings."))
    
    ggplot(dat, aes(x = x, y = y, fill = prob)) +
      geom_tile() +
      labs(
        x = "Difference in fighting ability (FightingDiff)",
        y = "Resource overlap (ResourceOverlap)",
        fill = "Probability (0 to 1)",
        title = paste0("P(Outcome_ft = ", input$outcome, ") | FracRefuge = ", frac_val(), " | InitialDiff = ", input$initialdiff)
      ) +
      theme_midnight_plot()
  })
}

shinyApp(ui, server)




