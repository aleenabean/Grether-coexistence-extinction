# app.R

# 1. Coexistence probability heatmap (Pr_Coexist)
# 2. Extinction probability heatmaps (Pr_Sp1_Extinct, Pr_Sp2_Extinct)
# 3. Outcome category probability heatmap (P(Outcome_ft == selected))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(bslib)
library(tidyr)

summary_path <- "Simulations_summary10.csv"
runs_path    <- "Simulation_results_24.csv"

# helper functions
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

# loading data
stop_if_missing(summary_path)
stop_if_missing(runs_path)

summary_df <- read_csv(summary_path, show_col_types = FALSE)
runs_df    <- read_csv(runs_path,    show_col_types = FALSE)

# ensuring correct columns (from summary file)
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

# -------------------------------------------------------------------
# Outcome category mapping + display names + descriptions
# -------------------------------------------------------------------

# Internal category keys (used in data)
outcome_choices_internal <- c("stasis", "2diverging", "divergence", "1chasing2",
                              "forked", "1diverging", "1converging")

# Display labels shown in the dropdown (maps internal -> nice label)
outcome_display_labels <- c(
  "stasis"      = "Stasis",
  "2diverging"  = "Species 2 Diverges",
  "divergence"  = "Both Species Diverge",
  "1chasing2"   = "Species 1 Chases Species 2",
  "forked"      = "Forked",
  "1diverging"  = "Species 1 Diverges",
  "1converging" = "Species 1 Converges"
)

# Descriptions shown below the dropdown when each category is selected
outcome_descriptions <- c(
  "stasis" = paste0(
    strong("Stasis:"), " Both Z and Mu of both species remained more or less the same ",
    "from beginning to end."
  ),
  "2diverging" = paste0(
    strong("Species 2 Diverges:"), " Species 2 diverged in Z, Mu, or both, becoming more ",
    "phenotypically distinct from Species 1. Species 1 remained relatively stable in its ",
    "values of Z and Mu from beginning to end."
  ),
  "divergence" = paste0(
    strong("Both Species Diverge:"), " Both Species 1 and Species 2 diverged in Z, Mu, or ",
    "both, becoming more phenotypically distinct from one another."
  ),
  "1chasing2" = paste0(
    strong("Species 1 Chases Species 2:"), " Species 2 diverged in Z, Mu, or both, while ",
    "Species 1 converged (became more phenotypically similar to) Species 2. This resulted in ",
    "Species 2 continuously evolving away from the Z and Mu of Species 1, while Species 1 evolved ",
    "in the same direction and remained phenotypically similar to Species 2. ",
    "Compare to predictions of the Red Queen hypothesis."
  ),
  "forked" = paste0(
    strong("Forked:"), " Either Species 1 or Species 2 (or both) diverged from itself, meaning its ",
    "Z-value and recognition function separated and it could no longer recognize conspecifics. ",
    "More likely to occur in Species 2 due to high costs of fights with Species 1. ",
    em("Not considered biologically plausible.")
  ),
  "1diverging" = paste0(
    strong("Species 1 Diverges:"), " Species 1 diverged in Z, Mu, or both, becoming more ",
    "phenotypically distinct from Species 2. Species 2 remained relatively stable in its ",
    "values of Z and Mu from beginning to end."
  ),
  "1converging" = paste0(
    strong("Species 1 Converges:"), " Species 1 converged in Z, Mu, or both, becoming more ",
    "phenotypically similar to Species 2. Species 2 remained relatively stable in its ",
    "values of Z and Mu from beginning to end."
  )
)

# Named vector for selectizeInput: names = display labels, values = internal keys
outcome_select_choices <- setNames(
  outcome_choices_internal,
  outcome_display_labels[outcome_choices_internal]
)

runs_df <- runs_df %>%
  mutate(
    Outcome_cat = case_when(
      Outcome_ft == "stasis" ~ "stasis",
      
      Outcome_ft %in% c("2diverging", "Z2diverging", "Mu2diverging") ~ "2diverging",
      
      Outcome_ft %in% c("divergence", "Z2Mu1diverging", "Z1Z2diverging",
                        "Z1Mu2diverging", "Mu1Mu2diverging") ~ "divergence",
      
      Outcome_ft %in% c("1chasing2", "Mu1chasingZ2", "Z1chasingMu2") ~ "1chasing2",
      
      Outcome_ft == "forked" ~ "forked",
      
      Outcome_ft %in% c("1diverging", "Z1diverging", "Mu1diverging") ~ "1diverging",
      
      Outcome_ft %in% c("1converging", "Mu1converging", "Z1converging") ~ "1converging",
      
      TRUE ~ NA_character_   # removes Z2converging and anything else not listed
    )
  ) %>%
  filter(!is.na(Outcome_cat))

# shared control values (refuge and InitialDiff)
fracs <- sort(unique(summary_df$FracRefuge))
initialdiff_choices <- sort(unique(summary_df$InitialDiff))

# fixed axis ordering for heatmap tiles
x_levels <- sort(unique(summary_df$FightingDiff), decreasing = TRUE)
y_levels <- sort(unique(summary_df$ResourceOverlap))

# precompute probabilities for PI categories
outcome_probs <- runs_df %>%
  count(InitialDiff, ResourceOverlap, FracRefuge, FightingDiff, Outcome_cat, name = "n") %>%
  group_by(InitialDiff, ResourceOverlap, FracRefuge, FightingDiff) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

# precompute n per (InitialDiff, FracRefuge) for summary-based heatmaps
summary_n <- runs_df %>%
  count(InitialDiff, FracRefuge, name = "n_runs")

# precompute n per (InitialDiff, FracRefuge) for outcome heatmaps (after category filtering)
outcome_n <- runs_df %>%
  count(InitialDiff, FracRefuge, name = "n_runs")

# UI text
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
    "Outcome category: grouped categories (PI-defined) summarizing the evolutionary/end-state outcome of a simulation run.",
    "In the heatmap, we show the probability (0 to 1) that the grouped category occurs for each parameter combination."
  )
)

# UI
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
    
    tooltip(
      sliderInput(
        "fr_idx",
        "Refuge habitat proportion: FracRefuge",
        min = min(fracs), max = max(fracs),
        value = min(fracs),
        step = NULL,
        animate = animationOptions(interval = 400, loop = TRUE)
      ),
      definition_text$FracRefuge,
      placement = "right"
    ),
    
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
      div(
        style = "padding: 10px;",
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
      div(
        style = "padding: 10px;",
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
      div(
        style = "padding: 10px 10px 0 10px;",
        p("This tab uses run-level outcomes and converts them into probabilities for each parameter combination."),
        p(definition_text$Outcome_ft)
      ),
      # Dropdown + description side by side — no card wrapping, no scrolling needed
      layout_columns(
        col_widths = c(3, 9),
        style = "padding: 0 10px 12px 10px; align-items: flex-start; overflow: visible;",
        
        div(
          tooltip(
            selectizeInput(
              "outcome",
              "Outcome category",
              choices = outcome_select_choices,
              selected = outcome_choices_internal[1],
              options = list(dropdownParent = "body", maxOptions = 10000)
            ),
            definition_text$Outcome_ft,
            placement = "right"
          )
        ),
        
        # Description always visible to the right of the dropdown
        uiOutput("outcome_description_box")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Probability that the selected outcome occurs"),
        plotOutput("heat_outcome", height = "620px")
      )
    ),
    
    nav_panel(
      "About",
      div(
        style = "padding: 12px;",
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
        h4("Outcome category definitions"),
        tags$ul(
          lapply(outcome_choices_internal, function(key) {
            tags$li(HTML(outcome_descriptions[[key]]))
          })
        ),
        tags$p(
          tags$em(
            tags$strong("Note \u2014 not plotted: "),
            "Species 2 Converges \u2014 Species 2 converged in Z, Mu, or both, becoming more ",
            "phenotypically similar to Species 1. Species 1 remained relatively stable in its ",
            "values of Z and Mu from beginning to end. Occurred only twice across all simulations; ",
            "probabilities not plotted."
          )
        ),
        h4("Interpretation"),
        p("All values shown in the heatmaps are probabilities between 0 and 1."),
        p("The app is intended for exploration and communication; interpretation should follow the assumptions and definitions in the paper."),
        
        hr(),
        
        p(
          tags$em("Made by Aleena Munshi."),
          " For questions or comments, contact ",
          tags$a("aleenamunshi001@g.ucla.edu", href = "mailto:aleenamunshi001@g.ucla.edu"),
          "."
        )
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  frac_val <- reactive({
    # snap input value to the nearest actual FracRefuge value in the data
    fracs[which.min(abs(fracs - input$fr_idx))]
  })
  
  # Render the description box for the selected outcome category
  output$outcome_description_box <- renderUI({
    req(input$outcome)
    desc_html <- outcome_descriptions[[input$outcome]]
    if (!is.null(desc_html)) {
      div(
        style = paste0(
          "margin-top: 0px; padding: 10px 14px; border-left: 3px solid #7AA2F7; ",
          "background: rgba(122,162,247,0.08); border-radius: 4px; font-size: 0.80em; ",
          "line-height: 1.6; min-height: 80px; overflow: visible;"
        ),
        HTML(desc_html)
      )
    }
  })
  
  # Helper: get n for summary-based heatmaps (runs that went into this slice)
  get_summary_n <- reactive({
    row <- summary_n %>%
      filter(
        InitialDiff == as.numeric(input$initialdiff),
        FracRefuge  == frac_val()
      )
    if (nrow(row) == 0) return(NULL)
    row$n_runs[1]
  })
  
  # Helper: get n for outcome heatmap
  get_outcome_n <- reactive({
    row <- outcome_n %>%
      filter(
        InitialDiff == as.numeric(input$initialdiff),
        FracRefuge  == frac_val()
      )
    if (nrow(row) == 0) return(NULL)
    row$n_runs[1]
  })
  
  # summary-based heatmaps
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
  
  # CHANGE 2: helper to build n= subtitle string
  n_subtitle <- function(n) {
    if (is.null(n)) return("n = unknown")
    paste0("n = ", formatC(n, format = "d", big.mark = ","), " simulation runs")
  }
  
  # coexistence heatmap
  output$heat_coexist <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Coexist)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    n <- get_summary_n()
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      scale_fill_continuous(limits = c(0, 1)) +
      labs(
        x        = "Difference in fighting ability (FightingDiff)",
        y        = "Resource overlap (ResourceOverlap)",
        fill     = "Probability (0 to 1)",
        title    = paste0("Coexistence probability | FracRefuge = ", frac_val(),
                          " | InitialDiff = ", input$initialdiff),
        subtitle = n_subtitle(n)
      ) +
      theme_midnight_plot()
  })
  
  # extinction heatmap for sp1
  output$heat_sp1 <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Sp1_Extinct)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    n <- get_summary_n()
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      scale_fill_continuous(limits = c(0, 1)) +
      labs(
        x        = "Difference in fighting ability (FightingDiff)",
        y        = "Resource overlap (ResourceOverlap)",
        fill     = "Probability (0 to 1)",
        title    = paste0("Species 1 extinction probability | FracRefuge = ", frac_val(),
                          " | InitialDiff = ", input$initialdiff),
        subtitle = n_subtitle(n)
      ) +
      theme_midnight_plot()
  })
  
  # extinction heatmap sp2
  output$heat_sp2 <- renderPlot({
    dat <- summary_slice() %>% mutate(val = Pr_Sp2_Extinct)
    validate(need(nrow(dat) > 0, "No data matches the current settings."))
    n <- get_summary_n()
    
    ggplot(dat, aes(x = x, y = y, fill = val)) +
      geom_tile() +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      scale_fill_continuous(limits = c(0, 1)) +
      labs(
        x        = "Difference in fighting ability (FightingDiff)",
        y        = "Resource overlap (ResourceOverlap)",
        fill     = "Probability (0 to 1)",
        title    = paste0("Species 2 extinction probability | FracRefuge = ", frac_val(),
                          " | InitialDiff = ", input$initialdiff),
        subtitle = n_subtitle(n)
      ) +
      theme_midnight_plot()
  })
  
  # outcome heatmap (PI categories) with fixed axes
  output$heat_outcome <- renderPlot({
    req(input$outcome)
    
    # Get the display label for the plot title
    display_label <- outcome_display_labels[input$outcome]
    n <- get_outcome_n()
    
    dat <- outcome_probs %>%
      filter(
        InitialDiff == as.numeric(input$initialdiff),
        FracRefuge == frac_val(),
        Outcome_cat == input$outcome
      ) %>%
      tidyr::complete(
        FightingDiff = x_levels,
        ResourceOverlap = y_levels,
        fill = list(prob = 0)
      ) %>%
      mutate(
        x = factor(FightingDiff, levels = x_levels),
        y = factor(ResourceOverlap, levels = y_levels)
      )
    
    validate(need(nrow(dat) > 0, "No outcome data matches the current settings."))
    
    ggplot(dat, aes(x = x, y = y, fill = prob)) +
      geom_tile() +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      scale_fill_continuous(limits = c(0, 1)) +
      labs(
        x        = "Difference in fighting ability (FightingDiff)",
        y        = "Resource overlap (ResourceOverlap)",
        fill     = "Probability (0 to 1)",
        title    = paste0("P(Outcome = ", display_label, ") | FracRefuge = ", frac_val(),
                          " | InitialDiff = ", input$initialdiff),
        subtitle = n_subtitle(n)   # CHANGE 2: n= added here
      ) +
      theme_midnight_plot()
  })
}
shinyApp(ui, server)


