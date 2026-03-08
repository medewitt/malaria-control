options(repos = c(
  mrcide = "https://mrc-ide.r-universe.dev",
  CRAN = "https://repo.r-wasm.org"
))

library(shiny)
library(bslib)
library(malariasimulation)
library(ggplot2)

# --- Configuration & Costs ---
BUDGET_CAP <- 50000
COST_NET <- 4.50 / 2 # Per person (2 people per net)
COST_IRS <- 7.00 # Per person
COST_VAX <- 25.00 # Per child (simplified for this model)
COST_TREAT <- 1.00 # Per clinical case

ui <- page_sidebar(
  title = "Malaria Control Strategy Lab",
  sidebar = sidebar(
    width = 350,
    h4("Global Budget: $50,000"),
    span(textOutput("budget_status"), style = "font-weight: bold;"),
    hr(),

    # Strategy 1: Vector Control
    accordion(
      accordion_panel(
        "Vector Control (Nets & Spray)",
        sliderInput("net_cov", "Bed Net Coverage:", 0, 1, 0.4),
        sliderInput("irs_cov", "IRS (Spraying) Coverage:", 0, 1, 0)
      ),
      # Strategy 2: Medical Intervention
      accordion_panel(
        "Medical (Vaccine & Drugs)",
        sliderInput("vax_cov", "Vaccine Coverage (R21):", 0, 1, 0),
        sliderInput("treat_seek", "Treatment Seeking Rate:", 0, 1, 0.3)
      )
    ),
    input_task_button("run_sim", "Run Simulation")
  ),
  layout_columns(
    card(
      card_header("Prevalence Over Time ($PfPR_{2-10}$)"),
      plotOutput("prev_plot")
    ),
    card(
      card_header("Budget Breakdown"),
      tableOutput("budget_table")
    ),
    col_widths = c(8, 4)
  )
)

server <- function(input, output) {
  # Calculate costs reactively
  costs <- reactive({
    pop <- 10000
    net_total <- input$net_cov * pop * COST_NET
    irs_total <- input$irs_cov * pop * COST_IRS
    vax_total <- input$vax_cov * (pop * 0.2) * COST_VAX # simplified: 20% are kids
    treat_total <- input$treat_seek * pop * 0.5 * COST_TREAT # placeholder estimate

    total <- net_total + irs_total + vax_total + treat_total

    data.frame(
      Category = c("Bed Nets", "IRS", "Vaccines", "Treatment", "TOTAL"),
      Cost = c(net_total, irs_total, vax_total, treat_total, total)
    )
  })

  output$budget_status <- renderText({
    spent <- tail(costs()$Cost, 1)
    remaining <- BUDGET_CAP - spent
    paste0(
      "Remaining: $", round(remaining, 2),
      if (remaining < 0) " (OVER BUDGET!)" else ""
    )
  })

  output$budget_table <- renderTable(
    {
      costs()
    },
    digits = 0
  )

  # Simulation Logic
  sim_results <- eventReactive(input$run_sim, {
    # 1. Setup parameters and equilibrium (so we start from endemic prevalence)
    params <- get_parameters(list(human_population = 10000))
    params <- set_equilibrium(parameters = params, init_EIR = 20)
    # 2. Set drugs so clinical treatment can use drug index 1 (AL = artemether-lumefantrine)
    params <- set_drugs(params, list(AL_params))
    # 3. Apply interventions based on UI inputs
    params <- set_bednets(
      params,
      timesteps = 100,
      coverages = input$net_cov,
      retention = 5 * 365,
      dn0 = matrix(0.352, nrow = 1, ncol = 1),
      rn = matrix(0.568, nrow = 1, ncol = 1),
      rnm = matrix(0.24, nrow = 1, ncol = 1),
      gamman = 2.226 * 365
    )
    # IRS decay params: 1 timestep, 1 species (gambiae). Values from IRS decay model (Sherrard-Smith et al.).
    params <- set_spraying(
      params,
      timesteps = 100,
      coverages = input$irs_cov,
      ls_theta = matrix(2.5, nrow = 1, ncol = 1),
      ls_gamma = matrix(0.05, nrow = 1, ncol = 1),
      ks_theta = matrix(2.5, nrow = 1, ncol = 1),
      ks_gamma = matrix(0.05, nrow = 1, ncol = 1),
      ms_theta = matrix(2.5, nrow = 1, ncol = 1),
      ms_gamma = matrix(0.05, nrow = 1, ncol = 1)
    )
    # Mass PEV (R21): one round at t=100, target 5mo–50yr, one booster 12 months after primary
    params <- set_mass_pev(
      params,
      profile = r21_profile,
      timesteps = 100,
      coverages = input$vax_cov,
      min_ages = 5 * 30, # 5 months (in days)
      max_ages = 50 * 365, # 50 years (in days)
      min_wait = 0,
      booster_spacing = 12 * 30, # 12 months after primary series
      booster_coverage = matrix(0.95),
      booster_profile = list(r21_booster_profile)
    )
    # Some malariasimulation code expects 'pars' in scope when run inside Shiny reactive
    pars <- params
    params <- set_clinical_treatment(parameters = params, drug = 1, timesteps = 1, coverages = input$treat_seek)

    # 4. Run
    run_simulation(timesteps = 500, parameters = params)
  })

  output$prev_plot <- renderPlot({
    res <- sim_results()
    ggplot(res, aes(x = timestep, y = n_detect_lm_730_3650 / n_age_730_3650)) +
      geom_line(color = "#2c3e50", size = 1) +
      labs(x = "Days", y = "Prevalence (2-10 yrs)") +
      theme_minimal()
  })
}

shinyApp(ui, server)
