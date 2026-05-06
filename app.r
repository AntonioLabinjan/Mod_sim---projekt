# ============================================================
#  MODELIRANJE I SIMULACIJE — Briškolaškai Casino Simulator
#  Shiny Web Application
# ============================================================

library(shiny)
library(shinyjs)
library(ggplot2)

# ── GAME LOGIC ──────────────────────────────────────────────

play_round <- function(bet, suits, cards_per_suit, n_cards_drawn, init_pts) {
  deck_suits    <- rep(1:suits, each = cards_per_suit)
  briscola_suit <- sample(1:suits, 1)
  shuffled      <- sample(deck_suits)
  hand          <- shuffled[1:n_cards_drawn]
  hits          <- sum(hand == briscola_suit)
  misses        <- n_cards_drawn - hits
  pts           <- init_pts + hits * 10 - misses * 10
  mult          <- pts / init_pts
  profit        <- (mult - 1) * bet
  list(hits = hits, profit = profit, mult = mult)
}

simulate_strategy <- function(
    strategy_name, n_sims, flat_bet, kelly_fraction,
    max_bet, start_bankroll,
    suits = 4, cards_per_suit = 10, n_cards_drawn = 10, init_pts = 120
) {
  bankroll   <- numeric(n_sims + 1)
  net_profit <- numeric(n_sims)
  hits_vec   <- numeric(n_sims)
  bets_vec   <- numeric(n_sims)

  bankroll[1] <- start_bankroll
  current_bet <- flat_bet

  for (i in seq_len(n_sims)) {
    bet <- switch(strategy_name,
                  "Flat"            = flat_bet,
                  "Martingale"      = current_bet,
                  "Anti-Martingale" = current_bet,
                  "Kelly"           = max(1, bankroll[i] * kelly_fraction))

    bet <- min(bet, bankroll[i], max_bet)
    bets_vec[i] <- bet

    if (bankroll[i] <= 0) {
      bankroll[(i + 1):(n_sims + 1)] <- 0
      break
    }

    res <- play_round(bet, suits, cards_per_suit, n_cards_drawn, init_pts)

    bankroll[i + 1] <- bankroll[i] + res$profit
    net_profit[i]   <- res$profit
    hits_vec[i]     <- res$hits

    if (strategy_name == "Martingale") {
      current_bet <- if (res$profit < 0) min(current_bet * 2, max_bet) else flat_bet
    } else if (strategy_name == "Anti-Martingale") {
      current_bet <- if (res$profit > 0) min(current_bet * 2, max_bet) else flat_bet
    }
  }

  list(name = strategy_name, bank = bankroll, prof = net_profit,
       hits = hits_vec, bets = bets_vec)
}

# Theoretical expected value (hypergeometric)
calc_rtp <- function(suits, cards_per_suit, n_cards_drawn, init_pts) {
  N <- suits * cards_per_suit
  K <- cards_per_suit
  n <- n_cards_drawn
  exp_hits  <- n * K / N
  exp_misses <- n - exp_hits
  exp_mult  <- (init_pts + exp_hits * 10 - exp_misses * 10) / init_pts
  exp_mult * 100
}

# ── THEME ───────────────────────────────────────────────────

casino_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#0d1117", colour = NA),
      panel.background = element_rect(fill = "#161b22", colour = NA),
      panel.grid.major = element_line(colour = "#30363d"),
      panel.grid.minor = element_blank(),
      text             = element_text(colour = "#e6edf3", family = "sans"),
      axis.text        = element_text(colour = "#8b949e"),
      axis.title       = element_text(colour = "#c9d1d9"),
      plot.title       = element_text(colour = "#f0f6fc", size = 15, face = "bold"),
      plot.subtitle    = element_text(colour = "#8b949e", size = 11),
      legend.background = element_rect(fill = "#161b22", colour = NA),
      legend.text       = element_text(colour = "#c9d1d9"),
      legend.title      = element_text(colour = "#8b949e"),
      strip.text        = element_text(colour = "#c9d1d9", face = "bold")
    )
}

STRAT_COLORS <- c(
  "Flat"            = "#58a6ff",
  "Martingale"      = "#ff7b72",
  "Anti-Martingale" = "#ffa657",
  "Kelly"           = "#3fb950"
)

# ── UI ──────────────────────────────────────────────────────

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&family=Inter:wght@300;400;600&display=swap');

      * { box-sizing: border-box; }

      body {
        background: #0d1117;
        color: #e6edf3;
        font-family: 'Inter', sans-serif;
        margin: 0;
        padding: 0;
      }

      /* ─── HEADER ─── */
      .app-header {
        background: linear-gradient(135deg, #161b22 0%, #1c2128 100%);
        border-bottom: 1px solid #30363d;
        padding: 22px 36px 18px;
        display: flex;
        align-items: center;
        gap: 18px;
      }
      .app-header .suit-icons { font-size: 26px; letter-spacing: 4px; }
      .app-header h1 {
        margin: 0; font-size: 22px; font-weight: 600;
        font-family: 'JetBrains Mono', monospace;
        color: #f0f6fc; letter-spacing: -0.5px;
      }
      .app-header .subtitle {
        font-size: 12px; color: #8b949e; margin-top: 2px;
        font-family: 'JetBrains Mono', monospace; letter-spacing: 0.5px;
      }
      .fipu-badge {
        margin-left: auto;
        background: #21262d;
        border: 1px solid #30363d;
        border-radius: 8px;
        padding: 6px 14px;
        font-size: 11px;
        color: #8b949e;
        font-family: 'JetBrains Mono', monospace;
      }

      /* ─── LAYOUT ─── */
      .main-layout {
        display: flex;
        min-height: calc(100vh - 80px);
      }

      /* ─── SIDEBAR ─── */
      .sidebar-panel {
        width: 290px;
        min-width: 290px;
        background: #161b22;
        border-right: 1px solid #30363d;
        padding: 20px 18px;
        overflow-y: auto;
      }
      .section-label {
        font-size: 10px;
        font-weight: 700;
        color: #8b949e;
        letter-spacing: 1.5px;
        text-transform: uppercase;
        margin: 18px 0 8px;
        padding-bottom: 6px;
        border-bottom: 1px solid #21262d;
      }
      .section-label:first-child { margin-top: 4px; }

      .form-group label {
        color: #c9d1d9 !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        margin-bottom: 4px !important;
      }
      .form-control, .selectize-input {
        background: #21262d !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
        border-radius: 6px !important;
        font-size: 13px !important;
        font-family: 'JetBrains Mono', monospace !important;
      }
      .form-control:focus {
        border-color: #58a6ff !important;
        box-shadow: 0 0 0 3px rgba(88,166,255,0.15) !important;
      }
      input[type=range] { accent-color: #58a6ff; }

      /* run button */
      #run_btn {
        width: 100%;
        background: linear-gradient(135deg, #238636, #2ea043);
        border: none;
        color: #fff;
        font-weight: 700;
        font-size: 14px;
        font-family: 'JetBrains Mono', monospace;
        letter-spacing: 0.5px;
        padding: 12px;
        border-radius: 8px;
        cursor: pointer;
        margin-top: 14px;
        transition: all .2s;
        box-shadow: 0 2px 8px rgba(46,160,67,.3);
      }
      #run_btn:hover { filter: brightness(1.15); transform: translateY(-1px); }
      #run_btn:active { transform: translateY(0); }

      /* strategy checkboxes */
      .strat-check .checkbox { margin: 3px 0; }
      .strat-check .checkbox label { color: #c9d1d9 !important; font-size: 13px !important; }

      /* ─── MAIN CONTENT ─── */
      .content-panel {
        flex: 1;
        padding: 24px 28px;
        overflow-y: auto;
      }

      /* ─── KPI CARDS ─── */
      .kpi-row {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
        gap: 14px;
        margin-bottom: 24px;
      }
      .kpi-card {
        background: #161b22;
        border: 1px solid #30363d;
        border-radius: 10px;
        padding: 14px 16px;
        position: relative;
        overflow: hidden;
      }
      .kpi-card::before {
        content: '';
        position: absolute; top: 0; left: 0; right: 0; height: 3px;
      }
      .kpi-card.blue::before  { background: #58a6ff; }
      .kpi-card.red::before   { background: #ff7b72; }
      .kpi-card.orange::before { background: #ffa657; }
      .kpi-card.green::before { background: #3fb950; }
      .kpi-card.grey::before  { background: #8b949e; }
      .kpi-card .kpi-label {
        font-size: 10px; color: #8b949e; letter-spacing: 1px;
        text-transform: uppercase; font-weight: 600; margin-bottom: 6px;
      }
      .kpi-card .kpi-value {
        font-size: 22px; font-weight: 700;
        font-family: 'JetBrains Mono', monospace; color: #f0f6fc;
      }
      .kpi-card .kpi-sub {
        font-size: 11px; color: #8b949e; margin-top: 3px;
      }

      /* ─── TABS ─── */
      .nav-tabs {
        border-bottom: 1px solid #30363d;
        margin-bottom: 20px;
      }
      .nav-tabs > li > a {
        color: #8b949e;
        background: transparent;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        font-size: 13px;
        font-weight: 500;
        padding: 8px 16px;
        border-radius: 0 !important;
        transition: all .15s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: #f0f6fc !important;
        background: transparent !important;
        border-bottom: 2px solid #58a6ff !important;
      }
      .tab-content { background: transparent; }

      /* ─── PLOT PANELS ─── */
      .plot-card {
        background: #161b22;
        border: 1px solid #30363d;
        border-radius: 10px;
        padding: 16px;
        margin-bottom: 20px;
      }
      .plot-card h4 {
        margin: 0 0 4px;
        font-size: 14px;
        font-weight: 600;
        color: #f0f6fc;
        font-family: 'JetBrains Mono', monospace;
      }
      .plot-card .plot-desc {
        font-size: 11px;
        color: #8b949e;
        margin-bottom: 12px;
      }

      /* ─── SUMMARY TABLE ─── */
      .summary-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 13px;
        font-family: 'JetBrains Mono', monospace;
      }
      .summary-table th {
        background: #21262d;
        color: #8b949e;
        font-size: 10px;
        letter-spacing: 1px;
        text-transform: uppercase;
        padding: 10px 14px;
        text-align: left;
        border-bottom: 1px solid #30363d;
      }
      .summary-table td {
        padding: 9px 14px;
        border-bottom: 1px solid #21262d;
        color: #c9d1d9;
      }
      .summary-table tr:last-child td { border-bottom: none; }
      .summary-table tr:hover td { background: #1c2128; }
      .val-pos { color: #3fb950 !important; }
      .val-neg { color: #ff7b72 !important; }

      /* ─── INFO BOX ─── */
      .info-box {
        background: #1c2128;
        border: 1px solid #388bfd;
        border-radius: 8px;
        padding: 12px 16px;
        margin-bottom: 16px;
        font-size: 12px;
        color: #79c0ff;
        line-height: 1.7;
      }
      .info-box strong { color: #a5d6ff; }

      /* spinner */
      #loading_overlay {
        display: none;
        position: fixed; inset: 0;
        background: rgba(13,17,23,.7);
        z-index: 9999;
        align-items: center; justify-content: center;
      }
      .spinner {
        width: 48px; height: 48px;
        border: 4px solid #30363d;
        border-top-color: #58a6ff;
        border-radius: 50%;
        animation: spin .8s linear infinite;
      }
      @keyframes spin { to { transform: rotate(360deg); } }

      /* risk badge */
      .risk-badge {
        display: inline-block;
        padding: 3px 10px;
        border-radius: 99px;
        font-size: 11px;
        font-weight: 700;
        font-family: 'JetBrains Mono', monospace;
        letter-spacing: 0.5px;
      }
      .risk-safe    { background: rgba(63,185,80,.15);  color: #3fb950; }
      .risk-caution { background: rgba(255,166,87,.15); color: #ffa657; }
      .risk-danger  { background: rgba(255,123,114,.15); color: #ff7b72; }
    "))
  ),

  # Loading overlay
  div(id = "loading_overlay",
    div(class = "spinner")
  ),

  # Header
  div(class = "app-header",
    div(class = "suit-icons", "♠♥♦♣"),
    div(
      h1("Briškola Casino Simulator"),
      div(class = "subtitle", "Monte Carlo simulacija strategija klađenja")
    ),
    div(class = "fipu-badge", "MIS · FIPU Pula")
  ),

  # Main layout
  div(class = "main-layout",

    # ── SIDEBAR ──
    div(class = "sidebar-panel",

      div(class = "section-label", "Parametri špila"),

      sliderInput("suits", "Broj boja (aduta)", 2, 6, 4, 1),
      sliderInput("cards_per_suit", "Karata po boji", 4, 13, 10, 1),
      sliderInput("n_cards_drawn", "Karata u ruci", 5, 15, 10, 1),
      numericInput("init_pts", "Početni bodovi", 120, 50, 300, 10),

      div(class = "section-label", "Parametri simulacije"),

      numericInput("n_sims",        "Broj rundi",         5000,  100,  100000, 100),
      numericInput("start_bankroll","Početni bankroll (€)", 10000, 100, 500000, 100),

      div(class = "section-label", "Parametri strategija"),

      numericInput("flat_bet",        "Osnovni ulog — Flat/Mart. (€)", 10,   1,   1000, 1),
      numericInput("kelly_fraction",  "Kelly udio bankrolla",           0.05, 0.01, 0.5, 0.01),
      numericInput("max_bet",         "Maksimalni ulog (€)",            500,  10,   10000, 50),

      div(class = "section-label", "Odabir strategija"),

      div(class = "strat-check",
        checkboxGroupInput("strategies", NULL,
          choices  = c("Flat", "Martingale", "Anti-Martingale", "Kelly"),
          selected = c("Flat", "Martingale", "Anti-Martingale", "Kelly")
        )
      ),

      actionButton("run_btn", "▶  POKRENUTI SIMULACIJU")
    ),

    # ── CONTENT ──
    div(class = "content-panel",

      # KPI row
      uiOutput("kpi_cards"),

      # Tabs
      tabsetPanel(id = "tabs",

        tabPanel("Kretanje bankrolla",
          div(class = "plot-card",
            h4("Trajektorija bankrolla"),
            div(class = "plot-desc", "Kretanje kapitala po rundi za sve odabrane strategije"),
            plotOutput("plot_bankroll", height = "380px")
          )
        ),

        tabPanel("Distribucija profita",
          div(class = "plot-card",
            h4("Distribucija neto profita po rundi"),
            div(class = "plot-desc", "Gustoća raspodjele dobitaka/gubitaka — simulirana vs teorijska"),
            plotOutput("plot_dist", height = "380px")
          )
        ),

        tabPanel("Analiza špila",
          div(class = "plot-card",
            h4("Distribucija pogodaka aduta"),
            div(class = "plot-desc",
                "Simulirana frekvencija pogodaka vs teorijska hipergeometrijska distribucija (crvena)"),
            plotOutput("plot_hyper", height = "360px")
          )
        ),

        tabPanel("Statistički sažetak",
          uiOutput("summary_table")
        ),

        tabPanel("Casino optimizacija",
          div(class = "info-box",
            HTML("<strong>Analiza limita rundi:</strong> Koliko prosječno casino zarađuje ovisno
                  o tome koliko rundi dozvoli igraču. Pokreće 200 replikacija po koraku.")
          ),
          div(class = "plot-card",
            h4("Očekivani profit casina vs. limit rundi"),
            plotOutput("plot_casino", height = "360px")
          ),
          uiOutput("casino_table")
        )
      )
    )
  )
)

# ── SERVER ──────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive: run simulation on button click
  sim_results <- eventReactive(input$run_btn, {
    req(length(input$strategies) > 0)

    shinyjs::runjs("document.getElementById('loading_overlay').style.display='flex';")
    on.exit(shinyjs::runjs("document.getElementById('loading_overlay').style.display='none';"))

    set.seed(42)
    strats <- input$strategies

    results <- lapply(strats, function(s) {
      simulate_strategy(
        strategy_name   = s,
        n_sims          = input$n_sims,
        flat_bet        = input$flat_bet,
        kelly_fraction  = input$kelly_fraction,
        max_bet         = input$max_bet,
        start_bankroll  = input$start_bankroll,
        suits           = input$suits,
        cards_per_suit  = input$cards_per_suit,
        n_cards_drawn   = input$n_cards_drawn,
        init_pts        = input$init_pts
      )
    })
    names(results) <- strats
    results
  }, ignoreNULL = FALSE)

  # RTP (reactive on deck params)
  rtp <- reactive({
    calc_rtp(input$suits, input$cards_per_suit, input$n_cards_drawn, input$init_pts)
  })

  # ── KPI CARDS ──
  output$kpi_cards <- renderUI({
    res <- sim_results()
    rtp_val <- rtp()
    house_edge <- 100 - rtp_val

    strats <- names(res)
    final_banks <- sapply(strats, function(s) tail(res[[s]]$bank[res[[s]]$bank > 0], 1))
    best_strat  <- strats[which.max(final_banks)]
    worst_strat <- strats[which.min(final_banks)]

    # Bankruptcy check
    bankrupt_info <- sapply(strats, function(s) {
      idx <- which(res[[s]]$bank <= 0)[1]
      if (!is.na(idx)) idx - 1 else NA
    })
    survived <- sum(is.na(bankrupt_info))

    div(class = "kpi-row",
      div(class = "kpi-card grey",
        div(class = "kpi-label", "Teorijski RTP"),
        div(class = "kpi-value", sprintf("%.1f%%", rtp_val)),
        div(class = "kpi-sub", sprintf("House Edge: %.1f%%", house_edge))
      ),
      div(class = "kpi-card blue",
        div(class = "kpi-label", "Rundi simulirano"),
        div(class = "kpi-value", formatC(input$n_sims, format = "d", big.mark = ".")),
        div(class = "kpi-sub", sprintf("%d strategija", length(strats)))
      ),
      div(class = "kpi-card green",
        div(class = "kpi-label", "Preživjele strategije"),
        div(class = "kpi-value", sprintf("%d / %d", survived, length(strats))),
        div(class = "kpi-sub", if (survived == length(strats)) "Sve prežive" else "Bankrot zabilježen")
      ),
      div(class = "kpi-card orange",
        div(class = "kpi-label", "Početni bankroll"),
        div(class = "kpi-value", sprintf("%s€", formatC(input$start_bankroll, format = "d", big.mark = "."))),
        div(class = "kpi-sub", sprintf("Max ulog: %d€", as.integer(input$max_bet)))
      )
    )
  })

  # ── BANKROLL PLOT ──
  output$plot_bankroll <- renderPlot({
    res <- sim_results()
    strats <- names(res)

    max_rounds <- max(sapply(strats, function(s) length(res[[s]]$bank)))
    plot_n <- min(max_rounds, 2000)  # cap for performance

    df <- do.call(rbind, lapply(strats, function(s) {
      bank <- res[[s]]$bank[1:plot_n]
      data.frame(Runda = seq_along(bank), Bankroll = bank, Strategija = s,
                 stringsAsFactors = FALSE)
    }))

    ggplot(df, aes(x = Runda, y = Bankroll, colour = Strategija)) +
      geom_line(linewidth = 0.7, alpha = 0.9) +
      geom_hline(yintercept = input$start_bankroll, linetype = "dashed",
                 colour = "#8b949e", linewidth = 0.5) +
      scale_colour_manual(values = STRAT_COLORS[strats]) +
      scale_y_continuous(labels = function(x) paste0(formatC(x, format = "d", big.mark = "."), "€")) +
      labs(x = "Runda", y = "Bankroll (€)", colour = "Strategija",
           subtitle = sprintf("Prikazano prvih %s rundi", formatC(plot_n, format = "d", big.mark = "."))) +
      casino_theme()
  }, bg = "#0d1117")

  # ── PROFIT DISTRIBUTION ──
  output$plot_dist <- renderPlot({
    res <- sim_results()
    strats <- names(res)

    df <- do.call(rbind, lapply(strats, function(s) {
      data.frame(Profit = res[[s]]$prof, Strategija = s, stringsAsFactors = FALSE)
    }))
    df <- df[df$Profit != 0, ]

    ggplot(df, aes(x = Profit, fill = Strategija)) +
      geom_density(alpha = 0.45, colour = NA) +
      geom_vline(xintercept = 0, colour = "#8b949e", linetype = "dashed", linewidth = 0.7) +
      scale_fill_manual(values = STRAT_COLORS[strats]) +
      facet_wrap(~Strategija, scales = "free") +
      labs(x = "Neto profit po rundi (€)", y = "Gustoća", fill = "Strategija") +
      casino_theme() +
      theme(legend.position = "none")
  }, bg = "#0d1117")

  # ── HYPERGEOMETRIC PLOT ──
  output$plot_hyper <- renderPlot({
    res <- sim_results()

    # Use Flat strategy hits for model validation
    hits_flat <- res[[1]]$hits
    hits_flat <- hits_flat[hits_flat > 0 | TRUE]

    n  <- input$n_cards_drawn
    K  <- input$cards_per_suit
    N  <- input$suits * input$cards_per_suit
    possible_hits <- 0:min(n, K)

    sim_freq <- table(factor(hits_flat, levels = possible_hits)) / length(hits_flat)
    theory   <- dhyper(possible_hits, K, N - K, n)

    df_sim    <- data.frame(Pogoci = possible_hits, Vjerojatnoca = as.numeric(sim_freq), Tip = "Simulacija")
    df_theory <- data.frame(Pogoci = possible_hits, Vjerojatnoca = theory, Tip = "Teorija (Hipergeometrijska)")
    df_all    <- rbind(df_sim, df_theory)

    ggplot() +
      geom_col(data = df_all[df_all$Tip == "Simulacija", ],
               aes(x = Pogoci, y = Vjerojatnoca), fill = "#58a6ff", alpha = 0.7, width = 0.6) +
      geom_line(data = df_all[df_all$Tip != "Simulacija", ],
                aes(x = Pogoci, y = Vjerojatnoca), colour = "#ff7b72", linewidth = 1.5) +
      geom_point(data = df_all[df_all$Tip != "Simulacija", ],
                 aes(x = Pogoci, y = Vjerojatnoca), colour = "#ff7b72", size = 3) +
      scale_x_continuous(breaks = possible_hits) +
      labs(x = "Broj aduta u ruci", y = "Vjerojatnost",
           subtitle = sprintf("N=%d, K=%d, n=%d | Strategija: %s", N, K, n, names(res)[1])) +
      casino_theme()
  }, bg = "#0d1117")

  # ── SUMMARY TABLE ──
  output$summary_table <- renderUI({
    res <- sim_results()
    strats <- names(res)

    rows <- lapply(strats, function(s) {
      bank <- res[[s]]$bank
      prof <- res[[s]]$prof[res[[s]]$prof != 0]
      final <- tail(bank[bank > 0], 1)
      if (length(final) == 0) final <- 0
      pnl <- final - input$start_bankroll
      bankrupt_round <- which(bank <= 0)[1] - 1
      bankrupt_txt <- if (!is.na(bankrupt_round)) as.character(bankrupt_round) else "—"
      max_bank <- max(bank)
      min_bank <- min(bank[bank > 0])
      avg_profit <- mean(prof)

      col_pnl <- if (pnl > 0) "val-pos" else "val-neg"
      col_avg <- if (avg_profit > 0) "val-pos" else "val-neg"
      col_dot  <- sprintf("style='color:%s;font-weight:700'", STRAT_COLORS[s])

      tags$tr(
        tags$td(span(style = sprintf("color:%s;font-weight:700", STRAT_COLORS[s]), s)),
        tags$td(class = col_pnl, sprintf("%+.0f€", pnl)),
        tags$td(class = col_avg, sprintf("%+.2f€", avg_profit)),
        tags$td(sprintf("%.0f€", max_bank)),
        tags$td(sprintf("%.0f€", if(length(min_bank)==0) 0 else min_bank)),
        tags$td(bankrupt_txt)
      )
    })

    tagList(
      div(class = "plot-card",
        h4("Sažetak po strategijama"),
        div(class = "plot-desc",
            sprintf("Rezultati simulacije — %s rundi, bankroll %s€",
                    formatC(input$n_sims, format="d", big.mark="."),
                    formatC(input$start_bankroll, format="d", big.mark="."))),
        tags$table(class = "summary-table",
          tags$thead(tags$tr(
            tags$th("Strategija"), tags$th("Ukupni PnL"),
            tags$th("Prosj. profit/rundi"), tags$th("Maks. bankroll"),
            tags$th("Min. bankroll"), tags$th("Bankrot (runda)")
          )),
          tags$tbody(rows)
        )
      )
    )
  })

  # ── CASINO OPTIMIZACIJA ──
  output$plot_casino <- renderPlot({
    req(input$run_btn)
    set.seed(99)

    limit_steps  <- seq(10, 100, by = 10)
    n_reps       <- 200
    strats_opt   <- c("Flat", "Martingale", "Kelly")

    casino_df <- data.frame()

    for (lim in limit_steps) {
      avg_profits <- sapply(strats_opt, function(s) {
        runs <- replicate(n_reps, {
          sim <- simulate_strategy(
            s, n_sims = lim,
            flat_bet       = input$flat_bet,
            kelly_fraction = input$kelly_fraction,
            max_bet        = input$max_bet,
            start_bankroll = input$start_bankroll,
            suits           = input$suits,
            cards_per_suit  = input$cards_per_suit,
            n_cards_drawn   = input$n_cards_drawn,
            init_pts        = input$init_pts
          )
          input$start_bankroll - tail(sim$bank[sim$bank >= 0], 1)
        })
        mean(runs)
      })
      casino_df <- rbind(casino_df, data.frame(
        Limit = lim,
        Flat       = avg_profits["Flat"],
        Martingale = avg_profits["Martingale"],
        Kelly      = avg_profits["Kelly"]
      ))
    }

    df_long <- reshape(casino_df,
      varying   = c("Flat", "Martingale", "Kelly"),
      v.names   = "Profit",
      timevar   = "Strategija",
      times     = c("Flat", "Martingale", "Kelly"),
      direction = "long")

    ggplot(df_long, aes(x = Limit, y = Profit, colour = Strategija, group = Strategija)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_colour_manual(values = STRAT_COLORS[c("Flat","Martingale","Kelly")]) +
      scale_y_continuous(labels = function(x) paste0(round(x), "€")) +
      labs(x = "Limit rundi", y = "Prosječna zarada casina (€)",
           subtitle = sprintf("200 replikacija po koraku — bankroll %s€",
                              formatC(input$start_bankroll, format="d", big.mark="."))) +
      casino_theme()
  }, bg = "#0d1117")

  output$casino_table <- renderUI({
    req(input$run_btn)
    set.seed(99)

    limit_steps <- seq(10, 100, by = 10)
    n_reps      <- 200

    casino_rows <- lapply(limit_steps, function(lim) {
      avg_profits <- sapply(c("Flat","Martingale","Kelly"), function(s) {
        runs <- replicate(n_reps, {
          sim <- simulate_strategy(
            s, n_sims = lim,
            flat_bet       = input$flat_bet,
            kelly_fraction = input$kelly_fraction,
            max_bet        = input$max_bet,
            start_bankroll = input$start_bankroll,
            suits           = input$suits,
            cards_per_suit  = input$cards_per_suit,
            n_cards_drawn   = input$n_cards_drawn,
            init_pts        = input$init_pts
          )
          input$start_bankroll - tail(sim$bank[sim$bank >= 0], 1)
        })
        mean(runs)
      })
      tags$tr(
        tags$td(lim),
        tags$td(sprintf("%.1f€", avg_profits["Flat"])),
        tags$td(sprintf("%.1f€", avg_profits["Martingale"])),
        tags$td(sprintf("%.1f€", avg_profits["Kelly"]))
      )
    })

    div(class = "plot-card",
      h4("Tablica: Prosječna zarada casina"),
      tags$table(class = "summary-table",
        tags$thead(tags$tr(
          tags$th("Limit rundi"),
          tags$th(style = sprintf("color:%s", STRAT_COLORS["Flat"]), "Flat"),
          tags$th(style = sprintf("color:%s", STRAT_COLORS["Martingale"]), "Martingale"),
          tags$th(style = sprintf("color:%s", STRAT_COLORS["Kelly"]), "Kelly")
        )),
        tags$tbody(casino_rows)
      )
    )
  })
}

shinyApp(ui, server)
