# server.R — NSMO Dashboard V2

server <- function(input, output, session) {

  # Null-coalescing operator
  `%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(as.character(a)) > 0) a else b


  # ══════════════════════════════════════════════════════════════════════════
  # SHARED FILTER HELPERS
  # ══════════════════════════════════════════════════════════════════════════

  apply_common_filters <- function(d, fico, period, race, loan_type, gender,
                                    first_mort = NULL, term = NULL,
                                    cashout = NULL, jumbo = NULL) {
    if (!is.null(fico)       && length(fico) > 0)       d <- d %>% filter(FICO_category        %in% fico)
    if (!is.null(period)     && length(period) > 0)     d <- d %>% filter(COVID_period          %in% period)
    if (!is.null(race)       && length(race) > 0)       d <- d %>% filter(Race                  %in% race)
    if (!is.null(loan_type)  && length(loan_type) > 0)  d <- d %>% filter(Loan_Type             %in% loan_type)
    if (!is.null(gender)     && length(gender) > 0)     d <- d %>% filter(Sex_Label             %in% gender)
    if (!is.null(first_mort) && length(first_mort) > 0) d <- d %>% filter(First_Mortgage_Label  %in% first_mort)
    if (!is.null(term)       && length(term) > 0)       d <- d %>% filter(Term_Label            %in% term)
    if (!is.null(cashout)    && length(cashout) > 0)    d <- d %>% filter(Cashout_Label         %in% cashout)
    if (!is.null(jumbo)      && length(jumbo) > 0)      d <- d %>% filter(Jumbo_Label           %in% jumbo)
    d
  }

  # ── Reactive datasets ──────────────────────────────────────────────────────

  summary_data <- reactive({
    apply_common_filters(df,
      fico = input$sum_fico, period = input$sum_period,
      race = input$sum_race, loan_type = input$sum_loan_type,
      gender = input$sum_gender, first_mort = input$sum_first_mort,
      term = input$sum_term, cashout = input$sum_cashout, jumbo = input$sum_jumbo)
  })

  trend_data <- reactive({
    apply_common_filters(df,
      fico = input$trend_fico, period = input$trend_period,
      race = input$trend_race, loan_type = input$trend_loan_type,
      gender = input$trend_gender, first_mort = input$trend_first_mort,
      term = input$trend_term, cashout = input$trend_cashout)
  })

  # ── Two-way sync: code picker ↔ description picker ──────────────────────────
  # When the CODE picker changes → update the DESCRIPTION picker to match
  observeEvent(input$trend_var_code, {
    if (!is.null(input$trend_var_code) && input$trend_var_code != input$trend_var) {
      updateSelectInput(session, "trend_var", selected = input$trend_var_code)
    }
  }, ignoreInit = TRUE)

  # When the DESCRIPTION picker changes → update the CODE picker to match
  observeEvent(input$trend_var, {
    if (!is.null(input$trend_var) && input$trend_var != input$trend_var_code) {
      updateSelectInput(session, "trend_var_code", selected = input$trend_var)
    }
  }, ignoreInit = TRUE)

  # Active variable used by all trend charts (always reads from trend_var)
  active_trend_var <- reactive({ input$trend_var })

  explorer_data <- reactive({
    d <- df
    if (!is.null(input$exp_years)       && length(input$exp_years) > 0) d <- d %>% filter(open_year %in% input$exp_years)
    if (input$exp_performance != "All") d <- d %>% filter(Performance    == input$exp_performance)
    if (input$exp_education   != "All") d <- d %>% filter(Education_Label == input$exp_education)
    if (input$exp_age         != "All") d <- d %>% filter(Age_Category   == input$exp_age)
    if (input$exp_ltv         != "All") d <- d %>% filter(LTV_Category   == input$exp_ltv)
    if (input$exp_dti         != "All") d <- d %>% filter(DTI_Category   == input$exp_dti)
    d
  })

  report_data <- reactive({
    apply_common_filters(df,
      fico = input$rpt_fico, period = input$rpt_period,
      race = input$rpt_race, loan_type = input$rpt_loan_type,
      gender = input$rpt_gender)
  })

  # ── Reset all report filters ───────────────────────────────────────────────
  observeEvent(input$rpt_reset, {
    updateSelectizeInput(session, "rpt_fico",      selected = character(0))
    updateSelectizeInput(session, "rpt_period",    selected = character(0))
    updateSelectizeInput(session, "rpt_race",      selected = character(0))
    updateSelectizeInput(session, "rpt_loan_type", selected = character(0))
    updateSelectizeInput(session, "rpt_gender",    selected = character(0))
    updateSelectizeInput(session, "rpt_purpose",   selected = character(0))
  }, ignoreNULL = TRUE)

  # ══════════════════════════════════════════════════════════════════════════
  # SIGNIFICANCE HELPERS (local)
  # ══════════════════════════════════════════════════════════════════════════

  # Weighted two-sample t-test (Welch approximation with survey weights)
  # Uses weighted means and weighted variances. This is the correct approach
  # for NSMO which uses probability sampling weights (analysis_weight).
  weighted_t_test <- function(x1, w1, x2, w2) {
    # Normalize weights to sum to n (effective sample size approach)
    w1n <- w1 / mean(w1, na.rm = TRUE)
    w2n <- w2 / mean(w2, na.rm = TRUE)
    n1  <- sum(!is.na(x1))
    n2  <- sum(!is.na(x2))
    if (n1 < 5 || n2 < 5) return(NA_real_)
    m1  <- weighted.mean(x1, w1n, na.rm = TRUE)
    m2  <- weighted.mean(x2, w2n, na.rm = TRUE)
    # Weighted variance: sum(w * (x - xbar)^2) / sum(w) * n/(n-1) correction
    v1  <- sum(w1n * (x1 - m1)^2, na.rm = TRUE) / (sum(w1n, na.rm=TRUE) - 1)
    v2  <- sum(w2n * (x2 - m2)^2, na.rm = TRUE) / (sum(w2n, na.rm=TRUE) - 1)
    se  <- sqrt(v1/n1 + v2/n2)
    if (se == 0 || is.na(se)) return(NA_real_)
    t_stat <- (m1 - m2) / se
    # Welch-Satterthwaite degrees of freedom
    df  <- (v1/n1 + v2/n2)^2 / ((v1/n1)^2/(n1-1) + (v2/n2)^2/(n2-1))
    2 * pt(-abs(t_stat), df = max(df, 1))
  }

  test_mean_sig <- function(d, var) {
    tryCatch({
      cu    <- d %>% filter(Institution_Type == "Credit Union",     !is.na(get(var)))
      noncu <- d %>% filter(Institution_Type == "Non-Credit Union", !is.na(get(var)))
      if (nrow(cu) < 10 || nrow(noncu) < 10) return(list(sig = FALSE, p = NA, star = ""))
      # Use weighted t-test with survey weights
      wt <- "analysis_weight"
      p  <- if (wt %in% names(d)) {
        weighted_t_test(cu[[var]], cu[[wt]], noncu[[var]], noncu[[wt]])
      } else {
        t.test(cu[[var]], noncu[[var]])$p.value
      }
      if (is.na(p)) return(list(sig = FALSE, p = NA, star = ""))
      list(sig = p < 0.05, p = p, star = sig_label(p))
    }, error = function(e) list(sig = FALSE, p = NA, star = ""))
  }

  wmean <- function(d, var, wt = "analysis_weight") {
    weighted.mean(d[[var]], d[[wt]], na.rm = TRUE)
  }

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 1 — EXECUTIVE SUMMARY
  # ══════════════════════════════════════════════════════════════════════════

  # ── Custom KPI scorecard ──────────────────────────────────────────────────
  output$sum_kpi_scorecard <- renderUI({
    d  <- summary_data()
    cu    <- d %>% filter(Institution_Type == "Credit Union")
    noncu <- d %>% filter(Institution_Type == "Non-Credit Union")

    n_cu    <- nrow(cu)
    n_noncu <- nrow(noncu)

    # Weighted means
    fico_cu    <- round(wmean(cu,    "Score_Origin"))
    fico_noncu <- round(wmean(noncu, "Score_Origin"))
    ltv_cu     <- round(wmean(cu,    "ltv"), 1)
    ltv_noncu  <- round(wmean(noncu, "ltv"), 1)
    dti_cu     <- round(wmean(cu,    "dti"), 1)
    dti_noncu  <- round(wmean(noncu, "dti"), 1)
    rate_cu    <- round(wmean(cu,    "Rate_Spread_Pct"), 3)
    rate_noncu <- round(wmean(noncu, "Rate_Spread_Pct"), 3)

    # Significance stars
    s_fico <- test_mean_sig(d, "Score_Origin")
    s_ltv  <- test_mean_sig(d, "ltv")
    s_dti  <- test_mean_sig(d, "dti")
    s_rate <- test_mean_sig(d, "Rate_Spread_Pct")

    # Loan amount labels
    amt_lbl <- c("1"="<$50K","2"="$50-99K","3"="$100-149K","4"="$150-199K",
                 "5"="$200-249K","6"="$250-299K","7"="$300-349K","8"="$350-399K","9"="$400K+")
    cu_amt    <- if(nrow(cu)>0)    amt_lbl[as.character(round(median(cu$loan_amount_cat,    na.rm=TRUE)))] else "N/A"
    noncu_amt <- if(nrow(noncu)>0) amt_lbl[as.character(round(median(noncu$loan_amount_cat, na.rm=TRUE)))] else "N/A"

    # Helper: build one scorecard card
    # Satisfaction
    sat_cu_v <- sat_noncu_v <- NA
    tryCatch({
      vb <- "x28a_binary"
      if (vb %in% names(d)) {
        sat_cu_v    <- round(mean(cu[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)*100, 1)
        sat_noncu_v <- round(mean(noncu[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)*100, 1)
      }
    }, error=function(e) NULL)
    s_sat <- tryCatch({
      x1 <- sum(cu[["x28a_binary"]]    == "Positive (Very/Somewhat)", na.rm=TRUE)
      x2 <- sum(noncu[["x28a_binary"]] == "Positive (Very/Somewhat)", na.rm=TRUE)
      p  <- prop.test(c(x1,x2), c(nrow(cu),nrow(noncu)), correct=FALSE)$p.value
      list(star=sig_label(p), sig=p<0.05)
    }, error=function(e) list(star="", sig=FALSE))

    cu_share <- round(n_cu / (n_cu + n_noncu) * 100, 1)

    # ── Bold emphatic KPI card builder ──────────────────────────────────────
    make_card <- function(icon_name, icon_class, card_class,
                          cu_val, noncu_val, label,
                          delta_val=NULL, delta_dir="flat", sig_star="", subtitle=NULL) {
      sig_html <- if (nchar(sig_star) > 0)
        tags$span(class="kpi-sig-badge kpi-sig-yes", paste0("Sig ", sig_star))
      else
        tags$span(class="kpi-sig-badge kpi-sig-no",  "n.s.")

      delta_html <- if (!is.null(delta_val)) {
        arrow <- if (delta_dir=="up") "▲" else if (delta_dir=="down") "▼" else "—"
        tags$div(style="display:flex;align-items:center;gap:6px;margin-top:8px;flex-wrap:wrap;",
          tags$span(class=paste("kpi-sc-delta", delta_dir),
            HTML(paste0(arrow, " ", delta_val))),
          if (!is.null(subtitle))
            tags$span(style="font-size:10px;color:#94A3B8;", subtitle)
        )
      } else {
        if (!is.null(subtitle))
          tags$div(style="font-size:10px;color:#94A3B8;margin-top:6px;", subtitle)
        else tags$div()
      }

      tags$div(class=paste("kpi-scorecard-card", card_class),
        # Sig badge (top-right absolute)
        sig_html,
        # Inner padded content
        tags$div(class="kpi-card-inner",
          # Icon
          tags$div(class=paste("kpi-icon-wrap", icon_class),
            tags$i(class=paste0("fas fa-", icon_name))
          ),
          # Text block
          tags$div(class="kpi-text",
            tags$div(class="kpi-sc-label", label),
            # CU vs Non-CU values as big pills
            tags$div(style="display:flex;align-items:baseline;gap:8px;margin-top:4px;flex-wrap:wrap;",
              tags$span(style="font-size:22px;font-weight:800;color:#2563EB;letter-spacing:-.5px;line-height:1;",
                cu_val),
              tags$span(style="font-size:11px;color:#CBD5E1;font-weight:600;", "vs"),
              tags$span(style="font-size:22px;font-weight:800;color:#DC2626;letter-spacing:-.5px;line-height:1;",
                noncu_val)
            ),
            # CU / Non-CU labels
            tags$div(style="display:flex;gap:6px;margin-top:4px;",
              tags$span(style="font-size:9px;font-weight:700;color:#2563EB;background:#EFF6FF;
                               border:1px solid #BFDBFE;border-radius:10px;padding:1px 7px;", "CU"),
              tags$span(style="font-size:9px;font-weight:700;color:#DC2626;background:#FEF2F2;
                               border:1px solid #FEE2E2;border-radius:10px;padding:1px 7px;", "Non-CU")
            ),
            # Delta row
            delta_html
          )
        )
      )
    }

    tags$div(class="kpi-scorecard",

      make_card("building-columns","kpi-icon-cu","kpi-card-cu",
        format(n_cu, big.mark=","), format(n_noncu, big.mark=","),
        "Total Loans in Sample",
        delta_val=paste0("Δ ", format(abs(n_cu-n_noncu), big.mark=",")),
        delta_dir="flat", subtitle="count difference"),

      make_card("credit-card","kpi-icon-cu","kpi-card-cu",
        as.character(fico_cu), as.character(fico_noncu),
        "Avg FICO Score",
        delta_val=paste0(ifelse(fico_cu-fico_noncu>=0,"+",""), fico_cu-fico_noncu),
        delta_dir=ifelse(fico_cu>=fico_noncu,"up","down"),
        sig_star=s_fico$star, subtitle="pts — selection effect caveat"),

      make_card("house","kpi-icon-amber","kpi-card-amber",
        paste0(ltv_cu,"%"), paste0(ltv_noncu,"%"),
        "Avg LTV (%)",
        delta_val=paste0(ifelse(ltv_cu-ltv_noncu>=0,"+",""), round(ltv_cu-ltv_noncu,1),"pp"),
        delta_dir=ifelse(ltv_cu<=ltv_noncu,"up","down"),
        sig_star=s_ltv$star, subtitle="lower = more equity at origination"),

      make_card("file-invoice-dollar","kpi-icon-amber","kpi-card-amber",
        paste0(dti_cu,"%"), paste0(dti_noncu,"%"),
        "Avg DTI (%)",
        delta_val=paste0(ifelse(dti_cu-dti_noncu>=0,"+",""), round(dti_cu-dti_noncu,1),"pp"),
        delta_dir=ifelse(dti_cu<=dti_noncu,"up","down"),
        sig_star=s_dti$star, subtitle="lower = less debt burden"),

      make_card("percent","kpi-icon-violet","kpi-card-neutral",
        paste0(round(rate_cu*100,1),"bp"), paste0(round(rate_noncu*100,1),"bp"),
        "Avg Rate Spread",
        delta_val=paste0(ifelse(rate_cu-rate_noncu>=0,"+",""), round((rate_cu-rate_noncu)*100,1),"bp"),
        delta_dir=ifelse(rate_cu<=rate_noncu,"up","down"),
        sig_star=s_rate$star, subtitle="basis pts above market benchmark"),

      make_card("dollar-sign","kpi-icon-green","kpi-card-green",
        if(!is.null(cu_amt) && !is.na(cu_amt)) cu_amt else "N/A",
        if(!is.null(noncu_amt) && !is.na(noncu_amt)) noncu_amt else "N/A",
        "Median Loan Amount", subtitle="origination amount band"),

      make_card("star","kpi-icon-green","kpi-card-green",
        if(!is.na(sat_cu_v)) paste0(sat_cu_v,"%") else "N/A",
        if(!is.na(sat_noncu_v)) paste0(sat_noncu_v,"%") else "N/A",
        "Satisfaction (% Positive)",
        delta_val=if(!is.na(sat_cu_v)&&!is.na(sat_noncu_v))
          paste0(ifelse(sat_cu_v-sat_noncu_v>=0,"+",""),round(sat_cu_v-sat_noncu_v,1),"pp") else NULL,
        delta_dir=ifelse(!is.na(sat_cu_v)&&!is.na(sat_noncu_v)&&sat_cu_v>=sat_noncu_v,"up","down"),
        sig_star=s_sat$star, subtitle="% rated lender positively"),

      # CU share card — special layout
      tags$div(class="kpi-scorecard-card kpi-card-neutral",
        tags$div(class="kpi-card-inner",
          tags$div(class="kpi-icon-wrap kpi-icon-violet",
            tags$i(class="fas fa-chart-pie")),
          tags$div(class="kpi-text",
            tags$div(class="kpi-sc-label", "CU Share of Sample"),
            tags$div(style="font-size:36px;font-weight:900;color:#7C3AED;
                            letter-spacing:-1.5px;line-height:1;margin-top:4px;",
              paste0(cu_share,"%")),
            tags$div(style="font-size:10px;color:#94A3B8;margin-top:4px;",
              paste0(format(n_cu+n_noncu,big.mark=","), " total loans in sample"))
          )
        )
      )
    )
  })

  # ── Key Metrics: separate subplot per metric (fixes scale dominance) ─────────
  output$sum_metrics_compare <- renderPlotly({
    cu    <- summary_data() %>% filter(Institution_Type == "Credit Union")
    noncu <- summary_data() %>% filter(Institution_Type == "Non-Credit Union")
    s_dti  <- test_mean_sig(summary_data(), "dti")
    s_ltv  <- test_mean_sig(summary_data(), "ltv")
    s_rate <- test_mean_sig(summary_data(), "Rate_Spread_Pct")
    s_fico <- test_mean_sig(summary_data(), "Score_Origin")

    metrics <- list(
      list(name="Avg DTI (%)",        cu=wmean(cu,"dti"),           noncu=wmean(noncu,"dti"),           star=s_dti$star),
      list(name="Avg LTV (%)",        cu=wmean(cu,"ltv"),           noncu=wmean(noncu,"ltv"),           star=s_ltv$star),
      list(name="Avg FICO",           cu=wmean(cu,"Score_Origin"),  noncu=wmean(noncu,"Score_Origin"),  star=s_fico$star),
      list(name="Rate Spread (%)",    cu=wmean(cu,"Rate_Spread_Pct"), noncu=wmean(noncu,"Rate_Spread_Pct"), star=s_rate$star)
    )

    plots <- lapply(seq_along(metrics), function(i) {
      m    <- metrics[[i]]
      vals <- c(m$cu, m$noncu)
      labs <- c(paste0(round(m$cu,  2), m$star), round(m$noncu, 2))
      col  <- c(v2_colors$cu, v2_colors$noncu)
      y_max <- max(vals, na.rm=TRUE) * 1.25

      plot_ly(
        x = c("CU", "Non-CU"), y = vals,
        type = "bar",
        marker = list(color = col,
                      line  = list(color = "white", width = 1.5)),
        text  = labs,
        textposition = "outside",
        textfont = list(size = 12, color = "#0F172A"),
        hovertemplate = paste0("<b>%{x}</b><br>", m$name, ": %{y:.2f}<extra></extra>"),
        showlegend = (i == 1),
        legendgroup = "inst",
        name = c("Credit Union", "Non-Credit Union")
      ) %>%
        layout(
          xaxis = list(title = list(text = m$name, font = list(size=11, color="#475569")),
                       tickfont = list(size=11), showgrid=FALSE),
          yaxis = list(range = c(0, y_max), showgrid=TRUE,
                       gridcolor="#F1F5F9", zeroline=FALSE,
                       tickfont=list(size=10)),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          bargap = 0.35
        )
    })

    subplot(plots, nrows=1, shareY=FALSE, titleX=TRUE, titleY=FALSE) %>%
      layout(
        font        = list(family="Inter, sans-serif"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(orientation="h", x=0.3, y=-0.25,
                      font=list(size=11)),
        margin = list(t=10, b=50, l=40, r=20),
        annotations = list(
          list(text="* p<0.05  ** p<0.01  *** p<0.001",
               x=1, y=-0.32, xref="paper", yref="paper",
               showarrow=FALSE, font=list(size=10, color="#94A3B8"), xanchor="right")
        )
      )
  })

  # ── FICO distribution: grouped horizontal bars (replaces broken heatmap) ──────
  output$sum_fico_grouped <- renderPlotly({
    d <- summary_data() %>%
      filter(FICO_category != "Unknown") %>%
      group_by(FICO_category, Institution_Type) %>%
      summarise(wn = sum(analysis_weight, na.rm=TRUE), .groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct = round(wn / sum(wn) * 100, 1),
             FICO_category = factor(FICO_category, levels = fico_order))

    cu_d    <- d %>% filter(Institution_Type == "Credit Union")
    noncu_d <- d %>% filter(Institution_Type == "Non-Credit Union")

    plot_ly() %>%
      add_bars(data = cu_d,    y = ~FICO_category, x = ~pct, name = "Credit Union",
               orientation = "h",
               marker = list(color = v2_colors$cu, line = list(color="white",width=1)),
               text = ~paste0(pct, "%"), textposition = "outside",
               hovertemplate = "<b>CU</b> %{y}: %{x:.1f}%<extra></extra>") %>%
      add_bars(data = noncu_d, y = ~FICO_category, x = ~pct, name = "Non-Credit Union",
               orientation = "h",
               marker = list(color = v2_colors$noncu, line = list(color="white",width=1)),
               text = ~paste0(pct, "%"), textposition = "outside",
               hovertemplate = "<b>Non-CU</b> %{y}: %{x:.1f}%<extra></extra>") %>%
      layout(
        barmode = "group",
        xaxis = list(title="% of Institution", ticksuffix="%",
                     showgrid=TRUE, gridcolor="#F1F5F9", zeroline=FALSE),
        yaxis = list(title="", autorange="reversed",
                     tickfont=list(size=10), showgrid=FALSE),
        font          = list(family="Inter, sans-serif", size=11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(orientation="h", x=0, y=-0.18, font=list(size=10)),
        margin = list(t=5, b=40, l=10, r=50)
      )
  })

  # ── Sex distribution pies ─────────────────────────────────────────────────────
  output$sum_sex_pie <- renderPlotly({
    sex_d <- summary_data() %>%
      filter(Sex_Label %in% c("Male","Female")) %>%
      group_by(Institution_Type, Sex_Label) %>%
      summarise(wn = sum(analysis_weight, na.rm=TRUE), .groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct = wn / sum(wn) * 100)
    cu_d    <- sex_d %>% filter(Institution_Type == "Credit Union")
    noncu_d <- sex_d %>% filter(Institution_Type == "Non-Credit Union")
    plot_ly() %>%
      add_pie(data=cu_d,    labels=~Sex_Label, values=~pct, name="CU",
              domain=list(x=c(0,.46), y=c(0,1)),
              marker=list(colors=c(v2_colors$cu, v2_colors$cu_light),
                          line=list(color="#fff",width=2)),
              textinfo="label+percent", hole=0.42,
              hovertemplate="<b>CU %{label}</b>: %{percent}<extra></extra>") %>%
      add_pie(data=noncu_d, labels=~Sex_Label, values=~pct, name="Non-CU",
              domain=list(x=c(.54,1), y=c(0,1)),
              marker=list(colors=c(v2_colors$noncu, v2_colors$noncu_light),
                          line=list(color="#fff",width=2)),
              textinfo="label+percent", hole=0.42,
              hovertemplate="<b>Non-CU %{label}</b>: %{percent}<extra></extra>") %>%
      layout(
        annotations=list(
          list(text="CU",     x=0.23, y=0.5, showarrow=FALSE,
               font=list(size=13, color=v2_colors$cu,    family="Inter")),
          list(text="Non-CU", x=0.77, y=0.5, showarrow=FALSE,
               font=list(size=11, color=v2_colors$noncu, family="Inter"))
        ),
        paper_bgcolor="rgba(0,0,0,0)", showlegend=FALSE,
        margin=list(t=10,b=10,l=10,r=10)
      )
  })

  # ── Race composition donuts ───────────────────────────────────────────────────
  output$sum_race_donut <- renderPlotly({
    d <- summary_data() %>%
      filter(Race != "Unknown") %>%
      group_by(Institution_Type, Race) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE), .groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct=wn/sum(wn)*100)
    cu_d    <- d %>% filter(Institution_Type == "Credit Union")
    noncu_d <- d %>% filter(Institution_Type == "Non-Credit Union")
    pal <- c("#2563EB","#0EA5E9","#7C3AED","#D97706")
    plot_ly() %>%
      add_pie(data=cu_d,    labels=~Race, values=~pct, name="CU",
              domain=list(x=c(0,.46), y=c(0,1)),
              marker=list(colors=pal, line=list(color="#fff",width=2)),
              textinfo="label+percent", hole=0.42,
              hovertemplate="<b>CU %{label}</b>: %{percent}<extra></extra>") %>%
      add_pie(data=noncu_d, labels=~Race, values=~pct, name="Non-CU",
              domain=list(x=c(.54,1), y=c(0,1)),
              marker=list(colors=pal, line=list(color="#fff",width=2)),
              textinfo="label+percent", hole=0.42,
              hovertemplate="<b>Non-CU %{label}</b>: %{percent}<extra></extra>") %>%
      layout(
        annotations=list(
          list(text="CU",     x=0.23, y=0.5, showarrow=FALSE,
               font=list(size=13,color=v2_colors$cu,    family="Inter")),
          list(text="Non-CU", x=0.77, y=0.5, showarrow=FALSE,
               font=list(size=11,color=v2_colors$noncu, family="Inter"))
        ),
        paper_bgcolor="rgba(0,0,0,0)",
        legend=list(orientation="h", x=0.15, y=-0.15, font=list(size=10)),
        margin=list(t=5,b=30,l=5,r=5)
      )
  })

  # ── Loan Purpose & Type grouped bars ─────────────────────────────────────────
  output$sum_purpose_type <- renderPlotly({
    # Purpose
    purp <- summary_data() %>%
      filter(Cashout_Label %in% c("Refinance","Purchase")) %>%
      group_by(Cashout_Label, Institution_Type) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE), .groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct=round(wn/sum(wn)*100,1))

    # Loan type
    ltype <- summary_data() %>%
      filter(Loan_Type %in% c("Conventional","FHA/VA/FSA")) %>%
      group_by(Loan_Type, Institution_Type) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE), .groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct=round(wn/sum(wn)*100,1))

    p1 <- plot_ly(purp, x=~Cashout_Label, y=~pct, color=~Institution_Type,
                  colors=c(v2_colors$cu, v2_colors$noncu), type="bar",
                  text=~paste0(pct,"%"), textposition="outside",
                  hovertemplate="%{x}: %{y:.1f}%<extra>%{fullData.name}</extra>",
                  showlegend=TRUE) %>%
      layout(barmode="group", bargap=0.3,
             xaxis=list(title="Purpose", showgrid=FALSE),
             yaxis=list(title="%", showgrid=TRUE, gridcolor="#F1F5F9",
                        range=c(0,100)),
             plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)")

    p2 <- plot_ly(ltype, x=~Loan_Type, y=~pct, color=~Institution_Type,
                  colors=c(v2_colors$cu, v2_colors$noncu), type="bar",
                  text=~paste0(pct,"%"), textposition="outside",
                  hovertemplate="%{x}: %{y:.1f}%<extra>%{fullData.name}</extra>",
                  showlegend=FALSE) %>%
      layout(barmode="group", bargap=0.3,
             xaxis=list(title="Loan Type", showgrid=FALSE),
             yaxis=list(showgrid=TRUE, gridcolor="#F1F5F9", range=c(0,100)),
             plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)")

    subplot(p1, p2, nrows=1, shareY=TRUE, titleX=TRUE) %>%
      layout(
        font=list(family="Inter, sans-serif", size=11),
        paper_bgcolor="rgba(0,0,0,0)",
        legend=list(orientation="h", x=0, y=-0.2, font=list(size=10)),
        margin=list(t=5,b=40,l=30,r=10)
      )
  })

  # ── Survey Satisfaction — zoomed x-axis to show real variation ──────────────
  output$sum_satisfaction_radar <- renderPlotly({
    show_vars <- response_vars[1:min(length(response_vars), 8)]
    sat_df <- lapply(show_vars, function(v) {
      vb <- paste0(v, "_binary")
      if (!vb %in% names(df)) return(NULL)
      summary_data() %>%
        filter(!is.na(get(vb))) %>%
        group_by(Institution_Type) %>%
        summarise(pct = mean(get(vb) == "Positive (Very/Somewhat)", na.rm = TRUE) * 100,
                  .groups = "drop") %>%
        mutate(Variable = if (!is.null(response_short_labels[[v]])) response_short_labels[[v]] else v)
    }) %>% bind_rows()

    if (nrow(sat_df) == 0) return(NULL)

    # Strip group prefix (everything up to and including ": ") for cleaner y-labels
    sat_df$ShortVar <- gsub("^[A-Za-z0-9/ -]+:\\s*", "", sat_df$Variable)
    sat_df$ShortVar <- ifelse(nchar(sat_df$ShortVar) > 34,
                              paste0(substr(sat_df$ShortVar, 1, 32), "…"),
                              sat_df$ShortVar)

    cu_d    <- sat_df %>% filter(Institution_Type == "Credit Union")
    noncu_d <- sat_df %>% filter(Institution_Type == "Non-Credit Union")

    # Dynamic x-axis: zoom in to the actual data range so small differences are visible
    all_pcts <- sat_df$pct[!is.na(sat_df$pct)]
    x_min    <- max(0,   floor(min(all_pcts, na.rm = TRUE) / 10) * 10 - 5)
    x_max    <- min(100, ceiling(max(all_pcts, na.rm = TRUE) / 10) * 10 + 8)
    # Generate clean tick marks within range
    tick_step <- if ((x_max - x_min) <= 20) 2 else if ((x_max - x_min) <= 40) 5 else 10
    tick_vals <- seq(x_min, x_max, by = tick_step)

    plot_ly() %>%
      add_bars(
        data = cu_d, y = ~ShortVar, x = ~pct, name = "Credit Union",
        orientation = "h",
        marker = list(color = v2_colors$cu, opacity = 0.88,
                      line = list(color = "white", width = 1)),
        # Show value at end of bar; use customdata to pass full label to hover
        text         = ~paste0(round(pct, 1), "%"),
        textposition = "outside",
        textfont     = list(size = 10, color = "#0F172A"),
        hovertemplate = "<b>Credit Union</b><br>%{y}<br>%{x:.1f}% positive<extra></extra>"
      ) %>%
      add_bars(
        data = noncu_d, y = ~ShortVar, x = ~pct, name = "Non-Credit Union",
        orientation = "h",
        marker = list(color = v2_colors$noncu, opacity = 0.88,
                      line = list(color = "white", width = 1)),
        text         = ~paste0(round(pct, 1), "%"),
        textposition = "outside",
        textfont     = list(size = 10, color = "#0F172A"),
        hovertemplate = "<b>Non-Credit Union</b><br>%{y}<br>%{x:.1f}% positive<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        bargap  = 0.22,
        xaxis = list(
          title      = "% Positive Response",
          range      = c(x_min, x_max),
          tickvals   = tick_vals,
          ticksuffix = "%",
          showgrid   = TRUE,
          gridcolor  = "#E2E8F0",
          gridwidth  = 1,
          zeroline   = FALSE,
          tickfont   = list(size = 10)
        ),
        yaxis = list(
          title      = "",
          autorange  = "reversed",
          tickfont   = list(size = 10),
          showgrid   = FALSE
        ),
        font          = list(family = "Inter, sans-serif", size = 11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(orientation = "h", x = 0.05, y = -0.18,
                      font = list(size = 10)),
        margin = list(t = 5, b = 45, l = 12, r = 65)
      )
  })

  # ── LTV distribution: weighted histogram with wider bins, tooltip only ─────
  output$sum_ltv_dist <- renderPlotly({
    d <- summary_data() %>%
      filter(!is.na(ltv), ltv > 0, ltv <= 115)

    if (nrow(d) == 0) return(NULL)

    # 10-point bins are wide enough to read; covers the meaningful 40–105 range
    breaks  <- c(seq(0, 115, by = 10))
    bin_wid <- 9.2    # slightly narrower than 10 so bars have a gap

    make_hist <- function(dd) {
      if (nrow(dd) == 0) return(data.frame(x_mid=numeric(0), pct=numeric(0),
                                           lo=numeric(0), hi=numeric(0)))
      dd$bin_f <- cut(dd$ltv, breaks = breaks, right = FALSE, include.lowest = TRUE)
      lvls     <- levels(dd$bin_f)
      lo_v     <- as.numeric(sub("\\[|\\(", "", sapply(strsplit(lvls, ","), `[`, 1)))
      hi_v     <- as.numeric(sub("\\]|\\)", "", sapply(strsplit(lvls, ","), `[`, 2)))
      mid_lkp  <- setNames((lo_v + hi_v) / 2, lvls)
      lo_lkp   <- setNames(lo_v, lvls)
      hi_lkp   <- setNames(hi_v, lvls)

      dd %>%
        group_by(bin_f) %>%
        summarise(wn = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          pct   = round(wn / sum(wn) * 100, 1),
          x_mid = mid_lkp[as.character(bin_f)],
          lo    = lo_lkp[as.character(bin_f)],
          hi    = hi_lkp[as.character(bin_f)]
        ) %>%
        filter(!is.na(x_mid))
    }

    cu_h    <- make_hist(d %>% filter(Institution_Type == "Credit Union"))
    noncu_h <- make_hist(d %>% filter(Institution_Type == "Non-Credit Union"))

    if (nrow(cu_h) == 0 && nrow(noncu_h) == 0) return(NULL)

    plot_ly() %>%
      add_bars(
        data   = cu_h,
        x      = ~x_mid,
        y      = ~pct,
        name   = "Credit Union",
        width  = bin_wid,
        # NO text on bars — keep it clean; all info in tooltip
        hovertemplate = paste0(
          "<b>Credit Union</b><br>",
          "LTV: %{customdata[0]}–%{customdata[1]}%<br>",
          "Share: <b>%{y:.1f}%</b> of CU loans",
          "<extra></extra>"
        ),
        customdata = ~cbind(lo, hi),
        marker = list(
          color = v2_colors$cu,
          opacity = 0.82,
          line  = list(color = "white", width = 1.5)
        )
      ) %>%
      add_bars(
        data   = noncu_h,
        x      = ~x_mid,
        y      = ~pct,
        name   = "Non-Credit Union",
        width  = bin_wid,
        hovertemplate = paste0(
          "<b>Non-Credit Union</b><br>",
          "LTV: %{customdata[0]}–%{customdata[1]}%<br>",
          "Share: <b>%{y:.1f}%</b> of Non-CU loans",
          "<extra></extra>"
        ),
        customdata = ~cbind(lo, hi),
        marker = list(
          color   = v2_colors$noncu,
          opacity = 0.65,
          line    = list(color = "white", width = 1.5)
        )
      ) %>%
      layout(
        barmode = "overlay",
        xaxis = list(
          title     = "LTV (%)",
          tickvals  = seq(10, 110, by = 10),
          ticktext  = paste0(seq(10, 110, by = 10), "%"),
          showgrid  = FALSE,
          zeroline  = FALSE,
          tickfont  = list(size = 10),
          range     = c(0, 118)
        ),
        yaxis = list(
          title      = "% of Loans",
          showgrid   = TRUE,
          gridcolor  = "#E2E8F0",
          zeroline   = FALSE,
          ticksuffix = "%",
          tickfont   = list(size = 10)
        ),
        font          = list(family = "Inter, sans-serif", size = 11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(orientation = "h", x = 0.1, y = -0.22,
                      font = list(size = 10)),
        margin = list(t = 5, b = 50, l = 48, r = 10)
      )
  })

  # Keep these outputs defined so old references don't error
  output$sum_total_loans_cu    <- renderValueBox({ valueBox(0,"",color="blue") })
  output$sum_total_loans_noncu <- renderValueBox({ valueBox(0,"",color="red")  })
  output$sum_rate_spread_cu    <- renderValueBox({ valueBox(0,"",color="blue") })
  output$sum_rate_spread_noncu <- renderValueBox({ valueBox(0,"",color="red")  })
  output$sum_median_amt_cat_cu    <- renderValueBox({ valueBox(0,"",color="blue") })
  output$sum_median_amt_cat_noncu <- renderValueBox({ valueBox(0,"",color="red")  })
  output$sum_avg_fico_cu    <- renderInfoBox({ infoBox("","",color="blue", fill=TRUE) })
  output$sum_avg_fico_noncu <- renderInfoBox({ infoBox("","",color="red",  fill=TRUE) })
  output$sum_avg_ltv_cu     <- renderInfoBox({ infoBox("","",color="blue", fill=TRUE) })
  output$sum_avg_ltv_noncu  <- renderInfoBox({ infoBox("","",color="red",  fill=TRUE) })
  output$sum_performance_metrics <- renderPlotly({ NULL })
  output$sum_fico_heatmap_cu    <- renderPlotly({ NULL })
  output$sum_fico_heatmap_noncu <- renderPlotly({ NULL })
  # ══════════════════════════════════════════════════════════════════════════
  # TAB 2 — SURVEY RESPONSES
  # ══════════════════════════════════════════════════════════════════════════

  make_yearly_pct <- function(d, var_binary) {
    d %>%
      filter(!is.na(get(var_binary)), !is.na(open_year)) %>%
      group_by(open_year, Institution_Type, response = get(var_binary)) %>%
      summarise(wn = sum(analysis_weight, na.rm=TRUE), .groups="drop") %>%
      group_by(open_year, Institution_Type) %>%
      mutate(pct = wn / sum(wn) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      select(open_year, Institution_Type, pct)
  }

  output$trend_yearly <- renderPlotly({
    vb <- paste0(active_trend_var(), "_binary")
    yd <- make_yearly_pct(trend_data(), vb)
    if (nrow(yd) == 0) return(NULL)
    mn <- max(0,   min(yd$pct, na.rm=TRUE) - 10)
    mx <- min(100, max(yd$pct, na.rm=TRUE) + 10)
    p <- ggplot(yd, aes(x=open_year, y=pct, color=Institution_Type, group=Institution_Type,
                         text=paste0(Institution_Type,": ",round(pct,1),"% (",open_year,")"))) +
      geom_line(linewidth=1.4) + geom_point(size=4) +
      scale_color_manual(values=c("Credit Union"=v2_colors$cu,"Non-Credit Union"=v2_colors$noncu)) +
      scale_y_continuous(limits=c(mn,mx)) +
      labs(x="Year", y="% Positive (Weighted)", color=NULL) +
      theme_minimal() + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>% v2_plotly_layout(legend_bottom=TRUE)
  })

  output$trend_yoy_change <- renderPlotly({
    vb <- paste0(active_trend_var(), "_binary")
    yd <- make_yearly_pct(trend_data(), vb) %>%
      arrange(Institution_Type, open_year) %>%
      group_by(Institution_Type) %>%
      mutate(yoy = pct - lag(pct)) %>%
      filter(!is.na(yoy))
    if (nrow(yd) == 0) return(NULL)
    p <- ggplot(yd, aes(x=open_year, y=yoy, color=Institution_Type, group=Institution_Type,
                         text=paste0(Institution_Type,": ",ifelse(yoy>=0,"+",""),round(yoy,1),"pp"))) +
      geom_line(linewidth=1.3) + geom_point(size=3.5) +
      geom_hline(yintercept=0, linetype="dashed", color="#94A3B8") +
      scale_color_manual(values=c("Credit Union"=v2_colors$cu,"Non-Credit Union"=v2_colors$noncu)) +
      labs(x="Year", y="YoY Change (pp)", color=NULL) +
      theme_minimal() + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>% v2_plotly_layout(legend_bottom=TRUE)
  })

  output$trend_net_diff <- renderPlotly({
    vb <- paste0(active_trend_var(), "_binary")
    yd <- make_yearly_pct(trend_data(), vb) %>%
      pivot_wider(names_from=Institution_Type, values_from=pct) %>%
      mutate(Net = `Credit Union` - `Non-Credit Union`,
             col = ifelse(Net >= 0, "CU Higher", "Non-CU Higher"))
    if (nrow(yd) == 0) return(NULL)
    p <- ggplot(yd, aes(x=open_year, y=Net, fill=col,
                         text=paste0(open_year,": ",ifelse(Net>=0,"+",""),round(Net,1),"pp"))) +
      geom_col(width=0.7) +
      geom_hline(yintercept=0, color="#0F172A") +
      geom_text(aes(label=paste0(ifelse(Net>=0,"+",""),round(Net,1),"%")),
                vjust=ifelse(yd$Net>=0,-0.5,1.5), size=3.5, fontface="bold") +
      scale_fill_manual(values=c("CU Higher"=v2_colors$positive,"Non-CU Higher"=v2_colors$negative)) +
      labs(x="Year", y="Net Diff (pp, CU−Non-CU)", fill=NULL) +
      theme_minimal() + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>% v2_plotly_layout(legend_bottom=TRUE)
  })

  output$trend_by_period <- renderPlotly({
    vb <- paste0(active_trend_var(), "_binary")
    pd <- trend_data() %>%
      filter(!is.na(get(vb)), COVID_period %in% period_order) %>%
      group_by(COVID_period, Institution_Type, response=get(vb)) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE), .groups="drop") %>%
      group_by(COVID_period, Institution_Type) %>%
      mutate(pct=wn/sum(wn)*100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      mutate(COVID_period=factor(COVID_period, levels=period_order))
    if (nrow(pd) == 0) return(NULL)
    p <- ggplot(pd, aes(x=COVID_period, y=pct, fill=Institution_Type,
                         text=paste0(Institution_Type,"\n",COVID_period,"\n",round(pct,1),"%"))) +
      geom_col(position="dodge", width=0.7) +
      geom_text(aes(label=paste0(round(pct,1),"%")),
                position=position_dodge(0.7), vjust=-0.5, size=3.5, fontface="bold") +
      scale_fill_manual(values=c("Credit Union"=v2_colors$cu,"Non-Credit Union"=v2_colors$noncu)) +
      scale_y_continuous(expand=expansion(mult=c(0,.18))) +
      labs(x=NULL, y="% Positive", fill=NULL) +
      theme_minimal() + theme(legend.position="bottom", axis.text.x=element_text(angle=15,hjust=1))
    ggplotly(p, tooltip="text") %>% v2_plotly_layout(legend_bottom=TRUE)
  })

  output$trend_net_by_period <- renderPlotly({
    vb <- paste0(active_trend_var(), "_binary")
    pd <- trend_data() %>%
      filter(!is.na(get(vb)), COVID_period %in% period_order) %>%
      group_by(COVID_period, Institution_Type, response=get(vb)) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE), .groups="drop") %>%
      group_by(COVID_period, Institution_Type) %>%
      mutate(pct=wn/sum(wn)*100) %>%
      filter(response=="Positive (Very/Somewhat)") %>%
      select(COVID_period, Institution_Type, pct) %>%
      pivot_wider(names_from=Institution_Type, values_from=pct) %>%
      mutate(Net=`Credit Union`-`Non-Credit Union`,
             col=ifelse(Net>=0,"CU Higher","Non-CU Higher"),
             COVID_period=factor(COVID_period, levels=period_order))
    if (nrow(pd) == 0) return(NULL)
    sig_stars <- sapply(period_order, function(per) {
      sub_d <- trend_data() %>% filter(COVID_period==per)
      if (nrow(sub_d) < 10) return("")
      test_significance(sub_d, active_trend_var(), "Institution_Type")$significant %>%
        ifelse("*","")
    })
    pd$star <- sig_stars[as.character(pd$COVID_period)]
    p <- ggplot(pd, aes(x=COVID_period, y=Net, fill=col,
                         text=paste0(COVID_period,"\n",ifelse(Net>=0,"+",""),round(Net,1),"pp",star))) +
      geom_col(width=0.65) +
      geom_hline(yintercept=0, color="#0F172A") +
      geom_text(aes(label=paste0(ifelse(Net>=0,"+",""),round(Net,1),"%",star)),
                vjust=ifelse(pd$Net>=0,-0.5,1.5), size=4, fontface="bold") +
      scale_fill_manual(values=c("CU Higher"=v2_colors$positive,"Non-CU Higher"=v2_colors$negative)) +
      labs(x=NULL, y="Net Diff (pp)", fill=NULL,
           caption="* p<0.05 (chi-sq test on positive response proportions)") +
      theme_minimal() + theme(legend.position="bottom", axis.text.x=element_text(angle=15,hjust=1),
                               plot.caption=element_text(size=10,color="#64748B"))
    ggplotly(p, tooltip="text") %>% v2_plotly_layout(legend_bottom=TRUE)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 3 — ML MODEL
  # ══════════════════════════════════════════════════════════════════════════

  # ── Outcome variable definitions ─────────────────────────────────────────────
  # Maps UI choice codes to the underlying variable, display name, and type.
  # Binary outcomes use the _binary column (1=positive, 0=negative).
  # Regression outcomes use the raw numeric column directly.
    # ── Outcome category metadata (for banner display) ─────────────────────────
    outcome_categories <- list(
      sat_overall          = list(cat="Overall Satisfaction",       cls="ob-sat",        icon="fa-star",           type_lbl="Binary"),
      best_deal            = list(cat="Overall Satisfaction",       cls="ob-sat",        icon="fa-thumbs-up",      type_lbl="Binary"),
      lowest_rate          = list(cat="Overall Satisfaction",       cls="ob-sat",        icon="fa-chart-line",     type_lbl="Binary"),
      sat_application      = list(cat="Process Satisfaction",       cls="ob-process",    icon="fa-file-alt",       type_lbl="Binary"),
      sat_closing          = list(cat="Process Satisfaction",       cls="ob-process",    icon="fa-handshake",      type_lbl="Binary"),
      sat_disclosure       = list(cat="Process Satisfaction",       cls="ob-process",    icon="fa-file-lines",     type_lbl="Binary"),
      sat_disclosure_timing= list(cat="Process Satisfaction",       cls="ob-process",    icon="fa-clock",          type_lbl="Binary"),
      costs_as_expected    = list(cat="Process Satisfaction",       cls="ob-process",    icon="fa-equals",         type_lbl="Binary"),
      closing_surprise     = list(cat="Closing Problems",           cls="ob-closing",    icon="fa-triangle-exclamation", type_lbl="Binary"),
      closing_rushed       = list(cat="Closing Problems",           cls="ob-closing",    icon="fa-bolt",           type_lbl="Binary"),
      costs_rolled         = list(cat="Closing Problems",           cls="ob-closing",    icon="fa-money-bill-transfer", type_lbl="Binary"),
      shopped_multiple     = list(cat="Consumer Engagement",        cls="ob-engage",     icon="fa-store",          type_lbl="Binary"),
      lenders_considered   = list(cat="Consumer Engagement",        cls="ob-engage",     icon="fa-list-check",     type_lbl="Binary"),
      preapproval          = list(cat="Consumer Engagement",        cls="ob-engage",     icon="fa-certificate",    type_lbl="Binary"),
      verified_terms       = list(cat="Consumer Engagement",        cls="ob-engage",     icon="fa-magnifying-glass-check", type_lbl="Binary"),
      counseling           = list(cat="Consumer Engagement",        cls="ob-engage",     icon="fa-chalkboard-user", type_lbl="Binary"),
      le_clear             = list(cat="Information & Disclosure",   cls="ob-disclosure", icon="fa-book-open",      type_lbl="Binary"),
      toolkit              = list(cat="Information & Disclosure",   cls="ob-disclosure", icon="fa-toolbox",        type_lbl="Binary"),
      has_arm              = list(cat="Product Complexity",         cls="ob-complexity", icon="fa-sliders",        type_lbl="Binary"),
      has_pmi              = list(cat="Product Complexity",         cls="ob-complexity", icon="fa-shield",         type_lbl="Binary"),
      points_tradeoff      = list(cat="Product Complexity",         cls="ob-complexity", icon="fa-rotate",         type_lbl="Binary"),
      financial_literacy   = list(cat="Financial Wellbeing",        cls="ob-wellbeing",  icon="fa-graduation-cap", type_lbl="Binary"),
      literacy_arm         = list(cat="Financial Wellbeing",        cls="ob-wellbeing",  icon="fa-graduation-cap", type_lbl="Binary"),
      payment_difficulty   = list(cat="Financial Wellbeing",        cls="ob-wellbeing",  icon="fa-house-crack",    type_lbl="Binary"),
      lender_trust         = list(cat="Financial Wellbeing",        cls="ob-wellbeing",  icon="fa-scale-balanced", type_lbl="Binary"),
      sat_recommend        = list(cat="Overall Satisfaction",       cls="ob-sat",        icon="fa-heart",          type_lbl="Binary"),
      rate_spread          = list(cat="Financial Outcome",          cls="ob-financial",  icon="fa-percent",        type_lbl="Continuous")
    )

    outcome_definitions <- list(

      # ── Overall Satisfaction ─────────────────────────────────────────────────
      sat_overall = list(
        var  = "x28a",
        name = "Overall Lender Satisfaction",
        type = "binary",
        desc = "% rating their mortgage lender as Very/Somewhat satisfied overall"
      ),
      best_deal = list(
        var  = "x27a",
        name = "Got Best-Fit Mortgage Terms",
        type = "binary",
        desc = "% who believe the mortgage they got had the best terms to fit their needs"
      ),
      lowest_rate = list(
        var  = "x27b",
        name = "Got Lowest Rate Available",
        type = "binary",
        desc = "% who believe they received the lowest interest rate for which they qualified"
      ),
      sat_recommend = list(
        var  = "x28a",
        name = "Likelihood to Recommend Lender",
        type = "binary",
        desc = "Overall lender satisfaction as a proxy for willingness to recommend"
      ),

      # ── Process Satisfaction ─────────────────────────────────────────────────
      sat_application = list(
        var  = "x28b",
        name = "Satisfaction: Application Process",
        type = "binary",
        desc = "% rating the mortgage application process as Very/Somewhat satisfying"
      ),
      sat_closing = list(
        var  = "x28d",
        name = "Satisfaction: Loan Closing",
        type = "binary",
        desc = "% rating the loan closing process as Very/Somewhat satisfying"
      ),
      sat_disclosure = list(
        var  = "x28e",
        name = "Satisfaction: Disclosure Documents",
        type = "binary",
        desc = "% satisfied with the information provided in mortgage disclosure documents (TRID)"
      ),
      sat_disclosure_timing = list(
        var  = "x28f",
        name = "Satisfaction: Disclosure Timeliness",
        type = "binary",
        desc = "% satisfied with how promptly mortgage disclosure documents were delivered"
      ),
      costs_as_expected = list(
        var  = "x49",
        name = "Closing Costs Matched Expectations",
        type = "binary",
        desc = "% whose final closing costs were similar to what the Loan Estimate showed"
      ),

      # ── Closing Problems ─────────────────────────────────────────────────────
      closing_surprise = list(
        var  = "x53d",
        name = "Faced Unexpected Terms at Closing",
        type = "binary",
        desc = "% who faced mortgage terms at closing that differed from what was expected"
      ),
      closing_rushed = list(
        var  = "x53g",
        name = "Felt Rushed at Loan Closing",
        type = "binary",
        desc = "% who felt rushed or not given enough time to read documents at closing"
      ),
      costs_rolled = list(
        var  = "x48b",
        name = "Closing Costs Rolled Into Loan",
        type = "binary",
        desc = "% whose closing costs were added to the mortgage principal (increases total interest paid)"
      ),

      # ── Consumer Engagement & Shopping ───────────────────────────────────────
      shopped_multiple = list(
        var  = "x12",
        name = "Applied to Multiple Lenders",
        type = "binary",
        desc = "% who actually submitted applications to more than one lender (active shopping)"
      ),
      lenders_considered = list(
        var  = "x11",
        name = "Number of Lenders Seriously Considered",
        type = "binary",
        desc = "% who seriously considered more than one lender before choosing (consideration set)"
      ),
      preapproval = list(
        var  = "x34b",
        name = "Got Pre-Approval Before Offer",
        type = "binary",
        desc = "% who obtained mortgage pre-approval or pre-qualification before making an offer"
      ),
      verified_terms = list(
        var  = "x20h",
        name = "Verified Terms With Outside Source",
        type = "binary",
        desc = "% who checked other sources to confirm their mortgage terms were reasonable"
      ),
      counseling = list(
        var  = "x29",
        name = "Took Home-Buying Course / Counseling",
        type = "binary",
        desc = "% who took a home-buying course or spoke to a professional housing counselor"
      ),

      # ── Information & Disclosure ─────────────────────────────────────────────
      le_clear = list(
        var  = "x21a",
        name = "Loan Estimate Was Easy to Understand",
        type = "binary",
        desc = "% who found their Loan Estimate disclosure document easy to understand"
      ),
      toolkit = list(
        var  = "x18",
        name = "Received Home Loan Toolkit Booklet",
        type = "binary",
        desc = "% who recall receiving the CFPB Home Loan Toolkit from their lender"
      ),

      # ── Product Complexity ───────────────────────────────────────────────────
      has_arm = list(
        var  = "x44",
        name = "Has Adjustable-Rate Mortgage",
        type = "binary",
        desc = "% with an ARM — higher ARM incidence suggests more complex/risky product selection"
      ),
      has_pmi = list(
        var  = "x46e",
        name = "Has Private Mortgage Insurance (PMI)",
        type = "binary",
        desc = "% with PMI — indicates down payment < 20%; CUs may offer portfolio alternatives"
      ),
      points_tradeoff = list(
        var  = "x45",
        name = "Rate-Points Tradeoff Decision Made",
        type = "binary",
        desc = "% who actively chose between paying points for lower rate vs higher rate with lower fees"
      ),

      # ── Financial Wellbeing & Literacy ───────────────────────────────────────
      financial_literacy = list(
        var  = "x56a",
        name = "Financial Literacy: Mortgage Process",
        type = "binary",
        desc = "% who say they can Very/Somewhat well explain the mortgage process to someone else"
      ),
      literacy_arm = list(
        var  = "x56b",
        name = "Financial Literacy: Fixed vs Adjustable Rate",
        type = "binary",
        desc = "% who can explain the difference between fixed-rate and adjustable-rate mortgages"
      ),
      payment_difficulty = list(
        var  = "x93b",
        name = "Likelihood of Future Payment Difficulty",
        type = "binary",
        desc = "% who say it is Very/Somewhat likely they will face payment difficulties in next 2 years"
      ),
      lender_trust = list(
        var  = "x88b",
        name = "Believes Lenders Treat Borrowers Fairly",
        type = "binary",
        desc = "% who agree that most mortgage lenders generally treat borrowers well"
      ),

      # ── Financial Outcome ────────────────────────────────────────────────────
      rate_spread = list(
        var  = "Rate_Spread_Pct",
        name = "Rate Spread Above Market (%)",
        type = "regression",
        desc = "Basis points above the prevailing PMMS market rate — higher = more costly loan"
      )
    )

  prepare_outcome <- function(d, outcome_type) {
    defn <- outcome_definitions[[outcome_type]]
    if (is.null(defn)) {
      # Fallback to overall satisfaction
      defn <- outcome_definitions[["sat_overall"]]
    }

    if (defn$type == "binary") {
      vb <- paste0(defn$var, "_binary")
      # Fall back gracefully if this specific var's binary column doesn't exist
      if (!vb %in% names(d)) {
        # Try first available response var
        vb <- paste0(response_vars[1], "_binary")
      }
      md <- d %>% filter(!is.na(get(vb))) %>%
        mutate(outcome = ifelse(get(vb) == "Positive (Very/Somewhat)", 1, 0))
      list(data = md, name = defn$name, type = "binary", desc = defn$desc)
    } else {
      md <- d %>% filter(!is.na(get(defn$var))) %>%
        mutate(outcome = as.numeric(get(defn$var)))
      list(data = md, name = defn$name, type = "regression", desc = defn$desc)
    }
  }

  # ── Build a feature matrix from a data frame ─────────────────────────────
  build_feature_matrix <- function(d, include_covid = FALSE, include_cu = FALSE) {
    base_cols <- c("FICO_category", "ltv", "dti", "Amount_Borrowed", "Interest_Rate",
                   "Age", "Sex_Label", "Race", "First_Mortgage_Label", "Loan_Type")
    if (include_covid) base_cols <- c(base_cols, "COVID_period")
    if (include_cu)    base_cols <- c(base_cols, "Institution_Type")
    feats <- d %>%
      select(all_of(base_cols), analysis_weight) %>%
      mutate(across(where(is.character), as.factor))
    # Drop single-level factors and all-NA / zero-variance columns
    for (col in names(feats)[sapply(feats, is.factor)])
      if (length(unique(feats[[col]])) < 2) feats[[col]] <- NULL
    feats <- feats %>% select(where(~!all(is.na(.)) && length(unique(na.omit(.))) > 1))
    X <- model.matrix(~ . - 1 - analysis_weight, data = feats)
    list(X = X, w = d$analysis_weight)
  }

  # ── Train XGBoost on one group (CU or Non-CU) ────────────────────────────
  train_xgb_model <- function(d, include_covid = FALSE) {
    mx  <- build_feature_matrix(d, include_covid = include_covid, include_cu = FALSE)
    obj <- ifelse(mean(d$outcome %in% c(0, 1)) == 1, "binary:logistic", "reg:squarederror")
    m   <- xgb.train(
      params = list(max_depth = 4, eta = 0.1, objective = obj),
      data   = xgb.DMatrix(mx$X, label = d$outcome, weight = mx$w),
      nrounds = 50, verbose = 0
    )
    imp <- xgb.importance(model = m, feature_names = colnames(mx$X))
    list(model = m, importance = imp, X = mx$X)
  }

  # ── Train a COMBINED model (both groups, Institution_Type as predictor) ───
  # This gives model-adjusted predicted values that account for all covariates.
  train_combined_model <- function(d, include_covid = FALSE) {
    mx  <- build_feature_matrix(d, include_covid = include_covid, include_cu = TRUE)
    obj <- ifelse(mean(d$outcome %in% c(0, 1)) == 1, "binary:logistic", "reg:squarederror")
    m   <- xgb.train(
      params = list(max_depth = 4, eta = 0.1, objective = obj),
      data   = xgb.DMatrix(mx$X, label = d$outcome, weight = mx$w),
      nrounds = 50, verbose = 0
    )
    # Predicted values on training data (in-sample — used for adjusted means)
    preds <- predict(m, xgb.DMatrix(mx$X))
    list(model = m, preds = preds, y = d$outcome,
         cu_flag = d$Institution_Type == "Credit Union",
         is_binary = mean(d$outcome %in% c(0, 1)) == 1)
  }

  # ── Model quality metrics ─────────────────────────────────────────────────
  compute_model_metrics <- function(combined) {
    preds    <- combined$preds
    y        <- combined$y
    cu_flag  <- combined$cu_flag

    if (combined$is_binary) {
      # AUC via trapezoid rule
      auc <- tryCatch({
        thresh  <- sort(unique(preds), decreasing = TRUE)
        tpr <- sapply(thresh, function(t) mean(preds[y == 1] >= t))
        fpr <- sapply(thresh, function(t) mean(preds[y == 0] >= t))
        sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1))) / 2
      }, error = function(e) NA_real_)

      # Accuracy at 0.5 threshold
      acc <- mean((preds >= 0.5) == y, na.rm = TRUE)

      # Model-adjusted means = average predicted probability by group
      adj_cu    <- mean(preds[cu_flag],  na.rm = TRUE)
      adj_noncu <- mean(preds[!cu_flag], na.rm = TRUE)

      list(auc = auc, acc = acc,
           adj_cu = adj_cu, adj_noncu = adj_noncu,
           metric_label = "AUC",
           metric_value = auc,
           metric_fmt   = function(x) sprintf("%.3f", x))
    } else {
      # R-squared
      ss_res <- sum((y - preds)^2, na.rm = TRUE)
      ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
      r2 <- 1 - ss_res / ss_tot

      adj_cu    <- mean(preds[cu_flag],  na.rm = TRUE)
      adj_noncu <- mean(preds[!cu_flag], na.rm = TRUE)

      list(r2 = r2,
           adj_cu = adj_cu, adj_noncu = adj_noncu,
           metric_label = "R²",
           metric_value = r2,
           metric_fmt   = function(x) sprintf("%.3f", x))
    }
  }

  ml_results <- reactiveValues(data = NULL)
  observeEvent(input$reset_ml_cu, { ml_results$data <- NULL })

  # ── Outcome category banner ───────────────────────────────────────────────
  output$ml_outcome_banner <- renderUI({
    sel  <- input$ml_cu_outcome
    if (is.null(sel)) sel <- "sat_overall"
    meta <- outcome_categories[[sel]]
    defn <- outcome_definitions[[sel]]
    if (is.null(meta) || is.null(defn)) return(NULL)

    tags$div(
      class = paste("outcome-banner", meta$cls),
      tags$div(
        class = "ob-icon",
        tags$i(class = paste("fas", meta$icon))
      ),
      tags$div(
        class = "ob-text",
        tags$div(class = "ob-category", meta$cat),
        tags$div(class = "ob-name",     defn$name),
        tags$div(class = "ob-desc",     defn$desc)
      )
    )
  })

  observeEvent(input$run_ml_cu, {
    tryCatch({
      oc   <- prepare_outcome(df, input$ml_cu_outcome)
      md   <- oc$data
      all3 <- length(input$ml_cu_period) == 3 && all(period_order %in% input$ml_cu_period)
      if (!is.null(input$ml_cu_period) && length(input$ml_cu_period) > 0 && !all3)
        md <- md %>% filter(COVID_period %in% input$ml_cu_period)

      cu_d    <- md %>% filter(Institution_Type == "Credit Union")
      noncu_d <- md %>% filter(Institution_Type == "Non-Credit Union")

      # ① Separate models (for feature importance charts)
      cu_m    <- train_xgb_model(cu_d,    include_covid = all3)
      noncu_m <- train_xgb_model(noncu_d, include_covid = all3)

      # ② Combined model (Institution_Type included as a predictor)
      #    This gives model-adjusted means that control for all covariates
      combined <- train_combined_model(md, include_covid = all3)
      metrics  <- compute_model_metrics(combined)

      # ③ Raw unadjusted comparison (for transparency — shown alongside adjusted)
      tr <- if (oc$type == "binary")
        prop.test(c(sum(cu_d$outcome), sum(noncu_d$outcome)), c(nrow(cu_d), nrow(noncu_d)))
      else
        t.test(cu_d$outcome, noncu_d$outcome)

      # ④ Test on model-adjusted means (approximate — using t-test on predicted values)
      adj_test <- tryCatch({
        preds_cu    <- combined$preds[combined$cu_flag]
        preds_noncu <- combined$preds[!combined$cu_flag]
        t.test(preds_cu, preds_noncu)$p.value
      }, error = function(e) NA_real_)

      period_txt <- if (all3) "All Periods (COVID as feature)" else paste(input$ml_cu_period, collapse=", ")

      ml_results$data <- list(
        cu_model      = cu_m,
        noncu_model   = noncu_m,
        outcome_name  = oc$name,
        outcome_type  = oc$type,
        outcome_desc  = oc$desc,
        # Raw (unadjusted) means
        cu_mean       = mean(cu_d$outcome, na.rm = TRUE),
        noncu_mean    = mean(noncu_d$outcome, na.rm = TRUE),
        raw_p         = tr$p.value,
        # Model-adjusted means (controlling for all features)
        adj_cu        = metrics$adj_cu,
        adj_noncu     = metrics$adj_noncu,
        adj_p         = adj_test,
        # Model fit
        model_metric_label = metrics$metric_label,
        model_metric_value = metrics$metric_value,
        n_cu          = nrow(cu_d),
        n_noncu       = nrow(noncu_d),
        period_text   = period_txt,
        success       = TRUE
      )
    }, error = function(e) ml_results$data <- list(success = FALSE, error = as.character(e)))
  })

  output$ml_cu_performance <- renderUI({
    res <- ml_results$data
    if (is.null(res)) return(HTML(
      "<div style='text-align:center;color:#94A3B8;padding:50px 20px;'>
         <i class='fas fa-brain fa-2x' style='margin-bottom:12px;opacity:.3;'></i><br>
         <strong>Select an outcome and click Run Model</strong><br>
         <span style='font-size:12px;'>The analysis will compare borrower outcomes for CU and Non-CU groups<br>
         plus a combined model to compute adjusted comparisons.</span>
       </div>"))
    if (!res$success) return(HTML(paste0(
      "<div class='alert alert-danger' style='margin:16px;'>",
      "<strong>Model error:</strong> ", res$error, "</div>")))

    is_bin <- res$outcome_type == "binary"

    fmt_raw <- function(x) if (is_bin) paste0(round(x * 100, 1), "%") else round(x, 3)
    fmt_adj <- function(x) if (is_bin) paste0(round(x * 100, 1), "%") else round(x, 3)

    raw_diff  <- res$cu_mean  - res$noncu_mean
    adj_diff  <- res$adj_cu   - res$adj_noncu

    raw_dir <- if (is_bin) {
      if (raw_diff > 0) paste0("+", round(raw_diff*100,1), " pp CU higher")
      else paste0(round(raw_diff*100,1), " pp CU lower")
    } else {
      if (raw_diff > 0) paste0("+", round(raw_diff,3), " CU higher")
      else paste0(round(raw_diff,3), " CU lower")
    }
    adj_dir <- if (is_bin) {
      if (adj_diff > 0) paste0("+", round(adj_diff*100,1), " pp CU higher")
      else paste0(round(adj_diff*100,1), " pp CU lower")
    } else {
      if (adj_diff > 0) paste0("+", round(adj_diff,3), " CU higher")
      else paste0(round(adj_diff,3), " CU lower")
    }

    raw_sig_col <- if (!is.na(res$raw_p) && res$raw_p < 0.05) v2_colors$positive else v2_colors$negative
    adj_sig_col <- if (!is.na(res$adj_p) && res$adj_p < 0.05) v2_colors$positive else v2_colors$negative
    raw_sig_lbl <- if (!is.na(res$raw_p) && res$raw_p < 0.05)
      paste0("Sig ", sig_label(res$raw_p)) else "n.s."
    adj_sig_lbl <- if (!is.na(res$adj_p) && res$adj_p < 0.05)
      paste0("Sig ", sig_label(res$adj_p)) else "n.s."

    fit_lbl <- ifelse(is_bin,
      sprintf("AUC = %.3f  (1.0 = perfect, 0.5 = random)", res$model_metric_value),
      sprintf("R\u00b2 = %.3f  (1.0 = perfect fit)", res$model_metric_value))

    auc_color <- if (!is.na(res$model_metric_value)) {
      v <- res$model_metric_value
      if (is_bin) { if (v >= 0.75) v2_colors$positive else if (v >= 0.60) "#D97706" else v2_colors$negative }
      else        { if (v >= 0.3)  v2_colors$positive else if (v >= 0.1)  "#D97706" else v2_colors$negative }
    } else "#94A3B8"

    HTML(sprintf('
      <div style="padding:4px 0;">

        <!-- Outcome header -->
        <div style="background:#F8FAFC;border-radius:8px;padding:10px 14px;margin-bottom:14px;
                    border-left:3px solid #7C3AED;display:flex;align-items:center;gap:12px;">
          <div style="flex:1;">
            <span style="font-size:11px;font-weight:700;color:#7C3AED;text-transform:uppercase;
                         letter-spacing:.6px;">OUTCOME</span>
            <span style="font-size:13px;font-weight:600;color:#0F172A;margin-left:10px;">%s</span><br>
            <span style="font-size:11px;color:#64748B;">%s &nbsp;|&nbsp;
              n = %s CU &nbsp;+&nbsp; %s Non-CU &nbsp;|&nbsp; Period: %s</span>
          </div>
          <div style="text-align:right;min-width:140px;">
            <div style="font-size:11px;color:#64748B;">Model fit</div>
            <div style="font-size:18px;font-weight:700;color:%s;">%s</div>
            <div style="font-size:10px;color:#94A3B8;">%s</div>
          </div>
        </div>

        <!-- Two-row comparison: raw vs adjusted -->
        <div style="display:grid;grid-template-columns:110px 1fr 1fr 90px 90px;gap:8px;
                    align-items:center;margin-bottom:8px;">
          <!-- Headers -->
          <div></div>
          <div style="text-align:center;font-size:10px;font-weight:700;color:%s;
                      text-transform:uppercase;letter-spacing:.5px;">Credit Union</div>
          <div style="text-align:center;font-size:10px;font-weight:700;color:%s;
                      text-transform:uppercase;letter-spacing:.5px;">Non-Credit Union</div>
          <div style="text-align:center;font-size:10px;font-weight:600;color:#475569;">Difference</div>
          <div style="text-align:center;font-size:10px;font-weight:600;color:#475569;">Result</div>

          <!-- Raw row -->
          <div style="font-size:11px;font-weight:600;color:#64748B;background:#F8FAFC;
                      border-radius:6px;padding:4px 8px;">
            <i class="fas fa-ruler" style="margin-right:4px;"></i>Raw
            <div style="font-size:9px;font-weight:400;">No controls</div>
          </div>
          <div style="text-align:center;background:#EFF6FF;border-radius:8px;padding:12px 8px;">
            <div style="font-size:20px;font-weight:700;color:%s;">%s</div>
          </div>
          <div style="text-align:center;background:#FEF2F2;border-radius:8px;padding:12px 8px;">
            <div style="font-size:20px;font-weight:700;color:%s;">%s</div>
          </div>
          <div style="text-align:center;font-size:12px;font-weight:600;color:#0F172A;">%s</div>
          <div style="text-align:center;font-size:12px;font-weight:700;color:%s;">%s</div>

          <!-- Adjusted row -->
          <div style="font-size:11px;font-weight:600;color:#7C3AED;background:#F5F3FF;
                      border-radius:6px;padding:4px 8px;">
            <i class="fas fa-sliders" style="margin-right:4px;"></i>Adjusted
            <div style="font-size:9px;font-weight:400;">Adjusted estimate</div>
          </div>
          <div style="text-align:center;background:#EFF6FF;border-radius:8px;padding:12px 8px;
                      border:2px solid #BFDBFE;">
            <div style="font-size:20px;font-weight:700;color:%s;">%s</div>
          </div>
          <div style="text-align:center;background:#FEF2F2;border-radius:8px;padding:12px 8px;
                      border:2px solid #FECACA;">
            <div style="font-size:20px;font-weight:700;color:%s;">%s</div>
          </div>
          <div style="text-align:center;font-size:12px;font-weight:600;color:#0F172A;">%s</div>
          <div style="text-align:center;font-size:12px;font-weight:700;color:%s;">%s</div>
        </div>

        <!-- Footnote -->
        <div style="font-size:10px;color:#94A3B8;margin-top:8px;padding:6px 10px;
                    background:#F8FAFC;border-radius:6px;">
          <strong>Raw:</strong> Simple group comparison (prop.test / t-test) — does not control for borrower characteristics.
          &nbsp;<strong>Adjusted:</strong> Predicted values controlling for FICO, LTV, DTI, loan type,
          race, gender, age, loan amount, interest rate%s.
          AUC and R\u00b2 measure how well the model predicts the outcome overall.
        </div>
      </div>',
      res$outcome_name,
      ifelse(is_bin, "Binary (% positive response)", "Continuous (mean value)"),
      format(res$n_cu, big.mark=","), format(res$n_noncu, big.mark=","),
      res$period_text,
      auc_color, ifelse(!is.na(res$model_metric_value),
                        ifelse(is_bin, sprintf("AUC %.3f", res$model_metric_value),
                                       sprintf("R\u00b2 %.3f", res$model_metric_value)),
                        "N/A"),
      fit_lbl,
      v2_colors$cu, v2_colors$noncu,
      # Raw row
      v2_colors$cu,    fmt_raw(res$cu_mean),
      v2_colors$noncu, fmt_raw(res$noncu_mean),
      raw_dir,
      raw_sig_col, raw_sig_lbl,
      # Adjusted row
      v2_colors$cu,    fmt_adj(res$adj_cu),
      v2_colors$noncu, fmt_adj(res$adj_noncu),
      adj_dir,
      adj_sig_col, adj_sig_lbl,
      # Footnote COVID note
      ifelse(grepl("COVID as feature", res$period_text), " + COVID period", "")
    ))
  })

  # ── Human-readable feature label lookup ──────────────────────────────────────
  # XGBoost encodes categorical variables as dummy columns named e.g.
  # "FICO_categoryGood (670-739)" or "RaceWhite". We map these back to
  # plain-English phrases a non-technical reader can understand.
  feature_label_map <- c(
    # FICO score category dummies
    "FICO_categoryExceptional (800+)"  = "Credit Score: Exceptional (800+)",
    "FICO_categoryVery Good (740-799)" = "Credit Score: Very Good (740-799)",
    "FICO_categoryGood (670-739)"      = "Credit Score: Good (670-739)",
    "FICO_categoryFair (580-669)"      = "Credit Score: Fair (580-669)",
    "FICO_categoryPoor (<580)"         = "Credit Score: Poor (<580)",
    "FICO_categoryUnknown"             = "Credit Score: Unknown",

    # COVID period dummies
    "COVID_periodPre-COVID (Before Mar 2020)" = "Time Period: Pre-COVID",
    "COVID_periodCOVID (Mar 2020 - Jun 2021)" = "Time Period: During COVID",
    "COVID_periodPost-COVID (Jul 2021+)"       = "Time Period: Post-COVID",

    # Continuous financial variables
    "ltv"             = "Loan-to-Value Ratio (LTV %)",
    "dti"             = "Debt-to-Income Ratio (DTI %)",
    "Amount_Borrowed" = "Loan Amount Borrowed ($)",
    "Interest_Rate"   = "Mortgage Interest Rate (%)",
    "Age"             = "Borrower Age",

    # Demographic dummies
    "Sex_LabelFemale"  = "Gender: Female",
    "Sex_LabelMale"    = "Gender: Male",
    "Sex_LabelUnknown" = "Gender: Unknown",
    "RaceWhite"        = "Race: White",
    "RaceBlack"        = "Race: Black",
    "RaceAsian"        = "Race: Asian",
    "RaceOther"        = "Race: Other",
    "RaceUnknown"      = "Race: Unknown",

    # Loan characteristic dummies
    "First_Mortgage_LabelFirst Mortgage" = "First-Time Homebuyer",
    "First_Mortgage_LabelNot First"      = "Not First Mortgage",
    "First_Mortgage_LabelUnknown"        = "First Mortgage: Unknown",
    "Loan_TypeConventional"              = "Loan Type: Conventional",
    "Loan_TypeFHA/VA/FSA"               = "Loan Type: FHA / VA / FSA",
    "Loan_TypeUnknown"                   = "Loan Type: Unknown"
  )

  # Convert a raw XGBoost feature name to a plain-English label
  readable_feature <- function(raw_name) {
    lbl <- feature_label_map[raw_name]
    if (!is.na(lbl)) return(lbl)
    # Fallback: clean up camelCase and underscores
    cleaned <- gsub("_", " ", raw_name)
    cleaned <- gsub("([a-z])([A-Z])", "\\1 \\2", cleaned)
    tools::toTitleCase(tolower(cleaned))
  }

  make_importance_plot <- function(imp_obj, total_gain, col) {
    top     <- as.data.frame(imp_obj) %>% head(10)
    top$Pct <- round(top$Gain / total_gain * 100, 1)

    # Apply human-readable labels
    top$Label <- sapply(top$Feature, readable_feature)

    # Truncate very long labels for display
    top$Label <- ifelse(nchar(top$Label) > 38,
                        paste0(substr(top$Label, 1, 36), "…"),
                        top$Label)

    top$Label <- factor(top$Label, levels = rev(top$Label))

    p <- ggplot(top,
                aes(x = Pct, y = Label,
                    text = paste0(top$Label, "\nImportance: ", top$Pct, "%"))) +
      geom_col(fill = col, width = 0.65) +
      geom_text(aes(label = paste0(Pct, "%")),
                hjust = -0.12, size = 3.4, fontface = "bold",
                color = v2_colors$text_primary) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
      labs(x = "Relative Importance (%)", y = NULL,
           caption = "Importance = share of total information gain explained by this feature") +
      theme_minimal(base_family = "sans") +
      theme(
        axis.text.y      = element_text(size = 10, color = "#0F172A"),
        axis.text.x      = element_text(size = 9,  color = "#475569"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.caption       = element_text(size = 9, color = "#94A3B8", hjust = 0)
      )

    ggplotly(p, tooltip = "text") %>%
      v2_plotly_layout(legend_bottom = FALSE) %>%
      layout(margin = list(l = 10, r = 20, t = 10, b = 40))
  }

  output$ml_cu_importance_cu <- renderPlotly({
    res <- ml_results$data
    if (is.null(res) || !res$success) return(NULL)
    make_importance_plot(res$cu_model$importance,
                         sum(res$cu_model$importance$Gain), v2_colors$cu)
  })
  output$ml_cu_importance_noncu <- renderPlotly({
    res <- ml_results$data
    if (is.null(res) || !res$success) return(NULL)
    make_importance_plot(res$noncu_model$importance,
                         sum(res$noncu_model$importance$Gain), v2_colors$noncu)
  })

  output$ml_cu_interpretation <- renderUI({
    res <- ml_results$data
    if (is.null(res)) return(HTML(
      "<p style='color:#94A3B8;font-size:13px;padding:8px;'>Run model to see findings.</p>"))
    if (!res$success) return(HTML(
      "<div class='alert alert-danger'>Model failed — check data availability for this outcome.</div>"))

    is_bin <- isTRUE(res$outcome_type == "binary")

    # ── Raw difference summary ────────────────────────────────────────────────
    raw_p  <- if (!is.null(res$raw_p)  && !is.na(res$raw_p))  res$raw_p  else 1
    adj_p  <- if (!is.null(res$adj_p)  && !is.na(res$adj_p))  res$adj_p  else 1
    cu_m   <- if (!is.null(res$cu_mean)   && !is.na(res$cu_mean))   res$cu_mean   else 0
    ncu_m  <- if (!is.null(res$noncu_mean)&& !is.na(res$noncu_mean))res$noncu_mean else 0
    adj_cu <- if (!is.null(res$adj_cu)   && !is.na(res$adj_cu))   res$adj_cu   else 0
    adj_nc <- if (!is.null(res$adj_noncu)&& !is.na(res$adj_noncu))res$adj_noncu else 0

    raw_diff <- cu_m  - ncu_m
    adj_diff <- adj_cu - adj_nc

    fmt_diff <- function(d) {
      if (is_bin) paste0(ifelse(d >= 0, "+", ""), round(d * 100, 1), " pp")
      else        paste0(ifelse(d >= 0, "+", ""), round(d, 3))
    }

    sig_badge <- function(p, label) {
      col <- if (p < 0.05) v2_colors$positive else "#94A3B8"
      icon <- if (p < 0.05) "fa-check-circle" else "fa-minus-circle"
      star <- sig_label(p)
      txt  <- if (p < 0.05) paste0("Significant ", star) else "Not significant"
      sprintf("<span style='color:%s;font-weight:700;font-size:12px;'>
               <i class='fas %s' style='margin-right:4px;'></i>%s (%s)</span>",
              col, icon, txt, label)
    }

    # ── Top features with readable labels ────────────────────────────────────
    cu_imp_df    <- tryCatch(as.data.frame(res$cu_model$importance),    error = function(e) NULL)
    noncu_imp_df <- tryCatch(as.data.frame(res$noncu_model$importance), error = function(e) NULL)

    top_cu    <- if (!is.null(cu_imp_df)    && nrow(cu_imp_df)    > 0)
                   readable_feature(cu_imp_df$Feature[1])    else "N/A"
    top_noncu <- if (!is.null(noncu_imp_df) && nrow(noncu_imp_df) > 0)
                   readable_feature(noncu_imp_df$Feature[1]) else "N/A"
    top2_cu    <- if (!is.null(cu_imp_df)    && nrow(cu_imp_df)    > 1)
                   readable_feature(cu_imp_df$Feature[2])    else "N/A"
    top2_noncu <- if (!is.null(noncu_imp_df) && nrow(noncu_imp_df) > 1)
                   readable_feature(noncu_imp_df$Feature[2]) else "N/A"

    # ── Model quality note ────────────────────────────────────────────────────
    fit_val   <- if (!is.null(res$model_metric_value) && !is.na(res$model_metric_value))
                   res$model_metric_value else NA
    fit_label <- if (!is.null(res$model_metric_label)) res$model_metric_label else "Fit"
    fit_note  <- if (!is.na(fit_val)) {
      quality <- if (is_bin) {
        if (fit_val >= 0.75) "good"
        else if (fit_val >= 0.60) "moderate"
        else "weak"
      } else {
        if (fit_val >= 0.30) "good"
        else if (fit_val >= 0.10) "moderate"
        else "weak"
      }
      qual_col  <- c(good="#16A34A", moderate="#D97706", weak="#94A3B8")[quality]
      qual_desc <- c(good = "The model fits well — adjusted comparisons are reliable.",
                     moderate = "Moderate model fit — adjusted results are indicative.",
                     weak = "Weak model fit — treat adjusted results cautiously.")[quality]
      sprintf("<span style='color:%s;font-weight:600;'>%s = %.3f (%s)</span> — %s",
              qual_col, fit_label, fit_val, quality, qual_desc)
    } else "Model fit metric unavailable."

    # ── Consistency check ─────────────────────────────────────────────────────
    both_sig  <- raw_p < 0.05 && adj_p < 0.05
    only_raw  <- raw_p < 0.05 && adj_p >= 0.05
    only_adj  <- raw_p >= 0.05 && adj_p < 0.05
    neither   <- raw_p >= 0.05 && adj_p >= 0.05
    same_dir  <- sign(raw_diff) == sign(adj_diff)

    consistency_html <- if (both_sig && same_dir) {
      "<div style='background:#F0FDF4;border-left:3px solid #16A34A;border-radius:6px;
                   padding:8px 12px;margin:8px 0;font-size:12px;color:#15803D;'>
         <strong>&#10003; Robust finding:</strong> The CU difference is significant both before
         and after controlling for borrower characteristics. This suggests the institution type
         itself — not just who the borrowers are — explains the difference.
       </div>"
    } else if (only_raw) {
      "<div style='background:#FFFBEB;border-left:3px solid #D97706;border-radius:6px;
                   padding:8px 12px;margin:8px 0;font-size:12px;color:#92400E;'>
         <strong>&#9888; Explained by borrower mix:</strong> The raw CU gap disappears after
         controlling for borrower characteristics. CU and Non-CU borrowers likely differ in
         FICO, loan type, or demographics — not necessarily lender behavior.
       </div>"
    } else if (only_adj) {
      "<div style='background:#EFF6FF;border-left:3px solid #2563EB;border-radius:6px;
                   padding:8px 12px;margin:8px 0;font-size:12px;color:#1E40AF;'>
         <strong>&#9432; Emerges after adjustment:</strong> No raw difference, but a gap appears
         after controlling for covariates. CU borrowers may be compositionally different in
         ways that mask an underlying lender effect.
       </div>"
    } else {
      "<div style='background:#F8FAFC;border-left:3px solid #94A3B8;border-radius:6px;
                   padding:8px 12px;margin:8px 0;font-size:12px;color:#475569;'>
         <strong>No significant difference</strong> detected either before or after adjustment.
         CU and Non-CU borrowers perform similarly on this outcome.
       </div>"
    }

    HTML(paste0(
      "<div style='font-size:13px;line-height:1.8;padding:4px;'>",

      # Significance row
      "<div style='display:flex;gap:16px;flex-wrap:wrap;margin-bottom:10px;'>",
      "<div>", sig_badge(raw_p,  "unadjusted"), "</div>",
      "<div>", sig_badge(adj_p, "adjusted estimate"), "</div>",
      "</div>",

      # Consistency interpretation
      consistency_html,

      # Magnitude
      "<div style='display:grid;grid-template-columns:1fr 1fr;gap:10px;margin:10px 0;'>",
        "<div style='background:#F8FAFC;border-radius:8px;padding:10px 12px;'>",
          "<div style='font-size:10px;font-weight:700;color:#475569;text-transform:uppercase;
                       letter-spacing:.5px;'>Raw Difference</div>",
          "<div style='font-size:18px;font-weight:700;color:#0F172A;margin-top:2px;'>",
            fmt_diff(raw_diff), "</div>",
          "<div style='font-size:10px;color:#94A3B8;'>CU vs Non-CU, no controls</div>",
        "</div>",
        "<div style='background:#F5F3FF;border-radius:8px;padding:10px 12px;'>",
          "<div style='font-size:10px;font-weight:700;color:#7C3AED;text-transform:uppercase;
                       letter-spacing:.5px;'>Adjusted Difference</div>",
          "<div style='font-size:18px;font-weight:700;color:#0F172A;margin-top:2px;'>",
            fmt_diff(adj_diff), "</div>",
          "<div style='font-size:10px;color:#94A3B8;'>After XGBoost controls</div>",
        "</div>",
      "</div>",

      # Top features
      "<hr class='v2'>",
      "<div style='font-size:12px;font-weight:600;color:#0F172A;margin-bottom:6px;'>",
        "&#127919; Top predictors of <em>", res$outcome_name, "</em></div>",
      "<div style='display:grid;grid-template-columns:1fr 1fr;gap:8px;font-size:11px;'>",
        "<div>",
          "<div style='font-weight:600;color:", v2_colors$cu, ";margin-bottom:3px;'>",
            "Credit Union borrowers</div>",
          "<div style='background:#EFF6FF;border-radius:6px;padding:6px 10px;margin-bottom:4px;'>",
            "1. ", top_cu, "</div>",
          "<div style='background:#EFF6FF;border-radius:6px;padding:6px 10px;'>",
            "2. ", top2_cu, "</div>",
        "</div>",
        "<div>",
          "<div style='font-weight:600;color:", v2_colors$noncu, ";margin-bottom:3px;'>",
            "Non-Credit Union borrowers</div>",
          "<div style='background:#FEF2F2;border-radius:6px;padding:6px 10px;margin-bottom:4px;'>",
            "1. ", top_noncu, "</div>",
          "<div style='background:#FEF2F2;border-radius:6px;padding:6px 10px;'>",
            "2. ", top2_noncu, "</div>",
        "</div>",
      "</div>",

      # Model fit
      "<hr class='v2'>",
      "<div style='font-size:11px;color:#475569;'>", fit_note, "</div>",
      "<div style='font-size:10px;color:#94A3B8;margin-top:4px;'>",
        "Period: ", res$period_text, "</div>",

      "</div>"
    ))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # ══════════════════════════════════════════════════════════════════════════
  # TAB 4 — REGRESSION ANALYSIS
  # ══════════════════════════════════════════════════════════════════════════

  # ── Outcome definitions for regression tab ─────────────────────────────────
  reg_outcome_defs <- list(
    # Satisfaction & Experience
    reg_sat_overall     = list(var="x28a",          name="Overall Lender Satisfaction",         type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-star"),
    reg_best_deal       = list(var="x27a",          name="Got Best-Fit Mortgage Terms",          type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-thumbs-up"),
    reg_lowest_rate     = list(var="x27b",          name="Got Lowest Rate Available",            type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-chart-line"),
    reg_sat_application = list(var="x28b",          name="Satisfaction: Application Process",   type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-file-alt"),
    reg_sat_closing     = list(var="x28d",          name="Satisfaction: Loan Closing",          type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-handshake"),
    reg_sat_disclosure  = list(var="x28e",          name="Satisfaction: Disclosure Documents",  type="binary",     cat="Satisfaction & Experience",    cls="ob-sat-reg",        icon="fa-file-lines"),
    # Closing & Transparency
    reg_costs_expected  = list(var="x49",           name="Closing Costs Matched Expectations",  type="binary",     cat="Closing & Transparency",       cls="ob-closing-reg",    icon="fa-equals"),
    reg_closing_surprise= list(var="x53d",          name="Faced Unexpected Terms at Closing",   type="binary",     cat="Closing & Transparency",       cls="ob-closing-reg",    icon="fa-triangle-exclamation"),
    reg_closing_rushed  = list(var="x53g",          name="Felt Rushed at Loan Closing",         type="binary",     cat="Closing & Transparency",       cls="ob-closing-reg",    icon="fa-bolt"),
    reg_costs_rolled    = list(var="x48b",          name="Closing Costs Rolled Into Loan",      type="binary",     cat="Closing & Transparency",       cls="ob-closing-reg",    icon="fa-money-bill-transfer"),
    # Consumer Behavior
    reg_shopped         = list(var="x12",           name="Applied to Multiple Lenders",         type="binary",     cat="Consumer Behavior & Engagement",cls="ob-engage-reg",    icon="fa-store"),
    reg_considered      = list(var="x11",           name="Considered Multiple Lenders",         type="binary",     cat="Consumer Behavior & Engagement",cls="ob-engage-reg",    icon="fa-list-check"),
    reg_preapproval     = list(var="x34b",          name="Got Pre-Approval Before Offer",       type="binary",     cat="Consumer Behavior & Engagement",cls="ob-engage-reg",    icon="fa-certificate"),
    reg_verified        = list(var="x20h",          name="Verified Terms Externally",           type="binary",     cat="Consumer Behavior & Engagement",cls="ob-engage-reg",    icon="fa-magnifying-glass-check"),
    reg_counseling      = list(var="x29",           name="Took Homebuying Course/Counseling",   type="binary",     cat="Consumer Behavior & Engagement",cls="ob-engage-reg",    icon="fa-chalkboard-user"),
    # Disclosure & Literacy
    reg_le_clear        = list(var="x21a",          name="Loan Estimate Was Clear",             type="binary",     cat="Disclosure & Financial Literacy",cls="ob-disclosure-reg",icon="fa-book-open"),
    reg_toolkit         = list(var="x18",           name="Received Home Loan Toolkit",          type="binary",     cat="Disclosure & Financial Literacy",cls="ob-disclosure-reg",icon="fa-toolbox"),
    reg_lit_mortgage    = list(var="x56a",          name="Financial Literacy: Mortgage Process",type="binary",     cat="Disclosure & Financial Literacy",cls="ob-disclosure-reg",icon="fa-graduation-cap"),
    reg_lit_arm         = list(var="x56b",          name="Financial Literacy: Fixed vs ARM",    type="binary",     cat="Disclosure & Financial Literacy",cls="ob-disclosure-reg",icon="fa-graduation-cap"),
    # Product & Risk
    reg_has_arm         = list(var="x44",           name="Has Adjustable-Rate Mortgage",        type="binary",     cat="Product & Risk",               cls="ob-product-reg",    icon="fa-sliders"),
    reg_has_pmi         = list(var="x46e",          name="Has Private Mortgage Insurance",      type="binary",     cat="Product & Risk",               cls="ob-product-reg",    icon="fa-shield"),
    reg_points          = list(var="x45",           name="Made Rate-Points Tradeoff Decision",  type="binary",     cat="Product & Risk",               cls="ob-product-reg",    icon="fa-rotate"),
    # Financial (continuous)
    reg_rate_spread     = list(var="Rate_Spread_Pct",name="Rate Spread Above Market (%)",       type="continuous", cat="Financial Outcomes",           cls="ob-financial-reg",  icon="fa-percent"),
    reg_interest_rate   = list(var="Interest_Rate", name="Mortgage Interest Rate (%)",          type="continuous", cat="Financial Outcomes",           cls="ob-financial-reg",  icon="fa-landmark"),
    # Wellbeing
    reg_payment_diff    = list(var="x93b",          name="Likelihood of Payment Difficulty",    type="binary",     cat="Financial Wellbeing",          cls="ob-wellbeing-reg",  icon="fa-house-crack"),
    reg_lender_trust    = list(var="x88b",          name="Believes Lenders Treat Borrowers Fairly",type="binary", cat="Financial Wellbeing",          cls="ob-wellbeing-reg",  icon="fa-scale-balanced")
  )

  # ── Readable term name mapping for regression output ───────────────────────
  reg_term_labels <- c(
    "CU"                          = "Institution Type: Credit Union",
    "Score_Origin"                = "FICO Credit Score",
    "Age"                         = "Borrower Age",
    "ltv"                         = "Loan-to-Value Ratio (LTV %)",
    "dti"                         = "Debt-to-Income Ratio (DTI %)",
    "Interest_Rate"               = "Mortgage Interest Rate (%)",
    "loan_amount_cat"             = "Loan Amount (Category)",
    "open_year"                   = "Survey Year",
    "COVID_dummy"                 = "COVID Period (Mar 2020 – Jun 2021)",
    "Post_COVID"                  = "Post-COVID Period (Jul 2021+)",
    "Sex_LabelFemale"             = "Gender: Female",
    "Sex_LabelMale"               = "Gender: Male",
    "RaceBlack"                   = "Race: Black",
    "RaceAsian"                   = "Race: Asian",
    "RaceOther"                   = "Race: Other",
    "Loan_TypeFHA/VA/FSA"         = "Loan Type: FHA / VA / FSA",
    "Cashout_LabelRefinance"      = "Loan Purpose: Refinance",
    "First_Mortgage_LabelNot First" = "Not First Mortgage",
    "Term_LabelOther"             = "Loan Term: Other (not 30yr)"
  )

  readable_term <- function(raw) {
    lbl <- reg_term_labels[raw]
    if (!is.na(lbl) && nchar(lbl) > 0) return(as.character(lbl))
    # Generic cleanup
    cleaned <- gsub("([a-z])([A-Z])", "\\1 \\2", raw)
    cleaned <- gsub("_", " ", cleaned)
    tools::toTitleCase(tolower(cleaned))
  }

  # ── Outcome banner (reactive, updates on dropdown change) ─────────────────
  output$reg_outcome_banner <- renderUI({
    sel  <- input$reg_outcome
    if (is.null(sel) || !sel %in% names(reg_outcome_defs)) return(NULL)
    d    <- reg_outcome_defs[[sel]]
    type_badge <- if (d$type == "continuous")
      "<span style='background:#F8FAFC;color:#334155;font-size:9px;font-weight:700;
               padding:2px 6px;border-radius:10px;border:1px solid #E2E8F0;
               text-transform:uppercase;letter-spacing:.4px;margin-left:6px;'>CONTINUOUS — OLS</span>"
    else
      "<span style='background:#F5F3FF;color:#6D28D9;font-size:9px;font-weight:700;
               padding:2px 6px;border-radius:10px;border:1px solid #DDD6FE;
               text-transform:uppercase;letter-spacing:.4px;margin-left:6px;'>BINARY — LOGISTIC</span>"

    HTML(sprintf(
      "<div class='outcome-banner %s' style='margin-bottom:12px;'>
         <div class='ob-icon'><i class='fas %s'></i></div>
         <div class='ob-text'>
           <div class='ob-category'>%s</div>
           <div class='ob-name'>%s%s</div>
         </div>
       </div>",
      d$cls, d$icon, d$cat, d$name, type_badge))
  })

  # ── Reactive: build predictor list from all checkbox groups ───────────────
  reg_predictors_combined <- reactive({
    c(input$reg_pred_borrower,
      input$reg_pred_loan,
      input$reg_pred_financial,
      input$reg_pred_time)
  })

  # ── Main regression observeEvent ──────────────────────────────────────────
  reg_results <- reactiveValues(model=NULL, trimmed=NULL, data=NULL, outcome_def=NULL, type=NULL)

  observeEvent(input$run_regression, {
    tryCatch({
      sel  <- input$reg_outcome
      defn <- reg_outcome_defs[[sel]]
      if (is.null(defn)) stop("Unknown outcome variable selected.")

      d <- df
      # Apply sample filters
      if (length(input$reg_period_filter) > 0)
        d <- d %>% filter(COVID_period %in% input$reg_period_filter)
      if (length(input$reg_fico_filter) > 0)
        d <- d %>% filter(FICO_category %in% input$reg_fico_filter)
      if (length(input$reg_race_filter) > 0)
        d <- d %>% filter(Race %in% input$reg_race_filter)
      if (length(input$reg_purpose_filter) > 0)
        d <- d %>% filter(Cashout_Label %in% input$reg_purpose_filter)

      # Build outcome column
      is_bin <- defn$type == "binary"
      if (is_bin) {
        vb <- paste0(defn$var, "_binary")
        if (!vb %in% names(d)) stop(paste("Binary column not found:", vb))
        d <- d %>% filter(!is.na(get(vb))) %>%
          mutate(outcome = as.integer(get(vb) == "Positive (Very/Somewhat)"))
      } else {
        if (!defn$var %in% names(d)) stop(paste("Variable not found:", defn$var))
        d <- d %>% filter(!is.na(get(defn$var))) %>%
          mutate(outcome = as.numeric(get(defn$var)))
      }

      # Always include CU dummy
      d <- d %>% mutate(CU = as.integer(Institution_Type == "Credit Union"))

      # Build predictor columns
      preds       <- reg_predictors_combined()
      use_covid   <- "covid_dummy" %in% preds
      use_year    <- "open_year"   %in% preds
      other_preds <- preds[!preds %in% c("covid_dummy", "open_year")]
      other_preds <- other_preds[other_preds %in% names(d)]

      if (use_covid)
        d <- d %>% mutate(
          COVID_dummy = as.integer(COVID_period == "COVID (Mar 2020 - Jun 2021)"),
          Post_COVID  = as.integer(COVID_period == "Post-COVID (Jul 2021+)"))

      rhs <- c("CU",
               other_preds,
               if (use_covid) c("COVID_dummy", "Post_COVID"),
               if (use_year)  "open_year")

      d_model <- d %>%
        select(outcome, all_of(rhs)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        na.omit()

      if (nrow(d_model) < 50) stop("Fewer than 50 observations after filtering — broaden filters.")

      # Drop single-level factors
      for (col in names(d_model))
        if (is.factor(d_model[[col]]) && length(levels(d_model[[col]])) < 2)
          d_model[[col]] <- NULL

      frm <- as.formula(paste("outcome ~",
                               paste(setdiff(names(d_model), "outcome"), collapse = " + ")))

      fit <- if (is_bin) glm(frm, data = d_model, family = binomial())
             else        lm(frm,  data = d_model)

      reg_results$model       <- fit
      reg_results$data        <- d_model
      reg_results$outcome_def <- defn
      reg_results$type        <- if (is_bin) "logistic" else "ols"

      # ── Trimmed model: keep only significant predictors (p < 0.05) ─────────
      # CU is always kept regardless of significance (it's the variable of interest)
      reg_results$trimmed <- tryCatch({
        s_full   <- summary(fit)
        coef_tbl <- as.data.frame(coef(s_full))
        cn       <- names(coef_tbl)
        names(coef_tbl)[grepl("Pr\\(|p.val", cn, ignore.case=TRUE)] <- "Pval"
        coef_tbl$Term <- rownames(coef_tbl)

        sig_terms <- coef_tbl %>%
          filter(Term != "(Intercept)") %>%
          mutate(pv = suppressWarnings(as.numeric(Pval))) %>%
          filter(Term == "CU" | (!is.na(pv) & pv < 0.05)) %>%
          pull(Term)

        if (length(sig_terms) < 1) return(NULL)

        # Map dummy-coded terms back to original column names in d_model
        # e.g. "RaceBlack" -> "Race", "Sex_LabelMale" -> "Sex_Label"
        orig_cols <- names(d_model)[names(d_model) != "outcome"]
        keep_cols <- unique(c("CU", unlist(lapply(sig_terms, function(t) {
          matched <- orig_cols[sapply(orig_cols, function(c) startsWith(t, c))]
          if (length(matched) > 0) matched else character(0)
        }))))
        keep_cols <- intersect(keep_cols, orig_cols)

        if (length(keep_cols) < 1) return(NULL)

        d_trim <- d_model %>% select(outcome, all_of(keep_cols))
        frm_t  <- as.formula(paste("outcome ~",
                                    paste(setdiff(names(d_trim), "outcome"), collapse=" + ")))
        if (is_bin) glm(frm_t, data=d_trim, family=binomial())
        else        lm(frm_t,  data=d_trim)
      }, error=function(e) NULL)

    }, error = function(e) {
      reg_results$model <- NULL
      showNotification(paste("Regression error:", e$message), type = "error", duration = 8)
    })
  })

  observeEvent(input$reset_regression, {
    reg_results$model       <- NULL
    reg_results$trimmed     <- NULL
    reg_results$data        <- NULL
    reg_results$outcome_def <- NULL
    reg_results$type        <- NULL
  })

  # ── Helper: parse coef table robustly ─────────────────────────────────────
  # ── Parse raw coef summary into tidy data frame ───────────────────────────
  get_coef_df <- function(m) {
    s     <- summary(m)
    coefs <- as.data.frame(coef(s))
    coefs$RawTerm <- rownames(coefs)
    rownames(coefs) <- NULL
    cn <- names(coefs)
    names(coefs)[grepl("Std",             cn, ignore.case=TRUE)] <- "SE"
    names(coefs)[grepl("Pr\\(|p.val",   cn, ignore.case=TRUE)] <- "Pval"
    names(coefs)[grepl("t val|z val",     cn, ignore.case=TRUE)] <- "Stat"
    coefs$Label <- sapply(coefs$RawTerm, readable_term)
    coefs
  }

  # ── Compute Average Marginal Effects for a logistic model ─────────────────
  # AME_k = mean over observations of [ beta_k * p_i * (1 - p_i) ]
  # where p_i = fitted probability for observation i.
  # This is the "delta method" AME — identical to what Stata's margins and
  # R's marginaleffects package compute by default.
  # Returns a data frame with the same columns as get_coef_df() but with
  # AME in Estimate and AME SE (via delta method), keeping original p-values.
  compute_ame <- function(m) {
    d      <- model.frame(m)
    X      <- model.matrix(m)                      # n x k design matrix
    beta   <- coef(m)                               # k log-odds coefficients
    p_hat  <- fitted(m)                             # n predicted probabilities
    w      <- p_hat * (1 - p_hat)                   # n weights = dF/d(X*beta)

    # AME for each coefficient: mean(w_i * beta_k)  [for continuous]
    # For dummy variables this is an approximation — exact AME would
    # need counterfactual prediction, but this is accurate enough and
    # widely used in practice (Stata default).
    ame_vals <- colMeans(w * X) * 0   # initialise
    for (j in seq_along(beta)) {
      ame_vals[j] <- mean(w * beta[j])
    }

    # Delta-method SE: SE_AME_k ≈ sqrt( mean(w)^2 * SE_beta_k^2 )
    mean_w  <- mean(w)
    raw_se  <- sqrt(diag(vcov(m)))
    ame_se  <- abs(mean_w) * raw_se

    # Reuse original p-values (Wald test still valid for AME sign/significance)
    s     <- summary(m)
    coefs <- as.data.frame(coef(s))
    coefs$RawTerm <- rownames(coefs)
    rownames(coefs) <- NULL
    cn <- names(coefs)
    names(coefs)[grepl("Pr\\(|p.val", cn, ignore.case=TRUE)] <- "Pval"

    out <- data.frame(
      RawTerm  = names(beta),
      Estimate = ame_vals,       # AME (probability scale)
      SE       = ame_se,
      Pval     = coefs$Pval[match(names(beta), coefs$RawTerm)],
      stringsAsFactors = FALSE
    )
    out$Label <- sapply(out$RawTerm, readable_term)
    out
  }

  # ── Unified helper: returns AME for logistic, raw coefs for OLS ───────────
  get_display_coefs <- function(m, type) {
    if (type == "logistic") compute_ame(m) else get_coef_df(m)
  }

  # ── Model Summary panel ────────────────────────────────────────────────────
  output$reg_summary_ui <- renderUI({
    m <- reg_results$model
    if (is.null(m)) return(HTML(
      "<div style='text-align:center;color:#94A3B8;padding:50px 20px;'>
         <i class='fas fa-chart-line fa-2x' style='opacity:.25;margin-bottom:12px;'></i><br>
         <strong>Configure your model and click Run Regression</strong><br>
         <span style='font-size:12px;'>Institution Type (CU) is always included.
         Add controls from the left panel to isolate its effect.</span>
       </div>"))

    defn    <- reg_results$outcome_def
    m_trim  <- reg_results$trimmed
    is_log  <- reg_results$type == "logistic"
    d       <- reg_results$data

    # ── Helper: build fit-stat cards for any model ────────────────────────────
    make_fit_cards <- function(mod, d_mod, style = "full") {
      bg  <- if (style == "trim") "#F0FDF4" else "#F8FAFC"
      bg2 <- if (style == "trim") "#F5F3FF" else "#F8FAFC"
      if (!is_log) {
        sv <- summary(mod)
        sprintf(
          "<div style='display:grid;grid-template-columns:repeat(2,1fr);gap:6px;margin-bottom:12px;'>
             <div style='background:%s;border-radius:7px;padding:8px;text-align:center;'>
               <div style='font-size:16px;font-weight:700;color:#0F172A;'>%.4f</div>
               <div style='font-size:10px;color:#64748B;'>Adj. R²</div></div>
             <div style='background:%s;border-radius:7px;padding:8px;text-align:center;'>
               <div style='font-size:16px;font-weight:700;color:#0F172A;'>%s</div>
               <div style='font-size:10px;color:#64748B;'>n</div></div>
           </div>",
          bg, sv$adj.r.squared, bg,
          format(nrow(d_mod), big.mark=","))
      } else {
        mcf <- 1 - mod$deviance / mod$null.deviance
        sprintf(
          "<div style='display:grid;grid-template-columns:repeat(2,1fr);gap:6px;margin-bottom:12px;'>
             <div style='background:%s;border-radius:7px;padding:8px;text-align:center;'>
               <div style='font-size:16px;font-weight:700;color:#7C3AED;'>%.4f</div>
               <div style='font-size:10px;color:#64748B;'>McFadden R²</div></div>
             <div style='background:%s;border-radius:7px;padding:8px;text-align:center;'>
               <div style='font-size:16px;font-weight:700;color:#0F172A;'>%.1f</div>
               <div style='font-size:10px;color:#64748B;'>AIC</div></div>
           </div>",
          bg2, mcf, bg, AIC(mod))
      }
    }

    # ── Helper: render a coef table for any model ────────────────────────────
    make_coef_table <- function(mod, mod_type, highlight_cu = TRUE) {
      cdf  <- get_display_coefs(mod, mod_type)
      is_l <- mod_type == "logistic"

      rows <- apply(cdf, 1, function(r) {
        pv    <- suppressWarnings(as.numeric(r[["Pval"]]))
        est   <- suppressWarnings(as.numeric(r[["Estimate"]]))
        se    <- suppressWarnings(as.numeric(r[["SE"]]))
        lo    <- if (!is.na(est) && !is.na(se)) est - 1.96*se else NA
        hi    <- if (!is.na(est) && !is.na(se)) est + 1.96*se else NA
        star  <- if (!is.na(pv)) sig_label(pv) else ""
        is_cu <- r[["RawTerm"]] == "CU"
        row_bg   <- if (is_cu && highlight_cu) "#FFFBEB"
                    else if (!is.na(pv) && pv < 0.05 && !is.na(est) && est > 0) "#F0FDF4"
                    else if (!is.na(pv) && pv < 0.05 && !is.na(est) && est < 0) "#FEF2F2"
                    else "#FFFFFF"
        cu_style <- if (is_cu && highlight_cu) "font-weight:700;border-left:3px solid #D97706;" else ""
        sig_col  <- if (!is.na(pv) && pv < 0.001) "#16A34A"
                    else if (!is.na(pv) && pv < 0.05) "#D97706"
                    else "#94A3B8"
        val_str  <- if (is_l && !is.na(est)) sprintf("%+.1f pp", est*100) else if (!is.na(est)) sprintf("%+.4f", est) else "N/A"
        ci_str   <- if (is_l && !is.na(lo) && !is.na(hi)) sprintf("[%+.1f, %+.1f]", lo*100, hi*100)
                    else if (!is.na(lo) && !is.na(hi)) sprintf("[%+.4f, %+.4f]", lo, hi)
                    else "N/A"
        sprintf(
          "<tr style='background:%s;'>
             <td style='padding:4px 8px;font-size:11px;%s'>%s</td>
             <td style='text-align:right;padding:4px 6px;font-size:11px;font-weight:600;color:%s;'>%s</td>
             <td style='text-align:right;padding:4px 6px;font-size:11px;color:#64748B;'>%s</td>
             <td style='text-align:center;padding:4px 6px;font-size:11px;color:%s;font-weight:700;'>%s</td>
           </tr>",
          row_bg, cu_style, r[["Label"]],
          ifelse(!is.na(est) && est > 0, "#15803D", ifelse(!is.na(est) && est < 0, "#B91C1C", "#0F172A")),
          val_str, ci_str, sig_col, ifelse(nchar(star) > 0, star, "n.s."))
      })

      est_hdr <- if (is_l) "<span style='color:#7C3AED;font-weight:700;'>AME</span>"
                 else      "Coef."

      paste0(
        "<div style='overflow-x:auto;'>",
        "<table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>",
        "<thead><tr style='background:#F8FAFC;border-bottom:2px solid #E2E8F0;'>",
        "<th style='text-align:left;padding:5px 8px;font-size:10px;color:#475569;'>Predictor</th>",
        "<th style='text-align:right;padding:5px 6px;font-size:10px;'>", est_hdr, "</th>",
        "<th style='text-align:right;padding:5px 6px;font-size:10px;color:#475569;'>95% CI</th>",
        "<th style='text-align:center;padding:5px 6px;font-size:10px;color:#475569;'>Sig.</th>",
        "</tr></thead><tbody>",
        paste(rows, collapse=""),
        "</tbody></table></div>")
    }

    # ── Comparison delta (trimmed vs full R²/AIC) ─────────────────────────────
    comparison_badge <- if (!is.null(m_trim)) {
      if (!is_log) {
        r2_f <- summary(m)$adj.r.squared
        r2_t <- summary(m_trim)$adj.r.squared
        d_r2 <- r2_t - r2_f
        sprintf(
          "<div style='font-size:10px;background:#F0FDF4;border-radius:6px;
                       padding:5px 10px;margin-bottom:8px;color:#15803D;font-weight:600;'>
             Trimmed model Adj. R² change: %+.4f — %s
           </div>",
          d_r2,
          if (abs(d_r2) < 0.005) "Negligible loss of fit. Trimmed model preferred for clarity."
          else if (d_r2 > -0.02) "Minor loss. Trimmed model is more parsimonious."
          else "Meaningful fit loss — review which variables were dropped.")
      } else {
        aic_f <- AIC(m); aic_t <- AIC(m_trim)
        d_aic <- aic_t - aic_f
        mcf_f <- round(1 - m$deviance/m$null.deviance, 4)
        mcf_t <- round(1 - m_trim$deviance/m_trim$null.deviance, 4)
        sprintf(
          "<div style='font-size:10px;background:#F0FDF4;border-radius:6px;
                       padding:5px 10px;margin-bottom:8px;color:#15803D;font-weight:600;'>
             AIC change: %+.1f &nbsp;|&nbsp; McFadden R² %s &rarr; %s &nbsp;|&nbsp; %s
           </div>",
          d_aic, mcf_f, mcf_t,
          if (d_aic < 2) "Trimmed model fits equally well or better."
          else if (d_aic < 10) "Small fit penalty — trimmed model is more interpretable."
          else "Larger fit penalty — consider keeping some dropped variables.")
      }
    } else ""

    # ── Assemble side-by-side layout ──────────────────────────────────────────
    model_label <- toupper(if (is_log) "Logistic Regression" else "OLS Regression")

    # Header
    header_html <- sprintf(
      "<div style='font-size:11px;font-weight:600;color:#475569;margin-bottom:8px;'>
         <i class='fas fa-%s' style='margin-right:6px;color:#D97706;'></i>%s — Outcome: %s
       </div>",
      ifelse(is_log, "percent", "chart-line"), model_label, defn$name)

    # AME legend (logistic only)
    ame_note <- if (is_log)
      "<div style='font-size:10px;background:#F5F3FF;border-radius:6px;
                   padding:5px 10px;margin-bottom:10px;color:#6D28D9;'>
         <strong>AME (pp)</strong> = percentage-point change in probability for a 1-unit predictor increase,
         averaged across all borrowers. Easier to interpret than log-odds.
       </div>"
    else ""

    if (is.null(m_trim)) {
      # Only full model — single-column with regular fit cards
      if (!is_log) {
        sv    <- summary(m)
        fstat <- sv$fstatistic[1]
        full_fit <- sprintf(
          "<div style='display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-bottom:14px;'>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%.4f</div>
               <div style='font-size:10px;color:#64748B;'>R²</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%.4f</div>
               <div style='font-size:10px;color:#64748B;'>Adj. R²</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%.2f</div>
               <div style='font-size:10px;color:#64748B;'>F-Stat</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%s</div>
               <div style='font-size:10px;color:#64748B;'>n</div></div>
           </div>",
          sv$r.squared, sv$adj.r.squared, fstat, format(nrow(d), big.mark=","))
      } else {
        mcf <- 1 - m$deviance/m$null.deviance
        full_fit <- sprintf(
          "<div style='display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-bottom:14px;'>
             <div style='background:#F5F3FF;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#7C3AED;'>%.4f</div>
               <div style='font-size:10px;color:#64748B;'>McFadden R²</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%.1f</div>
               <div style='font-size:10px;color:#64748B;'>AIC</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%.1f</div>
               <div style='font-size:10px;color:#64748B;'>Null Deviance</div></div>
             <div style='background:#F8FAFC;border-radius:8px;padding:10px;text-align:center;'>
               <div style='font-size:18px;font-weight:700;color:#0F172A;'>%s</div>
               <div style='font-size:10px;color:#64748B;'>n</div></div>
           </div>",
          mcf, AIC(m), m$null.deviance, format(nrow(d), big.mark=","))
      }
      HTML(paste0(header_html, ame_note, full_fit,
                  make_coef_table(m, reg_results$type)))
    } else {
      # Both models — two-column side-by-side
      n_full <- nrow(get_display_coefs(m, reg_results$type)) - 1  # excl intercept
      n_trim <- nrow(get_display_coefs(m_trim, reg_results$type)) - 1

      HTML(paste0(
        header_html, ame_note, comparison_badge,

        # Column headers
        "<div style='display:grid;grid-template-columns:1fr 1fr;gap:16px;'>",

          # ── LEFT: Full model ─────────────────────────────────────────────
          "<div>",
            "<div style='font-size:11px;font-weight:700;color:#475569;
                         background:#F8FAFC;border-radius:6px;padding:6px 10px;
                         margin-bottom:8px;display:flex;justify-content:space-between;
                         align-items:center;'>",
              "<span><i class='fas fa-list' style='margin-right:5px;color:#94A3B8;'></i>Full Model</span>",
              sprintf("<span style='font-size:10px;color:#94A3B8;'>%d predictors</span>", n_full),
            "</div>",
            make_fit_cards(m, d, "full"),
            make_coef_table(m, reg_results$type),
          "</div>",

          # ── RIGHT: Trimmed model ──────────────────────────────────────────
          "<div>",
            "<div style='font-size:11px;font-weight:700;color:#15803D;
                         background:#F0FDF4;border-radius:6px;padding:6px 10px;
                         margin-bottom:8px;display:flex;justify-content:space-between;
                         align-items:center;border-left:3px solid #16A34A;'>",
              "<span><i class='fas fa-scissors' style='margin-right:5px;'></i>Trimmed Model</span>",
              sprintf("<span style='font-size:10px;'>%d sig. predictors only</span>", n_trim),
            "</div>",
            make_fit_cards(m_trim, d, "trim"),
            make_coef_table(m_trim, reg_results$type, highlight_cu=TRUE),
            "<div style='font-size:10px;color:#64748B;margin-top:6px;padding:5px 8px;
                         background:#F8FAFC;border-radius:6px;'>",
              "<strong>Trimmed model</strong> keeps only predictors with p&lt;0.05 in the full model,
               plus Institution Type (CU) regardless of significance. Non-significant terms are omitted.",
            "</div>",
          "</div>",

        "</div>",  # close grid

        # Footer note
        "<div style='font-size:10px;color:#94A3B8;margin-top:10px;'>",
          "&#9733; p&lt;0.05 &nbsp; &#9733;&#9733; p&lt;0.01 &nbsp; &#9733;&#9733;&#9733; p&lt;0.001",
        "</div>"
      ))
    }
  })

  # ── Coefficient plot ────────────────────────────────────────────────────────
  output$reg_coef_plot <- renderPlotly({
    m <- reg_results$model
    if (is.null(m)) return(NULL)

    is_log <- reg_results$type == "logistic"
    coefs <- get_display_coefs(m, reg_results$type)
    coefs <- coefs %>%
      filter(RawTerm != "(Intercept)") %>%
      mutate(
        SE  = suppressWarnings(as.numeric(SE)),
        Est = suppressWarnings(as.numeric(Estimate)),
        lo  = Est - 1.96 * SE,
        hi  = Est + 1.96 * SE,
        Pval_num = suppressWarnings(as.numeric(Pval)),
        sig  = !is.na(Pval_num) & Pval_num < 0.05,
        is_cu= RawTerm == "CU",
        col  = case_when(
          is_cu  ~ "#D97706",
          sig & Est > 0 ~ v2_colors$positive,
          sig & Est < 0 ~ v2_colors$negative,
          TRUE ~ "#CBD5E1"),
        Label = factor(Label, levels = Label[order(Est)])
      )

    plot_ly(coefs,
      x    = ~Est, y = ~Label,
      type = "bar", orientation = "h",
      marker = list(
        color = ~col,
        line  = list(color = "white", width = 0.5)
      ),
      error_x = list(
        type       = "data",
        symmetric  = FALSE,
        arrayminus = ~(Est - lo),
        array      = ~(hi - Est),
        color      = "#64748B",
        thickness  = 1.5,
        width      = 4
      ),
      text = ~paste0(
        "<b>", Label, "</b><br>",
        ifelse(is_log,
          paste0("AME: ", sprintf("%+.1f", Est * 100), " pp",
                 "<br>95% CI: [", sprintf("%+.1f", lo * 100), ", ",
                 sprintf("%+.1f", hi * 100), "] pp"),
          paste0("Estimate: ", sprintf("%+.4f", Est),
                 "<br>95% CI: [", sprintf("%+.4f", lo), ", ",
                 sprintf("%+.4f", hi), "]")),
        "<br>p-value: ", ifelse(!is.na(Pval_num), formatC(Pval_num, format="e", digits=2), "N/A")
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title    = if (is_log)
                       "Avg. Marginal Effect on Probability (±1.96 SE)"
                     else "Coefficient Estimate (±1.96 SE)",
          zeroline = TRUE,  zerolinecolor = "#94A3B8", zerolinewidth = 1.5,
          showgrid = TRUE,  gridcolor     = "#F1F5F9"
        ),
        yaxis = list(title = "", tickfont = list(size = 10), showgrid = FALSE),
        font  = list(family = "Inter, sans-serif", size = 11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin        = list(l = 10, r = 30, t = 10, b = 50),
        shapes = list(list(
          type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(coefs) - 0.5,
          line = list(color = "#475569", width = 1.5, dash = "dash")
        )),
        annotations = list(list(
          text = if (is_log)
                   "&#9650; Highlighted bar = CU effect (in probability points, averaged across all borrowers)"
                 else
                   "&#9650; Highlighted bar = Institution Type (CU) coefficient",
          x = 0, y = -0.12, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 10, color = "#94A3B8"), xanchor = "left"
        ))
      )
  })

  # ── Key Findings interpretation ─────────────────────────────────────────────
  output$reg_interpretation <- renderUI({
    m <- reg_results$model
    if (is.null(m)) return(HTML(
      "<p style='color:#94A3B8;font-size:13px;'>Run regression to see interpretation.</p>"))

    defn   <- reg_results$outcome_def
    is_log <- reg_results$type == "logistic"
    coefs  <- get_display_coefs(m, reg_results$type)

    # ── CU effect card ────────────────────────────────────────────────────────
    cu_row <- coefs %>% filter(RawTerm == "CU")
    cu_html <- if (nrow(cu_row) > 0) {
      est <- suppressWarnings(as.numeric(cu_row$Estimate[1]))
      pv  <- suppressWarnings(as.numeric(cu_row$Pval[1]))
      sig <- !is.na(pv) && pv < 0.05
      col <- if (sig && !is.na(est) && est > 0) v2_colors$positive
             else if (sig && !is.na(est) && est < 0) v2_colors$negative
             else "#94A3B8"
      bg     <- if (sig) ifelse(!is.na(est) && est > 0, "#F0FDF4", "#FEF2F2") else "#F8FAFC"
      border <- if (sig) ifelse(!is.na(est) && est > 0, "#16A34A",  "#DC2626") else "#CBD5E1"

      if (is_log) {
        # AME: already on probability scale
        pp   <- est * 100          # percentage points
        dir  <- if (!is.na(pp) && pp > 0) "more likely" else "less likely"
        main <- sprintf("%+.1f pp", pp)
        plain <- sprintf(
          "CU borrowers are <strong>%.1f percentage points %s</strong> to report a positive outcome,
          compared to Non-CU borrowers with similar characteristics.",
          abs(pp), dir)
        sig_txt <- if (sig) paste0("Statistically significant (", sig_label(pv), ")")
                   else     sprintf("Not statistically significant (p = %.3f)", pv)
      } else {
        dir  <- if (!is.na(est) && est > 0) "higher" else "lower"
        main <- sprintf("%+.4f", est)
        plain <- sprintf(
          "CU borrowers score <strong>%.4f units %s</strong> on this outcome
          compared to Non-CU borrowers with similar characteristics.",
          abs(est), dir)
        sig_txt <- if (sig) paste0("Statistically significant (", sig_label(pv), ")")
                   else     sprintf("Not statistically significant (p = %.3f)", pv)
      }

      sprintf(
        "<div style='background:%s;border-left:4px solid %s;border-radius:8px;
                     padding:12px 14px;margin-bottom:12px;'>
           <div style='font-size:10px;font-weight:700;color:%s;text-transform:uppercase;
                       letter-spacing:.6px;margin-bottom:4px;'>
             Credit Union Effect on %s</div>
           <div style='font-size:26px;font-weight:800;color:#0F172A;line-height:1;
                       margin-bottom:6px;'>%s</div>
           <div style='font-size:12px;color:#334155;line-height:1.5;margin-bottom:4px;'>
             %s</div>
           <div style='font-size:11px;color:%s;font-weight:600;'>%s</div>
         </div>",
        bg, border, col,
        defn$name, main, plain, col, sig_txt)
    } else ""

    # ── Top significant predictors in plain English ────────────────────────────
    sig_df <- coefs %>%
      filter(RawTerm != "(Intercept)", RawTerm != "CU") %>%
      mutate(pv  = suppressWarnings(as.numeric(Pval)),
             est = suppressWarnings(as.numeric(Estimate))) %>%
      filter(!is.na(pv), pv < 0.05) %>%
      arrange(pv) %>% head(5)

    sig_html <- if (nrow(sig_df) > 0) {
      items <- apply(sig_df, 1, function(r) {
        est_v <- suppressWarnings(as.numeric(r[["est"]]))
        pv_v  <- suppressWarnings(as.numeric(r[["pv"]]))
        col   <- if (!is.na(est_v) && est_v > 0) "#15803D" else "#B91C1C"
        arrow <- if (!is.na(est_v) && est_v > 0) "&#8593;" else "&#8595;"
        bg_i  <- if (!is.na(est_v) && est_v > 0) "#F0FDF4" else "#FEF2F2"

        val_str <- if (is_log)
          sprintf("%+.1f pp", est_v * 100)
        else
          sprintf("%+.4f", est_v)

        sprintf(
          "<div style='display:flex;align-items:flex-start;gap:10px;padding:7px 10px;
                       background:%s;border-radius:7px;margin-bottom:5px;'>
             <span style='color:%s;font-size:18px;line-height:1.2;flex-shrink:0;'>%s</span>
             <div style='flex:1;min-width:0;'>
               <div style='font-size:12px;font-weight:600;color:#0F172A;'>%s</div>
               <div style='font-size:11px;color:#475569;margin-top:1px;'>
                 Effect: <strong style='color:%s;'>%s</strong> &nbsp;
                 <span style='color:#94A3B8;'>%s</span></div>
             </div>
           </div>",
          bg_i, col, arrow,
          r[["Label"]], col, val_str,
          sig_label(pv_v))
      })
      paste0(
        "<div style='font-size:11px;font-weight:700;color:#475569;margin-bottom:7px;",
               "text-transform:uppercase;letter-spacing:.5px;'>",
          if(is_log) "Top drivers of outcome probability" else "Top significant predictors",
          "</div>",
        paste(items, collapse=""))
    } else {
      "<div style='color:#94A3B8;font-size:12px;padding:8px;'>
         No other significant predictors at p&lt;0.05.</div>"
    }

    # ── Model quality note ─────────────────────────────────────────────────────
    model_note <- if (!is_log) {
      r2   <- summary(m)$r.squared
      qual <- if (r2 >= 0.3) c("#16A34A","good") else if (r2>=0.1) c("#D97706","moderate") else c("#94A3B8","weak")
      sprintf("<div style='font-size:10px;color:%s;background:#F8FAFC;border-radius:6px;
               padding:5px 8px;margin-top:8px;'>R² = %.3f (%s fit)</div>",
              qual[1], r2, qual[2])
    } else {
      mcf  <- 1 - m$deviance / m$null.deviance
      qual <- if (mcf >= 0.2) c("#16A34A","good") else if (mcf>=0.05) c("#D97706","moderate") else c("#94A3B8","weak")
      sprintf("<div style='font-size:10px;color:%s;background:#F5F3FF;border-radius:6px;
               padding:5px 8px;margin-top:8px;'>
               McFadden R² = %.3f (%s fit) &nbsp;|&nbsp; Effects shown as probability points (AME)</div>",
              qual[1], mcf, qual[2])
    }

    HTML(paste0(
      "<div style='font-size:12px;line-height:1.7;'>",
      cu_html,
      "<hr class='v2'>",
      sig_html,
      model_note,
      "</div>"))
  })

  # ── Residual diagnostics (OLS only) ────────────────────────────────────────
  # Dynamic panel title + subtitle (changes OLS ↔ Logistic)
  output$reg_diag_title <- renderUI({
    type <- reg_results$type
    if (is.null(type)) return(tags$span(icon("magnifying-glass-chart"), " Diagnostics"))
    if (type == "logistic")
      tags$span(icon("chart-area"), " ROC Curve — Model Discrimination")
    else
      tags$span(icon("magnifying-glass-chart"), " Residual Diagnostics")
  })

  output$reg_diag_subtitle <- renderUI({
    type <- reg_results$type
    if (is.null(type)) return(NULL)
    style <- "font-size:11px;color:#94A3B8;margin-bottom:4px;"
    if (type == "logistic")
      tags$div(style=style,
        "Prediction accuracy: how well the model separates positive from negative outcomes.",
        " Closer to 1.0 = better. Colour shows prediction quality: ",
        tags$span(style="color:#16A34A;font-weight:600;", "green ≥0.75"),
        " / ",
        tags$span(style="color:#D97706;font-weight:600;", "amber ≥0.60"),
        " / ",
        tags$span(style="color:#DC2626;font-weight:600;", "red <0.60"))
    else
      tags$div(style=style,
        "Random scatter around zero = good fit. Patterns suggest model misspecification.")
  })

  output$reg_residual_plot <- renderPlotly({
    m    <- reg_results$model
    type <- reg_results$type
    if (is.null(m)) return(NULL)

    if (type == "logistic") {
      # ── ROC curve for logistic models ──────────────────────────────────────
      d      <- reg_results$data
      probs  <- fitted(m)
      labels <- d$outcome

      # Compute ROC points via trapezoid
      thresholds <- c(1, sort(unique(probs), decreasing = TRUE), 0)
      tpr <- sapply(thresholds, function(t) mean(probs[labels == 1] >= t))
      fpr <- sapply(thresholds, function(t) mean(probs[labels == 0] >= t))

      # AUC
      auc_val <- abs(sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1))) / 2)

      # Colour the curve by TPR zone
      roc_df <- data.frame(fpr = fpr, tpr = tpr)

      plot_ly() %>%
        # Diagonal chance line
        add_trace(x = c(0,1), y = c(0,1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#CBD5E1", width = 1.5, dash = "dot"),
                  name = "Random classifier", showlegend = TRUE,
                  hoverinfo = "skip") %>%
        # ROC curve
        add_trace(data = roc_df,
                  x = ~fpr, y = ~tpr,
                  type = "scatter", mode = "lines",
                  line = list(
                    color = ifelse(auc_val >= 0.75, v2_colors$positive,
                                   ifelse(auc_val >= 0.60, "#D97706", v2_colors$noncu)),
                    width = 2.5),
                  fill = "tozeroy",
                  fillcolor = ifelse(auc_val >= 0.75, "rgba(22,163,74,0.08)",
                                     ifelse(auc_val >= 0.60, "rgba(217,119,6,0.08)",
                                            "rgba(220,38,38,0.08)")),
                  name = sprintf("ROC (AUC = %.3f)", auc_val),
                  hovertemplate = "FPR: %{x:.3f}<br>TPR: %{y:.3f}<extra></extra>") %>%
        layout(
          xaxis = list(title = "False Positive Rate (1 - Specificity)",
                       range = c(0, 1), showgrid = TRUE, gridcolor = "#F1F5F9",
                       zeroline = FALSE, tickformat = ".0%"),
          yaxis = list(title = "True Positive Rate (Sensitivity)",
                       range = c(0, 1), showgrid = TRUE, gridcolor = "#F1F5F9",
                       zeroline = FALSE, tickformat = ".0%"),
          font          = list(family = "Inter, sans-serif", size = 11),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          legend = list(orientation = "h", x = 0.35, y = 0.08,
                        font = list(size = 10)),
          margin = list(t = 10, b = 50, l = 55, r = 10),
          annotations = list(list(
            text = sprintf(
              "<b>AUC = %.3f</b> &nbsp; %s",
              auc_val,
              ifelse(auc_val >= 0.75, "Good discrimination",
                     ifelse(auc_val >= 0.60, "Moderate discrimination",
                            "Weak discrimination"))),
            x = 0.97, y = 0.06, xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 12, color =
                          ifelse(auc_val >= 0.75, v2_colors$positive,
                                 ifelse(auc_val >= 0.60, "#D97706", v2_colors$noncu))),
            xanchor = "right", bgcolor = "rgba(255,255,255,0.85)",
            bordercolor = "#E2E8F0", borderwidth = 1, borderpad = 6
          ))
        )

    } else {
      # ── Residuals vs Fitted for OLS models ─────────────────────────────────
      d_plot <- data.frame(fitted = fitted(m), residual = residuals(m))
      plot_ly(d_plot,
        x = ~fitted, y = ~residual,
        type = "scatter", mode = "markers",
        marker = list(color = v2_colors$cu, opacity = 0.25, size = 4),
        hovertemplate = "Fitted: %{x:.3f}<br>Residual: %{y:.3f}<extra></extra>",
        name = "Residuals"
      ) %>%
        layout(
          xaxis = list(title = "Fitted Values",
                       showgrid = TRUE, gridcolor = "#F1F5F9"),
          yaxis = list(title = "Residuals",
                       showgrid = TRUE, gridcolor = "#F1F5F9",
                       zeroline = TRUE, zerolinecolor = "#DC2626", zerolinewidth = 1.5),
          font          = list(family = "Inter, sans-serif", size = 11),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          margin = list(t = 10, b = 45, l = 50, r = 10),
          shapes = list(list(
            type = "line",
            x0 = min(fitted(m)), x1 = max(fitted(m)), y0 = 0, y1 = 0,
            line = list(color = "#DC2626", width = 1.5, dash = "dash")))
        )
    }
  })

  # ── Predicted outcome by FICO tier (CU vs Non-CU) ──────────────────────────
  output$reg_predicted_by_fico <- renderPlotly({
    m    <- reg_results$model
    d    <- reg_results$data
    defn <- reg_results$outcome_def
    if (is.null(m) || is.null(d)) return(NULL)
    if (!"FICO_category" %in% names(df)) return(NULL)

    tryCatch({
      # Build prediction grid: FICO x CU, other predictors at their means/modes
      fico_levels <- fico_order[fico_order != "Unknown"]

      pred_grid <- expand.grid(
        FICO_cat = fico_levels,
        CU_val   = c(0L, 1L),
        stringsAsFactors = FALSE
      )

      # Use means of numeric predictors in training data
      num_cols <- names(d)[sapply(d, is.numeric) & names(d) != "outcome" & names(d) != "CU"]
      fac_cols <- names(d)[sapply(d, is.factor)  & names(d) != "outcome"]

      new_d <- d[rep(1, nrow(pred_grid)), , drop=FALSE]
      new_d$CU <- pred_grid$CU_val
      for (col in num_cols) new_d[[col]] <- mean(d[[col]], na.rm=TRUE)
      for (col in fac_cols) {
        lv <- levels(d[[col]])[1]
        new_d[[col]] <- factor(lv, levels=levels(d[[col]]))
      }

      preds_raw <- tryCatch(predict(m, newdata=new_d, type="response"), error=function(e) NULL)
      if (is.null(preds_raw)) return(NULL)

      pred_grid$pred <- preds_raw
      pred_grid$Institution <- ifelse(pred_grid$CU_val == 1, "Credit Union", "Non-Credit Union")
      pred_grid$FICO_cat <- factor(pred_grid$FICO_cat, levels=fico_levels)

      cu_d    <- pred_grid %>% filter(Institution == "Credit Union")
      noncu_d <- pred_grid %>% filter(Institution == "Non-Credit Union")

      plot_ly() %>%
        add_bars(data=cu_d, x=~FICO_cat, y=~pred, name="Credit Union",
                 marker=list(color=v2_colors$cu, opacity=0.85),
                 text=~round(pred,3), textposition="outside",
                 hovertemplate="CU | %{x}<br>Predicted: %{y:.3f}<extra></extra>") %>%
        add_bars(data=noncu_d, x=~FICO_cat, y=~pred, name="Non-Credit Union",
                 marker=list(color=v2_colors$noncu, opacity=0.85),
                 text=~round(pred,3), textposition="outside",
                 hovertemplate="Non-CU | %{x}<br>Predicted: %{y:.3f}<extra></extra>") %>%
        layout(
          barmode="group",
          xaxis=list(title="FICO Credit Score Tier", tickfont=list(size=9), showgrid=FALSE),
          yaxis=list(title=if(defn$type=="continuous") "Predicted Value"
                          else "Predicted Probability",
                     showgrid=TRUE, gridcolor="#F1F5F9",
                     tickformat=if(defn$type=="binary") ".0%" else NULL),
          font=list(family="Inter, sans-serif", size=11),
          paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
          legend=list(orientation="h", x=0, y=-0.25, font=list(size=10)),
          margin=list(t=5, b=60, l=45, r=10)
        )
    }, error=function(e) NULL)
  })


  # ══════════════════════════════════════════════════════════════════════════
  # TAB 5 — POLICY REPORT
  # ══════════════════════════════════════════════════════════════════════════

  rpt_cu    <- reactive({ report_data() %>% filter(Institution_Type == "Credit Union") })
  rpt_noncu <- reactive({ report_data() %>% filter(Institution_Type == "Non-Credit Union") })

  # ── Shared helper: binary outcome rate for report ──────────────────────────
  rpt_bin_rate <- function(d, var) {
    vb <- paste0(var, "_binary")
    if (!vb %in% names(d)) return(NA_real_)
    mean(d[[vb]] == "Positive (Very/Somewhat)", na.rm = TRUE) * 100
  }

  # ── Section 1: Key Findings callout ───────────────────────────────────────
  output$rpt_key_findings <- renderUI({
    cu <- rpt_cu(); noncu <- rpt_noncu()
    if (nrow(cu) < 10 || nrow(noncu) < 10)
      return(HTML("<div class='rpt-key-findings'><p style='color:#94A3B8;'>
                   Insufficient data for current filters.</p></div>"))

    d <- report_data()

    # ── Helper: prop test for binary vars (returns p, star, sig) ─────────────
    bin_sig <- function(cu_d, noncu_d, var) {
      vb <- paste0(var, "_binary")
      if (!vb %in% names(cu_d)) return(list(p=NA, star="", sig=FALSE))
      tryCatch({
        x1 <- sum(cu_d[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)
        x2 <- sum(noncu_d[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)
        n1 <- sum(!is.na(cu_d[[vb]]))
        n2 <- sum(!is.na(noncu_d[[vb]]))
        if (n1 < 5 || n2 < 5) return(list(p=NA, star="", sig=FALSE))
        p  <- prop.test(c(x1, x2), c(n1, n2), correct=FALSE)$p.value
        list(p=p, star=sig_label(p), sig=p<0.05)
      }, error=function(e) list(p=NA, star="", sig=FALSE))
    }

    # ── Helper: sig badge HTML ────────────────────────────────────────────────
    sig_badge <- function(s, type="continuous") {
      if (is.na(s$p)) {
        return("<span style='font-size:10px;color:#94A3B8;font-style:italic;'>n/a</span>")
      }
      if (s$sig) {
        col  <- "#16A34A"
        icon <- "fa-check-circle"
        txt  <- paste0("Significant ", s$star)
        tip  <- if (s$p < 0.001) "p < 0.001" else if (s$p < 0.01) sprintf("p = %.3f", s$p)
                else sprintf("p = %.3f", s$p)
      } else {
        col  <- "#94A3B8"
        icon <- "fa-minus-circle"
        txt  <- "Not significant"
        tip  <- sprintf("p = %.3f", s$p)
      }
      test_lbl <- if (type == "binary") "prop.test" else "weighted t-test"
      sprintf(
        "<span style='display:inline-flex;align-items:center;gap:4px;
                      background:%s22;border:1px solid %s44;border-radius:20px;
                      padding:1px 8px;font-size:10px;font-weight:600;color:%s;
                      margin-left:6px;white-space:nowrap;' title='%s (%s)'>
           <i class='fas %s' style='font-size:9px;'></i> %s
         </span>",
        col, col, col, tip, test_lbl, icon, txt)
    }

    # ── Helper: render one finding row ────────────────────────────────────────
    make_finding <- function(num, text_html, tone="neutral") {
      bullet_col <- switch(tone,
        positive="#16A34A", negative="#DC2626",
        warning="#D97706",  neutral="#2563EB", "#2563EB")
      sprintf(
        "<div class='rpt-finding-item'>
           <div class='rpt-finding-bullet' style='background:%s;'>%s</div>
           <div style='flex:1;'>%s</div>
         </div>",
        bullet_col, num, text_html)
    }

    # ════════════════════════════════════════════════════════════════════════
    # Compute all metrics + run significance tests
    # ════════════════════════════════════════════════════════════════════════

    # 1. Rate Spread (continuous — weighted t-test)
    rate_cu    <- round(wmean(cu, "Rate_Spread_Pct"), 3)
    rate_noncu <- round(wmean(noncu, "Rate_Spread_Pct"), 3)
    rate_diff_bp <- round((rate_cu - rate_noncu) * 100, 1)
    rate_s     <- test_mean_sig(d, "Rate_Spread_Pct")

    # 2. LTV (continuous — weighted t-test)
    ltv_cu    <- round(wmean(cu, "ltv"), 1)
    ltv_noncu <- round(wmean(noncu, "ltv"), 1)
    ltv_s     <- test_mean_sig(d, "ltv")

    # 3. Overall Satisfaction (binary — prop.test)
    sat_cu    <- round(rpt_bin_rate(cu,    "x28a"), 1)
    sat_noncu <- round(rpt_bin_rate(noncu, "x28a"), 1)
    sat_s     <- bin_sig(cu, noncu, "x28a")

    # 4a. FICO Score (continuous — weighted t-test)
    fico_cu    <- round(wmean(cu,    "Score_Origin"))
    fico_noncu <- round(wmean(noncu, "Score_Origin"))
    fico_s     <- test_mean_sig(d, "Score_Origin")

    # 4b. DTI (continuous — weighted t-test)
    dti_cu    <- round(wmean(cu,    "dti"), 1)
    dti_noncu <- round(wmean(noncu, "dti"), 1)
    dti_s     <- test_mean_sig(d, "dti")

    # 5. Racial composition (binary prop test: % White)
    white_cu    <- round(mean(cu$Race    == "White", na.rm=TRUE)*100, 1)
    white_noncu <- round(mean(noncu$Race == "White", na.rm=TRUE)*100, 1)
    white_s     <- tryCatch({
      x1 <- sum(cu$Race    == "White", na.rm=TRUE)
      x2 <- sum(noncu$Race == "White", na.rm=TRUE)
      p  <- prop.test(c(x1,x2), c(nrow(cu),nrow(noncu)), correct=FALSE)$p.value
      list(p=p, star=sig_label(p), sig=p<0.05)
    }, error=function(e) list(p=NA, star="", sig=FALSE))

    # 6. Shopping: applied to multiple lenders (binary — prop.test)
    shop_cu    <- round(rpt_bin_rate(cu,    "x12"), 1)
    shop_noncu <- round(rpt_bin_rate(noncu, "x12"), 1)
    shop_s     <- bin_sig(cu, noncu, "x12")

    # Extra: Closing costs rolled (binary — prop.test)
    rolled_cu    <- round(rpt_bin_rate(cu,    "x48b"), 1)
    rolled_noncu <- round(rpt_bin_rate(noncu, "x48b"), 1)
    rolled_s     <- bin_sig(cu, noncu, "x48b")

    n_cu    <- format(nrow(cu),    big.mark=",")
    n_noncu <- format(nrow(noncu), big.mark=",")

    # ════════════════════════════════════════════════════════════════════════
    # Build each finding
    # ════════════════════════════════════════════════════════════════════════

    # F1: Rate Spread
    f1_tone <- if (rate_diff_bp < 0) "positive" else "negative"
    f1 <- make_finding("1", paste0(
      "<strong>Rate Spread Above Market</strong>",
      sig_badge(rate_s, "continuous"),
      "<br>CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", rate_cu,
        "%</span> &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", rate_noncu, "%</span>",
      " &mdash; difference: <strong>", ifelse(rate_diff_bp > 0, "+", ""), rate_diff_bp, " bp</strong>",
      " <span style='color:#64748B;font-size:11px;'>(weighted t-test; each bp = ~$350 on $200K/30yr)</span>"),
      f1_tone)

    # F2: LTV
    f2_tone <- if (ltv_cu < ltv_noncu) "positive" else "negative"
    f2 <- make_finding("2", paste0(
      "<strong>Loan-to-Value Ratio (LTV)</strong>",
      sig_badge(ltv_s, "continuous"),
      "<br>CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", ltv_cu, "%</span>",
      " &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", ltv_noncu, "%</span>",
      " &mdash; difference: <strong>", round(ltv_cu - ltv_noncu, 1), " pp</strong>",
      " <span style='color:#64748B;font-size:11px;'>(weighted t-test; lower LTV = more equity at origination)</span>"), f2_tone)

    # F3: Satisfaction
    sat_diff  <- if (!is.na(sat_cu) && !is.na(sat_noncu)) sat_cu - sat_noncu else NA
    f3_tone   <- if (!is.na(sat_diff) && sat_diff >= 0) "positive" else "negative"
    f3 <- if (!is.na(sat_cu)) make_finding("3", paste0(
      "<strong>Overall Lender Satisfaction</strong>",
      sig_badge(sat_s, "binary"),
      "<br>CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", sat_cu,
        "%</span> &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", sat_noncu, "%</span>",
      " of borrowers rated their lender positively",
      if (!is.na(sat_diff)) paste0(" &mdash; <strong>", ifelse(sat_diff>=0,"+",""), round(sat_diff,1), " pp</strong>") else "",
      " <span style='color:#64748B;font-size:11px;'>(two-proportion z-test)</span>"),
      f3_tone)
    else make_finding("3", "<strong>Satisfaction:</strong> Insufficient data for current filters.", "neutral")

    # F4: Borrower profile — FICO + DTI
    f4 <- make_finding("4", paste0(
      "<strong>Borrower Profile</strong>",
      " <span style='font-size:10px;color:#D97706;font-weight:600;'>⚠ Selection effect — see note</span>",
      "<br>FICO &mdash; CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", fico_cu,
        "</span> vs Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", fico_noncu, "</span>",
      sig_badge(fico_s, "continuous"),
      " &nbsp;&nbsp; DTI &mdash; CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", dti_cu,
        "%</span> vs Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", dti_noncu, "%</span>",
      sig_badge(dti_s, "continuous"),
      "<br><span style='color:#94A3B8;font-size:11px;'>These reflect borrower composition differences,",
        " not lender behavior. Use the Regression tab to isolate the institution effect.</span>"),
      "warning")

    # F5: Racial composition
    # When race filter is active: show % of that race group within CU vs Non-CU (from full df)
    # When no filter: show % White CU vs Non-CU
    race_filtered <- !is.null(input$rpt_race) && length(input$rpt_race) > 0 && !("All" %in% input$rpt_race)
    if (race_filtered) {
      sel_races   <- input$rpt_race
      race_label  <- paste(sel_races, collapse=" / ")

      # Pull from full unfiltered df so denominator = all borrowers of each institution type
      full_cu    <- df %>% filter(Institution_Type == "Credit Union",     !is.na(Race))
      full_noncu <- df %>% filter(Institution_Type == "Non-Credit Union", !is.na(Race))

      race_pct_cu    <- round(mean(full_cu$Race    %in% sel_races) * 100, 1)
      race_pct_noncu <- round(mean(full_noncu$Race %in% sel_races) * 100, 1)
      race_diff      <- round(race_pct_cu - race_pct_noncu, 1)

      # Prop test on full df
      race_s5 <- tryCatch({
        x1 <- sum(full_cu$Race    %in% sel_races)
        x2 <- sum(full_noncu$Race %in% sel_races)
        p  <- prop.test(c(x1, x2), c(nrow(full_cu), nrow(full_noncu)), correct=FALSE)$p.value
        list(p=p, star=sig_label(p), sig=p < 0.05)
      }, error=function(e) list(p=NA, star="", sig=FALSE))

      f5_tone <- "neutral"  # no evaluative framing — just descriptive
      f5 <- make_finding("5", paste0(
        "<strong>Borrower Representation: ", race_label, "</strong>",
        sig_badge(race_s5, "binary"),
        "<br>",
        "<span style='color:#64748B;font-size:11px;'>",
          "Share of <em>all</em> mortgages (across institution type) originated to ",
          race_label, " borrowers:</span><br>",
        "CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", race_pct_cu, "%</span>",
        " &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", race_pct_noncu, "%</span>",
        " &mdash; difference: <strong>", ifelse(race_diff >= 0, "+", ""), race_diff, " pp</strong>",
        " <span style='color:#64748B;font-size:11px;'>(two-proportion z-test on full sample)</span>"), f5_tone)
    } else {
      f5_tone <- if (!is.na(white_cu) && white_cu > white_noncu) "warning" else "positive"
      f5 <- make_finding("5", paste0(
        "<strong>Racial Composition (% White borrowers)</strong>",
        sig_badge(white_s, "binary"),
        "<br>",
        "CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", white_cu, "%</span>",
        " &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", white_noncu, "%</span>",
        " &mdash; difference: <strong>", round(white_cu - white_noncu, 1), " pp</strong>",
        " <span style='color:#64748B;font-size:11px;'>(two-proportion z-test)</span>"), f5_tone)
    }

    # F6: Shopping behavior
    f6_tone <- if (!is.na(shop_cu) && shop_cu >= shop_noncu) "positive" else "negative"
    f6 <- if (!is.na(shop_cu)) make_finding("6", paste0(
      "<strong>Shopping Behavior (% Applied to Multiple Lenders)</strong>",
      sig_badge(shop_s, "binary"),
      "<br>",
      "CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", shop_cu, "%</span>",
      " &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", shop_noncu, "%</span>",
      " &mdash; difference: <strong>", round(shop_cu - shop_noncu, 1), " pp</strong>",
      " <span style='color:#64748B;font-size:11px;'>(two-proportion z-test)</span>"), f6_tone)
    else make_finding("6", "<strong>Shopping Behavior:</strong> Insufficient data for current filters.", "neutral")

    # F7: Closing costs rolled (bonus finding)
    f7_tone <- if (!is.na(rolled_cu) && rolled_cu <= rolled_noncu) "positive" else "negative"
    f7 <- if (!is.na(rolled_cu) && !is.na(rolled_noncu)) make_finding("7", paste0(
      "<strong>Closing Costs Rolled Into Loan</strong>",
      sig_badge(rolled_s, "binary"),
      "<br>",
      "CU: <span style='color:#2563EB;font-weight:700;font-size:15px;'>", rolled_cu, "%</span>",
      " &nbsp;vs&nbsp; Non-CU: <span style='color:#DC2626;font-weight:700;font-size:15px;'>", rolled_noncu, "%</span>",
      " rolled closing costs into principal &mdash; difference: <strong>",
      round(rolled_cu - rolled_noncu, 1), " pp</strong>",
      " <span style='color:#64748B;font-size:11px;'>(two-proportion z-test)</span>"), f7_tone)
    else ""

    HTML(paste0(
      "<div class='rpt-key-findings'>",
        "<h4><i class='fas fa-list-check' style='margin-right:8px;'></i>",
          "Key Findings — ", n_cu, " CU Borrowers vs ", n_noncu, " Non-CU Borrowers",
          "<span style='font-size:10px;font-weight:400;color:#64748B;margin-left:12px;'>",
            "Statistical tests shown inline. Weighted t-test for continuous; prop.test for binary.",
          "</span>",
        "</h4>",
        f1, f2, f3, f4, f5, f6, f7,
      "</div>"))
  })

  # ── Section 2: 8-metric KPI scorecard ─────────────────────────────────────
  output$rpt_kpi_scorecard <- renderUI({
    cu <- rpt_cu(); noncu <- rpt_noncu()
    if (nrow(cu) < 10 || nrow(noncu) < 10) return(NULL)

    make_kpi <- function(label, cu_val, noncu_val, suffix="",
                         better="lower", card_class="cu-card", decimals=1) {
      fmt  <- function(x) paste0(round(x, decimals), suffix)
      diff <- cu_val - noncu_val
      diff_fmt <- paste0(ifelse(diff >= 0, "+", ""), round(diff, decimals), suffix)
      is_better <- (better == "lower" && diff < 0) || (better == "higher" && diff > 0)
      delta_cls <- if (is_better) "positive" else if (diff == 0) "neutral" else "negative"
      tags$div(class = paste("rpt-kpi-card", card_class),
        tags$div(class = "rpt-kpi-label", label),
        tags$div(class = "rpt-kpi-values",
          tags$span(class = "rpt-kpi-cu",    fmt(cu_val)),
          tags$span(class = "rpt-kpi-vs",    "vs"),
          tags$span(class = "rpt-kpi-noncu", fmt(noncu_val))
        ),
        tags$div(class = paste("rpt-kpi-delta", delta_cls),
          diff_fmt,
          tags$span(style="font-weight:400;color:#94A3B8;font-size:10px;margin-left:4px;",
            "(CU minus Non-CU)"))
      )
    }

    s_rate <- test_mean_sig(report_data(), "Rate_Spread_Pct")
    s_ltv  <- test_mean_sig(report_data(), "ltv")
    s_dti  <- test_mean_sig(report_data(), "dti")
    s_fico <- test_mean_sig(report_data(), "Score_Origin")

    tags$div(class = "rpt-kpi-grid",
      make_kpi(paste0("Rate Spread (%)", s_rate$star),
               wmean(cu,"Rate_Spread_Pct"), wmean(noncu,"Rate_Spread_Pct"),
               "%", "lower", "cu-card", 3),
      make_kpi(paste0("Avg LTV (%)", s_ltv$star),
               wmean(cu,"ltv"), wmean(noncu,"ltv"),
               "%", "lower", "noncu-card", 1),
      make_kpi(paste0("Avg DTI (%)", s_dti$star),
               wmean(cu,"dti"), wmean(noncu,"dti"),
               "%", "lower", "sig-card", 1),
      make_kpi(paste0("Avg FICO", s_fico$star),
               wmean(cu,"Score_Origin"), wmean(noncu,"Score_Origin"),
               "", "higher", "warn-card", 0),
      make_kpi("Avg Interest Rate (%)",
               local({ v <- wmean(cu,"Interest_Rate");    if(is.na(v)||v<1||v>20) NA else v }),
               local({ v <- wmean(noncu,"Interest_Rate"); if(is.na(v)||v<1||v>20) NA else v }),
               "%", "lower", "cu-card", 2),
      {
        sat_cu    <- rpt_bin_rate(cu,    "x28a")
        sat_noncu <- rpt_bin_rate(noncu, "x28a")
        if (!is.na(sat_cu) && !is.na(sat_noncu))
          make_kpi("Overall Satisfaction (%)", sat_cu, sat_noncu, "%", "higher", "sig-card", 1)
        else tags$div(class="rpt-kpi-card sig-card",
               tags$div(class="rpt-kpi-label","Overall Satisfaction"),
               tags$div(style="font-size:11px;color:#94A3B8;","No data"))
      },
      {
        shop_cu    <- rpt_bin_rate(cu,    "x12")
        shop_noncu <- rpt_bin_rate(noncu, "x12")
        if (!is.na(shop_cu) && !is.na(shop_noncu))
          make_kpi("Shopped Multiple Lenders (%)", shop_cu, shop_noncu, "%", "higher", "warn-card", 1)
        else tags$div(class="rpt-kpi-card warn-card",
               tags$div(class="rpt-kpi-label","Shopped Multiple"),
               tags$div(style="font-size:11px;color:#94A3B8;","No data"))
      },
      {
        rolled_cu    <- rpt_bin_rate(cu,    "x48b")
        rolled_noncu <- rpt_bin_rate(noncu, "x48b")
        if (!is.na(rolled_cu) && !is.na(rolled_noncu))
          make_kpi("Closing Costs Rolled (%)", rolled_cu, rolled_noncu, "%", "lower", "noncu-card", 1)
        else tags$div(class="rpt-kpi-card noncu-card",
               tags$div(class="rpt-kpi-label","Costs Rolled"),
               tags$div(style="font-size:11px;color:#94A3B8;","No data"))
      }
    )
  })

  # ── Metrics comparison table ───────────────────────────────────────────────
  output$rpt_metrics_table <- renderUI({
    cu <- rpt_cu(); noncu <- rpt_noncu()
    if (nrow(cu) < 5 || nrow(noncu) < 5)
      return(HTML("<p style='color:#94A3B8;padding:20px;'>Insufficient data.</p>"))

    # Age: modal Age_Category (uses x74r survey report, falls back to numeric age_o1/age_o2)
    modal_age <- function(d) {
      if (!"Age_Category" %in% names(d)) return("Unknown")
      cats <- d$Age_Category[!is.na(d$Age_Category) & d$Age_Category != "Unknown" & nchar(d$Age_Category) > 0]
      if (length(cats) == 0) {
        # Last-resort: try bucketing raw Age numeric column
        if ("Age" %in% names(d)) {
          ages <- d$Age[!is.na(d$Age) & d$Age > 0]
          if (length(ages) == 0) return("Unknown")
          bins  <- cut(ages, breaks=c(18,25,35,45,55,65,75,120),
                       labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+"),
                       right=FALSE, include.lowest=TRUE)
          valid <- bins[!is.na(bins)]
          if (length(valid) == 0) return("Unknown")
          return(as.character(names(sort(table(valid), decreasing=TRUE))[1]))
        }
        return("Unknown")
      }
      as.character(names(sort(table(cats), decreasing=TRUE))[1])
    }

    metrics <- list(
      list(nm="Sample Size",           cu_v=nrow(cu),   noncu_v=nrow(noncu),
           fmt=function(x) format(round(x),big.mark=","), sig="", better="na"),
      list(nm="Avg FICO Score",        cu_v=wmean(cu,"Score_Origin"), noncu_v=wmean(noncu,"Score_Origin"),
           fmt=round, sig=test_mean_sig(report_data(),"Score_Origin")$star, better="higher"),
      list(nm="Avg LTV (%)",           cu_v=wmean(cu,"ltv"), noncu_v=wmean(noncu,"ltv"),
           fmt=function(x) paste0(round(x,1),"%"), sig=test_mean_sig(report_data(),"ltv")$star, better="lower"),
      list(nm="Avg DTI (%)",           cu_v=wmean(cu,"dti"), noncu_v=wmean(noncu,"dti"),
           fmt=function(x) paste0(round(x,1),"%"), sig=test_mean_sig(report_data(),"dti")$star, better="lower"),
      list(nm="Avg Rate Spread (%)",   cu_v=wmean(cu,"Rate_Spread_Pct"), noncu_v=wmean(noncu,"Rate_Spread_Pct"),
           fmt=function(x) paste0(round(x,3),"%"), sig=test_mean_sig(report_data(),"Rate_Spread_Pct")$star, better="lower"),
      list(nm="Avg Interest Rate (%)",
           cu_v=local({ v <- wmean(cu,"Interest_Rate"); if(is.na(v)||v<1||v>20) NA else v }),
           noncu_v=local({ v <- wmean(noncu,"Interest_Rate"); if(is.na(v)||v<1||v>20) NA else v }),
           fmt=function(x) if(is.na(x)) "N/A" else paste0(round(x,2),"%"),
           sig=test_mean_sig(report_data(),"Interest_Rate")$star, better="lower"),
      list(nm="Modal Age Group",
           cu_v=0, noncu_v=0,   # placeholder — displayed as text separately
           fmt=function(x) "-", sig="", better="na"),
      list(nm="% 30-Year Term",
           cu_v=mean(cu$Term_Label=="30 Years",na.rm=TRUE)*100,
           noncu_v=mean(noncu$Term_Label=="30 Years",na.rm=TRUE)*100,
           fmt=function(x) paste0(round(x,1),"%"), sig="", better="na"),
      list(nm="% Conventional Loan",
           cu_v=mean(cu$Loan_Type=="Conventional",na.rm=TRUE)*100,
           noncu_v=mean(noncu$Loan_Type=="Conventional",na.rm=TRUE)*100,
           fmt=function(x) paste0(round(x,1),"%"), sig="", better="na"),
      list(nm="% Purchase (not Refi)",
           cu_v=mean(cu$Cashout_Label=="Purchase",na.rm=TRUE)*100,
           noncu_v=mean(noncu$Cashout_Label=="Purchase",na.rm=TRUE)*100,
           fmt=function(x) paste0(round(x,1),"%"), sig="", better="na")
    )

    cu_age    <- modal_age(cu)
    noncu_age <- modal_age(noncu)
    rows <- sapply(metrics, function(m) {
      if (m$nm == "Modal Age Group") {
        return(sprintf(
          "<tr style='background:#FFFFFF;'>
             <td style='padding:6px 10px;font-size:12px;font-weight:500;color:#0F172A;'>Modal Age Group</td>
             <td style='text-align:center;padding:6px 8px;font-size:12px;font-weight:700;color:#2563EB;'>%s</td>
             <td style='text-align:center;padding:6px 8px;font-size:12px;font-weight:700;color:#DC2626;'>%s</td>
             <td style='text-align:center;padding:6px 8px;font-size:11px;color:#94A3B8;'>—</td>
           </tr>", cu_age, noncu_age))
      }
      diff      <- m$cu_v - m$noncu_v
      is_better <- (m$better == "lower" && diff < 0) || (m$better == "higher" && diff > 0)
      is_worse  <- (m$better == "lower" && diff > 0) || (m$better == "higher" && diff < 0)
      row_bg    <- if (is_better) "#FAFFFE" else if (is_worse) "#FFFAFA" else "#FFFFFF"
      arrow     <- if (abs(diff) < 0.001) "=" else if (diff > 0) "▲" else "▼"
      arrow_col <- if (is_better) "#16A34A" else if (is_worse) "#DC2626" else "#94A3B8"
      sprintf(
        "<tr style='background:%s;'>
           <td style='padding:6px 10px;font-size:12px;font-weight:500;color:#0F172A;'>%s<sup style='color:#7C3AED;font-size:9px;margin-left:2px;'>%s</sup></td>
           <td style='text-align:center;padding:6px 8px;font-size:13px;font-weight:700;color:#2563EB;'>%s</td>
           <td style='text-align:center;padding:6px 8px;font-size:13px;font-weight:700;color:#DC2626;'>%s</td>
           <td style='text-align:center;padding:6px 8px;font-size:12px;color:%s;font-weight:700;'>%s</td>
         </tr>",
        row_bg, m$nm, m$sig,
        m$fmt(m$cu_v), m$fmt(m$noncu_v),
        arrow_col, arrow)
    })

    HTML(paste0(
      "<table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>",
      "<thead><tr style='background:#F8FAFC;border-bottom:2px solid #E2E8F0;'>",
      "<th style='text-align:left;padding:7px 10px;font-size:11px;color:#475569;'>Metric</th>",
      "<th style='text-align:center;padding:7px;font-size:11px;color:#2563EB;font-weight:700;'>CU</th>",
      "<th style='text-align:center;padding:7px;font-size:11px;color:#DC2626;font-weight:700;'>Non-CU</th>",
      "<th style='text-align:center;padding:7px;font-size:11px;color:#475569;'>CU vs Non-CU</th>",
      "</tr></thead><tbody>",
      paste(rows, collapse=""),
      "</tbody></table>",
      "<div style='font-size:10px;color:#94A3B8;margin-top:6px;'>",
      "Superscript: *** p&lt;0.001 &nbsp; ** p&lt;0.01 &nbsp; * p&lt;0.05 (weighted t-test)<br>",
      "Green row = CU advantage &nbsp; Red row = CU disadvantage",
      "</div>"))
  })

  # ── Satisfaction chart (zoomed axis) ──────────────────────────────────────
  output$rpt_satisfaction_chart <- renderPlotly({
    # Curated satisfaction variables with clean short labels
    # Using hardcoded var→label map so we get exactly 8 meaningful items
    sat_items <- list(
      list(var="x28a", label="Overall lender satisfaction"),
      list(var="x27a", label="Got best-fit mortgage terms"),
      list(var="x27b", label="Got lowest rate available"),
      list(var="x28b", label="Application process"),
      list(var="x28d", label="Loan closing process"),
      list(var="x28e", label="Disclosure documents"),
      list(var="x49",  label="Closing costs as expected"),
      list(var="x21a", label="Loan Estimate was clear")
    )

    d <- report_data()
    sat_df <- lapply(sat_items, function(item) {
      vb <- paste0(item$var, "_binary")
      if (!vb %in% names(d)) return(NULL)
      d %>% filter(!is.na(get(vb))) %>%
        group_by(Institution_Type) %>%
        summarise(
          pct = mean(get(vb) == "Positive (Very/Somewhat)", na.rm=TRUE) * 100,
          n   = n(),
          .groups = "drop"
        ) %>%
        mutate(Variable = item$label)
    }) %>% bind_rows()

    if (nrow(sat_df) == 0) return(
      plotly_empty() %>%
        layout(title=list(text="No satisfaction data for current filters",
                          font=list(color="#94A3B8", size=13)),
               paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"))

    # Keep only items that have data for BOTH institution types
    sat_df <- sat_df %>%
      group_by(Variable) %>%
      filter(n_distinct(Institution_Type) == 2, all(n >= 5)) %>%
      ungroup()

    if (nrow(sat_df) == 0) return(NULL)

    # Order variables by CU rate descending
    cu_order <- sat_df %>%
      filter(Institution_Type == "Credit Union") %>%
      arrange(desc(pct)) %>%
      pull(Variable)
    sat_df$Variable <- factor(sat_df$Variable, levels = rev(cu_order))

    # Zoomed x-axis — start 5pp below minimum for visual differentiation
    all_pcts <- sat_df$pct
    x_min <- max(0,  floor(min(all_pcts, na.rm=TRUE) / 5) * 5 - 3)
    x_max <- min(101, ceiling(max(all_pcts, na.rm=TRUE) / 5) * 5 + 5)

    cu_d    <- sat_df %>% filter(Institution_Type == "Credit Union")
    noncu_d <- sat_df %>% filter(Institution_Type == "Non-Credit Union")

    plot_ly() %>%
      add_bars(
        data = cu_d, y = ~Variable, x = ~pct,
        name = "Credit Union", orientation = "h",
        marker = list(color = v2_colors$cu, opacity = 0.88,
                      line = list(color = "white", width = 0.5)),
        text  = ~paste0(round(pct, 1), "%"),
        textposition = "outside",
        textfont = list(size = 10, color = "#0F172A"),
        hovertemplate = "<b>Credit Union</b><br>%{y}<br>%{x:.1f}% positive<extra></extra>"
      ) %>%
      add_bars(
        data = noncu_d, y = ~Variable, x = ~pct,
        name = "Non-Credit Union", orientation = "h",
        marker = list(color = v2_colors$noncu, opacity = 0.88,
                      line = list(color = "white", width = 0.5)),
        text  = ~paste0(round(pct, 1), "%"),
        textposition = "outside",
        textfont = list(size = 10, color = "#0F172A"),
        hovertemplate = "<b>Non-Credit Union</b><br>%{y}<br>%{x:.1f}% positive<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        bargap  = 0.25,
        xaxis = list(
          title      = "% of Borrowers Rating Positively",
          range      = c(x_min, x_max),
          ticksuffix = "%",
          showgrid   = TRUE, gridcolor = "#E2E8F0",
          zeroline   = FALSE, tickfont = list(size = 10)
        ),
        yaxis = list(
          title    = "",
          tickfont = list(size = 11, color = "#334155"),
          showgrid = FALSE,
          autorange = "reversed"
        ),
        font          = list(family = "Inter, sans-serif", size = 11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(orientation = "h", x = 0.1, y = -0.18,
                      font = list(size = 11)),
        margin = list(t = 5, b = 50, l = 15, r = 75)
      )
  })

  # ── Rate spread by COVID period ────────────────────────────────────────────
  output$rpt_rate_period <- renderPlotly({
    d <- report_data() %>%
      filter(COVID_period %in% period_order, !is.na(Rate_Spread_Pct)) %>%
      group_by(COVID_period, Institution_Type) %>%
      summarise(avg = mean(Rate_Spread_Pct, na.rm=TRUE),
                n   = n(), .groups="drop") %>%
      mutate(
        COVID_period = factor(COVID_period, levels = period_order),
        avg_bp       = round(avg * 100, 1),
        lbl = paste0(ifelse(avg_bp >= 0, "+", ""), avg_bp, " bp"),
        tip = paste0(
          "<b>", Institution_Type, "</b><br>",
          COVID_period, "<br>",
          "<b>", ifelse(avg_bp >= 0,
                        paste0("+", avg_bp, " bp above market"),
                        paste0(abs(avg_bp), " bp below market (borrower benefit)")), "</b>",
          "<br><span style='color:#94A3B8;'>n = ", format(n, big.mark=","), "</span>")
      )

    cu_d    <- d %>% filter(Institution_Type == "Credit Union")
    noncu_d <- d %>% filter(Institution_Type == "Non-Credit Union")

    y_rng <- range(d$avg, na.rm=TRUE)
    y_pad <- diff(range(y_rng)) * 0.32
    y_lo  <- y_rng[1] - y_pad
    y_hi  <- y_rng[2] + y_pad + 0.001

    # Label position: above bar if positive, below bar if negative
    cu_lbl_pos    <- ifelse(cu_d$avg    >= 0, "outside", "outside")
    noncu_lbl_pos <- ifelse(noncu_d$avg >= 0, "outside", "outside")

    plot_ly() %>%

      # ── Non-CU bars — always brand red ────────────────────────────────────
      add_bars(
        data          = noncu_d,
        x             = ~COVID_period,
        y             = ~avg,
        name          = "Non-Credit Union",
        marker        = list(
          color       = v2_colors$noncu,          # always red
          opacity     = 0.82,
          line        = list(color = "white", width = 2)
        ),
        text          = ~lbl,
        textposition  = noncu_lbl_pos,
        textfont      = list(size=11, color="#334155",
                             family="Inter, sans-serif", weight=600),
        hovertemplate = "%{customdata}<extra></extra>",
        customdata    = ~tip,
        offsetgroup   = "noncu"
      ) %>%

      # ── CU bars — always brand blue ────────────────────────────────────────
      add_bars(
        data          = cu_d,
        x             = ~COVID_period,
        y             = ~avg,
        name          = "Credit Union",
        marker        = list(
          color       = v2_colors$cu,             # always blue
          opacity     = 0.92,
          line        = list(color = "white", width = 2)
        ),
        text          = ~lbl,
        textposition  = cu_lbl_pos,
        textfont      = list(size=11, color="#334155",
                             family="Inter, sans-serif", weight=600),
        hovertemplate = "%{customdata}<extra></extra>",
        customdata    = ~tip,
        offsetgroup   = "cu"
      ) %>%

      layout(
        barmode     = "group",
        bargap      = 0.28,
        bargroupgap = 0.06,

        xaxis = list(
          title      = "",
          showgrid   = FALSE,
          tickfont   = list(size=11, color="#475569"),
          fixedrange = TRUE
        ),
        yaxis = list(
          title      = list(text = "Rate Spread (%)",
                            font = list(size=11, color="#64748B")),
          showgrid   = TRUE,
          gridcolor  = "#F1F5F9",
          gridwidth  = 1,
          zeroline   = FALSE,
          tickformat = ".2f",
          ticksuffix = "%",
          range      = list(y_lo, y_hi),
          fixedrange = TRUE
        ),

        shapes = list(
          # Dotted zero reference line
          list(type  = "line", x0 = 0, x1 = 1, xref = "paper",
               y0    = 0,      y1  = 0,
               line  = list(color = "#94A3B8", width = 1.5, dash = "dot")),
          # Subtle green tint in below-zero zone
          list(type      = "rect", x0 = 0, x1 = 1, xref = "paper",
               y0        = y_lo, y1 = 0,
               fillcolor = "rgba(22,163,74,0.05)",
               line      = list(width = 0), layer = "below")
        ),

        annotations = list(
          list(text      = "\u2193 BELOW MARKET \u2014 borrower benefit",
               x = 0.01, y = min(y_lo * 0.5, -0.001),
               xref = "paper", yref = "y",
               showarrow = FALSE, xanchor = "left",
               font = list(size = 9, color = "#16A34A",
                           family = "Inter, sans-serif")),
          list(text      = "\u2191 ABOVE MARKET \u2014 borrower cost",
               x = 0.01, y = max(y_hi * 0.72, 0.001),
               xref = "paper", yref = "y",
               showarrow = FALSE, xanchor = "left",
               font = list(size = 9, color = "#94A3B8",
                           family = "Inter, sans-serif"))
        ),

        font          = list(family = "Inter, sans-serif", size = 11),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        legend = list(
          orientation = "h", x = 0.5, y = -0.18, xanchor = "center",
          bgcolor     = "rgba(0,0,0,0)",
          font        = list(size = 11, color = "#475569"),
          traceorder  = "reversed"
        ),
        margin = list(t = 16, b = 60, l = 55, r = 16)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── FICO distribution ──────────────────────────────────────────────────────
  output$rpt_fico_inst <- renderPlotly({
    d <- report_data() %>%
      filter(FICO_category != "Unknown") %>%
      group_by(FICO_category, Institution_Type) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE),.groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct=round(wn/sum(wn)*100,1),
             FICO_category=factor(FICO_category,levels=fico_order))
    cu_d    <- d %>% filter(Institution_Type=="Credit Union")
    noncu_d <- d %>% filter(Institution_Type=="Non-Credit Union")
    plot_ly() %>%
      add_bars(data=cu_d, x=~FICO_category, y=~pct, name="Credit Union",
               marker=list(color=v2_colors$cu,opacity=.85),
               hovertemplate="CU %{x}: %{y:.1f}%<extra></extra>") %>%
      add_bars(data=noncu_d, x=~FICO_category, y=~pct, name="Non-Credit Union",
               marker=list(color=v2_colors$noncu,opacity=.85),
               hovertemplate="Non-CU %{x}: %{y:.1f}%<extra></extra>") %>%
      layout(barmode="group",
             xaxis=list(title="FICO Tier", showgrid=FALSE, tickfont=list(size=9)),
             yaxis=list(title="% of Institution", ticksuffix="%",
                        showgrid=TRUE, gridcolor="#F1F5F9",
                        range=list(0, NULL)),
             font=list(family="Inter,sans-serif",size=11),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             legend=list(orientation="h",x=0,y=-0.25,font=list(size=10)),
             margin=list(t=5,b=50,l=45,r=10))
  })

  # ── Race composition ───────────────────────────────────────────────────────
  output$rpt_race_chart <- renderPlotly({
    d <- report_data() %>%
      filter(Race != "Unknown") %>%
      group_by(Race, Institution_Type) %>%
      summarise(wn=sum(analysis_weight,na.rm=TRUE),.groups="drop") %>%
      group_by(Institution_Type) %>%
      mutate(pct=round(wn/sum(wn)*100,1))
    cu_d    <- d %>% filter(Institution_Type=="Credit Union")
    noncu_d <- d %>% filter(Institution_Type=="Non-Credit Union")
    plot_ly() %>%
      add_bars(data=cu_d, x=~Race, y=~pct, name="Credit Union",
               marker=list(color=v2_colors$cu,opacity=.85),
               text=~paste0(pct,"%"), textposition="outside",
               hovertemplate="CU %{x}: %{y:.1f}%%<extra></extra>") %>%
      add_bars(data=noncu_d, x=~Race, y=~pct, name="Non-Credit Union",
               marker=list(color=v2_colors$noncu,opacity=.85),
               text=~paste0(pct,"%"), textposition="outside",
               hovertemplate="Non-CU %{x}: %{y:.1f}%%<extra></extra>") %>%
      layout(barmode="group",
             xaxis=list(title="", showgrid=FALSE),
             yaxis=list(title="% of Institution", ticksuffix="%", showgrid=TRUE, gridcolor="#F1F5F9"),
             font=list(family="Inter,sans-serif",size=11),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             legend=list(orientation="h",x=0,y=-0.25,font=list(size=10)),
             margin=list(t=5,b=50,l=45,r=10))
  })

  # ── Closing experience problem rates ──────────────────────────────────────
  output$rpt_closing_chart <- renderPlotly({
    closing_vars <- list(
      "Costs Rolled Into Loan"     = "x48b",
      "Felt Rushed at Closing"     = "x53g",
      "Unexpected Terms at Closing"= "x53d"
    )
    # Note: x49 (costs matched expectations) is a POSITIVE outcome (higher = better transparency)
    # so it doesn't belong in a "problem rates" chart. Show it separately if needed.
    d <- report_data()
    rows <- lapply(names(closing_vars), function(nm) {
      vb <- paste0(closing_vars[[nm]], "_binary")
      if (!vb %in% names(d)) return(NULL)
      cu_r    <- mean(rpt_cu()[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      noncu_r <- mean(rpt_noncu()[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      data.frame(Variable=nm, CU=round(cu_r,1), NonCU=round(noncu_r,1))
    }) %>% bind_rows()
    if (nrow(rows) == 0) return(NULL)
    plot_ly() %>%
      add_bars(data=rows, y=~Variable, x=~CU, name="Credit Union",
               orientation="h", marker=list(color=v2_colors$cu,opacity=.85),
               text=~paste0(CU,"%"), textposition="outside",
               hovertemplate="<b>CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      add_bars(data=rows, y=~Variable, x=~NonCU, name="Non-Credit Union",
               orientation="h", marker=list(color=v2_colors$noncu,opacity=.85),
               text=~paste0(NonCU,"%"), textposition="outside",
               hovertemplate="<b>Non-CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      layout(barmode="group",
             xaxis=list(title="% of Borrowers", ticksuffix="%", showgrid=TRUE, gridcolor="#F1F5F9"),
             yaxis=list(title="", autorange="reversed", tickfont=list(size=10), showgrid=FALSE),
             font=list(family="Inter,sans-serif",size=11),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             legend=list(orientation="h",x=0,y=-0.22,font=list(size=10)),
             margin=list(t=5,b=45,l=10,r=60))
  })

  # ── Shopping behavior chart ────────────────────────────────────────────────
  output$rpt_shopping_chart <- renderPlotly({
    shop_vars <- list(
      "Applied to Multiple Lenders" = "x12",
      "Seriously Considered 2+ Lenders" = "x11",
      "Got Pre-Approval Before Offer" = "x34b",
      "Verified Terms Externally" = "x20h",
      "Took Homebuying Counseling" = "x29"
    )
    d <- report_data()
    rows <- lapply(names(shop_vars), function(nm) {
      vb <- paste0(shop_vars[[nm]], "_binary")
      if (!vb %in% names(d)) return(NULL)
      cu_r    <- mean(rpt_cu()[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      noncu_r <- mean(rpt_noncu()[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      data.frame(Variable=nm, CU=round(cu_r,1), NonCU=round(noncu_r,1))
    }) %>% bind_rows()
    if (nrow(rows) == 0) return(NULL)
    plot_ly() %>%
      add_bars(data=rows, y=~Variable, x=~CU, name="Credit Union",
               orientation="h", marker=list(color=v2_colors$cu,opacity=.85),
               text=~paste0(CU,"%"), textposition="outside",
               hovertemplate="<b>CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      add_bars(data=rows, y=~Variable, x=~NonCU, name="Non-Credit Union",
               orientation="h", marker=list(color=v2_colors$noncu,opacity=.85),
               text=~paste0(NonCU,"%"), textposition="outside",
               hovertemplate="<b>Non-CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      layout(barmode="group",
             xaxis=list(title="% of Borrowers", ticksuffix="%", showgrid=TRUE, gridcolor="#F1F5F9"),
             yaxis=list(title="", autorange="reversed", tickfont=list(size=10), showgrid=FALSE),
             font=list(family="Inter,sans-serif",size=11),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             legend=list(orientation="h",x=0,y=-0.22,font=list(size=10)),
             margin=list(t=5,b=45,l=10,r=60))
  })

  # ── Financial literacy chart ───────────────────────────────────────────────
  output$rpt_literacy_chart <- renderPlotly({
    lit_vars <- list(
      "Can explain mortgage process" = "x56a",
      "Can explain fixed vs ARM"     = "x56b",
      "Loan Estimate was clear"      = "x21a",
      "Received Home Loan Toolkit"   = "x18",
      "Satisfied with disclosures"   = "x28e"
    )
    d <- report_data()
    rows <- lapply(names(lit_vars), function(nm) {
      vb <- paste0(lit_vars[[nm]], "_binary")
      if (!vb %in% names(d)) return(NULL)
      cu_r    <- mean(rpt_cu()[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      noncu_r <- mean(rpt_noncu()[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)*100
      data.frame(Variable=nm, CU=round(cu_r,1), NonCU=round(noncu_r,1))
    }) %>% bind_rows()
    if (nrow(rows) == 0) return(NULL)
    all_vals <- c(rows$CU, rows$NonCU)
    x_min <- max(0, floor(min(all_vals,na.rm=TRUE)/10)*10 - 5)
    x_max <- min(100, ceiling(max(all_vals,na.rm=TRUE)/10)*10 + 8)
    plot_ly() %>%
      add_bars(data=rows, y=~Variable, x=~CU, name="Credit Union",
               orientation="h", marker=list(color=v2_colors$cu,opacity=.85),
               text=~paste0(CU,"%"), textposition="outside",
               hovertemplate="<b>CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      add_bars(data=rows, y=~Variable, x=~NonCU, name="Non-Credit Union",
               orientation="h", marker=list(color=v2_colors$noncu,opacity=.85),
               text=~paste0(NonCU,"%"), textposition="outside",
               hovertemplate="<b>Non-CU</b> %{y}: %{x:.1f}%%<extra></extra>") %>%
      layout(barmode="group",
             xaxis=list(title="% of Borrowers", range=c(x_min,x_max), ticksuffix="%",
                        showgrid=TRUE, gridcolor="#F1F5F9"),
             yaxis=list(title="", autorange="reversed", tickfont=list(size=10), showgrid=FALSE),
             font=list(family="Inter,sans-serif",size=11),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
             legend=list(orientation="h",x=0,y=-0.22,font=list(size=10)),
             margin=list(t=5,b=45,l=10,r=60))
  })

  # ── Policy narrative ────────────────────────────────────────────────────────
  output$rpt_policy_narrative <- renderUI({
    cu <- rpt_cu(); noncu <- rpt_noncu()
    if (nrow(cu) < 10 || nrow(noncu) < 10)
      return(HTML("<div class='rpt-narrative-box'><p style='color:#94A3B8;'>Generate report to see narrative.</p></div>"))

    d <- report_data()

    rate_cu      <- round(wmean(cu, "Rate_Spread_Pct"), 3)
    rate_noncu   <- round(wmean(noncu, "Rate_Spread_Pct"), 3)
    rate_diff_bp <- round((rate_cu - rate_noncu) * 100, 1)
    ltv_cu       <- round(wmean(cu,    "ltv"), 1)
    ltv_noncu    <- round(wmean(noncu, "ltv"), 1)
    dti_cu       <- round(wmean(cu,    "dti"), 1)
    dti_noncu    <- round(wmean(noncu, "dti"), 1)
    fico_cu      <- round(wmean(cu,    "Score_Origin"))
    fico_noncu   <- round(wmean(noncu, "Score_Origin"))
    n_cu         <- format(nrow(cu),    big.mark=",")
    n_noncu      <- format(nrow(noncu), big.mark=",")
    white_cu     <- round(mean(cu$Race    == "White", na.rm=TRUE) * 100, 1)
    white_noncu  <- round(mean(noncu$Race == "White", na.rm=TRUE) * 100, 1)

    # Significance tests
    s_rate  <- test_mean_sig(d, "Rate_Spread_Pct")
    ltv_s   <- test_mean_sig(d, "ltv")
    dti_s   <- test_mean_sig(d, "dti")

    # White composition sig test
    white_s <- tryCatch({
      x1 <- sum(cu$Race    == "White", na.rm=TRUE)
      x2 <- sum(noncu$Race == "White", na.rm=TRUE)
      p  <- prop.test(c(x1, x2), c(nrow(cu), nrow(noncu)), correct=FALSE)$p.value
      list(p=p, star=sig_label(p), sig=p < 0.05)
    }, error=function(e) list(p=NA, star="", sig=FALSE))

    # Race filter state
    race_filtered <- !is.null(input$rpt_race) &&
                     length(input$rpt_race) > 0 &&
                     !("All" %in% input$rpt_race)

    # ── Compute extra metrics for narrative ────────────────────────────────
    int_cu    <- round(wmean(cu,    "Interest_Rate"), 2)
    int_noncu <- round(wmean(noncu, "Interest_Rate"), 2)
    int_s      <- test_mean_sig(d, "Interest_Rate")
    fico_s_narr <- test_mean_sig(d, "Score_Origin")

    sat_cu2    <- round(rpt_bin_rate(cu,    "x28a"), 1)
    sat_noncu2 <- round(rpt_bin_rate(noncu, "x28a"), 1)

    shop_cu2    <- round(rpt_bin_rate(cu,    "x12"), 1)
    shop_noncu2 <- round(rpt_bin_rate(noncu, "x12"), 1)
    shop_s2     <- tryCatch({
      vb <- "x12_binary"
      if (!vb %in% names(cu)) return(list(p=NA,star="",sig=FALSE))
      x1<-sum(cu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      x2<-sum(noncu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      p<-prop.test(c(x1,x2),c(nrow(cu),nrow(noncu)),correct=FALSE)$p.value
      list(p=p,star=sig_label(p),sig=p<0.05)
    },error=function(e)list(p=NA,star="",sig=FALSE))

    le_cu    <- round(rpt_bin_rate(cu,    "x21a"), 1)
    le_noncu <- round(rpt_bin_rate(noncu, "x21a"), 1)
    le_s     <- tryCatch({
      vb <- "x21a_binary"
      if (!vb %in% names(cu)) return(list(p=NA,star="",sig=FALSE))
      x1<-sum(cu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      x2<-sum(noncu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      p<-prop.test(c(x1,x2),c(nrow(cu),nrow(noncu)),correct=FALSE)$p.value
      list(p=p,star=sig_label(p),sig=p<0.05)
    },error=function(e)list(p=NA,star="",sig=FALSE))

    rushed_cu    <- round(rpt_bin_rate(cu,    "x53g"), 1)
    rushed_noncu <- round(rpt_bin_rate(noncu, "x53g"), 1)
    rushed_s     <- tryCatch({
      vb <- "x53g_binary"
      if (!vb %in% names(cu)) return(list(p=NA,star="",sig=FALSE))
      x1<-sum(cu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      x2<-sum(noncu[[vb]]=="Positive (Very/Somewhat)",na.rm=TRUE)
      p<-prop.test(c(x1,x2),c(nrow(cu),nrow(noncu)),correct=FALSE)$p.value
      list(p=p,star=sig_label(p),sig=p<0.05)
    },error=function(e)list(p=NA,star="",sig=FALSE))

    # Helper: compact sig inline
    si <- function(s) {
      if (is.na(s$p)) return("")
      if (s$sig) paste0(" <span style='color:#16A34A;font-weight:600;font-size:11px;'>(Sig ",s$star,")</span>")
      else        paste0(" <span style='color:#94A3B8;font-size:11px;'>(n.s., p=",round(s$p,3),")</span>")
    }

    # Helper: CU vs NonCU inline with colors
    cv <- function(cu_v, noncu_v, suffix="", decimals=1) {
      paste0(
        "<span style='color:#2563EB;font-weight:700;'>", round(cu_v,decimals), suffix, "</span>",
        " vs ",
        "<span style='color:#DC2626;font-weight:700;'>", round(noncu_v,decimals), suffix, "</span>"
      )
    }

    # ── Pre-compute Section 3 metrics robustly (no silent NA collapse) ────────
    bin_test_narr <- function(vb) {
      tryCatch({
        if (!vb %in% names(cu)) return(list(p=NA, star="", sig=FALSE))
        x1 <- sum(cu[[vb]]    == "Positive (Very/Somewhat)", na.rm=TRUE)
        x2 <- sum(noncu[[vb]] == "Positive (Very/Somewhat)", na.rm=TRUE)
        n1 <- sum(!is.na(cu[[vb]]));  n2 <- sum(!is.na(noncu[[vb]]))
        if (n1 < 5 || n2 < 5) return(list(p=NA, star="", sig=FALSE))
        p <- prop.test(c(x1,x2), c(n1,n2), correct=FALSE)$p.value
        list(p=p, star=sig_label(p), sig=p < 0.05)
      }, error=function(e) list(p=NA, star="", sig=FALSE))
    }

    sat_s3    <- bin_test_narr("x28a_binary")
    shop_s3   <- bin_test_narr("x12_binary")
    le_s3     <- bin_test_narr("x21a_binary")
    rushed_s3 <- bin_test_narr("x53g_binary")
    rolled_s3 <- bin_test_narr("x48b_binary")

    rolled_cu2    <- round(rpt_bin_rate(cu,    "x48b"), 1)
    rolled_noncu2 <- round(rpt_bin_rate(noncu, "x48b"), 1)

    # Build each metric row — always shown, explicit fallback if data unavailable
    make_metric_row <- function(label, cu_v, noncu_v, s, suffix="%", decimals=1) {
      if (is.na(cu_v) || is.na(noncu_v)) {
        return(paste0(
          "<div class='rpt-metric-row' style='color:#94A3B8;'>",
            "<span class='rpt-metric-label'>", label, ":</span> ",
            "Not available for current filters.",
          "</div>"))
      }
      diff_v <- round(cu_v - noncu_v, decimals)
      diff_str <- paste0(ifelse(diff_v >= 0, "+", ""), diff_v, " pp")
      paste0(
        "<div class='rpt-metric-row'>",
          "<span class='rpt-metric-label'>", label, ":</span> ",
          "<span style='color:#2563EB;font-weight:700;'>", round(cu_v,decimals), suffix, "</span>",
          " vs ",
          "<span style='color:#DC2626;font-weight:700;'>", round(noncu_v,decimals), suffix, "</span>",
          " &mdash; diff <strong>", diff_str, "</strong>",
          si(s),
        "</div>")
    }

    s3_rows <- paste0(
      make_metric_row("Overall satisfaction (% positive)",        sat_cu2,    sat_noncu2,   sat_s3),
      make_metric_row("Applied to multiple lenders (%)",          shop_cu2,   shop_noncu2,  shop_s3),
      make_metric_row("Loan Estimate easy to understand (%)",     le_cu,      le_noncu,     le_s3),
      make_metric_row("Felt rushed at closing (%)",               rushed_cu,  rushed_noncu, rushed_s3),
      make_metric_row("Closing costs rolled into loan (%)",       rolled_cu2, rolled_noncu2, rolled_s3)
    )


    # Race representation for Section 2 narrative
    race_note_s2 <- if (race_filtered) {
      sel_races  <- input$rpt_race
      race_label <- paste(sel_races, collapse=" / ")
      full_cu_n    <- df %>% filter(Institution_Type == "Credit Union",     !is.na(Race))
      full_noncu_n <- df %>% filter(Institution_Type == "Non-Credit Union", !is.na(Race))
      rp_cu    <- round(mean(full_cu_n$Race    %in% sel_races) * 100, 1)
      rp_noncu <- round(mean(full_noncu_n$Race %in% sel_races) * 100, 1)
      rp_s     <- tryCatch({
        x1 <- sum(full_cu_n$Race    %in% sel_races)
        x2 <- sum(full_noncu_n$Race %in% sel_races)
        p  <- prop.test(c(x1,x2), c(nrow(full_cu_n),nrow(full_noncu_n)), correct=FALSE)$p.value
        list(p=p, star=sig_label(p), sig=p<0.05)
      }, error=function(e) list(p=NA,star="",sig=FALSE))
      paste0(
        "<div class='rpt-metric-row'>",
          "<span class='rpt-metric-label'>Share of all loans to ", race_label, " borrowers:</span> ",
          "<span style='color:#2563EB;font-weight:700;'>", rp_cu, "%</span>",
          " vs <span style='color:#DC2626;font-weight:700;'>", rp_noncu, "%</span>",
          " &mdash; diff <strong>", ifelse(rp_cu-rp_noncu>=0,"+",""), round(rp_cu-rp_noncu,1), " pp</strong>",
          si(rp_s),
          "<span style='color:#94A3B8;font-size:11px;margin-left:6px;'>",
            "(computed on full unfiltered sample; race filter does not affect denominator)",
          "</span>",
        "</div>")
    } else {
      make_metric_row("Share of loans to White borrowers", white_cu, white_noncu,
                      list(p=white_s$p, star=white_s$star, sig=white_s$sig))
    }

    HTML(paste0(
    "<div class='rpt-narrative-box'>",

    # ── Header ────────────────────────────────────────────────────────────────
    "<div style='display:flex;align-items:flex-start;margin-bottom:20px;",
                "padding-bottom:12px;border-bottom:2px solid #E2E8F0;'>",
      "<div>",
        "<div style='font-size:11px;font-weight:700;color:#2563EB;text-transform:uppercase;",
                    "letter-spacing:.8px;margin-bottom:4px;'>Data Analysis</div>",
        "<h3 style='margin:0;font-size:18px;font-weight:800;color:#0F172A;'>",
          "Credit Union vs. Non-Credit Union \u2014 Summary of Observed Differences</h3>",
        "<div style='font-size:12px;color:#64748B;margin-top:4px;'>",
          "NSMO Public Use File \u00b7 n\u202f=\u202f", n_cu, " CU + ", n_noncu, " Non-CU",
          " \u00b7 Generated: ", format(Sys.Date(),"%B %d, %Y"),
          " \u00b7 Weighted t-test (continuous) | prop.test (binary)",
        "</div>",
      "</div>",
    "</div>",

    # ── Section 1: Rate & loan terms ─────────────────────────────────────────
    "<div class='rpt-section'>",
      "<h4>1. Interest Rate, Spread, and Loan Terms</h4>",
      "<div style='display:grid;gap:8px;'>",
        make_metric_row("Rate spread above benchmark", rate_cu,  rate_noncu,  s_rate,  "%", 3),
        make_metric_row("Average mortgage interest rate", int_cu, int_noncu,  int_s,   "%", 2),
        make_metric_row("Average loan-to-value (LTV)", ltv_cu,  ltv_noncu,   ltv_s),
        make_metric_row("Average debt-to-income (DTI)", dti_cu,  dti_noncu,  dti_s),
      "</div>",
      if (s_rate$sig)
        paste0("<div class='rpt-policy-box' style='margin-top:10px;'>",
               "Rate spread difference of ", abs(rate_diff_bp), " bp is statistically significant (",
               s_rate$star, "). One basis point \u2248 $350 over a $200,000 30-year mortgage.",
               "</div>")
      else
        paste0("<div class='rpt-policy-box' style='margin-top:10px;background:#F8FAFC;border-color:#CBD5E1;'>",
               "Rate spread difference is not statistically significant at the 5% level in this sample.",
               "</div>"),
    "</div>",

    # ── Section 2: Borrower demographics (FICO + race only) ──────────────────
    "<div class='rpt-section'>",
      "<h4>2. Borrower Demographics</h4>",
      "<div style='display:grid;gap:8px;'>",
        paste0(
          "<div class='rpt-metric-row'>",
            "<span class='rpt-metric-label'>Average FICO at origination:</span> ",
            "<span style='color:#2563EB;font-weight:700;'>", fico_cu, "</span>",
            " vs <span style='color:#DC2626;font-weight:700;'>", fico_noncu, "</span>",
            " &mdash; diff <strong>", fico_cu - fico_noncu, " pts</strong>",
            si(fico_s_narr),
            "<span style='color:#94A3B8;font-size:11px;margin-left:6px;'>",
              "\u25b3 Membership composition \u2014 see Regression tab for controls",
            "</span>",
          "</div>"),
        race_note_s2,
      "</div>",
      "<div class='rpt-policy-box' style='margin-top:10px;'>",
        "<strong>Note:</strong> Borrower characteristic differences reflect membership composition, ",
        "not necessarily lender behavior. Use the Regression tab to control for FICO, LTV, DTI, ",
        "race, and loan type when assessing the institution-type effect.",
      "</div>",
    "</div>",

    # ── Section 3: Consumer experience ───────────────────────────────────────
    "<div class='rpt-section'>",
      "<h4>3. Consumer Experience</h4>",
      "<div style='display:grid;gap:8px;'>",
        s3_rows,
      "</div>",
    "</div>",

    # ── Methodology note ──────────────────────────────────────────────────────
    "<div class='rpt-limitation-note'>",
      "<strong>Methodology &amp; Limitations:</strong> ",
      "Source: NSMO Public Use File (FHFA/CFPB, Waves 1-42, 2013-2023), n=58,381. ",
      "Institution type derived from data pre-processing. ",
      "All survey responses are self-reported and subject to recall bias. ",
      "Continuous outcomes: weighted two-sample t-test (Welch approximation with survey weights). ",
      "Binary outcomes: two-proportion z-test (prop.test). ",
      "All results are descriptive and do not establish causation. ",
      "NSMO excludes HELOCs, reverse mortgages, and second liens. ",
      "Sample sizes vary by question due to skip patterns and wave coverage.",
    "</div>",

    "</div>"))
  })

  # ── Download handler ─────────────────────────────────────────────────────────
  output$rpt_download <- downloadHandler(
    filename = function() paste0("nsmo_cu_report_", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- report_data() %>%
        select(Institution_Type, FICO_category, COVID_period, Race, Sex_Label,
               Loan_Type, Cashout_Label, Term_Label, Score_Origin, ltv, dti,
               Rate_Spread_Pct, Interest_Rate, loan_amount_cat, open_year, Age) %>%
        mutate(across(everything(), as.character))
      write.csv(d, file, row.names = FALSE)
    }
  )

  # ── PDF Report Download ────────────────────────────────────────────────────
  # Reactive: detect PDF engine once at startup
  # ── HTML Report Download (always works; user prints to PDF from browser) ──
  output$rpt_pdf_engine_status <- renderUI({
    tags$div(
      style = "font-size:10px;color:#4ADE80;text-align:center;margin-top:-2px;
               background:rgba(22,163,74,0.15);border-radius:6px;padding:3px 8px;",
      tags$i(class="fas fa-circle-check", style="margin-right:4px;"),
      "Styled HTML report — open & Cmd+P to save as PDF"
    )
  })

  output$rpt_download_pdf <- downloadHandler(
    filename = function() paste0("nsmo_cu_report_", Sys.Date(), ".html"),
    contentType = "text/html",
    content = function(file) {

      cu    <- rpt_cu()
      noncu <- rpt_noncu()
      if (nrow(cu) < 5 || nrow(noncu) < 5) {
        writeLines("<html><body><p>Insufficient data. Please generate the report first.</p></body></html>", file)
        return(invisible(NULL))
      }

      # ── Compute all metrics ───────────────────────────────────────────────
      n_cu    <- nrow(cu);  n_noncu <- nrow(noncu)
      rate_cu    <- round(wmean(cu,    "Rate_Spread_Pct"), 3)
      rate_noncu <- round(wmean(noncu, "Rate_Spread_Pct"), 3)
      rate_bp    <- round((rate_cu - rate_noncu) * 100, 1)
      ltv_cu     <- round(wmean(cu,    "ltv"),          1);  ltv_noncu  <- round(wmean(noncu, "ltv"),          1)
      dti_cu     <- round(wmean(cu,    "dti"),          1);  dti_noncu  <- round(wmean(noncu, "dti"),          1)
      fico_cu    <- round(wmean(cu,    "Score_Origin"))   ;  fico_noncu <- round(wmean(noncu, "Score_Origin"))
      int_cu     <- round(wmean(cu,    "Interest_Rate"), 2);  int_noncu  <- round(wmean(noncu, "Interest_Rate"), 2)
      white_cu   <- round(mean(cu$Race    == "White", na.rm=TRUE)*100, 1)
      white_noncu<- round(mean(noncu$Race == "White", na.rm=TRUE)*100, 1)
      sat_cu     <- round(rpt_bin_rate(cu,    "x28a"), 1)
      sat_noncu  <- round(rpt_bin_rate(noncu, "x28a"), 1)
      shop_cu    <- round(rpt_bin_rate(cu,    "x12"),  1)
      shop_noncu <- round(rpt_bin_rate(noncu, "x12"),  1)
      le_cu      <- round(rpt_bin_rate(cu,    "x21a"), 1)
      le_noncu   <- round(rpt_bin_rate(noncu, "x21a"), 1)
      rushed_cu  <- round(rpt_bin_rate(cu,    "x53g"), 1)
      rushed_noncu<-round(rpt_bin_rate(noncu, "x53g"), 1)
      rolled_cu  <- round(rpt_bin_rate(cu,    "x48b"), 1)
      rolled_noncu<-round(rpt_bin_rate(noncu, "x48b"), 1)

      # Significance tests
      d      <- report_data()
      s_rate <- test_mean_sig(d, "Rate_Spread_Pct")
      s_ltv  <- test_mean_sig(d, "ltv")
      s_dti  <- test_mean_sig(d, "dti")
      s_fico <- test_mean_sig(d, "Score_Origin")

      # Filter summary
      filter_parts <- c(
        if (length(input$rpt_fico)     > 0) paste("FICO:",      paste(input$rpt_fico,     collapse=", ")),
        if (length(input$rpt_period)   > 0) paste("Period:",    paste(input$rpt_period,   collapse=", ")),
        if (length(input$rpt_race)     > 0) paste("Race:",      paste(input$rpt_race,     collapse=", ")),
        if (length(input$rpt_loan_type)> 0) paste("Loan Type:", paste(input$rpt_loan_type,collapse=", ")),
        if (length(input$rpt_gender)   > 0) paste("Gender:",    paste(input$rpt_gender,   collapse=", "))
      )
      filter_str <- if (length(filter_parts) > 0)
        paste(filter_parts, collapse=" &bull; ") else "All borrowers (no filters applied)"

      # ── Helper: one table row ───────────────────────────────────────────────
      mrow <- function(label, cu_v, noncu_v, suffix="", star="") {
        diff <- if (!is.na(cu_v) && !is.na(noncu_v)) round(cu_v - noncu_v, ifelse(suffix=="%",1,3)) else NA
        diff_str  <- if (!is.na(diff)) paste0(ifelse(diff>=0,"+",""), diff, suffix) else "&mdash;"
        diff_col  <- if (!is.na(diff) && diff > 0) "#16A34A" else if (!is.na(diff) && diff < 0) "#DC2626" else "#64748B"
        sprintf(
          "<tr><td>%s</td><td class=cu>%s%s</td><td class=ncu>%s%s</td>
           <td style='color:%s;font-weight:600;'>%s <small style='color:#94A3B8;'>%s</small></td></tr>",
          label,
          ifelse(!is.na(cu_v), cu_v, "N/A"), suffix,
          ifelse(!is.na(noncu_v), noncu_v, "N/A"), suffix,
          diff_col, diff_str, star)
      }

      # ── Helper: consumer experience rows ───────────────────────────────────
      brow <- function(label, cu_v, noncu_v) {
        diff <- if (!is.na(cu_v) && !is.na(noncu_v)) round(cu_v - noncu_v, 1) else NA
        diff_str <- if (!is.na(diff)) paste0(ifelse(diff>=0,"+",""), diff," pp") else "N/A"
        diff_col <- if (!is.na(diff) && diff > 0) "#16A34A" else if (!is.na(diff) && diff < 0) "#DC2626" else "#64748B"
        sprintf(
          "<tr><td>%s</td><td class=cu>%s%%</td><td class=ncu>%s%%</td>
           <td style='color:%s;font-weight:600;'>%s</td></tr>",
          label,
          ifelse(!is.na(cu_v), cu_v, "N/A"),
          ifelse(!is.na(noncu_v), noncu_v, "N/A"),
          diff_col, diff_str)
      }

      # ── Helper: key finding bullet ─────────────────────────────────────────
      fbullet <- function(n, col, html) {
        sprintf(
          "<div class=finding>
             <div class=fnum style='background:%s;'>%s</div>
             <div class=ftext>%s</div>
           </div>", col, n, html)
      }

      sigstr <- function(s) if (s$sig) paste0(" <span class=star>", s$star, "</span>") else " <span class=ns>n.s.</span>"

      f1 <- fbullet("1", if(rate_bp<0)"#16A34A" else "#DC2626", sprintf(
        "<strong>Rate Spread:</strong> CU <span class=cu>%.3f%%</span> vs Non-CU <span class=ncu>%.3f%%</span>
         &mdash; <strong>%.1f basis points</strong> %s%s",
        rate_cu, rate_noncu, abs(rate_bp),
        ifelse(rate_bp<0,"(CU advantage)","(Non-CU advantage)"), sigstr(s_rate)))

      f2 <- fbullet("2", if(ltv_cu<ltv_noncu)"#16A34A" else "#DC2626", sprintf(
        "<strong>Loan-to-Value:</strong> CU <span class=cu>%.1f%%</span> vs Non-CU <span class=ncu>%.1f%%</span>%s",
        ltv_cu, ltv_noncu, sigstr(s_ltv)))

      f3 <- fbullet("3", if(!is.na(sat_cu)&&!is.na(sat_noncu)&&sat_cu>=sat_noncu)"#16A34A" else "#D97706", sprintf(
        "<strong>Satisfaction:</strong> CU <span class=cu>%s%%</span> vs Non-CU <span class=ncu>%s%%</span> rated lender positively",
        ifelse(!is.na(sat_cu),sat_cu,"N/A"), ifelse(!is.na(sat_noncu),sat_noncu,"N/A")))

      f4 <- fbullet("4", "#D97706", sprintf(
        "<strong>Borrower Profile:</strong> FICO <span class=cu>%d</span> vs <span class=ncu>%d</span>%s
         &nbsp;&bull;&nbsp; DTI <span class=cu>%.1f%%</span> vs <span class=ncu>%.1f%%</span>%s
         <em class=caveat>Reflects membership composition, not lender behavior.</em>",
        fico_cu, fico_noncu, sigstr(s_fico), dti_cu, dti_noncu, sigstr(s_dti)))

      f5 <- fbullet("5", if(white_cu>white_noncu)"#D97706" else "#16A34A", sprintf(
        "<strong>Racial Composition:</strong> <span class=cu>%.1f%%</span> of CU vs <span class=ncu>%.1f%%</span> of Non-CU loans went to White borrowers (diff: %.1f pp)",
        white_cu, white_noncu, white_cu-white_noncu))

      f6 <- fbullet("6", if(!is.na(shop_cu)&&!is.na(shop_noncu)&&shop_cu>=shop_noncu)"#16A34A" else "#DC2626", sprintf(
        "<strong>Shopping:</strong> <span class=cu>%s%%</span> of CU vs <span class=ncu>%s%%</span> of Non-CU borrowers applied to multiple lenders",
        ifelse(!is.na(shop_cu),shop_cu,"N/A"), ifelse(!is.na(shop_noncu),shop_noncu,"N/A")))

      # ── Build HTML ─────────────────────────────────────────────────────────
      html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>NSMO Credit Union Report ', Sys.Date(), '</title>
<style>
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Arial,sans-serif;
       font-size:12px;color:#0F172A;background:#fff;line-height:1.5}

  /* Header */
  .hdr{background:linear-gradient(135deg,#0F172A,#1E3A5F);color:#fff;padding:28px 36px}
  .eyebrow{font-size:10px;font-weight:700;letter-spacing:2px;color:#93C5FD;
            text-transform:uppercase;margin-bottom:8px}
  .hdr h1{font-size:24px;font-weight:900;color:#F8FAFC;margin-bottom:4px}
  .hdr .sub{font-size:13px;color:#94A3B8;margin-bottom:14px}
  .pills{display:flex;gap:10px;flex-wrap:wrap}
  .pill{background:rgba(255,255,255,.1);border:1px solid rgba(255,255,255,.2);
        border-radius:20px;padding:3px 12px;font-size:10px;color:#E2E8F0}

  /* Print tip banner */
  .print-tip{background:#FFFBEB;border-bottom:1px solid #FDE68A;
              padding:8px 36px;font-size:11px;color:#92400E;display:flex;
              align-items:center;gap:8px}
  .print-tip strong{color:#78350F}

  /* Filter strip */
  .filters{background:#F1F5F9;border-bottom:1px solid #E2E8F0;
            padding:8px 36px;font-size:10px;color:#64748B}

  /* Content */
  .body{padding:24px 36px}
  .section{font-size:13px;font-weight:800;color:#0F172A;text-transform:uppercase;
            letter-spacing:.5px;border-bottom:2px solid #E2E8F0;
            padding-bottom:5px;margin:22px 0 12px}
  .section:first-child{margin-top:0}

  /* Key findings */
  .fbox{background:linear-gradient(135deg,#EFF6FF,#F0FDF4);
        border:1px solid #BFDBFE;border-left:5px solid #2563EB;
        border-radius:8px;padding:14px 18px;margin-bottom:20px}
  .fbox-hdr{font-size:10px;font-weight:800;color:#1D4ED8;text-transform:uppercase;
             letter-spacing:1px;margin-bottom:10px}
  .finding{display:flex;align-items:flex-start;gap:10px;padding:6px 0;
            border-bottom:1px solid rgba(37,99,235,.08);font-size:11.5px;line-height:1.5}
  .finding:last-child{border-bottom:none}
  .fnum{width:20px;height:20px;border-radius:50%;color:#fff;font-size:10px;
        font-weight:800;display:flex;align-items:center;justify-content:center;flex-shrink:0}
  .ftext{flex:1}
  .caveat{display:block;font-size:10px;color:#94A3B8;margin-top:2px}
  .star{color:#16A34A;font-weight:700}
  .ns{color:#94A3B8;font-size:10px}

  /* Tables */
  table{width:100%;border-collapse:collapse;margin-bottom:16px;font-size:11.5px}
  thead tr{background:#F8FAFC}
  th{text-align:left;padding:8px 10px;font-size:10px;font-weight:700;
     color:#475569;text-transform:uppercase;letter-spacing:.5px;
     border-bottom:2px solid #E2E8F0}
  th.cu{color:#2563EB} th.ncu{color:#DC2626}
  td{padding:7px 10px;border-bottom:1px solid #F1F5F9}
  tr:last-child td{border-bottom:none}
  td.cu{color:#2563EB;font-weight:700}
  td.ncu{color:#DC2626;font-weight:700}
  .cu{color:#2563EB;font-weight:700}
  .ncu{color:#DC2626;font-weight:700}

  /* Footer */
  .footer{margin-top:28px;padding:14px 36px;background:#F8FAFC;
          border-top:1px solid #E2E8F0;font-size:10px;color:#94A3B8;
          display:flex;justify-content:space-between}

  /* Print */
  @media print{
    body{print-color-adjust:exact;-webkit-print-color-adjust:exact}
    .print-tip{display:none}
    .hdr,.fbox{-webkit-print-color-adjust:exact}
    @page{margin:10mm;size:A4}
  }
</style>
</head>
<body>

<div class="hdr">
  <div class="eyebrow">NSMO Analysis &middot; Credit Union Mortgage Outcomes</div>
  <h1>Credit Union vs. Non-Credit Union</h1>
  <div class="sub">Mortgage Origination Outcomes: Rates, Satisfaction, Transparency &amp; Consumer Equity</div>
  <div class="pills">
    <span class="pill">&#128451; NSMO Public Use File &middot; 2013&ndash;2023</span>
    <span class="pill">&#128100; 58,381 Mortgage Originations</span>
    <span class="pill">&#127963; FHFA / CFPB Joint Survey</span>
    <span class="pill">&#128197; ', format(Sys.Date(),"%B %d, %Y"), '</span>
  </div>
</div>

<div class="print-tip">
  &#128438; <strong>To save as PDF:</strong> Press Cmd+P (Mac) or Ctrl+P (Windows), then choose &ldquo;Save as PDF&rdquo; as the destination.
</div>

<div class="filters">
  <strong>Filters:</strong> ', filter_str, ' &nbsp;&nbsp;
  <strong>Sample:</strong> ', format(n_cu,big.mark=","), ' CU &middot; ', format(n_noncu,big.mark=","), ' Non-CU
</div>

<div class="body">

<div class="section">Key Findings</div>
<div class="fbox">
  <div class="fbox-hdr">&#9776; Summary of Observed Differences &mdash; significance tests shown</div>
  ', f1, f2, f3, f4, f5, f6, '
</div>

<div class="section">Financial Metrics</div>
<table>
  <thead><tr>
    <th>Metric</th><th class="cu">Credit Union</th>
    <th class="ncu">Non-CU</th><th>Difference</th>
  </tr></thead>
  <tbody>
    ', mrow("Avg FICO Score",        fico_cu,  fico_noncu,  "",  s_fico$star),
       mrow("Avg LTV (%)",            ltv_cu,   ltv_noncu,   "%", s_ltv$star),
       mrow("Avg DTI (%)",            dti_cu,   dti_noncu,   "%", s_dti$star),
       mrow("Avg Rate Spread (%)",    rate_cu,  rate_noncu,  "%", s_rate$star),
       mrow("Avg Interest Rate (%)",  int_cu,   int_noncu,   "%", ""),
       mrow("White Borrowers (%)",    white_cu, white_noncu, "%", ""), '
  </tbody>
</table>

<div class="section">Consumer Experience</div>
<table>
  <thead><tr>
    <th>Metric</th><th class="cu">Credit Union</th>
    <th class="ncu">Non-CU</th><th>Difference</th>
  </tr></thead>
  <tbody>
    ', brow("Overall satisfaction (% positive)",        sat_cu,    sat_noncu),
       brow("Applied to multiple lenders (%)",           shop_cu,   shop_noncu),
       brow("Loan Estimate easy to understand (%)",      le_cu,     le_noncu),
       brow("Felt rushed at closing (%)",                rushed_cu, rushed_noncu),
       brow("Closing costs rolled into loan (%)",        rolled_cu, rolled_noncu), '
  </tbody>
</table>

</div>

<div class="footer">
  <span>Source: NSMO Public Use File (FHFA/CFPB, Waves 1&ndash;42, 2013&ndash;2023) &middot;
        Weighted t-test (continuous) | prop.test (binary) &middot; Descriptive only, not causal</span>
  <span>Generated ', format(Sys.Date(),"%B %d, %Y"), ' &middot; NSMO Dashboard</span>
</div>
</body></html>')

      writeLines(html, file, useBytes = TRUE)
    }
  )

    # TAB 6 — DATA EXPLORER
  # ══════════════════════════════════════════════════════════════════════════

  output$exp_total_loans <- renderValueBox({
    valueBox(format(nrow(explorer_data()), big.mark=","), "Total Loans", icon=icon("database"), color="blue")
  })
  output$exp_cu_pct <- renderValueBox({
    pct <- mean(explorer_data()$Institution_Type == "Credit Union", na.rm=TRUE) * 100
    valueBox(paste0(round(pct,1),"%"), "Credit Union %", icon=icon("percent"), color="green")
  })
  output$exp_avg_fico <- renderValueBox({
    avg <- mean(explorer_data()$Score_Origin, na.rm=TRUE)
    valueBox(round(avg), "Avg FICO", icon=icon("chart-line"), color="yellow")
  })
  output$exp_avg_amount <- renderValueBox({
    avg <- mean(explorer_data()$loan_amount_cat, na.rm=TRUE)
    valueBox(round(avg,1), "Avg Loan Cat", icon=icon("dollar-sign"), color="orange")
  })

  output$exp_data_table <- renderDT({
    d <- explorer_data() %>%
      select(Institution_Type, FICO_category, COVID_period, Race, Sex_Label,
             Loan_Type, Term_Label, LTV_Category, DTI_Category, Score_Origin,
             ltv, dti, Rate_Spread_Pct, Interest_Rate, loan_amount_cat, open_year) %>%
      rename(
        "Institution" = Institution_Type, "FICO Cat" = FICO_category,
        "Period" = COVID_period, "Loan Type" = Loan_Type,
        "Term" = Term_Label, "LTV Cat" = LTV_Category, "DTI Cat" = DTI_Category,
        "FICO" = Score_Origin, "LTV" = ltv, "DTI" = dti,
        "Rate Spread" = Rate_Spread_Pct, "Int. Rate" = Interest_Rate,
        "Loan Amt Cat" = loan_amount_cat, "Year" = open_year
      )
    datatable(d, options=list(pageLength=15, scrollX=TRUE, dom="Bfrtip",
                               buttons=c("copy","csv","excel"),
                               columnDefs=list(list(className="dt-center", targets="_all"))),
              extensions="Buttons", rownames=FALSE, filter="top",
              class="compact hover cell-border stripe")
  })

}
