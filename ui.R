# ui.R  —  NSMO Dashboard V2
# Improvements: modern dark-accent skin, shinyWidgets pills, new Regression tab, new Report tab

ui <- dashboardPage(
  skin = "black",

  # ── HEADER ────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$div(
      style = "display:flex;align-items:center;gap:10px;padding:2px 0;",
      # House icon in a colored circle
      tags$div(
        style = "width:32px;height:32px;border-radius:8px;background:linear-gradient(135deg,#2563EB,#0EA5E9);
                 display:flex;align-items:center;justify-content:center;flex-shrink:0;",
        tags$i(class="fas fa-house-chimney", style="color:white;font-size:15px;")
      ),
      tags$div(
        tags$div(style="font-weight:800;font-size:13px;color:#F8FAFC;letter-spacing:0.3px;line-height:1.1;",
          "NSMO Dashboard"),
        tags$div(style="font-size:9px;color:#94A3B8;letter-spacing:0.8px;text-transform:uppercase;line-height:1;",
          "Mortgage Survey Analytics")
      )
    ),
    titleWidth = 260
  ),

  # ── SIDEBAR ───────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 220,
    tags$div(
      style = "padding: 12px 16px 4px; color:#94a3b8; font-size:11px; font-weight:600; letter-spacing:1px; text-transform:uppercase;",
      "Navigation"
    ),
    sidebarMenu(
      id = "main_tabs",
      menuItem("Executive Summary", tabName = "summary",    icon = icon("gauge-high")),
      menuItem("Survey Responses",  tabName = "responses",  icon = icon("comments")),
      menuItem("Outcome Predictor", tabName = "ml_cu",      icon = icon("magnifying-glass-chart")),
      menuItem("Regression",        tabName = "regression", icon = icon("chart-line")),
      menuItem("Report",            tabName = "report",     icon = icon("file-lines")),
      menuItem("Data Explorer",     tabName = "explorer",   icon = icon("table"))
    )
  ),

  # ── BODY ──────────────────────────────────────────────────────────────────
  dashboardBody(

    # ── GLOBAL CSS ──────────────────────────────────────────────────────────
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
      tags$style(HTML("

        /* ── Reset & base ── */
        * { box-sizing: border-box; }
        body, .content-wrapper, .main-footer { background: #F1F5F9 !important; font-family: 'Inter', sans-serif; }

        /* ── Sidebar ── */
        .main-sidebar { background: #0F172A !important; }
        .main-sidebar .sidebar-menu > li > a {
          color: #CBD5E1 !important; font-size: 13px; font-weight: 500;
          border-left: 3px solid transparent !important; transition: all .2s;
        }
        .main-sidebar .sidebar-menu > li.active > a,
        .main-sidebar .sidebar-menu > li > a:hover {
          color: #FFFFFF !important; background: rgba(255,255,255,.07) !important;
          border-left-color: #3B82F6 !important;
        }
        .main-sidebar .sidebar-menu > li > a .fa,
        .main-sidebar .sidebar-menu > li > a .fas,
        .main-sidebar .sidebar-menu > li > a .far { color: #94A3B8 !important; width:20px; }
        .main-sidebar .sidebar-menu > li.active > a .fa,
        .main-sidebar .sidebar-menu > li.active > a .fas { color: #3B82F6 !important; }
        .main-header .logo { background: #0F172A !important; color: #F8FAFC !important; font-size:15px; }
        .main-header .navbar { background: #0F172A !important; }
        .main-header .navbar .sidebar-toggle { color: #94A3B8 !important; }

        /* ── Cards / boxes ── */
        .box {
          border: 1px solid #E2E8F0 !important; border-top: none !important;
          border-radius: 10px !important; box-shadow: 0 1px 4px rgba(0,0,0,.06);
          background: #FFFFFF;
        }
        .box-header.with-border {
          border-bottom: 1px solid #E2E8F0 !important;
          border-radius: 10px 10px 0 0 !important;
          padding: 10px 16px;
        }
        .box-title { font-size: 13px !important; font-weight: 600 !important; color: #0F172A !important; }
        .box-header .btn { font-size: 11px; padding: 2px 8px; }

        /* ── Accent top border per status ── */
        .box.box-primary  { border-top: 3px solid #2563EB !important; }
        .box.box-info     { border-top: 3px solid #0EA5E9 !important; }
        .box.box-success  { border-top: 3px solid #16A34A !important; }
        .box.box-warning  { border-top: 3px solid #D97706 !important; }
        .box.box-danger   { border-top: 3px solid #DC2626 !important; }

        /* ── Value boxes ── */
        .small-box { border-radius: 10px !important; box-shadow: 0 1px 4px rgba(0,0,0,.08) !important; }
        .small-box h3 { font-size: 22px !important; font-weight: 700; }
        .small-box p  { font-size: 12px !important; font-weight: 500; opacity:.9; }

        /* ── Info boxes ── */
        .info-box { border-radius: 10px !important; box-shadow: 0 1px 4px rgba(0,0,0,.06) !important;
                    min-height: 80px; margin-bottom: 12px; }
        .info-box-number { font-size: 20px !important; font-weight: 700 !important; }
        .info-box-text   { font-size: 11px !important; font-weight: 500 !important; }

        /* ── Page headers ── */
        .v2-page-header {
          display: flex; align-items: center; gap: 10px;
          margin-bottom: 18px; padding-bottom: 12px;
          border-bottom: 2px solid #E2E8F0;
        }
        .v2-page-header h2 {
          margin: 0; font-size: 20px; font-weight: 700; color: #0F172A; letter-spacing: -.3px;
        }
        .v2-page-header .badge {
          background: #EFF6FF; color: #2563EB; font-size: 11px; font-weight: 600;
          border-radius: 6px; padding: 3px 9px;
        }

        /* ── Filter bar ── */
        .filter-bar .box { background: #F8FAFC !important; }
        .filter-bar label { font-size: 11px !important; font-weight: 600 !important;
                            color: #475569 !important; margin-bottom: 3px; text-transform:uppercase; letter-spacing:.5px; }
        .selectize-control .selectize-input { font-size: 12px !important; border-radius: 6px !important; }

        /* ── Significance notes ── */
        .sig-note { font-size: 11px; color: #64748B; padding: 5px 10px; margin-top: 6px;
                    background: #F8FAFC; border-left: 3px solid #94A3B8; border-radius: 0 4px 4px 0; }
        .sig-yes  { border-left-color: #16A34A !important; color: #15803D; }
        .sig-no   { border-left-color: #94A3B8; }

        /* ── Metric KPI cards ── */
        .kpi-card {
          background: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 10px;
          padding: 16px 20px; text-align: center;
          box-shadow: 0 1px 3px rgba(0,0,0,.05); transition: box-shadow .2s;
        }
        .kpi-card:hover { box-shadow: 0 4px 12px rgba(0,0,0,.10); }
        .kpi-value { font-size: 26px; font-weight: 700; line-height: 1.1; }
        .kpi-label { font-size: 11px; font-weight: 500; color: #64748B; margin-top: 4px; text-transform:uppercase; letter-spacing:.6px; }
        .kpi-delta { font-size: 11px; font-weight: 600; margin-top: 4px; }
        .kpi-cu    .kpi-value { color: #2563EB; }
        .kpi-noncu .kpi-value { color: #DC2626; }

        /* ── Report tab ── */
        .report-section { margin-bottom: 28px; }
        .report-section h4 { font-size: 15px; font-weight: 700; color: #0F172A;
                              border-bottom: 2px solid #E2E8F0; padding-bottom: 6px; margin-bottom: 14px; }
        .metric-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr)); gap: 12px; }

        /* ── Regression tab ── */
        .reg-output pre { font-size: 12px; background: #F8FAFC; border: 1px solid #E2E8F0;
                          border-radius: 8px; padding: 14px; color: #0F172A; }
        .reg-badge { display:inline-block; padding:2px 8px; border-radius:4px; font-size:11px; font-weight:600; }
        .reg-sig   { background:#DCFCE7; color:#15803D; }
        .reg-ns    { background:#FEE2E2; color:#B91C1C; }

        /* ── Buttons ── */
        .btn-run  { background:#2563EB; border-color:#2563EB; color:#fff; font-weight:600;
                    border-radius:7px; padding:7px 18px; font-size:13px; }
        .btn-run:hover { background:#1D4ED8; border-color:#1D4ED8; color:#fff; }
        .btn-reset { background:#F1F5F9; border-color:#CBD5E1; color:#475569; font-weight:600;
                     border-radius:7px; padding:7px 14px; font-size:13px; }
        .btn-reset:hover { background:#E2E8F0; color:#0F172A; }

        /* ── Tabs ── */
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover { border-top: 2px solid #2563EB !important; color:#0F172A; font-weight:600; }

        /* ── DT table ── */
        .dataTables_wrapper { font-size: 12px; }
        table.dataTable thead th { background: #F8FAFC; color: #475569; font-weight: 600; }

        /* ── Loading spinner ── */
        .shiny-spinner-output-container { min-height: 60px; }

        /* ── Executive Summary Hero ── */
        .exec-hero {
          background: linear-gradient(135deg, #0F172A 0%, #1E3A5F 45%, #1D4ED8 100%);
          border-radius: 16px; margin-bottom: 20px;
          display: flex; align-items: stretch; overflow: hidden;
          box-shadow: 0 8px 32px rgba(15,23,42,.28); min-height: 164px; position: relative;
        }
        .exec-hero-text { flex:1; padding:28px 32px; position:relative; z-index:2; }
        .exec-hero-eyebrow {
          font-size:10px; font-weight:700; letter-spacing:2px;
          text-transform:uppercase; color:#93C5FD; margin-bottom:8px;
        }
        .exec-hero-title {
          font-size:24px; font-weight:800; color:#FFFFFF;
          line-height:1.2; margin:0 0 8px; letter-spacing:-.4px;
        }
        .exec-hero-subtitle { font-size:12px; color:#CBD5E1; line-height:1.6; max-width:500px; }
        .exec-hero-pills { display:flex; gap:8px; margin-top:14px; flex-wrap:wrap; }
        .exec-hero-pill {
          background:rgba(255,255,255,.12); border:1px solid rgba(255,255,255,.2);
          border-radius:20px; padding:3px 12px; font-size:11px; font-weight:600; color:#E0F2FE;
        }
        .exec-hero-visual {
          width:240px; flex-shrink:0; display:flex; align-items:center;
          justify-content:center; padding:16px;
          background:rgba(255,255,255,.04); border-left:1px solid rgba(255,255,255,.08);
        }

        /* ── Executive Summary KPI Scorecard ── */
        .kpi-scorecard {
          display: grid;
          grid-template-columns: repeat(4, 1fr);
          gap: 14px;
          margin-bottom: 4px;
        }
        .kpi-scorecard-card {
          background: #FFFFFF;
          border: 1px solid #E2E8F0;
          border-radius: 14px;
          padding: 0;
          box-shadow: 0 2px 8px rgba(0,0,0,.06);
          transition: box-shadow .2s, transform .15s;
          position: relative;
          overflow: hidden;
        }
        .kpi-scorecard-card:hover {
          box-shadow: 0 8px 24px rgba(0,0,0,.12);
          transform: translateY(-2px);
        }
        /* Coloured top bar */
        .kpi-scorecard-card::before {
          content: '';
          display: block;
          height: 4px;
          border-radius: 14px 14px 0 0;
          width: 100%;
        }
        .kpi-card-cu::before      { background: linear-gradient(90deg,#2563EB,#0EA5E9); }
        .kpi-card-noncu::before   { background: linear-gradient(90deg,#DC2626,#F97316); }
        .kpi-card-neutral::before { background: linear-gradient(90deg,#7C3AED,#6366F1); }
        .kpi-card-green::before   { background: linear-gradient(90deg,#16A34A,#0D9488); }
        .kpi-card-amber::before   { background: linear-gradient(90deg,#D97706,#F59E0B); }

        /* Inner padding wrapper */
        .kpi-card-inner {
          padding: 16px 18px 14px;
          display: flex;
          align-items: flex-start;
          gap: 14px;
        }

        /* Icon */
        .kpi-icon-wrap {
          width: 46px; height: 46px;
          border-radius: 12px;
          display: flex; align-items: center; justify-content: center;
          flex-shrink: 0;
          font-size: 19px;
        }
        .kpi-icon-cu     { background: linear-gradient(135deg,#EFF6FF,#DBEAFE); color: #2563EB; }
        .kpi-icon-noncu  { background: linear-gradient(135deg,#FEF2F2,#FEE2E2); color: #DC2626; }
        .kpi-icon-violet { background: linear-gradient(135deg,#F5F3FF,#EDE9FE); color: #7C3AED; }
        .kpi-icon-green  { background: linear-gradient(135deg,#F0FDF4,#DCFCE7); color: #16A34A; }
        .kpi-icon-amber  { background: linear-gradient(135deg,#FFFBEB,#FEF3C7); color: #D97706; }

        .kpi-text { flex: 1; min-width: 0; }
        .kpi-sc-value {
          font-size: 24px; font-weight: 800; line-height: 1;
          color: #0F172A; white-space: nowrap; letter-spacing: -0.5px;
        }
        .kpi-sc-label {
          font-size: 10px; font-weight: 600; color: #64748B;
          margin-top: 3px; text-transform: uppercase; letter-spacing: .7px;
        }
        .kpi-sc-delta {
          font-size: 11px; font-weight: 700; margin-top: 5px;
          display: inline-flex; align-items: center; gap: 3px;
        }
        .kpi-sc-delta.up   { color: #16A34A; }
        .kpi-sc-delta.down { color: #DC2626; }
        .kpi-sc-delta.flat { color: #64748B; }

        /* CU vs Non-CU pill row */
        .kpi-compare-row {
          display: flex; gap: 5px; margin-top: 8px; flex-wrap: wrap;
        }
        .kpi-pill {
          font-size: 10px; font-weight: 700; padding: 2px 8px;
          border-radius: 20px; white-space: nowrap;
        }
        .kpi-pill-cu    { background: #EFF6FF; color: #2563EB; border: 1px solid #BFDBFE; }
        .kpi-pill-noncu { background: #FEF2F2; color: #DC2626; border: 1px solid #FEE2E2; }

        /* Significance star badge */
        .kpi-sig-badge {
          position: absolute; top: 12px; right: 12px;
          font-size: 9px; font-weight: 800; padding: 1px 6px;
          border-radius: 10px; letter-spacing: .3px;
        }
        .kpi-sig-yes { background:#F0FDF4; color:#16A34A; border:1px solid #BBF7D0; }
        .kpi-sig-no  { background:#F8FAFC; color:#94A3B8; border:1px solid #E2E8F0; }

        /* ── ML Outcome category banner ── */
        .outcome-banner {
          border-radius: 8px;
          padding: 10px 14px;
          margin-bottom: 10px;
          display: flex;
          align-items: center;
          gap: 10px;
          transition: all .25s;
        }
        .outcome-banner .ob-icon {
          font-size: 20px;
          width: 36px;
          height: 36px;
          border-radius: 8px;
          display: flex;
          align-items: center;
          justify-content: center;
          flex-shrink: 0;
        }
        .outcome-banner .ob-text { flex: 1; }
        .outcome-banner .ob-category {
          font-size: 10px;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: .8px;
          opacity: .75;
        }
        .outcome-banner .ob-name {
          font-size: 13px;
          font-weight: 600;
          margin-top: 1px;
          line-height: 1.3;
        }
        .outcome-banner .ob-desc {
          font-size: 10px;
          margin-top: 3px;
          opacity: .8;
          line-height: 1.4;
        }
        .ob-sat     { background:#EFF6FF; color:#1D4ED8; }
        .ob-sat     .ob-icon { background:#DBEAFE; }
        .ob-process { background:#F0FDF4; color:#15803D; }
        .ob-process .ob-icon { background:#DCFCE7; }
        .ob-closing { background:#FEF2F2; color:#B91C1C; }
        .ob-closing .ob-icon { background:#FEE2E2; }
        .ob-engage  { background:#FFFBEB; color:#B45309; }
        .ob-engage  .ob-icon { background:#FEF3C7; }
        .ob-wellbeing { background:#F5F3FF; color:#6D28D9; }
        .ob-wellbeing .ob-icon { background:#EDE9FE; }
        .ob-disclosure { background:#ECFEFF; color:#0E7490; }
        .ob-disclosure .ob-icon { background:#CFFAFE; }
        .ob-complexity { background:#FFF7ED; color:#C2410C; }
        .ob-complexity .ob-icon { background:#FFEDD5; }
        .ob-financial { background:#F8FAFC; color:#334155; }
        .ob-financial .ob-icon { background:#E2E8F0; }

        /* ── ML Model run loading state ── */

        /* Animated shimmer progress bar */
        #ml-loading-bar {
          display: none;
          height: 3px;
          border-radius: 0 0 3px 3px;
          background: linear-gradient(90deg, #7C3AED 0%, #2563EB 40%, #7C3AED 100%);
          background-size: 200% 100%;
          animation: ml-shimmer 1.4s ease infinite;
          margin: -1px -1px 0 -1px;
        }
        @keyframes ml-shimmer {
          0%   { background-position: 200% center; }
          100% { background-position: -200% center; }
        }

        /* Full loading overlay inside the Performance Metrics box */
        #ml-loading-overlay {
          display: none;          /* toggled to flex via JS */
          position: absolute;
          inset: 0;
          background: rgba(255,255,255,0.94);
          backdrop-filter: blur(3px);
          -webkit-backdrop-filter: blur(3px);
          border-radius: 0 0 10px 10px;
          z-index: 200;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          gap: 16px;
        }

        /* Spinning brain icon */
        .ml-spinner {
          width: 56px; height: 56px;
          border-radius: 50%;
          background: linear-gradient(135deg, #7C3AED, #2563EB);
          display: flex; align-items: center; justify-content: center;
          animation: ml-pulse 1.2s ease-in-out infinite;
          box-shadow: 0 0 0 0 rgba(124,58,237,0.4);
        }
        .ml-spinner i { color: white; font-size: 22px; }
        @keyframes ml-pulse {
          0%   { transform: scale(1);    box-shadow: 0 0 0 0   rgba(124,58,237,0.5); }
          50%  { transform: scale(1.08); box-shadow: 0 0 0 14px rgba(124,58,237,0);  }
          100% { transform: scale(1);    box-shadow: 0 0 0 0   rgba(124,58,237,0);   }
        }

        /* Step progress dots */
        .ml-steps {
          display: flex; gap: 8px; align-items: center;
        }
        .ml-step {
          display: flex; align-items: center; gap: 6px;
          font-size: 11px; font-weight: 600; color: #94A3B8;
          transition: color .3s;
        }
        .ml-step .step-dot {
          width: 8px; height: 8px; border-radius: 50%;
          background: #E2E8F0;
          transition: background .3s;
        }
        .ml-step.active     { color: #7C3AED; }
        .ml-step.active .step-dot { background: #7C3AED; animation: dot-bounce .6s ease infinite; }
        .ml-step.done       { color: #16A34A; }
        .ml-step.done .step-dot   { background: #16A34A; }
        @keyframes dot-bounce {
          0%,100% { transform: scale(1); }
          50%     { transform: scale(1.5); }
        }

        /* Loading text */
        .ml-loading-text {
          font-size: 13px; font-weight: 600; color: #475569;
          text-align: center; line-height: 1.5;
        }
        .ml-loading-subtext {
          font-size: 11px; color: #94A3B8; text-align: center;
          margin-top: 2px;
        }

        /* Run button in loading state */
        #run_ml_cu.running {
          background: linear-gradient(90deg, #6D28D9, #2563EB);
          border-color: transparent;
          color: white;
          pointer-events: none;
          animation: btn-gradient 1.8s ease infinite;
          background-size: 200% 100%;
        }
        @keyframes btn-gradient {
          0%   { background-position: 0% center; }
          100% { background-position: 200% center; }
        }

        /* Performance box needs position:relative for overlay */
        .ml-perf-wrap { position: relative; min-height: 160px; }

        /* ── Regression loading bar (reuses ML shimmer animation) ── */
        #reg-loading-bar {
          display: none;
          height: 3px;
          border-radius: 0 0 3px 3px;
          background: linear-gradient(90deg, #D97706 0%, #F59E0B 40%, #D97706 100%);
          background-size: 200% 100%;
          animation: ml-shimmer 1.4s ease infinite;
          margin: -1px -1px 0 -1px;
        }

        /* ── Regression outcome banner (shares ob- classes with ML tab) ── */
        .ob-sat-reg     { background:#FFFBEB; color:#B45309; }
        .ob-sat-reg     .ob-icon { background:#FEF3C7; }
        .ob-closing-reg { background:#FEF2F2; color:#B91C1C; }
        .ob-closing-reg .ob-icon { background:#FEE2E2; }
        .ob-engage-reg  { background:#F0FDF4; color:#15803D; }
        .ob-engage-reg  .ob-icon { background:#DCFCE7; }
        .ob-disclosure-reg { background:#ECFEFF; color:#0E7490; }
        .ob-disclosure-reg .ob-icon { background:#CFFAFE; }
        .ob-product-reg { background:#FFF7ED; color:#C2410C; }
        .ob-product-reg .ob-icon { background:#FFEDD5; }
        .ob-financial-reg  { background:#F8FAFC; color:#334155; }
        .ob-financial-reg  .ob-icon { background:#E2E8F0; }
        .ob-wellbeing-reg  { background:#F5F3FF; color:#6D28D9; }
        .ob-wellbeing-reg  .ob-icon { background:#EDE9FE; }

        /* ── Regression coef table ── */
        .reg-sig   { color:#16A34A; font-weight:700; }
        .reg-ns    { color:#94A3B8; }
        .reg-badge { padding:2px 7px; border-radius:12px; font-size:10px; font-weight:600; }
        .reg-sig   { background:#F0FDF4; }
        .reg-ns    { background:#F8FAFC; }

        /* ── Predictor group headers ── */
        .pred-group-header {
          font-size:11px; font-weight:700; color:#475569;
          text-transform:uppercase; letter-spacing:.5px;
          margin:10px 0 4px 0; display:flex; align-items:center; gap:6px;
        }

        /* ── Policy Report styles ── */
        .rpt-key-findings {
          background: linear-gradient(135deg, #EFF6FF 0%, #F0FDF4 100%);
          border: 1px solid #BFDBFE;
          border-left: 5px solid #2563EB;
          border-radius: 10px;
          padding: 20px 24px;
          margin-bottom: 4px;
        }
        .rpt-key-findings h4 {
          font-size: 13px; font-weight: 700; color: #1D4ED8;
          text-transform: uppercase; letter-spacing: .7px;
          margin: 0 0 14px 0;
        }
        .rpt-finding-item {
          display: flex; align-items: flex-start; gap: 10px;
          padding: 8px 0; border-bottom: 1px solid rgba(37,99,235,.1);
          font-size: 13px; line-height: 1.5; color: #0F172A;
        }
        .rpt-finding-item:last-child { border-bottom: none; }
        .rpt-finding-bullet {
          width: 22px; height: 22px; border-radius: 50%;
          background: #2563EB; color: white;
          font-size: 11px; font-weight: 700;
          display: flex; align-items: center; justify-content: center;
          flex-shrink: 0; margin-top: 1px;
        }
        .rpt-number { font-size: 18px; font-weight: 800; color: #1D4ED8; }
        .rpt-number.red   { color: #DC2626; }
        .rpt-number.green { color: #16A34A; }
        .rpt-number.amber { color: #D97706; }
        .rpt-kpi-grid {
          display: grid; grid-template-columns: repeat(4, 1fr);
          gap: 10px; margin-bottom: 4px;
        }
        .rpt-kpi-card {
          background: white; border: 1px solid #E2E8F0;
          border-radius: 10px; padding: 14px 16px;
          position: relative; overflow: hidden;
        }
        .rpt-kpi-card::after {
          content: ''; position: absolute;
          bottom: 0; left: 0; right: 0; height: 3px;
        }
        .rpt-kpi-card.cu-card::after    { background: #2563EB; }
        .rpt-kpi-card.noncu-card::after { background: #DC2626; }
        .rpt-kpi-card.sig-card::after   { background: #16A34A; }
        .rpt-kpi-card.warn-card::after  { background: #D97706; }
        .rpt-kpi-label { font-size: 10px; font-weight: 700; color: #64748B;
          text-transform: uppercase; letter-spacing: .5px; margin-bottom: 6px; }
        .rpt-kpi-values { display: flex; align-items: baseline; gap: 8px; }
        .rpt-kpi-cu    { font-size: 20px; font-weight: 800; color: #2563EB; }
        .rpt-kpi-noncu { font-size: 20px; font-weight: 800; color: #DC2626; }
        .rpt-kpi-vs    { font-size: 11px; color: #CBD5E1; }
        .rpt-kpi-delta { font-size: 11px; font-weight: 600; margin-top: 3px; }
        .rpt-kpi-delta.positive { color: #16A34A; }
        .rpt-kpi-delta.negative { color: #DC2626; }
        .rpt-kpi-delta.neutral  { color: #94A3B8; }
        .rpt-narrative-box {
          background: white; border: 1px solid #E2E8F0;
          border-radius: 12px; padding: 24px 28px; margin-bottom: 4px;
        }
        .rpt-section { margin-bottom: 20px; }
        .rpt-section h4 { font-size: 14px; font-weight: 700; color: #0F172A;
          border-bottom: 2px solid #E2E8F0; padding-bottom: 6px; margin: 0 0 10px 0; }
        .rpt-policy-box { background:#FFFBEB; border-left:4px solid #D97706;
          border-radius:6px; padding:10px 14px; font-size:12px; color:#92400E; margin-top:10px; }
        .rpt-caution-box { background:#FEF2F2; border-left:4px solid #DC2626;
          border-radius:6px; padding:10px 14px; font-size:12px; color:#7F1D1D; margin-top:10px; }
        .rpt-positive-box { background:#F0FDF4; border-left:4px solid #16A34A;
          border-radius:6px; padding:10px 14px; font-size:12px; color:#14532D; margin-top:10px; }
        .rpt-limitation-note { background:#F8FAFC; border:1px solid #E2E8F0;
          border-radius:8px; padding:12px 16px; font-size:11px; color:#64748B; margin-top:16px; }

        /* Misc */
        hr.v2 { border: none; border-top: 1px solid #E2E8F0; margin: 16px 0; }
        .text-muted-v2 { color: #94A3B8; font-size: 12px; }

        /* ── HTML report download ── */
      "))
    ),



    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # TAB 1 — EXECUTIVE SUMMARY
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "summary",

        # ── HERO BANNER ─────────────────────────────────────────────────────────
        tags$div(
          style = "
            background: linear-gradient(135deg, #0F172A 0%, #1a3a5c 50%, #1e4976 100%);
            border-radius: 14px;
            padding: 0;
            margin-bottom: 20px;
            overflow: hidden;
            position: relative;
            box-shadow: 0 8px 32px rgba(15,23,42,0.25);
          ",

          # Background decorative elements
          tags$div(style="position:absolute;inset:0;overflow:hidden;pointer-events:none;",
            # Large circle top-right
            tags$div(style="position:absolute;top:-60px;right:-60px;width:260px;height:260px;
                            border-radius:50%;background:rgba(59,130,246,0.08);"),
            # Small circle bottom-left
            tags$div(style="position:absolute;bottom:-40px;left:20%;width:160px;height:160px;
                            border-radius:50%;background:rgba(16,185,129,0.06);"),
            # Dotted grid pattern strip
            tags$div(style="position:absolute;right:0;top:0;bottom:0;width:320px;
                            background-image:radial-gradient(rgba(255,255,255,0.04) 1px, transparent 1px);
                            background-size:18px 18px;")
          ),

          tags$div(
            style = "position:relative;display:flex;align-items:stretch;gap:0;",

            # LEFT: Text content
            tags$div(
              style = "flex:1;padding:28px 32px;",

              # Eyebrow label
              tags$div(
                style = "display:inline-flex;align-items:center;gap:6px;
                         background:rgba(59,130,246,0.18);border:1px solid rgba(59,130,246,0.3);
                         border-radius:20px;padding:3px 12px;margin-bottom:14px;",
                tags$i(class="fas fa-circle-dot", style="color:#60A5FA;font-size:8px;"),
                tags$span(style="font-size:10px;font-weight:700;letter-spacing:1.5px;
                                 color:#93C5FD;text-transform:uppercase;",
                  "NSMO Survey Analysis  ·  2013–2023")
              ),

              # Main headline
              tags$h1(
                style = "margin:0 0 6px 0;font-size:26px;font-weight:900;color:#F8FAFC;
                         line-height:1.15;letter-spacing:-0.5px;",
                "Credit Union", tags$br(),
                tags$span(style="color:#60A5FA;", "vs. Non-Credit Union")
              ),

              # Subtitle
              tags$p(
                style = "margin:0 0 18px 0;font-size:13px;color:#94A3B8;line-height:1.6;max-width:440px;",
                "Comparing mortgage origination outcomes across rates, borrower satisfaction,",
                " disclosure transparency, and consumer equity."
              ),

              # Stat pills row
              tags$div(
                style = "display:flex;gap:8px;flex-wrap:wrap;",
                tags$div(
                  style = "display:flex;align-items:center;gap:6px;background:rgba(255,255,255,0.07);
                           border:1px solid rgba(255,255,255,0.12);border-radius:8px;padding:6px 12px;",
                  tags$i(class="fas fa-database", style="color:#60A5FA;font-size:11px;"),
                  tags$div(
                    tags$div(style="font-size:14px;font-weight:800;color:#F8FAFC;line-height:1;", "58,381"),
                    tags$div(style="font-size:9px;color:#94A3B8;text-transform:uppercase;letter-spacing:.6px;", "Mortgages")
                  )
                ),
                tags$div(
                  style = "display:flex;align-items:center;gap:6px;background:rgba(255,255,255,0.07);
                           border:1px solid rgba(255,255,255,0.12);border-radius:8px;padding:6px 12px;",
                  tags$i(class="fas fa-building-columns", style="color:#34D399;font-size:11px;"),
                  tags$div(
                    tags$div(style="font-size:14px;font-weight:800;color:#F8FAFC;line-height:1;", "6.6%"),
                    tags$div(style="font-size:9px;color:#94A3B8;text-transform:uppercase;letter-spacing:.6px;", "Credit Unions")
                  )
                ),
                tags$div(
                  style = "display:flex;align-items:center;gap:6px;background:rgba(255,255,255,0.07);
                           border:1px solid rgba(255,255,255,0.12);border-radius:8px;padding:6px 12px;",
                  tags$i(class="fas fa-calendar", style="color:#F59E0B;font-size:11px;"),
                  tags$div(
                    tags$div(style="font-size:14px;font-weight:800;color:#F8FAFC;line-height:1;", "42"),
                    tags$div(style="font-size:9px;color:#94A3B8;text-transform:uppercase;letter-spacing:.6px;", "Survey Waves")
                  )
                ),
                tags$div(
                  style = "display:flex;align-items:center;gap:6px;background:rgba(255,255,255,0.07);
                           border:1px solid rgba(255,255,255,0.12);border-radius:8px;padding:6px 12px;",
                  tags$i(class="fas fa-shield-halved", style="color:#C084FC;font-size:11px;"),
                  tags$div(
                    tags$div(style="font-size:14px;font-weight:800;color:#F8FAFC;line-height:1;", "FHFA"),
                    tags$div(style="font-size:9px;color:#94A3B8;text-transform:uppercase;letter-spacing:.6px;", "/ CFPB Survey")
                  )
                )
              )
            ),

            # RIGHT: Illustrated mortgage scene (SVG)
            tags$div(
              style = "width:260px;flex-shrink:0;display:flex;align-items:center;
                       justify-content:center;padding:20px;",
              HTML('
<svg viewBox="0 0 220 200" xmlns="http://www.w3.org/2000/svg" width="220" height="200">
  <defs>
    <linearGradient id="houseGrad" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#2563EB" stop-opacity="0.9"/>
      <stop offset="100%" stop-color="#0EA5E9" stop-opacity="0.7"/>
    </linearGradient>
    <linearGradient id="roofGrad" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#1D4ED8"/>
      <stop offset="100%" stop-color="#3B82F6"/>
    </linearGradient>
    <linearGradient id="docGrad" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="#F8FAFC"/>
      <stop offset="100%" stop-color="#E2E8F0"/>
    </linearGradient>
  </defs>

  <!-- House body -->
  <rect x="55" y="95" width="110" height="80" rx="4" fill="url(#houseGrad)" opacity="0.9"/>
  <!-- Roof -->
  <polygon points="45,98 110,45 175,98" fill="url(#roofGrad)"/>
  <!-- Door -->
  <rect x="96" y="135" width="28" height="40" rx="3" fill="#0F172A" opacity="0.5"/>
  <circle cx="120" cy="157" r="2.5" fill="#F59E0B"/>
  <!-- Windows -->
  <rect x="63" y="108" width="22" height="18" rx="3" fill="rgba(255,255,255,0.25)"/>
  <rect x="135" y="108" width="22" height="18" rx="3" fill="rgba(255,255,255,0.25)"/>
  <!-- Window cross hairs -->
  <line x1="74" y1="108" x2="74" y2="126" stroke="rgba(255,255,255,0.4)" stroke-width="1"/>
  <line x1="63" y1="117" x2="85" y2="117" stroke="rgba(255,255,255,0.4)" stroke-width="1"/>
  <line x1="146" y1="108" x2="146" y2="126" stroke="rgba(255,255,255,0.4)" stroke-width="1"/>
  <line x1="135" y1="117" x2="157" y2="117" stroke="rgba(255,255,255,0.4)" stroke-width="1"/>

  <!-- Document / chart floating right -->
  <rect x="148" y="55" width="52" height="64" rx="5" fill="url(#docGrad)" opacity="0.92"/>
  <rect x="155" y="64" width="38" height="3" rx="1.5" fill="#2563EB" opacity="0.6"/>
  <rect x="155" y="71" width="28" height="2" rx="1" fill="#94A3B8" opacity="0.5"/>
  <rect x="155" y="77" width="32" height="2" rx="1" fill="#94A3B8" opacity="0.5"/>
  <!-- Mini bar chart in doc -->
  <rect x="155" y="88" width="6" height="16" rx="1" fill="#2563EB" opacity="0.7"/>
  <rect x="163" y="96" width="6" height="8" rx="1" fill="#0EA5E9" opacity="0.7"/>
  <rect x="171" y="92" width="6" height="12" rx="1" fill="#16A34A" opacity="0.7"/>
  <rect x="179" y="84" width="6" height="20" rx="1" fill="#7C3AED" opacity="0.7"/>

  <!-- Coin / dollar badge -->
  <circle cx="42" cy="65" r="20" fill="#F59E0B" opacity="0.9"/>
  <circle cx="42" cy="65" r="15" fill="#FBBF24" opacity="0.6"/>
  <text x="42" y="70" text-anchor="middle" font-size="14" font-weight="900" fill="white" font-family="sans-serif">$</text>

  <!-- Stars / sparkles -->
  <circle cx="28" cy="110" r="2.5" fill="#60A5FA" opacity="0.6"/>
  <circle cx="192" cy="40" r="2" fill="#34D399" opacity="0.7"/>
  <circle cx="185" cy="155" r="3" fill="#F59E0B" opacity="0.5"/>
  <circle cx="18" cy="145" r="2" fill="#C084FC" opacity="0.6"/>

  <!-- Ground line -->
  <line x1="30" y1="175" x2="190" y2="175" stroke="rgba(255,255,255,0.12)" stroke-width="1.5"/>

  <!-- CU vs non-CU label badges -->
  <rect x="20" y="182" width="40" height="14" rx="7" fill="#2563EB" opacity="0.8"/>
  <text x="40" y="192" text-anchor="middle" font-size="8" font-weight="700" fill="white" font-family="sans-serif">CU</text>
  <rect x="68" y="182" width="52" height="14" rx="7" fill="#DC2626" opacity="0.8"/>
  <text x="94" y="192" text-anchor="middle" font-size="8" font-weight="700" fill="white" font-family="sans-serif">Non-CU</text>
</svg>
              ')
            )
          )
        ),

        # ── Filter bar ──────────────────────────────────────────────────────────
        tags$div(class = "filter-bar",
          fluidRow(
            box(title = tags$span(icon("sliders"), " Filters"), width = 12,
                status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              fluidRow(
                column(2, selectizeInput("sum_fico",       "FICO Category:", choices = fico_order,
                  selected = NULL, multiple = TRUE, options = list(placeholder = "All FICO…"))),
                column(2, selectizeInput("sum_period",     "COVID Period:",  choices = period_order,
                  selected = NULL, multiple = TRUE, options = list(placeholder = "All periods…"))),
                column(1, selectizeInput("sum_first_mort", "First Time:",
                  choices = c("First Mortgage","Not First"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(1, selectizeInput("sum_loan_type",  "Loan Type:",
                  choices = c("Conventional","FHA/VA/FSA"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(1, selectizeInput("sum_term",       "Term:",
                  choices = c("30 Years","Other"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(1, selectizeInput("sum_cashout",    "Purpose:",
                  choices = c("Refinance","Purchase"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(2, selectizeInput("sum_race",       "Race:",
                  choices = c("White","Black","Asian","Other"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(1, selectizeInput("sum_gender",     "Gender:",
                  choices = c("Male","Female"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…"))),
                column(1, selectizeInput("sum_jumbo",      "Jumbo:",
                  choices = c("Jumbo","Non-Jumbo"), selected = NULL, multiple = TRUE,
                  options = list(placeholder = "All…")))
              )
            )
          )
        ),

        # ── Row 1: Elegant custom KPI scorecard ─────────────────────────────────
        fluidRow(
          column(12, uiOutput("sum_kpi_scorecard"))
        ),

        tags$div(style = "height:8px;"),

        # ── Row 2: Key Metrics comparison + FICO side-by-side ───────────────────
        fluidRow(
          box(
            title = tags$span(icon("chart-bar"), " Key Financial Metrics: CU vs Non-CU"),
            width = 8, status = "primary", solidHeader = TRUE,
            tags$div(style = "font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Each metric shown on its own scale — bars are not cross-comparable"),
            plotlyOutput("sum_metrics_compare", height = 340)
          ),
          box(
            title = tags$span(icon("layer-group"), " FICO Score Distribution (%)"),
            width = 4, status = "info", solidHeader = TRUE,
            plotlyOutput("sum_fico_grouped", height = 340)
          )
        ),

        # ── Row 3: Demographics ─────────────────────────────────────────────────
        fluidRow(
          box(title = tags$span(icon("venus-mars"), " Gender Distribution (%)"),
              width = 4, status = "info", solidHeader = TRUE,
              plotlyOutput("sum_sex_pie", height = 300)),
          box(title = tags$span(icon("users"), " Race Composition (%)"),
              width = 4, status = "info", solidHeader = TRUE,
              plotlyOutput("sum_race_donut", height = 300)),
          box(title = tags$span(icon("chart-line"), " Loan Purpose & Type"),
              width = 4, status = "info", solidHeader = TRUE,
              plotlyOutput("sum_purpose_type", height = 300))
        ),

        # ── Row 4: Satisfaction + LTV distribution ──────────────────────────────
        fluidRow(
          box(title = tags$span(icon("star"), " Survey Satisfaction Overview"),
              width = 6, status = "success", solidHeader = TRUE,
              plotlyOutput("sum_satisfaction_radar", height = 300)),
          box(title = tags$span(icon("chart-area"), " LTV Distribution by Institution"),
              width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("sum_ltv_dist", height = 300))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 2 — SURVEY RESPONSES
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "responses",

        tags$div(class = "v2-page-header",
          tags$i(class = "fas fa-comments fa-lg", style = "color:#0EA5E9"),
          tags$h2("Survey Response Trend Analysis"),
          tags$span(class = "badge", style = "background:#EFF6FF;color:#0EA5E9;", "Weighted")
        ),

        tags$div(class = "filter-bar",
          fluidRow(
            box(title = tags$span(icon("sliders"), " Filters"), width = 12,
                status = "info", solidHeader = TRUE, collapsible = TRUE,
              # ── Row 1: two linked variable selectors ──────────────────────────────
              fluidRow(
                column(2,
                  tags$div(
                    tags$label("Variable Code:",
                      style = "font-size:11px;font-weight:600;color:#475569;text-transform:uppercase;letter-spacing:.5px;margin-bottom:3px;display:block;"),
                    tags$div(style = "font-size:10px;color:#94A3B8;margin-bottom:4px;", "Select by code (e.g. X08A)"),
                    selectInput("trend_var_code", NULL,
                      choices  = response_choices_code,
                      selected = response_vars[1],
                      width    = "100%")
                  )
                ),
                column(4,
                  tags$div(
                    tags$label("Response Variable:",
                      style = "font-size:11px;font-weight:600;color:#475569;text-transform:uppercase;letter-spacing:.5px;margin-bottom:3px;display:block;"),
                    tags$div(style = "font-size:10px;color:#94A3B8;margin-bottom:4px;", "Select by description — syncs with code picker"),
                    selectInput("trend_var", NULL,
                      choices  = response_choices,
                      selected = response_vars[1],
                      width    = "100%")
                  )
                ),
                column(1, tags$div(style="margin-top:28px;text-align:center;color:#94A3B8;font-size:18px;", "⇄"))
              ),
              # ── Row 2: demographic filters ─────────────────────────────────────────
              fluidRow(
                column(1, selectizeInput("trend_fico",       "FICO:",     choices = fico_order, selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(1, selectizeInput("trend_period",     "COVID:",    choices = period_order, selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(2, selectizeInput("trend_first_mort", "1st Mort:", choices = c("First Mortgage","Not First"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(2, selectizeInput("trend_loan_type",  "Loan Type:",choices = c("Conventional","FHA/VA/FSA"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(1, selectizeInput("trend_term",       "Term:",     choices = c("30 Years","Other"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(1, selectizeInput("trend_cashout",    "Purpose:",  choices = c("Refinance","Purchase"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(2, selectizeInput("trend_race",       "Race:",     choices = c("White","Black","Asian","Other"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
                column(2, selectizeInput("trend_gender",     "Gender:",   choices = c("Male","Female"), selected = NULL, multiple = TRUE, options = list(placeholder = "All…")))
              )
            )
          )
        ),

        fluidRow(
          box(title = tags$span(icon("chart-line"), " Yearly Trend"),   width = 6, status = "info", solidHeader = TRUE, plotlyOutput("trend_yearly",      height = 320)),
          box(title = tags$span(icon("arrows-up-down"), " Year-over-Year Change"), width = 6, status = "info", solidHeader = TRUE, plotlyOutput("trend_yoy_change", height = 320))
        ),
        fluidRow(
          box(title = tags$span(icon("balance-scale"), " Net Difference by Year"), width = 6, status = "success", solidHeader = TRUE, plotlyOutput("trend_net_diff",   height = 320)),
          box(title = tags$span(icon("calendar-alt"), " By COVID Period"),         width = 6, status = "warning", solidHeader = TRUE, plotlyOutput("trend_by_period",  height = 320))
        ),
        fluidRow(
          box(title = tags$span(icon("star"), " Net Difference by Period (* = Significant)"),
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("trend_net_by_period", height = 300))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 3 — ML MODEL
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "ml_cu",

        tags$div(class = "v2-page-header",
          tags$i(class = "fas fa-magnifying-glass-chart fa-lg", style = "color:#7C3AED"),
          tags$h2("What Predicts Borrower Outcomes?"),
          tags$span(class = "badge", style = "background:#F5F3FF;color:#7C3AED;", "Predictive Analysis"),
          tags$span(class = "badge", style = "background:#EFF6FF;color:#2563EB;margin-left:4px;", "CU vs Non-CU")
        ),

        # ── Top shimmer bar (spans full width, shown during model run) ──────
        tags$div(id = "ml-loading-bar"),

        # ── ML animation script — lives INSIDE the tab so DOM is ready ─────
        tags$script(HTML('
(function() {
  var msgs = [
    "Preparing outcome variable...",
    "Training Credit Union model...",
    "Training Non-Credit Union model...",
    "Running adjusted combined model...",
    "Computing metrics..."
  ];
  var timer = null, step = 0, running = false;

  function q(id) { return document.getElementById(id); }

  function startAnim() {
    if (running) return;
    running = true; step = 0;

    /* shimmer bar */
    var bar = q("ml-loading-bar");
    if (bar) bar.style.display = "block";

    /* overlay */
    var ov = q("ml-loading-overlay");
    if (ov) ov.style.display = "flex";

    /* button */
    var btn = q("run_ml_cu");
    if (btn) {
      btn.disabled = true;
      btn.style.cssText = "background:linear-gradient(90deg,#6D28D9,#2563EB,#6D28D9);background-size:200% 100%;animation:btn-gradient 1.8s ease infinite;border-color:transparent;color:white;border-radius:7px;padding:7px 18px;font-size:13px;font-weight:600;";
      btn.innerHTML = "Running...";
    }

    advance();
  }

  function advance() {
    if (!running) return;
    var ids = ["step-1","step-2","step-3","step-4","step-5"];
    if (step > 0) {
      var p = q(ids[step-1]);
      if (p) { p.classList.remove("active"); p.classList.add("done"); }
    }
    if (step < ids.length) {
      var c = q(ids[step]);
      if (c) c.classList.add("active");
      var lbl = q("ml-step-label");
      if (lbl) lbl.textContent = msgs[step];
      step++;
      timer = setTimeout(advance, [1800,2500,3200,2000,1500][step-1] || 2000);
    }
  }

  function stopAnim() {
    if (!running && step === 0) return;
    clearTimeout(timer);
    running = false; step = 0;

    var bar = q("ml-loading-bar");
    if (bar) bar.style.display = "none";
    var ov = q("ml-loading-overlay");
    if (ov) ov.style.display = "none";

    var btn = q("run_ml_cu");
    if (btn) {
      btn.disabled = false;
      btn.style.cssText = "";
      btn.innerHTML = "&#9654; Run Model";
    }

    for (var i=1; i<=5; i++) {
      var s = q("step-"+i);
      if (s) s.classList.remove("active","done");
    }
    var lbl = q("ml-step-label");
    if (lbl) lbl.textContent = "Preparing data...";
  }

  /* --- event wiring --- */

  /* click on Run Model button */
  $(document).on("click", "#run_ml_cu", function() {
    setTimeout(startAnim, 30);
    /* safety timeout 60s */
    clearTimeout(window._mlSafe);
    window._mlSafe = setTimeout(stopAnim, 60000);
  });

  /* click on Reset button */
  $(document).on("click", "#reset_ml_cu", function() {
    stopAnim();
  });

  /* Shiny tells us the output has been updated */
  $(document).on("shiny:value", function(evt) {
    if (evt.name === "ml_cu_performance") {
      setTimeout(stopAnim, 350);
    }
  });

  /* belt-and-suspenders: shiny:inputchanged */
  $(document).on("shiny:inputchanged", function(evt) {
    if (evt.name === "run_ml_cu")   setTimeout(startAnim, 30);
    if (evt.name === "reset_ml_cu") stopAnim();
  });

})();
        ')),

        fluidRow(
          box(title = tags$span(icon("gear"), " Model Configuration"),
              width = 4, status = "primary", solidHeader = TRUE,

            # Category banner — updates reactively when outcome changes
            uiOutput("ml_outcome_banner"),

            tags$label("Outcome Variable:",
              style = "font-weight:600;font-size:11px;color:#475569;text-transform:uppercase;
                        letter-spacing:.5px;display:block;margin-bottom:4px;"),
            selectInput("ml_cu_outcome", NULL,
              choices = list(
                "── Overall Satisfaction ──────────────" = c(
                  "Overall Lender Satisfaction"             = "sat_overall",
                  "Got Best-Fit Mortgage Terms"             = "best_deal",
                  "Got Lowest Rate Available"               = "lowest_rate"
                ),
                "── Process Satisfaction ──────────────" = c(
                  "Satisfaction: Application Process"       = "sat_application",
                  "Satisfaction: Loan Closing"              = "sat_closing",
                  "Satisfaction: Disclosure Documents"      = "sat_disclosure",
                  "Satisfaction: Disclosure Timeliness"     = "sat_disclosure_timing",
                  "Closing Costs Matched Expectations"      = "costs_as_expected"
                ),
                "── Closing Problems ──────────────────" = c(
                  "Faced Unexpected Terms at Closing"       = "closing_surprise",
                  "Felt Rushed at Loan Closing"             = "closing_rushed",
                  "Closing Costs Rolled Into Loan"          = "costs_rolled"
                ),
                "── Consumer Engagement & Shopping ───" = c(
                  "Applied to Multiple Lenders"             = "shopped_multiple",
                  "Number of Lenders Considered"            = "lenders_considered",
                  "Got Pre-Approval Before Offer"           = "preapproval",
                  "Verified Terms With Outside Source"      = "verified_terms",
                  "Took Home-Buying Course / Counseling"    = "counseling"
                ),
                "── Information & Disclosure ──────────" = c(
                  "Loan Estimate Was Easy to Understand"    = "le_clear",
                  "Received Home Loan Toolkit Booklet"      = "toolkit"
                ),
                "── Product Complexity ────────────────" = c(
                  "Has Adjustable-Rate Mortgage"            = "has_arm",
                  "Has Private Mortgage Insurance (PMI)"    = "has_pmi",
                  "Rate-Points Tradeoff Decision Made"      = "points_tradeoff"
                ),
                "── Financial Wellbeing & Literacy ───" = c(
                  "Financial Literacy: Mortgage Process"    = "financial_literacy",
                  "Fin. Literacy: Fixed vs Adjustable Rate" = "literacy_arm",
                  "Likelihood of Future Payment Difficulty" = "payment_difficulty",
                  "Believes Lenders Treat Borrowers Fairly" = "lender_trust"
                ),
                "── Financial Outcome ─────────────────" = c(
                  "Rate Spread Above Market (%)"            = "rate_spread"
                )
              ),
              selected = "sat_overall"),
            hr(class = "v2"),
            tags$label("COVID Period Filter:",
              style = "font-weight:600;font-size:11px;color:#475569;text-transform:uppercase;letter-spacing:.5px;"),
            checkboxGroupInput("ml_cu_period", NULL,
              choices = period_order, selected = period_order, inline = FALSE),
            hr(class = "v2"),
            fluidRow(
              column(6, actionButton("run_ml_cu",   "▶ Run Model", class = "btn btn-run btn-block")),
              column(6, actionButton("reset_ml_cu", "↺ Reset",     class = "btn btn-reset btn-block"))
            ),
            tags$div(class = "sig-note", style = "margin-top:10px;font-size:10px;",
              "All 3 periods → COVID used as feature. Subset → data filtered.",
              tags$br(),
              tags$span(style="color:#7C3AED;", "●"), " Binary: % positive  ",
              tags$span(style="color:#334155;", "●"), " Continuous: mean value")
          ),

          box(title = tags$span(icon("chart-pie"), " How Well Does the Model Predict Outcomes?"),
              width = 8, status = "primary", solidHeader = TRUE,
            tags$div(class = "ml-perf-wrap",
              # Loading overlay — shown while model runs
              tags$div(id = "ml-loading-overlay",
                tags$div(class = "ml-spinner",
                  tags$i(class = "fas fa-brain")),
                tags$div(class = "ml-loading-text",
                  tags$span(id = "ml-step-label", "Preparing data…")),
                tags$div(class = "ml-steps",
                  tags$div(class = "ml-step", id = "step-1",
                    tags$div(class = "step-dot"), "Data"),
                  tags$span(style="color:#E2E8F0;font-size:10px;", "→"),
                  tags$div(class = "ml-step", id = "step-2",
                    tags$div(class = "step-dot"), "CU Model"),
                  tags$span(style="color:#E2E8F0;font-size:10px;", "→"),
                  tags$div(class = "ml-step", id = "step-3",
                    tags$div(class = "step-dot"), "Non-CU Model"),
                  tags$span(style="color:#E2E8F0;font-size:10px;", "→"),
                  tags$div(class = "ml-step", id = "step-4",
                    tags$div(class = "step-dot"), "Adjusted"),
                  tags$span(style="color:#E2E8F0;font-size:10px;", "→"),
                  tags$div(class = "ml-step", id = "step-5",
                    tags$div(class = "step-dot"), "Done")
                ),
                tags$div(class = "ml-loading-subtext",
                  "Analyzing borrower outcome patterns… typically 5–15 seconds")
              ),
              # Actual results
              uiOutput("ml_cu_performance")
            ))
        ),

        fluidRow(
          box(title = tags$span(
                tags$span(style="color:#2563EB;font-weight:700;","●"), " Credit Union — Top Factors Driving Outcomes"),
              width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("ml_cu_importance_cu", height = 360)),
          box(title = tags$span(
                tags$span(style="color:#DC2626;font-weight:700;","●"), " Non-Credit Union — Top Factors Driving Outcomes"),
              width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("ml_cu_importance_noncu", height = 360))
        ),

        fluidRow(
          box(title = tags$span(icon("lightbulb"), " What the Analysis Found"),
              width = 12, status = "success", solidHeader = TRUE,
              uiOutput("ml_cu_interpretation"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 4 — REGRESSION ANALYSIS
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "regression",

        tags$div(class = "v2-page-header",
          tags$i(class = "fas fa-chart-line fa-lg", style = "color:#D97706"),
          tags$h2("Regression Analysis"),
          tags$span(class = "badge", style = "background:#FFFBEB;color:#D97706;", "OLS / Logistic")
        ),

        # ── Shimmer bar ──────────────────────────────────────────────────────
        tags$div(id = "reg-loading-bar"),

        # ── Regression animation JS ─────────────────────────────────────────
        tags$script(HTML('
(function() {
  function q(id) { return document.getElementById(id); }
  var regRunning = false, regTimer = null;
  function regStart() {
    if (regRunning) return; regRunning = true;
    var bar = q("reg-loading-bar");
    if (bar) bar.style.display = "block";
    var btn = q("run_regression");
    if (btn) {
      btn.disabled = true;
      btn.style.cssText = "background:linear-gradient(90deg,#B45309,#D97706,#B45309);background-size:200% 100%;animation:btn-gradient 1.8s ease infinite;border-color:transparent;color:white;border-radius:7px;padding:7px 18px;font-size:13px;font-weight:600;";
      btn.innerHTML = "Running...";
    }
    regTimer = setTimeout(regStop, 60000);
  }
  function regStop() {
    if (!regRunning) return; regRunning = false;
    clearTimeout(regTimer);
    var bar = q("reg-loading-bar");
    if (bar) bar.style.display = "none";
    var btn = q("run_regression");
    if (btn) {
      btn.disabled = false;
      btn.style.cssText = "";
      btn.innerHTML = "&#9654; Run Regression";
    }
  }
  $(document).on("click","#run_regression", function(){ setTimeout(regStart,30); });
  $(document).on("click","#reset_regression", function(){ regStop(); });
  $(document).on("shiny:value", function(e){
    if (e.name === "reg_summary_ui") setTimeout(regStop, 350);
  });
  $(document).on("shiny:inputchanged", function(e){
    if (e.name === "run_regression") setTimeout(regStart,30);
    if (e.name === "reset_regression") regStop();
  });
})();
        ')),

        # ── Row 1: Configuration (left) + Model Summary (right) ─────────────
        fluidRow(

          # ── LEFT: Outcome + Predictors ─────────────────────────────────────
          box(title = tags$span(icon("gear"), " Model Configuration"),
              width = 4, status = "warning", solidHeader = TRUE,

            # Outcome banner
            uiOutput("reg_outcome_banner"),

            # Outcome variable (grouped dropdown matching ML tab quality)
            tags$label("Outcome Variable:",
              style = "font-weight:600;font-size:11px;color:#475569;text-transform:uppercase;
                        letter-spacing:.5px;display:block;margin-bottom:4px;"),
            selectInput("reg_outcome", NULL,
              choices = list(
                "── Satisfaction & Experience ──────────" = c(
                  "Overall Lender Satisfaction"          = "reg_sat_overall",
                  "Got Best-Fit Mortgage Terms"          = "reg_best_deal",
                  "Got Lowest Rate Available"            = "reg_lowest_rate",
                  "Satisfaction: Application Process"    = "reg_sat_application",
                  "Satisfaction: Loan Closing"           = "reg_sat_closing",
                  "Satisfaction: Disclosure Documents"   = "reg_sat_disclosure"
                ),
                "── Closing & Transparency ─────────────" = c(
                  "Closing Costs Matched Expectations"   = "reg_costs_expected",
                  "Faced Unexpected Terms at Closing"    = "reg_closing_surprise",
                  "Felt Rushed at Loan Closing"          = "reg_closing_rushed",
                  "Closing Costs Rolled Into Loan"       = "reg_costs_rolled"
                ),
                "── Consumer Behavior & Engagement ────" = c(
                  "Applied to Multiple Lenders"          = "reg_shopped",
                  "Considered Multiple Lenders"          = "reg_considered",
                  "Got Pre-Approval Before Offer"        = "reg_preapproval",
                  "Verified Terms Externally"            = "reg_verified",
                  "Took Homebuying Course/Counseling"    = "reg_counseling"
                ),
                "── Disclosure & Financial Literacy ───" = c(
                  "Loan Estimate Was Clear"              = "reg_le_clear",
                  "Received Home Loan Toolkit"           = "reg_toolkit",
                  "Financial Literacy: Mortgage Process" = "reg_lit_mortgage",
                  "Financial Literacy: Fixed vs ARM"     = "reg_lit_arm"
                ),
                "── Product & Risk ─────────────────────" = c(
                  "Has Adjustable-Rate Mortgage"         = "reg_has_arm",
                  "Has PMI"                              = "reg_has_pmi",
                  "Made Rate-Points Tradeoff Decision"   = "reg_points"
                ),
                "── Financial Outcomes (Continuous) ───" = c(
                  "Rate Spread Above Market (%)"         = "reg_rate_spread",
                  "Mortgage Interest Rate (%)"           = "reg_interest_rate"
                ),
                "── Financial Wellbeing ────────────────" = c(
                  "Likelihood of Payment Difficulty"     = "reg_payment_diff",
                  "Believes Lenders Treat Borrowers Fairly" = "reg_lender_trust"
                )
              ),
              selected = "reg_rate_spread"),

            hr(class = "v2"),

            # Always-included: Institution Type note
            tags$div(
              style = "background:#EFF6FF;border-left:3px solid #2563EB;border-radius:6px;
                       padding:8px 12px;margin-bottom:10px;font-size:11px;color:#1D4ED8;",
              tags$i(class = "fas fa-building-columns", style = "margin-right:6px;"),
              tags$strong("Institution Type (CU vs Non-CU)"),
              " is always included as a predictor."
            ),

            # Predictor groups
            tags$label("Control Variables to Include:",
              style = "font-weight:600;font-size:11px;color:#475569;text-transform:uppercase;
                        letter-spacing:.5px;display:block;margin-bottom:6px;"),

            tags$div(style = "font-size:11px;font-weight:600;color:#64748B;margin-bottom:3px;",
              tags$i(class="fas fa-user", style="margin-right:5px;color:#7C3AED;"),
              "Borrower Characteristics"),
            checkboxGroupInput("reg_pred_borrower", NULL,
              choices = c(
                "FICO Credit Score"     = "Score_Origin",
                "Borrower Age"          = "Age",
                "Gender"                = "Sex_Label",
                "Race / Ethnicity"      = "Race",
                "First-Time Homebuyer"  = "First_Mortgage_Label"
              ),
              selected = c("Score_Origin", "Age")),

            tags$div(style = "font-size:11px;font-weight:600;color:#64748B;margin:8px 0 3px 0;",
              tags$i(class="fas fa-house", style="margin-right:5px;color:#D97706;"),
              "Loan Characteristics"),
            checkboxGroupInput("reg_pred_loan", NULL,
              choices = c(
                "Loan Amount (Category)"  = "loan_amount_cat",
                "Loan Type (Conv/FHA)"    = "Loan_Type",
                "Loan Purpose (Purchase/Refi)" = "Cashout_Label",
                "Loan Term (30yr/Other)"  = "Term_Label"
              ),
              selected = c("loan_amount_cat", "Loan_Type")),

            tags$div(style = "font-size:11px;font-weight:600;color:#64748B;margin:8px 0 3px 0;",
              tags$i(class="fas fa-calculator", style="margin-right:5px;color:#16A34A;"),
              "Financial Variables"),
            checkboxGroupInput("reg_pred_financial", NULL,
              choices = c(
                "LTV Ratio"           = "ltv",
                "DTI Ratio"           = "dti",
                "Interest Rate (%)"   = "Interest_Rate"
              ),
              selected = c("ltv", "dti")),

            tags$div(style = "font-size:11px;font-weight:600;color:#64748B;margin:8px 0 3px 0;",
              tags$i(class="fas fa-calendar", style="margin-right:5px;color:#0EA5E9;"),
              "Time Controls"),
            checkboxGroupInput("reg_pred_time", NULL,
              choices = c(
                "COVID Period Dummies"   = "covid_dummy",
                "Survey Year"           = "open_year"
              ),
              selected = "covid_dummy"),

            hr(class = "v2"),

            # Sample filters
            tags$label("Sample Filters:",
              style = "font-weight:600;font-size:11px;color:#475569;text-transform:uppercase;
                        letter-spacing:.5px;display:block;margin-bottom:6px;"),
            selectizeInput("reg_period_filter", "COVID Period:",
              choices = period_order, selected = NULL, multiple = TRUE,
              options = list(placeholder = "All periods…")),
            selectizeInput("reg_fico_filter", "FICO Category:",
              choices = fico_order, selected = NULL, multiple = TRUE,
              options = list(placeholder = "All FICO…")),
            selectizeInput("reg_race_filter", "Race:",
              choices = c("White","Black","Asian","Other"),
              selected = NULL, multiple = TRUE,
              options = list(placeholder = "All races…")),
            selectizeInput("reg_purpose_filter", "Loan Purpose:",
              choices = c("Purchase","Refinance"),
              selected = NULL, multiple = TRUE,
              options = list(placeholder = "All…")),

            hr(class = "v2"),
            fluidRow(
              column(7, actionButton("run_regression",   "▶ Run Regression",
                class = "btn btn-run btn-block")),
              column(5, actionButton("reset_regression", "↺ Reset",
                class = "btn btn-reset btn-block"))
            ),
            tags$div(class = "sig-note", style = "margin-top:10px;font-size:10px;",
              tags$span(style="color:#D97706;", "●"), " Continuous → OLS  ",
              tags$span(style="color:#7C3AED;", "●"), " Binary → Logistic (log-odds)")
          ),

          # ── RIGHT: Model Summary ───────────────────────────────────────────
          box(title = tags$span(icon("table"), " Model Summary & Fit Statistics"),
              width = 8, status = "warning", solidHeader = TRUE,
            uiOutput("reg_summary_ui")
          )
        ),

        # ── Row 2: Coefficient plot (main) + Interpretation ──────────────────
        fluidRow(
          box(title = tags$span(icon("chart-bar"), " Coefficient Plot — Effect Sizes with 95% CI"),
              width = 8, status = "warning", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Each bar = estimated effect; whiskers = 95% CI; dashed line = zero. Green = positive, Red = negative."),
            plotlyOutput("reg_coef_plot", height = 440)),
          box(title = tags$span(icon("lightbulb"), " What the Analysis Found"),
              width = 4, status = "success", solidHeader = TRUE,
            uiOutput("reg_interpretation"))
        ),

        # ── Row 3: Diagnostics (OLS only) + Marginal effects ─────────────────
        fluidRow(
          box(title = uiOutput("reg_diag_title"),
              width = 6, status = "info", solidHeader = TRUE,
            uiOutput("reg_diag_subtitle"),
            plotlyOutput("reg_residual_plot", height = 300)),
          box(title = tags$span(icon("chart-area"), " CU vs Non-CU: Predicted Outcome by Subgroup"),
              width = 6, status = "info", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Model-predicted outcome means by institution type across FICO tiers."),
            plotlyOutput("reg_predicted_by_fico", height = 300))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 5 — POLICY REPORT
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "report",

        # ── Report header (CFPB Data Point style) ────────────────────────────
        tags$div(
          style = "background:linear-gradient(135deg,#0F172A 0%,#1E3A5F 100%);
                   border-radius:12px;padding:28px 32px;margin-bottom:20px;
                   color:white;position:relative;overflow:hidden;",
          tags$div(
            style = "position:absolute;right:-20px;top:-20px;opacity:.06;font-size:180px;
                     line-height:1;font-family:serif;pointer-events:none;",
            "§"),
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;gap:16px;flex-wrap:wrap;",
            tags$div(
              tags$div(style="font-size:11px;font-weight:700;letter-spacing:2px;
                              color:#93C5FD;text-transform:uppercase;margin-bottom:6px;",
                "NSMO ANALYSIS  ·  CREDIT UNION MORTGAGE OUTCOMES"),
              tags$h2(style="margin:0 0 6px 0;font-size:24px;font-weight:800;color:white;line-height:1.2;",
                "Credit Union vs. Non-Credit Union"),
              tags$h3(style="margin:0 0 10px 0;font-size:16px;font-weight:400;color:#CBD5E1;",
                "Mortgage Origination Outcomes: Rates, Satisfaction, Transparency & Consumer Equity"),
              tags$div(style="display:flex;gap:10px;flex-wrap:wrap;",
                tags$span(style="background:rgba(255,255,255,.12);border-radius:20px;
                                 padding:3px 12px;font-size:11px;color:#E2E8F0;",
                  icon("database"), " NSMO Public Use File · 2013–2023"),
                tags$span(style="background:rgba(255,255,255,.12);border-radius:20px;
                                 padding:3px 12px;font-size:11px;color:#E2E8F0;",
                  icon("users"), " 58,381 Mortgage Originations"),
                tags$span(style="background:rgba(255,255,255,.12);border-radius:20px;
                                 padding:3px 12px;font-size:11px;color:#E2E8F0;",
                  icon("building-columns"), " FHFA / CFPB Joint Survey")
              )
            ),
            tags$div(style="display:flex;flex-direction:column;gap:8px;min-width:190px;",
              # Auto-update badge
              tags$div(
                style="display:flex;align-items:center;gap:6px;background:rgba(22,163,74,0.18);
                       border:1px solid rgba(22,163,74,0.35);border-radius:8px;
                       padding:8px 14px;font-size:11px;font-weight:600;color:#4ADE80;",
                tags$i(class="fas fa-bolt", style="font-size:10px;"),
                "Filters update instantly"
              ),
              downloadButton("rpt_download_pdf", "↓ Download Report (HTML)",
                style = "background:#16A34A;color:white;border:none;
                         border-radius:8px;padding:9px 18px;font-size:12px;font-weight:600;width:100%;"),
              uiOutput("rpt_pdf_engine_status"),
              downloadButton("rpt_download", "↓ Download Data (CSV)",
                style = "background:rgba(255,255,255,.12);color:white;border:1px solid rgba(255,255,255,.3);
                         border-radius:8px;padding:9px 18px;font-size:12px;font-weight:500;width:100%;"),
              # Reset all filters button
              actionButton("rpt_reset", "× Clear All Filters",
                style = "background:rgba(255,255,255,.07);color:#94A3B8;border:1px solid rgba(255,255,255,.15);
                         border-radius:8px;padding:7px 14px;font-size:11px;font-weight:500;
                         cursor:pointer;width:100%;")
            )
          )
        ),

        # ── Filters bar ───────────────────────────────────────────────────────
        tags$div(
          style = "background:#F8FAFC;border:1px solid #E2E8F0;border-radius:10px;
                   padding:14px 18px;margin-bottom:16px;",
          tags$div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
            tags$div(style="font-size:11px;font-weight:700;color:#475569;text-transform:uppercase;
                            letter-spacing:.6px;",
              icon("sliders"), " Filter Sample"),
            tags$div(style="font-size:10px;color:#16A34A;font-weight:600;display:flex;align-items:center;gap:4px;",
              tags$i(class="fas fa-circle-dot", style="font-size:8px;"),
              "Results update automatically as you filter")
          ),
          fluidRow(
            column(2, selectizeInput("rpt_fico",     "FICO Tier:",    choices = fico_order,
              selected = NULL, multiple = TRUE, options = list(placeholder = "All tiers…"))),
            column(2, selectizeInput("rpt_period",   "COVID Period:", choices = period_order,
              selected = NULL, multiple = TRUE, options = list(placeholder = "All periods…"))),
            column(2, selectizeInput("rpt_race",     "Race:",
              choices = c("White","Black","Asian","Other"),
              selected = NULL, multiple = TRUE, options = list(placeholder = "All races…"))),
            column(2, selectizeInput("rpt_loan_type","Loan Type:",
              choices = c("Conventional","FHA/VA/FSA"),
              selected = NULL, multiple = TRUE, options = list(placeholder = "All types…"))),
            column(2, selectizeInput("rpt_gender",   "Gender:",
              choices = c("Male","Female"),
              selected = NULL, multiple = TRUE, options = list(placeholder = "All…"))),
            column(2, selectizeInput("rpt_purpose",  "Loan Purpose:",
              choices = c("Purchase","Refinance"),
              selected = NULL, multiple = TRUE, options = list(placeholder = "All…")))
          )
        ),

        # ── Section 1: Key Findings callout box (CFPB Data Point style) ──────
        fluidRow(
          column(12, uiOutput("rpt_key_findings"))
        ),

        tags$div(style="height:4px;"),

        # ── Section 2: 8-metric KPI scorecard ────────────────────────────────
        fluidRow(column(12, uiOutput("rpt_kpi_scorecard"))),

        tags$div(style="height:4px;"),

        # ── Section 3: Financial outcomes + Satisfaction side-by-side ────────
        fluidRow(
          box(title = tags$span(icon("chart-bar"),
                " Financial Metrics Comparison — CU vs Non-CU"),
              width = 5, status = "primary", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:6px;",
              "Weighted means. *** p<0.001  ** p<0.01  * p<0.05  (weighted t-test)"),
            uiOutput("rpt_metrics_table")),
          box(title = tags$span(icon("star"),
                " Survey Satisfaction — % Positive Response"),
              width = 7, status = "primary", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:6px;",
              "Zoomed x-axis — differences are small but may be meaningful at scale."),
            plotlyOutput("rpt_satisfaction_chart", height = 300))
        ),

        # ── Section 4: Rate spread over time + FICO distribution ─────────────
        fluidRow(
          box(title = tags$span(icon("chart-line"),
                " Rate Spread Above Market — by COVID Period"),
              width = 6, status = "info", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Average basis points above the prevailing PMMS market rate."),
            plotlyOutput("rpt_rate_period", height = 280)),
          box(title = tags$span(icon("layer-group"),
                " FICO Score Distribution — CU vs Non-CU"),
              width = 6, status = "info", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Share of loans within each FICO tier. Controls for credit-risk composition differences."),
            plotlyOutput("rpt_fico_inst", height = 280))
        ),

        # ── Section 5: Demographics + closing experience ──────────────────────
        fluidRow(
          box(title = tags$span(icon("users"),
                " Racial Composition — Who Does Each Institution Serve?"),
              width = 6, status = "warning", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Policy relevance: CU tax-exempt status is partly justified by mission to serve underserved communities."),
            plotlyOutput("rpt_race_chart", height = 280)),
          box(title = tags$span(icon("triangle-exclamation"),
                " Closing Experience — Problem Rates"),
              width = 6, status = "warning", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "% of borrowers reporting each closing problem. Lower = better consumer protection."),
            plotlyOutput("rpt_closing_chart", height = 280))
        ),

        # ── Section 6: Shopping behavior + Consumer empowerment ──────────────
        fluidRow(
          box(title = tags$span(icon("store"),
                " Shopping Behavior & Consumer Engagement"),
              width = 6, status = "success", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Active shopping produces better mortgage terms. Higher = more engaged consumer."),
            plotlyOutput("rpt_shopping_chart", height = 280)),
          box(title = tags$span(icon("graduation-cap"),
                " Financial Literacy & Consumer Empowerment"),
              width = 6, status = "success", solidHeader = TRUE,
            tags$div(style="font-size:11px;color:#94A3B8;margin-bottom:4px;",
              "Self-assessed ability to explain mortgage concepts. Higher = better-informed borrower."),
            plotlyOutput("rpt_literacy_chart", height = 280))
        ),

        # ── Section 7: Policy narrative (full-width) ──────────────────────────
        fluidRow(
          column(12, uiOutput("rpt_policy_narrative"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # TAB 6 — DATA EXPLORER
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "explorer",

        tags$div(class = "v2-page-header",
          tags$i(class = "fas fa-table fa-lg", style = "color:#475569"),
          tags$h2("Data Explorer")
        ),

        tags$div(class = "filter-bar",
          fluidRow(
            box(title = tags$span(icon("sliders"), " Advanced Filters"),
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                column(2, selectizeInput("exp_years", "Origination Years:", choices = available_years,
                  selected = NULL, multiple = TRUE, options = list(placeholder = "All years…"))),
                column(2, selectInput("exp_performance", "Performance:",
                  choices = c("All","Current/Other","60 Days Delinquent","180+ Days Delinquent","Foreclosure/Bankruptcy"), selected = "All")),
                column(2, selectInput("exp_education", "Education:",
                  choices = c("All","Some High School","High School Graduate","Tech School","Some College","College Graduate","Post-Graduate"), selected = "All")),
                column(2, selectInput("exp_age", "Age Category:",
                  choices = c("All","18-24","25-34","35-44","45-54","55-64","65-74","75+"), selected = "All")),
                column(2, selectInput("exp_ltv", "LTV Category:",
                  choices = c("All","≤80%","80-90%","90-95%",">95%"), selected = "All")),
                column(2, selectInput("exp_dti", "DTI Category:",
                  choices = c("All","≤36%","36-43%","43-50%",">50%"), selected = "All"))
              )
            )
          )
        ),

        fluidRow(
          valueBoxOutput("exp_total_loans", width = 3),
          valueBoxOutput("exp_cu_pct",      width = 3),
          valueBoxOutput("exp_avg_fico",    width = 3),
          valueBoxOutput("exp_avg_amount",  width = 3)
        ),

        fluidRow(
          box(title = tags$span(icon("table"), " Filtered Data"),
              width = 12, status = "primary", solidHeader = TRUE,
              DTOutput("exp_data_table"))
        )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage
