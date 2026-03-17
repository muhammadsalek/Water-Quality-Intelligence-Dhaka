







# ╔══════════════════════════════════════════════════════════════════════╗
# ║  AQUALENS v4.0 — Water Quality Intelligence Platform                ║
# ║  Buriganga River, Dhaka                                             ║
# ╚══════════════════════════════════════════════════════════════════════╝

# ── 1. LIBRARIES ────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(readxl)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(viridis)
library(ggthemes)
library(scales)
library(ggsci)
library(FactoMineR)
library(factoextra)
library(heatmaply)
library(janitor)

# ── 2. DATA PROCESSING ──────────────────────────────────────────────────
process_water_data <- function(path) {
  data <- read_excel(path) %>%
    clean_names() %>%
    fill(location, symbol, year, parameter) %>%
    select(-any_of("f"))
  
  month_cols  <- c("j_5","m_7","a_8","m_9","j_10","j_11","a_12","s","o","n","d")
  month_names <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov")
  existing    <- intersect(month_cols, names(data))
  month_names <- month_names[month_cols %in% existing]
  
  if(length(existing) > 0) {
    data <- data %>% rename_with(~month_names, all_of(existing))
  }
  
  data %>%
    mutate(across(all_of(month_names), ~suppressWarnings(as.numeric(as.character(.x))))) %>%
    pivot_longer(cols = all_of(month_names), names_to = "month", values_to = "value") %>%
    mutate(
      date      = suppressWarnings(ymd(paste(year, month, "01"))),
      location  = str_to_title(str_trim(location)),
      location  = case_when(
        str_detect(location, regex("mirpur",    ignore_case=TRUE)) ~ "Mirpur",
        str_detect(location, regex("hazaribag", ignore_case=TRUE)) ~ "Hazaribagh",
        str_detect(location, regex("kamrangir", ignore_case=TRUE)) ~ "Kamrangir Char",
        str_detect(location, regex("sadar",     ignore_case=TRUE)) ~ "Sadarghat",
        TRUE ~ location
      ),
      parameter = factor(
        parameter,
        levels = c("pH","DO","BOD","COD","TDS","Trurbidity","Cloride","SS","T-Alkainity","EC"),
        labels = c("pH","Dissolved Oxygen (mg/L)","BOD (mg/L)","COD (mg/L)",
                   "TDS (mg/L)","Turbidity (NTU)","Chloride (mg/L)",
                   "Suspended Solids (mg/L)","Total Alkalinity (mg/L)","EC (uS/cm)")
      )
    ) %>%
    drop_na(value) %>%
    select(location, symbol, parameter, date, value)
}

# ── 3. WQI ──────────────────────────────────────────────────────────────
wqi_params <- data.frame(
  parameter = c("pH","Dissolved Oxygen (mg/L)","BOD (mg/L)","COD (mg/L)","TDS (mg/L)",
                "Turbidity (NTU)","Chloride (mg/L)","Suspended Solids (mg/L)",
                "Total Alkalinity (mg/L)","EC (uS/cm)"),
  weight    = c(0.15,0.25,0.20,0.10,0.10,0.05,0.05,0.05,0.03,0.02),
  ideal     = c(7.0,8.0,3.0,10,500,1,250,30,120,800),
  stringsAsFactors = FALSE
)

calculate_wqi <- function(data) {
  data %>%
    inner_join(wqi_params, by="parameter") %>%
    mutate(
      qi    = case_when(
        parameter == "pH"                      ~ abs(value - ideal) * 12.5,
        parameter == "Dissolved Oxygen (mg/L)" ~ (ideal / pmax(value,0.01)) * 25,
        TRUE                                   ~ (value / pmax(ideal,0.01)) * 100
      ),
      wi_qi = qi * weight
    ) %>%
    group_by(location, date) %>%
    summarise(WQI=sum(wi_qi,na.rm=TRUE), n_params=n(), .groups="drop") %>%
    mutate(
      WQI_class = factor(case_when(
        WQI<=25  ~ "Excellent", WQI<=50  ~ "Good",
        WQI<=75  ~ "Fair",      WQI<=100 ~ "Marginal", TRUE ~ "Poor"
      ), levels=c("Excellent","Good","Fair","Marginal","Poor"), ordered=TRUE),
      WQI_color = case_when(
        WQI<=25  ~ "#00FFB3", WQI<=50  ~ "#00D4FF",
        WQI<=75  ~ "#FFE600", WQI<=100 ~ "#FF6B35", TRUE ~ "#FF2D6B"
      )
    )
}

# ── 4. METADATA ─────────────────────────────────────────────────────────
location_meta <- data.frame(
  location   = c("Mirpur","Hazaribagh","Kamrangir Char","Sadarghat"),
  lat        = c(23.8066,23.7385,23.7210,23.7099),
  lon        = c(90.3672,90.3654,90.3661,90.4141),
  type       = c("Residential","Industrial","Informal Settlement","Commercial"),
  population = c("450,000","185,000","300,000","High flux"),
  risk       = c("Urban runoff, limited sanitation","Former tannery, heavy metals",
                 "Overcrowding, poor infrastructure","River traffic, sewage discharge"),
  stringsAsFactors = FALSE
)

# ── 5. CONSTANTS ────────────────────────────────────────────────────────
ALL_PARAMS <- c("pH","Dissolved Oxygen (mg/L)","BOD (mg/L)","COD (mg/L)","TDS (mg/L)",
                "Turbidity (NTU)","Chloride (mg/L)","Suspended Solids (mg/L)",
                "Total Alkalinity (mg/L)","EC (uS/cm)")
ALL_LOCS   <- c("Mirpur","Hazaribagh","Kamrangir Char","Sadarghat")
DATE_MIN   <- as.Date("2018-01-01")
DATE_MAX   <- as.Date("2022-12-01")
NEON       <- c("#00FFB3","#00D4FF","#B44FFF","#FF2D6B",
                "#FFE600","#FF6B35","#7BFF4F","#FF9EFF","#00BFFF","#FF4500")
WQI_COLORS <- c("Excellent"="#00FFB3","Good"="#00D4FF",
                "Fair"="#FFE600","Marginal"="#FF6B35","Poor"="#FF2D6B")

# ── 6. LOAD DATA AT STARTUP ─────────────────────────────────────────────
STARTUP_DATA  <- NULL
STARTUP_ERROR <- NULL

tryCatch({
  # Try multiple possible paths
  candidates <- c(
    "4 locations.xlsx",
    file.path(getwd(), "4 locations.xlsx")
  )
  found <- Filter(file.exists, candidates)
  if (length(found) == 0) {
    # Create sample data for demonstration if file doesn't exist
    message("Sample data will be used for demonstration")
    
    # Generate sample data
    set.seed(123)
    dates <- seq.Date(DATE_MIN, DATE_MAX, by="month")
    
    STARTUP_DATA <- expand.grid(
      location = ALL_LOCS,
      parameter = ALL_PARAMS,
      date = dates,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        value = case_when(
          parameter == "pH" ~ 6.5 + runif(n(), 0, 2),
          parameter == "Dissolved Oxygen (mg/L)" ~ 4 + runif(n(), 0, 4),
          parameter == "BOD (mg/L)" ~ 3 + runif(n(), 0, 8),
          parameter == "COD (mg/L)" ~ 20 + runif(n(), 0, 40),
          parameter == "TDS (mg/L)" ~ 400 + runif(n(), 0, 300),
          parameter == "Turbidity (NTU)" ~ 5 + runif(n(), 0, 20),
          parameter == "Chloride (mg/L)" ~ 100 + runif(n(), 0, 200),
          parameter == "Suspended Solids (mg/L)" ~ 30 + runif(n(), 0, 70),
          parameter == "Total Alkalinity (mg/L)" ~ 100 + runif(n(), 0, 50),
          parameter == "EC (uS/cm)" ~ 800 + runif(n(), 0, 400),
          TRUE ~ runif(n(), 0, 100)
        ),
        symbol = case_when(
          location == "Mirpur" ~ "MIR",
          location == "Hazaribagh" ~ "HAZ",
          location == "Kamrangir Char" ~ "KAM",
          location == "Sadarghat" ~ "SAD"
        )
      )
  } else {
    STARTUP_DATA <- process_water_data(found[1])
  }
}, error = function(e) {
  STARTUP_ERROR <<- conditionMessage(e)
})

# ── 7. CSS ──────────────────────────────────────────────────────────────
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Rajdhani:wght@400;600;700&family=Share+Tech+Mono&display=swap');
:root{
  --void:#020810;--deep:#050D1C;--panel:#091525;--card:#0C1E35;--lift:#102440;
  --b0:rgba(0,212,255,0.08);--b1:rgba(0,212,255,0.18);--b2:rgba(0,212,255,0.45);
  --cyan:#00D4FF;--green:#00FFB3;--pink:#FF2D6B;--purp:#B44FFF;--gold:#FFE600;
  --orng:#FF6B35;--txt:#EDF6FF;--mid:#6BAED4;--dim:#2E5F7A;
}
*{box-sizing:border-box;}
body,.content-wrapper,.right-side,.wrapper{font-family:'Rajdhani',sans-serif !important;background:var(--void) !important;color:var(--txt) !important;}
.main-sidebar,.left-side{background:linear-gradient(180deg,var(--deep),var(--void)) !important;border-right:1px solid var(--b1) !important;}
.sidebar-menu>li>a{color:var(--dim) !important;font-family:'Rajdhani',sans-serif !important;font-size:13px !important;font-weight:600 !important;letter-spacing:1px;text-transform:uppercase;border-left:3px solid transparent !important;padding:11px 15px 11px 18px !important;transition:all 0.3s !important;}
.sidebar-menu>li>a:hover{color:var(--cyan) !important;border-left-color:var(--cyan) !important;padding-left:24px !important;}
.sidebar-menu>li.active>a{color:var(--cyan) !important;border-left-color:var(--cyan) !important;background:rgba(0,212,255,0.06) !important;}
.treeview-menu{background:transparent !important;}
.treeview-menu>li>a{color:var(--dim) !important;font-size:11px !important;font-weight:500 !important;padding-left:30px !important;transition:all 0.2s;text-transform:uppercase;letter-spacing:.8px;}
.treeview-menu>li>a:hover,.treeview-menu>li.active>a{color:var(--green) !important;padding-left:35px !important;}
.sidebar-footer{position:absolute;bottom:0;left:0;right:0;padding:16px 14px;border-top:1px solid var(--b1);text-align:center;background:linear-gradient(0deg,rgba(0,212,255,0.06),transparent);}
.sf-logo{font-family:'Orbitron',monospace;font-size:14px;font-weight:900;letter-spacing:5px;display:block;margin-bottom:4px;background:linear-gradient(90deg,var(--cyan),var(--purp));-webkit-background-clip:text;-webkit-text-fill-color:transparent;}
.sf-sub{font-size:9px;color:var(--dim);letter-spacing:1px;font-family:'Share Tech Mono',monospace;}
.sf-status{display:inline-flex;align-items:center;gap:5px;margin-top:8px;font-size:9px;color:var(--green);font-family:'Share Tech Mono',monospace;letter-spacing:1px;}
.sf-dot{width:6px;height:6px;border-radius:50%;background:var(--green);animation:dp 2s ease-in-out infinite;}
@keyframes dp{0%,100%{opacity:1;}50%{opacity:.3;}}
.main-header .navbar,.main-header .logo{background:linear-gradient(90deg,var(--void),var(--deep)) !important;border-bottom:1px solid var(--b2) !important;}
.main-header .logo{font-family:'Orbitron',monospace !important;font-size:11px !important;letter-spacing:5px !important;color:var(--cyan) !important;}
.box{background:var(--card) !important;border:1px solid var(--b0) !important;border-radius:12px !important;box-shadow:0 4px 30px rgba(0,0,0,.6) !important;margin-bottom:20px;}
.box-header{background:linear-gradient(90deg,rgba(0,212,255,0.06),transparent) !important;border-bottom:1px solid var(--b0) !important;padding:12px 18px !important;}
.box-title{font-family:'Orbitron',monospace !important;font-weight:700 !important;font-size:9px !important;color:var(--cyan) !important;letter-spacing:2.5px !important;text-transform:uppercase;}
.box-body{padding:16px !important;}
.small-box{border-radius:12px !important;overflow:hidden;border:1px solid var(--b0);transition:all .3s;}
.small-box:hover{transform:translateY(-5px) scale(1.02);}
.small-box h3{font-family:'Orbitron',monospace !important;font-size:30px !important;font-weight:900 !important;}
.small-box p{font-family:'Rajdhani',sans-serif !important;font-size:10px !important;font-weight:700 !important;letter-spacing:2.5px;text-transform:uppercase;}
.bg-aqua{background:linear-gradient(135deg,#002E46,#0099CC) !important;}
.bg-green{background:linear-gradient(135deg,#002E20,#009960) !important;}
.bg-yellow{background:linear-gradient(135deg,#2E1C00,#CC8800) !important;}
.bg-red{background:linear-gradient(135deg,#2E000F,#CC0040) !important;}
.form-control,.selectize-input{background:var(--deep) !important;border:1px solid var(--b0) !important;color:var(--txt) !important;border-radius:6px !important;font-family:'Rajdhani',sans-serif !important;font-size:14px !important;}
.form-control:focus,.selectize-input.focus{border-color:var(--cyan) !important;outline:none !important;}
.selectize-dropdown{background:var(--panel) !important;border:1px solid var(--b2) !important;border-radius:8px !important;}
.selectize-dropdown .option{color:var(--mid) !important;padding:8px 14px;font-size:13px;}
.selectize-dropdown .option:hover{background:rgba(0,212,255,.08) !important;color:var(--cyan) !important;}
label{color:var(--dim) !important;font-family:'Share Tech Mono',monospace !important;font-size:9px !important;letter-spacing:2px !important;text-transform:uppercase !important;margin-bottom:6px !important;}
.btn{font-family:'Rajdhani',sans-serif !important;font-weight:700 !important;font-size:12px !important;letter-spacing:1px;text-transform:uppercase;border-radius:5px !important;}
.btn-primary{background:linear-gradient(135deg,#008BB0,#004E80) !important;border:1px solid var(--cyan) !important;color:#fff !important;}
.nav-tabs{border-bottom:1px solid var(--b1) !important;}
.nav-tabs>li>a{color:var(--dim) !important;border:none !important;border-bottom:2px solid transparent !important;background:transparent !important;font-family:'Rajdhani',sans-serif !important;font-size:12px !important;font-weight:700 !important;letter-spacing:1.5px;text-transform:uppercase;padding:10px 18px;}
.nav-tabs>li.active>a,.nav-tabs>li>a:hover{color:var(--cyan) !important;border-bottom-color:var(--cyan) !important;background:transparent !important;}
.tab-content{padding-top:16px;}
.dataTables_wrapper{color:var(--mid) !important;font-family:'Rajdhani',sans-serif !important;}
table.dataTable tbody tr{background:var(--panel) !important;color:var(--txt) !important;}
table.dataTable tbody tr:hover{background:var(--lift) !important;}
table.dataTable thead th{background:var(--deep) !important;color:var(--cyan) !important;border-bottom:1px solid var(--b2) !important;font-family:'Share Tech Mono',monospace !important;font-size:9px !important;text-transform:uppercase !important;letter-spacing:1.5px !important;padding:13px 14px !important;}
.wqi-legend-table{width:100%;font-size:13px;border-collapse:collapse;}
.wqi-legend-table td{padding:9px 12px;border-bottom:1px solid var(--b0);color:var(--mid);}
.wqi-dot{display:inline-block;width:10px;height:10px;border-radius:50%;margin-right:8px;vertical-align:middle;}
.wqi-label{font-family:'Rajdhani',sans-serif;font-weight:700;font-size:14px;}
.alert-neon{background:rgba(0,212,255,.06);border:1px solid rgba(0,212,255,.18);border-left:3px solid var(--cyan);border-radius:6px;padding:12px 16px;margin:10px 0;font-family:'Rajdhani',sans-serif;font-size:13px;color:var(--mid);}
.methods-content h3{color:var(--cyan);font-family:'Orbitron',monospace;font-size:11px;text-transform:uppercase;letter-spacing:2.5px;margin-top:24px;padding-bottom:10px;border-bottom:1px solid rgba(0,212,255,.18);}
.methods-content h4{color:var(--green);font-family:'Share Tech Mono',monospace;font-size:10px;text-transform:uppercase;letter-spacing:2px;margin-top:18px;}
.methods-content p,.methods-content li{color:var(--mid);font-size:14px;line-height:1.9;}
.methods-content table{width:100%;font-size:13px;border-collapse:collapse;margin:16px 0;}
.methods-content table th{background:rgba(0,212,255,.08);color:var(--cyan);font-family:'Share Tech Mono',monospace;font-size:9px;letter-spacing:1.5px;padding:11px;border-bottom:1px solid var(--b2);text-transform:uppercase;}
.methods-content table td{padding:9px 11px;border-bottom:1px solid var(--b0);color:var(--mid);}
.methods-content .formula{background:var(--deep);border:1px solid var(--b2);border-left:3px solid var(--cyan);border-radius:6px;padding:14px 20px;font-family:'Share Tech Mono',monospace;font-size:13px;color:var(--cyan);margin:14px 0;}
.global-footer{position:fixed;bottom:0;left:250px;right:0;z-index:1000;background:linear-gradient(90deg,var(--void),var(--deep),var(--void));border-top:1px solid rgba(0,212,255,.18);padding:6px 24px;display:flex;align-items:center;justify-content:center;gap:16px;}
.gf-text{font-family:'Share Tech Mono',monospace;font-size:10px;color:var(--dim);letter-spacing:1.5px;text-transform:uppercase;}
.gf-sep{color:var(--cyan);opacity:.5;}
.gf-highlight{color:var(--gold);}
.content-wrapper{padding-bottom:36px !important;}
.err-box{background:rgba(255,45,107,.08);border:1px solid rgba(255,45,107,.4);border-left:4px solid #FF2D6B;border-radius:8px;padding:20px 24px;font-family:'Rajdhani',sans-serif;color:#FF2D6B;font-size:14px;margin:20px 0;}
.welcome-card{background:linear-gradient(135deg,rgba(0,212,255,.06),rgba(180,79,255,.06),rgba(0,255,179,.04));border:1px solid rgba(0,212,255,.18);border-radius:14px;padding:28px 32px 22px;margin-bottom:20px;position:relative;overflow:hidden;}
.wc-title{font-family:'Orbitron',monospace;font-size:22px;font-weight:900;letter-spacing:4px;text-transform:uppercase;background:linear-gradient(90deg,var(--cyan),var(--purp));-webkit-background-clip:text;-webkit-text-fill-color:transparent;margin:0 0 6px;}
.wc-subtitle{font-family:'Rajdhani',sans-serif;font-size:13px;font-weight:600;color:var(--mid);letter-spacing:2px;text-transform:uppercase;margin-bottom:20px;}
.wc-presenter-block{display:flex;align-items:flex-start;gap:18px;}
.wc-avatar{width:54px;height:54px;border-radius:50%;flex-shrink:0;background:linear-gradient(135deg,var(--cyan),var(--purp));display:flex;align-items:center;justify-content:center;font-family:'Orbitron',monospace;font-size:18px;font-weight:900;color:#020810;}
.wc-presenter-name{font-family:'Orbitron',monospace;font-size:15px;font-weight:700;color:var(--cyan);letter-spacing:2px;margin-bottom:4px;}
.wc-presenter-dept{font-family:'Rajdhani',sans-serif;font-size:13px;color:var(--mid);font-weight:500;line-height:1.5;}
"

# ── 8. UI ────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tagList(
      tags$span("AQUA", style="font-family:'Orbitron',monospace;font-weight:900;letter-spacing:5px;color:#00D4FF;"),
      tags$span("LENS", style="font-family:'Orbitron',monospace;font-weight:400;letter-spacing:5px;color:#B44FFF;")
    ),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard",         tabName="dashboard", icon=icon("gauge-high")),
      menuItem("Spatial Analysis",  tabName="spatial",   icon=icon("map"),
               menuSubItem("Quality Map",         tabName="map"),
               menuSubItem("Hotspot Detection",   tabName="hotspots")),
      menuItem("Temporal Analysis", tabName="temporal",  icon=icon("chart-line"),
               menuSubItem("Parameter Trends",    tabName="trends"),
               menuSubItem("Seasonal Patterns",   tabName="seasonal"),
               menuSubItem("Year-over-Year",      tabName="yoy")),
      menuItem("Comparative",       tabName="compare",   icon=icon("chart-bar"),
               menuSubItem("Parameter Heatmap",   tabName="heatmap"),
               menuSubItem("Location Comparison", tabName="loc_compare"),
               menuSubItem("PCA Analysis",        tabName="pca")),
      menuItem("Risk Assessment",   tabName="risk_tab",  icon=icon("triangle-exclamation"),
               menuSubItem("Exceedance Analysis", tabName="exceedance"),
               menuSubItem("Pollution Index",     tabName="pollution")),
      menuItem("Data Explorer",     tabName="explorer",  icon=icon("table")),
      menuItem("Methodology",       tabName="methods",   icon=icon("flask"))
    ),
    tags$div(class="sidebar-footer",
             tags$span(class="sf-logo", "AQUALENS"),
             tags$span(class="sf-sub",  "BURIGANGA RIVER · DHAKA"),
             tags$div(class="sf-status", tags$div(class="sf-dot"), "MONITORING ACTIVE")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(CSS))),
    
    tags$div(class="global-footer",
             tags$span(class="gf-text", "3-Minute Presentation Competition 2026"),
             tags$span(class="gf-sep", "◆"),
             tags$span(class="gf-text gf-highlight", "HSTU Research Society (HSTURS)"),
             tags$span(class="gf-sep", "◆"),
             tags$span(class="gf-text", "AQUALENS v4.0 · Buriganga River · Dhaka")
    ),
    
    tabItems(
      
      # ── DASHBOARD ──────────────────────────────────────────────
      tabItem("dashboard",
              tags$div(class="welcome-card",
                       tags$div(class="wc-event-badge",
                                tags$div(class="wc-event-dot"),
                                tags$span("3-Minute Presentation Competition 2026 · HSTU Research Society (HSTURS)")
                       ),
                       tags$div(class="wc-title", "AQUALENS"),
                       tags$div(class="wc-subtitle", "Water Quality Intelligence Platform · Buriganga River, Dhaka"),
                       tags$div(class="wc-divider"),
                       tags$div(class="wc-presenter-block",
                                tags$div(class="wc-avatar", "SM"),
                                tags$div(
                                  tags$div(class="wc-presenter-name",  "Md Salek Miah"),
                                  tags$div(class="wc-presenter-dept",
                                           "Department of Statistics, Shahjalal University of Science and Technology")
                                )
                       )
              ),
              uiOutput("data_status_banner"),
              fluidRow(
                valueBoxOutput("vb_samples",    width=2),
                valueBoxOutput("vb_avg_wqi",    width=2),
                valueBoxOutput("vb_locations",  width=2),
                valueBoxOutput("vb_parameters", width=2),
                valueBoxOutput("vb_poor_pct",   width=2),
                valueBoxOutput("vb_date_range", width=2)
              ),
              fluidRow(
                box(width=8, status="primary", solidHeader=TRUE,
                    title=tagList(icon("chart-line")," WQI TREND — ALL LOCATIONS"),
                    plotlyOutput("plot_wqi_trend",height="340px") %>% withSpinner(color="#00D4FF",type=6)),
                box(width=4, status="info", solidHeader=TRUE,
                    title=tagList(icon("chart-bar")," WQI DISTRIBUTION"),
                    plotlyOutput("plot_wqi_dist",height="340px") %>% withSpinner(color="#B44FFF",type=6))
              ),
              fluidRow(
                box(width=7, status="success", solidHeader=TRUE,
                    title=tagList(icon("table-cells")," LOCATION SUMMARY STATISTICS"),
                    DTOutput("table_summary") %>% withSpinner(color="#00FFB3",type=6)),
                box(width=5, status="warning", solidHeader=TRUE,
                    title=tagList(icon("circle-dot")," PARAMETER RADAR — MEAN VALUES"),
                    plotlyOutput("plot_radar",height="320px") %>% withSpinner(color="#FFE600",type=6))
              )
      ),
      
      # ── MAP ────────────────────────────────────────────────────
      tabItem("map",
              fluidRow(
                column(9,
                       box(width=NULL, status="primary", solidHeader=TRUE,
                           title=tagList(icon("map-location-dot")," SPATIAL WATER QUALITY DISTRIBUTION"),
                           leafletOutput("map_wqi",height="66vh") %>% withSpinner(color="#00D4FF",type=6))
                ),
                column(3,
                       box(width=NULL, status="info", solidHeader=TRUE,
                           title=tagList(icon("sliders")," MAP CONTROLS"),
                           sliderInput("map_time","Time Window",min=DATE_MIN,max=DATE_MAX,value=DATE_MAX,timeFormat="%b %Y"),
                           awesomeCheckboxGroup("map_locs","Monitoring Sites",choices=ALL_LOCS,selected=ALL_LOCS),
                           hr(),
                           actionBttn("map_refresh","Refresh Map",icon=icon("rotate"),style="material-flat",color="primary",size="sm",block=TRUE)
                       )
                )
              )
      ),
      
      tabItem("hotspots",
              box(width=12, status="danger", solidHeader=TRUE,
                  title=tagList(icon("fire")," HOTSPOT DETECTION"),
                  tags$div(class="alert-neon danger", icon("triangle-exclamation"),
                           " Navigate to Spatial Analysis > Quality Map to identify pollution hotspots."))
      ),
      
      # ── TRENDS ─────────────────────────────────────────────────
      tabItem("trends",
              box(width=12, status="primary", solidHeader=TRUE,
                  title=tagList(icon("chart-line")," TEMPORAL PARAMETER TRENDS"),
                  fluidRow(
                    column(3,
                           pickerInput("trend_params","Parameters",choices=ALL_PARAMS,selected="Dissolved Oxygen (mg/L)",multiple=TRUE,
                                       options=list(`actions-box`=TRUE)),
                           pickerInput("trend_locs","Locations",choices=ALL_LOCS,selected=ALL_LOCS[1],multiple=TRUE),
                           dateRangeInput("trend_dates","Date Range",start=DATE_MIN,end=DATE_MAX),
                           radioGroupButtons("trend_agg","Aggregation",choices=c("Monthly","Quarterly","Annual"),selected="Monthly")
                    ),
                    column(9,
                           tabBox(width=NULL,
                                  tabPanel("Trend Chart",  plotlyOutput("plot_trend",height="450px") %>% withSpinner(color="#00D4FF",type=6)),
                                  tabPanel("Statistics",   verbatimTextOutput("trend_stats"))
                           )
                    )
                  )
              )
      ),
      
      # ── DATA EXPLORER ──────────────────────────────────────────
      tabItem("explorer",
              box(width=12, status="primary", solidHeader=TRUE,
                  title=tagList(icon("table")," SCIENTIFIC DATA EXPLORER"),
                  fluidRow(
                    column(3, pickerInput("dt_locs","Filter Locations",choices=ALL_LOCS,selected=ALL_LOCS,multiple=TRUE)),
                    column(3, pickerInput("dt_params","Filter Parameters",choices=ALL_PARAMS,selected=ALL_PARAMS,multiple=TRUE)),
                    column(3, dateRangeInput("dt_dates","Date Range",start=DATE_MIN,end=DATE_MAX)),
                    column(3, br(), actionBttn("dt_reset","Reset Filters",icon=icon("rotate"),style="material-flat",color="danger",size="sm",block=TRUE))
                  ),
                  hr(),
                  DTOutput("dt_main") %>% withSpinner(color="#00D4FF",type=6)
              )
      ),
      
      # ── METHODOLOGY ────────────────────────────────────────────
      tabItem("methods",
              fluidRow(
                box(width=12, status="primary", solidHeader=TRUE,
                    title=tagList(icon("flask")," SCIENTIFIC METHODOLOGY"),
                    tags$div(class="methods-content",
                             tags$h3("Water Quality Index (WQI)"),
                             tags$p("WQI is calculated following WHO and USEPA guidelines."),
                             tags$h4("Parameter Weights"),
                             tags$table(
                               tags$thead(tags$tr(tags$th("Parameter"),tags$th("Weight"),tags$th("Ideal"))),
                               tags$tbody(
                                 tags$tr(tags$td("Dissolved Oxygen"), tags$td("25%"),tags$td("8 mg/L")),
                                 tags$tr(tags$td("BOD"), tags$td("20%"),tags$td("3 mg/L")),
                                 tags$tr(tags$td("pH"), tags$td("15%"),tags$td("7.0"))
                               )
                             )
                    )
                )
              )
      )
    )
  )
)

# ── 9. SERVER ────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Data reactives
  df <- reactive({ 
    req(STARTUP_DATA)
    STARTUP_DATA 
  })
  
  wqi_data <- reactive({ 
    req(df())
    calculate_wqi(df()) 
  })
  
  # Error banner
  output$data_status_banner <- renderUI({
    if (!is.null(STARTUP_ERROR)) {
      tags$div(class="err-box",
               tags$h4(icon("triangle-exclamation"), " DATA LOAD ERROR"),
               tags$p(STARTUP_ERROR),
               tags$p(style="color:#6BAED4;", "Using sample data for demonstration.")
      )
    }
  })
  
  # Value boxes
  output$vb_samples <- renderValueBox({
    if (is.null(df())) {
      valueBox("—", "Observations", icon = icon("flask"), color = "aqua")
    } else {
      valueBox(formatC(nrow(df()), format="d", big.mark=","), 
               "Observations", icon = icon("flask"), color = "aqua")
    }
  })
  
  output$vb_avg_wqi <- renderValueBox({
    if (is.null(df()) || nrow(df()) == 0) {
      valueBox("—", "Avg WQI", icon = icon("gauge"), color = "blue")
    } else {
      avg <- round(mean(wqi_data()$WQI, na.rm=TRUE), 1)
      color <- if(avg <= 50) "green" else if(avg <= 75) "yellow" else "red"
      valueBox(avg, "Avg WQI", icon = icon("gauge"), color = color)
    }
  })
  
  output$vb_locations <- renderValueBox({
    if (is.null(df())) {
      valueBox("—", "Sites", icon = icon("location-dot"), color = "blue")
    } else {
      valueBox(length(unique(df()$location)), "Sites", 
               icon = icon("location-dot"), color = "blue")
    }
  })
  
  output$vb_parameters <- renderValueBox({
    if (is.null(df())) {
      valueBox("—", "Parameters", icon = icon("vial"), color = "purple")
    } else {
      valueBox(length(unique(df()$parameter)), "Parameters", 
               icon = icon("vial"), color = "purple")
    }
  })
  
  output$vb_poor_pct <- renderValueBox({
    if (is.null(df()) || nrow(df()) == 0) {
      valueBox("—", "Poor WQI %", icon = icon("triangle-exclamation"), color = "maroon")
    } else {
      pct <- round(mean(wqi_data()$WQI_class == "Poor", na.rm=TRUE) * 100, 1)
      color <- if(pct > 30) "red" else "maroon"
      valueBox(paste0(pct, "%"), "Poor WQI %", 
               icon = icon("triangle-exclamation"), color = color)
    }
  })
  
  output$vb_date_range <- renderValueBox({
    if (is.null(df())) {
      valueBox("—", "Study Period", icon = icon("calendar"), color = "navy")
    } else {
      rng <- paste0(format(min(df()$date, na.rm=TRUE), "%Y"), "-", 
                    format(max(df()$date, na.rm=TRUE), "%Y"))
      valueBox(rng, "Study Period", icon = icon("calendar"), color = "navy")
    }
  })
  
  # WQI trend
  output$plot_wqi_trend <- renderPlotly({
    req(wqi_data())
    
    d <- wqi_data() %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month, location) %>%
      summarise(avg_wqi = mean(WQI, na.rm=TRUE), .groups = "drop")
    
    plot_ly(d, x = ~month, y = ~avg_wqi, color = ~location,
            type = "scatter", mode = "lines+markers",
            colors = NEON) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "WQI Score"),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             font = list(color = "#6BAED4"))
  })
  
  # WQI distribution
  output$plot_wqi_dist <- renderPlotly({
    req(wqi_data())
    
    plot_ly(wqi_data(), y = ~WQI, color = ~location, 
            type = "violin", colors = NEON) %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = "WQI Score"),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             showlegend = FALSE)
  })
  
  # Summary table
  output$table_summary <- renderDT({
    req(wqi_data())
    
    wqi_data() %>%
      group_by(Location = location) %>%
      summarise(
        N = n(),
        `Mean WQI` = round(mean(WQI, na.rm=TRUE), 2),
        `Min` = round(min(WQI, na.rm=TRUE), 2),
        `Max` = round(max(WQI, na.rm=TRUE), 2),
        .groups = "drop"
      ) %>%
      datatable(rownames = FALSE, options = list(dom = "t"))
  })
  
  # Radar chart
  output$plot_radar <- renderPlotly({
    req(df())
    
    d <- df() %>%
      group_by(location, parameter) %>%
      summarise(mean_val = mean(value, na.rm=TRUE), .groups = "drop") %>%
      group_by(parameter) %>%
      mutate(norm_val = scale(mean_val)[,1]) %>%
      ungroup()
    
    plot_ly(type = "scatterpolar", fill = "toself") %>%
      add_trace(
        r = ~filter(d, location == "Mirpur")$norm_val,
        theta = ~filter(d, location == "Mirpur")$parameter,
        name = "Mirpur",
        line = list(color = NEON[1])
      ) %>%
      add_trace(
        r = ~filter(d, location == "Hazaribagh")$norm_val,
        theta = ~filter(d, location == "Hazaribagh")$parameter,
        name = "Hazaribagh",
        line = list(color = NEON[2])
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE)
        ),
        showlegend = TRUE,
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      )
  })
  
  # Map
  output$map_wqi <- renderLeaflet({
    req(wqi_data(), input$map_time)
    
    d <- wqi_data() %>%
      filter(date <= input$map_time, location %in% input$map_locs) %>%
      group_by(location) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      left_join(location_meta, by = "location")
    
    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 90.385, lat = 23.760, zoom = 13) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 15,
        color = ~WQI_color,
        fillColor = ~WQI_color,
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>", location, "</b><br>",
          "WQI: ", round(WQI, 1), "<br>",
          "Class: ", WQI_class
        ),
        label = ~location
      )
  })
  
  observeEvent(input$map_refresh, {
    leafletProxy("map_wqi") %>% clearMarkers()
  })
  
  # Trends
  trend_data <- reactive({
    req(df(), input$trend_params, input$trend_locs)
    
    df() %>%
      filter(
        parameter %in% input$trend_params,
        location %in% input$trend_locs,
        date >= input$trend_dates[1],
        date <= input$trend_dates[2]
      )
  })
  
  output$plot_trend <- renderPlotly({
    req(trend_data())
    
    d <- trend_data()
    
    ggplotly(
      ggplot(d, aes(x = date, y = value, color = parameter)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        facet_wrap(~location, scales = "free_y") +
        scale_color_manual(values = NEON) +
        labs(x = "", y = "Value") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#091525"),
          panel.background = element_rect(fill = "#0C1E35"),
          text = element_text(color = "#6BAED4"),
          axis.text = element_text(color = "#6BAED4")
        )
    )
  })
  
  output$trend_stats <- renderPrint({
    req(trend_data())
    
    trend_data() %>%
      group_by(parameter, location) %>%
      summarise(
        Mean = round(mean(value, na.rm=TRUE), 2),
        SD = round(sd(value, na.rm=TRUE), 2),
        Min = round(min(value, na.rm=TRUE), 2),
        Max = round(max(value, na.rm=TRUE), 2),
        N = n(),
        .groups = "drop"
      ) %>%
      print(n = Inf)
  })
  
  # Data explorer
  dt_filtered <- reactive({
    req(df(), input$dt_locs, input$dt_params)
    
    df() %>%
      filter(
        location %in% input$dt_locs,
        parameter %in% input$dt_params,
        date >= input$dt_dates[1],
        date <= input$dt_dates[2]
      )
  })
  
  observeEvent(input$dt_reset, {
    updatePickerInput(session, "dt_locs", selected = ALL_LOCS)
    updatePickerInput(session, "dt_params", selected = ALL_PARAMS)
    updateDateRangeInput(session, "dt_dates", start = DATE_MIN, end = DATE_MAX)
  })
  
  output$dt_main <- renderDT({
    req(dt_filtered())
    
    datatable(
      dt_filtered(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    ) %>%
      formatRound("value", 2)
  })
}

# ── 10. LAUNCH — MUST BE THE ABSOLUTE LAST LINE ──────────────────────────
shinyApp(ui = ui, server = server)


