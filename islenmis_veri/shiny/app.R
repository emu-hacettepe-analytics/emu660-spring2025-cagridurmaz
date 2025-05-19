# app.R
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(forcats)

# --- Modular Functions ---

# Data Loading
load_data <- function(file_path = "anonim_temiz_veri_listesi.rds") {
  absolute_path <- "/Users/cagridurmaz/Documents/GitHub/emu660 spring2025-cagridurmaz/islenmis_veri/anonim_temiz_veri_listesi.rds"
  if (file.exists(absolute_path)) {
    cat("Loading data from absolute path:", absolute_path, "\n")
    data <- readRDS(absolute_path)
  } else if (file.exists(file_path)) {
    cat("Loading data from relative path:", file_path, "\n")
    data <- readRDS(file_path)
  } else {
    stop("Data file not found at either:\n  Absolute path: ", absolute_path, "\n  Relative path: ", file_path)
  }
  
  required_dfs <- c("affiliations", "users", "works", "companies", "industries",
                    "highschools", "positions", "erasmus", "universities", "employmentmethod")
  missing_dfs <- setdiff(required_dfs, names(data))
  if (length(missing_dfs) > 0) {
    stop("Missing data frames: ", paste(missing_dfs, collapse = ", "))
  }
  cat("Data loaded successfully. Data frames:", paste(names(data), collapse = ", "), "\n")
  return(data)
}

# Gender summary
get_gender_summary <- function(affiliations, users) {
  data <- affiliations %>%
    filter(type == 2) %>%
    select(fake_student_id) %>%
    distinct() %>%
    inner_join(users %>% select(fake_student_id, gender), by = "fake_student_id") %>%
    count(gender, name = "n") %>%
    mutate(
      oran = n / sum(n),
      yuzde = oran * 100,
      etiket = sprintf("%.1f%%", yuzde),
      Cinsiyet = case_when(
        gender == "female" ~ "Kadın",
        gender == "male"   ~ "Erkek",
        TRUE               ~ as.character(gender)
      )
    )
  return(data)
}

# General summary
get_general_summary <- function(works, companies, industries) {
  country_df <- works %>%
    select(fake_student_id, country) %>%
    distinct() %>%
    filter(!is.na(country) & country != "")
  unique_countries  <- n_distinct(country_df$country)
  
  industry_df <- works %>%
    select(fake_student_id, companyid) %>%
    distinct() %>%
    filter(!is.na(companyid) & companyid != "") %>%
    left_join(companies %>% select(id, industry), by = c("companyid" = "id")) %>%
    filter(!is.na(industry) & industry != "") %>%
    distinct()
  unique_industries <- n_distinct(industry_df$industry)
  
  position_df <- works %>%
    select(fake_student_id, positionid) %>%
    distinct() %>%
    filter(!is.na(positionid) & positionid != "")
  unique_positions  <- n_distinct(position_df$positionid)
  
  employer_df <- works %>%
    select(companyid) %>%
    distinct() %>%
    filter(!is.na(companyid) & companyid != "")
  unique_employers  <- n_distinct(employer_df$companyid)
  
  result <- list(
    text = sprintf(
      "Öğrencilerimiz ve mezunlarımız %d ülke, %d endüstri, %d iş pozisyonu ve %d işverende çalışmış veya halen çalışmaktadır.",
      unique_countries, unique_industries, unique_positions, unique_employers
    ),
    counts = list(
      countries   = unique_countries,
      industries  = unique_industries,
      positions   = unique_positions,
      employers   = unique_employers
    )
  )
  return(result)
}

# Company summary
get_company_summary <- function(works, companies, max_companies = 10) {
  company_df <- works %>%
    select(fake_student_id, companyid) %>%
    distinct() %>%
    filter(!is.na(companyid) & companyid != "") %>%
    left_join(companies %>% select(id, name), by = c("companyid" = "id")) %>%
    filter(!is.na(name) & name != "")
  
  company_summary <- company_df %>%
    count(name) %>%
    arrange(desc(n)) %>%
    mutate(
      oran   = n / sum(n),
      yuzde  = oran * 100,
      etiket = sprintf("%.1f%%", yuzde),
      name   = fct_reorder(name, n)
    ) %>%
    slice_head(n = max_companies)
  
  unique_companies <- n_distinct(company_df$name)
  return(list(data = company_summary, total = unique_companies))
}

# Province summary
get_province_summary <- function(affiliations, users, highschools, max_provinces = 10) {
  hs_prov <- affiliations %>%
    filter(type %in% c(2,3)) %>%
    select(fake_student_id) %>%
    inner_join(users %>% select(fake_student_id, highschool), by = "fake_student_id") %>%
    inner_join(highschools %>% select(id, city), by = c("highschool" = "id")) %>%
    filter(!is.na(city) & city != "") %>%
    count(city, name = "n") %>%
    arrange(desc(n)) %>%
    mutate(
      pct   = n / sum(n),
      label = sprintf("%.1f%%", pct*100)
    ) %>%
    slice_max(order_by = n, n = max_provinces)
  return(hs_prov)
}

# Position summary
get_position_summary <- function(affiliations, works, positions, max_positions = 10) {
  grad_ids <- affiliations %>% filter(type == 2) %>% pull(fake_student_id)
  pos_dist <- works %>%
    filter(fake_student_id %in% grad_ids, !is.na(positionid) & positionid != "") %>%
    distinct(fake_student_id, .keep_all = TRUE) %>%
    inner_join(positions %>% select(id, position), by = c("positionid" = "id")) %>%
    count(position, name = "n") %>%
    arrange(desc(n)) %>%
    mutate(
      pct   = n / sum(n),
      label = percent(pct, accuracy = 0.1)
    ) %>%
    slice_max(order_by = n, n = max_positions)
  result <- list(
    data            = pos_dist,
    total_positions = n_distinct(pos_dist$position),
    total_people    = n_distinct(works$fake_student_id)
  )
  return(result)
}

# CGPA summary
get_cgpa_summary <- function(affiliations, erasmus) {
  affiliations <- affiliations %>%
    mutate(
      cgpa_clean = suppressWarnings(as.numeric(gsub(",", ".", cgpa))),
      cgpa       = if_else(is.na(cgpa_clean) & !is.na(cgpa), NA_real_, cgpa_clean)
    )
  
  grad_erasmus_ids <- affiliations %>%
    filter(type == 2) %>%
    pull(fake_student_id) %>%
    intersect(erasmus %>% pull(fake_student_id))
  
  gpa_df <- affiliations %>%
    filter(type == 2, !is.na(cgpa)) %>%
    distinct(fake_student_id, cgpa) %>%
    mutate(
      Grup = if_else(fake_student_id %in% grad_erasmus_ids, "Erasmus", "Non-Erasmus")
    )
  
  median_df <- gpa_df %>%
    group_by(Grup) %>%
    summarise(medyan = median(cgpa, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(label = sprintf("%.2f", medyan))
  
  return(list(data = gpa_df, medians = median_df))
}

# Erasmus summary
get_erasmus_summary <- function(affiliations, erasmus, universities) {
  valid_ids <- affiliations %>% filter(type %in% c(2,3)) %>% pull(fake_student_id) %>% unique()
  eras_country <- erasmus %>%
    filter(fake_student_id %in% valid_ids) %>%
    inner_join(universities %>% select(id, name, country), by = c("universityid" = "id")) %>%
    filter(!is.na(country) & country != "") %>%
    group_by(country) %>%
    summarise(
      n            = n_distinct(fake_student_id),
      universities = paste(unique(name), collapse = ", ")
    ) %>%
    ungroup() %>%
    arrange(desc(n))
  total_countries <- n_distinct(eras_country$country)
  total_students  <- sum(eras_country$n)
  return(list(data = eras_country, total_countries = total_countries, total_students = total_students))
}

# Method summary
get_method_summary <- function(works, employment_methods, affiliations, companies) {
  valid_ids <- affiliations %>% filter(type %in% c(2,3)) %>% pull(fake_student_id) %>% unique()
  method_dist <- works %>%
    filter(fake_student_id %in% valid_ids, !is.na(employmentmethod) & employmentmethod != "",
           !is.na(companyid) & companyid != "") %>%
    distinct(fake_student_id, employmentmethod, companyid) %>%
    inner_join(employment_methods %>% select(id, name), by = c("employmentmethod" = "id")) %>%
    inner_join(companies %>% select(id, name), by = c("companyid" = "id"))
  
  method_summary <- method_dist %>%
    group_by(name.x) %>%
    summarise(
      n         = n_distinct(fake_student_id),
      companies = paste(unique(name.y), collapse = ", ")
    ) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    mutate(
      pct   = n / sum(n),
      label = percent(pct, accuracy = 0.1),
      name  = name.x
    )
  total_methods <- n_distinct(method_summary$name)
  return(list(data = method_summary, total_methods = total_methods))
}

# --- Plotting Functions ---
plot_gender <- function(data) {
  ggplot(data, aes(x = Cinsiyet, y = oran, fill = Cinsiyet)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = etiket), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "Mezunların Cinsiyet Dağılımı", x = "Cinsiyet", y = "Oran") +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_company <- function(data) {
  ggplot(data$data, aes(y = name, x = yuzde, fill = name)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = etiket), hjust = 0, size = 3) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = paste0("Mezunların Çalıştığı Şirketler (Toplam ", data$total, " şeferik)"),
         x = "Yüzde (%)", y = "Şirket") +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_province <- function(data) {
  ggplot(data, aes(x = reorder(city, -n), y = n, fill = city)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = label), vjust = -0.5, size = 3) +
    labs(title = "En Çok Öğrenci/Mezun Gelen İlk 10 İl", x = "İl", y = "Kişi Sayısı") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_position <- function(data) {
  ggplot(data$data, aes(x = pct, y = reorder(position, pct), fill = position)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = label), hjust = -0.1, size = 3) +
    scale_x_continuous(labels = percent_format(), limits = c(0, max(data$data$pct)*1.1)) +
    labs(title = "Mezunların En Çok Çalıştığı İlk 10 Pozisyon", x = "Oran", y = "Pozisyon") +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_cgpa <- function(data) {
  ggplot(data$data, aes(x = Grup, y = cgpa, fill = Grup)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.15, size = 1, alpha = 0.3, na.rm = TRUE) +
    geom_label(data = data$medians, aes(x = Grup, y = medyan, label = label),
               nudge_y = 0.1, fill = "white", size = 4) +
    labs(title = "Mezunların CGPA Dağılımı", x = NULL, y = "CGPA") +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_erasmus <- function(data) {
  plot_ly(
    data = data$data,
    y    = ~reorder(country, n),
    x    = ~n,
    type = "bar",
    orientation = "h",
    text = ~n,
    textposition = "auto",
    hovertemplate = paste(
      "Ülke: %{y}<br>",
      "Öğrenci Sayısı: %{x}<br>",
      "Üniversiteler: %{customdata}<extra></extra>"
    ),
    customdata = ~universities,
    marker = list(color = "#2C3E50")
  ) %>%
    layout(
      title   = paste0("Erasmus Ülke Dağılımı (", data$total_countries, " ülke, ", data$total_students, " öğrenci)"),
      xaxis   = list(title = "Öğrenci Sayısı"),
      yaxis   = list(title = "Ülke")
    )
}

plot_methods_interactive <- function(data) {
  plot_ly(
    data = data$data,
    y    = ~reorder(name, n),
    x    = ~n,
    type = "bar",
    orientation = "h",
    text = ~label,
    textposition = "auto",
    hovertemplate = paste(
      "Metot: %{y}<br>",
      "Kişi: %{x}<br>",
      "Yüzde: %{text}<br>",
      "Şirketler: %{customdata}<extra></extra>"
    ),
    customdata = ~companies,
    marker = list(color = "#2C3E50")
  ) %>%
    layout(
      title = paste0("İşe Giriş Metotları (", data$total_methods, " metot)")
    )
}

# --- Load Data ---
cat("Starting data load...\n")
data <- load_data()

# Extract Data Frames
affiliations        <- data[["affiliations"]]
users               <- data[["users"]]
works               <- data[["works"]]
companies           <- data[["companies"]]
industries          <- data[["industries"]]
highschools         <- data[["highschools"]]
positions           <- data[["positions"]]
erasmus             <- data[["erasmus"]]
universities        <- data[["universities"]]
employment_methods  <- data[["employmentmethod"]]

# --- UI ---
header <- dashboardHeader(title = "Hacettepe Ü. İ. Müh. Gösterge Paneli")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Genel Bilgiler", tabName = "general", icon = icon("info-circle")),
    menuItem("İş Bilgileri", tabName = "eda", icon = icon("chart-bar")),
    menuItem("Analysis", tabName = "analysis", icon = icon("line-chart")),
    menuItem("Diğer", tabName = "details", icon = icon("table"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "general",
      fluidRow(
        box(
          width = 12,
          title = "Project Overview and Scope",
          status = "primary",
          solidHeader = TRUE,
          p("Bu projede, Hacettepe Üniversitesi Endüstri Mühendisliği bölümü mezun lisans öğrencilerinin kariyer bilgileri incelenerek analizler yapılmıştır.")
        ),
        box(
          width = 12,
          title = "Data Source",
          status = "primary",
          solidHeader = TRUE,
          p("Veri anonim olarak üniversite bilgi sisteminden sağlanmıştır."),
          tags$ul(
            tags$li("GPA (Sayısal)"),
            tags$li("Mezun olunan lise türü"),
            tags$li("Erasmus+ katılım durumu"),
            tags$li("İşe başlama süresi (ay)"),
            tags$li("Çalışılan şirket"),
            tags$li("İş pozisyonu"),
            tags$li("Cinsiyet")
          )
        ),
        box(
          width = 12,
          title = "General Summary",
          status = "primary",
          solidHeader = TRUE,
          textOutput("generalSummary")
        )
      ),
      fluidRow(
        valueBoxOutput("countriesBox",  width = 3),
        valueBoxOutput("industriesBox", width = 3),
        valueBoxOutput("positionsBox",  width = 3),
        valueBoxOutput("employersBox",  width = 3)
      ),
      fluidRow(
        box(
          width = 6,
          title = "Cinsiyet Dağılımı",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("genderPlot")
        ),
        box(
          width = 6,
          title = "İl Dağılımı",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("provincePlot")
        )
      )
    ),
    tabItem(
      tabName = "eda",
      fluidRow(
        box(
          width = 6,
          title = "Çalışılan Şirketler",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("companyPlot")
        ),
        box(
          width = 6,
          title = "Pozisyon Dağılımı",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("positionPlot")
        )
      )
    ),
    tabItem(
      tabName = "analysis",
      fluidRow(
        box(
          width = 6,
          title = "CGPA Dağılımı",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("cgpaPlot")
        ),
        box(
          width = 6,
          title = "Erasmus Ülke Dağılımı",
          status = "primary",
          solidHeader = TRUE,
          p("Bar üzerine gelerek detay görebilirsiniz."),
          plotlyOutput("erasmusPlot")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "İşe Giriş Metotları",
          status = "primary",
          solidHeader = TRUE,
          p("Bar üzerine gelerek detay görebilirsiniz."),
          plotlyOutput("methodPlot")
        )
      )
    ),
    tabItem(
      tabName = "details",
      fluidRow(
        box(
          width = 12,
          title = "Details",
          status = "primary",
          solidHeader = TRUE,
          p("Gelecekteki detaylı tablolar için ayrılmıştır.")
        )
      )
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

# --- Server ---
server <- function(input, output, session) {
  # Hazırlık
  gender_data  <- get_gender_summary(affiliations, users)
  general_data <- get_general_summary(works, companies, industries)
  company_data <- get_company_summary(works, companies)
  province_data <- get_province_summary(affiliations, users, highschools)
  position_data <- get_position_summary(affiliations, works, positions)
  cgpa_data    <- get_cgpa_summary(affiliations, erasmus)
  erasmus_data <- get_erasmus_summary(affiliations, erasmus, universities)
  method_data  <- get_method_summary(works, employment_methods, affiliations, companies)
  
  # Text
  output$generalSummary <- renderText({ general_data$text })
  
  # Value Boxes
  output$countriesBox <- renderValueBox({
    valueBox(general_data$counts$countries, "Çalışılan Ülke", icon = icon("globe"), color = "aqua")
  })
  output$industriesBox <- renderValueBox({
    valueBox(general_data$counts$industries, "Çalışılan Endüstri", icon = icon("industry"), color = "green")
  })
  output$positionsBox <- renderValueBox({
    valueBox(general_data$counts$positions, "Çalışılan Pozisyon", icon = icon("briefcase"), color = "yellow")
  })
  output$employersBox <- renderValueBox({
    valueBox(general_data$counts$employers, "Çalışılan İşveren", icon = icon("building"), color = "purple")
  })
  
  # Plots
  output$genderPlot   <- renderPlot({ plot_gender(gender_data) })
  output$companyPlot  <- renderPlot({ plot_company(company_data) })
  output$provincePlot <- renderPlot({ plot_province(province_data) })
  output$positionPlot <- renderPlot({ plot_position(position_data) })
  output$cgpaPlot     <- renderPlot({ plot_cgpa(cgpa_data) })
  output$erasmusPlot  <- renderPlotly({ plot_erasmus(erasmus_data) })
  output$methodPlot   <- renderPlotly({ plot_methods_interactive(method_data) })
}

# --- Run App ---
shinyApp(ui, server)
