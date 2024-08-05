ui <- fluidPage(
  theme = shinytheme("paper"),
  useWaiter(),
  titlePanel("Central limit theorem"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  sidebarLayout(

    sidebarPanel(
      id = "sidebar",
      uiOutput("choose_distr"),
      uiOutput("choose_mu"),
      uiOutput("choose_sigma"),
      uiOutput("choose_nu"),
      uiOutput("choose_n"),
      uiOutput("choose_R", style="margin-bottom:10px;"),
      materialSwitch(inputId = "group2_ui", label = "Add group 2",
                      value = FALSE, status = "primary"),
      conditionalPanel(
        condition = "input.group2_ui == true",
        uiOutput("group2_sliders", style = "margin-top:13px;")
      ),
      actionButton("go", "Simulate", style = "margin-top:13px;"),
      p(""),
      p("App developed by DLC and NAM / 20240125", style = "color:#FFFFF0;"),
      img(
        src = paste0("https://www.cruk.cam.ac.uk/wp-content/themes/",
                    "cambridge-theme/images/interface/main-logo-small.png"),
        width = 100
      )
    ),

    mainPanel(align = "center",

      tabsetPanel(id = "base_panel",
        tabPanel(uiOutput("density_tab_title"),
                  conditionalPanel(
                    condition = "input.group2_ui == false",
                    fluidRow(
                      column(2, h6(HTML(rep("<br>", 12)),
                      style = "font-size: 8px;")),
                      column(6, h6(""), tableOutput("mean_table")),
                      column(2)
                    ),
                    plotlyOutput(outputId = "density") %>% 
                    shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                    size = .7, hide.ui = TRUE),
                    plotlyOutput(outputId = "samples_plot", height = "150px"),
                    htmlOutput("tab1_legend")),

                conditionalPanel(
                    condition = "input.group2_ui == true",
                    fluidRow(
                      column(2, h6(HTML(rep("<br>", 12)))),
                      column(6, h6(""), tableOutput("mean_table_group")),
                      column(2)
                    ),
                    plotlyOutput(outputId = "density_2groups") %>%
                    shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                    size = .7, hide.ui = TRUE),
                    plotlyOutput(outputId = "group1_samples_plot",
                    height = "150px"),
                    plotlyOutput(outputId = "group2_samples_plot",
                    height = "150px"),
                    htmlOutput("tab1_legend_group")),
                  ),

        tabPanel(uiOutput("mean_tab_title"),
                  conditionalPanel(
                    condition = "input.group2_ui == true",
                    plotOutput("diffmean", height = "360px") %>%
                    shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                    size = .7, hide.ui = TRUE)
                  ),
                  plotOutput("mean", height = "360px") %>%
                    shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                    size = .7, hide.ui = TRUE),
                  htmlOutput("tab2_legend")),

        tabPanel("Estimated coverage of CIs",
                  conditionalPanel(
                    condition = "input.group2_ui == true",
                    fluidRow(
                      column(2, h6(HTML(rep("<br>", 12)),
                      style = "font-size: 12px;")),
                      column(6, h6(""), tableOutput("ci_summary")),
                      column(2)
                    ),

                    uiOutput("choose_ttest", align = "left"),
                    plotlyOutput("ci_compare") %>%
                    shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                size = .7, hide.ui = TRUE),
                  htmlOutput("tab3_legend_group")
                  ),
                conditionalPanel(
                  condition = "input.group2_ui == false",
                    fluidRow(
                      column(2, h6(HTML(rep("<br>", 12)),
                      style = "font-size: 12px;")),
                      column(6, h6(""), tableOutput("onesample_CI_table")),
                      column(2)
                    ),
                plotlyOutput(outputId = "coverage") %>%
                shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                size = .7, hide.ui = TRUE),
                htmlOutput("tab3_legend")
                )
                ),

        tabPanel("P-values",
                  textOutput("ttest_header"),
                  plotOutput("pvalue") %>%
                  shinycssloaders::withSpinner(type = 1, color = "#1e81b0",
                                                  size = .7, hide.ui = TRUE),
                  textOutput("ttest_text"),
                  conditionalPanel(
                    condition = "input.group2_ui == true",
              fluidRow(
                  column(4, h6("Student's t-test", style = "font-size: 12px;"),
                  tableOutput("ttest.std")),
                  column(4, h6("Welch t-test",     style = "font-size: 12px;"),
                  tableOutput("ttest.wlch")),
                  column(4, h6("Wilcoxon test",  style = "font-size: 12px;"),
                  tableOutput("ttest.wilc"))
                  ),
                htmlOutput("tab4_legend_group")),
                conditionalPanel(
                  condition = "input.group2_ui == false",
                  fluidRow(
                    column(4, h6("Student's t-test",
                    style = "font-size: 12px;"),
                    tableOutput("ttest.std_onesample")),
                    column(4, h6("Wilcoxon test",
                    style = "font-size: 12px;"),
                    tableOutput("ttest.wilc_onesample"))
                  ),
                  htmlOutput("tab4_legend"))
        )
        )
      )
    ))
