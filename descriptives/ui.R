library(shiny)
library(gplots)

theme_set(theme_bw(base_size = 24))

shinyUI(fixedPage(
  titlePanel("TechSoup Scratch"),
        div(class="row",
            column(6,dateRangeInput("date_range",label = h4("Transactions between"),start=min(.ts.data$Transaction_Date),end=max(.ts.data$Transaction_Date)))),

    tabsetPanel(
            tabPanel("Descriptives",

                     div(class="row",
                         column(6,plotOutput("h_log_budget")),
                         column(6,plotOutput("h_log_liscenses"))
			),
                     div(class="row",
                         column(6,plotOutput("h_n_items_per_org")),
                         column(6,plotOutput("h_log_n_items_per_vendor"))
                         ),
                     div(class="row",
                         column(6,plotOutput("h_log_n_licenses_per_org")),
                         column(6,plotOutput("h_log_n_licenses_per_vendor"))
                         ),
                     div(class="row",
                         column(6,plotOutput("h_log_n_orders_per_org")),
                         column(6,plotOutput("h_log_n_orders_per_vendor"))
                         ),
                     div(class="row",
                         column(6,plotOutput("h_org_subtype_per_type"))
                         ),
                    div(class="row",
                        column(6,plotOutput("gr_log_budget_value")),
                         column(6,plotOutput("gr_log_budget_revenue"))
			),
                     div(class="row",
                        column(6,plotOutput("gr_n_org_per_budget"))
                         )
                     )
	)))
