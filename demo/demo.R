#### RSPI in 60 Seconds ####
# Steve Martin
# Dec 18 2020

#---- Setup ----
# Attached libraries
library(piar)
library(gpindex)
library(rspiSynth)
# Function to identify outliers
outlier <- function(x, g, cutoff = log(2)) {
  x <- log(x)
  g <- as.factor(g)
  num <- abs(x - ave(x, g, FUN = function(z) median(z, na.rm = TRUE)))
  denom <- ave(x, g, FUN = function(z) mad(z, na.rm = TRUE))
  num / denom > cutoff
}
# Names of price columns
prices <- c("RetailPrice", "VendorPrice", "MarginPrice")
price_relative <- function(x, period, product) x / back_price(x, period, product)

#---- Bring in data ----
sc <- sample_classification()
pp <- merge(product_data(periods = months_between(2011, 2020)), sc[c("PPDId", "Classification")])

#---- Wrangle data ----
# Remove negative margins
pp$MarginPrice[pp$MarginPrice <= 0] <- NA
# Make price relatives
pp[prices] <- lapply(pp[prices], price_relative, pp$SurveyCycleCode, pp$ProductId)
# Remove outliers
group <- interaction(pp$Classification, pp$SurveyCycleCode)
pp[prices] <- lapply(pp[prices], function(x) replace(x, outlier(x, group), NA))

#---- Price index aggregation structure ----
pias <- do.call(aggregation_structure, 
               c(expand_classification(paste0(sc$Classification, substr(sc$Stratum, 2, 2))), 
                 list(sc$PPDId), 
                 weights = list(sc$Weight)))

#---- Calculate elementary price indexes ----
epr <- lapply(pp[prices], elemental_index, pp$PPDId, pp$SurveyCycleCode)

#---- Aggregate ----
rspi <- lapply(epr, aggregate, pias)

#---- Plot ----
library(shiny)
library(zoo)

ui <- fluidPage(
  titlePanel("Retail Services Price Index"),
  sidebarLayout(
    sidebarPanel(
      selectInput("naics",
                  "Index Level",
                  names(rspi$RetailPrice$index[[1]])[1:100]),
      selectInput("index",
                  "Index Series",
                  c(`Retail Price`= "RetailPrice",
                    `Vendor Price` = "VendorPrice",
                    `Margin Price` = "MarginPrice"))
    ),
    mainPanel(
      plotOutput("distPlot", height = "800px")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    y <- rspi[[input$index]][[input$naics]] * 100
    x <- as.yearmon(names(y), format = "%Y%m")
    plot(x, y, type = "l", bty = "n", xlab = "", ylab = "", las = 1)
  })
}

shinyApp(ui = ui, server = server)
