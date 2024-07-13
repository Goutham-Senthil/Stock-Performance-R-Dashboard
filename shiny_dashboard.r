library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(shinythemes)
library(RColorBrewer)

# Load and prepare the data
df <- read_excel("C:/Users/gouth/Downloads/stock portfolio performance data set.xlsx")
correct_names <- c("ID", "Large B/P", "Large ROE", "Large S/P", "Large Return Rate in the last quarter",
                   "Large Market Value", "Small systematic Risk", "Original Investment Annual Return",
                   "Original Investment Excess Return", "Original Investment Systematic Risk",
                   "Original Investment Total Risk", "Original Investment Abs. Win Rate",
                   "Original Investment Rel. Win Rate", "Normalized Investment Annual Return",
                   "Normalized Investment Excess Return", "Normalized Investment Systematic Risk",
                   "Normalized Investment Total Risk", "Normalized Investment Abs. Win Rate",
                   "Normalized Investment Rel. Win Rate")
names(df) <- correct_names
df <- df[2:64,]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Performance Portfolio 20BDS0330", titleWidth = 600),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 4, box(width = "100%", plotOutput('plot1'))),
      column(width = 4, box(width = "100%", plotOutput('plot2'))),
      column(width = 4, box(width = "100%", plotOutput('plot3')))
    ),
    fluidRow(
      column(width = 6, box(width = "100%", plotOutput('plot4'))),
      column(width = 6, box(width = "100%", plotOutput('plot5')))
    )
  ),
  skin = "purple"
)

# Define server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(df, aes(x = "", fill = factor(df$`Small systematic Risk`))) +
      geom_bar(width = 1) +
      labs(fill = "Class", title = "Possible Systematic Risks") +
      coord_polar(theta = "y", start = 0) +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "#ffffff", hjust = 0.5),
        legend.background = element_rect(fill = "#17202A"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")
      ) +
      scale_fill_brewer(palette = "Accent")
  })

  output$plot2 <- renderPlot({
    ggplot(df, aes(df$`Large Market Value`)) +
      geom_density(aes(fill = factor(df$`Large ROE`)), alpha = 0.8) +
      labs(title = "Large Market Value Grouped by ROE", x = "Market Share", y = "Density", fill = "# ROE") +
      scale_fill_manual(values = c("#00BFFF", "#FFD700", "#008000", "#FF8C00", "#9400D3", "#FF6347", "#FF1493")) +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff", hjust = 0.5),
        legend.background = element_rect(fill = "#17202A"),
        legend.text = element_text(color = "#ffffff"),
        legend.title = element_text(color = "white")
      )
  })

  output$plot3 <- renderPlot({
    ggplot(df, aes(x = df$`Large B/P`)) +
      geom_bar(color = "lightblue", fill = "#FDFD96") +
      labs(title = "Unique Large B/P values", x = "Large B/P", y = "Count") +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff", hjust = 0.5),
        legend.position = "none"
      )
  })

  output$plot4 <- renderPlot({
    plot(df$`Normalized Investment Annual Return`, type = "o", col = "green", xlab = "Stocks ID", ylab = "Percentage",
         main = "Annual Return vs Annual Risk", fg = "white", col.lab = "white", col.axis = "white", col.main = "white")
    lines(df$`Normalized Investment Total Risk`, type = "o", col = "red")
  })

  output$plot5 <- renderPlot({
    ggplot(df, aes(x = df$`Normalized Investment Annual Return`, y = df$`Normalized Investment Excess Return`)) +
      geom_point(shape = 6, fill = "cyan", color = "gold") +
      labs(title = "Correlation between Annual Return and Excess Return", x = "Annual Return", y = "Excess Return") +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff", hjust = 0.5),
        legend.position = "none"
      )
  })
}

# Run the application
shinyApp(ui, server)
