#### Dependencies ####

library(tidyverse)
library(viridis)
library(umap)



#### UI ####

ui <- fluidPage(

  # Application title
  titlePanel("Log Relative Abundance of Select Clostridiales"),
  sidebarLayout(
    sidebarPanel(
      # Define inputs as an rds file
      fileInput("df_data", "Choose .rds file containing data",
        accept = c("rds")
      )
    ),

    # Define UMAP plot as output
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", tableOutput("summary_table")),
        tabPanel("Box Plots", plotOutput("box_plots", height = 800, width = 500)),
        tabPanel("Density Plots", plotOutput("density_plots", height = 800, width = 500)),
        tabPanel("UMAP", plotOutput("umap_plot", height = 600, width = 600))
      )
    )
  )
)



#### Server ####

server <- function(input, output, session) {
  # Define reused variables
  color_vector <- c(
    alpha("grey59", alpha = 0.7),
    viridis(2, alpha = 0.7, begin = 0.4, end = 0.7, option = "D")
  )


  #### Input-dependent variables ####

  # Reactive expression that reads the file
  df_data <- reactive({
    req(input$df_data)

    readRDS(input$df_data$datapath)
  })

  # Reformat data for box and density plots as well as summary table.
  df_data_tall <- reactive({
    df_data_tall <- df_data()
    if (is.null(df_data_tall)) {
      return(NULL)
    }

    df_data_tall <- df_data_tall %>%
      pivot_longer(cols = c(
        `R. intestinalis`, `E. hallii`,
        `F. prausnitzii`, `A. caccae`
      ))

    df_data_tall
  })



  #### Outputs ####

  # Tabular overview of data
  output$summary_table <- renderTable({
    summary_df <- df_data_tall()
    if (is.null(summary_df)) {
      return()
    }

    summary_df %>%
      group_by(AJCC_stage, name) %>%
      summarise(
        N = n(),
        Median = median(value, na.rm = TRUE),
        Mean = mean(value, na.rm = TRUE),
        SD = sd(value, na.rm = TRUE)
      )
  })

  # Box plots
  output$box_plots <- renderPlot({
    box_plots_df <- df_data_tall()
    if (is.null(box_plots_df)) {
      return()
    }

    box_plots <- ggplot(box_plots_df, aes(x = name, y = value, fill = AJCC_stage)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(aes(col = AJCC_stage),
        position = position_jitterdodge(jitter.width = 0.1)
      ) +
      coord_flip() +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      xlab("") +
      ylab("log. rel. ab.") +
      labs(col = "Group") +
      scale_fill_manual(
        values = color_vector,
        guide = "none"
      ) +
      scale_colour_manual(values = color_vector) +
      theme_bw() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

    box_plots
  })

  # Density plots
  output$density_plots <- renderPlot({
    density_plots_df <- df_data_tall()
    if (is.null(density_plots_df)) {
      return()
    }

    density_plots <- ggplot(density_plots_df, aes(x = value, fill = AJCC_stage)) +
      geom_density() +
      facet_wrap(~name, scales = "free_y", ncol = 1) +
      xlab("log. rel. ab.") +
      labs(fill = "Group") +
      scale_fill_manual(values = color_vector) +
      theme_bw()

    density_plots
  })

  # UMAP
  output$umap_plot <- renderPlot({
    umap_df <- df_data()
    if (is.null(umap_df)) {
      return()
    }

    umap_object <- umap_df %>%
      dplyr::select_if(is.numeric) %>%
      umap()

    umap_plot_data <- data.frame(umap_object$layout,
      Group = umap_df$AJCC_stage
    )

    umap_plot <- ggplot(umap_plot_data, aes(x = X1, y = X2)) +
      geom_point(size = 2, aes(color = Group)) +
      theme_bw() +
      xlab("Component 1") +
      ylab("Component 2") +
      scale_color_manual(values = color_vector)

    umap_plot
  })
}


#### Run the app ####

shinyApp(ui, server)
