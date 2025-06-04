process_models <- function(df_clean){
  # Create an empty list to store each plot
  plot_list <- list()
  
  meta_data <- list(
    variable = c("Solar.R", "Wind", "Temp"),
    formula = c("Ozone ~ Solar.R", "Ozone ~ Wind", "Ozone ~ Temp"),
    lab_txt = c("Solar Radiation (lang)", "Wind Speed (mph)", "Temperature (°F)")
  )
  
  for (i in 1:length(meta_data$variable)) {
    # Dynamically generate the regression formula
    formula <- as.formula(meta_data$formula[i])
    
    # Compute the regression model
    model <- lm(formula, data = df_clean)
    summary_model <- summary(model)
    
    # Extract R² and p-value
    r_squared <- round(summary_model$r.squared, 3)
    p_value <- summary_model$coefficients[2, 4]  # p-value is the 4th element in the second row
    
    # Format p-value text
    p_text <- if (p_value < 0.005) {
      "P < 0.005"
    } else {
      paste("P =", round(p_value, 3))
    }
    
    # Create annotation text
    annotation_text <- paste("R² =", r_squared, "\n", p_text)
    
    # Plot each variable against Ozone
    suppressWarnings(
      p <- ggplot(df_clean, aes_string(x = meta_data$variable[i], y = "Ozone")) +
        geom_point(color = "black", alpha = 0.8) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "red",
          linewidth = 0.5
        ) +
        annotate(
          "text",
          x = max(df_clean[[meta_data$variable[i]]], na.rm = TRUE) - 5,
          y = max(df_clean$Ozone, na.rm = TRUE) - 10,
          label = annotation_text,
          hjust = 1,
          size = 3,
          color = "black"
        ) +
        labs(x = meta_data$lab_txt[i], y = NULL) +
        theme_classic()
    )
    # Add the plot to the list
    plot_list[[i]] <- p
  }
  
  # Return the list containing all the plots
  return(plot_list)
}