
# Plot bivariate relationship with GPP
plot_bi <- function(df, x_var, x_label) {
  ggplot(df, aes(x = .data[[x_var]], y = GPP_NT_VUT_REF)) +
    geom_point(size = 0.75, alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    labs(
      x = x_label,
      y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, " s"^-1, ")"))
    ) +
    theme_classic()
}

# Combine and save multiple plots
save_combined_plot <- function(plot_list, 
                               filename = "re_stepwise_combined_plots.png",
                               save_path = here::here("data/figures"),
                               width = 12,
                               height = 12) {
  
  combined <- plot_grid(
    plotlist = plot_list,
    ncol = 3,
    align = "v"
  )
  
  final_plot <- combined
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  ggsave(
    file.path(save_path, filename),
    plot = final_plot,
    width = width,
    height = height
  )
}


# Convert text to plotmath expression
to_expression <- function(text) {
  text <- gsub(" ", "~", text)
  text <- gsub("CO₂", "CO[2]", text)
  parse(text = text)
}

# Compare model metrics
plot_model_metrics <- function(models) {
  metrics <- data.frame(
    Variable = unlist(models$var_names),
    AIC = unlist(models$AICs),
    R2 = unlist(models$R2s)
  )
  
  metrics_long <- pivot_longer(
    metrics,
    cols = c("AIC", "R2"),
    names_to = "Metric",
    values_to = "Value"
  ) |>
    mutate(Value_scaled = ifelse(Metric == "R2", Value * 1e5, Value))
  
  ggplot(metrics_long, aes(x = reorder(Variable, Value), y = Value_scaled, fill = Metric)) +
    
    # Draw bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    
    # Add R² value labels above bars
    geom_text(
      data = subset(metrics_long, Metric == "R2"),
      aes(label = round(Value, 2)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      hjust = -0.2,
      size = 3,
      color = "grey40"  # Match axis/grid color
    ) +
    
    # Flip coordinates for horizontal bars
    coord_flip() +
    
    # Chart and axis titles
    labs(
      # title = "Single Predictor Model Comparison",
      x = "Predictor",
      y = "Metric Value"
    ) +
    
    # Manual color scale
    scale_fill_manual(
      values = c("AIC" = "#E69F00", "R2" = "#56B4E9"),
      labels = c("AIC", expression(R^2))
    ) +
    
    # Y-axis and secondary axis
    scale_y_continuous(
      name = "AIC",
      sec.axis = sec_axis(~./1e5, name = expression(R^2))  # Adjust if needed
    ) +
    
    # Proper parsing of variable names
    scale_x_discrete(labels = function(x) parse(text = to_expression(x))) +
    
    # Theme and formatting
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(10, 30, 10, 10),
      # plot.clip = "off",
      # panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),  # Horizontal dashed grid lines
    )
  
}