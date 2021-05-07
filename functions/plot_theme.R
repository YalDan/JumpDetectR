plot_theme <- theme(panel.border = element_blank(),
                    axis.text = element_text(size = 24, face = "bold"),
                    axis.title = element_text(size = 34, face = "bold"),
                    strip.text = element_text(size = 24, face = "bold"),
                    plot.title = element_text(size = 34, face = "bold", hjust = .5),
                    panel.background = element_rect(fill = "transparent"), # bg of the panel
                    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                    panel.grid.major = element_blank(), # get rid of major grid
                    panel.grid.minor = element_blank(), # get rid of minor grid
                    legend.position = "none"
)