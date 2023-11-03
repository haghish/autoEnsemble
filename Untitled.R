print(MODEL3 <- MODEL +
  xlab("Features") +
  ylab("SHAP contribution") +
  ggtitle("") +
  theme_classic() +
  labs(colour = "Normalized values") +

  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(unit(0, "cm")),
    legend.text = element_text(colour = "black", size = 6, face = "bold"),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.5, "cm"),

  )
)

print(MODEL4 <- MODEL2 +
        xlab("Features") +
        ylab("SHAP contribution") +
        ggtitle("") +
        theme_classic() +
        labs(colour = "Normalized values") +
        guides(colour = guide_legend(title.position = "top",
                                     title.hjust = 0.5,
                                     # how can I pull the legend a little towards the top and reduce empty space above it?

                                     #reduce empty space above the legend
                                     #legend.margin = margin(t = -1, unit = "cm"),
                                     override.aes = list(size = 2)
        )) +
        theme(
          legend.position = "top",
          legend.justification = "right",
          #legend.margin = margin(unit(0, "cm")),
          #legend.text = element_text(colour = "black", size = 6, face = "bold"),
          #legend.key.height = unit(0.4, "cm"),
          #legend.key.width = unit(0.5, "cm")
        )
)
