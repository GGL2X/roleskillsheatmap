role_skills_heatmap <- function(data) {
    library(ggplot2)
    data$value <- as.numeric(data$value)  # match lowercase column name

    importance_labels <- c("Not required", "Critical", "Important", "Nice to have")

    ggplot(data, aes(x = Role, y = skill, fill = value)) +
        geom_tile(color = "white") +   # use geom_tile for heatmap squares
        scale_fill_gradientn(
            colors = c("#f2e6ff", "#c084fc", "#9333ea", "#4b0082"),
            name = "Importance",
            breaks = c(0, 1, 2, 3),
            labels = importance_labels
        ) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9),
            panel.grid = element_blank()
        ) +
        labs(
            title = "Role-skill Importance Heatmap",
            x = "Role",
            y = "Skill"
        )
}