#' Create a role-skills heatmap
#'
#' This function takes a dataset of skills, roles, and values,
#' and produces a heatmap visualization.
#'
#' @param data A data frame with columns: skill, Role, value
#' @return A ggplot2 heatmap object
#' @examples
#' test <- data.frame(
#'   skill = c("R", "Python", "SQL"),
#'   Role = c("Data Scientist", "Data Engineer", "Analyst"),
#'   value = c(5, 3, 4)
#' )
#' role_skills_heatmap(test)
#' @export
role_skills_heatmap <- function(data) {
    data$value <- as.numeric(data$value)

    importance_labels <- c("Not required", "Nice to have", "Important", "Critical")

    ggplot2::ggplot(data, ggplot2::aes(x = Role, y = skill, fill = value)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradientn(
            colors = c("#e6dcff", "#a100ff", "#7500c0", "#460073"),
            name = "Importance",
            breaks = c(0, 1, 2, 3),
            labels = importance_labels
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = ggplot2::element_text(size = 9),
            panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(
            title = "Role-skill Importance Heatmap",
            x = "Role",
            y = "Skill"
        )
}