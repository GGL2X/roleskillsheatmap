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

    data <- tidyr::complete(data, Role, skill, fill = list(value = 0))

    importance_labels <- c("Not required", "Nice to have", "Important", "Critical")

    ggplot2::ggplot(data, ggplot2::aes(x = Role, y = skill, fill = value)) +
        ggplot2::geom_tile(color = "white", size = 0.5, width = 0.95, height = 0.95) +
        ggplot2::scale_fill_gradientn(
            colors = c("#e6dcff","#c2a3ff" "#a100ff", "#7500c0", "#460073"),
            values = scales::rescale(c(0, 1, 2, 3)),
            name = "Importance",
            breaks = c(0, 1, 2, 3),
            labels = importance_labels
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(aspect.ratio = 0.75) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 40, hjust = 1, size = 11,
                margin = ggplot2::margin(t = 12)
            ),
            axis.text.y = ggplot2::element_text(
                size = 11,
                margin = ggplot2::margin(r = 12)
            ),
            axis.title.x = ggplot2::element_text(
                size = 13,
                margin = ggplot2::margin(t = 15)
            ),
            axis.title.y = ggplot2::element_text(
                size = 13,
                margin = ggplot2::margin(r = 15)
            ),
            plot.title = ggplot2::element_text(
                size = 16, face = "bold",
                margin = ggplot2::margin(b = 15)
            ),
            panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(
            title = "Role-skill Importance Heatmap",
            x = "Role",
            y = "Skill"
        )
}