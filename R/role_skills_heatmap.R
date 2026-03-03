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
#' Create a role-skills heatmap
#'
#' This function takes a dataset of skills, roles, and values,
#' and produces a heatmap visualization.
#'
#' @param data A data frame with columns: skill, Role, value
#' @return A ggplot2 heatmap object
#' @export
role_skills_heatmap <- function(data) {
    data$value <- as.numeric(data$value)

    data <- tidyr::complete(data, Role, skill, fill = list(value = 0))

    importance_labels <- c("Not required", "Nice to have", "Important", "Critical")

    n_roles  <- length(unique(data$Role))
    n_skills <- length(unique(data$skill))

    raw_aspect <- n_skills / pmax(1, n_roles)
    dynamic_aspect <- max(0.4, min(2.5, raw_aspect))

    x_text_size <- if (n_roles > 12) 8 else 11
    y_text_size <- if (n_skills > 15) 8 else 11

    bottom_margin <- if (n_roles > 12) 20 + (n_roles - 12) * 1.5 else 12
    left_margin   <- if (n_skills > 15) 12 + (n_skills - 15) * 1.2 else 12

    ggplot2::ggplot(data, ggplot2::aes(x = Role, y = skill, fill = value)) +
        ggplot2::geom_tile(color = "white", size = 0.5) +
        ggplot2::scale_fill_gradientn(
            colors = c("#e6dcff", "#c2a3ff", "#a100ff", "#7500c0", "#460073"),
            values = scales::rescale(c(0, 1, 2, 3)),
            name = "Importance",
            breaks = c(0, 1, 2, 3),
            labels = importance_labels
        ) +

        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(aspect.ratio = dynamic_aspect) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 40, hjust = 1, size = x_text_size,
                margin = ggplot2::margin(t = bottom_margin)
            ),
            axis.text.y = ggplot2::element_text(
                size = y_text_size,
                margin = ggplot2::margin(r = left_margin)
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
            panel.grid = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = bottom_margin + 5, l = left_margin + 5)
        ) +
        ggplot2::labs(
            title = "Role-skill Importance Heatmap",
            x = "Role",
            y = "Skill"
        )
}