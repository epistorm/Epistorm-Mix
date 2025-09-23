#### Requires constructing_contact_matrices.R ####

ggplot(age_matrix_ls[[1]]) +
  geom_raster(aes(x = resp_age_plot, y = cont_age_plot, fill = pred), interpolate = TRUE) +
  ggtitle("Title") + # Main title
  xlab("Respondent Age") + # X-axis title
  ylab("Contact Age") + # Y-axis title
  scale_fill_gradientn(
    colors = c("#005f87", "#87afd7", "white"),
    values = c(0, 0.4, 1),
    name = "" # Legend title
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80)) +
  scale_x_continuous(breaks = c(0,20,40,60,80)) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA)#,
  )

ggplot(by5_matrix) +
  geom_tile(aes(x = resp_age_by5_80, y = cont_age_by5_80, fill = ncont)) +
  ggtitle("Total") + # Main title
  xlab("Respondent Age") + # X-axis title
  ylab("Contact Age") + # Y-axis title
  scale_fill_gradientn(colors = c("#005f87", "#87afd7", "lightblue")
  ) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA)
  )
