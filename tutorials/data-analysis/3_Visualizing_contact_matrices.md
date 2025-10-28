# Visualizing contact matrices

In this tutorial, we will explore how to visualize contact matrices using [R](https://www.r-project.org/).

## Requirements

This tutorial assumes some familiarity with programming languages and completion of the previous tutorials ([Tutorial 1](./1_Accessing_and_extracting_data.md) and [Tutorial 2](./2_Constructing_contact_matrices.md)).

In the code examples, we directly use the following R packages: [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/) for the visualization.

## Plot

There were two age-specific contact matrices calculated in the previous tutorial: `age_matrix_s' - the smoothed contact matrix by age for single-year age brackets; and `age_matrix' - the contact matrix by age for 5-year age brackets (non-smoothed). Both of the calculated matrices were saved in the form of a data frame (long format) and in the form of a matrix (wide format). In this visualization, we will use the data frames for direct use in ggplot. If you would like to visualize a different matrix that is provided in the wide format using the approach below, please transform it into a data frame. Otherwise, please use a different tool for heatmaps.

A simple tile or raster visualization can be created for the contact matrices to illustrate the 3-dimensional data. Both visualizations below show the average number of contacts between a respondent of age group i and a contact of age group j.

```         
ggplot(age_matrix_s) +
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
```

<img src = "./figures/2_age_total.png" width = 60%>

The plot for the contact matrix on age groups (by 5 up to 80 years old) can be visualized using the code below. This plot shows the average number of contacts of one age group per respondent age group. 

```         
ggplot(age_matrix) +
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
```

<img src = "./figures/2_by5_80-Unconditional-Total.png" width = 70%>

### Full code

You can check out the full code for this tutorial here: [visualizing_contact_matrices.R](./code/visualizing_contact_matrices.R)
