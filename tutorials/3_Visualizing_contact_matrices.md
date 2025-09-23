# Visualizing contact matrices

In this tutorial, we will explore how to visualize contact matrices using contact patterns data.

## Requirements

This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming languages.

In the code examples, we directly use the following R packages: - [`dplyr`](https://cran.r-project.org/web/packages/dplyr/) for efficient operations with dataframes. - [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/) for visualizing results.

The example in this tutorial uses data from the previous tutorial. It is recommended to complete tutorial 2 on constructing contact matrices prior to starting this tutorial.

## Plot

There were two age-specific contact matrices calculated for individual age and by age groups.

The plot for individual ages can be created using the code below:

```         
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
```

![Age Plot 1](./figures/2_age_total.png)

The plot for age groups can be visualized using the code below:

```         
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
```

![Age Plot 2](./figures/2_by5_80-Unconditional-Total.png)

### Full code

You can check out the full code for this tutorial here: [visualizing_contact_matrices.R](./code/visualizing_contact_matrices.R)
