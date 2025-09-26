"""
Utility functions and classes for the `compartmental_epimodel` of the
`Contact survey epistorm` project.
"""

from collections import OrderedDict, defaultdict
from pathlib import Path

from cycler import cycler
from matplotlib import pyplot as plt
from matplotlib.colors import to_rgb, ListedColormap, LinearSegmentedColormap
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import yaml


def prepare_dict_for_yaml_export(d: dict):
    """Converts some data types within a dictionary into other objects
    that can be read in a file (e.g. strings).
    Operates recursively through contained dictionaries.
    Changes are made inplace for all dictionaries.
    """
    for key, val in d.items():

        # Ordered and default dict
        if isinstance(val, (OrderedDict, defaultdict)):
            d[key] = dict(val)

        # pathlib.Path into its string
        if isinstance(val, Path):
            d[key] = str(val.expanduser())

        # Timestamps into string repr.
        if isinstance(val, pd.Timestamp):
            # d[key] = str(val)
            d[key] = val.isoformat()

        # Specified iterables
        if isinstance(
                val, (tuple, np.ndarray)
        ):
            d[key] = list(val)

        # Recurse through inner dictionary
        if isinstance(val, dict):
            prepare_dict_for_yaml_export(val)


def aggregate_age_distribution(
        age_groups_array: np.ndarray,
        age_distribution_df: pd.DataFrame,
        colname_age="age",
        colname_n_indiv="n_indiv",
        colname_distribution="distribution",
):
    """Aggregates the age distribution data into the age groups specified.

    Parameters
    ----------
    age_groups_array : np.ndarray
        The array of age groups to aggregate into.
    age_distribution_df : pd.DataFrame
        The DataFrame with the age distribution data.
    colname_age : str
        The name of the column containing the age.
    colname_n_indiv : str
        The name of the column containing the number of individuals.
    colname_distribution : str
        The name of the column containing the age distribution value.

    Returns
    -------
    agg_df : pd.DataFrame
        The DataFrame with the age distribution aggregated into the given age groups.
    age_distribution_df : pd.DataFrame
        The original DataFrame with the age distribution data.
    """

    # Aux variables
    _age = colname_age
    _n_indiv = colname_n_indiv
    _distribution = colname_distribution

    # Prepare the bins and labels
    age_groups = age_groups_array
    bins = list(age_groups) + [np.inf]  # include the final group as open-ended
    labels = [f"{age_groups[i]}-{age_groups[i+1]-1}" for i in range(len(age_groups)-1)]
    labels.append(f"{age_groups[-1]}+")  # last group is open-ended

    # Convert index to column for binning
    df = age_distribution_df.reset_index()

    # Assign group labels to each row
    df["age_group_label"] = pd.cut(df[_age], bins=bins, right=False, labels=labels)

    # Create a variable with the age group index for each age in the distribution
    positions = np.searchsorted(age_groups, df[_age], side='right') - 1
    positions = np.clip(positions, 0, len(age_groups) - 1)
    df["age_group_index"] = positions

    # Aggregate by age group
    agg_df = df.groupby("age_group_label", observed=False).agg(
        **{_n_indiv: (_n_indiv, "sum")}
    )

    # Recompute the distribution as normalized counts
    agg_df[_distribution] = agg_df[_n_indiv] / agg_df[_n_indiv].sum()
    agg_df["age_min"] = age_groups_array  # Add the column with the lower bound of each age group

    # Update the distribution df to have age group information
    age_distribution_df = df.set_index(_age)

    return agg_df, age_distribution_df


def apply_opacity(color, opacity):

    """
    Adds opacity (alpha channel) to a color by appending the opacity to
    an rgba tuple.

    Parameters
    ----------
    color : str
        The color to modify. Accepted formats include hex, rgb, rgba,
        and any named color recognized by matplotlib.to_rgb.
    opacity : float
        The opacity (between 0 and 1) to apply to the color.

    Returns
    -------
    rgba_color : str
        The color with the opacity applied, in rgba format.
    """
    rgb_tuple = to_rgb(color)
    rgba_tuple = (*rgb_tuple, opacity)
    return f"rgba{rgba_tuple}"


def plotly_fill_between(
        fig, x, y1, y2, fillcolor=None, opacity=0.5, name=None,
        row=1, col=1,  # Arguments to `add_trace()`
        **kwargs
):
    """
    Fills an area between two lines in a plotly figure.

    Parameters
    ----------
    fig : plotly.graph_objects.Figure
        The figure to plot on.
    x : array-like
        The x coordinates of the lines.
    y1 : array-like
        The y coordinates of the first line.
    y2 : array-like
        The y coordinates of the second line.
    fillcolor : str or None, optional
        The color to use for the fill. If None, defaults to the color cycle.
    opacity : float, optional
        The opacity to use for the fill. Must be between 0 and 1.
    name : str or tuple, optional
        The name of the trace. Used for legend labels.
        If a tuple is informed, then its first element is used for y1 and
        its second element is used for y2.

    Remaining keyword arguments are forwarded to ecah call to `go.Scatter().`

    Returns
    -------
    traces : list
        A list of the two traces that were added to the figure.
    """
    """"""

    # --- Manipulate color and opacity
    # OBS: Due to limitations in how plotly works, we cannot set the
    #  color opacity without explicitly specifying the color.
    if opacity is not None and fillcolor is not None:
        fillcolor = apply_opacity(fillcolor, opacity)
        # rgb_tuple = to_rgb(fillcolor)
        # rgba_tuple = (*rgb_tuple, opacity)  # Appends opacity
        # fillcolor = f"rgba{rgba_tuple}"

    # --- Manipulate trace names
    if isinstance(name, tuple) and len(name) >= 2:
        names = name
    else:
        names = (name, name)

    # Plot
    # ================
    fig.add_trace(go.Scatter(
        x=x, y=y1,
        mode="lines",
        fillcolor=fillcolor,
        name=names[0],
        # legendgroup=name,
        **kwargs
    ),
        row=row, col=col
    )

    fig.add_trace(go.Scatter(
        x=x, y=y2,
        mode="lines",
        fill='tonexty',  # fill area between this and the last trace
        fillcolor=fillcolor,
        name=names[1],
        # legendgroup=name,
        **kwargs
    ),
        row=row, col=col
    )

    return fig.data[-2:]


# ============ Matplotlib

def get_color_cycle_list(rc=None):
    """
    Returns the current color cycle list.

    Parameters
    ----------
    rc : dict or None
        The rcParams dictionary. If None, it uses the current rcParams.

    Returns
    -------
    color_cycle_list : list
        The current color cycle list.
    """
    rc = plt.rcParams if rc is None else rc
    return rc['axes.prop_cycle'].by_key()['color']


supmat_color_cycle_01 = [
    '#A978CC', '#63CC78', '#5AADCC', '#CC8550', '#A8CC5B',
    "red", "green", "blue"
]

supmat_color_cycle_02 = [
    "#3cafa3",  # Medium pool-green
    "#f6d55a",  # CAASO yellow
    "#1f616f",  # Dark green
    "#ed563b",  # Red-orange-ish
    "#593B85",  # Dark violet
]

# Style dictionary (rcParams) for supplemental material plots done with matplotlib.
supmat_rc_params = {
    # --- Fonts
    #     "font.family": "DejaVu Sans",
    "font.family": "Open Sans",
    # font.style:   "normal",
    # font.variant: "normal",
    "font.weight": "light",
    "font.stretch": "condensed",
    "font.size": 12.0,

    # --- Figure Props
    "figure.figsize": (4., 4.),

    # --- Colors
    # "axes.prop_cycle": cycler(color=supmat_color_cycle_01),
    "axes.prop_cycle": cycler(color=supmat_color_cycle_02),

    # --- Unwanted linewidths
    "patch.linewidth": 0,
    "lines.markeredgewidth": 0,
    "lines.markeredgecolor": "none",

    # Misc
    "legend.fontsize": "small",
}


def make_axes_seq(num_axes, max_cols=3, total_width=9., ax_height=3.):
    """Creates a sequence of num_axes axes in a Matplotlib figure.
    The axes are periodically displayed into rows of at most max_cols elements.
    Remaining axes in the last row are removed

    Parameters
    ----------
    num_axes : int
        The total number of axes to create.
    max_cols : int, optional
        The maximum number of columns in the figure layout. Default is 3.
    total_width : float, optional
        The total width of the figure in inches. Default is 9.
    ax_height : float, optional
        The height of each axis in inches. Default is 3.

    Returns
    -------
    fig : matplotlib.figure.Figure
        The figure object that contains the axes.
    axes : list of matplotlib.axes.Axes
        A list of axes objects created in the figure.
    """
    # Basic dependent numbers
    num_rows = (num_axes - 1) // max_cols + 1

    # Empty figure initialization and gridspec object (divides space into grids).
    fig = plt.figure(figsize=(total_width, num_rows * ax_height))
    gridspecs = fig.add_gridspec(num_rows, max_cols)

    # Creates the list of axes with the required number of axes
    axes = [fig.add_subplot(gridspecs[i]) for i in range(num_axes)]

    return fig, axes


def rotate_ax_labels(ax, angle=60, xy="x", which="major"):
    """
    Rotate the axis tick labels of the given axis object.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The axis object with the labels to be rotated.
    angle : float, optional
        The angle of rotation in degrees. Default is 60.
    xy : str, optional
        The axis with the labels to rotate. Accepted values are "x" or "y".
        Default is "x".
    which : str, optional
        The type of tick labels to rotate. Accepted values are "major" or "minor".
        Default is "major".
    """
    labels = (
        ax.get_xticklabels(which=which)
        if xy == "x" else ax.get_yticklabels(which=which))

    for label in labels:
        label.set(rotation=angle, horizontalalignment='right')


def make_discrete_colormap(n_bins=5, cmap_name="green_yellow_red", colors=None):
    """
    Create a discrete colormap from green (low) → yellow (mid) → red (high).

    Parameters
    ----------
    n_bins : int
        Number of discrete color levels.
    cmap_name : str
        Name of the custom colormap.

    Returns
    -------
    cmap : ListedColormap
        Discrete colormap for use in matplotlib.
    """
    # colors = colors or ["green", "yellow", "red"]
    # colors = colors or ["#B0E6A5", "#EBD434", "#E0401A"]  # Milder green-yellow-red
    # colors = colors or ["#98cce1", "#EBD434", "#E0401A"]
    colors = colors or ["#98cce1", "#E0401A"]
    # Define continuous base colormap (green → yellow → red)
    base_cmap = LinearSegmentedColormap.from_list(
        cmap_name, colors
    )

    # Sample it into n_bins discrete steps
    colors = base_cmap(np.linspace(0, 1, n_bins))
    cmap = ListedColormap(colors, name=cmap_name)

    return cmap

