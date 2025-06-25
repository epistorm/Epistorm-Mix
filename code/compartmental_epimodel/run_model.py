"""
run_model.py

Run the compartmental_epimodel for a given input file.

The input file must be specified as a YAML file, please check [inputs][./inputs] directory
for examples. Besides model and program parameters, the input YAML file contains paths 
to other data files needed for the program to run (e.g. age structure, contact matrix).

Usage
-----
For a complete list of command line arguments, run:
  >  python run_model.py --help

Regular usage follows:
  >  python run_model.py input_file.yaml output_dir


"""
import argparse
import shutil
import time
from copy import deepcopy
from pathlib import Path

import numpy as np
import pandas as pd
import plotly.express as px
import scipy
import yaml  # Requires: PyYaml

from project_utils import prepare_dict_for_yaml_export


# -------------------------------------------------------------------
# PROGRAM STRUCTURES
# -------------------------------------------------------------------

class Params:
    """All parameters. Include hardcoded script params, those read
    from inputs, and overriden by command line flags.

    Parameters will have type hints in PyCharm, but raise
    AttributeError if referenced before assignment.

    This class works more as an autocomplete guide. It will receive
    any parameter that is either defined here, at the input
    file or command line arguments.
    """
    # Parameters with no default values (required from the input file)
    # =========================
    input_file: Path  # Path to the input file with model parameters 
    output_dir: Path  # Path to the output directory

    age_structure_fpath: Path  # Path to the population age distribution
    contact_matrix_fpath: Path  # Path to the contact matrix
    contact_matrix_age_groups: list  # Pre-informed age groups in which the contact matrix is constructed

    nsim: int  # Number of independent runs
    seed: int  # Seed for the random number generator

    model_nsteps_max: int  # Maximum number of time steps to run
    model_dt: float     # Model time step length
    model_beta: float   # Per-contact transmission rate
    model_gamma: float  # Recovery/removal rate

    def __init__(self):
        # Parameters with default values (optional from the input file)
        # ===========================
        # --- Column names expected in the age structure file
        self.colname_age: str = "age"
        self.colname_next_age: str = "next_age"
        self.colname_n_indiv: str = "n_indiv"
        self.colname_distribution: str = "distribution"

        self.seed_leap = 1  # For each new simulation, re-seed the RNG with `seed + i * seed_leap`

        self.model_use_r0 = False  # Basic reproduction number. If set, overrides model_beta to match R0.
        self.model_initial_mode: str = "number"  # Mode of model initialization: "fraction" or "number"
        self.model_initial_infec: int = 1  # Number of initial infections
        self.model_initial_infec_fraction: float = 1E-5  # Fraction of the total population
        self.model_initial_infec_age: int = 0  # Age of initial infections

        # Details of the contact matrix file
        self.cm_has_header = False  # Contact matrix file has a header
        self.cm_has_index = False   # Contact matrix file has index column
        self.cm_transpose = True    # TRUE: i = contact, j = respondent.

        self.do_export: bool = True  # Whether to export outputs to files
        self.do_export_individual_sims = False  # Export the results of each individual simulation
        self.export_incidence: bool = True  # Whether to export the incidence time series
        self.do_copy_input_file = True  # Whether to copy the input file into the output dir
        self.do_copy_age_structure_file = True  # Whether to copy the age structure file to the output dir
        self.do_copy_contact_matrix_file = True  # Whether to copy the contact matrix file to the output dir

        self.sim_name_fmt: str = "sim_{:06d}"  # A format string to identify files from a given simulation
        self.parameters_file_basename: str = "parameters.yaml"
        self.summary_file_basename: str = "summary.yaml"
        self.simulations_summary_file_basename: str = "simulations_summary.csv"

    def preprocess(self, args):
        """This function is called once after the parameters were read
        from the input file.
        """
        # Converts path strings to pathlib.Path objects
        for param in ["input_file", "output_dir"]:
            setattr(self, param, Path(getattr(self, param)))

        # Converts parameters ending with "_fpath" into pathlib.Path objects
        for key in self.__dict__.keys():
            if key.endswith("_fpath"):
                setattr(self, key, Path(getattr(self, key)))

        # --- Override some parameters with arguments
        if args.export is not None:
            self.do_export = args.export


class Data:
    """This class holds data that is created during the execution
    of the program, but not directly informed in the main input file.
    """
    # Data structures without previous initialization
    # ===============================================
    num_ages: int  # Number of ages or age groups in the age distribution data
    num_age_groups: int
    age_groups_array: np.ndarray  # Age groups used for contacts
    age_distribution_df: pd.DataFrame  # Age distr. as read from the data file. Must have 1yr resolution.
    age_groups_distribution_df: pd.DataFrame  # Age distr. aggregated in age groups
    # age_to_group: None
    # group_to_age: None
    contact_matrix_df: pd.DataFrame  # Contact matrix in age groups

    cm_spectral_radius: float

    simulations_summary_df: pd.DataFrame
    summary_yaml_str: str

    def __init__(self):
        # Data structures with an initialized value
        # =================
        self.res_avg_attack_rate_list = list()
        self.res_attack_rate_list = list()
        self.res_num_steps_list = list()
        self.incidence_list = list()
        self.simulation_time_fname_list = list()
        self.total_pop_list = list()

        self.program_call_tstamp: pd.Timestamp = pd.Timestamp.now()


# -------------------------------------------------------------------
# MAIN FUNCTION
# -------------------------------------------------------------------

def main(params: Params, data: Data, argv: list[str] = None):
    # Parse command line arguments
    args = get_arg_parser().parse_args(argv)
    # args: CLArgs = get_arg_parser().parse_known_args(argv)
    #               ^ ^ Use this to accept extra unknown arguments.

    # Import from input file
    import_and_preprocess_params(args, params, data)

    # ------
    import_and_preprocess_program_data(params, data)
    run_simulations(params, data)
    postprocess_all_simulations(params, data)
    export_program_results(params, data)


# -------------------------------------------------------------------
# PROGRAM PROCEDURES
# -------------------------------------------------------------------

def get_arg_parser() -> argparse.ArgumentParser:
    """Interprets and stores the command line arguments."""

    parser = argparse.ArgumentParser(
        # usage="[[COMMAND LINE SIGNATURE (autogenerated)[]",
        description="Add here a description to show on --help calls.",
        # epilog="[[TEXT DISPLAYED AFTER ARGUMENTS DESCRIPTION]]",
    )

    # --- Positional paths
    parser.add_argument(
        "input_file", type=Path,
        help="Path to the file with input parameters."
    )
    parser.add_argument(
        "output_dir", type=Path,
        help="Path to the output directory, where all "
             "output files are stored."
    )

    # --- Optional flags
    parser.add_argument(  # Example. Retrieve as `optional_arg`
        "--export", type=bool, action=argparse.BooleanOptionalAction,
        help="Whether to produce any output files. Use `--no-export` to "
             "suppress outputs. By default, outputs are exported.",
    )

    return parser


# ===========================

def aux_aggregate_age_distribution(params: Params, data: Data):
    # Aux variables
    """
    Aggregate the age distribution data into the age groups specified.
    This function has an updated version for usage outside `run_model.py`. You can find 
    it in `project_utils.py`

    Parameters
    ----------
    params : Params
        The parameters of the model.
    data : Data
        The data of the model.

    Returns
    -------
    agg_df : pd.DataFrame
        The DataFrame with the age distribution aggregated into the given age groups.
    """
    _age = params.colname_age
    _n_indiv = params.colname_n_indiv
    _distribution = params.colname_distribution

    # Prepare the bins and labels
    age_groups = data.age_groups_array
    bins = list(age_groups) + [np.inf]  # include the final group as open-ended
    labels = [f"{age_groups[i]}-{age_groups[i+1]-1}" for i in range(len(age_groups)-1)]
    labels.append(f"{age_groups[-1]}+")  # last group is open-ended

    # Convert index to column for binning
    df = data.age_distribution_df.reset_index()

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

    # Update the distribution df to have age group information
    data.age_distribution_df = df.set_index(_age)

    return agg_df


# ===========================

def import_and_preprocess_params(args, params: Params, data: Data):
    """Imports the main YAML parameter file."""
    # --- Read file
    with open(args.input_file, "r") as fp:
        input_dict = yaml.load(fp, yaml.Loader)

    # --- Select command line arguments that are not `None`
    valid_args_dict = {
        key: val for key, val in args.__dict__.items() if val is not None
    }

    # --- Populate the Params in priority order
    # Script Default < Input File < Command Line Arguments
    params.__dict__.update(input_dict)
    params.__dict__.update(valid_args_dict)

    # data.input_dict = input_dict  # Use this to keep the input_dict

    # --- Preprocessing
    params.preprocess(args)


def import_and_preprocess_program_data(params: Params, data: Data):
    """Import data used throughout the entire program execution"""

    # Age distribution file
    # ===========
    data.age_distribution_df = pd.read_csv(
        params.age_structure_fpath, index_col=params.colname_age
    )

    # Contact matrix file
    # ===========
    data.contact_matrix_df = pd.read_csv(
        params.contact_matrix_fpath,
        header=0 if params.cm_has_header else None,
        index_col=0 if params.cm_has_index else None,
    )

    if params.cm_transpose:
        data.contact_matrix_df = data.contact_matrix_df.T

    if data.contact_matrix_df.isna().any().any():
        print("WARNING: Contact matrix has NA values, which will be replaced by 0.")
        data.contact_matrix_df.fillna(0, inplace=True)

    # Preprocess age groups info
    # ============
    data.num_ages = data.age_distribution_df.shape[0]

    # --- Set default contact matrix age groups
    if hasattr(params, "contact_matrix_age_groups"):
        data.age_groups_array = np.array(params.contact_matrix_age_groups)
    else:
        data.age_groups_array = data.age_distribution_df.index.to_numpy()
    data.age_groups_array = np.sort(data.age_groups_array)
    data.num_age_groups = data.age_groups_array.shape[0]

    # --- Check shape equality and consistency
    cm_shape = data.contact_matrix_df.shape

    # Square shape
    if len(cm_shape) < 2 or cm_shape[0] != cm_shape[1]:
        raise ValueError(
            f"Contact matrix must be a square matrix. Got {cm_shape=}"
        )

    # Match between the contact matrix and the age groups array
    if data.num_age_groups != cm_shape[0]:
        raise ValueError(
            f"Shape mismatch between the contact matrix and the informed"
            f"age groups.\n"
            f"Contact matrix shape: {cm_shape=}\n"
            f"Age groups: {data.age_groups_array} (len = {data.num_age_groups})"
        )

    # --- Aggregate the age distribution
    data.age_groups_distribution_df = (
        aux_aggregate_age_distribution(params, data)
    )

    # Reproduction number, if requested
    # ======================
    # --- Calculate largest eigenvalue of the contact matrix
    eigs = scipy.linalg.eigvals(
        data.contact_matrix_df.to_numpy()
    )
    data.cm_spectral_radius = max(abs(eigs))

    if params.model_use_r0:
        params.model_beta = (
                params.model_use_r0 * params.model_gamma /
                data.cm_spectral_radius
        )
        print(f"NOTE: beta modified to {params.model_beta} to achieve R0 = {params.model_use_r0}")


class SimulationOutputs:
    """Structure with outputs from a single simulation"""
    age_counts_array: np.ndarray  # Population by age
    attack_rate_array: np.ndarray  # Attack rate by age
    avg_attack_rate: float
    overall_incidence: np.ndarray
    total_pop: int

    def __init__(
            self, s, i, r, incidence, num_steps,
            simulation_time_fname=None
    ):
        
        """
        Initialize SimulationOutputs object.

        Parameters
        ----------
        s, i, r: np.ndarray (2D)
            2D arrays with the simulation outputs, where the first index is the step
            and the second index is the age.
        incidence: np.ndarray (2D)
            2D array with the incidence in each age group.
        num_steps: int
            Number of steps in the simulation.
        simulation_time_fname: str
            File name with the time points of the simulation.
        """
        self.s: np.ndarray = s
        self.i: np.ndarray = i
        self.r: np.ndarray = r
        self.incidence: np.ndarray = incidence
        #  ^  ^ Array signature (all above): a[i_step, i_age]
        self.num_steps: int = num_steps
        self.simulation_time_fname: str = simulation_time_fname

    def postprocess_simu(self):
        """Post-process a simulation. This function computes the attack rate and the total 
        population from the simulation outputs.
        """
        self.age_counts_array = self.s[-1] + self.i[-1] + self.r[-1]
        self.overall_incidence = self.incidence.sum(axis=1)
        self.total_pop = self.age_counts_array.sum()
        self.attack_rate_array = self.r[-1] / self.age_counts_array
        self.avg_attack_rate = self.r[-1].sum() / self.total_pop


def run_single_simulation(i_sim: int, params: Params, data: Data):
    """
    Run a single simulation of the compartmental epimodel.
    This function implements the core dynamics of a compartmental SIR model with 
    age-dependent contact rates via contact matrices.

    Parameters
    ----------
    i_sim: int
        Simulation number, used to index the simulation and seed the random number generator.
    params: Params
        Parameters of the model.
    data: Data
        Data used in the simulation.

    Returns
    -------
    out: SimulationOutputs
        Object with the simulation outputs.
    """
    # SIMULATION PREAMBLE
    # ==============
    # Unpack some parameters
    num_ages = data.num_ages
    nsteps_max = params.model_nsteps_max
    age_counts_array = data.age_distribution_df[params.colname_n_indiv].to_numpy()
    age_group_counts_array = data.age_groups_distribution_df[params.colname_n_indiv].to_numpy()

    total_pop = age_counts_array.sum()
    dt = params.model_dt
    beta = params.model_beta
    gamma = params.model_gamma
    cmat = data.contact_matrix_df.values
    age_to_group = data.age_distribution_df["age_group_index"].to_numpy()

    # SIMULATION INITIALIZATION
    # ====================
    # Setup random number generator
    seed = params.seed + i_sim * params.seed_leap
    rng = np.random.default_rng(seed)

    # Define structures and initialize simulation
    S = np.zeros((nsteps_max + 1, num_ages), dtype=int)
    I = np.zeros((nsteps_max + 1, num_ages), dtype=int)
    R = np.zeros((nsteps_max + 1, num_ages), dtype=int)
    incidence = np.zeros((nsteps_max + 1, num_ages), dtype=int)
    #   ^ ^ signature: A[i_step, i_age] = number of individuals
    # First step is the initial condition. Last step is the result after the last iteration.

    # Initial condition
    if params.model_initial_mode == "number":
        initial_infec_num = params.model_initial_infec  # -()- Number
    elif params.model_initial_mode == "fraction":
        initial_infec_num = int(round(total_pop * params.model_initial_infec_fraction))  # -()- Fraction
    else:
        raise ValueError(f"Invalid value: {params.model_initial_mode}")

    S[0] = age_counts_array
    I[0, params.model_initial_infec_age] = initial_infec_num
    S[0, params.model_initial_infec_age] -= initial_infec_num

    # MAIN SIMULATION LOOP
    # =========================
    loop_xt0 = time.time()

    i_step = 0  # This ensures it is defined after the loop.
    for i_step in range(params.model_nsteps_max):
        t = i_step * dt

        # Force of infection (p_inf) for each age group
        prevalence = np.bincount(age_to_group, weights=I[i_step]) / age_group_counts_array
        lambda_t = beta * (cmat @ prevalence)  # By age group.
        p_inf_grp = np.clip(lambda_t * dt, 0, 1)
        p_inf = p_inf_grp[age_to_group]  # Broadcast back to ages

        # New infections: Binomial(S, λ Δt)
        new_infections = rng.binomial(S[i_step], p_inf)
        incidence[i_step] = new_infections

        # New removals: Binomial(I, γ Δt)
        p_rem = np.clip(gamma * dt, 0, 1)
        new_removals = rng.binomial(I[i_step], p_rem)

        # Update compartments for the next step
        S[i_step + 1] = S[i_step] - new_infections
        I[i_step + 1] = I[i_step] + new_infections - new_removals
        R[i_step + 1] = R[i_step] + new_removals

        # === Apply Stop condition Here
        if I[i_step + 1].sum() == 0:
            # Fill remaining
            S[i_step + 2:] = S[i_step + 1]
            I[i_step + 2:] = I[i_step + 1]
            R[i_step + 2:] = R[i_step + 1]
            break

    loop_xtf = time.time()
    # print(f"DEBUG loop time (simulation {i_sim}) = {loop_xtf - loop_xt0:e}s")

    # SIMULATION POSTPROCESSING
    # ====================
    out = SimulationOutputs(
        S, I, R, incidence, i_step
    )

    out.postprocess_simu()

    # SIMULATION DATA EXPORT
    # ====================
    # --- DATA FRAME indexed by simulation time
    out_dict = dict()
    if params.export_incidence:
        # Overall incidence
        out_dict["incidence"] = out.overall_incidence

        # Incidence by age
        out_dict.update(
            {f"incid_age_{i_age:03d}": array for i_age, array in enumerate(out.incidence.T)}
        )

    out_df = pd.DataFrame(out_dict)
    out_df.index.name = "i_step"

    # Export this data frame
    if params.do_export and params.do_export_individual_sims:
        fpath = params.output_dir / (params.sim_name_fmt.format(i_sim) + "_time.csv")
        out.simulation_time_fname = str(fpath.name)
        fpath.parent.mkdir(exist_ok=True, parents=True)
        out_df.to_csv(fpath)

        print(f"Exported sim data to {fpath}")

    # --- DATA FRAME indexed by age

    return out


def run_simulations(params: Params, data: Data):
    """
    Runs the specified number of simulations. For each simulation,
    it calls run_single_simulation() and stores the results in the data object.
    """

    # ----
    for i_sim in range(params.nsim):
        # Run the simulation
        # ==================
        print(f"Running simulation {i_sim}")
        sim_out: SimulationOutputs = run_single_simulation(i_sim, params, data)

        # Retrieve summary results from simulation into the program registry
        # =================
        data.res_avg_attack_rate_list.append(sim_out.avg_attack_rate)
        data.res_attack_rate_list.append(sim_out.attack_rate_array)
        data.res_num_steps_list.append(sim_out.num_steps)
        data.incidence_list.append(sim_out.incidence)
        data.simulation_time_fname_list.append(sim_out.simulation_time_fname)
        data.total_pop_list.append(sim_out.total_pop)


def postprocess_all_simulations(params: Params, data: Data):
    """Work on the results held after all simulations were run."""

    # Simulation scalar outputs
    # ==============
    # Prepare the dataframe
    out_df = pd.DataFrame(
        {
            "num_steps": data.res_num_steps_list,
            "total_pop": data.total_pop_list,
            "simulation_time_fname": data.simulation_time_fname_list,
            "avg_attack_rate": data.res_avg_attack_rate_list,
            **{
                f"attack_rate_{age:03d}": np.fromiter(
                    (data.res_attack_rate_list[i_sim][i_age] for i_sim in range(params.nsim)),
                    dtype=float
                )
                # for a in range(data.num_ages)
                for i_age, age in enumerate(data.age_distribution_df.index)
            }
        }
    )
    out_df.index.name = "i_sim"

    data.simulations_summary_df = out_df

    # Summary outputs (program scalar results)
    # ==============
    # Format a YAML string
    out_str = ""

    out_str += f"program_call_datetime: {data.program_call_tstamp.isoformat()}\n"
    out_str += f"input_file: {params.input_file}\n"
    out_str += f"output_dir: {params.output_dir}\n"
    out_str += f"total_pop: {max(data.total_pop_list)}\n"  # TOTAL POP: Assume the same for all simus.
    out_str += f"age_groups_array: {data.age_groups_array.tolist()}\n"
    out_str += f"cm_spectral_radius: {data.cm_spectral_radius}\n"

    data.summary_yaml_str = out_str


def export_program_results(params: Params, data: Data):
    """Export outputs at the end of the program execution"""
    if not params.do_export:
        print("SKIPPING EXPORTS – params.do_export is False")
        return

    # Processed parameters as a YAML file
    # ==================
    params_dict = deepcopy(params.__dict__)
    prepare_dict_for_yaml_export(params_dict)
    # -
    fpath = params.output_dir / params.parameters_file_basename
    fpath.parent.mkdir(parents=True, exist_ok=True)
    with open(fpath, "w") as fp:
        yaml.dump(params_dict, fp, yaml.Dumper)
        print(f"Exported parameters dictionary to {fpath}")

    # Copy data and input files
    # =============
    if params.do_copy_input_file:
        shutil.copy(params.input_file, params.output_dir / "input_file.yaml")
    if params.do_copy_age_structure_file:
        shutil.copy(params.age_structure_fpath, params.output_dir / "age_structure.csv")
    if params.do_copy_contact_matrix_file:
        shutil.copy(params.contact_matrix_fpath, params.output_dir / "contact_matrix.csv")

    # Simulation scalar outputs
    # =============
    fpath = params.output_dir / params.simulations_summary_file_basename
    fpath.parent.mkdir(parents=True, exist_ok=True)
    data.simulations_summary_df.to_csv(fpath)
    print(f"Exported simulation summary to {fpath}")

    # Program scalar outputs
    # ==============
    fpath = params.output_dir / params.summary_file_basename
    fpath.parent.mkdir(parents=True, exist_ok=True)
    with open(fpath, "w") as fp:
        fp.write(data.summary_yaml_str)
    print(f"Exported program summary to {fpath}")


# ======================================================================


if __name__ == "__main__":
    main_params = Params()
    main_data = Data()
    main(main_params, main_data)
