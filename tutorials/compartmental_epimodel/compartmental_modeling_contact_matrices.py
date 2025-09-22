import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# ============================
# Loading the necessary data
# ============================
# --- Population distribution by age
age_structure_fpath = "data/age_distributions/singleyr-age-dist.csv"
age_distribution_df = pd.read_csv(age_structure_fpath, index_col="age")

# --- Contact matrix in age groups
contact_matrix_fpath = "data/survey_epistorm/ageby5_80-matrix.csv"
contact_matrix_df = pd.read_csv(
        contact_matrix_fpath,
        header=0, index_col=0,
)

# # Uncomment this line if the matrix has respondent age as column index instead of row index.
# contact_matrix_df = contact_matrix_df.T

# --- Age groups defined as first age in each group. Must match the contact matrix.
age_groups = np.array([0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80])


# --- Aggregate the population distribution from individual ages into age groups
# Convert the age index into a column for binning
df = age_distribution_df.reset_index()

# Prepare the bins and labels
bins = list(age_groups) + [np.inf]  # include the final group as open-ended
labels = [f"{age_groups[i]}-{age_groups[i+1]-1}" for i in range(len(age_groups)-1)]
labels.append(f"{age_groups[-1]}+")  # last group is open-ended

# (1.) Assign group labels to each row
df["age_group_label"] = pd.cut(df["age"], bins=bins, right=False, labels=labels)

# Create a variable with the age group index for each age in the distribution
positions = np.searchsorted(age_groups, df["age"], side='right') - 1
positions = np.clip(positions, 0, len(age_groups) - 1)
df["age_group_index"] = positions

# (2.) Aggregate by age group
agg_df = df.groupby("age_group_label", observed=False).agg(
    **{"n_indiv": ("n_indiv", "sum")}
)

# Recompute the distribution as normalized counts
agg_df["distribution"] = agg_df["n_indiv"] / agg_df["n_indiv"].sum()
agg_df["age_min"] = age_groups  # Add the column with the lower bound of each age group

# Update the distribution df so it contains age group information
age_distribution_df = df.set_index("age")
age_groups_distribution_df = agg_df


# ====================
# Model Initialization
# ====================

# Parameters
dt = 1.0  # Length of the time step, in days
beta = 0.090  # Per-contact transmission risk
gamma = 0.333  # Rate of removal from the infectious compartment
n_steps = 800  # Number of steps of the model
initial_infec_fraction = 1.E-5  # Fraction to start as infectious
initial_infec_age = 45  # Age of initial infections
total_pop = age_distribution_df["n_indiv"].sum()  # Total number of individuals
num_ages = age_distribution_df.shape[0]  # Number of ages accounted in the distribution

# Etc
cmat = contact_matrix_df.to_numpy()  # For use with other numpy arrays
age_to_group = age_distribution_df["age_group_index"].to_numpy()  # Array to convert from age to age group index
n_indiv_array = age_distribution_df["n_indiv"].to_numpy()  # Population distribution as a numpy arrray
n_indiv_grp_array = age_groups_distribution_df["n_indiv"].to_numpy()  # Population distribution as a numpy arrray

# --- Define status structures
S = np.zeros((n_steps + 1, num_ages), dtype=int)
I = np.zeros((n_steps + 1, num_ages), dtype=int)
R = np.zeros((n_steps + 1, num_ages), dtype=int)
incidence = np.zeros((n_steps + 1, num_ages), dtype=int)

# --- Setup a random number generator
seed = 42
#seed = seed + i_sim  # If running a loop of simulations, uncomment this to change the seed every iteration
rng = np.random.default_rng(seed)

# --- Initialize the model statuses
S[0] = n_indiv_array
initial_infec_num = int(round(total_pop * initial_infec_fraction))
I[0, initial_infec_age] = initial_infec_num
S[0, initial_infec_age] -= initial_infec_num


# ================
# Model simulation
# ================

for i_step in range(n_steps):
    # Force of infection (p_inf) for each age group
    prevalence = np.bincount(age_to_group, weights=I[i_step]) / n_indiv_grp_array  # Fraction of infectious by age group
    lambda_t = beta * (cmat @ prevalence)  # By age group. Implements the formula above.
    p_inf_grp = np.clip(lambda_t * dt, 0, 1)
    p_inf = p_inf_grp[age_to_group]  # Broadcast back to ages

    # New infections: Binomial(S, p_inf Δt)
    new_infections = rng.binomial(S[i_step], p_inf)
    incidence[i_step] = new_infections

    # New removals: Binomial(I, γ Δt)
    p_rem = np.clip(gamma * dt, 0, 1)
    new_removals = rng.binomial(I[i_step], p_rem)

    # Update compartments for the next time step
    S[i_step + 1] = S[i_step] - new_infections
    I[i_step + 1] = I[i_step] + new_infections - new_removals
    R[i_step + 1] = R[i_step] + new_removals

    # Apply stop condition
    if I[i_step + 1].sum() == 0:
        # Fill remaining
        S[i_step + 2:] = S[i_step + 1]
        I[i_step + 2:] = I[i_step + 1]
        R[i_step + 2:] = R[i_step + 1]
        break


# =================================
# Post-processing and visualization
# =================================

# --- Plot incidence over time
overall_incidence = incidence.sum(axis=1)  # Infection incidence over time for all ages
attack_rate_array = R[-1] / n_indiv_array  # Infected fraction by age. We can use R here because immunity does not wane.
avg_attack_rate = R[-1].sum() / total_pop  # Fraction of the entire population that was infected at some point.

fig, ax = plt.subplots()

t_array = dt * np.arange(overall_incidence.shape[0])  # Create array of time
ax.plot(t_array, overall_incidence / total_pop)
ax.set_xlabel("Time (days)")
ax.set_ylabel("Normalized Infection Incidence")

fig.show()

# --- Plot attack rate by age
fig, ax = plt.subplots()

ax.plot(age_distribution_df.index, attack_rate_array)
ax.set_xlabel("Age")
ax.set_ylabel("Attack rate")

fig.show()
