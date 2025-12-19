from concurrent.futures import ProcessPoolExecutor

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import scipy
import scipy.optimize
from scipy.optimize import OptimizeResult


def compute_bellow_moment_force(p, R_outer, r_inner, n_bellow):
    """Approximate the total force and moment from the bellow structure. Approximate bellow
    structure area.

    Args:
        p (float): Pressure in Pa
        R_outer (float): Outer radius of the bellow in mm
        r_inner (float): Inner radius of the bellow in mm
        n_bellow (np.array): Number of bellows in the structure
    """
    # Reshape n_bellow to column vector
    n_bellow = n_bellow.reshape(-1, 1)
    # Effective area of a single bellow - hollow semicircle with radius R_outer and r_inner
    A_bellow = np.pi * (R_outer**2 - r_inner**2) / 2
    # Total force from bellow structure
    F_bellow = n_bellow * p * A_bellow
    # Total moment from bellow structure
    M_bellow = 2 / 3 * n_bellow * p * (R_outer**3 - r_inner**3)

    return A_bellow, F_bellow, M_bellow


def calculate_stiffness_hollow_semicircular_annulus_thick(L, E, R_out, r_in, t):
    ###### Fourth set of equations corresponds to Hollow semicircular annulus with
    # thickened rectangular element below

    # Area of the semicirular annulus
    A_semi = np.pi * (R_out**2 - r_in**2) / 2
    # Area of the rectangular base width 2R_out and height t
    A_rect = 2 * R_out * t

    # Area moment of inertia of the semicircular annulus
    I_semi = np.pi / 8 * (R_out**4 - r_in**4)
    # Area moment of inertia of the rectangular base
    I_rect_centroid = 1 / 12 * 2 * R_out * t**3
    # Parallel axis theorem to calculate I_rect about upper edge (base)
    I_rect = I_rect_centroid + A_rect * (t / 2) ** 2
    # Total area moment of inertia about upper edge (base)
    I_total = I_semi + I_rect

    # Centroid of the semicircular annulus from the base
    # y_semi = 4 / 3 / np.pi * (R_out**3 - r_in**3) / (R_out**2 - r_in**2)
    # Centroid of the composite shape from the base
    # y_total = (A_semi * y_semi + A_rect * (- t / 2)) / (A_semi + A_rect)
    # Total area moment of inertia about centroid (lower than about base)
    # I_total_centroid = I_total + (A_semi + A_rect) * y_total**2

    # Stifness - in L direction for elongation E * A / L
    Cz = E / L * (A_semi + A_rect)  # F / delta_x

    # Rotational stiffness
    # Ky = E / L * (np.pi / 8 * (R_out**4 - r_in**4) + 2 / 3 * R_out * t**3)  # M / delta_theta_y
    # Ky = E / L * I_total_centroid # M / delta_theta_y
    Ky = E / L * I_total  # M / delta_theta_y

    return A_semi, A_rect, I_semi, I_rect, I_total, Cz, Ky


def compute_prb_params_end_moment(
    E_I_div_L,
    M_acting,
    gamma_characteristic_radius_factor,
    L_initial_length,
    c_theta_parametric_angle_coefficient,
    quadratic_correction_term,
):
    # We can skip computing stifness of equivalent torsional spring, as the terms cancel out
    # for end-moment calculation
    # Beam end angular deflection
    # Add correction quadratic term to account for small nonlinearities
    theta_end_angular_deflection = (
        M_acting / E_I_div_L * (1 + quadratic_correction_term * M_acting / E_I_div_L)
    )
    # Coordinates of the end of deflected beam
    x_end = (
        (1 - gamma_characteristic_radius_factor) * L_initial_length
        + gamma_characteristic_radius_factor
        * L_initial_length
        * np.cos(theta_end_angular_deflection / c_theta_parametric_angle_coefficient)
    )
    y_end = (
        gamma_characteristic_radius_factor
        * L_initial_length
        * np.sin(theta_end_angular_deflection / c_theta_parametric_angle_coefficient)
    )
    return theta_end_angular_deflection, x_end, y_end


def compute_prb_joint_start_coord(
    x_prev_joint_end, y_prev_joint_end, inter_joint_dist, theta_prev_joint_cumulative
):
    x_joint_start = x_prev_joint_end + inter_joint_dist * np.cos(
        theta_prev_joint_cumulative
    )
    y_joint_start = y_prev_joint_end + inter_joint_dist * np.sin(
        theta_prev_joint_cumulative
    )
    return x_joint_start, y_joint_start


def compute_prb_joint_end_coord(
    x_joint_start,
    y_joint_start,
    joint_end_rel_x,
    joint_end_rel_y,
    theta_prev_joint_cumulative,
    delta_l_joint_elongation,
    theta_joint_cumulative,
):
    x_joint_end = (
        x_joint_start
        + joint_end_rel_x * np.cos(theta_prev_joint_cumulative)
        - joint_end_rel_y * np.sin(theta_prev_joint_cumulative)
        + delta_l_joint_elongation * np.cos(theta_joint_cumulative)
    )
    y_joint_end = (
        y_joint_start
        + joint_end_rel_y * np.cos(theta_prev_joint_cumulative)
        + joint_end_rel_x * np.sin(theta_prev_joint_cumulative)
        + delta_l_joint_elongation * np.sin(theta_joint_cumulative)
    )
    return x_joint_end, y_joint_end


def compute_joint_coordinates(
    lmbd,
    gamma,
    delta_l_theta_modifier,
    delta_l_n_bellow_modifier,
    c_theta,
    quadratic_correction_term,
    pressure,
    R_outer,
    r_inner,
    n_bellow,
    L_init,
    E,
    d_hole,
    D_bellow_out,
    d_bellow_in,
    t,
):
    # Compute single bellow area, force and moment
    A_bellow, F_bellow, M_bellow = compute_bellow_moment_force(
        p=pressure, R_outer=R_outer, r_inner=r_inner, n_bellow=n_bellow
    )
    # Calculate equivalent diameter
    D_bellow_out_equivalent = lmbd * D_bellow_out + (1 - lmbd) * d_bellow_in
    # Calculate stiffness
    _, _, _, _, _, Cz, Ky = calculate_stiffness_hollow_semicircular_annulus_thick(
        L_init, E, D_bellow_out_equivalent / 2, d_hole / 2, t
    )
    # PRB parameters
    theta_end, joint_end_rel_x, joint_end_rel_y = compute_prb_params_end_moment(
        E_I_div_L=Ky[:, np.newaxis],
        M_acting=M_bellow,
        gamma_characteristic_radius_factor=gamma[:, np.newaxis],
        L_initial_length=L_init[:, np.newaxis],
        c_theta_parametric_angle_coefficient=c_theta[:, np.newaxis],
        quadratic_correction_term=quadratic_correction_term[:, np.newaxis],
    )
    # Compute total elongation (axial + bending)
    delta_l = F_bellow / Cz[:, np.newaxis] + (
        theta_end**2 * delta_l_theta_modifier[:, np.newaxis]
        + n_bellow[:, np.newaxis] * delta_l_n_bellow_modifier[:, np.newaxis]
    )
    # Compute joint coordinates
    # Solve for MCP joint first
    mcp_joint_start_x, mcp_joint_start_y = (
        np.zeros_like(pressure),
        np.zeros_like(pressure),
    )
    mcp_joint_end_x, mcp_joint_end_y = compute_prb_joint_end_coord(
        x_joint_start=mcp_joint_start_x,
        y_joint_start=mcp_joint_start_y,
        joint_end_rel_x=joint_end_rel_x[0, :],
        joint_end_rel_y=joint_end_rel_y[0, :],
        theta_prev_joint_cumulative=0,
        delta_l_joint_elongation=delta_l[0, :],
        # delta_l_joint_elongation=0,
        theta_joint_cumulative=theta_end[0, :],
    )
    # Solve for PIP joint second
    pip_joint_start_x, pip_joint_start_y = compute_prb_joint_start_coord(
        x_prev_joint_end=mcp_joint_end_x,
        y_prev_joint_end=mcp_joint_end_y,
        inter_joint_dist=inter_joint_dist[0],
        theta_prev_joint_cumulative=theta_end[0, :],
    )
    pip_joint_end_x, pip_joint_end_y = compute_prb_joint_end_coord(
        x_joint_start=pip_joint_start_x,
        y_joint_start=pip_joint_start_y,
        joint_end_rel_x=joint_end_rel_x[1, :],
        joint_end_rel_y=joint_end_rel_y[1, :],
        theta_prev_joint_cumulative=theta_end[0, :],
        delta_l_joint_elongation=delta_l[1, :],
        # delta_l_joint_elongation=0,
        theta_joint_cumulative=theta_end[0:2, :].sum(axis=0),
    )
    # Solve for DIP joint last
    dip_joint_start_x, dip_joint_start_y = compute_prb_joint_start_coord(
        x_prev_joint_end=pip_joint_end_x,
        y_prev_joint_end=pip_joint_end_y,
        inter_joint_dist=inter_joint_dist[1],
        theta_prev_joint_cumulative=theta_end[0:2, :].sum(axis=0),
    )
    dip_joint_end_x, dip_joint_end_y = compute_prb_joint_end_coord(
        x_joint_start=dip_joint_start_x,
        y_joint_start=dip_joint_start_y,
        joint_end_rel_x=joint_end_rel_x[2, :],
        joint_end_rel_y=joint_end_rel_y[2, :],
        theta_prev_joint_cumulative=theta_end[0:2, :].sum(axis=0),
        delta_l_joint_elongation=delta_l[2, :],
        # delta_l_joint_elongation=0,
        theta_joint_cumulative=theta_end.sum(axis=0),
    )
    # Create 6 x 100 x 2 array of joint coordinates
    # Each row corresponds to one joint, each column to one pressure, each depth to x and y coordinate
    joint_coords = np.stack(
        (
            # X coordinates
            np.vstack(
                (
                    mcp_joint_start_x,
                    mcp_joint_end_x,
                    pip_joint_start_x,
                    pip_joint_end_x,
                    dip_joint_start_x,
                    dip_joint_end_x,
                )
            ),
            # Y coordinates - sign is inverted due to coordinate system
            -np.vstack(
                (
                    mcp_joint_start_y,
                    mcp_joint_end_y,
                    pip_joint_start_y,
                    pip_joint_end_y,
                    dip_joint_start_y,
                    dip_joint_end_y,
                )
            ),
        ),
        axis=2,
    )
    return joint_coords, theta_end


def obj_func_single_param(x):
    # print(x)
    lmbd = x[0:3]
    delta_l_modifier = x[3:6]
    delta_l_n_bellow_modifier = x[6:9]
    gamma = x[9:12]
    c_theta = x[12:15]
    quadratic_correction_term = x[15:18]
    joint_coords_prb, theta_end = compute_joint_coordinates(
        lmbd=lmbd,
        gamma=gamma,
        delta_l_theta_modifier=delta_l_modifier,
        delta_l_n_bellow_modifier=delta_l_n_bellow_modifier,
        c_theta=c_theta,
        quadratic_correction_term=quadratic_correction_term,
        pressure=pressure,
        R_outer=R_outer,
        r_inner=r_inner,
        n_bellow=n_bellow,
        L_init=L_init,
        E=E,
        d_hole=d_hole,
        D_bellow_out=D_bellow_out,
        d_bellow_in=d_bellow_in,
        t=t,
    )
    # Maximum or mean euclidean to optimize
    return np.mean(np.linalg.norm(joint_coords_fem - joint_coords_prb, axis=2))


#### Optimizazion of PRB model - END MOMENT LOADING
#### Hollow Semicircular Annulus and rectangular base

# Load data
# fem_data = pd.read_csv('kinematics/Hand9DOF/Index/data/FEM_position_data_arc_approx_joint_elongation_angles.csv')
fem_data = pd.read_csv(
    # "data/FEM_position_data_index_arc_approx_joint_elongation_angles_resample.csv"
    "data/FEM_position_data_index_arc_approx_joint_elongation_angles_resample_ogden.csv"
)
# fem_data = pd.read_csv('kinematics/Hand9DOF/Index/data/FEM_position_data_little_arc_approx_joint_elongation_angles_resample.csv')
inter_joint_dist_df = pd.read_csv("data/inter_joint_distance_index.csv")
# inter_joint_dist_df = pd.read_csv('kinematics/Hand9DOF/Index/data/inter_joint_distance_little.csv')
# Length of all three bellow segments in mm
L_init = fem_data["L_init"].unique()
pressure = fem_data["pressure"].unique()  # Internal pressure (MPa)
inter_joint_dist = inter_joint_dist_df["max"].to_numpy()  # Maximum inter-joint distance

joint_coords_fem = np.stack(
    (
        np.vstack(
            (
                fem_data[fem_data["joint"] == "MCP"][
                    ["resample_u1_start", "resample_u1_end"]
                ]
                .to_numpy()
                .T,
                fem_data[fem_data["joint"] == "PIP"][
                    ["resample_u1_start", "resample_u1_end"]
                ]
                .to_numpy()
                .T,
                fem_data[fem_data["joint"] == "DIP"][
                    ["resample_u1_start", "resample_u1_end"]
                ]
                .to_numpy()
                .T,
            )
        ),
        np.vstack(
            (
                fem_data[fem_data["joint"] == "MCP"][
                    ["resample_u2_start", "resample_u2_end"]
                ]
                .to_numpy()
                .T,
                fem_data[fem_data["joint"] == "PIP"][
                    ["resample_u2_start", "resample_u2_end"]
                ]
                .to_numpy()
                .T,
                fem_data[fem_data["joint"] == "DIP"][
                    ["resample_u2_start", "resample_u2_end"]
                ]
                .to_numpy()
                .T,
            )
        ),
    ),
    axis=2,
)

# Equvalent Young's modulus for TPU 85A in Mpa, fitted to strain 0 - 20% usin OLS regression
# Start from 0
E = 31.23
# Just slope
# E = 21.96
# E = 27
# Error less than 0.5% in tip deflection up to angular deflection of theta_0max = 124.4 deg
# Parametric angle coefficient - part of the optimization
# c_theta = 1.5164
# Diameters in mm - reinforced and unreinforced
d_hole = 6
D_bellow_out = 22
d_bellow_in = 7.85
# Thickness of bottom layer in mm
# t = (2.5 + 4.7) / 2
# t = 2.5
t = np.array([2.5, 2.5, 2.5])

# Remove thickness of the bellow from R_outer, and add it to r_inner (+0.9 due to geometry)
R_outer = (
    15.5 - 0.8
)  # Outer diameter of bellow (mm) - 15.5 is from thick element to top
r_inner = d_bellow_in + 0.575  # Inner diameter of bellow (mm)
n_bellow = np.array([7, 5, 5])  # Number of bellows


# Basin hopping search near optimum point, bounded by max_bin_count +- (approx_max_loc * max_num_bins)
# No local minimization
class BoundsNoLocalMinimization:
    def __init__(self, lower_bounds, upper_bounds):
        self.lower_bounds = lower_bounds
        self.upper_bounds = upper_bounds
        # Storing x and f values for each iteration
        self.x = []
        self.f = []
        self.accepted = []

    def no_minimization(self, fun, x0, args, **options):
        # Evaluate function at x0
        fun_res = fun(x0).ravel()
        return OptimizeResult(x=x0, fun=fun_res, success=True, nfev=1)

    def impose_bounds(self, **kwargs):
        # Limit search space
        x = kwargs["x_new"]
        return np.all((self.lower_bounds <= x) * (x <= self.upper_bounds))

    def store_jumps(self, x, f, accepted):
        # Can return True to terminate optimization early
        self.x.append(x)
        self.f.append(f.ravel())
        self.accepted.append(accepted)


class TakeStepRoutine:
    def __init__(self, stepsize):
        self.stepsize = stepsize
        self.rng = np.random.default_rng()
        self.stepsize_hist = []

    def __call__(self, x):
        self.stepsize_hist.append(self.stepsize)
        # Step for lambda
        x[0:3] += self.rng.uniform(-self.stepsize * 2, self.stepsize * 2, size=3)
        # Step for delta_l_theta_modifier
        x[3:6] += self.rng.uniform(-self.stepsize * 10, self.stepsize * 10, size=3)
        # Step for delta_l_n_bellow_modifier
        x[6:9] += self.rng.uniform(-self.stepsize, self.stepsize, size=3)
        # Step for gamma
        x[9:12] += self.rng.uniform(-self.stepsize * 2, self.stepsize * 2, size=3)
        # Step for c_theta
        x[12:15] += self.rng.uniform(-self.stepsize * 6, self.stepsize * 6, size=3)
        # Step for quadratic_correction_term
        x[15:18] += self.rng.uniform(-self.stepsize * 10, self.stepsize * 10, size=3)
        return x


def run_basinhopping_optimization(*args, **kwargs):
    bounds_minimizer = BoundsNoLocalMinimization(
        lower_bounds=np.array(
            [
                0.40,
                0.40,
                0.40,
                0.00,
                0.00,
                0.00,
                0.00,
                0.00,
                0.00,
                0.0,
                0.0,
                0.0,
                1.0,
                1.0,
                1.0,
                -1.0,
                -1.0,
                -1.0,
            ]
        ),
        upper_bounds=np.array(
            [
                1.5,
                1.5,
                1.5,
                15.0,
                15.0,
                15.0,
                0.20,
                0.20,
                0.20,
                1.0,
                1.0,
                1.0,
                8.0,
                8.0,
                8.0,
                20.0,
                20.0,
                20.0,
            ]
        ),
    )
    take_step_routine = TakeStepRoutine(stepsize=0.01)
    opt_res_bh = scipy.optimize.basinhopping(
        func=obj_func_single_param,
        # Random initial guess between bounds
        x0=np.random.uniform(
            bounds_minimizer.lower_bounds, bounds_minimizer.upper_bounds
        ),
        niter=1_000_000,  # Number of iterations
        T=0.2,
        take_step=take_step_routine,
        interval=100,  # Update stepsize every 200 iterations
        stepwise_factor=0.9,  # 90% stepsize adjustment
        target_accept_rate=0.60,  # 60% acceptance rate on step taking
        callback=bounds_minimizer.store_jumps,
        accept_test=bounds_minimizer.impose_bounds,
        minimizer_kwargs={"method": bounds_minimizer.no_minimization},
    )
    return opt_res_bh, bounds_minimizer, take_step_routine


with ProcessPoolExecutor(max_workers=4) as executor:
    results_list = list(executor.map(run_basinhopping_optimization, range(4)))


best_results = min(results_list, key=lambda x: x[0].fun)
# Get fun and x for results in list
[(i[0].fun, i[0].x) for i in results_list]
# results_list
best_results[0]
# plt.plot(best_results[1].f)

# Plot results
joint_coords, theta_end = compute_joint_coordinates(
    lmbd=best_results[0].x[0:3],
    delta_l_theta_modifier=best_results[0].x[3:6],
    delta_l_n_bellow_modifier=best_results[0].x[6:9],
    gamma=best_results[0].x[9:12],
    c_theta=best_results[0].x[12:15],
    quadratic_correction_term=best_results[0].x[15:18],
    pressure=pressure,
    R_outer=R_outer,
    r_inner=r_inner,
    n_bellow=n_bellow,
    L_init=L_init,
    E=E,
    d_hole=d_hole,
    D_bellow_out=D_bellow_out,
    d_bellow_in=d_bellow_in,
    t=t,
)
# Plot all joint coords as scatterplot
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10, 10))
ax.scatter(joint_coords[:, :, 0], joint_coords[:, :, 1], marker=".")
ax.scatter(joint_coords_fem[:, :, 0], joint_coords_fem[:, :, 1], marker=".")
# Add gridlines at 10 mm intervals
ax.set_xticks(np.arange(0, 120, 10))
ax.set_yticks(np.arange(-100, 10, 10))
ax.grid()
ax.set_aspect("equal")
plt.tight_layout()
plt.show()
