# Heterogeneous Firm Model with Intangible Capital

**Author:** Alejandro Vicente
**Institution:** University of Alicante
**Paper:** "Financial Frictions, Intangible Capital and Productivity: A Model of Skill-Biased Stagnation"

## Overview

This code implements a quantitative general equilibrium model of heterogeneous firms with:

- **Two types of capital**: Tangible (K) and Intangible (S)
- **Two types of labor**: Unskilled (L) and Skilled (H = HP + HR)
- **Nested CES production function** with complementarity between intangibles and skilled labor
- **Financial frictions**: Collateral constraint where tangible capital is more pledgeable than intangible capital (αK > αS)
- **Firm dynamics**: Entry, exit, and idiosyncratic productivity shocks
- **R&D technology**: Intangible capital produced by skilled labor in R&D

### Key Mechanism: Skill-Biased Stagnation

When skilled labor supply increases:
- **Unconstrained firms**: Exploit complementarity by investing in both intangibles (S) and skilled labor (HP)
- **Constrained firms**: Cannot finance R&D due to low pledgeability of intangibles, leading to underexploitation of skilled labor

Result: Productivity gains from higher human capital are muted by financial frictions causing misallocation.

## Directory Structure

```
model_fortran/
├── src/                        # Source code
│   ├── mod_parameters.f90      # Model parameters
│   ├── mod_globals.f90         # Global variables and grids
│   ├── mod_utility.f90         # Production and utility functions
│   ├── mod_interpolation.f90   # Interpolation routines
│   ├── mod_firm_problem.f90    # Value function iteration
│   ├── mod_distribution.f90    # Stationary distribution
│   ├── mod_equilibrium.f90     # Equilibrium solver
│   └── main.f90                # Main program
├── build/                      # Compiled objects (created automatically)
├── output/                     # Results (created automatically)
│   ├── aggregates.txt          # Aggregate statistics
│   └── distribution.txt        # Firm distribution
├── Makefile                    # Build system
└── README.md                   # This file
```

## Requirements

### Fortran Compiler

Choose one of:
- **GNU Fortran (gfortran)** - Free, open-source (recommended for most users)
  - Install on Linux: `sudo apt-get install gfortran`
  - Install on macOS: `brew install gcc`
  - Install on Windows: MinGW-w64 or WSL

- **Intel Fortran (ifort)** - Commercial, faster for some operations
  - Part of Intel oneAPI HPC Toolkit

### System Requirements

- **RAM**: ~4-8 GB recommended
- **CPU**: Multi-core processor (code uses OpenMP parallelization)
- **Disk**: ~100 MB for code and output

## Compilation

### Quick Start

```bash
# Navigate to model directory
cd model_fortran

# Compile (default: gfortran with optimization)
make

# Run the model
./model_vicente
```

### Makefile Targets

```bash
make          # Compile with optimization flags
make debug    # Compile with debug flags and runtime checks
make clean    # Remove compiled files
make run      # Compile and run
make time     # Compile and run with timing information
make help     # Show available targets
```

### Manual Compilation (if Makefile doesn't work)

```bash
gfortran -O3 -c src/mod_parameters.f90
gfortran -O3 -c src/mod_globals.f90
gfortran -O3 -c src/mod_utility.f90
gfortran -O3 -c src/mod_interpolation.f90
gfortran -O3 -c src/mod_firm_problem.f90
gfortran -O3 -c src/mod_distribution.f90
gfortran -O3 -c src/mod_equilibrium.f90
gfortran -O3 -c src/main.f90
gfortran -O3 -o model_vicente *.o
```

## Running the Model

### Basic Execution

```bash
./model_vicente
```

The program will:
1. Initialize grids and parameters
2. Solve firm value function (VFI)
3. Compute stationary distribution
4. Iterate on wages until equilibrium
5. Save results to `output/` directory

### Expected Runtime

- **Small grid** (nz=7, nK=20, nS=20, nD=15): ~5-15 minutes
- **Medium grid** (nz=11, nK=30, nS=30, nD=20): ~30-60 minutes
- **Large grid** (nz=15, nK=40, nS=40, nD=30): ~2-4 hours

Runtime scales roughly as O(nz × nK × nS × nD × nIK × nHR × nDprime).

## Model Parameters

### Key Parameters (in `src/mod_parameters.f90`)

#### Preferences and Technology
```fortran
beta = 0.96          ! Discount factor
alpha_prod = 0.33    ! Capital share
nu = 0.85            ! Returns to scale
rho_K = -0.50        ! K-Q substitution (σK = 2/3)
rho_Q = -1.50        ! S-HP substitution (σQ = 0.4)
```

#### Capital Accumulation
```fortran
delta_K = 0.10       ! Tangible depreciation
delta_S = 0.15       ! Intangible depreciation
Gamma_RD = 1.50      ! R&D productivity
xi = 0.90            ! R&D returns to scale
```

#### Financial Frictions
```fortran
alpha_K = 0.381      ! Tangible pledgeability
alpha_S = 0.134      ! Intangible pledgeability
```
Source: Holttinen et al. (2025) structural estimates for UK firms

#### Productivity Process
```fortran
rho_z = 0.95         ! AR(1) persistence
sigma_z = 0.15       ! Std dev of innovations
```

#### Entry/Exit
```fortran
zeta = 0.10          ! Exit probability
ce = 1.00            ! Entry cost
```

### Labor Supply (Counterfactual Experiments)

```fortran
Lbar = 0.60          ! Unskilled labor supply
Hbar = 0.40          ! Skilled labor supply
```

To simulate skill-biased changes, adjust `Hbar` while keeping `Lbar + Hbar = 1`.

## Output Files

### `output/aggregates.txt`

Contains equilibrium aggregate quantities:
- Output (Y), Consumption (C)
- Capital stocks (K, S)
- Labor demands (L, HP, HR)
- Wages (wL, wH)
- Financial statistics (leverage, fraction constrained)

Example:
```
Output_Y                 125.432156
Consumption_C            112.345678
Tangible_capital_K        85.234567
Intangible_capital_S      32.123456
...
```

### `output/distribution.txt`

Firm distribution over state space (sparse format):
```
iz iK iS iD z K S D mass
1  5  3  2  0.8234  5.234  2.145  1.234  0.002345
...
```

## Counterfactual Experiments

### 1. Skill Supply Shock

To study the effect of increased skilled labor:

```fortran
! In src/mod_parameters.f90, change:
Hbar = 0.45   ! Increase from 0.40
Lbar = 0.55   ! Decrease to maintain sum = 1
```

Recompile and run:
```bash
make clean
make run
```

Compare `output/aggregates.txt` before and after.

### 2. Financial Liberalization

To study the effect of improved intangible pledgeability:

```fortran
! In src/mod_parameters.f90, change:
alpha_S = 0.25   ! Increase from 0.134
```

This simulates financial innovation improving intangible collateral value.

### 3. Productivity Growth

To study the effect of higher TFP growth:

```fortran
! In src/mod_parameters.f90, change:
rho_z = 0.98     ! More persistent productivity
sigma_z = 0.20   ! Higher variance
```

### 4. No Financial Frictions (First Best)

To compute first-best allocation:

```fortran
! In src/mod_parameters.f90, set:
alpha_S = alpha_K = 1.00   ! Perfect pledgeability
```

Compare with baseline to measure efficiency losses from frictions.

## Code Architecture

### Module Hierarchy

```
mod_parameters
    └── mod_globals
            ├── mod_utility
            ├── mod_interpolation
            │       └── mod_firm_problem
            │       └── mod_distribution
            └── mod_equilibrium
                    └── main
```

### Solution Algorithm

1. **Grid Construction** (`mod_equilibrium::initialize_grids`)
   - Tauchen discretization for AR(1) productivity
   - Log-spaced capital grids
   - Linear debt grid

2. **Value Function Iteration** (`mod_firm_problem::solve_firm_problem`)
   - For each state (z,K,S,D₋₁):
     - Solve static labor (L, HP) via FOCs
     - Optimize over (I^K, H^R, D) via grid search
     - Compute continuation value with interpolation
   - Iterate until convergence (supnorm < 10⁻⁶)

3. **Distribution** (`mod_distribution::compute_stationary_distribution`)
   - Young iteration with trilinear interpolation
   - Account for entry (at K=S=D=0) and exit (probability ζ)
   - Iterate until convergence

4. **Equilibrium** (`mod_equilibrium::solve_equilibrium`)
   - Outer loop over wages (wL, wH)
   - Inner loop: solve firm problem → distribution → aggregates
   - Update wages based on labor market excess demand
   - Converge when |excess_L|, |excess_H| < 10⁻⁴

## Numerical Considerations

### Grid Size Trade-off

- **Smaller grids**: Faster but less accurate, may miss important heterogeneity
- **Larger grids**: More accurate but computationally expensive

Recommended starting point:
```fortran
nz = 11,  nK = 30,  nS = 30,  nD = 20
```

### Convergence Issues

If VFI or distribution doesn't converge:

1. **Reduce update weights**:
   ```fortran
   update_VFI = 0.3    ! Lower from 0.5
   update_dist = 0.05  ! Lower from 0.10
   ```

2. **Increase max iterations**:
   ```fortran
   maxiter_VFI = 3000
   maxiter_dist = 10000
   ```

3. **Check parameter bounds**: Ensure grids cover relevant state space

4. **Start with simplified case**: Set `alpha_S = 0` (fully non-pledgeable intangibles)

### Parallelization

The code uses OpenMP for parallel loops. Set thread count:
```bash
export OMP_NUM_THREADS=8  # Use 8 cores
./model_vicente
```

## Troubleshooting

### Compilation Errors

**Error**: `Undefined reference to 'erf'`
- **Solution**: Add `-lm` flag for math library:
  ```bash
  gfortran ... -lm
  ```

**Error**: Module not found
- **Solution**: Check compilation order in Makefile (dependencies)

### Runtime Errors

**Segmentation fault**
- **Cause**: Stack overflow from large arrays
- **Solution**: Increase stack size:
  ```bash
  ulimit -s unlimited
  ```
  Or compile with heap allocation

**NaN or Inf values**
- **Cause**: Division by zero or negative values in power functions
- **Solution**: Check epsilon guards in `mod_utility.f90`

### Unexpected Results

**All firms constrained**
- **Cause**: Grids too coarse or wages too high
- **Solution**: Expand K, S grids or adjust initial wage guess

**No convergence**
- **Cause**: Update weights too aggressive
- **Solution**: Reduce dampening parameters

## Extensions

The modular structure facilitates extensions:

### 1. Richer Labor Market
- Add search frictions (`mod_utility.f90`)
- Endogenous wage determination (`mod_equilibrium.f90`)

### 2. Capital-Skill Complementarity at Firm Level
- Modify production function in `mod_utility.f90::production_Y`

### 3. Stochastic Exit
- Make ζ depend on (z, K, S) in `mod_distribution.f90`

### 4. Policy Experiments
- R&D subsidies: Reduce effective cost of HR in `mod_firm_problem.f90`
- Credit subsidies: Relax collateral constraint for small firms

### 5. Aggregate Shocks
- Add time-varying TFP or R in `mod_parameters.f90`
- Solve transition dynamics

## Citation

If you use this code, please cite:

```
Vicente, Alejandro (2026). "Financial Frictions, Intangible Capital and Productivity:
    A Model of Skill-Biased Stagnation." Working Paper, University of Alicante.
```

## Contact

For questions, bugs, or suggestions:
- **Email**: alejandro.vicente@ua.es
- **GitHub**: [Include repository URL when available]

## License

This code is provided for academic research purposes. Please contact the author for commercial use.

## Acknowledgments

This code builds on insights from:
- Peters & Taylor (2017): Intangible capital measurement
- Holttinen et al. (2025): Pledgeability estimates
- Gozen & Ozkara (2024): Intangible-skill complementarity evidence
- Standard heterogeneous firm models (Hopenhayn 1992, Khan & Thomas 2008)

---

**Last updated**: January 9, 2026
