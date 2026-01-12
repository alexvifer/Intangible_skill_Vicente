# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an empirical economics research project analyzing the relationship between intangible capital, skilled labor, and financial constraints using Portuguese firm-level data (2011-2022). The project combines:
- **QP (Quadros de Pessoal)**: Worker and establishment-level data
- **SCIE**: Firm balance sheet data

The pipeline constructs intangible capital stocks using the Perpetual Inventory Method (PIM) and examines three main empirical facts:
1. Complementarity between intangibles and skilled labor
2. Pecking order distortion (financial constraints reduce intangible investment)
3. Underexploitation of complementarity by constrained firms

## Running the Full Pipeline

### Main Entry Point
```stata
cd QP_SCIE/code
do do_all.do
```

**Before running**: Update the `rootdir` global in `do_all.do` (line 47) to match your system path.

The master file executes 9 modules in sequence:
1. `SCIE_clean.do` - Clean balance sheet data
2. `firm_clean.do` - Clean firm characteristics
3. `worker_skills.do` - Process workers and calculate skill measures
4. `merge_final.do` - Merge QP and SCIE datasets
5. `prepare_deflators.do` - Import price deflators (GDP, GFCF, Capital)
6. `prepare_rfr.do` - Import risk-free rate data
7. `data_clean_final.do` - Deflate variables to 2020 prices and apply sample restrictions
8. `construct_intangibles.do` - Build intangible capital stocks with sector-specific depreciation
9. `analysis_prep.do` - Create analytical variables and winsorize

### Running Individual Modules
Each module can be run independently if intermediate datasets exist:
```stata
global rootdir "your/path/to/QP_SCIE"
cd "$rootdir/code"
do SCIE_clean.do
```

### Running Empirical Analysis
After the pipeline completes:
```stata
global rootdir "your/path/to/QP_SCIE"
cd "$rootdir/code"
do facts.do
```

This generates regression tables and figures for all three empirical facts.

## Directory Structure

```
QP_SCIE/
├── code/              # All Stata do files
├── input/
│   ├── QP_data/      # Raw worker/establishment data (.sav files, 2008-2023)
│   ├── SCIE_data/    # Raw balance sheet data (.sav files, 2011-2023)
│   └── *.csv         # Deflators and risk-free rate data
├── output/           # Intermediate .dta files and logs
├── results/          # Final tables (.tex) and figures (.png)
└── Documents/        # Data documentation
```

## Key Architectural Details

### Data Processing Workflow

The pipeline follows a strict dependency chain. Breaking it into phases:

**Phase 1 (Parallel Processing)**: These three modules can run in parallel as they process different source data:
- `SCIE_clean.do`: Cleans balance sheet data → `SCIE_2011_2022.dta`
- `firm_clean.do`: Cleans firm characteristics → `firm_2011_2022.dta`
- `worker_skills.do`: Aggregates workers to firm-year level, calculates skill shares → `firm_skills_wages_2011_2022.dta`

**Phase 2 (Integration)**: Depends on Phase 1 completion:
- `merge_final.do`: Merges all Phase 1 outputs → `final_data_2011_2022.dta`

**Phase 3 (Auxiliary Data)**: Independent, can run in parallel:
- `prepare_deflators.do`: Creates price index dataset → `deflators.dta`
- `prepare_rfr.do`: Creates risk-free rate dataset → `rfr.dta`
- `prepare_2010_debt.do`: Extracts 2010 debt for lag structures → `debt_2010.dta`

**Phase 4 (Cleaning)**: Depends on Phases 2 & 3:
- `data_clean_final.do`: Merges deflators, converts to real 2020 prices, applies restrictions → `final_data_2011_2022_cleaned.dta`

**Phase 5 (Capital Construction)**: Depends on Phase 4:
- `construct_intangibles.do`: Applies Perpetual Inventory Method with sector-specific parameters → `final_data_2011_2022_intangibles.dta`

**Phase 6 (Analysis Prep)**: Depends on Phase 5:
- `analysis_prep.do`: Creates ratios, intensities, winsorizes → `final_data_2011_2022_analysis.dta`

### Intangible Capital Construction

The project supports **multiple intangible capital definitions** simultaneously:

1. **Peters & Taylor (2017) Full** (`K_intangible_pt`):
   - Knowledge capital (from R&D expenses)
   - Organization capital (from SG&A expenses)
   - Balance sheet intangibles

2. **Balance Sheet + R&D** (`K_intangible_bs_rd`):
   - Knowledge capital (from R&D)
   - Balance sheet intangibles only (no organization capital)

**Critical Feature**: Sector-specific depreciation rates from Ewens, Peters & Wang (2025):
- Consumer: 43%
- Manufacturing: 50%
- High Tech: 42%
- Health: 33%
- Other: 35%

Organization capital uses uniform 20% depreciation across all sectors.

The PIM method requires balanced panel for initialization. Missing years are handled by carrying forward the last known stock with depreciation.

### Skill Measures

Skills are classified by education level in `worker_skills.do`:
- **Skilled**: ISCED 5-8 (tertiary education)
- **Unskilled**: ISCED 0-4

Key variables constructed:
- `share_skilled`: Share of workers with tertiary education
- `share_rd_workers`: Share in R&D occupations (CNP codes 21, 214, 215)
- `avg_wage_skilled` / `avg_wage_unskilled`: Average wages by skill group

### Financial Constraint Measures

Multiple measures constructed in `analysis_prep.do`:
- `leverage`: Total debt / Total capital
- `leverage_assets`: Total debt / Total assets
- `interest_rate`: Implicit interest rate (financial expenses / total debt)
- `credit_spread`: Interest rate minus risk-free rate (Portuguese 10-year government bonds)

High/low leverage groups defined relative to **sector-year median** to control for industry heterogeneity in capital structure.

## Key Variables in Final Dataset

After running the full pipeline, `final_data_2011_2022_analysis.dta` contains:

**Identifiers**:
- `firm_id`: Unique firm identifier
- `ano`: Year
- `cae3`: 3-digit industry code
- `ff5_industry`: Fama-French 5 industry classification

**Capital Stocks** (real 2020 euros):
- `K_knowledge`: Knowledge capital from R&D
- `K_org`: Organization capital from SG&A
- `K_intangible_bs`: Balance sheet intangibles
- `K_intangible_pt`: Peters & Taylor full measure
- `K_intangible_bs_rd`: BS + R&D only
- `K_physical`: Tangible fixed assets
- `K_total_*`: Total capital for each intangible definition

**Intensity Measures**:
- `intang_intensity_*`: K_intangible / K_total (for each definition)
- `share_skilled`: Share of tertiary-educated workers
- `share_rd_workers`: Share of R&D workers
- `rnd_intensity`: R&D expenses / Revenue

**Investment Rates** (normalized by K_total):
- `inv_rate_physical`
- `inv_rate_knowledge` (R&D investment)
- `inv_rate_org` (SG&A investment)
- `inv_rate_intang_*` (for each intangible definition)

**Financial Variables**:
- `leverage`, `leverage_assets`
- `interest_rate`, `credit_spread`
- `rfr`: Risk-free rate

**Winsorized versions**: Variables with `_w` or `_win` suffix are winsorized at appropriate percentiles.

## Working with the Data

### Panel Structure
Always declare the panel before analysis:
```stata
xtset firm_id ano
```

### Year Range
Data spans 2011-2022, but:
- SCIE data available 2011-2023
- QP worker data available 2008-2023
- Analysis typically focuses on 2011-2022 due to merge requirements

### Missing Data Handling
- Firms must have both QP and SCIE data to appear in final dataset
- Missing R&D or SG&A treated as zero (common in non-R&D-intensive firms)
- Balance sheet intangibles can be zero (many firms have no recorded intangibles)

### Sector Classification
The project uses Fama-French 5 (`ff5_industry`) based on CAE codes for sector-specific parameters. This mapping is critical for intangible capital construction.

## Common Tasks

### Re-running with Different Intangible Measures
To add a new intangible capital definition, edit `construct_intangibles.do`:
1. Define new depreciation rates
2. Add PIM loop for new measure
3. Create corresponding `K_total_*` variable
4. Update intensity measures in `analysis_prep.do`

### Changing Sample Restrictions
Edit `data_clean_final.do` around the sample restriction section. Common restrictions:
- Positive employment
- Non-negative balance sheet items
- Minimum firm age
- Valid industry codes

### Modifying Winsorization
Winsorization levels set in `analysis_prep.do`. Current defaults:
- Financial variables: 5%-95% by year-sector
- Investment rates: 5%-95% by year-sector
- Intensities: 1%-99% typically

### Adding New Deflators
Add data to `QP_SCIE/input/` and modify `prepare_deflators.do` to import and merge.

## Important Notes

- All monetary variables in the final dataset are in **real 2020 euros**
- The pipeline is **idempotent**: running twice produces identical results if input data unchanged
- Logs are saved to `output/` directory for each module
- The master log `master_pipeline.log` contains timing information for each phase
- Graph exports require `binscatter` command (install via `ssc install binscatter` if needed)
- Regression tables require `estout` package (`ssc install estout`)
- Some modules require `reghdfe` for high-dimensional fixed effects (`ssc install reghdfe ftools`)
- Winsorization requires `winsor2` command (`ssc install winsor2`)

## Data Sources

- **QP**: Portuguese Ministry of Labor matched employer-employee data
- **SCIE**: Portuguese Central Balance Sheet database
- **Deflators**: Statistics Portugal (INE)
- **Risk-free rate**: FRED (Portuguese 10-year government bond yields)

## File Naming Convention

Input files follow pattern:
- QP: `QP_{Trabalhadores|Estabelecimentos|Empresas}_{YEAR}_*.sav`
- SCIE: `SCIE{YEAR}_SEC2010_*.sav`

Output datasets follow progressive naming:
- `{source}_{years}.dta` for cleaned inputs
- `final_data_{years}_{stage}.dta` for merged outputs

---

# Structural Model (Fortran Code)

## Overview

The `model_fortran/` directory contains a complete implementation of the structural heterogeneous firm model described in the Model.pdf paper. This is a quantitative general equilibrium model with:

- **Heterogeneous firms**: Idiosyncratic productivity shocks (AR(1))
- **Two capital types**: Tangible (K) and Intangible (S)
- **Two labor types**: Unskilled (L) and Skilled (H = HP + HR)
- **Nested CES production**: Complementarity between intangibles and skilled labor
- **Financial frictions**: Collateral constraint with αK > αS (tangible more pledgeable)
- **Entry/exit dynamics**: Constant exit probability ζ, free entry condition

## Directory Structure

```
model_fortran/
├── src/                        # Fortran source code
│   ├── mod_parameters.f90      # All model parameters
│   ├── mod_globals.f90         # Global variables and grids
│   ├── mod_utility.f90         # Production functions and CES aggregators
│   ├── mod_interpolation.f90   # Multi-dimensional interpolation
│   ├── mod_firm_problem.f90    # Value function iteration
│   ├── mod_distribution.f90    # Stationary distribution computation
│   ├── mod_equilibrium.f90     # Equilibrium solver and grid initialization
│   └── main.f90                # Main program
├── build/                      # Compiled objects (auto-generated)
├── output/                     # Results
│   ├── aggregates.txt          # Equilibrium aggregates
│   └── distribution.txt        # Firm distribution over state space
├── Makefile                    # Build system
└── README.md                   # Comprehensive documentation
```

## Compiling and Running

### Quick Start

```bash
cd model_fortran
make              # Compile with optimization
./model_vicente   # Run the model
```

### Requirements

- **Fortran compiler**: gfortran (GNU) or ifort (Intel)
- **RAM**: 4-8 GB
- **Runtime**: 30-60 minutes for medium grid (depends on grid size)

### Build Targets

```bash
make          # Compile with optimization (default)
make debug    # Compile with debug flags and runtime checks
make clean    # Remove compiled files
make run      # Compile and run
make time     # Compile and run with timing
```

## Model Architecture

### Module Hierarchy

The code is highly modular with clear dependency structure:

```
mod_parameters (standalone)
    └── mod_globals (uses parameters)
            ├── mod_utility (production functions)
            ├── mod_interpolation (for value function)
            │       └── mod_firm_problem (VFI solver)
            │       └── mod_distribution (stationary dist)
            └── mod_equilibrium (wage iteration)
                    └── main (orchestrates solution)
```

### Solution Algorithm

1. **Grid Construction** (`initialize_grids`)
   - Tauchen discretization for AR(1) productivity process
   - Log-spaced tangible capital grid
   - Linear intangible capital and debt grids

2. **Value Function Iteration** (`solve_firm_problem`)
   - State: (z, K, S, D₋₁)
   - Static choices: L, HP (solve FOCs analytically)
   - Dynamic choices: I^K, H^R, D (grid search)
   - Continuation value computed via multilinear interpolation
   - Iterate until ||V_new - V|| < 10⁻⁶

3. **Stationary Distribution** (`compute_stationary_distribution`)
   - Young iteration with trilinear interpolation weights
   - Account for entry (at K=S=D=0) and exit (probability ζ)
   - Iterate until ||dist_new - dist|| < 10⁻⁶

4. **Equilibrium** (`solve_equilibrium`)
   - Outer loop: iterate on wages (wL, wH)
   - Inner loop: VFI → distribution → aggregates
   - Update wages based on excess demand
   - Converge when labor markets clear

## Key Parameters

Located in `src/mod_parameters.f90`:

### Technology
- `alpha_prod = 0.33`: Capital share in production
- `nu = 0.85`: Returns to scale parameter
- `rho_K = -0.50`: Elasticity K-Q bundle (σK ≈ 0.67)
- `rho_Q = -1.50`: Elasticity S-HP bundle (σQ ≈ 0.40)

### Financial Frictions
- `alpha_K = 0.381`: Tangible pledgeability (from Holttinen et al. 2025)
- `alpha_S = 0.134`: Intangible pledgeability
- The gap (αK - αS = 0.247) drives the pecking-order distortion

### Capital Accumulation
- `delta_K = 0.10`: Tangible depreciation
- `delta_S = 0.15`: Intangible depreciation
- `Gamma_RD = 1.50`: R&D productivity
- `xi = 0.90`: R&D returns to scale

### Grid Dimensions
- `nz = 15`: Productivity states
- `nK = 40`: Tangible capital grid points
- `nS = 40`: Intangible capital grid points
- `nD = 30`: Debt grid points

**Total state space**: nz × nK × nS × nD = 720,000 points

## Counterfactual Experiments

### 1. Skill Supply Shock

Edit `src/mod_parameters.f90`:
```fortran
Hbar = 0.45   ! Increase from baseline 0.40
Lbar = 0.55   ! Decrease to maintain sum = 1
```

Recompile and compare results to measure skill-biased stagnation effect.

### 2. Financial Liberalization

Edit `src/mod_parameters.f90`:
```fortran
alpha_S = 0.25   ! Increase from baseline 0.134
```

Simulates financial innovation improving intangible collateral value.

### 3. First-Best Benchmark

Edit `src/mod_parameters.f90`:
```fortran
alpha_S = 1.00   ! Perfect pledgeability
alpha_K = 1.00
```

Compute efficiency losses from financial frictions.

## Output Files

### `output/aggregates.txt`

Contains equilibrium values:
- Output (Y), Consumption (C)
- Capital stocks (K, S)
- Labor allocations (L, HP, HR)
- Wages (wL, wH)
- Fraction of constrained firms
- Average leverage and intangible intensity

### `output/distribution.txt`

Firm distribution over (z,K,S,D) in sparse format (non-zero entries only).

## Numerical Considerations

### Grid Size Trade-off

- **Small**: nz=7, nK=20, nS=20, nD=15 → ~5-15 min
- **Medium**: nz=11, nK=30, nS=30, nD=20 → ~30-60 min
- **Large**: nz=15, nK=40, nS=40, nD=30 → ~2-4 hours

Larger grids capture firm heterogeneity more accurately but increase computation time.

### Convergence

If VFI or distribution doesn't converge:
1. Reduce update weights: `update_VFI = 0.3`, `update_dist = 0.05`
2. Increase max iterations: `maxiter_VFI = 3000`
3. Check grid bounds cover relevant state space

### Parallelization

Code uses OpenMP. Set thread count:
```bash
export OMP_NUM_THREADS=8
./model_vicente
```

## Extending the Model

The modular structure facilitates extensions:

- **Modify production function**: Edit `mod_utility.f90`
- **Add new state variables**: Expand grids in `mod_globals.f90`
- **Change financial friction**: Edit constraint in `mod_firm_problem.f90`
- **Add aggregate shocks**: Introduce time-varying parameters
- **Policy experiments**: Subsidies, taxes, regulations

## Technical Notes

- **State space**: (z, K, S, D₋₁) where z is discrete, others continuous
- **Interpolation**: Trilinear for (K, S, D) given discrete z
- **Distribution**: Young iteration with mass reallocation
- **Entry**: All entrants start at (z, K_min, S_min, D_min) with z drawn from stationary dist
- **Exit**: Exiting firms distribute liquidation value to households

## References for Fortran Model

The implementation follows standard numerical methods from:
- Tauchen (1986) for AR(1) discretization
- Stokey & Lucas (1989) for VFI and distribution iteration
- Khan & Thomas (2008) for firm dynamics
- Contemporary heterogeneous firm models (Ottonello & Winberry 2020)

---

## Integration: Empirical + Structural

The empirical analysis (Stata) provides:
1. **Reduced-form facts** to guide model mechanisms
2. **Parameter targets** for calibration (e.g., leverage, intangible intensity)
3. **Validation** of model predictions

The structural model (Fortran) provides:
1. **Quantitative predictions** of policy counterfactuals
2. **Welfare analysis** of financial frictions
3. **Decomposition** of TFP into technology vs. misallocation
