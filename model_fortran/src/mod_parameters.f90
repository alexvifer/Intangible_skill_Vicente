!===================================================================================
! MODULE: mod_parameters
!
! DESCRIPTION:
!   Contains all model parameters for the heterogeneous firm model with intangible
!   capital and financial frictions.
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_parameters
    implicit none

    ! Precision
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp), parameter :: pi = 3.141592653589793_dp

    !---------------------------------------------------------------------------
    ! HOUSEHOLD PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: beta = 0.96_dp          ! Discount factor
    real(dp), parameter :: R = 1.0_dp / beta       ! Risk-free rate (1/beta)

    !---------------------------------------------------------------------------
    ! LABOR SUPPLY (exogenous)
    !---------------------------------------------------------------------------
    real(dp) :: Lbar = 0.85_dp                     ! Unskilled labor supply
    real(dp) :: Hbar = 0.15_dp                     ! Skilled labor supply
    ! Note: Lbar + Hbar = 1 (normalized total labor)

    !---------------------------------------------------------------------------
    ! PRODUCTION FUNCTION PARAMETERS
    !---------------------------------------------------------------------------
    ! Outer nest: Y = z * [X^α * L^γ]^ν
    real(dp), parameter :: alpha_prod = 0.33_dp    ! Capital share in inner nest
    real(dp), parameter :: gamma_prod = 0.67_dp    ! Labor share in inner nest (=1-α)
    real(dp), parameter :: nu = 0.85_dp            ! Returns to scale parameter

    ! Capital composite: X = [θK*K^ρK + θQ*Q^ρK]^(1/ρK)
    real(dp), parameter :: theta_K = 0.65_dp       ! Weight on tangible capital
    real(dp), parameter :: theta_Q = 0.35_dp       ! Weight on intangible-skill bundle
    real(dp), parameter :: rho_K = -0.50_dp        ! Substitution param (σK = 2/3)
    real(dp), parameter :: sigma_K = 1.0_dp / (1.0_dp - rho_K)  ! Elasticity

    ! Intangible-skill bundle: Q = [ω*S^ρQ + (1-ω)*(HP)^ρQ]^(1/ρQ)
    real(dp), parameter :: omega = 0.50_dp         ! Weight on intangible capital
    real(dp), parameter :: rho_Q = -1.50_dp        ! Substitution param (σQ = 0.4)
    real(dp), parameter :: sigma_Q = 1.0_dp / (1.0_dp - rho_Q)  ! Elasticity

    !---------------------------------------------------------------------------
    ! CAPITAL ACCUMULATION PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: delta_K = 0.10_dp       ! Tangible capital depreciation
    real(dp), parameter :: delta_S = 0.15_dp       ! Intangible capital depreciation

    ! R&D production: S' = (1-δS)*S + Γ*(HR)^ξ
    real(dp), parameter :: Gamma_RD = 1.50_dp      ! R&D productivity parameter
    real(dp), parameter :: xi = 0.90_dp            ! Returns to scale in R&D (≤1)

    !---------------------------------------------------------------------------
    ! ADJUSTMENT COST PARAMETERS
    ! Convex adjustment costs: AC = (φ/2) * (I/K)^2 * K
    ! Setting φ = 0 gives no adjustment costs (nests frictionless investment)
    !---------------------------------------------------------------------------
    real(dp), parameter :: phi_K = 0.0_dp          ! Tangible capital adjustment cost
    real(dp), parameter :: phi_S = 0.0_dp          ! Intangible capital adjustment cost
    ! Note: Adjustment costs apply to gross investment I^K and new intangibles Γ*(H^R)^ξ
    ! Baseline sets both to 0; can be increased for robustness exercises

    !---------------------------------------------------------------------------
    ! FINANCIAL FRICTION PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: alpha_K = 0.781_dp    ! Tangible pledgeability (Holttinen et al. 2025)
    real(dp), parameter :: alpha_S = 0.134_dp    ! Intangible pledgeability (Holttinen et al. 2025)
    ! Note: alpha_S < alpha_K creates pecking-order distortion

    ! Costly equity issuance - DISABLED (hard div >= 0 constraint instead)
    ! Kept for reference. Hennessy & Whited (2007) range: 4-15%.
    ! real(dp), parameter :: lambda_equity = 0.06_dp

    !---------------------------------------------------------------------------
    ! COUNTERFACTUAL: NO FINANCIAL CONSTRAINTS
    ! When true, sets λ=0 for all firms (collateral constraint never binds).
    ! This removes the constraint entirely from the optimization, allowing
    ! unlimited borrowing. Use this for the "first-best" benchmark.
    ! NOTE: This is different from setting alpha=1, which still limits
    ! borrowing to collateral value. Young/productive firms with little
    ! collateral can still be constrained under alpha=1.
    !---------------------------------------------------------------------------
    logical, parameter :: remove_collateral_constraint = .false.

    !---------------------------------------------------------------------------
    ! PRODUCTIVITY PROCESS (AR(1) in logs)
    !---------------------------------------------------------------------------
    ! log(z') = ρz * log(z) + σz * ε,  ε ~ N(0,1)
    real(dp), parameter :: rho_z = 0.95_dp         ! Persistence
    real(dp), parameter :: sigma_z = 0.15_dp       ! Std dev of innovations
    integer, parameter :: nz = 11                  ! Grid points for z (increased from 5)
    real(dp), parameter :: m_z = 3.0_dp            ! Grid coverage (m std devs)

    !---------------------------------------------------------------------------
    ! ENTRY AND EXIT
    !---------------------------------------------------------------------------
    real(dp), parameter :: zeta = 0.10_dp          ! Exit probability
    real(dp), parameter :: ce = 1.00_dp            ! Entry cost (to be calibrated)

    !---------------------------------------------------------------------------
    ! STATE SPACE GRIDS
    !---------------------------------------------------------------------------
    integer, parameter :: nK = 50                  ! Grid points for tangible capital (increased from 30)
    integer, parameter :: nS = 50                  ! Grid points for intangible capital (increased from 30)
    integer, parameter :: nD = 20                  ! Grid points for debt (increased from 15)

    ! Grid bounds (will be adjusted based on solution)
    ! Note: S_min > 0 required for numerical stability with CES complements (rho < 0)
    ! Using small S_min to avoid "free intangibles" from grid bound clamping
    real(dp), parameter :: K_min = 0.01_dp
    real(dp), parameter :: K_max = 2.0_dp
    real(dp), parameter :: S_min = 0.01_dp         ! Small but positive for CES stability
    real(dp), parameter :: S_max = 2.5_dp          ! Must exceed max achievable S'
    real(dp), parameter :: D_min = 0.0_dp
    real(dp), parameter :: D_max = 1.20_dp          ! Must cover max collateral value

    !---------------------------------------------------------------------------
    ! CHOICE GRIDS
    ! Total choice combinations per state = nIK * nHR (D' is analytical)
    !---------------------------------------------------------------------------
    integer, parameter :: nIK = 30                 ! Grid points for tangible investment
    integer, parameter :: nHR = 30                 ! Grid points for R&D labor
    integer, parameter :: nDprime = 20             ! Grid points for new debt (used by grid_Dprime)

    ! Maximum R&D labor per firm (decoupled from Hbar for proper optimization)
    ! Individual firms CAN demand more than aggregate supply - it's the weighted
    ! average that must clear. Setting HR_max > Hbar allows high-z firms to
    ! optimally choose large R&D without artificial truncation.
    real(dp), parameter :: HR_max = 0.50_dp        ! Max HR per firm (was Hbar=0.15)

    !---------------------------------------------------------------------------
    ! NUMERICAL PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: tol_VFI = 1.0e-4_dp     ! Tolerance for value function iteration
    real(dp), parameter :: tol_dist = 1.0e-4_dp    ! Tolerance for distribution
    real(dp), parameter :: tol_eq = 5.0e-2_dp      ! Tolerance for equilibrium (relative excess demand)
    integer, parameter :: maxiter_VFI = 3000       ! Max iterations for VFI (increased from 2000)
    integer, parameter :: maxiter_dist = 5000      ! Max iterations for distribution
    integer, parameter :: maxiter_eq = 300         ! Max iterations for equilibrium (increased from 100)
    real(dp), parameter :: update_VFI = 1.0_dp     ! Full VFI update (no dampening needed with limited liability)
    real(dp), parameter :: update_dist = 0.10_dp   ! Update weight for distribution
    real(dp), parameter :: update_wage = 0.05_dp   ! Update weight for wage iteration

    ! Howard's Policy Improvement parameters
    ! Do full policy optimization every howard_freq iterations
    ! Between optimizations, just evaluate V with fixed policy (much faster)
    integer, parameter :: howard_freq = 15         ! Policy improvement frequency
    integer, parameter :: howard_eval_steps = 20   ! Max evaluation steps between improvements

    ! Small number to avoid division by zero
    real(dp), parameter :: epsilon = 1.0e-10_dp

    !---------------------------------------------------------------------------
    ! OUTPUT CONTROL
    !---------------------------------------------------------------------------
    logical, parameter :: print_progress = .true.
    integer, parameter :: print_freq = 10          ! Print every N iterations

end module mod_parameters