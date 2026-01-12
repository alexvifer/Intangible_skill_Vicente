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
    real(dp) :: Lbar = 0.60_dp                     ! Unskilled labor supply
    real(dp) :: Hbar = 0.40_dp                     ! Skilled labor supply
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
    ! FINANCIAL FRICTION PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: alpha_K = 0.381_dp      ! Tangible pledgeability (Holttinen 2025)
    real(dp), parameter :: alpha_S = 0.134_dp      ! Intangible pledgeability
    ! Note: alpha_S < alpha_K creates pecking-order distortion

    !---------------------------------------------------------------------------
    ! PRODUCTIVITY PROCESS (AR(1) in logs)
    !---------------------------------------------------------------------------
    ! log(z') = ρz * log(z) + σz * ε,  ε ~ N(0,1)
    real(dp), parameter :: rho_z = 0.95_dp         ! Persistence
    real(dp), parameter :: sigma_z = 0.15_dp       ! Std dev of innovations
    integer, parameter :: nz = 15                  ! Grid points for z
    real(dp), parameter :: m_z = 3.0_dp            ! Grid coverage (m std devs)

    !---------------------------------------------------------------------------
    ! ENTRY AND EXIT
    !---------------------------------------------------------------------------
    real(dp), parameter :: zeta = 0.10_dp          ! Exit probability
    real(dp), parameter :: ce = 1.00_dp            ! Entry cost (to be calibrated)

    !---------------------------------------------------------------------------
    ! STATE SPACE GRIDS
    !---------------------------------------------------------------------------
    integer, parameter :: nK = 40                  ! Grid points for tangible capital
    integer, parameter :: nS = 40                  ! Grid points for intangible capital
    integer, parameter :: nD = 30                  ! Grid points for debt

    ! Grid bounds (will be adjusted based on solution)
    real(dp), parameter :: K_min = 0.10_dp
    real(dp), parameter :: K_max = 100.0_dp
    real(dp), parameter :: S_min = 0.0_dp
    real(dp), parameter :: S_max = 50.0_dp
    real(dp), parameter :: D_min = 0.0_dp
    real(dp), parameter :: D_max = 50.0_dp

    !---------------------------------------------------------------------------
    ! CHOICE GRIDS
    !---------------------------------------------------------------------------
    integer, parameter :: nIK = 50                 ! Grid points for tangible investment
    integer, parameter :: nHR = 30                 ! Grid points for R&D labor
    integer, parameter :: nDprime = 35             ! Grid points for new debt

    !---------------------------------------------------------------------------
    ! NUMERICAL PARAMETERS
    !---------------------------------------------------------------------------
    real(dp), parameter :: tol_VFI = 1.0e-6_dp     ! Tolerance for value function iteration
    real(dp), parameter :: tol_dist = 1.0e-6_dp    ! Tolerance for distribution
    real(dp), parameter :: tol_eq = 1.0e-4_dp      ! Tolerance for equilibrium
    integer, parameter :: maxiter_VFI = 2000       ! Max iterations for VFI
    integer, parameter :: maxiter_dist = 5000      ! Max iterations for distribution
    integer, parameter :: maxiter_eq = 50          ! Max iterations for equilibrium
    real(dp), parameter :: update_VFI = 0.50_dp    ! Update weight for VFI (Howard improvement)
    real(dp), parameter :: update_dist = 0.10_dp   ! Update weight for distribution
    real(dp), parameter :: update_wage = 0.20_dp   ! Update weight for wage iteration

    ! Small number to avoid division by zero
    real(dp), parameter :: epsilon = 1.0e-10_dp

    !---------------------------------------------------------------------------
    ! OUTPUT CONTROL
    !---------------------------------------------------------------------------
    logical, parameter :: print_progress = .true.
    integer, parameter :: print_freq = 100         ! Print every N iterations

end module mod_parameters
