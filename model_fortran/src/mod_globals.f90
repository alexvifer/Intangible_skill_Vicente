!===================================================================================
! MODULE: mod_globals
!
! DESCRIPTION:
!   Global variables, grids, and arrays for the model solution.
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_globals
    use mod_parameters
    implicit none

    !---------------------------------------------------------------------------
    ! STATE SPACE GRIDS
    !---------------------------------------------------------------------------
    real(dp), allocatable :: grid_z(:)             ! Productivity grid
    real(dp), allocatable :: grid_K(:)             ! Tangible capital grid
    real(dp), allocatable :: grid_S(:)             ! Intangible capital grid
    real(dp), allocatable :: grid_D(:)             ! Debt grid

    ! Transition matrix for productivity
    real(dp), allocatable :: Pi_z(:,:)             ! Markov transition (nz x nz)
    real(dp), allocatable :: stat_dist_z(:)        ! Stationary distribution

    !---------------------------------------------------------------------------
    ! CHOICE GRIDS
    !---------------------------------------------------------------------------
    real(dp), allocatable :: grid_IK(:)            ! Tangible investment choices
    real(dp), allocatable :: grid_HR(:)            ! R&D labor choices
    real(dp), allocatable :: grid_Dprime(:)        ! New debt choices

    !---------------------------------------------------------------------------
    ! VALUE FUNCTIONS AND POLICY FUNCTIONS
    !---------------------------------------------------------------------------
    ! Value function: V(z, K, S, D_{-1})
    real(dp), allocatable :: V(:,:,:,:)            ! (nz, nK, nS, nD)
    real(dp), allocatable :: V_new(:,:,:,:)        ! For iteration

    ! Policy functions
    real(dp), allocatable :: pol_Kprime(:,:,:,:)   ! K' choice
    real(dp), allocatable :: pol_Sprime(:,:,:,:)   ! S' choice
    real(dp), allocatable :: pol_Dprime(:,:,:,:)   ! D' choice
    real(dp), allocatable :: pol_IK(:,:,:,:)       ! I^K choice
    real(dp), allocatable :: pol_HR(:,:,:,:)       ! H^R choice
    real(dp), allocatable :: pol_L(:,:,:,:)        ! L choice (static)
    real(dp), allocatable :: pol_HP(:,:,:,:)       ! H^P choice (static)

    ! Auxiliary policy functions
    real(dp), allocatable :: pol_Y(:,:,:,:)        ! Output
    real(dp), allocatable :: pol_mu(:,:,:,:)       ! Shadow value of funds
    real(dp), allocatable :: pol_lambda(:,:,:,:)   ! Collateral constraint multiplier
    logical, allocatable :: pol_constr(:,:,:,:)    ! Constraint binding indicator

    ! Policy indices for local search optimization
    integer, allocatable :: pol_iIK(:,:,:,:)       ! Index of optimal I^K
    integer, allocatable :: pol_iHR(:,:,:,:)       ! Index of optimal H^R
    integer, allocatable :: pol_iDp(:,:,:,:)       ! DEPRECATED: D' now computed analytically (kept for compatibility)

    !---------------------------------------------------------------------------
    ! PRECOMPUTED STATIC LABOR SOLUTIONS
    ! These only depend on (z, K, S) and wages, not on debt D
    !---------------------------------------------------------------------------
    real(dp), allocatable :: static_L(:,:,:)       ! Optimal L(z,K,S)
    real(dp), allocatable :: static_HP(:,:,:)      ! Optimal HP(z,K,S)
    real(dp), allocatable :: static_Y(:,:,:)       ! Output Y(z,K,S)
    real(dp), allocatable :: static_Pi(:,:,:)      ! Gross profits Pi(z,K,S)

    !---------------------------------------------------------------------------
    ! FIRM DISTRIBUTION
    !---------------------------------------------------------------------------
    real(dp), allocatable :: dist(:,:,:,:)         ! Firm distribution Ψ(z,K,S,D)
    real(dp), allocatable :: dist_new(:,:,:,:)     ! For iteration

    !---------------------------------------------------------------------------
    ! EQUILIBRIUM PRICES AND QUANTITIES
    !---------------------------------------------------------------------------
    real(dp) :: wL                                 ! Unskilled wage
    real(dp) :: wH                                 ! Skilled wage

    ! Aggregate quantities
    real(dp) :: agg_C                              ! Consumption
    real(dp) :: agg_K                              ! Tangible capital
    real(dp) :: agg_S                              ! Intangible capital
    real(dp) :: agg_L                              ! Unskilled labor demand
    real(dp) :: agg_HP                             ! Skilled labor in production
    real(dp) :: agg_HR                             ! Skilled labor in R&D
    real(dp) :: agg_H                              ! Total skilled labor demand
    real(dp) :: agg_D                              ! Total debt
    real(dp) :: agg_Y                              ! Output
    real(dp) :: agg_IK                             ! Tangible investment
    real(dp) :: mass_firms                         ! Mass of firms (=1 by normalization)

    ! Statistics for reporting
    real(dp) :: frac_constrained                   ! Fraction of constrained firms
    real(dp) :: avg_leverage                       ! Average leverage
    real(dp) :: avg_intang_intensity               ! Average S/(K+S)

    !---------------------------------------------------------------------------
    ! ITERATION CONTROL
    !---------------------------------------------------------------------------
    integer :: iter_VFI                            ! VFI iteration counter
    integer :: iter_dist                           ! Distribution iteration counter
    integer :: iter_eq                             ! Equilibrium iteration counter
    real(dp) :: metric_VFI                         ! VFI convergence metric
    real(dp) :: metric_dist                        ! Distribution convergence metric
    real(dp) :: metric_eq_L                        ! Labor market clearing metric
    real(dp) :: metric_eq_H                        ! Skilled labor market clearing

contains

    !===================================================================================
    ! SUBROUTINE: allocate_arrays
    !
    ! DESCRIPTION:
    !   Allocates all global arrays based on grid dimensions.
    !===================================================================================
    subroutine allocate_arrays()
        implicit none

        ! State grids
        allocate(grid_z(nz))
        allocate(grid_K(nK))
        allocate(grid_S(nS))
        allocate(grid_D(nD))
        allocate(Pi_z(nz,nz))
        allocate(stat_dist_z(nz))

        ! Choice grids
        allocate(grid_IK(nIK))
        allocate(grid_HR(nHR))
        allocate(grid_Dprime(nDprime))

        ! Value and policy functions
        allocate(V(nz,nK,nS,nD))
        allocate(V_new(nz,nK,nS,nD))
        allocate(pol_Kprime(nz,nK,nS,nD))
        allocate(pol_Sprime(nz,nK,nS,nD))
        allocate(pol_Dprime(nz,nK,nS,nD))
        allocate(pol_IK(nz,nK,nS,nD))
        allocate(pol_HR(nz,nK,nS,nD))
        allocate(pol_L(nz,nK,nS,nD))
        allocate(pol_HP(nz,nK,nS,nD))
        allocate(pol_Y(nz,nK,nS,nD))
        allocate(pol_mu(nz,nK,nS,nD))
        allocate(pol_lambda(nz,nK,nS,nD))
        allocate(pol_constr(nz,nK,nS,nD))

        ! Policy indices for local search
        allocate(pol_iIK(nz,nK,nS,nD))
        allocate(pol_iHR(nz,nK,nS,nD))
        allocate(pol_iDp(nz,nK,nS,nD))

        ! Precomputed static labor solutions
        allocate(static_L(nz,nK,nS))
        allocate(static_HP(nz,nK,nS))
        allocate(static_Y(nz,nK,nS))
        allocate(static_Pi(nz,nK,nS))

        ! Distribution
        allocate(dist(nz,nK,nS,nD))
        allocate(dist_new(nz,nK,nS,nD))

        ! Initialize
        V = 0.0_dp
        V_new = 0.0_dp
        dist = 0.0_dp
        dist_new = 0.0_dp
        pol_constr = .false.

        ! Initialize policy indices to middle of grids (for local search)
        ! Note: pol_iDp deprecated (D' now computed analytically)
        pol_iIK = nIK / 2
        pol_iHR = nHR / 2
        pol_iDp = 1  ! Unused but kept for compatibility

        ! Initialize static arrays
        static_L = 0.0_dp
        static_HP = 0.0_dp
        static_Y = 0.0_dp
        static_Pi = 0.0_dp

        print *, "Arrays allocated successfully."

    end subroutine allocate_arrays

    !===================================================================================
    ! SUBROUTINE: deallocate_arrays
    !
    ! DESCRIPTION:
    !   Deallocates all global arrays.
    !===================================================================================
    subroutine deallocate_arrays()
        implicit none

        deallocate(grid_z, grid_K, grid_S, grid_D)
        deallocate(Pi_z, stat_dist_z)
        deallocate(grid_IK, grid_HR, grid_Dprime)
        deallocate(V, V_new)
        deallocate(pol_Kprime, pol_Sprime, pol_Dprime)
        deallocate(pol_IK, pol_HR, pol_L, pol_HP, pol_Y)
        deallocate(pol_mu, pol_lambda, pol_constr)
        deallocate(pol_iIK, pol_iHR, pol_iDp)
        deallocate(static_L, static_HP, static_Y, static_Pi)
        deallocate(dist, dist_new)

        print *, "Arrays deallocated successfully."

    end subroutine deallocate_arrays

end module mod_globals
