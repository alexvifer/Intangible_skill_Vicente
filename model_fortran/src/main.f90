!===================================================================================
! PROGRAM: main
!
! DESCRIPTION:
!   Main program for solving Vicente (2026) heterogeneous firm model with
!   intangible capital and financial frictions.
!
!   Model features:
!     - Heterogeneous firms with idiosyncratic productivity shocks
!     - Two types of capital: tangible (K) and intangible (S)
!     - Two types of labor: unskilled (L) and skilled (H = HP + HR)
!     - Nested CES production with complementarity: S and HP are complements
!     - Collateral constraint: tangible more pledgeable than intangible
!     - Entry and exit dynamics
!
!   Solution method:
!     1. VFI to solve firm problem
!     2. Distribution iteration for stationary equilibrium
!     3. Outer loop for wage equilibrium (labor market clearing)
!
! USAGE:
!   ./main
!
! AUTHOR: Alejandro Vicente
! DATE: 2026-01-09
!===================================================================================
program main
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_interpolation
    use mod_firm_problem
    use mod_distribution
    use mod_equilibrium
    implicit none

    real(dp) :: start_time, end_time

    ! Print header
    print *, ""
    print *, "=========================================================================="
    print *, "   HETEROGENEOUS FIRM MODEL WITH INTANGIBLE CAPITAL"
    print *, "   Financial Frictions and Skill-Biased Stagnation"
    print *, ""
    print *, "   Author: Alejandro Vicente"
    print *, "   Institution: University of Alicante"
    print *, "   Date: January 2026"
    print *, "=========================================================================="
    print *, ""

    ! Record start time
    call cpu_time(start_time)

    !---------------------------------------------------------------------------
    ! STEP 1: SETUP
    !---------------------------------------------------------------------------
    print *, "STEP 1: Setting up model..."
    print *, ""
    print *, "Model parameters:"
    print '(A,F8.4)', "  Discount factor (β):              ", beta
    print '(A,F8.4)', "  Risk-free rate (R = 1/β):         ", R
    print '(A,F8.4)', "  Exit probability (ζ):             ", zeta
    print *, ""
    print *, "Labor supply:"
    print '(A,F8.4)', "  Unskilled (L̄):                    ", Lbar
    print '(A,F8.4)', "  Skilled (H̄):                      ", Hbar
    print *, ""
    print *, "Production function:"
    print '(A,F8.4)', "  Capital share (α):                ", alpha_prod
    print '(A,F8.4)', "  Returns to scale (ν):             ", nu
    print '(A,F8.4)', "  Elasticity σK (K-Q substitution): ", sigma_K
    print '(A,F8.4)', "  Elasticity σQ (S-HP substitution):", sigma_Q
    print *, ""
    print *, "Capital accumulation:"
    print '(A,F8.4)', "  Tangible depreciation (δK):       ", delta_K
    print '(A,F8.4)', "  Intangible depreciation (δS):     ", delta_S
    print '(A,F8.4)', "  R&D productivity (Γ):             ", Gamma_RD
    print '(A,F8.4)', "  R&D returns to scale (ξ):         ", xi
    print *, ""
    print *, "Financial frictions:"
    print '(A,F8.4)', "  Tangible pledgeability (αK):      ", alpha_K
    print '(A,F8.4)', "  Intangible pledgeability (αS):    ", alpha_S
    print '(A,F8.4)', "  Pledgeability gap (αK - αS):      ", alpha_K - alpha_S
    print *, ""
    print *, "Productivity process:"
    print '(A,F8.4)', "  Persistence (ρz):                 ", rho_z
    print '(A,F8.4)', "  Std deviation (σz):               ", sigma_z
    print *, ""
    print *, "Grid dimensions:"
    print '(A,I5)', "  Productivity (nz):                ", nz
    print '(A,I5)', "  Tangible capital (nK):            ", nK
    print '(A,I5)', "  Intangible capital (nS):          ", nS
    print '(A,I5)', "  Debt (nD):                        ", nD
    print '(A,I5)', "  Total state space points:         ", nz * nK * nS * nD
    print *, ""

    ! Allocate arrays
    call allocate_arrays()

    ! Initialize grids
    call initialize_grids()

    !---------------------------------------------------------------------------
    ! STEP 2: SOLVE MODEL
    !---------------------------------------------------------------------------
    print *, ""
    print *, "STEP 2: Solving model..."
    print *, ""

    ! Solve for equilibrium
    call solve_equilibrium()

    !---------------------------------------------------------------------------
    ! STEP 3: DIAGNOSTICS AND SAVE RESULTS
    !---------------------------------------------------------------------------
    print *, ""
    print *, "STEP 3: Computing diagnostics and saving results..."

    ! Compute and display detailed distribution diagnostics
    call compute_distribution_diagnostics()

    ! Save results to files
    call save_results()

    !---------------------------------------------------------------------------
    ! STEP 4: COUNTERFACTUALS (OPTIONAL)
    !---------------------------------------------------------------------------
    print *, ""
    print *, "=========================================================================="
    print *, "   COUNTERFACTUAL EXPERIMENTS"
    print *, "=========================================================================="
    print *, ""
    print *, "To run counterfactual experiments:"
    print *, "  1. Change Hbar in mod_parameters.f90"
    print *, "  2. Change alpha_S to study pledgeability effects"
    print *, "  3. Recompile and run"
    print *, ""

    !---------------------------------------------------------------------------
    ! CLEANUP AND EXIT
    !---------------------------------------------------------------------------
    call deallocate_arrays()

    ! Record end time
    call cpu_time(end_time)

    print *, ""
    print *, "=========================================================================="
    print '(A,F10.2,A)', "   Total computation time: ", end_time - start_time, " seconds"
    print *, "=========================================================================="
    print *, ""
    print *, "Program completed successfully."
    print *, ""

end program main
