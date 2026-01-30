!===================================================================================
! MODULE: mod_equilibrium
!
! DESCRIPTION:
!   Solves for general equilibrium: wages that clear labor markets.
!   Uses bisection/fixed-point iteration.
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_equilibrium
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_firm_problem
    use mod_distribution
    implicit none

contains

    !===================================================================================
    ! SUBROUTINE: initialize_grids
    !
    ! DESCRIPTION:
    !   Creates grids for state and choice variables.
    !===================================================================================
    subroutine initialize_grids()
        implicit none
        integer :: i

        print *, ""
        print *, "======================================"
        print *, "INITIALIZING GRIDS"
        print *, "======================================"

        !-----------------------------------------------------------------------
        ! Productivity grid (Tauchen method for AR(1))
        !-----------------------------------------------------------------------
        call tauchen_discretize()

        !-----------------------------------------------------------------------
        ! Tangible capital grid (log-spaced for better resolution near zero)
        !-----------------------------------------------------------------------
        do i = 1, nK
            grid_K(i) = K_min * exp(real(i-1, dp) / real(nK-1, dp) * log(K_max / K_min))
        end do

        !-----------------------------------------------------------------------
        ! Intangible capital grid (FIX #1: log-spaced like K grid)
        ! With CES complements (rho_Q < 0), marginal products are very high
        ! near S_min. Log-spacing provides better resolution where it matters.
        !-----------------------------------------------------------------------
        do i = 1, nS
            grid_S(i) = S_min * exp(real(i-1, dp) / real(nS-1, dp) * log(S_max / S_min))
        end do

        !-----------------------------------------------------------------------
        ! Debt grid
        !-----------------------------------------------------------------------
        do i = 1, nD
            grid_D(i) = D_min + (D_max - D_min) * real(i-1, dp) / real(nD-1, dp)
        end do

        !-----------------------------------------------------------------------
        ! Choice grids
        !-----------------------------------------------------------------------
        ! Tangible investment grid: three-segment design
        !   Segment 1: disinvestment [-IK_neg_max, 0)    -- 1/5 of points
        !   Segment 2: small investment [0, IK_thresh]    -- 2/5 of points (critical for entrants)
        !   Segment 3: large investment (IK_thresh, IK_pos_max] -- 2/5 of points
        ! This ensures resolution at all firm sizes: small firms invest 0.001-0.05,
        ! large firms invest 0.20-0.80.
        block
            real(dp), parameter :: IK_neg_max = 0.20_dp   ! Max disinvestment
            real(dp), parameter :: IK_thresh  = 0.05_dp   ! Small/large boundary
            real(dp), parameter :: IK_pos_max = 0.80_dp   ! Max investment
            integer :: n_neg, n_small, n_large

            n_neg   = max(nIK / 5, 2)                     ! ~6 points for disinvestment
            n_small = max((nIK * 2) / 5, 3)               ! ~12 points for small investment
            n_large = nIK - n_neg - n_small                ! ~12 points for large investment

            ! Segment 1: disinvestment (linearly spaced)
            do i = 1, n_neg
                grid_IK(i) = -IK_neg_max + IK_neg_max * real(i-1, dp) / real(n_neg, dp)
            end do

            ! Segment 2: small investment (linearly spaced for fine resolution)
            do i = 1, n_small
                grid_IK(n_neg + i) = IK_thresh * real(i-1, dp) / real(n_small - 1, dp)
            end do

            ! Segment 3: large investment (linearly spaced)
            do i = 1, n_large
                grid_IK(n_neg + n_small + i) = IK_thresh &
                    + (IK_pos_max - IK_thresh) * real(i, dp) / real(n_large, dp)
            end do
        end block

        ! FIX #7: R&D labor grid with extra fine resolution at low values
        ! With xi < 1, the R&D production function has high curvature near zero,
        ! so we need many points there. We use a two-segment approach:
        !   - First half of grid points: cover [0, HR_threshold] with log spacing
        !   - Second half: cover [HR_threshold, HR_max] with log spacing
        ! This ensures good resolution both at very low HR and at moderate HR.
        block
            real(dp), parameter :: HR_min_grid = 1.0e-5_dp  ! Smallest positive HR (finer than before)
            real(dp), parameter :: HR_threshold = 0.02_dp   ! Transition point
            real(dp) :: log_ratio
            integer :: n_low, n_high

            grid_HR(1) = 0.0_dp  ! Keep zero as an option

            if (nHR > 4) then
                ! Split grid: more points in low range
                n_low = (nHR * 2) / 3    ! 2/3 of points in low range
                n_high = nHR - n_low

                ! Low range: log spacing from HR_min_grid to HR_threshold
                log_ratio = log(HR_threshold / HR_min_grid)
                do i = 2, n_low
                    grid_HR(i) = HR_min_grid * exp(real(i-2, dp) / real(n_low-2, dp) * log_ratio)
                end do

                ! High range: log spacing from HR_threshold to HR_max
                log_ratio = log(HR_max / HR_threshold)
                do i = n_low + 1, nHR
                    grid_HR(i) = HR_threshold * exp(real(i-n_low-1, dp) / real(n_high-1, dp) * log_ratio)
                end do
            else
                ! Fallback for very small nHR
                log_ratio = log(HR_max / HR_min_grid)
                do i = 2, nHR
                    grid_HR(i) = HR_min_grid * exp(real(i-2, dp) / real(nHR-2, dp) * log_ratio)
                end do
            end if
        end block

        ! New debt
        do i = 1, nDprime
            grid_Dprime(i) = D_min + (D_max - D_min) * real(i-1, dp) / real(nDprime-1, dp)
        end do

        print *, "  Grids initialized."
        print '(A,I4,A,F8.3,A,F8.3)', "    z grid:  ", nz, " points, range [", &
            grid_z(1), ", ", grid_z(nz), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    K grid:  ", nK, " points, range [", &
            grid_K(1), ", ", grid_K(nK), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    S grid:  ", nS, " points, range [", &
            grid_S(1), ", ", grid_S(nS), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    D grid:  ", nD, " points, range [", &
            grid_D(1), ", ", grid_D(nD), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    IK grid: ", nIK, " points, range [", &
            grid_IK(1), ", ", grid_IK(nIK), "]"
        print *, ""
        print *, "  HR grid (log-spaced, max decoupled from Hbar):"
        print '(A,F10.6,A,F10.6)', "    Range: [", grid_HR(1), ", ", grid_HR(nHR), "]"
        print '(A,F10.6,A,F10.6,A,F10.6,A,F10.6,A,F10.6)', "    First 5: ", &
            grid_HR(1), ", ", grid_HR(2), ", ", grid_HR(3), ", ", grid_HR(4), ", ", grid_HR(5)
        print '(A,F10.6,A,F10.6)', "    Hbar (aggregate supply): ", Hbar, ", HR_max (per firm): ", HR_max

    end subroutine initialize_grids

    !===================================================================================
    ! SUBROUTINE: tauchen_discretize
    !
    ! DESCRIPTION:
    !   Discretizes AR(1) process using Tauchen (1986) method.
    !   log(z') = ρz * log(z) + σz * ε
    !===================================================================================
    subroutine tauchen_discretize()
        implicit none
        integer :: i, j
        real(dp) :: log_z_min, log_z_max, step
        real(dp) :: log_z_j, log_z_jp1, log_z_mean
        real(dp) :: sigma_z_uncond, cdf_upper, cdf_lower
        real(dp) :: stat_dist_new(nz), metric
        integer :: iter

        ! Unconditional std dev
        sigma_z_uncond = sigma_z / sqrt(1.0_dp - rho_z**2)

        ! Grid bounds
        log_z_min = -m_z * sigma_z_uncond
        log_z_max =  m_z * sigma_z_uncond
        step = (log_z_max - log_z_min) / real(nz - 1, dp)

        ! Create grid
        do i = 1, nz
            grid_z(i) = exp(log_z_min + step * real(i-1, dp))
        end do

        ! Transition matrix
        do i = 1, nz
            log_z_mean = rho_z * log(grid_z(i))

            do j = 1, nz
                if (j == 1) then
                    log_z_jp1 = log(grid_z(j)) + step / 2.0_dp
                    cdf_upper = normal_cdf((log_z_jp1 - log_z_mean) / sigma_z)
                    Pi_z(i, j) = cdf_upper

                else if (j == nz) then
                    log_z_j = log(grid_z(j)) - step / 2.0_dp
                    cdf_lower = normal_cdf((log_z_j - log_z_mean) / sigma_z)
                    Pi_z(i, j) = 1.0_dp - cdf_lower

                else
                    log_z_j = log(grid_z(j)) - step / 2.0_dp
                    log_z_jp1 = log(grid_z(j)) + step / 2.0_dp
                    cdf_upper = normal_cdf((log_z_jp1 - log_z_mean) / sigma_z)
                    cdf_lower = normal_cdf((log_z_j - log_z_mean) / sigma_z)
                    Pi_z(i, j) = cdf_upper - cdf_lower
                end if
            end do
        end do

        ! Compute stationary distribution (power iteration)
        stat_dist_z = 1.0_dp / real(nz, dp)  ! Uniform initial guess

        do iter = 1, 10000
            stat_dist_new = matmul(stat_dist_z, Pi_z)
            metric = maxval(abs(stat_dist_new - stat_dist_z))
            stat_dist_z = stat_dist_new
            if (metric < 1.0e-10_dp) exit
        end do

        ! Normalize
        stat_dist_z = stat_dist_z / sum(stat_dist_z)

    end subroutine tauchen_discretize

    !===================================================================================
    ! FUNCTION: normal_cdf
    !
    ! DESCRIPTION:
    !   Cumulative distribution function of standard normal using error function.
    !===================================================================================
    function normal_cdf(x) result(cdf)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: cdf

        cdf = 0.5_dp * (1.0_dp + erf(x / sqrt(2.0_dp)))

    end function normal_cdf

    !===================================================================================
    ! SUBROUTINE: solve_equilibrium
    !
    ! DESCRIPTION:
    !   Outer loop: iterates on wages until labor markets clear.
    !===================================================================================
    subroutine solve_equilibrium()
        implicit none
        integer :: iter_outer
        real(dp) :: wL_old, wH_old, excess_L, excess_H
        real(dp) :: update_wL, update_wH
        real(dp) :: time_start, time_current, time_elapsed_min

        print *, ""
        print *, "======================================"
        print *, "SOLVING FOR EQUILIBRIUM"
        print *, "======================================"

        ! Report counterfactual mode status
        if (remove_collateral_constraint) then
            print *, ""
            print *, "  *** COUNTERFACTUAL MODE: λ = 0 FOR ALL FIRMS ***"
            print *, "  Collateral constraint removed (unlimited borrowing)."
            print *, "  This is the 'first-best' / 'no frictions' benchmark."
            print *, ""
        end if

        ! Start timing for equilibrium iterations
        call cpu_time(time_start)

        ! Initial wage guess (wH higher due to skill scarcity with Hbar = 0.15)
        wL = 0.655180_dp
        wH = 0.871480_dp

        do iter_outer = 1, maxiter_eq

            wL_old = wL
            wH_old = wH

            print *, ""
            print '(A,I3)', "Equilibrium iteration: ", iter_outer
            print '(A,F10.6,A,F10.6)', "  Wages: wL = ", wL, ", wH = ", wH

            ! Solve firm problem given wages
            call solve_firm_problem()

            ! Compute stationary distribution (using optimized sparse method)
            call compute_stationary_distribution_sparse()

            ! Compute aggregates (OpenMP parallelized)
            call compute_aggregates()

            ! Check labor market clearing (relative excess demand)
            excess_L = agg_L - Lbar
            excess_H = agg_H - Hbar

            metric_eq_L = abs(excess_L) / Lbar
            metric_eq_H = abs(excess_H) / Hbar

            print *, ""
            print *, "  Market clearing:"
            print '(A,F10.6,A,F10.6,A,F10.6)', "    Unskilled: demand = ", agg_L, &
                ", supply = ", Lbar, ", excess = ", excess_L
            print '(A,F10.6,A,F10.6,A,F10.6)', "    Skilled:   demand = ", agg_H, &
                ", supply = ", Hbar, ", excess = ", excess_H
            print '(A,F10.6,A,F10.6)', "    Relative excess: L = ", metric_eq_L, ", H = ", metric_eq_H

            ! Display cumulative elapsed time
            call cpu_time(time_current)
            time_elapsed_min = (time_current - time_start) / 60.0_dp
            print *, ""
            print '(A,F8.2,A)', "  >>> Cumulative time elapsed: ", time_elapsed_min, " minutes"

            ! Check convergence (relative excess demand < tol_eq)
            if (metric_eq_L < tol_eq .and. metric_eq_H < tol_eq) then
                print *, ""
                print *, "======================================"
                print *, "EQUILIBRIUM FOUND!"
                print *, "======================================"
                call print_aggregates()
                exit
            end if

            ! Update wages (simple fixed-point with dampening)
            update_wL = wL * (1.0_dp + update_wage * excess_L / Lbar)
            update_wH = wH * (1.0_dp + update_wage * excess_H / Hbar)

            ! Bounds on wages (wide bounds to allow equilibrium to be found)
            wL = max(0.01_dp, min(100.0_dp, update_wL))
            wH = max(0.01_dp, min(100.0_dp, update_wH))

            if (iter_outer == maxiter_eq) then
                print *, ""
                print *, "WARNING: Equilibrium did not converge!"
                print '(A,F10.6)', "  L excess: ", excess_L
                print '(A,F10.6)', "  H excess: ", excess_H
                call print_aggregates()
            end if

        end do

    end subroutine solve_equilibrium

    !===================================================================================
    ! SUBROUTINE: save_results
    !
    ! DESCRIPTION:
    !   Saves key results to output files.
    !===================================================================================
    subroutine save_results()
        implicit none
        integer :: unit_agg, unit_pol
        integer :: iz, iK, iS, iD
        integer :: ios
        logical :: dir_exists

        print *, ""
        print *, "Saving results to output files..."

        ! Create output directory if it doesn't exist
        ! First try to open a test file to check if directory exists
        inquire(file='output/.', exist=dir_exists)
        if (.not. dir_exists) then
            print *, "  Creating output directory..."
            call execute_command_line('mkdir output', wait=.true.)
        end if

        ! Save aggregates
        open(newunit=unit_agg, file='output/aggregates.txt', status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "  ERROR: Could not open output/aggregates.txt"
            print *, "  Trying to create output directory..."
            call execute_command_line('mkdir output', wait=.true.)
            open(newunit=unit_agg, file='output/aggregates.txt', status='replace', iostat=ios)
            if (ios /= 0) then
                print *, "  FATAL: Could not create output file. Skipping save."
                return
            end if
        end if
        write(unit_agg, '(A,F12.6)') 'Output_Y                 ', agg_Y
        write(unit_agg, '(A,F12.6)') 'Consumption_C            ', agg_C
        write(unit_agg, '(A,F12.6)') 'Tangible_capital_K       ', agg_K
        write(unit_agg, '(A,F12.6)') 'Intangible_capital_S     ', agg_S
        write(unit_agg, '(A,F12.6)') 'Total_debt_D             ', agg_D
        write(unit_agg, '(A,F12.6)') 'Unskilled_labor_L        ', agg_L
        write(unit_agg, '(A,F12.6)') 'Skilled_labor_prod_HP    ', agg_HP
        write(unit_agg, '(A,F12.6)') 'Skilled_labor_RD_HR      ', agg_HR
        write(unit_agg, '(A,F12.6)') 'Unskilled_wage_wL        ', wL
        write(unit_agg, '(A,F12.6)') 'Skilled_wage_wH          ', wH
        write(unit_agg, '(A,F12.6)') 'Frac_constrained         ', frac_constrained
        write(unit_agg, '(A,F12.6)') 'Avg_intang_intensity     ', avg_intang_intensity
        write(unit_agg, '(A,F12.6)') 'Avg_leverage             ', avg_leverage
        close(unit_agg)

        ! Save distribution (sparse format: only non-zero entries)
        open(newunit=unit_pol, file='output/distribution.txt', status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "  ERROR: Could not open output/distribution.txt"
            return
        end if
        write(unit_pol, '(A)') 'iz iK iS iD z K S D mass'
        do iz = 1, nz
            do iK = 1, nK
                do iS = 1, nS
                    do iD = 1, nD
                        if (dist(iz,iK,iS,iD) > 1.0e-8_dp) then
                            write(unit_pol, '(4I6, 5F14.6)') iz, iK, iS, iD, &
                                grid_z(iz), grid_K(iK), grid_S(iS), grid_D(iD), &
                                dist(iz,iK,iS,iD)
                        end if
                    end do
                end do
            end do
        end do
        close(unit_pol)

        print *, "Results saved successfully."
        print *, "  output/aggregates.txt"
        print *, "  output/distribution.txt"

        ! Also save diagnostics to file
        call save_diagnostics_to_file()

        ! FIX #9: Save policy functions for debugging
        call save_policy_functions()

    end subroutine save_results

    !===================================================================================
    ! SUBROUTINE: save_diagnostics_to_file
    !
    ! DESCRIPTION:
    !   Saves detailed diagnostics to a separate file for analysis.
    !===================================================================================
    subroutine save_diagnostics_to_file()
        implicit none
        integer :: unit_diag, ios
        integer :: iz, iK, iS, iD, i
        real(dp) :: V_S_estimate

        open(newunit=unit_diag, file='output/diagnostics.txt', status='replace', iostat=ios)
        if (ios /= 0) then
            print *, "  WARNING: Could not open output/diagnostics.txt"
            return
        end if

        write(unit_diag, '(A)') '========================================'
        write(unit_diag, '(A)') 'MODEL DIAGNOSTICS'
        write(unit_diag, '(A)') '========================================'
        write(unit_diag, *)

        ! Wages
        write(unit_diag, '(A)') 'EQUILIBRIUM WAGES:'
        write(unit_diag, '(A,F12.6)') '  wL (unskilled): ', wL
        write(unit_diag, '(A,F12.6)') '  wH (skilled):   ', wH
        write(unit_diag, '(A,F12.6)') '  Skill premium:  ', wH / wL
        write(unit_diag, *)

        ! Grid info
        write(unit_diag, '(A)') 'GRID INFORMATION:'
        write(unit_diag, '(A,F12.6,A,F12.6)') '  K grid: [', grid_K(1), ', ', grid_K(nK), ']'
        write(unit_diag, '(A,F12.6,A,F12.6)') '  S grid: [', grid_S(1), ', ', grid_S(nS), ']'
        write(unit_diag, '(A,F12.6,A,F12.6)') '  D grid: [', grid_D(1), ', ', grid_D(nD), ']'
        write(unit_diag, '(A,F12.6,A,F12.6)') '  HR grid: [', grid_HR(1), ', ', grid_HR(nHR), ']'
        write(unit_diag, *)

        ! Value function profile in S
        ! FIX #4: Use E-format for V to handle large/small values without overflow
        write(unit_diag, '(A)') 'VALUE FUNCTION PROFILE (z=median, K=median, D=0):'
        write(unit_diag, '(A)') '  iS        S              V'
        do iS = 1, nS
            write(unit_diag, '(I5, F12.6, ES16.6)') iS, grid_S(iS), V((nz+1)/2, (nK+1)/2, iS, 1)
        end do
        write(unit_diag, *)

        ! Value function profile in K
        write(unit_diag, '(A)') 'VALUE FUNCTION PROFILE (z=median, S=median, D=0):'
        write(unit_diag, '(A)') '  iK        K              V'
        do iK = 1, nK
            write(unit_diag, '(I5, F12.6, ES16.6)') iK, grid_K(iK), V((nz+1)/2, iK, (nS+1)/2, 1)
        end do
        write(unit_diag, *)

        ! Policy functions at median state
        write(unit_diag, '(A)') 'POLICY FUNCTIONS AT MEDIAN STATE (z=median, K=median, S=median, D=0):'
        write(unit_diag, '(A,F12.6)') '  L:         ', pol_L((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  HP:        ', pol_HP((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  HR:        ', pol_HR((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  I^K:       ', pol_IK((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  D'':        ', pol_Dprime((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  K'':        ', pol_Kprime((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  S'':        ', pol_Sprime((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, '(A,F12.6)') '  Y:         ', pol_Y((nz+1)/2, (nK+1)/2, (nS+1)/2, 1)
        write(unit_diag, *)

        ! Policy at entry state (K_min, S_min, D=0)
        write(unit_diag, '(A)') 'POLICY FUNCTIONS AT ENTRY STATE (z=median, K=K_min, S=S_min, D=0):'
        write(unit_diag, '(A,F12.6)') '  L:         ', pol_L((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  HP:        ', pol_HP((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  HR:        ', pol_HR((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  I^K:       ', pol_IK((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  D'':        ', pol_Dprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  K'':        ', pol_Kprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  S'':        ', pol_Sprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Y:         ', pol_Y((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Profit:    ', pol_Y((nz+1)/2, 1, 1, 1) - wL*pol_L((nz+1)/2, 1, 1, 1) &
                                                      - wH*pol_HP((nz+1)/2, 1, 1, 1)
        write(unit_diag, *)

        ! R&D profitability analysis
        ! FIX #4: Use E-format for values that may be large
        write(unit_diag, '(A)') 'R&D PROFITABILITY ANALYSIS:'
        V_S_estimate = (V((nz+1)/2, (nK+1)/2, 5, 1) - V((nz+1)/2, (nK+1)/2, 1, 1)) / (grid_S(5) - grid_S(1))
        write(unit_diag, '(A,ES16.6)') '  Estimated dV/dS:                 ', V_S_estimate
        write(unit_diag, '(A,F12.6)') '  wH (cost per HR):                ', wH
        write(unit_diag, '(A,F12.6)') '  Gamma_RD:                        ', Gamma_RD
        write(unit_diag, '(A,F12.6)') '  xi:                              ', xi
        write(unit_diag, '(A,F12.6)') '  beta*(1-zeta):                   ', beta * (1.0_dp - zeta)
        write(unit_diag, '(A,F12.6)') '  At HR=0.01, dS = Gamma*HR^xi:    ', Gamma_RD * 0.01_dp**xi
        write(unit_diag, '(A,ES16.6)') '  Marginal benefit at HR=0.01:     ', &
              beta * (1.0_dp - zeta) * V_S_estimate * Gamma_RD * xi * 0.01_dp**(xi - 1.0_dp)
        write(unit_diag, '(A,F12.6)') '  Marginal cost (wH):              ', wH
        write(unit_diag, '(A,ES16.6)') '  Net marginal benefit:            ', &
              beta * (1.0_dp - zeta) * V_S_estimate * Gamma_RD * xi * 0.01_dp**(xi - 1.0_dp) - wH
        write(unit_diag, *)

        ! Budget constraint analysis at entry
        write(unit_diag, '(A)') 'BUDGET CONSTRAINT AT ENTRY STATE:'
        write(unit_diag, '(A,F12.6)') '  Gross profit (Y - wL*L - wH*HP): ', &
              pol_Y((nz+1)/2, 1, 1, 1) - wL*pol_L((nz+1)/2, 1, 1, 1) - wH*pol_HP((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Debt repayment (R*D):            ', R * grid_D(1)
        write(unit_diag, '(A,F12.6)') '  New borrowing (D''):              ', pol_Dprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Resources available:             ', &
              pol_Y((nz+1)/2, 1, 1, 1) - wL*pol_L((nz+1)/2, 1, 1, 1) - wH*pol_HP((nz+1)/2, 1, 1, 1) &
              - R * grid_D(1) + pol_Dprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Tangible investment (I^K):       ', pol_IK((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  R&D cost (wH*HR):                ', wH * pol_HR((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Total expenses:                  ', &
              pol_IK((nz+1)/2, 1, 1, 1) + wH * pol_HR((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,F12.6)') '  Dividends (resources - expenses):', &
              pol_Y((nz+1)/2, 1, 1, 1) - wL*pol_L((nz+1)/2, 1, 1, 1) - wH*pol_HP((nz+1)/2, 1, 1, 1) &
              - R * grid_D(1) + pol_Dprime((nz+1)/2, 1, 1, 1) &
              - pol_IK((nz+1)/2, 1, 1, 1) - wH * pol_HR((nz+1)/2, 1, 1, 1)
        write(unit_diag, *)

        ! Collateral constraint
        write(unit_diag, '(A)') 'COLLATERAL CONSTRAINT AT ENTRY:'
        write(unit_diag, '(A,F12.6)') '  alpha_K * K_min:                 ', alpha_K * grid_K(1)
        write(unit_diag, '(A,F12.6)') '  alpha_S * S_min:                 ', alpha_S * grid_S(1)
        write(unit_diag, '(A,F12.6)') '  Max borrowing:                   ', alpha_K * grid_K(1) + alpha_S * grid_S(1)
        write(unit_diag, '(A,F12.6)') '  Actual D'':                       ', pol_Dprime((nz+1)/2, 1, 1, 1)
        write(unit_diag, '(A,L)') '  Constraint binding?              ', &
              pol_Dprime((nz+1)/2, 1, 1, 1) >= (alpha_K * grid_K(1) + alpha_S * grid_S(1)) - 0.01_dp

        close(unit_diag)
        print *, "  output/diagnostics.txt"

    end subroutine save_diagnostics_to_file

    !===================================================================================
    ! SUBROUTINE: save_policy_functions  (FIX #9)
    !
    ! DESCRIPTION:
    !   Saves policy functions to CSV files for debugging and plotting.
    !   Creates separate files for different cross-sections:
    !     - policies_by_K.csv: policies as function of K (at median z, S, D=0)
    !     - policies_by_S.csv: policies as function of S (at median z, K, D=0)
    !     - policies_by_z.csv: policies as function of z (at median K, S, D=0)
    !===================================================================================
    subroutine save_policy_functions()
        implicit none
        integer :: unit_pol, ios
        integer :: iz, iK, iS, iD
        integer :: iz_med, iK_med, iS_med

        print *, ""
        print *, "Saving policy functions..."

        ! Median indices
        iz_med = (nz + 1) / 2
        iK_med = (nK + 1) / 2
        iS_med = (nS + 1) / 2
        iD = 1  ! D = 0

        ! Policy functions by K (at median z, S, D=0)
        open(newunit=unit_pol, file='output/policies_by_K.csv', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_pol, '(A)') 'K,V,L,HP,HR,IK,Kprime,Sprime,Dprime,Y,constrained'
            do iK = 1, nK
                write(unit_pol, '(10(ES14.6,","),I1)') &
                    grid_K(iK), V(iz_med, iK, iS_med, iD), &
                    pol_L(iz_med, iK, iS_med, iD), pol_HP(iz_med, iK, iS_med, iD), &
                    pol_HR(iz_med, iK, iS_med, iD), pol_IK(iz_med, iK, iS_med, iD), &
                    pol_Kprime(iz_med, iK, iS_med, iD), pol_Sprime(iz_med, iK, iS_med, iD), &
                    pol_Dprime(iz_med, iK, iS_med, iD), pol_Y(iz_med, iK, iS_med, iD), &
                    merge(1, 0, pol_constr(iz_med, iK, iS_med, iD))
            end do
            close(unit_pol)
            print *, "  output/policies_by_K.csv"
        end if

        ! Policy functions by S (at median z, K, D=0)
        open(newunit=unit_pol, file='output/policies_by_S.csv', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_pol, '(A)') 'S,V,L,HP,HR,IK,Kprime,Sprime,Dprime,Y,constrained'
            do iS = 1, nS
                write(unit_pol, '(10(ES14.6,","),I1)') &
                    grid_S(iS), V(iz_med, iK_med, iS, iD), &
                    pol_L(iz_med, iK_med, iS, iD), pol_HP(iz_med, iK_med, iS, iD), &
                    pol_HR(iz_med, iK_med, iS, iD), pol_IK(iz_med, iK_med, iS, iD), &
                    pol_Kprime(iz_med, iK_med, iS, iD), pol_Sprime(iz_med, iK_med, iS, iD), &
                    pol_Dprime(iz_med, iK_med, iS, iD), pol_Y(iz_med, iK_med, iS, iD), &
                    merge(1, 0, pol_constr(iz_med, iK_med, iS, iD))
            end do
            close(unit_pol)
            print *, "  output/policies_by_S.csv"
        end if

        ! Policy functions by z (at median K, S, D=0)
        open(newunit=unit_pol, file='output/policies_by_z.csv', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_pol, '(A)') 'z,V,L,HP,HR,IK,Kprime,Sprime,Dprime,Y,constrained'
            do iz = 1, nz
                write(unit_pol, '(10(ES14.6,","),I1)') &
                    grid_z(iz), V(iz, iK_med, iS_med, iD), &
                    pol_L(iz, iK_med, iS_med, iD), pol_HP(iz, iK_med, iS_med, iD), &
                    pol_HR(iz, iK_med, iS_med, iD), pol_IK(iz, iK_med, iS_med, iD), &
                    pol_Kprime(iz, iK_med, iS_med, iD), pol_Sprime(iz, iK_med, iS_med, iD), &
                    pol_Dprime(iz, iK_med, iS_med, iD), pol_Y(iz, iK_med, iS_med, iD), &
                    merge(1, 0, pol_constr(iz, iK_med, iS_med, iD))
            end do
            close(unit_pol)
            print *, "  output/policies_by_z.csv"
        end if

        ! FIX #10: Full policy grid at D=0 for 3D plotting
        ! Format: z, K, S, V, HR, IK, constrained
        open(newunit=unit_pol, file='output/policies_3D.csv', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_pol, '(A)') 'iz,iK,iS,z,K,S,V,HR,IK,Sprime,constrained'
            do iz = 1, nz
                do iK = 1, nK
                    do iS = 1, nS
                        write(unit_pol, '(3(I4,","),7(ES14.6,","),I1)') &
                            iz, iK, iS, grid_z(iz), grid_K(iK), grid_S(iS), &
                            V(iz, iK, iS, 1), pol_HR(iz, iK, iS, 1), &
                            pol_IK(iz, iK, iS, 1), pol_Sprime(iz, iK, iS, 1), &
                            merge(1, 0, pol_constr(iz, iK, iS, 1))
                    end do
                end do
            end do
            close(unit_pol)
            print *, "  output/policies_3D.csv (for 3D plotting)"
        end if

    end subroutine save_policy_functions

    !===================================================================================
    ! SUBROUTINE: compute_euler_errors  (FIX #8)
    !
    ! DESCRIPTION:
    !   Computes Euler equation errors to assess solution accuracy.
    !   For interior solutions, the FOC for tangible investment is:
    !     1 + phi_K * (I^K/K) = beta*(1-zeta)*E[dV/dK']
    !
    !   The Euler error is: |LHS - RHS| / LHS
    !   Reports mean, max, and distribution of errors across states.
    !===================================================================================
    subroutine compute_euler_errors()
        use mod_interpolation
        implicit none
        integer :: iz, iK, iS, iD, n_interior
        real(dp) :: K_val, S_val, Kprime, Sprime, Dprime, IK_val
        real(dp) :: LHS, RHS, euler_error
        real(dp) :: sum_error, max_error, mean_error
        real(dp) :: V_Kp_plus, V_Kp_minus, dV_dKp
        real(dp) :: dK_step, EV_plus, EV_minus
        integer :: unit_euler, ios

        print *, ""
        print *, "Computing Euler equation errors..."

        sum_error = 0.0_dp
        max_error = 0.0_dp
        n_interior = 0

        ! Small step for numerical derivative
        dK_step = 0.01_dp * (K_max - K_min)

        do iz = 1, nz
            do iK = 2, nK-1  ! Interior points only
                do iS = 2, nS-1
                    do iD = 1, nD

                        ! Skip if at boundary or very low mass
                        if (dist(iz, iK, iS, iD) < 1.0e-8_dp) cycle

                        K_val = grid_K(iK)
                        S_val = grid_S(iS)
                        Kprime = pol_Kprime(iz, iK, iS, iD)
                        Sprime = pol_Sprime(iz, iK, iS, iD)
                        Dprime = pol_Dprime(iz, iK, iS, iD)
                        IK_val = pol_IK(iz, iK, iS, iD)

                        ! Skip if at investment bounds (corner solution)
                        if (abs(IK_val - grid_IK(1)) < epsilon .or. &
                            abs(IK_val - grid_IK(nIK)) < epsilon) cycle

                        ! Skip if constrained (Euler doesn't hold with equality)
                        if (pol_constr(iz, iK, iS, iD)) cycle

                        n_interior = n_interior + 1

                        ! LHS: 1 + adjustment cost derivative
                        if (K_val > epsilon) then
                            LHS = 1.0_dp + phi_K * (IK_val / K_val)
                        else
                            LHS = 1.0_dp
                        end if

                        ! RHS: beta*(1-zeta)*E[dV/dK']
                        ! Approximate dV/dK' numerically
                        EV_plus = expect_V(iz, min(Kprime + dK_step, K_max), Sprime, Dprime)
                        EV_minus = expect_V(iz, max(Kprime - dK_step, K_min), Sprime, Dprime)
                        dV_dKp = (EV_plus - EV_minus) / (2.0_dp * dK_step)

                        RHS = beta * (1.0_dp - zeta) * dV_dKp

                        ! Euler error (relative)
                        if (abs(LHS) > epsilon) then
                            euler_error = abs(LHS - RHS) / abs(LHS)
                        else
                            euler_error = abs(LHS - RHS)
                        end if

                        sum_error = sum_error + euler_error
                        max_error = max(max_error, euler_error)

                    end do
                end do
            end do
        end do

        if (n_interior > 0) then
            mean_error = sum_error / real(n_interior, dp)
        else
            mean_error = 0.0_dp
        end if

        print '(A,I8)', "  Interior points checked: ", n_interior
        print '(A,ES12.4)', "  Mean Euler error:        ", mean_error
        print '(A,ES12.4)', "  Max Euler error:         ", max_error

        ! Save to file
        open(newunit=unit_euler, file='output/euler_errors.txt', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_euler, '(A)') 'EULER EQUATION ERROR DIAGNOSTICS'
            write(unit_euler, '(A)') '================================'
            write(unit_euler, '(A,I8)') 'Interior points checked: ', n_interior
            write(unit_euler, '(A,ES16.6)') 'Mean Euler error:        ', mean_error
            write(unit_euler, '(A,ES16.6)') 'Max Euler error:         ', max_error
            write(unit_euler, *)
            write(unit_euler, '(A)') 'Note: Euler error = |1 + phi*(I/K) - beta*(1-zeta)*E[dV/dK'']| / LHS'
            write(unit_euler, '(A)') 'Good accuracy: mean < 0.01, max < 0.05'
            close(unit_euler)
            print *, "  output/euler_errors.txt"
        end if

    end subroutine compute_euler_errors

end module mod_equilibrium