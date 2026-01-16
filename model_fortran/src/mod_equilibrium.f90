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
        real(dp) :: step

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
        ! Intangible capital grid
        !-----------------------------------------------------------------------
        do i = 1, nS
            grid_S(i) = S_min + (S_max - S_min) * real(i-1, dp) / real(nS-1, dp)
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
        ! Tangible investment (allow negative for disinvestment, but bounded)
        do i = 1, nIK
            step = real(i-1, dp) / real(nIK-1, dp)
            grid_IK(i) = -delta_K * K_max * 0.50_dp + step * (K_max * 0.20_dp)
        end do

        ! R&D labor (0 to Hbar)
        do i = 1, nHR
            grid_HR(i) = Hbar * real(i-1, dp) / real(nHR-1, dp)
        end do

        ! New debt
        do i = 1, nDprime
            grid_Dprime(i) = D_min + (D_max - D_min) * real(i-1, dp) / real(nDprime-1, dp)
        end do

        print *, "  Grids initialized."
        print '(A,I4,A,F8.3,A,F8.3)', "    z grid: ", nz, " points, range [", &
            grid_z(1), ", ", grid_z(nz), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    K grid: ", nK, " points, range [", &
            grid_K(1), ", ", grid_K(nK), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    S grid: ", nS, " points, range [", &
            grid_S(1), ", ", grid_S(nS), "]"
        print '(A,I4,A,F8.3,A,F8.3)', "    D grid: ", nD, " points, range [", &
            grid_D(1), ", ", grid_D(nD), "]"

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

        print *, ""
        print *, "======================================"
        print *, "SOLVING FOR EQUILIBRIUM"
        print *, "======================================"

        ! Initial wage guess (wH higher due to skill scarcity with Hbar = 0.15)
        wL = 0.075_dp
        wH = 0.52_dp

        do iter_outer = 1, maxiter_eq

            wL_old = wL
            wH_old = wH

            print *, ""
            print '(A,I3)', "Equilibrium iteration: ", iter_outer
            print '(A,F10.6,A,F10.6)', "  Wages: wL = ", wL, ", wH = ", wH

            ! Solve firm problem given wages
            call solve_firm_problem()

            ! Compute stationary distribution
            call compute_stationary_distribution()

            ! Compute aggregates
            call compute_aggregates()

            ! Check labor market clearing
            excess_L = agg_L - Lbar
            excess_H = agg_H - Hbar

            metric_eq_L = abs(excess_L)
            metric_eq_H = abs(excess_H)

            print *, ""
            print *, "  Market clearing:"
            print '(A,F10.6,A,F10.6,A,F10.6)', "    Unskilled: demand = ", agg_L, &
                ", supply = ", Lbar, ", excess = ", excess_L
            print '(A,F10.6,A,F10.6,A,F10.6)', "    Skilled:   demand = ", agg_H, &
                ", supply = ", Hbar, ", excess = ", excess_H

            ! Check convergence
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

        print *, ""
        print *, "Saving results to output files..."

        ! Save aggregates
        open(newunit=unit_agg, file='../output/aggregates.txt', status='replace')
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
        open(newunit=unit_pol, file='../output/distribution.txt', status='replace')
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
        print *, "  ../output/aggregates.txt"
        print *, "  ../output/distribution.txt"

    end subroutine save_results

end module mod_equilibrium
