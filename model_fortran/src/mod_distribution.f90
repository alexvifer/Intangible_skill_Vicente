!===================================================================================
! MODULE: mod_distribution
!
! DESCRIPTION:
!   Computes the stationary distribution of firms over state space (z,K,S,D).
!   Uses policy functions from firm problem and accounts for entry/exit.
!
!   COMPUTATIONAL OPTIMIZATIONS:
!     1. OpenMP parallelization for aggregates computation
!     2. Sparse distribution tracking (only active states)
!     3. Early termination for converged states
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_distribution
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_interpolation
    !$ use omp_lib
    implicit none

contains

    !===================================================================================
    ! SUBROUTINE: compute_stationary_distribution
    !
    ! DESCRIPTION:
    !   Iterates on distribution until convergence.
    !   Accounts for:
    !     - Policy functions determining transitions
    !     - Productivity shocks (Markov)
    !     - Exit (probability ζ)
    !     - Entry (mass replacing exiters, starting at K=S=D=0)
    !===================================================================================
    subroutine compute_stationary_distribution()
        implicit none
        integer :: iter, iz, iK, iS, iD, izp, iKp, iSp, iDp
        real(dp) :: Kprime, Sprime, Dprime, mass_exit, mass_entry
        real(dp) :: metric, wKp, wSp, wDp
        real(dp) :: dist_contrib

        print *, ""
        print *, "======================================"
        print *, "COMPUTING STATIONARY DISTRIBUTION"
        print *, "======================================"

        ! Initialize distribution
        ! Start all mass at entry point: K=S=D=0, z drawn from stationary dist
        dist = 0.0_dp
        do iz = 1, nz
            dist(iz, 1, 1, 1) = stat_dist_z(iz)  ! Entry at (z, K_min, S_min, D_min)
        end do

        ! Normalize
        dist = dist / sum(dist)

        ! Iterate on distribution
        do iter = 1, maxiter_dist

            dist_new = 0.0_dp

            ! Entry mass (replaces exiters)
            mass_exit = zeta * sum(dist)
            mass_entry = mass_exit

            ! Add entrants at (z, K_min, S_min, D_min) with productivity drawn from stat dist
            do iz = 1, nz
                dist_new(iz, 1, 1, 1) = dist_new(iz, 1, 1, 1) + mass_entry * stat_dist_z(iz)
            end do

            ! Survivors transition according to policy functions
            do iz = 1, nz
                do iK = 1, nK
                    do iS = 1, nS
                        do iD = 1, nD

                            if (dist(iz, iK, iS, iD) < epsilon) cycle

                            ! Survival probability
                            dist_contrib = (1.0_dp - zeta) * dist(iz, iK, iS, iD)

                            ! Get next period choices
                            Kprime = pol_Kprime(iz, iK, iS, iD)
                            Sprime = pol_Sprime(iz, iK, iS, iD)
                            Dprime = pol_Dprime(iz, iK, iS, iD)

                            ! Locate on grids for interpolation
                            call locate_grid(grid_K, nK, Kprime, iKp, wKp)
                            call locate_grid(grid_S, nS, Sprime, iSp, wSp)
                            call locate_grid(grid_D, nD, Dprime, iDp, wDp)

                            ! FIX #2: Clamp weights to [0,1] to prevent mass leakage
                            ! When policies exceed grid bounds, locate_grid returns w<0 or w>1.
                            ! Without clamping, this causes negative mass assignments.
                            wKp = max(0.0_dp, min(1.0_dp, wKp))
                            wSp = max(0.0_dp, min(1.0_dp, wSp))
                            wDp = max(0.0_dp, min(1.0_dp, wDp))

                            ! Distribute mass to neighboring grid points (trilinear weights)
                            ! Loop over z' using Markov transition
                            do izp = 1, nz

                                if (Pi_z(iz, izp) < epsilon) cycle

                                ! 8 corners for trilinear interpolation
                                dist_new(izp, iKp,   iSp,   iDp)   = dist_new(izp, iKp,   iSp,   iDp) + &
                                    dist_contrib * Pi_z(iz,izp) * (1-wKp)*(1-wSp)*(1-wDp)

                                dist_new(izp, iKp,   iSp,   iDp+1) = dist_new(izp, iKp,   iSp,   iDp+1) + &
                                    dist_contrib * Pi_z(iz,izp) * (1-wKp)*(1-wSp)*wDp

                                dist_new(izp, iKp,   iSp+1, iDp)   = dist_new(izp, iKp,   iSp+1, iDp) + &
                                    dist_contrib * Pi_z(iz,izp) * (1-wKp)*wSp*(1-wDp)

                                dist_new(izp, iKp,   iSp+1, iDp+1) = dist_new(izp, iKp,   iSp+1, iDp+1) + &
                                    dist_contrib * Pi_z(iz,izp) * (1-wKp)*wSp*wDp

                                dist_new(izp, iKp+1, iSp,   iDp)   = dist_new(izp, iKp+1, iSp,   iDp) + &
                                    dist_contrib * Pi_z(iz,izp) * wKp*(1-wSp)*(1-wDp)

                                dist_new(izp, iKp+1, iSp,   iDp+1) = dist_new(izp, iKp+1, iSp,   iDp+1) + &
                                    dist_contrib * Pi_z(iz,izp) * wKp*(1-wSp)*wDp

                                dist_new(izp, iKp+1, iSp+1, iDp)   = dist_new(izp, iKp+1, iSp+1, iDp) + &
                                    dist_contrib * Pi_z(iz,izp) * wKp*wSp*(1-wDp)

                                dist_new(izp, iKp+1, iSp+1, iDp+1) = dist_new(izp, iKp+1, iSp+1, iDp+1) + &
                                    dist_contrib * Pi_z(iz,izp) * wKp*wSp*wDp

                            end do  ! izp

                        end do  ! iD
                    end do  ! iS
                end do  ! iK
            end do  ! iz

            ! Normalize
            dist_new = dist_new / sum(dist_new)

            ! Compute metric
            metric = maxval(abs(dist_new - dist))

            ! Update with dampening
            dist = update_dist * dist_new + (1.0_dp - update_dist) * dist

            ! Print progress
            if (print_progress .and. mod(iter, print_freq) == 0) then
                print '(A,I5,A,E12.5)', "  Distribution Iteration ", iter, ", metric = ", metric
            end if

            ! Check convergence
            if (metric < tol_dist) then
                print *, ""
                print '(A,I5,A)', "  Distribution converged in ", iter, " iterations."
                print '(A,E12.5)', "  Final metric: ", metric
                exit
            end if

            if (iter == maxiter_dist) then
                print *, ""
                print *, "  WARNING: Distribution did not converge!"
                print '(A,E12.5)', "  Final metric: ", metric
            end if

        end do  ! Distribution iteration

    end subroutine compute_stationary_distribution

    !===================================================================================
    ! SUBROUTINE: build_sparse_distribution
    !
    ! DESCRIPTION:
    !   Converts dense distribution array to sparse representation.
    !   Only tracks states with mass above threshold.
    !===================================================================================
    subroutine build_sparse_distribution()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: mass

        n_active_states = 0

        do iz = 1, nz
            do iK = 1, nK
                do iS = 1, nS
                    do iD = 1, nD
                        mass = dist(iz, iK, iS, iD)
                        if (mass > mass_threshold) then
                            n_active_states = n_active_states + 1
                            if (n_active_states <= max_active_states) then
                                active_states(n_active_states)%iz = iz
                                active_states(n_active_states)%iK = iK
                                active_states(n_active_states)%iS = iS
                                active_states(n_active_states)%iD = iD
                                active_states(n_active_states)%mass = mass
                            end if
                        end if
                    end do
                end do
            end do
        end do

        if (n_active_states > max_active_states) then
            print '(A,I8,A,I8)', "  WARNING: Active states (", n_active_states, &
                  ") exceeds max (", max_active_states, ")"
            n_active_states = max_active_states
        end if

    end subroutine build_sparse_distribution

    !===================================================================================
    ! SUBROUTINE: compute_stationary_distribution_sparse
    !
    ! DESCRIPTION:
    !   OPTIMIZED: Sparse iteration that only processes active states.
    !   Much faster when distribution is concentrated on a subset of states.
    !===================================================================================
    subroutine compute_stationary_distribution_sparse()
        implicit none
        integer :: iter, i, iz, iK, iS, iD, izp, iKp, iSp, iDp
        real(dp) :: Kprime, Sprime, Dprime, mass_exit, mass_entry
        real(dp) :: metric, wKp, wSp, wDp
        real(dp) :: dist_contrib, mass

        print *, ""
        print *, "======================================"
        print *, "COMPUTING STATIONARY DISTRIBUTION (SPARSE)"
        print *, "======================================"

        ! Initialize distribution
        dist = 0.0_dp
        do iz = 1, nz
            dist(iz, 1, 1, 1) = stat_dist_z(iz)
        end do
        dist = dist / sum(dist)

        ! Build initial sparse representation
        call build_sparse_distribution()
        print '(A,I8)', "  Initial active states: ", n_active_states

        ! Iterate on distribution
        do iter = 1, maxiter_dist

            dist_new = 0.0_dp

            ! Entry mass (replaces exiters)
            mass_exit = zeta * sum(dist)
            mass_entry = mass_exit

            ! Add entrants
            do iz = 1, nz
                dist_new(iz, 1, 1, 1) = dist_new(iz, 1, 1, 1) + mass_entry * stat_dist_z(iz)
            end do

            ! Process only active states (sparse iteration)
            do i = 1, n_active_states
                iz = active_states(i)%iz
                iK = active_states(i)%iK
                iS = active_states(i)%iS
                iD = active_states(i)%iD
                mass = active_states(i)%mass

                ! Survival probability
                dist_contrib = (1.0_dp - zeta) * mass

                ! Get next period choices
                Kprime = pol_Kprime(iz, iK, iS, iD)
                Sprime = pol_Sprime(iz, iK, iS, iD)
                Dprime = pol_Dprime(iz, iK, iS, iD)

                ! Locate on grids
                call locate_grid(grid_K, nK, Kprime, iKp, wKp)
                call locate_grid(grid_S, nS, Sprime, iSp, wSp)
                call locate_grid(grid_D, nD, Dprime, iDp, wDp)

                ! FIX #2: Clamp weights to [0,1] to prevent mass leakage
                wKp = max(0.0_dp, min(1.0_dp, wKp))
                wSp = max(0.0_dp, min(1.0_dp, wSp))
                wDp = max(0.0_dp, min(1.0_dp, wDp))

                ! Distribute mass to neighboring grid points (trilinear weights)
                ! Note: Sequential iteration - no race conditions
                do izp = 1, nz
                    if (Pi_z(iz, izp) < epsilon) cycle

                    ! 8 corners for trilinear interpolation
                    dist_new(izp, iKp,   iSp,   iDp)   = dist_new(izp, iKp,   iSp,   iDp) + &
                        dist_contrib * Pi_z(iz,izp) * (1-wKp)*(1-wSp)*(1-wDp)

                    dist_new(izp, iKp,   iSp,   iDp+1) = dist_new(izp, iKp,   iSp,   iDp+1) + &
                        dist_contrib * Pi_z(iz,izp) * (1-wKp)*(1-wSp)*wDp

                    dist_new(izp, iKp,   iSp+1, iDp)   = dist_new(izp, iKp,   iSp+1, iDp) + &
                        dist_contrib * Pi_z(iz,izp) * (1-wKp)*wSp*(1-wDp)

                    dist_new(izp, iKp,   iSp+1, iDp+1) = dist_new(izp, iKp,   iSp+1, iDp+1) + &
                        dist_contrib * Pi_z(iz,izp) * (1-wKp)*wSp*wDp

                    dist_new(izp, iKp+1, iSp,   iDp)   = dist_new(izp, iKp+1, iSp,   iDp) + &
                        dist_contrib * Pi_z(iz,izp) * wKp*(1-wSp)*(1-wDp)

                    dist_new(izp, iKp+1, iSp,   iDp+1) = dist_new(izp, iKp+1, iSp,   iDp+1) + &
                        dist_contrib * Pi_z(iz,izp) * wKp*(1-wSp)*wDp

                    dist_new(izp, iKp+1, iSp+1, iDp)   = dist_new(izp, iKp+1, iSp+1, iDp) + &
                        dist_contrib * Pi_z(iz,izp) * wKp*wSp*(1-wDp)

                    dist_new(izp, iKp+1, iSp+1, iDp+1) = dist_new(izp, iKp+1, iSp+1, iDp+1) + &
                        dist_contrib * Pi_z(iz,izp) * wKp*wSp*wDp

                end do  ! izp
            end do  ! active states

            ! Normalize
            dist_new = dist_new / sum(dist_new)

            ! Compute metric
            metric = maxval(abs(dist_new - dist))

            ! Update with dampening
            dist = update_dist * dist_new + (1.0_dp - update_dist) * dist

            ! Rebuild sparse representation periodically
            if (mod(iter, 50) == 0) then
                call build_sparse_distribution()
            end if

            ! Print progress
            if (print_progress .and. mod(iter, print_freq) == 0) then
                print '(A,I5,A,E12.5,A,I8)', "  Dist Iter ", iter, &
                    ", metric = ", metric, ", active = ", n_active_states
            end if

            ! Check convergence
            if (metric < tol_dist) then
                call build_sparse_distribution()
                print *, ""
                print '(A,I5,A)', "  Distribution converged in ", iter, " iterations."
                print '(A,E12.5)', "  Final metric: ", metric
                print '(A,I8)', "  Final active states: ", n_active_states
                exit
            end if

            if (iter == maxiter_dist) then
                print *, ""
                print *, "  WARNING: Distribution did not converge!"
                print '(A,E12.5)', "  Final metric: ", metric
            end if

        end do  ! Distribution iteration

    end subroutine compute_stationary_distribution_sparse

    !===================================================================================
    ! SUBROUTINE: compute_aggregates
    !
    ! DESCRIPTION:
    !   Computes aggregate quantities by integrating over distribution.
    !   OPTIMIZED with OpenMP reduction.
    !===================================================================================
    subroutine compute_aggregates()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: mass
        real(dp) :: loc_K, loc_S, loc_L, loc_HP, loc_HR, loc_D, loc_Y, loc_IK
        real(dp) :: loc_mass, loc_constr

        ! Initialize
        agg_K = 0.0_dp
        agg_S = 0.0_dp
        agg_L = 0.0_dp
        agg_HP = 0.0_dp
        agg_HR = 0.0_dp
        agg_D = 0.0_dp
        agg_Y = 0.0_dp
        agg_IK = 0.0_dp
        mass_firms = 0.0_dp
        frac_constrained = 0.0_dp

        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, mass) &
        !$OMP& REDUCTION(+:agg_K, agg_S, agg_L, agg_HP, agg_HR, agg_D, agg_Y, agg_IK, mass_firms, frac_constrained) &
        !$OMP& SCHEDULE(static)
        do iz = 1, nz
            do iK = 1, nK
                do iS = 1, nS
                    do iD = 1, nD

                        mass = dist(iz, iK, iS, iD)
                        if (mass < epsilon) cycle

                        mass_firms = mass_firms + mass

                        agg_K = agg_K + mass * grid_K(iK)
                        agg_S = agg_S + mass * grid_S(iS)
                        agg_L = agg_L + mass * pol_L(iz, iK, iS, iD)
                        agg_HP = agg_HP + mass * pol_HP(iz, iK, iS, iD)
                        agg_HR = agg_HR + mass * pol_HR(iz, iK, iS, iD)
                        agg_D = agg_D + mass * pol_Dprime(iz, iK, iS, iD)
                        agg_Y = agg_Y + mass * pol_Y(iz, iK, iS, iD)
                        agg_IK = agg_IK + mass * pol_IK(iz, iK, iS, iD)

                        if (pol_constr(iz, iK, iS, iD)) then
                            frac_constrained = frac_constrained + mass
                        end if

                    end do
                end do
            end do
        end do
        !$OMP END PARALLEL DO

        ! Total skilled labor
        agg_H = agg_HP + agg_HR

        ! Consumption (from resource constraint)
        agg_C = agg_Y - agg_IK - zeta * ce * mass_firms

        ! Statistics
        if (agg_K + agg_S > epsilon) then
            avg_intang_intensity = agg_S / (agg_K + agg_S)
        else
            avg_intang_intensity = 0.0_dp
        end if

        if (agg_K + agg_S > epsilon) then
            avg_leverage = agg_D / (agg_K + agg_S)
        else
            avg_leverage = 0.0_dp
        end if

        frac_constrained = frac_constrained / max(mass_firms, epsilon)

    end subroutine compute_aggregates

    !===================================================================================
    ! SUBROUTINE: print_aggregates
    !
    ! DESCRIPTION:
    !   Prints aggregate statistics
    !===================================================================================
    subroutine print_aggregates()
        implicit none

        print *, ""
        print *, "======================================"
        print *, "AGGREGATE STATISTICS"
        print *, "======================================"
        print '(A,F12.6)', "  Output (Y):                  ", agg_Y
        print '(A,F12.6)', "  Consumption (C):             ", agg_C
        print '(A,F12.6)', "  Tangible capital (K):        ", agg_K
        print '(A,F12.6)', "  Intangible capital (S):      ", agg_S
        print '(A,F12.6)', "  Tangible investment:         ", agg_IK
        print '(A,F12.6)', "  Total debt (D):              ", agg_D
        print *, ""
        print '(A,F12.6)', "  Unskilled labor (L):         ", agg_L
        print '(A,F12.6)', "  Skilled labor - Prod (HP):   ", agg_HP
        print '(A,F12.6)', "  Skilled labor - R&D (HR):    ", agg_HR
        print '(A,F12.6)', "  Total skilled (HP+HR):       ", agg_H
        print *, ""
        print '(A,F12.6)', "  Mass of firms:               ", mass_firms
        print '(A,F10.2,A)', "  Fraction constrained:        ", frac_constrained*100.0_dp, "%"
        print '(A,F10.4)', "  Avg intangible intensity:    ", avg_intang_intensity
        print '(A,F10.4)', "  Avg leverage:                ", avg_leverage
        print *, ""

    end subroutine print_aggregates

    !===================================================================================
    ! SUBROUTINE: compute_distribution_diagnostics
    !
    ! DESCRIPTION:
    !   Computes detailed distribution diagnostics: quintiles, policy stats, etc.
    !   Helps understand the cross-sectional distribution of firms.
    !===================================================================================
    subroutine compute_distribution_diagnostics()
        use mod_interpolation
        implicit none
        integer :: iz, iK, iS, iD, i, j, n_firms
        real(dp) :: mass, total_mass
        real(dp) :: cum_mass, target_pct

        ! Arrays for computing weighted statistics
        integer, parameter :: max_firms = 100000
        real(dp) :: firm_K(max_firms), firm_S(max_firms), firm_Ktot(max_firms)
        real(dp) :: firm_D(max_firms), firm_profit(max_firms)
        real(dp) :: firm_L(max_firms), firm_HP(max_firms), firm_HR(max_firms)
        real(dp) :: firm_IK(max_firms), firm_Y(max_firms)
        real(dp) :: firm_mass(max_firms)
        real(dp) :: sorted_val(max_firms), sorted_mass(max_firms)

        ! Quintile boundaries
        real(dp) :: q20, q40, q60, q80
        real(dp) :: mean_val, std_val, min_val, max_val

        ! R&D and constraint diagnostics
        real(dp) :: mass_positive_HR, mass_positive_D, mass_at_Smin, mass_at_Kmin
        real(dp) :: avg_HR_if_positive, avg_D_if_positive
        real(dp) :: V_S_estimate, EV_high_S, EV_low_S
        integer :: iS_low, iS_high

        print *, ""
        print *, "======================================"
        print *, "DISTRIBUTION DIAGNOSTICS"
        print *, "======================================"

        ! Collect firm-level data from distribution
        n_firms = 0
        total_mass = 0.0_dp
        mass_positive_HR = 0.0_dp
        mass_positive_D = 0.0_dp
        mass_at_Smin = 0.0_dp
        mass_at_Kmin = 0.0_dp

        do iz = 1, nz
            do iK = 1, nK
                do iS = 1, nS
                    do iD = 1, nD
                        mass = dist(iz, iK, iS, iD)
                        if (mass < epsilon) cycle

                        n_firms = n_firms + 1
                        if (n_firms > max_firms) then
                            print *, "  WARNING: Too many firm types for diagnostics"
                            exit
                        end if

                        firm_mass(n_firms) = mass
                        firm_K(n_firms) = grid_K(iK)
                        firm_S(n_firms) = grid_S(iS)
                        firm_Ktot(n_firms) = grid_K(iK) + grid_S(iS)
                        firm_D(n_firms) = grid_D(iD)
                        firm_L(n_firms) = pol_L(iz, iK, iS, iD)
                        firm_HP(n_firms) = pol_HP(iz, iK, iS, iD)
                        firm_HR(n_firms) = pol_HR(iz, iK, iS, iD)
                        firm_IK(n_firms) = pol_IK(iz, iK, iS, iD)
                        firm_Y(n_firms) = pol_Y(iz, iK, iS, iD)
                        firm_profit(n_firms) = pol_Y(iz, iK, iS, iD) - wL * pol_L(iz, iK, iS, iD) &
                                              - wH * pol_HP(iz, iK, iS, iD)

                        total_mass = total_mass + mass

                        ! Track special cases
                        if (pol_HR(iz, iK, iS, iD) > epsilon) then
                            mass_positive_HR = mass_positive_HR + mass
                        end if
                        if (pol_Dprime(iz, iK, iS, iD) > epsilon) then
                            mass_positive_D = mass_positive_D + mass
                        end if
                        if (iS == 1) mass_at_Smin = mass_at_Smin + mass
                        if (iK == 1) mass_at_Kmin = mass_at_Kmin + mass

                    end do
                end do
            end do
        end do

        print '(A,I8)', "  Number of firm types with positive mass: ", n_firms
        print '(A,F10.6)', "  Total mass (should be 1): ", total_mass
        print *, ""

        ! Mass at boundaries and debt diagnostics
        print *, "  Mass at grid boundaries:"
        print '(A,F8.2,A)', "    At K_min:  ", 100.0_dp * mass_at_Kmin / total_mass, "%"
        print '(A,F8.2,A)', "    At S_min:  ", 100.0_dp * mass_at_Smin / total_mass, "%"
        print '(A,F8.2,A)', "    With HR>0: ", 100.0_dp * mass_positive_HR / total_mass, "%"
        print '(A,F8.2,A)', "    With D'>0 (choice): ", 100.0_dp * mass_positive_D / total_mass, "%"
        print *, ""

        ! Detailed debt diagnostics
        block
            real(dp) :: mass_D_state_zero, avg_Dprime_positive, sum_Dprime_positive
            real(dp) :: mass_Dprime_large, Dprime_val, grid_D_step
            integer :: j

            ! Mass at D=0 state
            mass_D_state_zero = 0.0_dp
            do i = 1, n_firms
                if (firm_D(i) < epsilon) mass_D_state_zero = mass_D_state_zero + firm_mass(i)
            end do
            print '(A,F8.2,A)', "    With D=0 (state):   ", 100.0_dp * mass_D_state_zero / total_mass, "%"

            ! Average D' among those with D'>0
            sum_Dprime_positive = 0.0_dp
            mass_Dprime_large = 0.0_dp
            grid_D_step = (D_max - D_min) / real(nD - 1, dp)

            do iz = 1, nz
                do iK = 1, nK
                    do iS = 1, nS
                        do iD = 1, nD
                            mass = dist(iz, iK, iS, iD)
                            if (mass < epsilon) cycle
                            Dprime_val = pol_Dprime(iz, iK, iS, iD)
                            if (Dprime_val > epsilon) then
                                sum_Dprime_positive = sum_Dprime_positive + mass * Dprime_val
                            end if
                            ! Count mass with D' > half grid step (would map mostly to iD>=2)
                            if (Dprime_val > 0.5_dp * grid_D_step) then
                                mass_Dprime_large = mass_Dprime_large + mass
                            end if
                        end do
                    end do
                end do
            end do

            if (mass_positive_D > epsilon) then
                avg_Dprime_positive = sum_Dprime_positive / mass_positive_D
            else
                avg_Dprime_positive = 0.0_dp
            end if

            print *, ""
            print *, "  Debt choice analysis:"
            print '(A,F12.6)', "    Avg D'' among D''>0:       ", avg_Dprime_positive
            print '(A,F12.6)', "    Grid step (D_max/nD-1):   ", grid_D_step
            print '(A,F8.2,A)', "    With D'' > 0.5*grid_step: ", 100.0_dp * mass_Dprime_large / total_mass, "%"
            print *, ""
            print *, "  NOTE: If avg D'' << grid_step, most D''>0 firms interpolate to D=0 state"
        end block
        print *, ""

        ! Compute weighted statistics for key variables
        print *, "  Weighted Statistics (across firm distribution):"
        print *, "  -----------------------------------------------"

        ! Total capital quintiles
        call compute_weighted_quintiles(firm_Ktot, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  TOTAL CAPITAL (K+S):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F12.4,A,F12.4)', "    Min:  ", min_val, "  Max: ", max_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Tangible capital
        call compute_weighted_quintiles(firm_K, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  TANGIBLE CAPITAL (K):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Intangible capital
        call compute_weighted_quintiles(firm_S, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  INTANGIBLE CAPITAL (S):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Profits
        call compute_weighted_quintiles(firm_profit, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  GROSS PROFITS (Y - wL*L - wH*HP):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Output
        call compute_weighted_quintiles(firm_Y, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  OUTPUT (Y):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Labor
        call compute_weighted_quintiles(firm_L, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  UNSKILLED LABOR (L):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! HP
        call compute_weighted_quintiles(firm_HP, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  SKILLED PRODUCTION LABOR (HP):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! HR
        call compute_weighted_quintiles(firm_HR, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  R&D LABOR (HR):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Tangible investment
        call compute_weighted_quintiles(firm_IK, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  TANGIBLE INVESTMENT (I^K):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! Debt
        call compute_weighted_quintiles(firm_D, firm_mass, n_firms, &
                                        mean_val, std_val, min_val, max_val, q20, q40, q60, q80)
        print *, ""
        print *, "  DEBT (D):"
        print '(A,F12.4,A,F12.4)', "    Mean: ", mean_val, "  Std: ", std_val
        print '(A,F10.4,A,F10.4,A,F10.4,A,F10.4)', "    Quintiles: ", q20, " | ", q40, " | ", q60, " | ", q80

        ! VALUE FUNCTION DIAGNOSTICS
        print *, ""
        print *, "  -----------------------------------------------"
        print *, "  VALUE FUNCTION DIAGNOSTICS (for R&D incentives)"
        print *, "  -----------------------------------------------"

        ! Check if V is increasing in S at median K, median z, D=0
        iS_low = 1
        iS_high = min(nS, 5)  ! Compare S_min to 5th grid point

        print *, ""
        print *, "  V(z_median, K_median, S, D_min) for varying S:"
        do iS = 1, min(nS, 10)
            print '(A,I3,A,F8.3,A,ES14.6)', "    iS=", iS, ", S=", grid_S(iS), &
                  ", V=", V((nz+1)/2, (nK+1)/2, iS, 1)
        end do

        ! Estimate marginal value of S
        V_S_estimate = (V((nz+1)/2, (nK+1)/2, iS_high, 1) - V((nz+1)/2, (nK+1)/2, iS_low, 1)) &
                      / (grid_S(iS_high) - grid_S(iS_low))
        print *, ""
        print '(A,ES14.6)', "  Estimated dV/dS at median state: ", V_S_estimate

        ! Check R&D profitability
        print *, ""
        print *, "  R&D PROFITABILITY CHECK:"
        print *, "  (Benefit of HR = second grid point vs HR = 0)"
        print '(A,F10.6)', "    wH (cost per unit HR):           ", wH
        print '(A,F10.6)', "    grid_HR(2):                      ", grid_HR(2)
        print '(A,F10.6)', "    Cost of HR(2):                   ", wH * grid_HR(2)
        print '(A,F10.6)', "    S investment from HR(2):         ", Gamma_RD * grid_HR(2)**xi
        print '(A,F10.6)', "    beta*(1-zeta)*dV/dS*dS:          ", &
              beta * (1.0_dp - zeta) * V_S_estimate * Gamma_RD * grid_HR(2)**xi
        print '(A,F10.6)', "    Net benefit of R&D (should be >0 if R&D worthwhile): ", &
              beta * (1.0_dp - zeta) * V_S_estimate * Gamma_RD * grid_HR(2)**xi - wH * grid_HR(2)

    end subroutine compute_distribution_diagnostics

    !===================================================================================
    ! SUBROUTINE: compute_weighted_quintiles
    !
    ! DESCRIPTION:
    !   Computes weighted mean, std, min, max, and quintile boundaries.
    !===================================================================================
    subroutine compute_weighted_quintiles(values, weights, n, mean_val, std_val, &
                                          min_val, max_val, q20, q40, q60, q80)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: values(n), weights(n)
        real(dp), intent(out) :: mean_val, std_val, min_val, max_val
        real(dp), intent(out) :: q20, q40, q60, q80

        real(dp) :: total_weight, cum_weight
        real(dp) :: sorted_val(n), sorted_wgt(n)
        integer :: idx(n), i, j, itemp
        real(dp) :: temp

        if (n <= 0) then
            mean_val = 0.0_dp
            std_val = 0.0_dp
            min_val = 0.0_dp
            max_val = 0.0_dp
            q20 = 0.0_dp
            q40 = 0.0_dp
            q60 = 0.0_dp
            q80 = 0.0_dp
            return
        end if

        ! Handle n=1 case (no sorting needed)
        if (n == 1) then
            mean_val = values(1)
            std_val = 0.0_dp
            min_val = values(1)
            max_val = values(1)
            q20 = values(1)
            q40 = values(1)
            q60 = values(1)
            q80 = values(1)
            return
        end if

        ! Compute weighted mean
        total_weight = sum(weights(1:n))
        mean_val = sum(values(1:n) * weights(1:n)) / total_weight

        ! Compute weighted std
        std_val = sqrt(sum(weights(1:n) * (values(1:n) - mean_val)**2) / total_weight)

        ! Min and max
        min_val = minval(values(1:n))
        max_val = maxval(values(1:n))

        ! Sort by value for quintiles (simple insertion sort)
        do i = 1, n
            idx(i) = i
        end do

        do i = 2, n
            j = i
            ! Note: Fortran .and. does NOT short-circuit, so we must use nested ifs
            do while (j > 1)
                if (values(idx(j-1)) > values(idx(j))) then
                    itemp = idx(j)
                    idx(j) = idx(j-1)
                    idx(j-1) = itemp
                    j = j - 1
                else
                    exit
                end if
            end do
        end do

        ! Copy to sorted arrays
        do i = 1, n
            sorted_val(i) = values(idx(i))
            sorted_wgt(i) = weights(idx(i))
        end do

        ! Find quintile boundaries using boolean flags
        ! (fixes bug when sorted_val(1) equals a legitimate quintile value)
        cum_weight = 0.0_dp
        q20 = sorted_val(n)  ! Default to max if not found
        q40 = sorted_val(n)
        q60 = sorted_val(n)
        q80 = sorted_val(n)

        block
            logical :: found_q20, found_q40, found_q60, found_q80
            found_q20 = .false.
            found_q40 = .false.
            found_q60 = .false.
            found_q80 = .false.

            do i = 1, n
                cum_weight = cum_weight + sorted_wgt(i)
                if (.not. found_q20 .and. cum_weight / total_weight >= 0.20_dp) then
                    q20 = sorted_val(i)
                    found_q20 = .true.
                end if
                if (.not. found_q40 .and. cum_weight / total_weight >= 0.40_dp) then
                    q40 = sorted_val(i)
                    found_q40 = .true.
                end if
                if (.not. found_q60 .and. cum_weight / total_weight >= 0.60_dp) then
                    q60 = sorted_val(i)
                    found_q60 = .true.
                end if
                if (.not. found_q80 .and. cum_weight / total_weight >= 0.80_dp) then
                    q80 = sorted_val(i)
                    found_q80 = .true.
                end if
            end do
        end block

    end subroutine compute_weighted_quintiles

end module mod_distribution