!===================================================================================
! MODULE: mod_distribution
!
! DESCRIPTION:
!   Computes the stationary distribution of firms over state space (z,K,S,D).
!   Uses policy functions from firm problem and accounts for entry/exit.
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_distribution
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_interpolation
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
    ! SUBROUTINE: compute_aggregates
    !
    ! DESCRIPTION:
    !   Computes aggregate quantities by integrating over distribution.
    !===================================================================================
    subroutine compute_aggregates()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: mass

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

end module mod_distribution
