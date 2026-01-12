!===================================================================================
! MODULE: mod_firm_problem
!
! DESCRIPTION:
!   Solves the firm's optimization problem via value function iteration.
!   Firm state: (z, K, S, D_{-1})
!   Firm choices: (I^K, H^R, D) subject to collateral constraint
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_firm_problem
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_interpolation
    implicit none

contains

    !===================================================================================
    ! SUBROUTINE: solve_static_labor
    !
    ! DESCRIPTION:
    !   Solves for optimal static labor choices (L, HP) given state and wages.
    !   FOCs: ∂Y/∂L = wL,  ∂Y/∂HP = wH
    !
    ! INPUTS:
    !   z, K, S  - Current state
    !   wL, wH   - Wages
    !
    ! OUTPUTS:
    !   L_opt, HP_opt - Optimal labor choices
    !   Y_opt         - Resulting output
    !===================================================================================
    subroutine solve_static_labor(z, K, S, wL_in, wH_in, L_opt, HP_opt, Y_opt)
        implicit none
        real(dp), intent(in) :: z, K, S, wL_in, wH_in
        real(dp), intent(out) :: L_opt, HP_opt, Y_opt
        real(dp) :: L_try, HP_try, MPL, MPHP
        real(dp) :: L_low, L_high, HP_low, HP_high
        real(dp) :: tol_labor
        integer :: iter, maxiter_labor
        logical :: converged

        tol_labor = 1.0e-6_dp
        maxiter_labor = 100

        ! Initial guesses based on average values
        L_try = 0.50_dp
        HP_try = 0.20_dp

        ! Bounds
        L_low = 0.01_dp
        L_high = 2.0_dp
        HP_low = 0.01_dp
        HP_high = 1.0_dp

        ! Simple fixed-point iteration (could use Newton for speed)
        converged = .false.
        do iter = 1, maxiter_labor
            ! Compute marginal products
            MPL = marginal_product_L(z, K, S, L_try, HP_try)
            MPHP = marginal_product_HP(z, K, S, L_try, HP_try)

            ! Update based on FOCs (with dampening)
            L_opt = 0.5_dp * L_try + 0.5_dp * L_try * (MPL / (wL_in + epsilon))
            HP_opt = 0.5_dp * HP_try + 0.5_dp * HP_try * (MPHP / (wH_in + epsilon))

            ! Enforce bounds
            L_opt = max(L_low, min(L_high, L_opt))
            HP_opt = max(HP_low, min(HP_high, HP_opt))

            ! Check convergence
            if (abs(L_opt - L_try) < tol_labor .and. abs(HP_opt - HP_try) < tol_labor) then
                converged = .true.
                exit
            end if

            L_try = L_opt
            HP_try = HP_opt
        end do

        ! Compute output with optimal labor
        Y_opt = production_Y(z, K, S, L_opt, HP_opt)

    end subroutine solve_static_labor

    !===================================================================================
    ! SUBROUTINE: solve_firm_problem
    !
    ! DESCRIPTION:
    !   Value function iteration to solve firm problem.
    !   For each state (z,K,S,D_{-1}), chooses (I^K, H^R, D) to maximize value.
    !
    ! TIMING:
    !   1. Static labor (L, HP) chosen to maximize gross profits
    !   2. Dynamic choices (I^K, H^R, D) subject to budget and collateral constraints
    !===================================================================================
    subroutine solve_firm_problem()
        implicit none
        integer :: iz, iK, iS, iD, iter
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: V_best, V_try
        integer :: iIK, iHR, iDp
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: resources, expenses, dividends
        real(dp) :: D_max_coll, EV
        real(dp) :: metric

        print *, ""
        print *, "======================================"
        print *, "SOLVING FIRM PROBLEM (VFI)"
        print *, "======================================"

        ! Initialize value function
        V = 0.0_dp

        ! Value function iteration
        do iter = 1, maxiter_VFI

            V_new = V  ! Start with previous iteration

            ! Loop over state space
            do iz = 1, nz
                z_val = grid_z(iz)

                do iK = 1, nK
                    K_val = grid_K(iK)

                    do iS = 1, nS
                        S_val = grid_S(iS)

                        do iD = 1, nD
                            D_old_val = grid_D(iD)

                            ! Solve static labor problem
                            call solve_static_labor(z_val, K_val, S_val, wL, wH, &
                                                   L_opt, HP_opt, Y_val)

                            ! Gross profits
                            Pi_gross = Y_val - wL * L_opt - wH * HP_opt

                            ! Available resources after repaying old debt
                            resources = Pi_gross - R * D_old_val

                            ! If negative resources, firm is insolvent - set V to very negative
                            if (resources < -epsilon) then
                                V_new(iz, iK, iS, iD) = -1.0e10_dp
                                pol_constr(iz, iK, iS, iD) = .false.
                                cycle
                            end if

                            ! Collateral constraint for new borrowing
                            D_max_coll = collateral_constraint(K_val, S_val)

                            ! Optimize over choices
                            V_best = -1.0e10_dp

                            ! Grid search over (D', I^K, H^R)
                            do iDp = 1, nDprime
                                Dp_choice = grid_Dprime(iDp)

                                ! Check collateral constraint
                                if (Dp_choice > D_max_coll + epsilon) cycle

                                ! Net borrowing
                                resources = Pi_gross - R * D_old_val + Dp_choice

                                do iIK = 1, nIK
                                    IK_choice = grid_IK(iIK)

                                    do iHR = 1, nHR
                                        HR_choice = grid_HR(iHR)

                                        ! Expenses
                                        expenses = IK_choice + wH * HR_choice

                                        ! Dividends (non-negativity constraint)
                                        dividends = resources - expenses

                                        if (dividends < -epsilon) cycle

                                        ! Capital evolution
                                        Kprime = (1.0_dp - delta_K) * K_val + IK_choice
                                        inv_S = RD_production(HR_choice)
                                        Sprime = (1.0_dp - delta_S) * S_val + inv_S

                                        ! Continuation value
                                        EV = expect_V(iz, Kprime, Sprime, Dp_choice)

                                        ! Value (accounting for exit)
                                        V_try = dividends + beta * (1.0_dp - zeta) * EV

                                        ! Update best
                                        if (V_try > V_best) then
                                            V_best = V_try
                                            ! Store policies
                                            pol_IK(iz, iK, iS, iD) = IK_choice
                                            pol_HR(iz, iK, iS, iD) = HR_choice
                                            pol_Dprime(iz, iK, iS, iD) = Dp_choice
                                            pol_Kprime(iz, iK, iS, iD) = Kprime
                                            pol_Sprime(iz, iK, iS, iD) = Sprime
                                            pol_L(iz, iK, iS, iD) = L_opt
                                            pol_HP(iz, iK, iS, iD) = HP_opt
                                            pol_Y(iz, iK, iS, iD) = Y_val

                                            ! Check if collateral constraint binds
                                            pol_constr(iz, iK, iS, iD) = &
                                                (abs(Dp_choice - D_max_coll) < 0.01_dp)
                                        end if
                                    end do  ! HR
                                end do  ! IK
                            end do  ! Dp

                            ! Update value
                            V_new(iz, iK, iS, iD) = V_best

                        end do  ! iD
                    end do  ! iS
                end do  ! iK
            end do  ! iz

            ! Compute metric
            metric = maxval(abs(V_new - V))

            ! Update with dampening
            V = update_VFI * V_new + (1.0_dp - update_VFI) * V

            ! Print progress
            if (print_progress .and. mod(iter, print_freq) == 0) then
                print '(A,I5,A,E12.5)', "  VFI Iteration ", iter, ", metric = ", metric
            end if

            ! Check convergence
            if (metric < tol_VFI) then
                print *, ""
                print '(A,I5,A)', "  VFI converged in ", iter, " iterations."
                print '(A,E12.5)', "  Final metric: ", metric
                exit
            end if

            if (iter == maxiter_VFI) then
                print *, ""
                print *, "  WARNING: VFI did not converge!"
                print '(A,E12.5)', "  Final metric: ", metric
            end if

        end do  ! VFI iteration

    end subroutine solve_firm_problem

end module mod_firm_problem
