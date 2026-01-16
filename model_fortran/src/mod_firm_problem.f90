!===================================================================================
! MODULE: mod_firm_problem
!
! DESCRIPTION:
!   Solves the firm's optimization problem via value function iteration
!   with Howard's Policy Improvement Algorithm for acceleration.
!
!   Firm state: (z, K, S, D_{-1})
!   Firm choices: (I^K, H^R, D) subject to collateral constraint
!
!   Howard's improvement: Alternates between
!     1. Policy Improvement (expensive): Full optimization over choice space
!     2. Policy Evaluation (cheap): Update V with fixed policy (no search)
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
    !===================================================================================
    subroutine solve_static_labor(z, K, S, wL_in, wH_in, L_opt, HP_opt, Y_opt)
        implicit none
        real(dp), intent(in) :: z, K, S, wL_in, wH_in
        real(dp), intent(out) :: L_opt, HP_opt, Y_opt
        real(dp) :: L_try, HP_try, MPL, MPHP
        real(dp) :: L_new, HP_new
        real(dp) :: L_low, L_high, HP_low, HP_high
        real(dp) :: tol_labor, dampen
        integer :: iter, maxiter_labor
        logical :: converged

        tol_labor = 1.0e-5_dp
        maxiter_labor = 200
        dampen = 0.3_dp

        L_try = 0.30_dp
        HP_try = 0.15_dp

        L_low = 0.01_dp
        L_high = 2.0_dp
        HP_low = 0.01_dp
        HP_high = 1.0_dp

        converged = .false.
        do iter = 1, maxiter_labor
            MPL = marginal_product_L(z, K, S, L_try, HP_try)
            MPHP = marginal_product_HP(z, K, S, L_try, HP_try)

            if (isnan(MPL) .or. isnan(MPHP) .or. MPL > 1.0e10_dp .or. MPHP > 1.0e10_dp) then
                L_opt = L_low
                HP_opt = HP_low
                Y_opt = production_Y(z, K, S, L_opt, HP_opt)
                return
            end if

            L_new = L_try * (MPL / (wL_in + epsilon))**dampen
            HP_new = HP_try * (MPHP / (wH_in + epsilon))**dampen

            L_new = max(L_low, min(L_high, L_new))
            HP_new = max(HP_low, min(HP_high, HP_new))

            if (abs(L_new - L_try) < tol_labor .and. abs(HP_new - HP_try) < tol_labor) then
                converged = .true.
                L_opt = L_new
                HP_opt = HP_new
                exit
            end if

            L_try = L_new
            HP_try = HP_new
        end do

        if (.not. converged) then
            L_opt = L_try
            HP_opt = HP_try
        end if

        Y_opt = production_Y(z, K, S, L_opt, HP_opt)

    end subroutine solve_static_labor

    !===================================================================================
    ! SUBROUTINE: policy_improvement_step
    !
    ! DESCRIPTION:
    !   Full policy optimization step (expensive).
    !   For each state, searches over all choice combinations to find optimal policy.
    !   Updates both V_new and policy functions.
    !===================================================================================
    subroutine policy_improvement_step(show_progress)
        implicit none
        logical, intent(in) :: show_progress
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: V_best, V_try
        integer :: iIK, iHR, iDp
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: resources, expenses, dividends
        real(dp) :: D_max_coll, EV
        integer :: total_states, state_count

        total_states = nz * nK * nS * nD
        state_count = 0

        ! Loop over state space
        do iz = 1, nz
            z_val = grid_z(iz)

            do iK = 1, nK
                K_val = grid_K(iK)

                do iS = 1, nS
                    S_val = grid_S(iS)

                    do iD = 1, nD
                        D_old_val = grid_D(iD)
                        state_count = state_count + 1

                        ! Solve static labor problem
                        call solve_static_labor(z_val, K_val, S_val, wL, wH, &
                                               L_opt, HP_opt, Y_val)

                        ! Gross profits
                        Pi_gross = Y_val - wL * L_opt - wH * HP_opt

                        ! Note: We don't check solvency here because firm can borrow D' to cover
                        ! temporary liquidity shortfalls. The dividend constraint inside the
                        ! choice loop will handle true insolvency.

                        ! Collateral constraint
                        D_max_coll = collateral_constraint(K_val, S_val)

                        ! Optimize over choices
                        V_best = -1.0e10_dp

                        ! Grid search over (D', I^K, H^R)
                        do iDp = 1, nDprime
                            Dp_choice = grid_Dprime(iDp)

                            if (Dp_choice > D_max_coll + epsilon) cycle

                            resources = Pi_gross - R * D_old_val + Dp_choice

                            do iIK = 1, nIK
                                IK_choice = grid_IK(iIK)

                                ! Check that K' stays within grid bounds
                                Kprime = (1.0_dp - delta_K) * K_val + IK_choice
                                if (Kprime < K_min .or. Kprime > K_max) cycle

                                do iHR = 1, nHR
                                    HR_choice = grid_HR(iHR)

                                    expenses = IK_choice + wH * HR_choice
                                    dividends = resources - expenses

                                    if (dividends < -epsilon) cycle
                                    inv_S = RD_production(HR_choice)
                                    Sprime = (1.0_dp - delta_S) * S_val + inv_S

                                    EV = expect_V(iz, Kprime, Sprime, Dp_choice)
                                    V_try = dividends + beta * (1.0_dp - zeta) * EV

                                    if (V_try > V_best) then
                                        V_best = V_try
                                        pol_IK(iz, iK, iS, iD) = IK_choice
                                        pol_HR(iz, iK, iS, iD) = HR_choice
                                        pol_Dprime(iz, iK, iS, iD) = Dp_choice
                                        pol_Kprime(iz, iK, iS, iD) = Kprime
                                        pol_Sprime(iz, iK, iS, iD) = Sprime
                                        pol_L(iz, iK, iS, iD) = L_opt
                                        pol_HP(iz, iK, iS, iD) = HP_opt
                                        pol_Y(iz, iK, iS, iD) = Y_val
                                        ! Constrained if borrowing at or near collateral limit
                                        ! Use relative tolerance for robustness across scales
                                        pol_constr(iz, iK, iS, iD) = &
                                            (Dp_choice >= D_max_coll - 0.01_dp * max(1.0_dp, D_max_coll))
                                    end if
                                end do  ! HR
                            end do  ! IK
                        end do  ! Dp

                        V_new(iz, iK, iS, iD) = V_best

                    end do  ! iD
                end do  ! iS
            end do  ! iK
        end do  ! iz

    end subroutine policy_improvement_step

    !===================================================================================
    ! SUBROUTINE: policy_evaluation_step
    !
    ! DESCRIPTION:
    !   Policy evaluation step (cheap).
    !   Updates V using FIXED policy functions - no optimization search.
    !   This is O(state_space) vs O(state_space * choice_space).
    !===================================================================================
    subroutine policy_evaluation_step()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime
        real(dp) :: resources, expenses, dividends
        real(dp) :: EV, V_eval

        ! Loop over state space - using FIXED policies
        do iz = 1, nz
            z_val = grid_z(iz)

            do iK = 1, nK
                K_val = grid_K(iK)

                do iS = 1, nS
                    S_val = grid_S(iS)

                    do iD = 1, nD
                        D_old_val = grid_D(iD)

                        ! Skip insolvent states
                        if (V(iz, iK, iS, iD) < -1.0e9_dp) then
                            V_new(iz, iK, iS, iD) = V(iz, iK, iS, iD)
                            cycle
                        end if

                        ! Use stored policy functions (no optimization!)
                        L_opt = pol_L(iz, iK, iS, iD)
                        HP_opt = pol_HP(iz, iK, iS, iD)
                        Y_val = pol_Y(iz, iK, iS, iD)
                        IK_choice = pol_IK(iz, iK, iS, iD)
                        HR_choice = pol_HR(iz, iK, iS, iD)
                        Dp_choice = pol_Dprime(iz, iK, iS, iD)
                        Kprime = pol_Kprime(iz, iK, iS, iD)
                        Sprime = pol_Sprime(iz, iK, iS, iD)

                        ! Compute value with fixed policy
                        Pi_gross = Y_val - wL * L_opt - wH * HP_opt
                        resources = Pi_gross - R * D_old_val + Dp_choice
                        expenses = IK_choice + wH * HR_choice
                        dividends = resources - expenses

                        ! Continuation value (this is the only "expensive" part)
                        EV = expect_V(iz, Kprime, Sprime, Dp_choice)

                        ! Value
                        V_eval = dividends + beta * (1.0_dp - zeta) * EV

                        V_new(iz, iK, iS, iD) = V_eval

                    end do  ! iD
                end do  ! iS
            end do  ! iK
        end do  ! iz

    end subroutine policy_evaluation_step

    !===================================================================================
    ! SUBROUTINE: solve_firm_problem
    !
    ! DESCRIPTION:
    !   Value function iteration with Howard's Policy Improvement.
    !   Alternates between:
    !     - Policy Improvement (every howard_freq iterations): Full optimization
    !     - Policy Evaluation (other iterations): Just update V with fixed policy
    !===================================================================================
    subroutine solve_firm_problem()
        implicit none
        integer :: iter, eval_iter
        real(dp) :: metric, metric_eval
        integer :: total_states
        logical :: do_improvement

        print *, ""
        print *, "======================================"
        print *, "SOLVING FIRM PROBLEM (VFI + HOWARD)"
        print *, "======================================"

        total_states = nz * nK * nS * nD
        print '(A,I8)', "  Total state space points: ", total_states
        print '(A,I8)', "  Choice combinations per state: ", nIK * nHR * nDprime
        print '(A,I4)', "  Howard improvement frequency: ", howard_freq
        print '(A,I4)', "  Howard evaluation steps: ", howard_eval_steps
        print *, ""

        ! Initialize value function and policies
        V = 0.0_dp
        V_new = 0.0_dp
        pol_IK = 0.0_dp
        pol_HR = 0.0_dp
        pol_Dprime = 0.0_dp
        pol_Kprime = K_min
        pol_Sprime = S_min
        pol_L = 0.1_dp
        pol_HP = 0.1_dp
        pol_Y = 0.0_dp

        ! Main iteration loop
        do iter = 1, maxiter_VFI

            ! Decide: Policy Improvement or Policy Evaluation?
            do_improvement = (iter == 1) .or. (mod(iter-1, howard_freq) == 0)

            if (do_improvement) then
                !---------------------------------------------------------------
                ! POLICY IMPROVEMENT STEP (expensive)
                !---------------------------------------------------------------
                print '(A,I5,A)', "  Iter ", iter, ": Policy IMPROVEMENT (full optimization)..."

                call policy_improvement_step(iter == 1)

                ! Compute metric
                metric = maxval(abs(V_new - V))

                ! Update V
                V = update_VFI * V_new + (1.0_dp - update_VFI) * V

                print '(A,E12.5)', "           metric = ", metric

                ! Check convergence
                if (metric < tol_VFI) then
                    print *, ""
                    print '(A,I5,A)', "  VFI converged in ", iter, " iterations."
                    print '(A,E12.5)', "  Final metric: ", metric
                    exit
                end if

            else
                !---------------------------------------------------------------
                ! POLICY EVALUATION STEPS (cheap) - do multiple
                !---------------------------------------------------------------
                do eval_iter = 1, howard_eval_steps

                    call policy_evaluation_step()

                    metric_eval = maxval(abs(V_new - V))

                    ! Full update during policy evaluation (no dampening needed
                    ! since policy is fixed - we're just solving for V given policy)
                    V = V_new

                    ! Early exit if evaluation converged
                    if (metric_eval < tol_VFI * 0.1_dp) exit

                end do

                if (print_progress .and. mod(iter, print_freq) == 0) then
                    print '(A,I5,A,I3,A,E12.5)', "  Iter ", iter, &
                        ": Policy EVALUATION (", eval_iter, " steps), metric = ", metric_eval
                end if

                ! Check overall convergence
                if (metric_eval < tol_VFI) then
                    print *, ""
                    print '(A,I5,A)', "  VFI converged in ", iter, " iterations."
                    print '(A,E12.5)', "  Final metric: ", metric_eval
                    exit
                end if

            end if

            if (iter == maxiter_VFI) then
                print *, ""
                print *, "  WARNING: VFI did not converge!"
                print '(A,E12.5)', "  Final metric: ", metric
            end if

        end do  ! Main iteration

    end subroutine solve_firm_problem

end module mod_firm_problem
