!===================================================================================
! MODULE: mod_firm_problem
!
! DESCRIPTION:
!   Solves the firm's optimization problem via value function iteration
!   with Howard's Policy Improvement Algorithm and computational optimizations.
!
!   Firm state: (z, K, S, D_{-1})
!   Firm choices: (I^K, H^R, D) subject to collateral constraint
!
!   COMPUTATIONAL OPTIMIZATIONS:
!     1. OpenMP parallelization of state space loops
!     2. Local search around previous optimal policy (±local_search_radius)
!     3. Precomputed static labor solutions (depend only on z,K,S, not D)
!     4. Howard's policy iteration (policy evaluation between improvements)
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_firm_problem
    use mod_parameters
    use mod_globals
    use mod_utility
    use mod_interpolation
    !$ use omp_lib
    implicit none

    ! Local search radius (search ± this many grid points around previous optimum)
    integer, parameter :: local_search_radius = 3

contains

    !===================================================================================
    ! SUBROUTINE: initialize_value_function
    !
    ! DESCRIPTION:
    !   Initializes the value function with approximate stationary values.
    !   This breaks the "cold start" trap where V=0 makes all future investments
    !   appear worthless, leading to zero R&D from the first iteration.
    !
    !   For each state (z, K, S, D), we compute the approximate perpetuity value
    !   of staying at that state forever with no investment:
    !     V ≈ profit / (1 - β(1-ζ))
    !   where profit = Y - wL*L - wH*HP - R*D
    !===================================================================================
    subroutine initialize_value_function()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_val
        real(dp) :: L_init, HP_init, Y_init, profit_init
        real(dp) :: discount_factor, V_init

        print *, "  Initializing value function with stationary approximation..."

        ! Discount factor for perpetuity (accounts for exit)
        discount_factor = 1.0_dp / (1.0_dp - beta * (1.0_dp - zeta))

        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, z_val, K_val, S_val, D_val, &
        !$OMP&         L_init, HP_init, Y_init, profit_init, V_init) &
        !$OMP& SCHEDULE(dynamic)
        do iz = 1, nz
            do iK = 1, nK
                z_val = grid_z(iz)
                K_val = grid_K(iK)

                do iS = 1, nS
                    S_val = grid_S(iS)

                    ! Compute optimal static labor and output at this state
                    call solve_static_labor(z_val, K_val, S_val, wL, wH, &
                                           L_init, HP_init, Y_init)

                    do iD = 1, nD
                        D_val = grid_D(iD)

                        ! Compute profit net of debt service
                        profit_init = Y_init - wL * L_init - wH * HP_init - R * D_val

                        ! Approximate value as perpetuity of profits
                        ! Bound below to avoid very negative values for high debt states
                        V_init = max(profit_init * discount_factor, 0.0_dp)

                        V(iz, iK, iS, iD) = V_init
                        V_new(iz, iK, iS, iD) = V_init

                    end do
                end do
            end do
        end do
        !$OMP END PARALLEL DO

        print *, "  Value function initialized."
        print '(A,F12.4,A,F12.4)', "    V range: [", minval(V), ", ", maxval(V), "]"

    end subroutine initialize_value_function

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
    ! SUBROUTINE: precompute_static_labor
    !
    ! DESCRIPTION:
    !   Precomputes optimal static labor choices for all (z,K,S) combinations.
    !   Since static labor only depends on (z,K,S) and wages (not debt D),
    !   we can compute these once and reuse for all D values.
    !   This reduces redundant computation by a factor of nD.
    !===================================================================================
    subroutine precompute_static_labor()
        implicit none
        integer :: iz, iK, iS
        real(dp) :: z_val, K_val, S_val
        real(dp) :: L_opt, HP_opt, Y_opt

        !$OMP PARALLEL DO PRIVATE(iz, iK, iS, z_val, K_val, S_val, L_opt, HP_opt, Y_opt) &
        !$OMP& SCHEDULE(dynamic)
        do iz = 1, nz
            z_val = grid_z(iz)
            do iK = 1, nK
                K_val = grid_K(iK)
                do iS = 1, nS
                    S_val = grid_S(iS)

                    call solve_static_labor(z_val, K_val, S_val, wL, wH, &
                                           L_opt, HP_opt, Y_opt)

                    static_L(iz, iK, iS) = L_opt
                    static_HP(iz, iK, iS) = HP_opt
                    static_Y(iz, iK, iS) = Y_opt
                    static_Pi(iz, iK, iS) = Y_opt - wL * L_opt - wH * HP_opt

                end do
            end do
        end do
        !$OMP END PARALLEL DO

    end subroutine precompute_static_labor

    !===================================================================================
    ! SUBROUTINE: policy_improvement_step
    !
    ! DESCRIPTION:
    !   Full policy optimization step with local search optimization.
    !   For each state, searches over choice combinations near previous optimum.
    !   Uses OpenMP for parallelization over the state space.
    !
    !   LOCAL SEARCH: Instead of searching all nIK*nHR*nDprime combinations,
    !   searches only ±local_search_radius around previous optimal indices.
    !   On first iteration (no previous policy), searches full grid.
    !===================================================================================
    subroutine policy_improvement_step(first_iteration)
        implicit none
        logical, intent(in) :: first_iteration
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: V_best, V_try
        integer :: iIK, iHR, iDp
        integer :: iIK_lo, iIK_hi, iHR_lo, iHR_hi, iDp_lo, iDp_hi
        integer :: best_iIK, best_iHR, best_iDp
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: resources, expenses, dividends
        real(dp) :: D_max_coll, EV
        real(dp) :: AC_K_val, AC_S_val

        ! First, precompute static labor solutions
        call precompute_static_labor()

        ! Loop over state space with OpenMP parallelization
        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, z_val, K_val, S_val, D_old_val, &
        !$OMP&         L_opt, HP_opt, Y_val, Pi_gross, V_best, V_try, &
        !$OMP&         iIK, iHR, iDp, iIK_lo, iIK_hi, iHR_lo, iHR_hi, iDp_lo, iDp_hi, &
        !$OMP&         best_iIK, best_iHR, best_iDp, IK_choice, HR_choice, Dp_choice, &
        !$OMP&         Kprime, Sprime, inv_S, resources, expenses, dividends, &
        !$OMP&         D_max_coll, EV, AC_K_val, AC_S_val) &
        !$OMP& SCHEDULE(dynamic)
        do iz = 1, nz
            do iK = 1, nK
                z_val = grid_z(iz)
                K_val = grid_K(iK)

                do iS = 1, nS
                    S_val = grid_S(iS)

                    ! Use precomputed static labor solutions
                    L_opt = static_L(iz, iK, iS)
                    HP_opt = static_HP(iz, iK, iS)
                    Y_val = static_Y(iz, iK, iS)
                    Pi_gross = static_Pi(iz, iK, iS)

                    ! Note: Collateral constraint now based on NEXT period capital (K', S')
                    ! This is computed inside the loop after K' and S' are determined
                    ! Lenders care about collateral value at repayment time

                    do iD = 1, nD
                        D_old_val = grid_D(iD)

                        ! Determine search bounds (local search or full grid)
                        if (first_iteration) then
                            ! Full grid search on first iteration
                            iDp_lo = 1
                            iDp_hi = nDprime
                            iIK_lo = 1
                            iIK_hi = nIK
                            iHR_lo = 1
                            iHR_hi = nHR
                        else
                            ! Local search around previous optimum
                            iDp_lo = max(1, pol_iDp(iz, iK, iS, iD) - local_search_radius)
                            iDp_hi = min(nDprime, pol_iDp(iz, iK, iS, iD) + local_search_radius)
                            iIK_lo = max(1, pol_iIK(iz, iK, iS, iD) - local_search_radius)
                            iIK_hi = min(nIK, pol_iIK(iz, iK, iS, iD) + local_search_radius)
                            iHR_lo = max(1, pol_iHR(iz, iK, iS, iD) - local_search_radius)
                            iHR_hi = min(nHR, pol_iHR(iz, iK, iS, iD) + local_search_radius)
                        end if

                        ! Initialize best value and indices
                        V_best = -1.0e10_dp
                        best_iIK = iIK_lo
                        best_iHR = iHR_lo
                        best_iDp = iDp_lo

                        ! Grid search over (I^K, H^R, D') - reordered so collateral
                        ! constraint can use next-period capital (K', S')
                        do iIK = iIK_lo, iIK_hi
                            IK_choice = grid_IK(iIK)

                            ! Check that K' stays within grid bounds
                            Kprime = (1.0_dp - delta_K) * K_val + IK_choice
                            if (Kprime < K_min .or. Kprime > K_max) cycle

                            do iHR = iHR_lo, iHR_hi
                                HR_choice = grid_HR(iHR)

                                inv_S = RD_production(HR_choice)
                                Sprime = (1.0_dp - delta_S) * S_val + inv_S
                                ! Allow S to go below S_min but impose a small floor
                                Sprime = max(Sprime, 1.0e-6_dp)

                                ! Collateral constraint based on NEXT period capital
                                D_max_coll = collateral_constraint(Kprime, Sprime)

                                do iDp = iDp_lo, iDp_hi
                                    Dp_choice = grid_Dprime(iDp)

                                    ! Check collateral constraint (now based on K', S')
                                    if (Dp_choice > D_max_coll + epsilon) cycle

                                    ! Compute adjustment costs (0 if phi_K=0 or phi_S=0)
                                    AC_K_val = adjustment_cost_K(IK_choice, K_val)
                                    AC_S_val = adjustment_cost_S(inv_S, S_val)

                                    resources = Pi_gross - R * D_old_val + Dp_choice
                                    expenses = IK_choice + wH * HR_choice + AC_K_val + AC_S_val
                                    dividends = resources - expenses

                                    if (dividends < -epsilon) cycle

                                    EV = expect_V(iz, Kprime, Sprime, Dp_choice)
                                    V_try = dividends + beta * (1.0_dp - zeta) * EV

                                    if (V_try > V_best) then
                                        V_best = V_try
                                        best_iIK = iIK
                                        best_iHR = iHR
                                        best_iDp = iDp
                                        pol_IK(iz, iK, iS, iD) = IK_choice
                                        pol_HR(iz, iK, iS, iD) = HR_choice
                                        pol_Dprime(iz, iK, iS, iD) = Dp_choice
                                        pol_Kprime(iz, iK, iS, iD) = Kprime
                                        pol_Sprime(iz, iK, iS, iD) = Sprime
                                        pol_L(iz, iK, iS, iD) = L_opt
                                        pol_HP(iz, iK, iS, iD) = HP_opt
                                        pol_Y(iz, iK, iS, iD) = Y_val
                                        ! Constrained if borrowing at or near collateral limit
                                        pol_constr(iz, iK, iS, iD) = &
                                            (Dp_choice >= D_max_coll - 0.01_dp * max(1.0_dp, D_max_coll))
                                    end if
                                end do  ! Dp
                            end do  ! HR
                        end do  ! IK

                        ! Store optimal indices for next iteration's local search
                        pol_iIK(iz, iK, iS, iD) = best_iIK
                        pol_iHR(iz, iK, iS, iD) = best_iHR
                        pol_iDp(iz, iK, iS, iD) = best_iDp

                        V_new(iz, iK, iS, iD) = V_best

                    end do  ! iD
                end do  ! iS
            end do  ! iK
        end do  ! iz
        !$OMP END PARALLEL DO

    end subroutine policy_improvement_step

    !===================================================================================
    ! SUBROUTINE: policy_evaluation_step
    !
    ! DESCRIPTION:
    !   Policy evaluation step (cheap).
    !   Updates V using FIXED policy functions - no optimization search.
    !   This is O(state_space) vs O(state_space * choice_space).
    !   Uses OpenMP for parallelization.
    !===================================================================================
    subroutine policy_evaluation_step()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: D_old_val, K_val, S_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: resources, expenses, dividends
        real(dp) :: EV, V_eval
        real(dp) :: AC_K_val, AC_S_val

        ! Loop over state space with OpenMP - using FIXED policies
        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, D_old_val, K_val, S_val, L_opt, HP_opt, Y_val, Pi_gross, &
        !$OMP&         IK_choice, HR_choice, Dp_choice, Kprime, Sprime, inv_S, &
        !$OMP&         resources, expenses, dividends, EV, V_eval, AC_K_val, AC_S_val) &
        !$OMP& SCHEDULE(dynamic)
        do iz = 1, nz
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
                        Sprime = max(pol_Sprime(iz, iK, iS, iD), 1.0e-6_dp)  ! Safety floor

                        ! Compute intangible investment from R&D labor (for adjustment costs)
                        inv_S = RD_production(HR_choice)

                        ! Compute adjustment costs (0 if phi_K=0 or phi_S=0)
                        AC_K_val = adjustment_cost_K(IK_choice, K_val)
                        AC_S_val = adjustment_cost_S(inv_S, S_val)

                        ! Compute value with fixed policy
                        Pi_gross = Y_val - wL * L_opt - wH * HP_opt
                        resources = Pi_gross - R * D_old_val + Dp_choice
                        expenses = IK_choice + wH * HR_choice + AC_K_val + AC_S_val
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
        !$OMP END PARALLEL DO

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
        integer :: total_states, total_choices
        logical :: do_improvement, first_improvement
        integer :: num_threads

        print *, ""
        print *, "======================================"
        print *, "SOLVING FIRM PROBLEM (VFI + HOWARD)"
        print *, "======================================"

        total_states = nz * nK * nS * nD
        total_choices = nIK * nHR * nDprime
        print '(A,I8)', "  Total state space points: ", total_states
        print '(A,I8)', "  Choice combinations (full): ", total_choices
        print '(A,I8)', "  Choice combinations (local): ", (2*local_search_radius+1)**3
        print '(A,I4)', "  Howard improvement frequency: ", howard_freq
        print '(A,I4)', "  Howard evaluation steps: ", howard_eval_steps

        ! Report OpenMP thread count
        num_threads = 1
        !$ num_threads = omp_get_max_threads()
        print '(A,I4)', "  OpenMP threads: ", num_threads
        print *, ""

        ! Initialize value function with approximate stationary values
        ! This avoids the "cold start" trap where V=0 makes R&D worthless
        call initialize_value_function()

        ! Initialize policies
        pol_IK = 0.0_dp
        pol_HR = 0.0_dp
        pol_Dprime = 0.0_dp
        pol_Kprime = K_min
        pol_Sprime = S_min
        pol_L = 0.1_dp
        pol_HP = 0.1_dp
        pol_Y = 0.0_dp

        ! Initialize policy indices to middle of grids
        pol_iIK = nIK / 2
        pol_iHR = nHR / 2
        pol_iDp = nDprime / 2

        first_improvement = .true.

        ! Main iteration loop
        do iter = 1, maxiter_VFI

            ! Decide: Policy Improvement or Policy Evaluation?
            do_improvement = (iter == 1) .or. (mod(iter-1, howard_freq) == 0)

            if (do_improvement) then
                !---------------------------------------------------------------
                ! POLICY IMPROVEMENT STEP (expensive)
                !---------------------------------------------------------------
                print '(A,I5,A)', "  Iter ", iter, ": Policy IMPROVEMENT (full optimization)..."

                call policy_improvement_step(first_improvement)
                first_improvement = .false.

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
