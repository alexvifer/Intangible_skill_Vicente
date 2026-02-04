!===================================================================================
! MODULE: mod_firm_problem
!
! DESCRIPTION:
!   Solves the firm's optimization problem via value function iteration
!   with Howard's Policy Improvement Algorithm and computational optimizations.
!
!   BELLMAN EQUATION:
!     V(z,K,S,D) = zeta * exit_val + (1-zeta) * max(W, 0)
!   where:
!     exit_val = max(Pi + (1-dK)*K + (1-dS)*S - R*D, 0)  [exit: sell depreciated capital]
!     W = max_{IK,HR} { div + beta * E[V'] }  [continuation value]
!     div = Pi - R*D + D' - IK - wH*HR - AC   [dividends, must be >= 0]
!     D' determined analytically (Khan & Thomas 2013 style)
!
!   EXIT TIMING: After production, before investment. Exiting firms sell
!   depreciated tangible and intangible capital, repay debt R*D.
!
!   ANALYTICAL D' (3 regions, no grid search):
!     Type A (mu=0, lambda=0): D'=0, div>0      -- unconstrained (absorbing)
!     Type B (mu>0, lambda=0): D'=D_needed, div=0 -- potentially constrained
!     Type C (mu>0, lambda>0): D'=D_ub, div=0    -- actually constrained
!     Infeasible: D_needed > D_ub, skip this (IK,HR)
!
!   COMPUTATIONAL OPTIMIZATIONS:
!     1. OpenMP parallelization of state space loops
!     2. Local search around previous optimal policy
!     3. Precomputed static labor solutions (depend only on z,K,S, not D)
!     4. Howard's policy iteration (policy evaluation between improvements)
!     5. Analytical D' eliminates inner loop (1 eval per IK,HR instead of nDp)
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

    ! Local search radius (search +/- this many grid points around previous optimum)
    integer, parameter :: local_search_radius = 5

    ! Frequency of full grid search reset (to escape local optima)
    ! Every full_search_freq policy improvement iterations, do a full grid search
    integer, parameter :: full_search_freq = 10

contains

    !===================================================================================
    ! SUBROUTINE: initialize_value_function
    !
    ! DESCRIPTION:
    !   Initializes the value function with approximate stationary values.
    !   Accounts for the new Bellman structure:
    !     V = zeta * exit_val + (1-zeta) * continuation
    !   Uses perpetuity approximation for continuation value.
    !===================================================================================
    subroutine initialize_value_function()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_val
        real(dp) :: L_init, HP_init, Y_init, Pi_init
        real(dp) :: discount_factor, exit_val, cont_val, V_init

        print *, "  Initializing value function with stationary approximation..."

        ! Discount factor for perpetuity (accounts for exit)
        discount_factor = 1.0_dp / (1.0_dp - beta * (1.0_dp - zeta))

        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, z_val, K_val, S_val, D_val, &
        !$OMP&         L_init, HP_init, Y_init, Pi_init, &
        !$OMP&         exit_val, cont_val, V_init) &
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
                    Pi_init = Y_init - wL * L_init - wH * HP_init

                    do iD = 1, nD
                        D_val = grid_D(iD)

                        ! Exit value: sell undepreciated capital, repay debt
                        exit_val = max(Pi_init + (1.0_dp - delta_K) * K_val &
                                       + (1.0_dp - delta_S) * S_val - R * D_val, 0.0_dp)

                        ! Continuation approximation: perpetuity of net profits
                        cont_val = max(Pi_init - (R - 1.0_dp) * D_val, 0.0_dp) * discount_factor

                        ! V = zeta * exit_val + (1-zeta) * continuation
                        V_init = zeta * exit_val + (1.0_dp - zeta) * cont_val

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
    !   FOCs: dY/dL = wL,  dY/dHP = wH
    !   Uses nested bisection for robustness.
    !===================================================================================
    subroutine solve_static_labor(z, K, S, wL_in, wH_in, L_opt, HP_opt, Y_opt)
        implicit none
        real(dp), intent(in) :: z, K, S, wL_in, wH_in
        real(dp), intent(out) :: L_opt, HP_opt, Y_opt
        real(dp) :: L_lo, L_hi, L_mid, HP_star
        real(dp) :: MPL_lo, MPL_hi, MPL_mid
        real(dp) :: tol_labor
        integer :: iter_L, maxiter_bisect

        tol_labor = 1.0e-6_dp
        maxiter_bisect = 50

        ! Bounds for L (unskilled labor)
        L_lo = 0.001_dp
        L_hi = 10.0_dp

        ! Check that bounds bracket the solution
        call solve_HP_given_L(z, K, S, L_lo, wH_in, HP_star)
        MPL_lo = marginal_product_L(z, K, S, L_lo, HP_star)

        call solve_HP_given_L(z, K, S, L_hi, wH_in, HP_star)
        MPL_hi = marginal_product_L(z, K, S, L_hi, HP_star)

        ! Handle edge cases where solution is at boundary
        if (MPL_lo <= wL_in) then
            L_opt = L_lo
            call solve_HP_given_L(z, K, S, L_opt, wH_in, HP_opt)
            Y_opt = production_Y(z, K, S, L_opt, HP_opt)
            return
        end if

        if (MPL_hi >= wL_in) then
            L_opt = L_hi
            call solve_HP_given_L(z, K, S, L_opt, wH_in, HP_opt)
            Y_opt = production_Y(z, K, S, L_opt, HP_opt)
            return
        end if

        ! Bisection on L
        do iter_L = 1, maxiter_bisect
            L_mid = 0.5_dp * (L_lo + L_hi)

            ! For this L, find optimal HP
            call solve_HP_given_L(z, K, S, L_mid, wH_in, HP_star)

            ! Evaluate MPL at (L_mid, HP_star)
            MPL_mid = marginal_product_L(z, K, S, L_mid, HP_star)

            if (abs(MPL_mid - wL_in) < tol_labor * wL_in .or. (L_hi - L_lo) < tol_labor) then
                L_opt = L_mid
                HP_opt = HP_star
                Y_opt = production_Y(z, K, S, L_opt, HP_opt)
                return
            end if

            if (MPL_mid > wL_in) then
                L_lo = L_mid
            else
                L_hi = L_mid
            end if
        end do

        ! Return best guess if not converged
        L_opt = L_mid
        HP_opt = HP_star
        Y_opt = production_Y(z, K, S, L_opt, HP_opt)

    end subroutine solve_static_labor

    !===================================================================================
    ! SUBROUTINE: solve_HP_given_L
    !
    ! DESCRIPTION:
    !   Given L, solves for HP such that MPH(L,HP) = wH using bisection.
    !===================================================================================
    subroutine solve_HP_given_L(z, K, S, L_val, wH_in, HP_opt)
        implicit none
        real(dp), intent(in) :: z, K, S, L_val, wH_in
        real(dp), intent(out) :: HP_opt
        real(dp) :: HP_lo, HP_hi, HP_mid
        real(dp) :: MPH_lo, MPH_hi, MPH_mid
        real(dp) :: tol_labor
        integer :: iter, maxiter_bisect

        tol_labor = 1.0e-6_dp
        maxiter_bisect = 40

        HP_lo = 0.001_dp
        HP_hi = 5.0_dp

        MPH_lo = marginal_product_HP(z, K, S, L_val, HP_lo)
        MPH_hi = marginal_product_HP(z, K, S, L_val, HP_hi)

        if (MPH_lo <= wH_in .or. isnan(MPH_lo)) then
            HP_opt = HP_lo
            return
        end if

        if (MPH_hi >= wH_in) then
            HP_opt = HP_hi
            return
        end if

        do iter = 1, maxiter_bisect
            HP_mid = 0.5_dp * (HP_lo + HP_hi)
            MPH_mid = marginal_product_HP(z, K, S, L_val, HP_mid)

            if (isnan(MPH_mid)) then
                HP_lo = HP_mid
                cycle
            end if

            if (abs(MPH_mid - wH_in) < tol_labor * wH_in .or. (HP_hi - HP_lo) < tol_labor) then
                HP_opt = HP_mid
                return
            end if

            if (MPH_mid > wH_in) then
                HP_lo = HP_mid
            else
                HP_hi = HP_mid
            end if
        end do

        HP_opt = HP_mid

    end subroutine solve_HP_given_L

    !===================================================================================
    ! SUBROUTINE: precompute_static_labor
    !
    ! DESCRIPTION:
    !   Precomputes optimal static labor choices for all (z,K,S) combinations.
    !   Since static labor only depends on (z,K,S) and wages (not debt D),
    !   we can compute these once and reuse for all D values.
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
    !   Full policy optimization step with local search.
    !
    !   NEW BELLMAN STRUCTURE:
    !     V(z,K,S,D) = zeta * exit_val + (1-zeta) * max(W_best, 0)
    !   where exit_val = max(Pi + (1-dK)*K + (1-dS)*S - R*D, 0)  [state-dependent]
    !   and W_best = max over feasible (IK,HR) of { div + beta * E[V'] }
    !
    !   EXIT TIMING: After production, before investment.
    !   Exiting firms sell undepreciated tangible (K) and intangible (S) capital,
    !   repay R*D. Shareholders get residual under limited liability.
    !
    !   ANALYTICAL D' (no grid search needed):
    !     cash_flow = Pi - R*D  (available after production and debt repayment)
    !     D_needed = expenses - cash_flow  (how much borrowing needed)
    !     If D_needed <= 0: D'=0, div=-D_needed (Type A: unconstrained)
    !     If 0 < D_needed <= D_ub: D'=D_needed, div=0 (Type B or C)
    !     If D_needed > D_ub: infeasible, skip this (IK,HR)
    !
    !   With beta*R = 1 and exit repayment, over-borrowing (D' > D_needed)
    !   is never optimal: each extra dollar of debt costs zeta*R in exit states.
    !   So D' = max(D_needed, 0) is the unique analytical solution.
    !===================================================================================
    subroutine policy_improvement_step(first_iteration)
        implicit none
        logical, intent(in) :: first_iteration
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: W_best, W_try, exit_val
        integer :: iIK, iHR
        integer :: iIK_lo, iIK_hi, iHR_lo, iHR_hi
        integer :: best_iIK, best_iHR
        real(dp) :: IK_choice, HR_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: cash_flow, expenses, D_needed, Dp_opt, dividends
        real(dp) :: D_max_coll, D_ub, EV
        real(dp) :: AC_K_val, AC_S_val
        logical :: best_mu_binding, best_lambda_binding
        ! Variables for marginal constraint check (Type C detection)
        real(dp) :: IK_test, HR_test, Kp_test, Sp_test, inv_S_test
        real(dp) :: AC_K_test, AC_S_test, expenses_test, D_needed_test, D_ub_test

        ! Precompute expected value grid and static labor
        call precompute_EV_grid()
        call precompute_static_labor()

        ! Loop over state space with OpenMP parallelization
        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, z_val, K_val, S_val, D_old_val, &
        !$OMP&         L_opt, HP_opt, Y_val, Pi_gross, W_best, W_try, exit_val, &
        !$OMP&         iIK, iHR, iIK_lo, iIK_hi, iHR_lo, iHR_hi, &
        !$OMP&         best_iIK, best_iHR, IK_choice, HR_choice, &
        !$OMP&         Kprime, Sprime, inv_S, cash_flow, expenses, &
        !$OMP&         D_needed, Dp_opt, dividends, &
        !$OMP&         D_max_coll, D_ub, EV, AC_K_val, AC_S_val, &
        !$OMP&         best_mu_binding, best_lambda_binding, &
        !$OMP&         IK_test, HR_test, Kp_test, Sp_test, inv_S_test, &
        !$OMP&         AC_K_test, AC_S_test, expenses_test, D_needed_test, D_ub_test) &
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

                    do iD = 1, nD
                        D_old_val = grid_D(iD)

                        ! ==========================================================
                        ! EXIT VALUE (depends on current state, not choices)
                        ! Exit happens after production, before investment.
                        ! Firm sells undepreciated K and S, repays R*D.
                        ! ==========================================================
                        exit_val = max(Pi_gross + (1.0_dp - delta_K) * K_val &
                                       + (1.0_dp - delta_S) * S_val - R * D_old_val, 0.0_dp)

                        ! Cash flow available for investment
                        cash_flow = Pi_gross - R * D_old_val

                        ! Determine search bounds (local search or full grid)
                        if (first_iteration) then
                            iIK_lo = 1
                            iIK_hi = nIK
                            iHR_lo = 1
                            iHR_hi = nHR
                        else
                            iIK_lo = max(1, pol_iIK(iz, iK, iS, iD) - local_search_radius)
                            iIK_hi = min(nIK, pol_iIK(iz, iK, iS, iD) + local_search_radius)
                            iHR_lo = max(1, pol_iHR(iz, iK, iS, iD) - local_search_radius)
                            iHR_hi = min(nHR, pol_iHR(iz, iK, iS, iD) + local_search_radius)
                        end if

                        ! Initialize: sentinel below limited liability floor
                        W_best = -1.0e10_dp
                        best_iIK = iIK_lo
                        best_iHR = iHR_lo
                        best_mu_binding = .false.
                        best_lambda_binding = .false.

                        ! ==========================================================
                        ! Grid search over (I^K, H^R) with ANALYTICAL D'
                        ! ==========================================================
                        do iIK = iIK_lo, iIK_hi
                            IK_choice = grid_IK(iIK)

                            ! K' = (1-delta_K)*K + IK
                            Kprime = (1.0_dp - delta_K) * K_val + IK_choice
                            if (Kprime < K_min .or. Kprime > K_max) cycle

                            do iHR = iHR_lo, iHR_hi
                                HR_choice = grid_HR(iHR)

                                inv_S = RD_production(HR_choice)
                                Sprime = (1.0_dp - delta_S) * S_val + inv_S
                                Sprime = max(Sprime, 1.0e-6_dp)

                                ! S' bounds check
                                if (Sprime > S_max) cycle

                                ! Collateral constraint: D' <= alpha_K*K' + alpha_S*S'
                                if (remove_collateral_constraint) then
                                    D_max_coll = D_max
                                else
                                    D_max_coll = collateral_constraint(Kprime, Sprime)
                                end if

                                ! Adjustment costs
                                AC_K_val = adjustment_cost_K(IK_choice, K_val)
                                AC_S_val = adjustment_cost_S(inv_S, S_val)

                                ! Total investment expenses
                                expenses = IK_choice + wH * HR_choice + AC_K_val + AC_S_val

                                ! D' upper bound (can't exceed grid boundary)
                                D_ub = min(D_max_coll, D_max)

                                ! ======================================================
                                ! ANALYTICAL D' DETERMINATION
                                ! D_needed = how much the firm needs to borrow
                                ! With beta*R = 1, over-borrowing is never optimal
                                ! (exit repayment creates cost zeta*R per dollar)
                                ! ======================================================
                                D_needed = expenses - cash_flow

                                if (D_needed <= 0.0_dp) then
                                    ! TYPE A: Unconstrained (mu=0, lambda=0)
                                    ! Firm has enough cash, no borrowing needed
                                    Dp_opt = 0.0_dp
                                    dividends = -D_needed  ! = cash_flow - expenses > 0

                                else if (D_needed <= D_ub) then
                                    ! TYPE B/C: Borrow exactly what's needed (div = 0)
                                    ! B: D' < D_ub (mu>0, lambda=0)
                                    ! C: D' = D_ub (mu>0, lambda>0)
                                    Dp_opt = D_needed
                                    dividends = 0.0_dp

                                else
                                    ! INFEASIBLE: D_needed > D_ub
                                    ! Even max borrowing can't cover expenses
                                    cycle
                                end if

                                ! Continuation value: E[V'(z', K', S', D')]
                                EV = expect_V(iz, Kprime, Sprime, Dp_opt)

                                ! W = div + beta * E[V']
                                W_try = dividends + beta * EV

                                if (W_try > W_best) then
                                    W_best = W_try
                                    best_iIK = iIK
                                    best_iHR = iHR
                                    pol_IK(iz, iK, iS, iD) = IK_choice
                                    pol_HR(iz, iK, iS, iD) = HR_choice
                                    pol_Dprime(iz, iK, iS, iD) = Dp_opt
                                    pol_Kprime(iz, iK, iS, iD) = Kprime
                                    pol_Sprime(iz, iK, iS, iD) = Sprime
                                    pol_L(iz, iK, iS, iD) = L_opt
                                    pol_HP(iz, iK, iS, iD) = HP_opt
                                    pol_Y(iz, iK, iS, iD) = Y_val

                                    ! Determine constraint status
                                    if (D_needed <= 0.0_dp) then
                                        best_mu_binding = .false.
                                        best_lambda_binding = .false.
                                    else if (D_needed < D_ub - epsilon) then
                                        best_mu_binding = .true.
                                        best_lambda_binding = .false.
                                    else
                                        best_mu_binding = .true.
                                        best_lambda_binding = .true.
                                    end if
                                end if

                            end do  ! HR
                        end do  ! IK

                        ! ==========================================================
                        ! MARGINAL CONSTRAINT CHECK (Type C detection)
                        ! On a discrete grid, D_needed jumps across D_ub so the
                        ! exact D_needed = D_ub condition never triggers. Instead,
                        ! check if increasing IK or HR by one grid step would make
                        ! the choice infeasible. If so, the collateral constraint
                        ! is marginally binding (Type C).
                        ! ==========================================================
                        if (best_mu_binding .and. (.not. best_lambda_binding) &
                            .and. W_best > -1.0e9_dp) then

                            ! Check IK+1 direction
                            if (best_iIK < nIK) then
                                IK_test = grid_IK(best_iIK + 1)
                                Kp_test = (1.0_dp - delta_K) * K_val + IK_test
                                HR_test = grid_HR(best_iHR)
                                inv_S_test = RD_production(HR_test)
                                Sp_test = max((1.0_dp - delta_S) * S_val + inv_S_test, 1.0e-6_dp)

                                AC_K_test = adjustment_cost_K(IK_test, K_val)
                                AC_S_test = adjustment_cost_S(inv_S_test, S_val)
                                expenses_test = IK_test + wH * HR_test + AC_K_test + AC_S_test
                                D_needed_test = expenses_test - cash_flow

                                if (remove_collateral_constraint) then
                                    D_ub_test = D_max
                                else
                                    D_ub_test = min(collateral_constraint(Kp_test, Sp_test), D_max)
                                end if

                                if (D_needed_test > D_ub_test) then
                                    best_lambda_binding = .true.
                                end if
                            end if

                            ! Check HR+1 direction (only if not already classified as Type C)
                            if ((.not. best_lambda_binding) .and. best_iHR < nHR) then
                                IK_test = grid_IK(best_iIK)
                                Kp_test = (1.0_dp - delta_K) * K_val + IK_test
                                HR_test = grid_HR(best_iHR + 1)
                                inv_S_test = RD_production(HR_test)
                                Sp_test = max((1.0_dp - delta_S) * S_val + inv_S_test, 1.0e-6_dp)

                                AC_K_test = adjustment_cost_K(IK_test, K_val)
                                AC_S_test = adjustment_cost_S(inv_S_test, S_val)
                                expenses_test = IK_test + wH * HR_test + AC_K_test + AC_S_test
                                D_needed_test = expenses_test - cash_flow

                                if (remove_collateral_constraint) then
                                    D_ub_test = D_max
                                else
                                    D_ub_test = min(collateral_constraint(Kp_test, Sp_test), D_max)
                                end if

                                if (D_needed_test > D_ub_test) then
                                    best_lambda_binding = .true.
                                end if
                            end if

                        end if

                        ! ==========================================================
                        ! COMBINE EXIT VALUE AND CONTINUATION VALUE
                        ! V = zeta * exit_val + (1-zeta) * max(W_best, 0)
                        ! ==========================================================
                        if (W_best < -1.0e9_dp) then
                            ! No feasible investment choice
                            V_new(iz, iK, iS, iD) = zeta * exit_val
                            pol_IK(iz, iK, iS, iD) = 0.0_dp
                            pol_HR(iz, iK, iS, iD) = 0.0_dp
                            pol_Dprime(iz, iK, iS, iD) = 0.0_dp
                            pol_Kprime(iz, iK, iS, iD) = max((1.0_dp - delta_K) * K_val, K_min)
                            pol_Sprime(iz, iK, iS, iD) = max((1.0_dp - delta_S) * S_val, S_min)
                            pol_L(iz, iK, iS, iD) = L_opt
                            pol_HP(iz, iK, iS, iD) = HP_opt
                            pol_Y(iz, iK, iS, iD) = Y_val
                            best_mu_binding = .false.
                            best_lambda_binding = .false.
                        else
                            V_new(iz, iK, iS, iD) = zeta * exit_val &
                                + (1.0_dp - zeta) * max(W_best, 0.0_dp)
                        end if

                        ! ==========================================================
                        ! CONSTRAINT STATUS (Khan & Thomas classification)
                        ! mu > 0: dividend constraint binds (div = 0)
                        ! lambda > 0: collateral constraint binds (D' = D_ub)
                        ! ==========================================================
                        if (best_mu_binding) then
                            pol_mu(iz, iK, iS, iD) = 1.0_dp
                        else
                            pol_mu(iz, iK, iS, iD) = 0.0_dp
                        end if

                        if (best_lambda_binding) then
                            pol_lambda(iz, iK, iS, iD) = 1.0_dp
                        else
                            pol_lambda(iz, iK, iS, iD) = 0.0_dp
                        end if

                        pol_constr(iz, iK, iS, iD) = best_lambda_binding

                        ! Store optimal indices for next iteration's local search
                        pol_iIK(iz, iK, iS, iD) = best_iIK
                        pol_iHR(iz, iK, iS, iD) = best_iHR

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
    !   Uses the new Bellman: V = zeta * exit_val + (1-zeta) * max(W, 0)
    !===================================================================================
    subroutine policy_evaluation_step()
        implicit none
        integer :: iz, iK, iS, iD
        real(dp) :: D_old_val, K_val, S_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: expenses, dividends
        real(dp) :: EV, W_eval, exit_val
        real(dp) :: AC_K_val, AC_S_val

        ! Precompute expected value grid from current V
        call precompute_EV_grid()

        ! Loop over state space with OpenMP - using FIXED policies
        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, D_old_val, K_val, S_val, &
        !$OMP&         L_opt, HP_opt, Y_val, Pi_gross, &
        !$OMP&         IK_choice, HR_choice, Dp_choice, Kprime, Sprime, inv_S, &
        !$OMP&         expenses, dividends, EV, W_eval, exit_val, &
        !$OMP&         AC_K_val, AC_S_val) &
        !$OMP& SCHEDULE(dynamic)
        do iz = 1, nz
            do iK = 1, nK
                K_val = grid_K(iK)

                do iS = 1, nS
                    S_val = grid_S(iS)

                    do iD = 1, nD
                        D_old_val = grid_D(iD)

                        ! Use stored policy functions (no optimization!)
                        L_opt = pol_L(iz, iK, iS, iD)
                        HP_opt = pol_HP(iz, iK, iS, iD)
                        Y_val = pol_Y(iz, iK, iS, iD)
                        IK_choice = pol_IK(iz, iK, iS, iD)
                        HR_choice = pol_HR(iz, iK, iS, iD)
                        Dp_choice = pol_Dprime(iz, iK, iS, iD)
                        Kprime = pol_Kprime(iz, iK, iS, iD)
                        Sprime = max(pol_Sprime(iz, iK, iS, iD), 1.0e-6_dp)

                        ! Compute gross profit
                        Pi_gross = Y_val - wL * L_opt - wH * HP_opt

                        ! Exit value (current state, not choices)
                        exit_val = max(Pi_gross + (1.0_dp - delta_K) * K_val &
                                       + (1.0_dp - delta_S) * S_val - R * D_old_val, 0.0_dp)

                        ! Compute intangible investment (for adjustment costs)
                        inv_S = RD_production(HR_choice)

                        ! Adjustment costs
                        AC_K_val = adjustment_cost_K(IK_choice, K_val)
                        AC_S_val = adjustment_cost_S(inv_S, S_val)

                        ! Compute dividends with fixed policy
                        expenses = IK_choice + wH * HR_choice + AC_K_val + AC_S_val
                        dividends = Pi_gross - R * D_old_val + Dp_choice - expenses

                        ! Clamp dividends to 0 (hard div >= 0 constraint)
                        dividends = max(dividends, 0.0_dp)

                        ! Continuation value
                        EV = expect_V(iz, Kprime, Sprime, Dp_choice)
                        W_eval = dividends + beta * EV

                        ! V = zeta * exit_val + (1-zeta) * max(W, 0)
                        V_new(iz, iK, iS, iD) = zeta * exit_val &
                            + (1.0_dp - zeta) * max(W_eval, 0.0_dp)

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
        logical :: do_improvement, first_improvement, do_full_search
        integer :: num_threads
        integer :: n_improvements

        print *, ""
        print *, "======================================"
        print *, "SOLVING FIRM PROBLEM (VFI + HOWARD)"
        print *, "======================================"

        total_states = nz * nK * nS * nD
        total_choices = nIK * nHR
        print '(A,I8)', "  Total state space points: ", total_states
        print '(A,I8)', "  Choice combinations (IK x HR): ", total_choices
        print '(A)',    "  D'' determination: ANALYTICAL (Khan & Thomas)"
        print '(A)',    "    Type A: D''=0, div>0 (unconstrained, absorbing)"
        print '(A)',    "    Type B: D''=D_needed, div=0 (potentially constrained)"
        print '(A)',    "    Type C: D''=D_ub, div=0 (actually constrained)"
        print '(A)',    "  Exit timing: after production, before investment"
        print '(A)',    "  Exit value: max(Pi + (1-dK)*K + (1-dS)*S - R*D, 0)"
        print '(A)',    "  Dividend constraint: div >= 0 (hard, no equity issuance)"
        print '(A,I8)', "  Choice combinations (local): ", (2*local_search_radius+1)**2
        print '(A,I4)', "  Howard improvement frequency: ", howard_freq
        print '(A,I4)', "  Howard evaluation steps: ", howard_eval_steps

        ! Report OpenMP thread count
        num_threads = 1
        !$ num_threads = omp_get_max_threads()
        print '(A,I4)', "  OpenMP threads: ", num_threads
        print *, ""

        ! Initialize value function
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
        pol_mu = 0.0_dp
        pol_lambda = 0.0_dp

        ! Initialize policy indices to middle of grids
        pol_iIK = nIK / 2
        pol_iHR = nHR / 2

        first_improvement = .true.
        n_improvements = 0

        ! Main iteration loop
        do iter = 1, maxiter_VFI

            ! Decide: Policy Improvement or Policy Evaluation?
            do_improvement = (iter == 1) .or. (mod(iter-1, howard_freq) == 0)

            if (do_improvement) then
                !---------------------------------------------------------------
                ! POLICY IMPROVEMENT STEP (expensive)
                !---------------------------------------------------------------
                n_improvements = n_improvements + 1

                do_full_search = first_improvement .or. (mod(n_improvements, full_search_freq) == 0)

                if (do_full_search) then
                    print '(A,I5,A)', "  Iter ", iter, ": Policy IMPROVEMENT (FULL grid search)..."
                else
                    print '(A,I5,A)', "  Iter ", iter, ": Policy IMPROVEMENT (local search)..."
                end if

                call policy_improvement_step(do_full_search)
                first_improvement = .false.

                ! Compute metric
                metric = maxval(abs(V_new - V))

                ! Update V
                V = V_new

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

                    V = V_new

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
