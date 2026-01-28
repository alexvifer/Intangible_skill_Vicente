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

    ! FIX #6: Frequency of full grid search reset (to escape local optima)
    ! Every full_search_freq policy improvement iterations, do a full grid search
    integer, parameter :: full_search_freq = 50

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
    !
    !   FIX #5: Uses nested bisection for robustness.
    !   For each L, solve for HP via bisection (MPH(L,HP) = wH).
    !   Then bisect on L to find MPL(L,HP*(L)) = wL.
    !   This avoids the instability of multiplicative updates.
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
        ! MPL is decreasing in L, so MPL(L_lo) > wL and MPL(L_hi) < wL typically
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

            ! MPL is decreasing in L, so:
            ! If MPL_mid > wL_in, L is too low -> increase L_lo
            ! If MPL_mid < wL_in, L is too high -> decrease L_hi
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
    !   This is the inner loop of the nested bisection algorithm.
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

        ! Bounds for HP (skilled production labor)
        HP_lo = 0.001_dp
        HP_hi = 5.0_dp

        ! Check bounds
        MPH_lo = marginal_product_HP(z, K, S, L_val, HP_lo)
        MPH_hi = marginal_product_HP(z, K, S, L_val, HP_hi)

        ! Handle edge cases
        if (MPH_lo <= wH_in .or. isnan(MPH_lo)) then
            HP_opt = HP_lo
            return
        end if

        if (MPH_hi >= wH_in) then
            HP_opt = HP_hi
            return
        end if

        ! Bisection on HP
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

            ! MPH is decreasing in HP
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
    !   OPTIMIZATION: D' is computed ANALYTICALLY instead of grid search.
    !   With R = 1/β, firms are indifferent about borrowing (NPV = 0), so:
    !     - If firm has surplus funds: D' = 0
    !     - If firm needs financing: D' = min(financing_gap, collateral_limit)
    !   This eliminates the nDprime loop, reducing computation by ~20x.
    !
    !   LOCAL SEARCH: Searches ±local_search_radius around previous optimal indices
    !   for I^K and H^R. On first iteration, searches full grid.
    !===================================================================================
    subroutine policy_improvement_step(first_iteration)
        implicit none
        logical, intent(in) :: first_iteration
        integer :: iz, iK, iS, iD
        real(dp) :: z_val, K_val, S_val, D_old_val
        real(dp) :: L_opt, HP_opt, Y_val, Pi_gross
        real(dp) :: V_best, V_try
        integer :: iIK, iHR
        integer :: iIK_lo, iIK_hi, iHR_lo, iHR_hi
        integer :: best_iIK, best_iHR
        real(dp) :: IK_choice, HR_choice, Dp_choice
        real(dp) :: Kprime, Sprime, inv_S
        real(dp) :: resources_before_D, expenses, dividends, financing_gap
        real(dp) :: D_max_coll, EV
        real(dp) :: AC_K_val, AC_S_val
        real(dp) :: EV_invest                  ! Continuation value component
        logical :: constraint_binds            ! Whether collateral constraint binds at optimal choice
        real(dp) :: V_unconstrained_best       ! Best value ignoring feasibility (for constraint detection)
        logical :: unconstrained_is_infeasible ! Whether unconstrained optimum is infeasible

        ! OPTIMIZATION: Precompute expected value grid ONCE per VFI iteration
        ! This moves the O(nz) summation outside the choice loop
        ! Speedup: ~11x for expect_V calls
        call precompute_EV_grid()

        ! Precompute static labor solutions
        call precompute_static_labor()

        ! Loop over state space with OpenMP parallelization
        !$OMP PARALLEL DO COLLAPSE(2) &
        !$OMP& PRIVATE(iz, iK, iS, iD, z_val, K_val, S_val, D_old_val, &
        !$OMP&         L_opt, HP_opt, Y_val, Pi_gross, V_best, V_try, &
        !$OMP&         iIK, iHR, iIK_lo, iIK_hi, iHR_lo, iHR_hi, &
        !$OMP&         best_iIK, best_iHR, IK_choice, HR_choice, Dp_choice, &
        !$OMP&         Kprime, Sprime, inv_S, resources_before_D, expenses, dividends, &
        !$OMP&         financing_gap, D_max_coll, EV, AC_K_val, AC_S_val, &
        !$OMP&         EV_invest, constraint_binds, V_unconstrained_best, &
        !$OMP&         unconstrained_is_infeasible) &
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

                    ! Note: Collateral constraint is based on NEXT-PERIOD capital (K', S')
                    ! following the literature convention: d_{t+1} <= alpha_K * k_{t+1} + alpha_S * s_{t+1}
                    ! This means D_max_coll is computed INSIDE the investment loop below

                    do iD = 1, nD
                        D_old_val = grid_D(iD)

                        ! Resources available before new borrowing
                        resources_before_D = Pi_gross - R * D_old_val

                        ! Determine search bounds (local search or full grid)
                        if (first_iteration) then
                            ! Full grid search on first iteration
                            iIK_lo = 1
                            iIK_hi = nIK
                            iHR_lo = 1
                            iHR_hi = nHR
                        else
                            ! Local search around previous optimum
                            iIK_lo = max(1, pol_iIK(iz, iK, iS, iD) - local_search_radius)
                            iIK_hi = min(nIK, pol_iIK(iz, iK, iS, iD) + local_search_radius)
                            iHR_lo = max(1, pol_iHR(iz, iK, iS, iD) - local_search_radius)
                            iHR_hi = min(nHR, pol_iHR(iz, iK, iS, iD) + local_search_radius)
                        end if

                        ! Initialize best value and indices
                        V_best = -1.0e10_dp
                        best_iIK = iIK_lo
                        best_iHR = iHR_lo

                        ! For constraint detection: track whether the UNCONSTRAINED optimum
                        ! (highest value ignoring feasibility) is infeasible.
                        ! A firm is constrained (λ > 0) when it WOULD choose a higher
                        ! investment level but CAN'T due to the collateral constraint.
                        V_unconstrained_best = -1.0e10_dp
                        unconstrained_is_infeasible = .false.

                        ! Grid search over (I^K, H^R) - D' computed analytically
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

                                ! ============================================================
                                ! COLLATERAL CONSTRAINT: Literature Timing Convention
                                ! d_{t+1} <= alpha_K * k_{t+1} + alpha_S * s_{t+1}
                                !
                                ! Debt taken today (D') is collateralized by NEXT-PERIOD capital
                                ! (K', S'). This means investment decisions directly affect
                                ! borrowing capacity within the same period.
                                !
                                ! COUNTERFACTUAL (λ=0): When remove_collateral_constraint=.true.,
                                ! set D_max_coll to effectively infinity, allowing unlimited
                                ! borrowing. This is the proper "no frictions" benchmark.
                                ! ============================================================
                                if (remove_collateral_constraint) then
                                    D_max_coll = 1.0e10_dp  ! Effectively infinite borrowing
                                else
                                    D_max_coll = collateral_constraint(Kprime, Sprime)
                                end if

                                ! Compute adjustment costs (0 if phi_K=0 or phi_S=0)
                                AC_K_val = adjustment_cost_K(IK_choice, K_val)
                                AC_S_val = adjustment_cost_S(inv_S, S_val)

                                ! Total expenses for this (IK, HR) combination
                                expenses = IK_choice + wH * HR_choice + AC_K_val + AC_S_val

                                ! ============================================================
                                ! ANALYTICAL D' COMPUTATION
                                ! With R = 1/β, firm is indifferent about borrowing level.
                                ! Optimal policy: borrow only what's needed, up to constraint.
                                ! ============================================================
                                financing_gap = expenses - resources_before_D

                                if (financing_gap <= 0.0_dp) then
                                    ! Firm has enough internal funds - no need to borrow
                                    Dp_choice = 0.0_dp
                                else if (financing_gap <= D_max_coll) then
                                    ! Firm needs external funds but can borrow enough
                                    Dp_choice = financing_gap
                                else
                                    ! Firm needs more than collateral allows
                                    Dp_choice = D_max_coll
                                end if

                                ! Compute dividends
                                dividends = resources_before_D + Dp_choice - expenses

                                ! Compute continuation value for this investment choice
                                EV = expect_V(iz, Kprime, Sprime, Dp_choice)

                                ! Investment value = continuation value only
                                EV_invest = beta * (1.0_dp - zeta) * EV

                                ! Compute total value (may be infeasible if dividends < 0)
                                V_try = dividends + EV_invest

                                ! ============================================================
                                ! CONSTRAINT DETECTION: Track unconstrained optimum
                                ! The "unconstrained" choice is the one with highest V_try
                                ! regardless of whether dividends < 0.
                                ! If this choice is infeasible (dividends < 0), firm is constrained.
                                ! ============================================================
                                if (V_try > V_unconstrained_best) then
                                    V_unconstrained_best = V_try
                                    unconstrained_is_infeasible = (dividends < -epsilon)
                                end if

                                ! Skip if dividends negative (infeasible due to collateral constraint)
                                if (dividends < -epsilon) cycle

                                if (V_try > V_best) then
                                    V_best = V_try
                                    best_iIK = iIK
                                    best_iHR = iHR
                                    pol_IK(iz, iK, iS, iD) = IK_choice
                                    pol_HR(iz, iK, iS, iD) = HR_choice
                                    pol_Dprime(iz, iK, iS, iD) = Dp_choice
                                    pol_Kprime(iz, iK, iS, iD) = Kprime
                                    pol_Sprime(iz, iK, iS, iD) = Sprime
                                    pol_L(iz, iK, iS, iD) = L_opt
                                    pol_HP(iz, iK, iS, iD) = HP_opt
                                    pol_Y(iz, iK, iS, iD) = Y_val
                                end if

                            end do  ! HR
                        end do  ! IK

                        ! ============================================================
                        ! CONSTRAINT STATUS: λ > 0 (Collateral Constraint Binds)
                        !
                        ! A firm is constrained when its UNCONSTRAINED optimum
                        ! (the (IK,HR) choice with highest value ignoring feasibility)
                        ! is infeasible due to the collateral constraint.
                        !
                        ! This means the firm WOULD invest more but CAN'T.
                        !
                        ! In counterfactual mode (remove_collateral_constraint=.true.),
                        ! all choices are feasible so no firm is ever constrained.
                        ! ============================================================
                        constraint_binds = unconstrained_is_infeasible
                        pol_constr(iz, iK, iS, iD) = constraint_binds

                        ! Also store shadow value indicator (λ > 0 when constrained)
                        ! pol_lambda can be used for welfare calculations
                        if (constraint_binds) then
                            pol_lambda(iz, iK, iS, iD) = 1.0_dp  ! Qualitative indicator
                        else
                            pol_lambda(iz, iK, iS, iD) = 0.0_dp
                        end if

                        ! Store optimal indices for next iteration's local search
                        pol_iIK(iz, iK, iS, iD) = best_iIK
                        pol_iHR(iz, iK, iS, iD) = best_iHR

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

        ! OPTIMIZATION: Precompute expected value grid from current V
        ! Must be called each evaluation step since V changes
        call precompute_EV_grid()

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
        logical :: do_improvement, first_improvement, do_full_search
        integer :: num_threads
        integer :: n_improvements  ! Counter for policy improvement steps

        print *, ""
        print *, "======================================"
        print *, "SOLVING FIRM PROBLEM (VFI + HOWARD)"
        print *, "======================================"

        total_states = nz * nK * nS * nD
        total_choices = nIK * nHR  ! D' is now computed analytically
        print '(A,I8)', "  Total state space points: ", total_states
        print '(A,I8)', "  Choice combinations (full): ", total_choices
        print '(A)', "  Note: D' computed analytically (not grid search)"
        print '(A,I8)', "  Choice combinations (local): ", (2*local_search_radius+1)**2
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
        ! Note: pol_iDp no longer used (D' computed analytically)
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

                ! FIX #6: Periodically do full grid search to escape local optima
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
