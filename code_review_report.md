# Code Review: Fortran Implementation vs. Model Specification

## Executive Summary

After careful comparison of your Fortran implementation against the model specification in your paper, I've identified several issues ranging from **critical bugs** that will produce incorrect results to **minor inconsistencies** that should be addressed for clarity. The most significant issues relate to:

1. **Collateral constraint timing** - using current K,S instead of next-period K',S'
2. **State-dependent investment bounds** - the issue you already identified
3. **Entry mechanism** - entrants starting at K_min, S_min instead of 0
4. **Missing intangible investment in resource constraint**

---

## Critical Issues

### Issue 1: Collateral Constraint Timing (CRITICAL)

**Model Specification (Equation 37, p.18):**
> D_{j,t} ≤ α_K K_{j,t} + α_S S_{j,t}

**Your Paper's Timing (p.18-19):**
The timing states that in Stage 3, the firm "obtains new debt D_t from banks, subject to the collateral constraint (37) **based on current capital stocks (K_t, S_t)**."

**Current Code (`mod_firm_problem.f90`, line 151):**
```fortran
D_max_coll = collateral_constraint(K_val, S_val)  ! Uses current K, S
```

**Assessment:** ✅ **This is actually CORRECT** per your timing specification. The collateral constraint is based on current capital stocks (K_t, S_t), not next-period stocks. This is consistent with Kiyotaki-Moore (1997) style constraints where borrowing capacity is determined by assets in place.

---

### Issue 2: State-Dependent Investment Bounds (CRITICAL - You Identified This)

**Problem:** The investment grid is fixed globally, but feasible investment depends on the current state.

**Current Code (`mod_equilibrium.f90`, lines 66-69):**
```fortran
do i = 1, nIK
    step = real(i-1, dp) / real(nIK-1, dp)
    grid_IK(i) = -delta_K * K_max * 0.50_dp + step * (K_max * 0.20_dp)
end do
```

This creates a grid roughly from -5.0 to +15.0 for investment, regardless of current capital.

**Problem Cases:**
- If K_val = K_min = 0.10 and IK_choice = -5.0, then K' = (1-0.10)*0.10 - 5.0 = -4.91 < 0 ❌
- Many grid points will be infeasible for low-capital firms

**Recommended Fix:** Compute state-dependent bounds within the optimization loop:
```fortran
! In policy_improvement_step, inside the state loop:
IK_min_state = -(1.0_dp - delta_K) * K_val + K_min  ! Ensures K' >= K_min
IK_max_state = some_reasonable_upper_bound
```

---

### Issue 3: Entry Mechanism Inconsistency (CRITICAL)

**Model Specification (p.16):**
> "Entrants pay entry cost c_e > 0... and start with **K_0 = S_0 = D_{-1} = 0**"

**Current Code (`mod_distribution.f90`, lines 45-46):**
```fortran
dist(iz, 1, 1, 1) = stat_dist_z(iz)  ! Entry at (z, K_min, S_min, D_min)
```

**Problem:** Entrants are placed at grid point 1, which corresponds to:
- K = K_min = 0.10 (not 0)
- S = S_min = 0.10 (not 0)
- D = D_min = 0.0 (correct)

**Impact:** This gives entrants a "free" endowment of capital, affecting equilibrium outcomes.

**Recommended Fix Options:**
1. Set K_min = S_min = 0 (but CES with ρ < 0 requires positive inputs)
2. Use very small K_min, S_min (e.g., 0.001) as numerical approximation to 0
3. Model entry as receiving a small starting capital (reinterpret the model)

Since CES with ρ_Q = -1.5 requires strictly positive inputs, option 2 is most practical. The current values (0.10) may be too large - consider reducing to 0.01 or smaller.

---

### Issue 4: Missing Intangible Investment in Resource Constraint (MODERATE)

**Model Budget Constraint (Equation 38):**
> I^K_{j,t} + w_L L_{j,t} + w_H H^P_{j,t} + **w_H H^R_{j,t}** + R D_{j,t-1} ≤ Y_{j,t} + D_{j,t}

The code correctly includes `wH * HR_choice` in expenses (line 170), so this is fine.

**However**, the goods market clearing condition (Definition 1(e)) states:
> C + ∫ I^K_j dΨ + ζ c_e N = Y_agg

**Current Code (`mod_distribution.f90`, line 212):**
```fortran
agg_C = agg_Y - agg_IK - zeta * ce * mass_firms
```

**Missing:** This doesn't account for resources used in R&D (skilled labor producing intangibles). However, since R&D labor is hired at wage w_H and paid out of firm revenues, it's implicitly captured in the budget constraint. The goods market clearing should actually be:
> C + ∫ I^K_j dΨ + ζ c_e N = Y_agg

This appears correct since R&D doesn't use physical goods, just labor.

**Assessment:** ✅ Code is correct.

---

## Moderate Issues

### Issue 5: Negative Capital Check Missing

**Current Code:** Allows K' and S' to be computed without checking they remain positive.

**Problem:** With negative investment from the grid, K' could go negative.

**Lines 175-177:**
```fortran
Kprime = (1.0_dp - delta_K) * K_val + IK_choice
inv_S = RD_production(HR_choice)
Sprime = (1.0_dp - delta_S) * S_val + inv_S
```

**Recommended Fix:**
```fortran
Kprime = (1.0_dp - delta_K) * K_val + IK_choice
if (Kprime < K_min) cycle  ! Skip infeasible choices

inv_S = RD_production(HR_choice)
Sprime = (1.0_dp - delta_S) * S_val + inv_S
! S' should always be >= (1-delta_S)*S_min > 0 since HR >= 0
```

---

### Issue 6: Constraint Classification Tolerance

**Current Code (line 192-193):**
```fortran
pol_constr(iz, iK, iS, iD) = (abs(Dp_choice - D_max_coll) < 0.01_dp)
```

**Problem:** A fixed tolerance of 0.01 may be inappropriate across different scales. If D_max_coll is small (e.g., 0.05), this tolerance represents 20% slack. If D_max_coll is large (e.g., 50), it's negligible.

**Recommended Fix:** Use a relative tolerance:
```fortran
pol_constr(iz, iK, iS, iD) = (Dp_choice > D_max_coll - 0.01_dp * max(1.0_dp, D_max_coll))
```

Or simply:
```fortran
pol_constr(iz, iK, iS, iD) = (Dp_choice >= D_max_coll - epsilon)
```

---

### Issue 7: Solvency Check May Be Too Strict

**Current Code (lines 140-148):**
```fortran
resources = Pi_gross - R * D_old_val
if (resources < -epsilon) then
    V_new(iz, iK, iS, iD) = -1.0e10_dp
    cycle
end if
```

**Problem:** This checks if `Pi_gross - R * D_old < 0` before considering new borrowing. But a firm could be temporarily illiquid yet able to borrow to cover the gap.

**Model Interpretation:** The firm can borrow D' first, then pay R*D_{-1}. The constraint is:
> Div = Pi_gross - I^K - w_H H^R - R D_{-1} + D' ≥ 0

So the solvency check should happen AFTER considering new borrowing, not before.

**Recommended Fix:** Remove the early solvency check, let the dividend constraint handle it:
```fortran
! Remove lines 140-148 (the early solvency check)
! The constraint `if (dividends < -epsilon) cycle` at line 173 handles this
```

Actually, looking more carefully, line 162 updates resources to include D':
```fortran
resources = Pi_gross - R * D_old_val + Dp_choice
```

So the early check creates a more restrictive condition than necessary. A firm with negative `Pi_gross - R*D_old` could still survive by borrowing more.

---

### Issue 8: Distribution Array Bounds in Interpolation

**Current Code (`mod_distribution.f90`, lines 94-116):**
When `wDp = 1.0` (i.e., Dprime is at grid maximum), the code accesses `iDp+1` which could exceed array bounds.

**Check in `locate_grid` (`mod_interpolation.f90`, lines 45-48):**
```fortran
else if (x >= grid(n)) then
    i = n - 1
    w = 1.0_dp
    return
end if
```

This sets `i = n-1`, so `i+1 = n` which is valid. ✅ This is correctly handled.

---

## Minor Issues

### Issue 9: R&D Labor Grid Upper Bound

**Current Code (`mod_equilibrium.f90`, lines 71-73):**
```fortran
do i = 1, nHR
    grid_HR(i) = Hbar * real(i-1, dp) / real(nHR-1, dp)
end do
```

**Concern:** This caps H^R at H̄ = 0.15 (total skilled labor supply). In principle, a single firm could hire more than the aggregate supply at the prevailing wage, though this would be infeasible in equilibrium.

**Assessment:** This is reasonable for computational purposes, though you might want to allow some slack (e.g., 1.2*Hbar) to ensure the grid doesn't artificially constrain the solution.

---

### Issue 10: Static Labor Solver Initial Guess

**Current Code (`mod_firm_problem.f90`, lines 48-49):**
```fortran
L_try = 0.30_dp
HP_try = 0.15_dp
```

**Concern:** Fixed initial guesses may lead to slow convergence or convergence issues for extreme states.

**Recommended Enhancement:** Use a smarter initial guess based on the state:
```fortran
! Rough scaling based on capital
scale = sqrt(K * S) / sqrt(K_max * S_max)
L_try = Lbar * scale
HP_try = Hbar * scale * 0.5_dp  ! Half to production, half to R&D
```

---

### Issue 11: Wage Update Direction

**Current Code (`mod_equilibrium.f90`, lines 241-242):**
```fortran
update_wL = wL * (1.0_dp + update_wage * excess_L / Lbar)
update_wH = wH * (1.0_dp + update_wage * excess_H / Hbar)
```

**Logic Check:** 
- If excess_L > 0 (demand > supply), wages should increase → ✅ Correct
- If excess_L < 0 (supply > demand), wages should decrease → ✅ Correct

**Assessment:** ✅ Direction is correct.

---

## Production Function Verification

Let me verify the production function implementation matches the paper.

**Model (Equations 30-32):**
1. Q = [ω S^ρQ + (1-ω)(H^P)^ρQ]^{1/ρQ}
2. X = [θ_K K^ρK + θ_Q Q^ρK]^{1/ρK}  
3. Y = z [X^α L^γ]^ν

**Code (`mod_utility.f90`):**
- `CES_Q`: ✅ Correctly implements equation (31)
- `CES_X`: ✅ Correctly implements equation (30)
- `production_Y`: ✅ Correctly implements equation (32)

**Marginal Products:** The chain-rule derivatives appear correct for ∂Y/∂L, ∂Y/∂HP, ∂Y/∂S, ∂Y/∂K.

---

## Recommended Priority of Fixes

1. **HIGH:** Fix state-dependent investment bounds (Issue 2)
2. **HIGH:** Remove or relax early solvency check (Issue 7)  
3. **MEDIUM:** Reduce K_min, S_min for entry consistency (Issue 3)
4. **MEDIUM:** Add explicit K' >= K_min check (Issue 5)
5. **LOW:** Improve constraint classification tolerance (Issue 6)
6. **LOW:** Improve static labor initial guesses (Issue 10)

---

## Summary

The core model structure (production function, capital accumulation, collateral constraint) is implemented correctly. The main issues are:

1. **State-dependent bounds:** The fixed investment grid can produce infeasible capital values for low-capital firms.

2. **Entry point:** Entrants start at (K_min, S_min) = (0.10, 0.10) instead of (0, 0) due to numerical requirements of CES with complements.

3. **Solvency check:** The early check may be overly restrictive; firms should be able to borrow to cover temporary liquidity shortfalls.

With these fixes, particularly the state-dependent bounds, the model should produce economically sensible results with appropriate numbers of constrained firms.

---

## Calibration Notes

Your paper mentions using pledgeability parameters from Holttinen et al. (2025): αK = 0.381, αS = 0.134 for UK firms. Your current values (αK = 0.50, αS = 0.10) are different - you might want to either:
1. Update to match the Holttinen values
2. Clarify in the paper that these are different calibration choices

**Grid Bounds Recommendations:**

Given K_min = 0.01, K_max = 100, the log-spaced grid gives good resolution near zero:
- grid_K(1) = 0.01
- grid_K(10) ≈ 0.05
- grid_K(25) ≈ 1.0
- grid_K(50) = 100

This should capture the dynamics of small entrants well.

**Potential Numerical Issues:**

With ρ_Q = -1.5 (strong complementarity), the CES function can exhibit sharp curvature when S and HP are very different in magnitude. The min_input = 1.0e-4 safeguard in `CES_Q` helps, but you might see some numerical sensitivity. Consider:
1. Monitoring the range of Q values across the state space
2. Checking that marginal products remain well-behaved

---

## Files Provided

I've created corrected versions of the key modules:

1. `mod_firm_problem_corrected.f90` - Fixes state-dependent bounds, removes early solvency check
2. `mod_parameters_corrected.f90` - Reduces K_min, S_min for better entry approximation

These can be compared against your originals to see the specific changes.
