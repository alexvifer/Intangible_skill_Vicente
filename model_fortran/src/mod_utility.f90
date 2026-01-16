!===================================================================================
! MODULE: mod_utility
!
! DESCRIPTION:
!   Utility functions for production, CES aggregation, and other calculations.
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_utility
    use mod_parameters
    implicit none

contains

    !===================================================================================
    ! FUNCTION: CES_Q
    !
    ! DESCRIPTION:
    !   Computes the intangible-skill composite Q = [ω*S^ρQ + (1-ω)*(HP)^ρQ]^(1/ρQ)
    !
    ! INPUTS:
    !   S  - Intangible capital
    !   HP - Skilled labor in production
    !
    ! OUTPUT:
    !   Q  - Intangible-skill composite
    !===================================================================================
    function CES_Q(S, HP) result(Q)
        implicit none
        real(dp), intent(in) :: S, HP
        real(dp) :: Q
        real(dp) :: S_safe, HP_safe
        real(dp), parameter :: min_input = 1.0e-4_dp

        ! For CES with rho < 0 (complements), inputs must be strictly positive
        ! to avoid numerical issues with negative exponents
        S_safe = max(S, min_input)
        HP_safe = max(HP, min_input)

        if (abs(rho_Q) < epsilon) then
            ! Cobb-Douglas limit
            Q = (S_safe**omega) * (HP_safe**(1.0_dp - omega))
        else
            Q = (omega * S_safe**rho_Q + (1.0_dp - omega) * HP_safe**rho_Q)**(1.0_dp / rho_Q)
        end if

        ! Ensure output is bounded
        Q = max(Q, min_input)

    end function CES_Q

    !===================================================================================
    ! FUNCTION: CES_X
    !
    ! DESCRIPTION:
    !   Computes capital composite X = [θK*K^ρK + θQ*Q^ρK]^(1/ρK)
    !
    ! INPUTS:
    !   K - Tangible capital
    !   Q - Intangible-skill composite
    !
    ! OUTPUT:
    !   X - Capital composite
    !===================================================================================
    function CES_X(K, Q) result(X)
        implicit none
        real(dp), intent(in) :: K, Q
        real(dp) :: X
        real(dp) :: K_safe, Q_safe
        real(dp), parameter :: min_input = 1.0e-4_dp

        ! For CES with rho < 0 (complements), inputs must be strictly positive
        K_safe = max(K, min_input)
        Q_safe = max(Q, min_input)

        if (abs(rho_K) < epsilon) then
            ! Cobb-Douglas limit
            X = (K_safe**theta_K) * (Q_safe**theta_Q)
        else
            X = (theta_K * K_safe**rho_K + theta_Q * Q_safe**rho_K)**(1.0_dp / rho_K)
        end if

        ! Ensure output is bounded
        X = max(X, min_input)

    end function CES_X

    !===================================================================================
    ! FUNCTION: production_Y
    !
    ! DESCRIPTION:
    !   Computes output Y = z * [X^α * L^γ]^ν
    !
    ! INPUTS:
    !   z - Productivity
    !   K - Tangible capital
    !   S - Intangible capital
    !   L - Unskilled labor
    !   HP - Skilled labor in production
    !
    ! OUTPUT:
    !   Y - Output
    !===================================================================================
    function production_Y(z, K, S, L, HP) result(Y)
        implicit none
        real(dp), intent(in) :: z, K, S, L, HP
        real(dp) :: Q, X, inner_nest, Y

        ! Build from bottom up
        Q = CES_Q(S, HP)
        X = CES_X(K, Q)

        ! Inner nest: X^α * L^γ (with α + γ = 1)
        inner_nest = (X**alpha_prod) * (L**gamma_prod)

        ! Output
        Y = z * (inner_nest**nu)

    end function production_Y

    !===================================================================================
    ! FUNCTION: marginal_product_L
    !
    ! DESCRIPTION:
    !   Computes ∂Y/∂L for static labor FOC
    !
    ! INPUTS:
    !   z, K, S, L, HP
    !
    ! OUTPUT:
    !   MPL - Marginal product of unskilled labor
    !===================================================================================
    function marginal_product_L(z, K, S, L, HP) result(MPL)
        implicit none
        real(dp), intent(in) :: z, K, S, L, HP
        real(dp) :: Q, X, inner_nest, MPL
        real(dp) :: L_safe, X_safe
        real(dp), parameter :: min_input = 1.0e-4_dp
        real(dp), parameter :: max_MP = 1.0e6_dp

        L_safe = max(L, min_input)

        Q = CES_Q(S, HP)
        X = CES_X(K, Q)
        X_safe = max(X, min_input)
        inner_nest = (X_safe**alpha_prod) * (L_safe**gamma_prod)

        ! ∂Y/∂L = z * ν * inner_nest^(ν-1) * γ * X^α * L^(γ-1)
        MPL = z * nu * (inner_nest**(nu - 1.0_dp)) * gamma_prod * (X_safe**alpha_prod) * (L_safe**(gamma_prod - 1.0_dp))

        ! Cap at reasonable value
        MPL = min(MPL, max_MP)

    end function marginal_product_L

    !===================================================================================
    ! FUNCTION: marginal_product_HP
    !
    ! DESCRIPTION:
    !   Computes ∂Y/∂HP using chain rule through Q and X
    !
    ! INPUTS:
    !   z, K, S, L, HP
    !
    ! OUTPUT:
    !   MPHP - Marginal product of skilled production labor
    !===================================================================================
    function marginal_product_HP(z, K, S, L, HP) result(MPHP)
        implicit none
        real(dp), intent(in) :: z, K, S, L, HP
        real(dp) :: Q, X, inner_nest, dY_dX, dX_dQ, dQ_dHP, MPHP
        real(dp) :: HP_safe, Q_safe, X_safe, L_safe
        real(dp), parameter :: min_input = 1.0e-4_dp
        real(dp), parameter :: max_MP = 1.0e6_dp

        ! Ensure inputs are safe for negative exponents
        HP_safe = max(HP, min_input)
        L_safe = max(L, min_input)

        Q = CES_Q(S, HP_safe)
        X = CES_X(K, Q)
        Q_safe = max(Q, min_input)
        X_safe = max(X, min_input)
        inner_nest = (X_safe**alpha_prod) * (L_safe**gamma_prod)

        ! ∂Y/∂X
        dY_dX = z * nu * (inner_nest**(nu - 1.0_dp)) * alpha_prod * (X_safe**(alpha_prod - 1.0_dp)) * (L_safe**gamma_prod)

        ! ∂X/∂Q
        if (abs(rho_K) < epsilon) then
            dX_dQ = (X_safe / Q_safe) * theta_Q
        else
            dX_dQ = (X_safe**(1.0_dp - rho_K)) * theta_Q * (Q_safe**(rho_K - 1.0_dp))
        end if

        ! ∂Q/∂HP
        if (abs(rho_Q) < epsilon) then
            dQ_dHP = (Q_safe / HP_safe) * (1.0_dp - omega)
        else
            dQ_dHP = (Q_safe**(1.0_dp - rho_Q)) * (1.0_dp - omega) * (HP_safe**(rho_Q - 1.0_dp))
        end if

        ! Chain rule
        MPHP = dY_dX * dX_dQ * dQ_dHP

        ! Cap at reasonable value to prevent numerical overflow
        MPHP = min(MPHP, max_MP)

    end function marginal_product_HP

    !===================================================================================
    ! FUNCTION: marginal_product_S
    !
    ! DESCRIPTION:
    !   Computes ∂Y/∂S using chain rule through Q and X
    !
    ! INPUTS:
    !   z, K, S, L, HP
    !
    ! OUTPUT:
    !   MPS - Marginal product of intangible capital
    !===================================================================================
    function marginal_product_S(z, K, S, L, HP) result(MPS)
        implicit none
        real(dp), intent(in) :: z, K, S, L, HP
        real(dp) :: Q, X, inner_nest, dY_dX, dX_dQ, dQ_dS, MPS
        real(dp) :: S_safe, L_safe, Q_safe, X_safe
        real(dp), parameter :: min_input = 1.0e-4_dp
        real(dp), parameter :: max_MP = 1.0e6_dp

        ! Ensure inputs are safe for negative exponents
        S_safe = max(S, min_input)
        L_safe = max(L, min_input)

        Q = CES_Q(S_safe, HP)
        X = CES_X(K, Q)
        Q_safe = max(Q, min_input)
        X_safe = max(X, min_input)
        inner_nest = (X_safe**alpha_prod) * (L_safe**gamma_prod)

        ! ∂Y/∂X
        dY_dX = z * nu * (inner_nest**(nu - 1.0_dp)) * alpha_prod * (X_safe**(alpha_prod - 1.0_dp)) * (L_safe**gamma_prod)

        ! ∂X/∂Q
        if (abs(rho_K) < epsilon) then
            dX_dQ = (X_safe / Q_safe) * theta_Q
        else
            dX_dQ = (X_safe**(1.0_dp - rho_K)) * theta_Q * (Q_safe**(rho_K - 1.0_dp))
        end if

        ! ∂Q/∂S
        if (abs(rho_Q) < epsilon) then
            dQ_dS = (Q_safe / S_safe) * omega
        else
            dQ_dS = (Q_safe**(1.0_dp - rho_Q)) * omega * (S_safe**(rho_Q - 1.0_dp))
        end if

        ! Chain rule
        MPS = dY_dX * dX_dQ * dQ_dS

        ! Cap at reasonable value to prevent numerical overflow
        MPS = min(MPS, max_MP)

    end function marginal_product_S

    !===================================================================================
    ! FUNCTION: marginal_product_K
    !
    ! DESCRIPTION:
    !   Computes ∂Y/∂K using chain rule through X
    !
    ! INPUTS:
    !   z, K, S, L, HP
    !
    ! OUTPUT:
    !   MPK - Marginal product of tangible capital
    !===================================================================================
    function marginal_product_K(z, K, S, L, HP) result(MPK)
        implicit none
        real(dp), intent(in) :: z, K, S, L, HP
        real(dp) :: Q, X, inner_nest, dY_dX, dX_dK, MPK
        real(dp) :: K_safe, L_safe, Q_safe, X_safe
        real(dp), parameter :: min_input = 1.0e-4_dp
        real(dp), parameter :: max_MP = 1.0e6_dp

        ! Ensure inputs are safe for negative exponents
        K_safe = max(K, min_input)
        L_safe = max(L, min_input)

        Q = CES_Q(S, HP)
        X = CES_X(K_safe, Q)
        Q_safe = max(Q, min_input)
        X_safe = max(X, min_input)
        inner_nest = (X_safe**alpha_prod) * (L_safe**gamma_prod)

        ! ∂Y/∂X
        dY_dX = z * nu * (inner_nest**(nu - 1.0_dp)) * alpha_prod * (X_safe**(alpha_prod - 1.0_dp)) * (L_safe**gamma_prod)

        ! ∂X/∂K
        if (abs(rho_K) < epsilon) then
            dX_dK = (X_safe / K_safe) * theta_K
        else
            dX_dK = (X_safe**(1.0_dp - rho_K)) * theta_K * (K_safe**(rho_K - 1.0_dp))
        end if

        ! Chain rule
        MPK = dY_dX * dX_dK

        ! Cap at reasonable value to prevent numerical overflow
        MPK = min(MPK, max_MP)

    end function marginal_product_K

    !===================================================================================
    ! FUNCTION: RD_production
    !
    ! DESCRIPTION:
    !   Computes intangible investment from R&D: ΔS = Γ*(HR)^ξ
    !
    ! INPUTS:
    !   HR - R&D labor
    !
    ! OUTPUT:
    !   Investment in intangible capital
    !===================================================================================
    function RD_production(HR) result(inv_S)
        implicit none
        real(dp), intent(in) :: HR
        real(dp) :: inv_S

        if (HR < epsilon) then
            inv_S = 0.0_dp
        else
            inv_S = Gamma_RD * (HR**xi)
        end if

    end function RD_production

    !===================================================================================
    ! FUNCTION: marginal_RD_prod
    !
    ! DESCRIPTION:
    !   Computes marginal product of R&D labor: ∂(ΔS)/∂HR = Γ*ξ*(HR)^(ξ-1)
    !
    ! INPUTS:
    !   HR - R&D labor
    !
    ! OUTPUT:
    !   Marginal R&D productivity
    !===================================================================================
    function marginal_RD_prod(HR) result(dInvS_dHR)
        implicit none
        real(dp), intent(in) :: HR
        real(dp) :: dInvS_dHR

        if (HR < epsilon) then
            dInvS_dHR = 0.0_dp
        else
            dInvS_dHR = Gamma_RD * xi * (HR**(xi - 1.0_dp))
        end if

    end function marginal_RD_prod

    !===================================================================================
    ! FUNCTION: collateral_constraint
    !
    ! DESCRIPTION:
    !   Computes maximum borrowing: D ≤ αK*K + αS*S
    !
    ! INPUTS:
    !   K - Tangible capital
    !   S - Intangible capital
    !
    ! OUTPUT:
    !   Maximum debt
    !===================================================================================
    function collateral_constraint(K, S) result(D_max)
        implicit none
        real(dp), intent(in) :: K, S
        real(dp) :: D_max

        D_max = alpha_K * K + alpha_S * S

    end function collateral_constraint

end module mod_utility
