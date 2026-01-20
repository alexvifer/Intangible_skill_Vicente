!===================================================================================
! MODULE: mod_interpolation
!
! DESCRIPTION:
!   Interpolation routines for value function and distribution.
!   Uses multilinear interpolation for 4D value function V(z,K,S,D).
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_interpolation
    use mod_parameters
    use mod_globals
    implicit none

contains

    !===================================================================================
    ! SUBROUTINE: locate_grid
    !
    ! DESCRIPTION:
    !   Binary search to locate grid point for interpolation.
    !   ALLOWS EXTRAPOLATION: If x is outside grid bounds, we extrapolate linearly
    !   using the nearest two grid points. This is important for allowing S to
    !   depreciate below S_min naturally without artificial clamping.
    !
    ! INPUTS:
    !   grid - 1D grid array
    !   n    - Size of grid
    !   x    - Value to locate
    !
    ! OUTPUT:
    !   i    - Lower grid index (grid(i) ≤ x < grid(i+1) for interior)
    !   w    - Weight on upper point (can be negative or >1 for extrapolation)
    !===================================================================================
    subroutine locate_grid(grid, n, x, i, w)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: grid(n), x
        integer, intent(out) :: i
        real(dp), intent(out) :: w
        integer :: il, iu, im

        ! Handle extrapolation below grid
        if (x <= grid(1)) then
            i = 1
            ! Linear extrapolation: w can be negative
            w = (x - grid(1)) / (grid(2) - grid(1))
            return
        end if

        ! Handle extrapolation above grid
        if (x >= grid(n)) then
            i = n - 1
            ! Linear extrapolation: w can be > 1
            w = (x - grid(n-1)) / (grid(n) - grid(n-1))
            return
        end if

        ! Binary search for interior points
        il = 1
        iu = n
        do while (iu - il > 1)
            im = (iu + il) / 2
            if (x >= grid(im)) then
                il = im
            else
                iu = im
            end if
        end do

        i = il
        w = (x - grid(i)) / (grid(i+1) - grid(i))

    end subroutine locate_grid

    !===================================================================================
    ! FUNCTION: interp_V
    !
    ! DESCRIPTION:
    !   4D multilinear interpolation of value function V(z,K,S,D)
    !   Given continuous state (zprime, Kprime, Sprime, Dprime), interpolate V
    !
    ! INPUTS:
    !   iz      - Current z index (discrete, no interpolation needed)
    !   Kprime  - Next period tangible capital
    !   Sprime  - Next period intangible capital
    !   Dprime  - Next period debt
    !
    ! OUTPUT:
    !   Interpolated value
    !===================================================================================
    function interp_V(iz, Kprime, Sprime, Dprime) result(V_interp)
        implicit none
        integer, intent(in) :: iz
        real(dp), intent(in) :: Kprime, Sprime, Dprime
        real(dp) :: V_interp
        integer :: iK, iS, iD
        real(dp) :: wK, wS, wD
        real(dp) :: V000, V001, V010, V011, V100, V101, V110, V111
        real(dp) :: V00, V01, V10, V11, V0, V1

        ! Locate on grids
        call locate_grid(grid_K, nK, Kprime, iK, wK)
        call locate_grid(grid_S, nS, Sprime, iS, wS)
        call locate_grid(grid_D, nD, Dprime, iD, wD)

        ! Trilinear interpolation (3D: K, S, D; z is discrete)
        ! Get 8 corner values
        V000 = V(iz, iK,   iS,   iD)
        V001 = V(iz, iK,   iS,   iD+1)
        V010 = V(iz, iK,   iS+1, iD)
        V011 = V(iz, iK,   iS+1, iD+1)
        V100 = V(iz, iK+1, iS,   iD)
        V101 = V(iz, iK+1, iS,   iD+1)
        V110 = V(iz, iK+1, iS+1, iD)
        V111 = V(iz, iK+1, iS+1, iD+1)

        ! Interpolate along D
        V00 = (1.0_dp - wD) * V000 + wD * V001
        V01 = (1.0_dp - wD) * V010 + wD * V011
        V10 = (1.0_dp - wD) * V100 + wD * V101
        V11 = (1.0_dp - wD) * V110 + wD * V111

        ! Interpolate along S
        V0 = (1.0_dp - wS) * V00 + wS * V01
        V1 = (1.0_dp - wS) * V10 + wS * V11

        ! Interpolate along K
        V_interp = (1.0_dp - wK) * V0 + wK * V1

    end function interp_V

    !===================================================================================
    ! FUNCTION: expect_V
    !
    ! DESCRIPTION:
    !   Computes expected continuation value E[V(z',K',S',D') | z]
    !   Uses transition matrix Pi_z
    !
    ! INPUTS:
    !   iz      - Current z index
    !   Kprime  - Next period K
    !   Sprime  - Next period S
    !   Dprime  - Next period D
    !
    ! OUTPUT:
    !   Expected value
    !===================================================================================
    function expect_V(iz, Kprime, Sprime, Dprime) result(EV)
        implicit none
        integer, intent(in) :: iz
        real(dp), intent(in) :: Kprime, Sprime, Dprime
        real(dp) :: EV
        integer :: izp
        real(dp) :: V_zp

        EV = 0.0_dp

        do izp = 1, nz
            V_zp = interp_V(izp, Kprime, Sprime, Dprime)
            EV = EV + Pi_z(iz, izp) * V_zp
        end do

    end function expect_V

end module mod_interpolation
