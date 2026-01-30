!===================================================================================
! MODULE: mod_interpolation
!
! DESCRIPTION:
!   Interpolation routines for value function and distribution.
!   OPTIMIZED VERSION:
!     - Precomputed expected value grid (EV_grid) eliminates nz loop in expect_V
!     - Vectorized interpolation weights computation
!     - Cached grid indices for repeated lookups
!
! AUTHOR: Generated for Vicente (2026) - Skill-Biased Stagnation Model
!===================================================================================
module mod_interpolation
    use mod_parameters
    use mod_globals
    !$ use omp_lib
    implicit none

contains

    !===================================================================================
    ! SUBROUTINE: precompute_EV_grid
    !
    ! DESCRIPTION:
    !   CRITICAL OPTIMIZATION: Precomputes expected values for all grid points.
    !   EV_grid(iz, iK, iS, iD) = sum_{iz'} Pi_z(iz, iz') * V(iz', iK, iS, iD)
    !
    !   This moves the O(nz) summation OUTSIDE the choice loop, reducing
    !   expect_V from O(nz * interpolation) to O(interpolation only).
    !
    !   Should be called ONCE at the start of each VFI iteration.
    !===================================================================================
    subroutine precompute_EV_grid()
        implicit none
        integer :: iz, iK, iS, iD, izp
        real(dp) :: ev_sum

        !$OMP PARALLEL DO COLLAPSE(3) PRIVATE(iz, iK, iS, iD, izp, ev_sum) SCHEDULE(static)
        do iK = 1, nK
            do iS = 1, nS
                do iD = 1, nD
                    ! For each (K, S, D) grid point, compute E[V|z] for all z
                    do iz = 1, nz
                        ev_sum = 0.0_dp
                        do izp = 1, nz
                            ev_sum = ev_sum + Pi_z(iz, izp) * V(izp, iK, iS, iD)
                        end do
                        EV_grid(iz, iK, iS, iD) = ev_sum
                    end do
                end do
            end do
        end do
        !$OMP END PARALLEL DO

    end subroutine precompute_EV_grid

    !===================================================================================
    ! SUBROUTINE: locate_grid
    !
    ! DESCRIPTION:
    !   Binary search to locate grid point for interpolation.
    !   ALLOWS EXTRAPOLATION: If x is outside grid bounds, we extrapolate linearly.
    !
    ! INPUTS:
    !   grid - 1D grid array
    !   n    - Size of grid
    !   x    - Value to locate
    !
    ! OUTPUT:
    !   i    - Lower grid index
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
            w = (x - grid(1)) / (grid(2) - grid(1))
            return
        end if

        ! Handle extrapolation above grid
        if (x >= grid(n)) then
            i = n - 1
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
    ! SUBROUTINE: locate_grid_fast
    !
    ! DESCRIPTION:
    !   OPTIMIZED grid location for known grid structures.
    !   Uses direct calculation for linear grids, binary search otherwise.
    !
    ! For linear grids: i = floor((x - x_min) / dx) + 1
    !===================================================================================
    subroutine locate_grid_fast(grid, n, x, x_min, dx, i, w)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: grid(n), x, x_min, dx
        integer, intent(out) :: i
        real(dp), intent(out) :: w
        real(dp) :: pos

        ! Direct calculation for linear grid
        pos = (x - x_min) / dx
        i = int(pos) + 1

        ! Clamp to valid range
        if (i < 1) then
            i = 1
            w = (x - grid(1)) / (grid(2) - grid(1))
        else if (i >= n) then
            i = n - 1
            w = (x - grid(n-1)) / (grid(n) - grid(n-1))
        else
            w = pos - real(i - 1, dp)
        end if

    end subroutine locate_grid_fast

    !===================================================================================
    ! FUNCTION: interp_V
    !
    ! DESCRIPTION:
    !   3D trilinear interpolation of value function V(K, S, D) for given z index.
    !
    ! INPUTS:
    !   iz      - Current z index (discrete)
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
    ! FUNCTION: interp_EV
    !
    ! DESCRIPTION:
    !   OPTIMIZED: Interpolates from precomputed EV_grid instead of V.
    !   This avoids the O(nz) summation inside the choice loop.
    !
    ! INPUTS:
    !   iz      - Current z index
    !   Kprime  - Next period K
    !   Sprime  - Next period S
    !   Dprime  - Next period D
    !
    ! OUTPUT:
    !   Expected value E[V(z',K',S',D') | z]
    !===================================================================================
    function interp_EV(iz, Kprime, Sprime, Dprime) result(EV)
        implicit none
        integer, intent(in) :: iz
        real(dp), intent(in) :: Kprime, Sprime, Dprime
        real(dp) :: EV
        integer :: iK, iS, iD
        real(dp) :: wK, wS, wD
        real(dp) :: E000, E001, E010, E011, E100, E101, E110, E111
        real(dp) :: E00, E01, E10, E11, E0, E1

        ! Locate on grids
        call locate_grid(grid_K, nK, Kprime, iK, wK)
        call locate_grid(grid_S, nS, Sprime, iS, wS)
        call locate_grid(grid_D, nD, Dprime, iD, wD)

        ! Trilinear interpolation from EV_grid (already has E[V|z])
        E000 = EV_grid(iz, iK,   iS,   iD)
        E001 = EV_grid(iz, iK,   iS,   iD+1)
        E010 = EV_grid(iz, iK,   iS+1, iD)
        E011 = EV_grid(iz, iK,   iS+1, iD+1)
        E100 = EV_grid(iz, iK+1, iS,   iD)
        E101 = EV_grid(iz, iK+1, iS,   iD+1)
        E110 = EV_grid(iz, iK+1, iS+1, iD)
        E111 = EV_grid(iz, iK+1, iS+1, iD+1)

        ! Interpolate along D
        E00 = (1.0_dp - wD) * E000 + wD * E001
        E01 = (1.0_dp - wD) * E010 + wD * E011
        E10 = (1.0_dp - wD) * E100 + wD * E101
        E11 = (1.0_dp - wD) * E110 + wD * E111

        ! Interpolate along S
        E0 = (1.0_dp - wS) * E00 + wS * E01
        E1 = (1.0_dp - wS) * E10 + wS * E11

        ! Interpolate along K
        EV = (1.0_dp - wK) * E0 + wK * E1

    end function interp_EV

    !===================================================================================
    ! FUNCTION: expect_V
    !
    ! DESCRIPTION:
    !   Computes expected continuation value E[V(z',K',S',D') | z]
    !   OPTIMIZED: Now just calls interp_EV which uses precomputed EV_grid.
    !
    !   OLD: O(nz * trilinear) = O(11 * 8) = 88 operations per call
    !   NEW: O(trilinear) = O(8) operations per call
    !   SPEEDUP: ~11x for this function
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

        ! Use precomputed expected value grid
        EV = interp_EV(iz, Kprime, Sprime, Dprime)

    end function expect_V

    !===================================================================================
    ! FUNCTION: expect_V_direct
    !
    ! DESCRIPTION:
    !   Direct computation of E[V|z] without precomputed grid.
    !   Use this if EV_grid is not yet computed (e.g., initialization).
    !===================================================================================
    function expect_V_direct(iz, Kprime, Sprime, Dprime) result(EV)
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

    end function expect_V_direct

    !===================================================================================
    ! SUBROUTINE: get_interp_weights
    !
    ! DESCRIPTION:
    !   Computes all interpolation weights at once for a given (K', S', D').
    !   Useful when the same point is used multiple times.
    !===================================================================================
    subroutine get_interp_weights(Kprime, Sprime, Dprime, iK, iS, iD, wK, wS, wD)
        implicit none
        real(dp), intent(in) :: Kprime, Sprime, Dprime
        integer, intent(out) :: iK, iS, iD
        real(dp), intent(out) :: wK, wS, wD

        call locate_grid(grid_K, nK, Kprime, iK, wK)
        call locate_grid(grid_S, nS, Sprime, iS, wS)
        call locate_grid(grid_D, nD, Dprime, iD, wD)

    end subroutine get_interp_weights

    !===================================================================================
    ! FUNCTION: trilinear_interp
    !
    ! DESCRIPTION:
    !   Generic trilinear interpolation given indices and weights.
    !   Avoids redundant grid location when weights are precomputed.
    !===================================================================================
    function trilinear_interp(arr, iz, iK, iS, iD, wK, wS, wD) result(val)
        implicit none
        real(dp), intent(in) :: arr(:,:,:,:)
        integer, intent(in) :: iz, iK, iS, iD
        real(dp), intent(in) :: wK, wS, wD
        real(dp) :: val
        real(dp) :: v000, v001, v010, v011, v100, v101, v110, v111
        real(dp) :: v00, v01, v10, v11, v0, v1

        v000 = arr(iz, iK,   iS,   iD)
        v001 = arr(iz, iK,   iS,   iD+1)
        v010 = arr(iz, iK,   iS+1, iD)
        v011 = arr(iz, iK,   iS+1, iD+1)
        v100 = arr(iz, iK+1, iS,   iD)
        v101 = arr(iz, iK+1, iS,   iD+1)
        v110 = arr(iz, iK+1, iS+1, iD)
        v111 = arr(iz, iK+1, iS+1, iD+1)

        v00 = (1.0_dp - wD) * v000 + wD * v001
        v01 = (1.0_dp - wD) * v010 + wD * v011
        v10 = (1.0_dp - wD) * v100 + wD * v101
        v11 = (1.0_dp - wD) * v110 + wD * v111

        v0 = (1.0_dp - wS) * v00 + wS * v01
        v1 = (1.0_dp - wS) * v10 + wS * v11

        val = (1.0_dp - wK) * v0 + wK * v1

    end function trilinear_interp

end module mod_interpolation