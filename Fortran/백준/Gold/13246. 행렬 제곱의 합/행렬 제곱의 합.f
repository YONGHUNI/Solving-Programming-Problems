program matrix_sum_power
  implicit none
  integer, parameter :: mod_val = 1000
  integer :: N, i, j
  integer(8) :: B
  ! 최대 크기가 5x5이므로 모든 행렬을 5x5로 선언
  integer :: A(5,5), P(5,5), S(5,5)

  ! 입력: 첫 줄에 N과 B, 그 후 N개의 줄에 행렬 A의 원소들
  read(*,*) N, B
  do i = 1, N
     read(*,*) (A(i,j), j = 1, N)
  end do

  ! A의 B제곱까지의 합 S = A^1 + A^2 + ... + A^B 및 A^B (P)에 대해 재귀적으로 계산
  call calcsum(B, N, A, P, S)

  ! 결과 행렬 S 출력 (각 원소는 이미 mod 1000 적용됨)
    do i = 1, N
       do j = 1, N
          write(*,'(I0)', advance='no') S(i,j)
          if (j < N) then
             write(*,'(A)', advance='no') ' '
          end if
       end do
       write(*,*)  ! 행의 끝에서는 줄바꿈
    end do

contains

  !---------------------------------------------------------------------
  ! 재귀 서브루틴 calcsum: 
  !   입력:  b, 행렬 크기 N, 행렬 A(5,5)
  !   출력:  P = A^b, S = A^1 + A^2 + ... + A^b   (모든 계산은 mod 1000)
  !---------------------------------------------------------------------
  recursive subroutine calcsum(b, N, A, P, S)
    implicit none
    integer(8), intent(in) :: b
    integer, intent(in)    :: N
    integer, intent(in)    :: A(5,5)
    integer, intent(out)   :: P(5,5), S(5,5)
    integer :: i, j
    integer :: temp_mat(5,5)
    integer(8) :: half
    integer :: P_half(5,5), S_half(5,5)
    integer :: P_prev(5,5), S_prev(5,5)

    if (b == 1_8) then
       do i = 1, N
          do j = 1, N
             P(i,j) = mod(A(i,j), mod_val)
             S(i,j) = mod(A(i,j), mod_val)
          end do
       end do
    else if (mod(b, 2_8) == 0_8) then
       half = b / 2
       call calcsum(half, N, A, P_half, S_half)
       call matmul_mod(P_half, P_half, N, P)
       call matmul_mod(P_half, S_half, N, temp_mat)
       call matadd_mod(S_half, temp_mat, N, S)
    else
       call calcsum(b - 1, N, A, P_prev, S_prev)
       call matmul_mod(P_prev, A, N, P)
       call matadd_mod(S_prev, P, N, S)
    end if
  end subroutine calcsum

  !---------------------------------------------------------------------
  ! 서브루틴 matmul_mod: 행렬 곱셈 (mod 1000)
  !   Z = X * Y (X, Y, Z는 5x5 행렬; N은 실제 연산에 사용할 크기)
  !---------------------------------------------------------------------
  subroutine matmul_mod(X, Y, N, Z)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: X(5,5), Y(5,5)
    integer, intent(out) :: Z(5,5)
    integer :: i, j, k, sum

    do i = 1, N
       do j = 1, N
          sum = 0
          do k = 1, N
             sum = sum + X(i,k) * Y(k,j)
          end do
          Z(i,j) = mod(sum, mod_val)
       end do
    end do
  end subroutine matmul_mod

  !---------------------------------------------------------------------
  ! 서브루틴 matadd_mod: 행렬 덧셈 (mod 1000)
  !   Z = X + Y (각 행렬은 5x5, N은 실제 연산에 사용할 크기)
  !---------------------------------------------------------------------
  subroutine matadd_mod(X, Y, N, Z)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: X(5,5), Y(5,5)
    integer, intent(out) :: Z(5,5)
    integer :: i, j

    do i = 1, N
       do j = 1, N
          Z(i,j) = mod(X(i,j) + Y(i,j), mod_val)
       end do
    end do
  end subroutine matadd_mod

end program matrix_sum_power
