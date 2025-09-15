program sort3_fast_optimized
  implicit none
  integer, parameter :: Z = 10000         ! 최대 숫자 (0 ~ 10000)
  integer, parameter :: BUF_SIZE = 2000000  ! 출력 버퍼 크기 (문자 개수)
  
  integer :: N, i, k, num
  integer, dimension(0:Z) :: count
  character(len=BUF_SIZE) :: buffer
  integer :: pos
  
  ! 각 숫자의 문자열 표현(개행 포함)을 저장할 배열: 고정 길이 32 사용
  character(len=32), dimension(0:Z) :: rep
  ! 각 문자열의 실제 길이(개행 포함)를 저장할 배열
  integer, dimension(0:Z) :: rep_len
  character(len=32) :: tempStr

  ! count 배열 초기화
  count = 0

  ! 입력: 첫 줄에 N, 이후 N개의 줄에 숫자
  read(*,*) N
  do i = 1, N
     read(*,*) num
     count(num) = count(num) + 1
  end do

  ! 0부터 Z까지 각 숫자에 대해 문자열 표현(개행문자 포함)을 미리 구함  
  do i = 0, Z
     write(tempStr, '(I0)') i
     rep(i) = trim(tempStr) // new_line('A')
     rep_len(i) = len_trim(rep(i))
  end do

  ! 출력 버퍼 초기화
  buffer = ''
  pos = 1

  ! 0부터 Z까지 오름차순으로, 각 숫자를 등장 횟수만큼 버퍼에 누적
  do i = 0, Z
     if (count(i) > 0) then
        do k = 1, count(i)
           if (pos + rep_len(i) - 1 > BUF_SIZE) then
              ! 버퍼가 꽉 찼으면 지금까지 누적된 내용을 한 번에 출력
              write(*, '(A)', advance='no') buffer(1:pos-1)
              pos = 1
              buffer = ''
           end if
           buffer(pos:pos+rep_len(i)-1) = rep(i)(1:rep_len(i))
           pos = pos + rep_len(i)
        end do
     end if
  end do

  ! 남은 버퍼 내용 출력
  if (pos > 1) then
     write(*, '(A)', advance='no') buffer(1:pos-1)
  end if

end program sort3_fast_optimized
