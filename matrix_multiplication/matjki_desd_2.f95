PROGRAM matjki
implicit none
integer, parameter :: m = 512, r = 512, n = 512
real, dimension(m, r) :: A
real, dimension(r, n) :: B
real, dimension(m, n) :: C
integer :: i, j, k, loop
real(kind=2) :: t1, t2, high_res_clock@, tempo

C=0

call random_number(A)
call random_number(B)

t1=0
t2=0
tempo=0

do loop=1, 10
t1=high_res_clock@(.TRUE.)

    do j=1, n
      do k=1, r
        do i=1, m, 2
          C(i,j)=C(i,j)+A(i,k)*B(k,j)
          C(i+1,j)=C(i+1,j)+A(i+1,k)*B(k,j)
        end do
      end do
    end do
    
t2=high_res_clock@(.FALSE.)
tempo = tempo + (t2-t1)
print *, "Tempo em ", loop, ":", t2-t1
end do

    print *, 'Matriz JKI - DESDOBRAMENTO 2 -> (media) calculada em: ', tempo / 10
END PROGRAM matjki