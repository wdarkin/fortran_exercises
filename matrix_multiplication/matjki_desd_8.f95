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
        do i=1, m, 8
          C(i,j)=C(i,j)+A(i,k)*B(k,j)
          C(i+1,j)=C(i+1,j)+A(i+1,k)*B(k,j)
          C(i+2,j)=C(i+2,j)+A(i+2,k)*B(k,j)
          C(i+3,j)=C(i+3,j)+A(i+3,k)*B(k,j)
          C(i+4,j)=C(i+4,j)+A(i+4,k)*B(k,j)
          C(i+5,j)=C(i+5,j)+A(i+5,k)*B(k,j)
          C(i+6,j)=C(i+6,j)+A(i+6,k)*B(k,j)
          C(i+7,j)=C(i+7,j)+A(i+7,k)*B(k,j)
        end do
      end do
    end do
    
t2=high_res_clock@(.FALSE.)
tempo = tempo + (t2-t1)
print *, "Tempo em ", loop, ":", t2-t1
end do

    print *, 'Matriz JKI - DESDOBRAMENTO 8 -> (media) calculada em: ', tempo / 10
END PROGRAM matjki