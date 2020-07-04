
program ex1
  implicit none
  integer, parameter :: n = 3
  double precision, allocatable, dimension(:) :: b,x,y
  double precision, allocatable, dimension(:,:) :: A,LU
  integer, allocatable, dimension(:) :: ipiv
  integer :: info
  double precision :: nrm2

  allocate(A(n,n),LU(n,n),b(n),x(n),y(n),ipiv(n))
  
  call random_number(A)
  x = 1.d0
  b = matmul(A,x)
  y = b
  LU = A
  call DGETRF(n,n,LU,n,ipiv,info)
  call DGETRS('N',n,1,LU,n,ipiv,y,n,info)

  x = x-y
  call compute_l2_norm(x,nrm2,n)
  write(*,*) "Norm of error: ", nrm2
  x = matmul(A,y)-b
  call compute_l2_norm(x,nrm2,n)
  write(*,*) "Norm of residual: ", nrm2

  deallocate(A,LU,b,x,y,ipiv)
  
end program ex1

subroutine compute_l2_norm(x,nrm2,n)
  implicit none
  integer :: i,n
  double precision :: nrm2
  double precision :: x(n)

  nrm2 = 0.0
  do i = 1,n
   nrm2 = nrm2 + x(i)**2 
  end do
  nrm2 = sqrt(nrm2)
end subroutine compute_l2_norm
