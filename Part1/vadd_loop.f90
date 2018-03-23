program vadd
  !$ use omp_lib
  IMPLICIT NONE
  integer, parameter :: n = 300
  integer :: i,j,k,nreps,nt,nthreads,nstride
  real(kind = 8), allocatable :: a(:),b(:),c(:)
  real(kind = 8) :: time1,time2
  nthreads = 24
  nstride = 10
  OPEN(unit = 29, file = 'parallel_do.txt' ,status="REPLACE")
  !$ do nt = 1,nthreads
  !$ call omp_set_num_threads(nt)
  !$ OMP PARALLEL DO COLLAPSE(3)                                                                                                     
  do k = 1,n,nstride
     allocate(a(k),b(k),c(k))
     a = 1.0d0
     b = 2.0d0
     nreps = 1000*n/k
     time1 = omp_get_wtime()
     do j = 1,nreps 
        do i = 1,k
           c(i) = a(i)+b(i)
	end do 
     end do
     time2 = omp_get_wtime()
     write(29,'(I3,I12,I12,2es12.4)') nt,k,nreps,(time2-time1),(time2-time1)/dble(k*nreps)
     deallocate(a,b,c)
  end do
  !$ OMP PARALLEL END DO
  !$ end do
  close(29)         

  OPEN(unit = 29, file = 'serial_do.txt' ,status="REPLACE")
  do k = 1,n,nstride
     allocate(a(k),b(k),c(k))
     a = 1.0d0
     b = 2.0d0
     nreps = 1000*n/k
     time1 = omp_get_wtime()
     do j = 1,nreps
        do i = 1,k
           c(i) = a(i)+b(i)
        end do
     end do
     time2 = omp_get_wtime()
     write(29,'(I3,I12,I12,2es12.4)') nt,k,nreps,(time2-time1),(time2-time1)/dble(k*nreps)
     deallocate(a,b,c)
  end do
  close(29)         


end program vadd
