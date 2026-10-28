program main
  use heapsort, only: heapify
  implicit real(A-H,O-Z)
  integer(4),parameter :: n=10
  integer(4) i,j,k
  real(8) arr(n)
  
  arr=(/4,10,5,2,-5,8,32,3,7,6/)
  
  do i=n/2,1,-1 
    call heapify(arr,i,n)
  enddo

  j=0

  do i=n-j,1,-1
    atmp=arr(1)
    arr(1)=arr(n-j)
    arr(n-j)=atmp
    j=j+1
    call heapify(arr,1,n-j)
  enddo
  print *,'sorted array : ',arr

end program main
