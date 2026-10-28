module heapsort
  implicit real(A-H,O-Z)
  private

  public :: heapify
contains
 recursive subroutine heapify(arr,i,n)
    integer(4), intent(in) :: n
    integer(4), intent(in) :: i
    !integer(4), intent(out) :: ilargest
    real(8), intent(inout) :: arr(n)
    integer(4) iright,ileft,ilargest
    !******************************************
    ileft=2*i
    iright=2*i+1
    ilargest=i
    if (ileft>n) goto 1

    if (arr(ileft) > arr(ilargest)) ilargest=ileft
    if (ileft/=n) then
      if (arr(iright) > arr(ilargest)) then
        ilargest=iright
      endif
    endif
    
    if (ilargest/=i) then
      atmp=arr(ilargest)
      arr(ilargest)=arr(i)
      arr(i)=atmp
      call heapify(arr,ilargest,n)
    endif

1  end subroutine heapify
end module heapsort
