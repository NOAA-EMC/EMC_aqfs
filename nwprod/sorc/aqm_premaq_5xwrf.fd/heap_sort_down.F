!-------------------------------------------------------------------------------
! Name:     Heap Sort
! Purpose:  Sorts ARRAY into descending numerical order by Heap sort method
! Notes:    Taken from "Numerical Recipes"
!           http://www.library.cornell.edu/nr/cornell_only/fortran.210/f90/recipes/sort_heap.f90
! Revision history:
!   01/24/05 David Wong
!     -- modified the code to sort array in descending order (original code 
!        is to sort array in ascending order) and avoid to use swap function
!-------------------------------------------------------------------------------

        SUBROUTINE heap_sort_down(array)

        IMPLICIT NONE

	INTEGER, DIMENSION(:), INTENT(INOUT) :: array

	INTEGER :: i,n,temp

	n=size(array)

	do i=n/2,1,-1
 	   call sift_down(i,n)
	end do

	do i=n,2,-1
           temp = array(1)
           array(1) = array(i)
           array(i) =temp
	   call sift_down(1,i-1)
	end do

	CONTAINS

	SUBROUTINE sift_down(l,r)

	INTEGER, INTENT(IN) :: l,r

	INTEGER :: j,jold
	INTEGER :: a

	a=array(l)
	jold=l
	j=l+l
	do
	   if (j > r) exit
	   if (j < r) then
!             if (array(j) < array(j+1)) j=j+1
	      if (array(j) >= array(j+1)) j=j+1
	   end if
!          if (a >= array(j)) exit
	   if (a <= array(j)) exit
	   array(jold)=array(j)
	   jold=j
	   j=j+j
	end do

	array(jold)=a

	END SUBROUTINE sift_down

        END SUBROUTINE heap_sort_down
