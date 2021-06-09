!==================================================================
!COMPILE USING gfortran -c MODULE_GRID.f90

!==================================================================
MODULE MODULE_GRID
CONTAINS
!==================================================================
! SUBROUTINE TO GENERATE 1D STRETCHED GRID WITH N1 POINTS
! This subroutine basically divides the domain of length 1 unit from 0 to 1 in N1 number points
! According to the requirement, the discretized points have to be multiplied with length H to have a grid of size H. If this is not multiplied, it wil simply create the mesh from 0 to 1 
 !==================================================================
SUBROUTINE SGRID_GENERATOR(N1,ALPHA,YPAD)
IMPLICIT NONE 
REAL(8),INTENT(IN)				::ALPHA
INTEGER,INTENT(IN)				::N1
REAL(8),DIMENSION(N1),INTENT(INOUT)		::YPAD
INTEGER						::J
REAL(8)						::ALPHA1,ALPHA2
INTEGER						::N05,N06
REAL(8)						::COEF
WRITE(*,3)N1,ALPHA
3 FORMAT('GRID: NOMBRE DE POINTS: ',I3,' COEFFICIENT ALPHA: ',F7.3)
ALPHA1=ALPHA
ALPHA2=ALPHA
N05=(N1+1)/2
N06=N05+1
COEF=DEXP(ALPHA1*(1.-N05))
DO J=2,N05
YPAD(J)=.5*(DEXP(ALPHA1*(J-N05))-COEF)/(1.-COEF)
ENDDO
COEF=DEXP(ALPHA2*(N05-N1))
DO J=N06,N1-1
  YPAD(J)=(1.-.5*(DEXP(ALPHA2*(N05-J))-COEF)/(1.-COEF))
ENDDO
YPAD(1)=-YPAD(2)
YPAD(N1)=2.-YPAD(N1-1)
END SUBROUTINE
END MODULE
