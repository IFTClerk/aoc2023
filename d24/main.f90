MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! REAL(KIND=REAL64), PARAMETER :: minp=7, maxp=27
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  REAL(KIND=REAL128), PARAMETER :: minp=200000000000000_real64, maxp=400000000000000_real64

CONTAINS

ELEMENTAL LOGICAL FUNCTION INAREA(val)
  REAL(KIND=REAL128), INTENT(IN) :: val

  inarea = (val.GE.minp) .AND. (val.LE.maxp)
END FUNCTION INAREA

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER ios, s, nl, ia, i, j
  REAL(KIND=REAL128), DIMENSION(:,:), ALLOCATABLE :: pos, vel
  REAL(KIND=REAL128) x1, x2, y1 ,y2, vx1, vx2, vy1, vy2
  REAL(KIND=REAL128) intx(2), di(2), dj(2)

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  nl = NLINES(10)
  ALLOCATE(pos(3,nl))
  ALLOCATE(vel(3,nl))
  i = 0
  DO
     i = i + 1
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     ia = INDEX(l, "@")
     l(ia:ia) = ','
     READ(l, *) pos(:,i), vel(:,i)
  END DO
  CLOSE(10)

  DO i=1,nl
     DO j=1,nl
        IF (i.GE.j) CYCLE
        ! IF (DOT_PRODUCT(vel(1:2,i),vel(1:2,j)).LE.0) CYCLE
        ! IF (DOT_PRODUCT(vel(1:2,i)-vel(1:2,j),pos(1:2,i)-pos(1:2,j)).GE.0) CYCLE
        x1 = pos(1,i)
        y1 = pos(2,i)
        x2 = pos(1,j)
        y2 = pos(2,j)
        vx1 = vel(1,i)
        vy1 = vel(2,i)
        vx2 = vel(1,j)
        vy2 = vel(2,j)

        intx(1) = -(x1*(y1+vy1)-y1*(x1+vx1)) * vx2 + vx1 * (x2*(y2+vy2)-y2*(x2+vx2))
        intx(1) = intx(1) / (vx1*vy2 - vy1*vx2)
        intx(2) = -(x1*(y1+vy1)-y1*(x1+vx1)) * vy2 + vy1 * (x2*(y2+vy2)-y2*(x2+vx2))
        intx(2) = intx(2) / (vx1*vy2 - vy1*vx2)

        ! print *, intx
        IF (ALL(INAREA(intx))) THEN
           IF (DOT_PRODUCT(intx-pos(1:2,i),vel(1:2,i)).LT.0) CYCLE
           IF (DOT_PRODUCT(intx-pos(1:2,j),vel(1:2,j)).LT.0) CYCLE
           s = s + 1
        END IF
     END DO
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2
  IMPLICIT NONE

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Go use Mathematica"
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART1()
  ! CALL PART2()
END PROGRAM MAIN
