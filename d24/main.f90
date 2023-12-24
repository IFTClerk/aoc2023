MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! REAL(KIND=REAL64), PARAMETER :: minp=7, maxp=27
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  REAL(KIND=REAL128), PARAMETER :: minp=200000000000000_real128, maxp=400000000000000_real128

  INTEGER, PARAMETER :: INT128=SELECTED_INT_KIND(32)
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

RECURSIVE FUNCTION DET128(mat, n) RESULT(det)
  INTEGER, INTENT(IN) :: n
  INTEGER i, sgn
  REAL(KIND=REAL128), DIMENSION(n,n), INTENT(IN) :: mat
  REAL(KIND=REAL128), DIMENSION(n-1,n-1) :: minor
  REAL(KIND=REAL128) det

  IF (n.EQ.1) THEN
     det = mat(1,1)
  ELSE
     det = 0
     sgn = 1
     DO i=1,n
        minor(:,:(i-1)) = mat(2:, :i-1)
        minor(:,i:) = mat(2:, i+1:)
        det = det + sgn * mat(1,i) * DET128(minor, n-1)
        sgn = -sgn
     END DO
  END IF
END FUNCTION DET128

SUBROUTINE PART2
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER ios, ia, i, n
  INTEGER(KIND=INT128) s
  REAL(KIND=REAL128) det
  REAL(KIND=REAL128), DIMENSION(6,6) :: A, Ai
  REAL(KIND=REAL128), DIMENSION(6) :: B, sol
  REAL(KIND=REAL128), DIMENSION(3,3) :: pos, vel

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  i = 0
  DO
     i = i + 1
     IF (i.GT.3) EXIT
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     ia = INDEX(l, "@")
     l(ia:ia) = ','
     READ(l, *) pos(:,i), vel(:,i)
  END DO
  CLOSE(10)

  A = 0
  B = 0

  ! column 1
  A(:,1) = [REAL(KIND=REAL128) :: (vel(2,i)-vel(2,1), 0, vel(3,1)-vel(3,i), i=2,3)]
  ! column 2
  A(:,2) = [REAL(KIND=REAL128) :: (vel(1,1)-vel(1,i), vel(3,i)-vel(3,1), 0, i=2,3)]
  ! column 3
  A(:,3) = [REAL(KIND=REAL128) :: (0, vel(2,1)-vel(2,i), vel(1,i)-vel(1,1), i=2,3)]
  ! column 4
  A(:,4) = [REAL(KIND=REAL128) :: (pos(2,1)-pos(2,i), 0, pos(3,i)-pos(3,1), i=2,3)]
  ! column 5
  A(:,5) = [REAL(KIND=REAL128) :: (pos(1,i)-pos(1,1), pos(3,1)-pos(3,i), 0, i=2,3)]
  ! column 6
  A(:,6) = [REAL(KIND=REAL128) :: (0, pos(2,i)-pos(2,1), pos(1,1)-pos(1,i), i=2,3)]
  ! CALL PRINTMAT(A)
  ! B is a single column vector
  B(:) = [REAL(KIND=REAL128) :: (pos(2,1)*vel(1,1)-pos(1,1)*vel(2,1)+pos(1,i)*vel(2,i)-pos(2,i)*vel(1,i), &
                                  pos(3,1)*vel(2,1)-pos(2,1)*vel(3,1)+pos(2,i)*vel(3,i)-pos(3,i)*vel(2,i), &
                                  pos(1,1)*vel(3,1)-pos(3,1)*vel(1,1)+pos(3,i)*vel(1,i)-pos(1,i)*vel(3,i), &
                                  i=2,3)]

  ! Cramer's rule
  n = 6
  det = DET128(A,n)
  DO i=1,3
     Ai = A
     Ai(:,i) = B(:)
     sol(i) = DET128(Ai,n) / det
  END DO

  ! take nearest integer because precision issues
  s = SUM(NINT(sol, KIND=INT128))

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART2

END MODULE MOD

PROGRAM MAIN
  USE MOD

  ! CALL PART1()
  CALL PART2()
END PROGRAM MAIN
