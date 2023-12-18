MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, DIMENSION(2), PARAMETER :: UP=[-1,0],DN=[1,0],LT=[0,-1],RT=[0,1]
CONTAINS

SUBROUTINE PUSHCOL(mat, col)
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: mat, tmp
  INTEGER(KIND=INT64) col(2)
  INTEGER ns

  ns = SIZE(mat,DIM=2)
  ALLOCATE(tmp(2,ns+1))
  tmp(:,:ns) = mat
  tmp(:,ns+1) = col
  CALL MOVE_ALLOC(tmp, mat)
END SUBROUTINE PUSHCOL

SUBROUTINE DIG(dir, dis, vs)
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: dir
  INTEGER r
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: dis
  INTEGER(KIND=INT64) pos(2), mov(2)
  INTEGER ni, i, s
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: vs

  ni = SIZE(dir)
  IF (SIZE(dis).NE.ni) THEN
     PRINT *, "Instruction length mismatch"
     RETURN
  END IF

  ALLOCATE(vs(2,0))
  ! current position
  pos = [0,0]

  DO i=1,ni
     CALL PUSHCOL(vs, pos)
     r = dir(i)
     s = dis(i)
     SELECT CASE (r)
        CASE (0)
           mov = RT
        CASE (1)
           mov = DN
        CASE (2)
           mov = LT
        CASE (3)
           mov = UP
     END SELECT
     pos = pos + mov * s
  END DO
END SUBROUTINE DIG

FUNCTION AREA(vs)
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: vs
  INTEGER nv, i
  INTEGER(KIND=INT64) area

  area = 0
  nv = SIZE(vs,DIM=2)
  DO i=1,nv
     area = area + vs(1,1)*vs(2,2) - vs(1,2)*vs(2,1)
     vs = CSHIFT(vs,1,DIM=2)
  END DO
  area = ABS(area / 2)
END FUNCTION AREA

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  CHARACTER, DIMENSION(:), ALLOCATABLE :: dir
  INTEGER, DIMENSION(:), ALLOCATABLE :: idir
  INTEGER, DIMENSION(:), ALLOCATABLE :: dis
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: vs
  INTEGER(KIND=INT64) s
  INTEGER ios, nl, i

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  nl = NLINES(10)
  ALLOCATE(dir(nl))
  ALLOCATE(idir(nl))
  ALLOCATE(dis(nl))
  DO
     READ(10, *, IOSTAT=ios) (dir(i), dis(i), l, i=1,nl)
     IF (ios.EQ.IOSTAT_END) EXIT
  END DO
  CLOSE(10)

  WHERE (dir.EQ.'R')
     idir = 0
  ELSEWHERE (dir.EQ.'D')
     idir = 1
  ELSEWHERE (dir.EQ.'L')
     idir = 2
  ELSEWHERE (dir.EQ.'U')
     idir = 3
  END WHERE

  CALL DIG(idir, dis, vs)
  s = AREA(vs)
  s = s + SUM(dis)/2 + 1

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER, DIMENSION(:), ALLOCATABLE :: idir
  INTEGER, DIMENSION(:), ALLOCATABLE :: dis
  INTEGER(KIND=INT64), DIMENSION(:,:), ALLOCATABLE :: vs
  INTEGER(KIND=INT64) s
  INTEGER ios, nl, i, nh

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  nl = NLINES(10)
  ALLOCATE(idir(nl))
  ALLOCATE(dis(nl))
  DO i=1,nl
     READ(10, '(A)', IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     nh = INDEX(l, '#')

     READ(l(nh+1:nh+7), '(Z5,Z1)') dis(i), idir(i)
  END DO
  CLOSE(10)

  CALL DIG(idir, dis, vs)
  s = AREA(vs)
  s = s + SUM(dis)/2 + 1

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
