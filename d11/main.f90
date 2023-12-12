MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  INTEGER vac
CONTAINS

SUBROUTINE PUSHV(arr, v)
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: arr, tmp
  INTEGER v(2), nr, nc

  IF (.NOT.ALLOCATED(arr)) THEN
     ALLOCATE(arr(2,1))
     arr(:,1) = v
     RETURN
  END IF

  nr = SIZE(arr, DIM=1)
  nc = SIZE(arr, DIM=2)
  ALLOCATE(tmp(nr,nc+1))

  tmp(:,1:nc) = arr
  tmp(:,nc+1) = v

  DEALLOCATE(arr)
  ALLOCATE(arr(nr,nc+1))
  arr = tmp
  DEALLOCATE(tmp)
END SUBROUTINE PUSHV

SUBROUTINE READMAP(map, nr, nc, gals)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  CHARACTER(LEN=256) l
  INTEGER ios, nr, nc, i, j
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: gals
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: msk

  nr = NLINES(fin)
  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(10)

  ALLOCATE(map(nr, nc))
  ALLOCATE(msk(nr, nc))

  DO j=1,nr
     READ(10, "(*(A))") (map(j,i), i=1,nc)
  END DO
  ! find galaxies
  msk = map.EQ.'#'
  DO i=1,nr
     DO j=1,nc
        IF (msk(i,j)) CALL PUSHV(gals, [i,j])
     END DO
  END DO

  CLOSE(10)
END SUBROUTINE READMAP

PURE INTEGER FUNCTION L1(sep)
  INTEGER, INTENT(IN) :: sep(2)

  ! Don't we all love Manhattan
  l1 = SUM(ABS(sep))
END FUNCTION L1

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER, DIMENSION(:), ALLOCATABLE :: emptyc, emptyr
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: gals
  INTEGER nr, nc, i, j, ng, e1, e2
  ! I DIDNT REALISE THE ANSEWR WOULD
  ! OVERFLOW WITH INT32 FOR >30 MINS
  INTEGER(KIND=int64) dist, s
  INTEGER gal(2), gal2(2)

  s = 0
  OPEN(10, FILE=fin, STATUS='OLD')

  CALL READMAP(map, nr, nc, gals)

  CLOSE(10)
  ! CALL PRINTMTX(map)

  ALLOCATE(emptyc(nc))
  ALLOCATE(emptyr(nr))
  emptyc = 0
  emptyr = 0
  DO i=nc,1,-1
     IF (ALL(map(:,i).EQ.'.')) THEN
        ! empty column
        emptyc(i) = i
     END IF
  END DO
  DO i=nr,1,-1
     IF (ALL(map(i,:).EQ.'.')) THEN
        ! empty row
        emptyr(i) = i
     END IF
  END DO

  ng = SIZE(gals, DIM=2)
  DO i=1,ng-1
     gal = gals(:,i)
     DO j=i+1,ng
        gal2 = gals(:,j)
        e1 = COUNT(emptyr.GT.MIN(gal(1),gal2(1)) .AND. &
             emptyr.LT.MAX(gal(1),gal2(1)))
        e2 = COUNT(emptyc.GT.MIN(gal(2),gal2(2)) .AND. &
             emptyc.LT.MAX(gal(2),gal2(2)))
        dist = L1(gal-gal2) + (e1 + e2)*(vac-1)

        s = s + dist
     END DO
  END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

END MODULE MOD

PROGRAM MAIN
  USE MOD

  vac = 2
  CALL PART1()
  ! actually part 2
  vac = 1000000
  CALL PART1()
END PROGRAM MAIN
