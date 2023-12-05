MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! INTEGER, PARAMETER :: nseeds=4
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
  INTEGER, PARAMETER :: nseeds=20
CONTAINS

INTEGER FUNCTION NEXTBLOCK(lu)
  CHARACTER l*256
  INTEGER lu, i, ios

  nextblock = 0
  DO
     READ(lu, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     IF (LEN(TRIM(l)).EQ.0) EXIT
     nextblock = nextblock + 1
  END DO

  ! rewind
  DO i=1,nextblock+1
     BACKSPACE(lu)
  END DO

END FUNCTION NEXTBLOCK

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER(KIND=int64) seeds(nseeds), loc(nseeds)
  INTEGER ios, ic, nb, i, j
  INTEGER(KIND=int64) s
  INTEGER(KIND=int64), DIMENSION(:), ALLOCATABLE :: srcs, srcd, dest
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: map
  LOGICAL, DIMENSION(:), ALLOCATABLE :: insrc

  OPEN(10, FILE=fin, STATUS='OLD')
  s = 0

  ! Read in seeds
  READ(10, "(A)", IOSTAT=ios) l
  IF (ios.EQ.IOSTAT_END) WRITE(6,*) "Failed to read seeds"
  ic = SCAN(l, ":")
  READ(l(ic+1:), *) seeds
  loc = seeds

  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! start of new map
     IF (INDEX(l,":").GT.0) THEN
        nb = NEXTBLOCK(10)
     ELSE
        CYCLE
     END IF

     ! read in new map
     ALLOCATE(map(nb, 3))
     ALLOCATE(srcs(nb))
     ALLOCATE(srcd(nb))
     ALLOCATE(dest(nb))
     ALLOCATE(insrc(nb))
     DO i=1,nb
        READ(10,*) map(i,:)
     END DO

     srcs = map(:,2)
     srcd = map(:,2) + map(:,3)
     dest = map(:,1)
     DO i=1,nseeds
        ! find where the seed belongs in the intervals
        insrc = (srcs.LE.loc(i) .AND. srcd.GT.loc(i))
        DO j=1,nb
           IF (insrc(j)) THEN
              ! offset by correct amount
              loc(i) = dest(j) + loc(i) - srcs(j)
           END IF
        END DO
     END DO

     ! PRINT*, loc

     DEALLOCATE(map)
     DEALLOCATE(srcs)
     DEALLOCATE(srcd)
     DEALLOCATE(dest)
     DEALLOCATE(insrc)
  END DO

  CLOSE(10)

  s = MINVAL(loc)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

! INTEGER(KIND=int64) FUNCTION MININTVL(lu)
!   INTEGER lu, ios
!   INTEGER(KIND=int64) sdi(3)

!   minintvl = -1

!   DO
!      READ(lu, *, IOSTAT=ios) sdi
!      IF (ios.EQ.IOSTAT_END) EXIT
!      IF (ios.GT.0) CYCLE
!      IF (minintvl.LT.0) THEN
!         minintvl = sdi(3)
!      ELSE IF (sdi(3).LT.minintvl) THEN
!         minintvl = sdi(3)
!      END IF
!   END DO

!   REWIND(lu)
! END FUNCTION MININTVL

SUBROUTINE XTEND2(arr, i1, i2)
  INTEGER i, larr
  INTEGER(KIND=int64) i1, i2
  INTEGER(KIND=int64), DIMENSION(:), ALLOCATABLE :: arr, tmp

  ! create temp array
  larr = SIZE(arr)
  ALLOCATE(tmp(larr+2))
  FORALL(i=1:larr) tmp(i)=arr(i)
  tmp(larr+1) = i1
  tmp(larr+2) = i2

  ! reallocate array
  DEALLOCATE(arr)
  ALLOCATE(arr(larr+2))

  ! copy over temp array
  FORALL(i=1:larr+2) arr(i)=tmp(i)

  ! delete temp array
  DEALLOCATE(tmp)
END SUBROUTINE XTEND2

SUBROUTINE SORTMAP(map, nb)
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: smap
  LOGICAL, DIMENSION(nb) :: msk
  INTEGER minidx, nb, i

  msk = .TRUE.
  ALLOCATE(smap(nb,3))
  DO i=1,nb
     minidx = MINLOC(map(:,2), DIM=1, MASK=msk)
     smap(i,:) = map(minidx,:)
     msk(minidx) = .FALSE.
  END DO
  ! swap
  FORALL(i=1:nb) map(i,:) = smap(i,:)
  DEALLOCATE(smap)
END SUBROUTINE SORTMAP

SUBROUTINE FULLMAP(map, nb)
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: fmap
  INTEGER(KIND=int64) mmax
  INTEGER nb, i, nmiss, offset

  ! find how many intervals are missing
  nmiss = 0
  mmax = 0
  DO i=1,nb
     ! disjoint bits in the map
     IF (map(i,2).NE.mmax) nmiss = nmiss + 1
     mmax = map(i,2) + map(i,3)
  END DO

  ALLOCATE(fmap(nb+nmiss, 3))
  offset = 0
  mmax = 0_int64
  DO i=1,nb
     ! fill in missing entries
     IF (map(i,2).NE.mmax) THEN
        ! insert one row
        fmap(i+offset,:) = [mmax, mmax, map(i,2)-mmax]
        offset = offset + 1
     END IF
     fmap(i+offset,:) = map(i,:)
     mmax = map(i,2) + map(i,3)
  END DO
  DEALLOCATE(map)
  ALLOCATE(map(nb+nmiss, 3))
  ! swap
  FORALL(i=1:nb+nmiss) map(i,:) = fmap(i,:)
  DEALLOCATE(fmap)
  nb = nb + nmiss
END SUBROUTINE FULLMAP

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER l*256
  INTEGER(KIND=int64) seeds(nseeds)
  INTEGER ios, ic, nb, i, j, nloc
  INTEGER(KIND=int64) s, tloc
  INTEGER(KIND=int64), DIMENSION(:), ALLOCATABLE :: srcs, srcd, srci, dest, loc
  INTEGER(KIND=int64), DIMENSION(:,:), ALLOCATABLE :: map
  LOGICAL, DIMENSION(:), ALLOCATABLE :: insrc

  OPEN(10, FILE=fin, STATUS='OLD')
  s = 0

  ! Read in seeds
  READ(10, "(A)", IOSTAT=ios) l
  IF (ios.EQ.IOSTAT_END) WRITE(6,*) "Failed to read seeds"
  ic = SCAN(l, ":")
  READ(l(ic+1:), *) seeds

  ALLOCATE(loc(nseeds))
  loc = seeds
  nloc = nseeds

  DO
     READ(10, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT

     ! start of new map
     IF (INDEX(l,":").GT.0) THEN
        nb = NEXTBLOCK(10)
     ELSE
        CYCLE
     END IF

     ! read in new map
     ALLOCATE(map(nb, 3))
     ALLOCATE(srcs(nb))
     ALLOCATE(srcd(nb))
     ALLOCATE(dest(nb))
     ALLOCATE(insrc(nb))
     DO i=1,nb
        READ(10,*) map(i,:)
     END DO

     CALL SORTMAP(map, nb)
     CALL FULLMAP(map, nb)
     ! PRINT *, "NEW MAP!"
     ! PRINT *, map
     ! PRINT *

     srcs = map(:,2)
     srcd = map(:,2) + map(:,3)
     srci = map(:,3)
     dest = map(:,1)

     i = 1
     ! loop over seeds in an indeterminate loop
     DO
        ! find where the seed belongs in the intervals
        insrc = (srcs.LE.loc(i) .AND. srcd.GT.loc(i))
        DO j=1,nb
        ! we sorted the map before so this will work
           IF (.NOT.insrc(j)) CYCLE
           ! crosses boundary, time to split
           IF (srcd(j).LT.(loc(i)+loc(i+1))) THEN
              ! seeds overflow the interval
              tloc = loc(i+1)
              loc(i+1) = srcd(j) - loc(i)
              CALL XTEND2(loc, srcd(j), loc(i)+tloc-srcd(j))
              nloc = nloc + 2
           END IF
           ! PRINT *, "------------"
           ! PRINT *, loc
           ! PRINT *, "------------"
           ! offset by correct amount
           loc(i) = dest(j) + loc(i) - srcs(j)
           EXIT
        END DO
        ! detect loop end
        IF ((i+2).GT.nloc) EXIT
        i = i + 2
     END DO

     ! PRINT*, loc

     DEALLOCATE(map)
     DEALLOCATE(srcs)
     DEALLOCATE(srcd)
     DEALLOCATE(dest)
     DEALLOCATE(insrc)
  END DO

  CLOSE(10)

  s = MINVAL(loc(1:nloc:2))

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
