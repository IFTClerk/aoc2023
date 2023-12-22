MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  TYPE :: BRICK
     INTEGER, DIMENSION(3) :: start
     INTEGER, DIMENSION(3) :: end
  END TYPE BRICK

CONTAINS

SUBROUTINE SORT(tower, nb)
  TYPE(BRICK), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: tower
  TYPE(BRICK), DIMENSION(:), ALLOCATABLE :: stow
  INTEGER, INTENT(IN) :: nb
  INTEGER i, mi
  LOGICAL, DIMENSION(nb) :: msk

  msk = .TRUE.
  ALLOCATE(stow(nb))
  DO i=1,nb
     mi = MINLOC(tower%start(3), DIM=1, MASK=msk)
     stow(i) = tower(mi)
     msk(mi) = .FALSE.
  END DO
  CALL MOVE_ALLOC(stow, tower)
END SUBROUTINE SORT

SUBROUTINE READTOWER(lu, tower, nb)
  CHARACTER l*256
  INTEGER lu, ios, i, j, nb
  TYPE(BRICK), DIMENSION(:), ALLOCATABLE :: tower

  nb = NLINES(lu)
  ALLOCATE(tower(nb))
  DO j=1,nb
     READ(lu, "(A)", IOSTAT=ios) l
     IF (ios.EQ.IOSTAT_END) EXIT
     ! replace the tilde
     DO i=1,LEN_TRIM(l)
        IF (l(i:i).EQ.'~') THEN
           l(i:i) = ','
           EXIT
        END IF
     END DO
     READ(l, *) tower(j)
  END DO
  ! fix stupid indexing
  tower%start(1) = tower%start(1) + 1
  tower%start(2) = tower%start(2) + 1
  tower%end(1) = tower%end(1) + 1
  tower%end(2) = tower%end(2) + 1
END SUBROUTINE READTOWER

SUBROUTINE FALL(tower, nb, ocpd, nr, nc, nh)
  TYPE(BRICK), DIMENSION(nb), TARGET, INTENT(INOUT) :: tower
  TYPE(BRICK), POINTER :: br
  INTEGER sta(3), end(3)
  INTEGER, INTENT(IN) :: nb, nr, nc, nh
  INTEGER i, j, lb
  LOGICAL, DIMENSION(nr,nc,nh), INTENT(INOUT) :: ocpd

  DO i=1,nb
     br => tower(i)
     sta = br%start
     end = br%end
     DO j=1,sta(3)
        IF (.NOT.ANY(ocpd(sta(1):end(1), sta(2):end(2), j:sta(3)))) EXIT
     END DO
     lb = end(3) - sta(3)
     sta(3) = j
     end(3) = j + lb
     br%start(3) = sta(3)
     br%end(3) = end(3)

     ocpd(sta(1):end(1),sta(2):end(2),sta(3):end(3)) = .TRUE.
  END DO
END SUBROUTINE FALL

SUBROUTINE FINDSAFE(tower, nb, safe, ocpd, nr, nc, nh)
  TYPE(BRICK), DIMENSION(nb), TARGET, INTENT(IN) :: tower
  TYPE(BRICK), POINTER :: br, br2
  INTEGER i, j, sta(3), end(3)
  INTEGER, INTENT(IN) :: nb, nr, nc, nh
  LOGICAL, DIMENSION(nb), INTENT(OUT) :: safe
  LOGICAL, DIMENSION(nr,nc,nh), INTENT(IN) :: ocpd
  LOGICAL, DIMENSION(nr,nc) :: msk, slc

  safe = .TRUE.
  DO i=1,nb
     msk = .FALSE.
     br => tower(i)
     sta = br%start
     end = br%end
     slc = ocpd(:,:,end(3))
     msk(sta(1):end(1),sta(2):end(2)) = .TRUE.
     slc = slc.NEQV.msk ! this is XOR
     DO j=1,nb
        IF (tower(j)%start(3).NE.(end(3)+1)) CYCLE
        br2 => tower(j)
        IF (ANY(slc(br2%start(1):br2%end(1),br2%start(2):br2%end(2)))) THEN
           CYCLE
        ELSE
           safe(i) = .FALSE.
        END IF
     END DO
  END DO
END SUBROUTINE FINDSAFE

SUBROUTINE FINDNFALL(tower, nb, nfall, safe, ocpd, nr, nc, nh)
  TYPE(BRICK), DIMENSION(nb), TARGET, INTENT(IN) :: tower
  TYPE(BRICK), POINTER :: br, br2, br3
  INTEGER i, j, k, b, sta(3), end(3)
  INTEGER, INTENT(IN) :: nb, nr, nc, nh
  INTEGER, DIMENSION(nb), INTENT(OUT) :: nfall
  INTEGER, DIMENSION(:), ALLOCATABLE :: q
  LOGICAL flag
  LOGICAL, DIMENSION(nb), INTENT(OUT) :: safe
  LOGICAL, DIMENSION(nb) :: seen, fall
  LOGICAL, DIMENSION(nr,nc,nh), INTENT(IN) :: ocpd
  LOGICAL, DIMENSION(nr,nc) :: msk, msk2, slc, slc2

  nfall = 0
  ALLOCATE(q(0))
  DO i=1,nb
     IF (safe(i)) CYCLE
     q = [q, i]
     seen = .FALSE.
     fall = .FALSE.
     DO
        IF (SIZE(q).EQ.0) EXIT
        b = q(1)
        q = q(2:)
        br => tower(b)
        sta = br%start
        end = br%end

        msk = .FALSE.
        slc = ocpd(:,:,end(3)+1)
        msk(sta(1):end(1),sta(2):end(2)) = .TRUE.
        slc = slc.AND.msk
        DO j=1,nb
           ! check upwards
           IF (tower(j)%start(3).NE.(end(3)+1)) CYCLE
           br2 => tower(j)
           ! intersects the one that gets disintegrated
           IF (ANY(slc(br2%start(1):br2%end(1),br2%start(2):br2%end(2)))) THEN
              ! check downwards that it would actually fall
              slc2 = ocpd(:,:,br2%start(3)-1).NEQV.msk
              msk2 = .FALSE.
              msk2(br2%start(1):br2%end(1),br2%start(2):br2%end(2)) = .TRUE.
              slc2 = slc2.AND.msk2
              flag = .TRUE.
              DO k=1,nb
                 IF (tower(k)%end(3).NE.(br2%start(3)-1)) CYCLE
                 IF (fall(k)) CYCLE
                 br3 => tower(k)
                 IF (ANY(slc2(br3%start(1):br3%end(1),br3%start(2):br3%end(2)))) THEN
                    flag = .FALSE.
                 ELSE
                    CYCLE
                 END IF
              END DO
              IF (flag) THEN
                 fall(j) = .TRUE.
                 IF (.NOT.seen(j)) THEN
                    q = [q, j]
                    seen(j) = .TRUE.
                 END IF
              END IF
           END IF
        END DO
     END DO
     nfall(i) = COUNT(fall)
  END DO
END SUBROUTINE FINDNFALL

SUBROUTINE PART12()
  IMPLICIT NONE
  INTEGER s1, s2, nb, nr, nc, nh
  INTEGER, DIMENSION(:), ALLOCATABLE :: nfall
  TYPE(BRICK), DIMENSION(:), ALLOCATABLE :: tower
  LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: ocpd
  LOGICAL, DIMENSION(:), ALLOCATABLE :: safe

  s1 = 0
  s2 = 0
  OPEN(10, FILE=fin, STATUS='OLD')
  CALL READTOWER(10, tower, nb)
  CLOSE(10)
  CALL SORT(tower, nb)

  nr = MAXVAL(tower%end(1))
  nc = MAXVAL(tower%end(2))
  nh = MAXVAL(tower%end(3))
  ALLOCATE(ocpd(nr,nc,nh))
  ocpd = .FALSE.

  CALL FALL(tower, nb, ocpd, nr, nc, nh)
  CALL SORT(tower, nb)

  ALLOCATE(safe(nb))
  CALL FINDSAFE(tower, nb, safe, ocpd, nr, nc, nh)
  s1 = COUNT(safe)

  ALLOCATE(nfall(nb))
  CALL FINDNFALL(tower, nb, nfall, safe, ocpd, nr, nc, nh)
  s2 = SUM(nfall)

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s1
  WRITE(6,*) "-----------------"

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 2", s2
  WRITE(6,*) "-----------------"
END SUBROUTINE PART12

END MODULE MOD

PROGRAM MAIN
  USE MOD

  CALL PART12()
END PROGRAM MAIN
