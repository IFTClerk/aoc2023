MODULE MOD
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"
CONTAINS

ELEMENTAL INTEGER FUNCTION HANDSTR(hand)
  CHARACTER(LEN=5), INTENT(in) :: hand
  CHARACTER :: harr(5)
  LOGICAL :: msk(5), omsk(5)
  INTEGER i, str

  ! pair = 1, triplet = 3, four = 5, five = 6
  handstr=0
  msk = .FALSE.

  ! fucking fortran characters
  FORALL(i=1:5) harr(i) = hand(i:i)
  DO i=1,5
     ! skip if masked as true
     IF (msk(i)) CYCLE
     ! save old mask
     omsk = msk
     msk = harr.EQ.harr(i)
     ! figure out strength
     str = COUNT(msk)
     ! go backwards because we dont want to lowball
     IF (str.EQ.5) THEN
        handstr = handstr + 6
     ELSE IF (str.EQ.4) THEN
        handstr = handstr + 5
     ELSE IF (str.EQ.3) THEN
        handstr = handstr + 3
     ELSE IF (str.EQ.2) THEN
        handstr = handstr + 1
     END IF
     ! mask processed ones
     msk = omsk.OR.msk
  END DO
END FUNCTION HANDSTR

ELEMENTAL INTEGER FUNCTION HANDSTR2(hand)
  CHARACTER(LEN=5), INTENT(in) :: hand
  CHARACTER :: harr(5)
  LOGICAL :: msk(5), omsk(5), mskj(5)
  INTEGER i, str, maxj, subj

  ! pair = 1, triplet = 3, four = 5, five = 6
  handstr2 = 0
  msk = .FALSE.
  ! fucking fortran characters
  FORALL(i=1:5) harr(i) = hand(i:i)
  subj = 0
  IF (MODULO(COUNT(harr.EQ.'J'), 5).GT.0) THEN
     ! we have some wild cards
     maxj = 0
     ! first pass
     DO i=1,5
        mskj = (harr.EQ.harr(i)) .OR. (harr.EQ.'J')
        IF (COUNT(mskj).GT.maxj) THEN
           maxj = COUNT(mskj)
           subj = i
        END IF
     END DO
  END IF
  ! second pass
  DO i=1,5
     ! skip if masked as true
     IF (msk(i)) CYCLE
     ! or if we are using J as something else
     IF (subj.GT.0 .AND. harr(i).EQ.'J') CYCLE
     ! save old mask
     omsk = msk
     IF (subj.EQ.0) THEN
        ! consider as if normal
        msk = harr.EQ.harr(i)
     ELSE IF ((subj.GT.0) .AND. (i.EQ.subj)) THEN
        ! include wildcards
        msk = (harr.EQ.harr(i)) .OR. (harr.EQ.'J')
     ELSE IF ((subj.GT.0) .AND. (i.NE.subj)) THEN
        msk = harr.EQ.harr(i)
     END IF
     ! figure out strength
     str = COUNT(msk)
     ! go backwards because we dont want to lowball
     IF (str.EQ.5) THEN
        handstr2 = handstr2 + 6
     ELSE IF (str.EQ.4) THEN
        handstr2 = handstr2 + 5
     ELSE IF (str.EQ.3) THEN
        handstr2 = handstr2 + 3
     ELSE IF (str.EQ.2) THEN
        handstr2 = handstr2 + 1
     END IF
     ! mask processed ones
     msk = omsk.OR.msk
  END DO
END FUNCTION HANDSTR2

SUBROUTINE SORTHAND(hnd, bid, str, nh)
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: hnd, shnd, sshnd
  INTEGER, DIMENSION(:), ALLOCATABLE :: bid, str
  INTEGER, DIMENSION(:), ALLOCATABLE :: sbid, sstr
  INTEGER, DIMENSION(:), ALLOCATABLE :: ssbid, ssstr
  LOGICAL, DIMENSION(nh) :: msk
  INTEGER nh, maxi, i, nm

  msk = .TRUE.
  ALLOCATE(shnd(nh))
  ALLOCATE(sbid(nh))
  ALLOCATE(sstr(nh))
  ! sort
  DO i=1,nh
     maxi = MAXLOC(str, DIM=1, MASK=msk)
     shnd(i) = hnd(maxi)
     sbid(i) = bid(maxi)
     sstr(i) = str(maxi)
     msk(maxi) = .FALSE.
  END DO
  ! break degeneracies
  DO i=0,6
     msk = sstr.EQ.i
     IF (COUNT(msk).GT.1) THEN
        ! FUCKING HELL I DONT KNOW HOW TO POINTERS
        nm = COUNT(msk)
        ALLOCATE(sshnd(nm))
        ALLOCATE(ssbid(nm))
        ALLOCATE(ssstr(nm))
        sshnd = PACK(shnd, msk)
        ssbid = PACK(sbid, msk)
        ssstr = PACK(sstr, msk)
        CALL SORTHAND2(sshnd, ssbid, ssstr, nm)
        ! recombine
        shnd = UNPACK(sshnd, msk, shnd)
        sbid = UNPACK(ssbid, msk, sbid)
        sstr = UNPACK(ssstr, msk, sstr)
        ! ...
        DEALLOCATE(sshnd)
        DEALLOCATE(ssbid)
        DEALLOCATE(ssstr)
     END IF
  END DO
  ! swap
  FORALL(i=1:nh) hnd(i) = shnd(i)
  FORALL(i=1:nh) bid(i) = sbid(i)
  FORALL(i=1:nh) str(i) = sstr(i)
  ! I don't technically need this because they should auto-
  ! matically be destroyed at the end of this subroutine?
  DEALLOCATE(shnd)
  DEALLOCATE(sbid)
  DEALLOCATE(sstr)
END SUBROUTINE SORTHAND

SUBROUTINE SORTHAND2(hnd, bid, str, nh)
  CHARACTER(LEN=5), DIMENSION(nh) :: hnd, shnd
  INTEGER, DIMENSION(nh) :: bid, str
  INTEGER, DIMENSION(nh) :: sbid, sstr
  CHARACTER(LEN=5), DIMENSION(nh) :: thnd
  LOGICAL, DIMENSION(nh) :: msk
  INTEGER nh, maxi, i, j

  msk = .TRUE.
  DO i=1,nh
     DO j=1,5
     ! haha
        IF (hnd(i)(j:j).EQ.'A') THEN
           thnd(i)(j:j) = 'X'
        ELSE IF (hnd(i)(j:j).EQ.'K') THEN
           thnd(i)(j:j) = 'W'
        ELSE IF (hnd(i)(j:j).EQ.'Q') THEN
           thnd(i)(j:j) = 'V'
        ELSE IF (hnd(i)(j:j).EQ.'J') THEN
           ! uncomment this for part 1
           ! thnd(i)(j:j) = 'U'
           ! uncomment this for part 2
           thnd(i)(j:j) = '1'
        ELSE
           thnd(i)(j:j) = hnd(i)(j:j)
        END IF
     END DO
  END DO
  ! sort
  DO i=1,nh
     maxi = MAXLOC(thnd, DIM=1, MASK=msk)
     shnd(i) = hnd(maxi)
     sbid(i) = bid(maxi)
     sstr(i) = str(maxi)
     msk(maxi) = .FALSE.
  END DO
  ! swap
  FORALL(i=1:nh) hnd(i) = shnd(i)
  FORALL(i=1:nh) bid(i) = sbid(i)
  FORALL(i=1:nh) str(i) = sstr(i)
END SUBROUTINE SORTHAND2

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: hnd
  INTEGER, DIMENSION(:), ALLOCATABLE :: bid, str, rank
  INTEGER ios, s, nl, i

  s = 0
  nl = NLINES(fin)
  ALLOCATE(hnd(nl))
  ALLOCATE(bid(nl))
  ALLOCATE(str(nl))
  ALLOCATE(rank(nl))

  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, "(A5,1X,I4)", IOSTAT=ios) (hnd(i), bid(i), i=1,nl)
  CLOSE(10)

  str = HANDSTR(hnd)
  CALL SORTHAND(hnd, bid, str, nl)
  rank = [(i, i=nl,1,-1)]
  s = SUM(rank * bid)

  ! DO i=1,nl
  !    WRITE(6, '(1X, I0)', ADVANCE="no") rank(i)
  !    WRITE(6, '(1X, A5, 5X)', ADVANCE="no") hnd(i)
  !    WRITE(6, *) bid(i)
  ! END DO

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: hnd
  INTEGER, DIMENSION(:), ALLOCATABLE :: bid, str, rank
  INTEGER ios, s, nl, i

  s = 0
  nl = NLINES(fin)
  ALLOCATE(hnd(nl))
  ALLOCATE(bid(nl))
  ALLOCATE(str(nl))
  ALLOCATE(rank(nl))

  OPEN(10, FILE=fin, STATUS='OLD')
  READ(10, "(A5,1X,I4)", IOSTAT=ios) (hnd(i), bid(i), i=1,nl)
  CLOSE(10)

  str = HANDSTR2(hnd)
  CALL SORTHAND(hnd, bid, str, nl)
  rank = [(i, i=nl,1,-1)]
  s = SUM(rank * bid)

  ! DO i=1,nl
  !    WRITE(6, '(1X, I0)', ADVANCE="no") rank(i)
  !    WRITE(6, '(1X, A5, 5X)', ADVANCE="no") hnd(i)
  !    WRITE(6, *) str(i)
  ! END DO

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
