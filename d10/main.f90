MODULE MOD
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE UTILS
  IMPLICIT NONE

  ! CHARACTER*(*), PARAMETER :: fin = "test01.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test02.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test03.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test04.txt"
  ! CHARACTER*(*), PARAMETER :: fin = "test05.txt"
  CHARACTER*(*), PARAMETER :: fin = "input.txt"

  CHARACTER, DIMENSION(8) :: dirs=['U','D','T','R','1','2','3','4']
  CHARACTER, DIMENSION(4) :: turns=['F','7','J','L']
  INTEGER, DIMENSION(2,4) :: brain=RESHAPE([-1,0,0,1,1,0,0,-1], [2,4])
  INTEGER, DIMENSION(2,4) :: bbrain=RESHAPE([0,-1,-1,0,0,1,1,0], [2,4])
  INTEGER, DIMENSION(2,2) :: rot=RESHAPE([0,-1,1,0], [2,2])

CONTAINS

SUBROUTINE READMAP(map, nr, nc, sr, sc)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  CHARACTER(LEN=256) l
  INTEGER ios, nr, nc, i, j, sr, sc

  OPEN(10, FILE=fin, STATUS='OLD')
  nr = NLINES(10)
  READ(10, "(A)", IOSTAT=ios) l
  nc = LEN_TRIM(l)
  BACKSPACE(10)

  ALLOCATE(map(nr, nc))

  DO j=1,nr
     READ(10, "(*(A))") (map(j,i), i=1,nc)
     ! find start while we are at it
     IF (ANY(map(j,:).EQ.'S')) THEN
        sr = j
        sc = FINDLOC(map(j,:), 'S', DIM=1)
     END IF
  END DO

  CLOSE(10)
END SUBROUTINE READMAP

INTEGER FUNCTION FOLLOW(map, sr, sc, dir) RESULT(pl)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  CHARACTER tile
  INTEGER dir(2), loc(2), sr, sc

  loc = [sr, sc]
  pl = 1
  DO
     loc = loc + dir
     tile = map(loc(1), loc(2))
     IF (tile.EQ.'S') EXIT
     pl = pl + 1

     IF (ANY(tile.EQ.turns)) THEN
        ! dont worry about it
        dir = (DOT_PRODUCT(dir, PACK(brain(:,FINDLOC(turns,tile)),.TRUE.)) - &
               DOT_PRODUCT(dir, PACK(bbrain(:,FINDLOC(turns,tile)),.TRUE.))) * &
               MATMUL(rot,dir)
        IF (ALL(dir.EQ.0)) THEN
           PRINT *, "uh oh"
           EXIT
        END IF
     END IF
  END DO
END FUNCTION FOLLOW

SUBROUTINE PART1()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER s, nr, nc, sr, sc
  INTEGER dir(2)

  s = 0

  CALL READMAP(map, nr, nc, sr, sc)
  ! CALL PRINTMTX(map)
  dir = [1,0]
  s = FOLLOW(map, sr, sc, dir) / 2

  WRITE(6,*) "-----------------"
  WRITE(6,*) "Part 1", s
  WRITE(6,*) "-----------------"
END SUBROUTINE PART1

SUBROUTINE FOLLOW2(map, sr, sc, dir, omap)
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, omap
  CHARACTER tile, udlr
  INTEGER dir(2), loc(2), sr, sc, nc, ncc
  LOGICAL cw

  loc = [sr, sc]
  nc = 0
  ncc = 0
  DO
     IF (ALL(dir.EQ.[1,0])) THEN
        udlr = 'D'
     ELSE IF (ALL(dir.EQ.[-1,0])) THEN
        udlr = 'U'
     ELSE IF (ALL(dir.EQ.[0,1])) THEN
        udlr = 'R'
     ELSE IF (ALL(dir.EQ.[0,-1])) THEN
        ! lefT because L is taken
        udlr = 'T'
     ELSE
        PRINT *, "oh no"
        EXIT
     END IF

     loc = loc + dir
     tile = map(loc(1), loc(2))
     IF (tile.EQ.'S') EXIT

     IF (ANY(tile.EQ.turns)) THEN
        cw = DOT_PRODUCT(dir, PACK(brain(:,FINDLOC(turns,tile)),.TRUE.)).EQ.1
        ! dont worry about it
        dir = (DOT_PRODUCT(dir, PACK(brain(:,FINDLOC(turns,tile)),.TRUE.)) - &
               DOT_PRODUCT(dir, PACK(bbrain(:,FINDLOC(turns,tile)),.TRUE.))) * &
               MATMUL(rot,dir)
        IF (ALL(dir.EQ.0)) THEN
           PRINT *, "uh oh"
           EXIT
        END IF

        ! think quadrant labels
        IF (cw) THEN
           nc = nc + 1
           IF (udlr.EQ.'U') THEN
              udlr='1'
           ELSE IF (udlr.EQ.'D') THEN
              udlr='3'
           ELSE IF (udlr.EQ.'T') THEN
              udlr='2'
           ELSE IF (udlr.EQ.'R') THEN
              udlr='4'
           END IF
        ELSE
           ncc = ncc + 1
           IF (udlr.EQ.'U') THEN
              udlr='2'
           ELSE IF (udlr.EQ.'D') THEN
              udlr='4'
           ELSE IF (udlr.EQ.'T') THEN
              udlr='3'
           ELSE IF (udlr.EQ.'R') THEN
              udlr='1'
           END IF
        END IF
     END IF

     omap(loc(1), loc(2)) = udlr
  END DO
  PRINT *, "Clockwise: ", nc, "Counterclockwise: ", ncc
END SUBROUTINE FOLLOW2

CHARACTER FUNCTION FDIR(los, ll)
  CHARACTER, DIMENSION(ll) :: los
  INTEGER ll, i

  DO i=1,ll
     IF (ANY(los(i).EQ.dirs)) THEN
        fdir = los(i)
        EXIT
     END IF
  END DO
END FUNCTION FDIR

LOGICAL FUNCTION ISSUR(map, nr, nc, xr, xc)
  CHARACTER c
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map
  INTEGER xr, xc, nr, nc

  issur = .FALSE.

  ! assuming we are going clockwise
  ! looking down
  c = FDIR(map(xr:nr,xc), nr-xr+1)
  IF (ANY(c.EQ.['T','2','3'])) THEN
     CONTINUE
  ELSE
     RETURN
  END IF
  ! looking up
  c = FDIR(map(xr:1:-1,xc), xr)
  IF (ANY(c.EQ.['R','1','4'])) THEN
     CONTINUE
  ELSE
     RETURN
  END IF
  ! looking left
  c = FDIR(map(xr,xc:1:-1), xc)
  IF (ANY(c.EQ.['U','1','2'])) THEN
     CONTINUE
  ELSE
     RETURN
  END IF
  ! looking right
  c = FDIR(map(xr,xc:nc), nc-xc+1)
  IF (ANY(c.EQ.['D','3','4'])) THEN
     CONTINUE
  ELSE
     RETURN
  END IF

  ! I see a clockwise curve going around me, so I'm inside
  issur = .TRUE.
END FUNCTION ISSUR

SUBROUTINE PART2()
  IMPLICIT NONE
  CHARACTER, DIMENSION(:,:), ALLOCATABLE :: map, omap
  INTEGER s, nr, nc, sr, sc, i, j
  INTEGER dir(2)

  s = 0

  CALL READMAP(map, nr, nc, sr, sc)
  ALLOCATE(omap(nr,nc))
  omap = map
  ! Choose direction such that you make more clockwise turns then
  ! counterclockwise, which means we are making a clockwise loop
  dir = [1,0]
  ! dir = [0,1]
  CALL FOLLOW2(map, sr, sc, dir, omap)

  DO i=1,nr
     DO j=1,nc
        IF (ISSUR(omap,nr,nc,i,j)) s = s + 1
     END DO
  END DO

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
