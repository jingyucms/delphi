CDECK  ID>, BANKS.
 
CDECK  ID>, MAMCDE.
 
CDECK  ID>, REMCDE.
*     
CDECK  ID>, PSMAIN.
      PROGRAM PSMAIN
************************************************************************
*                                                                      *
*     Name           :  PSMAIN                                         *
*     Called by      :  Main                                           *
*     Date of coding :  Jan 18, 1992                                   *
*     Last update    :  Nov 25, 1993                                   *
*     Task           :  Main routine                                   *
*                                                                      *
************************************************************************
*
*--   PHDST package initialization
*
      CALL PHDST (' ',   0, IFLAG)
*
      END
*
CDECK  ID>, USER00.
      SUBROUTINE USER00
************************************************************************
*                                                                      *
*     Name           :  USER00                                         *
*     Called by      :  PHINIT                                         *
*     Date of coding :  Oct 23, 1993                                   *
*     Last update    :  Mar 07, 1995                                   *
*     Task           :  User initialization                            *
*                                                                      *
************************************************************************
      IMPLICIT NONE
*
*-----------------------------------------------------------------*
*       I/O LUNs:                                                 *
*          LUNDST -- input channel                                *
*          LUNSTR -- output channel                               *
*          LUNLOG -- LOG-information                              *
*          LUNTTY -- TTY-information                              *
*          LUNHST -- user's channel to write histograms etc       *
*          LUNHBO -- user's channel to write histograms etc       *
*          LUNPDL -- channel to read PDL-file                     *
*          LUNDSC -- channel to read Description File             *
*          LUNPTR -- the "file pointer" returned by               *
*                    the C library routine "fopen" ( for UNIX )   *
*          LUNFAT -- LUN for the FATMEN RZ file                   *
*          LUNFAN -- LUN for the FATMEN nicknames file            *
*          LUNZIP -- LUN for "memory" medium for data compression *
*-----------------------------------------------------------------*
      INTEGER        LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR, LUNFAT, LUNFAN, LUNZIP
      COMMON /PHLUN/ LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR(0:9), LUNFAT, LUNFAN, LUNZIP
*
*     (from ZEBRA)
      INTEGER         INFLUN, INFSTA, INFOFZ
      COMMON /FZSTAT/ INFLUN, INFSTA, INFOFZ(40)
*
*
*--------------------------------------*
*       ZEBRA links, storage etc.      *
*--------------------------------------*
      INTEGER  LTOP,LINKS(10),LUX(12)
      EQUIVALENCE    ( LTEMP(1) , LUX(1) )
      INTEGER  MAXPIL,MXPBUF,NPILTS,IPILTS,NBPLTS
      INTEGER  NBMIDS
*
*- Dimension of UX store + working space
      INTEGER     NSIZEQ           , NSIZWS
      PARAMETER ( NSIZEQ = 1400000 , NSIZWS = 350000 )
      INTEGER        IQUEST
      COMMON /QUEST/ IQUEST(100)
*
      INTEGER     NLNKMX
      PARAMETER ( NLNKMX = 30 )
      REAL     Q(NSIZEQ)
      INTEGER IQ(NSIZEQ) , LQ(NSIZEQ)
      REAL     UXFENC
      INTEGER  LUXSTR , LUXREF
      EQUIVALENCE    ( LQ(1) , LUXSTR(1) ) , ( Q(1) , IQ(1) , LQ(9) )
      COMMON /UXCOM/ UXFENC(10) , LUXSTR(NLNKMX) , LUXREF(NLNKMX)
*
*
      INTEGER        LTOP1, LTOP2, LTOP3, LTOP4, LTOP5
      INTEGER        LTOP6, LTOP7, LTOP8, LTOP9
      EQUIVALENCE    ( LQ(1), LTOP1, LTOP, LUXSTR )
      EQUIVALENCE    ( LQ(2), LTOP2 )
      EQUIVALENCE    ( LQ(3), LTOP3 )
      EQUIVALENCE    ( LQ(4), LTOP4 )
      EQUIVALENCE    ( LQ(5), LTOP5 )
      EQUIVALENCE    ( LQ(6), LTOP6 )
      EQUIVALENCE    ( LQ(7), LTOP7 )
      EQUIVALENCE    ( LQ(8), LTOP8 )
      EQUIVALENCE    ( LQ(9), LTOP9 )
      EQUIVALENCE    (LUXREF(2),LINKS)
*
*     IXSTOR -- main ZEBRA store
*     IXDIV  -- ZEBRA division for data
*     IXRDIV -- ZEBRA division for run information (SOR,COR etc)
*
      INTEGER        IXSTOR, IXDIV, IXRDIV
      COMMON /PHDIV/ IXSTOR, IXDIV, IXRDIV
*
*     MAXPIL -- maximum pilot length
*     MXPBUF -- maximum number of pilots buffered
*     NBMIDS -- LQ(.) is a link to temporary minidst structure
*     NPILOT -- current pilot length
*     IPILOT -- current pilot
*     NPILTS -- array of lengths of the pilots buffered
*     IPILTS -- pilots buffered
*     NBPLTS -- number of pilots buffered
*
      PARAMETER      ( MAXPIL = 1024, MXPBUF = 10, NBMIDS = 9 )
      COMMON /PHPIL/ NPILTS(MXPBUF),IPILTS(MAXPIL,MXPBUF), NBPLTS
*
      INTEGER         LTEMP  ,LRTOP  ,LSTOP  ,LTTOP  ,LITOP  ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
      COMMON /UXLINK/ LTEMP(2) ,LRTOP ,LSTOP ,LTTOP ,LITOP ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
* 
      INTEGER    NCPTMX
      PARAMETER (NCPTMX=100)
      INTEGER          LRUNLK  ,LSTRUN,LEORUN,LCORUN,LCPT,
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP,
     +                          LORSTR,LOREOR,LORCOR,LORCPT
      COMMON /UXLRUN/ LRUNLK(2),LSTRUN,LEORUN,LCORUN,LCPT(NCPTMX),
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP(NCPTMX),
     +                          LORSTR,LOREOR,LORCOR,LORCPT(NCPTMX)
*
      INTEGER  NWPXMA,NWPILT,IUPILT 
      PARAMETER (NWPXMA=1024)
      COMMON /PXCHDR/NWPILT,IUPILT(NWPXMA)
*
*
*--   Make equivalence between PHDST and PXDST pilots
      INTEGER  NPILOT, IPILOT(MAXPIL)
      EQUIVALENCE ( NPILOT, NWPILT )
      EQUIVALENCE ( IPILOT(1), IUPILT(1) )
*
*
*---------------------------------------------------------------------*
*       General information about event processing:
*       INTRCT = .TRUE. if job is INTeRaCTive
*       NFZFIL = Number of files(mediums) processed including
*                current
*       NFZPIL = Number of pilots read for all files
*       NFZPIX = Number of pilots read for current file
*       NFZGET = Number of data's read for all files
*       NFZGEX = Number of data's read for current file
*       NEvent = Number of investigated events for all files
*       NEvenX = Number of investigated events for current file
*       NGOODS = Number of events selected for all files
*       NGOODX = Number of events selected for current file
*       NFILOU = Number of files written
*       NDSSAV(1..9) -- Number of d/s saved on each stream
*       NDSSAV(10) == NEVOUT   -- Total number of d/s saved
*       TIMTOT = Approximate job time limit ( sec )
*       TIMEND = Time needed for termination of the job ( sec )
*       MAXPST = Maximum number of prestage command
*       LIMPSF = Limit on number of prestaged files in one prestage command
*       LAPSTG = Lapse in sec between PRESTAGE and STAGE commands
*       PACKED = .TRUE. if MiniDST used
*       IVPACK = Version of PHMINI used
*       FILIMT = Size limit of an output file (in mega-words)
*       STGPUTW = .TRUE. -- stageput will be executed with wait option
*       SGNEXIT != 0 -- OS signal came asking to terminate the job
*--------------------------------------------------------------------*
      INTEGER        PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              NFILOU,NEVOUT,NDSSAV
     +,              MAXPST,LIMPSF,LAPSTG,IVPACK,SGNEXIT,PHGLST
      LOGICAL        INTRCT,PACKED,FILACC, STGPUTW
      REAL           TIMTOT,TIMEND, FILIMT
      COMMON/PHGEN/  PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              INTRCT, TIMTOT,TIMEND, MAXPST, LIMPSF, LAPSTG
     +,              NFILOU, NDSSAV(10)
     +,              PACKED, IVPACK, FILIMT, FILACC, STGPUTW, SGNEXIT
     +,              PHGLST
      EQUIVALENCE ( NDSSAV(10), NEVOUT )
*
*
*---------------------------------------------------------------------*
*       General information about event from pilot
*                 DAS
*      IIIEXP --  5.Experiment number
*      IIIRUN --  6.Run number
*      IIFILE --  7.File sequence number
*      IIIEVT --  9.Event number
*      IIIDAT -- 10.Event date ('yymmdd')
*      IIITIM -- 11.Event time ('hhmmss')
*                 LEP
*      IIFILL --  6.Fill number
*---------------------------------------------------------------------*
      INTEGER         IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
      COMMON /PHCIII/ IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
*
*
*     FOR EVENT LIST
*
*     EVLPHV -- PHDST version used to write the Event List
*     NRUNEL -- run   number as it is read from Event List
*     NEVTEL -- event number as it is read from Event List
*     NUW    -- number of user words for the current event
*     IUWORD -- array containing the user words
*
      INTEGER     NUWMAX
      PARAMETER ( NUWMAX = 100 )
      INTEGER         EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD
      COMMON /PHEVLC/ EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD(NUWMAX)

*
*     FOR RANDOM ACCESS TO DISK FZ-FILES
*
*     IRABSZ -- sizes of banks for random access
*     IRABDL -- quantum to increment the size of bank for random access
*     NMASKS -- number of masks for current criterion
*     MASKEV -- masks of criterion
*     JDSA1  -- number of the physical record in which
*               the current input d/s starts
*     JDSA2  -- off-set within this record
*     IRSTOR -- store index
*     IRDIV1 -- division index
*     LDIRAT -- links to the DaT banks
*     IQRAN  -- store for the DaT banks
*     NRANUS -- numbers of words of the DaT banks used
*     0:9    -- mediums
*
      INTEGER     ISZRAN         , NMSKMX
      PARAMETER ( ISZRAN = 250000, NMSKMX = 50 )
      INTEGER         IRABSZ, IRABDL, NMASKS, MASKEV, IRSTOR, IRDIV1
      INTEGER         NRANUS, FENRAN, LQRAN, JDSA1, JDSA2
      COMMON /PHRNDM/ IRABSZ(9), IRABDL
     +              , NMASKS, MASKEV(4,NMSKMX)
     +              , JDSA1, JDSA2, IRSTOR, IRDIV1
     +              , NRANUS(0:9), FENRAN(10), LQRAN(ISZRAN)
      INTEGER LDIRAT(0:9), IQRAN(ISZRAN)
      EQUIVALENCE ( LDIRAT(0), LQRAN(1) ), ( IQRAN(1), LQRAN(9) )

CC 27-MAR-1996
CC+KEEP, PHYEAR.
CC*
CC*     IYEAR -- year of DELANA processing
CC*
CC      INTEGER        IYEAR, NMULAY
CC      COMMON /PHYEAR/IYEAR, NMULAY
CC+SEQ, PHYEAR.
 
*
*     Flags to fill the COMMONs: 0 - do NOT fill the commons
*                                1 - filling from  the tapes
*                                2 - filling trough packages
*
*      flag    fill the COMMON's with        values  default
*     ------   ----------------------        ------  -------
*     IFLTRA - Track      Information        0 1        1
*              0 - COMMOM not filled
*              1 - COMMON filled
*
*     IFLODR - Re-order tracks               0 1        1
*              0 - order of DST PAs (DO NOT USE WITH OLD CODES!)
*              1 - chged then neutral
*     
*     IFLVEC - VECP vector filling           0 -22      22
*              0 - no VECP vector filling
*              1 - fill all tracks except the "new incoming" ones 
*                    and PAs in REMCLU clusters
*             11 - fill all tracks,  lock the "new incoming" ones 
*              2 - fill all tracks except the "charged outgoing" ones 
*                    and PAs in REMCLU clusters
*             22 - fill all tracks,  lock the "charged outgoing" ones
*              3 - fill all tracks except the "charged outgoing" ones
*                    and ignore the REMCLU PAs, using the old original ones
*                    if ISVER.LT.108 is set equivalent to IFLVEC = 2
*
*     IFLSTR - Track selection :             0 -11      11
*              0 - no selection applied
*              1 - track selection applied , rejected tracks removed
*             11 - track selection applied , rejected tracks flagged
*                    in LVLOCK,LVSELE
*
*     IFLCUT - Track selection tuning        0 - 3      3      
*              1 - Old SKELANA selection
*              2 - May 98 tuning for 97 data
*              3 - April 99 tuning for 98 data (SKELANA/XSDST 1.07)
*
*     IFLRVR - Recovery routine              0-111        111
*              0 - recovery routine not applied
*             >0 - routine applied, overwrites VECP, TRAC bank  
*            ..1 - high momentum track refit with PV constraint
*            .1. - mammoth recovery
*            1.. - recover charged tracks as neutrals
* (e.g. 111- run all three, 011 - don't use netral recovery)
*              
*     IFLSIM - Simulation Information        0 1        1
*              0 - COMMOM not filled
*              1 - COMMON filled
*
*     IFLBSP - Beam Spot  Information        0 1 2      2
*              0 - COMMON not filled
*              1 - Filled from DST bank
*              2 - Read from Beamspot file
*
*     IFLBTG - B tagging  Information        0 1 2      2
*              0 - COMMON not filled
*              1 - Filled from XDST bank, or recalculate for FullDST
*              2 - recalculate with AABTAG
*
*     IFLPVT - Primary vertex treatment      0 - 1      1
*              0 - DELANA primary vertex
*              1 - Btagging primary vertex (if b-tagging used)
*    
*     IFLVDR - refit VD with Z tracks with PV constraint 0-1 1
*              0 - Inactive
*              1 - Active
*
*     IFLFCT - refit FCA/FCB (RIF)  with PV constraint 0-1 1
*              0 - Inactive
*              1 - Active
*
*     IFLRNQ - Run quality selection         0 1        0 
*              0 - Inactive
*              1 - Read Runquality file and apply selection
*
*     IFLBHP - Skip the bad 1997 HPC events  0 1        1
*              0 - Yes
*              1 - No
*
*     IFLUTE - Unassociated TE banks           0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLVDH - Vertex     Detector hits      0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLMUO - Muon       Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLECL - Electromagnetic cluster Information      0 -22      2
*              0 - no REMCLU
*              1 - from the DST, fill VECP with clusters
*              2 - rerun, fill VECP with clusters
*             11 - from the DST, fill PSCECL COMMON only
*             22 - rerun, fill PSCECL COMMON only
*
*     IFLELE - ELEPHANT Electron Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLEMC - Electromagnetic Calorimetry        0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLPHO - Photon     Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLPHC - Photon     Conversion         0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLSTC - STIC information              0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLHAC - Hadron     Calorimetry        0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLHAD - Hadron     Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLRV0 - V0         Reconstruction     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XShortDST
*
*     IFLJET - Jet reconstruction algorithm  0 - 3      0
*              0 - no reconstruction
*              1 - LUCLUS (standard)
*              2 - JADE  scaled inv. mass
*              3 - JADE  fixed  inv. mass
*
*     IFLFIX - dummy (unused)                0          0

      INTEGER IFLTRA,IFLFIX,IFLRNQ,IFLSTR,IFLJET
      INTEGER IFLSIM,IFLBSP,IFLBTG,IFLEMC,IFLHAC
      INTEGER IFLSTC,IFLELE,IFLPHO,IFLMUO,IFLHAD
      INTEGER IFLVDH,IFLRV0,IFLUTE,IFLPHC,IFLVEC
      INTEGER IFLBHP,IFLECL,IFLRVR,IFLODR,IFLPVT
      INTEGER IFLCUT,IFLVDR,IFLFCT,IFLENR
*
      COMMON /PSCFLG/ IFLTRA,IFLFIX,IFLRNQ,IFLSTR,IFLJET
     +,               IFLSIM,IFLBSP,IFLBTG,IFLEMC,IFLHAC
     +,               IFLSTC,IFLELE,IFLPHO,IFLMUO,IFLHAD
     +,               IFLVDH,IFLRV0,IFLUTE,IFLPHC,IFLVEC
     +,               IFLBHP,IFLECL,IFLRVR,IFLODR,IFLPVT
     +,               IFLCUT,IFLVDR,IFLFCT,IFLENR
*
*
*     Fill characteristics from the RQ files :
*
*     NRQFIL         - Total   number of fills
*     MRQFIL         - Maximum number of fills
*     IRQFIL(MRQFIL) - LEP number of the fills
*     ERQFIL(MRQFIL) - LEP energy of the fills
*
*     DELPHI detector/trigger status summary :
*
*     NRQDET         - Number of detectors in the RQ files
*     NRQTRG         - Number of triggers  in the RQ files
*     NRQT9X         - Number of triggers  after  1990
*     NRQT90         - Number of triggers  during 1990
*     IRQDMN(NRQDET) - Minimum status level for the detectors (1-31)
*     IRQDMX(NRQDET) - Maximum status level for the detectors (1-31)
*     IRQTMN(NRQTRG) - Minimum status level for the triggers  (1-23)
*     IRQTMX(NRQTRG) - Maximum status level for the triggers  (1-23)
*
*     IRQDET( 1)     - MVX_A   detector run quality
*     IRQDET( 2)     - MVX_C   detector run quality
*     IRQDET( 3)     - ID_JET  detector run quality
*     IRQDET( 4)     - ID_TRIG detector run quality
*     IRQDET( 5)     - TPC_0   detector run quality
*     IRQDET( 6)     - TPC_1   detector run quality
*     IRQDET( 7)     - BRICH_L detector run quality
*     IRQDET( 8)     - BRICH_G detector run quality
*     IRQDET( 9)     - OD_B    detector run quality
*     IRQDET(10)     - OD_D    detector run quality
*     IRQDET(11)     - HPC_0   detector run quality
*     IRQDET(12)     - HPC_1   detector run quality
*     IRQDET(13)     - HCAB_A  detector run quality
*     IRQDET(14)     - HCAB_C  detector run quality
*     IRQDET(15)     - MUB_B   detector run quality
*     IRQDET(16)     - MUB_B   detector run quality
*     IRQDET(17)     - FCA_A   detector run quality
*     IRQDET(18)     - FCA_C   detector run quality
*     IRQDET(19)     - RIF_A   detector run quality
*     IRQDET(20)     - RIF_C   detector run quality
*     IRQDET(21)     - FCB_A   detector run quality
*     IRQDET(22)     - FCB_C   detector run quality
*     IRQDET(23)     - EMF_A   detector run quality
*     IRQDET(24)     - EMF_C   detector run quality
*     IRQDET(25)     - HCAF_A  detector run quality
*     IRQDET(26)     - HCAF_C  detector run quality
*     IRQDET(27)     - MUF_A   detector run quality
*     IRQDET(28)     - MUF_C   detector run quality
*     IRQDET(29)     - SAT_CAL detector run quality
*     IRQDET(30)     - SAT_TRA detector run quality
*     IRQDET(31)     - VSAT    detector run quality
*
*                                      after 1990 :
*     IRQTRG( 1)     - TRIG_T1 trigger  run quality
*     IRQTRG( 2)     - TRIG_T2 trigger  run quality
*     IRQTRG( 3)     - B1_OR   trigger  run quality
*     IRQTRG( 4)     - B1_FW   trigger  run quality
*     IRQTRG( 5)     - PYT_SA  trigger  run quality
*     IRQTRG( 6)     - PYT_BM2 trigger  run quality
*     IRQTRG( 7)     - PYT_FM2 trigger  run quality
*     IRQTRG( 8)     - PYT_BMJ trigger  run quality
*     IRQTRG( 9)     - PYT_FMJ trigger  run quality
*     IRQTRG(10)     - SAT     trigger  run quality
*     IRQTRG(11)     - VSAT    trigger  run quality
*
*                                     during 1990 :
*     IRQTRG(12)     - ID      trigger  run quality
*     IRQTRG(13)     - OD      trigger  run quality
*     IRQTRG(14)     - HPC     trigger  run quality
*     IRQTRG(15)     - TOF     trigger  run quality
*     IRQTRG(16)     - FCA_A   trigger  run quality
*     IRQTRG(17)     - FCA_C   trigger  run quality
*     IRQTRG(18)     - FCB_A   trigger  run quality
*     IRQTRG(19)     - FCB_C   trigger  run quality
*     IRQTRG(20)     - EMF_A   trigger  run quality
*     IRQTRG(21)     - EMF_C   trigger  run quality
*     IRQTRG(22)     - HOF     trigger  run quality
*     IRQTRG(23)     - SAT     trigger  run quality
*
*     List of selected (rejected) run sets
*
*     NSRUNS         - Total   number of selected run sets
*     MSRUNS         - Maximum number of selected run sets
*     IFRUNS(MSRUNS) - First run of the selected  run sets
*     ILRUNS(MSRUNS) - Last  run of the selected  run sets
*     IFFILE(MSRUNS) - File number of the IFRUNS
*     ILFILE(MSRUNS) - File number of the ILRUNS
*
      INTEGER    MRQFIL
      PARAMETER (MRQFIL = 5000)
*
      INTEGER    NRQDET,NRQTRG
      PARAMETER (NRQDET = 38, NRQTRG = 23)
      INTEGER    NRQT90,NRQT9X
      PARAMETER (NRQT9X = 11, NRQT90 = 12)
*
      INTEGER    MSRUNS
      PARAMETER (MSRUNS = 3000)
*
      INTEGER NRQFIL,IRQFIL,IRQDET,IRQTRG
      INTEGER IRQDMN,IRQDMX,IRQTMN,IRQTMX
      INTEGER IFRUNS,ILRUNS,IFFILE,ILFILE
      INTEGER NSRUNS
      REAL    ERQFIL
      REAL    RQTMN(NRQTRG),RQTMX(NRQTRG)
*
      COMMON /PSCRNQ/ NRQFIL,IRQFIL(MRQFIL),ERQFIL(MRQFIL)
     +,                      IRQDMN(NRQDET),IRQDMX(NRQDET)
     +,                      IRQTMN(NRQTRG),IRQTMX(NRQTRG)
     +,                      IRQDET(NRQDET),IRQTRG(NRQTRG)
     +,               NSRUNS,IFRUNS(MSRUNS),ILRUNS(MSRUNS)
     +,                      IFFILE(MSRUNS),ILFILE(MSRUNS)
*
      EQUIVALENCE    (IRQTMN,RQTMN),(IRQTMX,RQTMX)
*
*
*
      INTEGER    NCUT
      PARAMETER (NCUT  =  3)
      INTEGER VDONLY,IDVDWZ
      INTEGER IHADRJ,ISTOEL
      REAL TRKMOM,TRKLEN,TRKRPH
      REAL TRKZET,TRCCOS,TRNCOS
      REAL TRKMAX,TRKERR,RECCAL
      REAL EHPC,EFEMC,EHAC,ESTIC 

      COMMON / PSCUTT / TRKMOM(NCUT),TRKLEN(NCUT),TRKRPH(NCUT),
     +                  TRKZET(NCUT),TRCCOS(NCUT),TRNCOS(NCUT),
     +                  TRKMAX(NCUT),TRKERR(NCUT),RECCAL(NCUT),
     +                    EHPC(NCUT), EFEMC(NCUT),  EHAC(NCUT),
     +                  ESTIC(NCUT), VDONLY(NCUT),IDVDWZ(NCUT),
     +                  IHADRJ(NCUT),ISTOEL(NCUT)
*
***********charged :
*     TRKMOM  -  min Track momentum in GeV
*     TRKMAX  -  max momentum in GeV
*     TRKERR  -  max dp/p 
*     TRKRPH  -  max Rphi impact parameter
*     TRKZET  -  max Z    impact parameter
*     TRKLEN  -  min Track length in cm.
*     TRCCOS  -  max COS (theta)  for charged tracks
*     VDONLY  -  reject VDONLY tracks
*             -  1- reject VD only without measued Z tracks
*             -  2- reject all VD only tracks
*
*     IDVDWZ  -  if >0, reject IDVD without Z tracks
*
*     IHADRJ - Hadron calorimeter noise rejection level 0-2
*                0 - Inactive
*                1 - reject most noisy clusters (tight tag)
*                2 - reject noisy clusters (loose or tight)
*
*     ISTOEL - Off momentum electrons rejection in STIC 0
*                0 - Inactive
*                1 - Active
*   
************recovery :
*                recover rejected charged tracks with
*     RECCAL  -  calorimeter deposits (GeV)
************neutral :
*                neutral cluster cuts :
*     EHPC    -  HPC   energy for neutral tracks  
*     EFEMC   -  FEMC  energy for neutral tracks
*     EHAC    -  EHAC  energy for neutral tracks
*     ESTIC   -  ESTIC energy for neutral tracks
*     TRNCOS  -  COS (theta)  for neutral tracks
*
*
*--   Skelana initialization
      CALL PSINI
*
*--   Change the default values
*--   of control flags and keys
      IFLBSP = 2
      IFLBTG = 2
*
*--   Put your initialization code here...
*
*--   Set-up the default values
*--   of control flags and keys
      IFLTRA = 1
      IFLODR = 1
      IFLVEC = 22
      IFLSTR = 11
      IFLCUT = 3
      IFLRVR = 111
      IFLSIM = 1
      IFLBSP = 2
      IFLBTG = 2
      IFLPVT = 1
      IFLVDR = 1
      IFLFCT = 1
      IFLRNQ = 0
      IFLBHP = 1
      IFLUTE = 1
      IFLVDH = 1
      IFLMUO = 1
      IFLECL = 22
      IFLELE = 1
      IFLEMC = 1
      IFLPHO = 1
      IFLPHC = 1
      IFLSTC = 1
      IFLHAC = 1
      IFLHAD = 1
      IFLRV0 = 1
      IFLJET = 0
      IFLENR = 0
*
*--   Set the default track selection cuts
*
*--   Old SKELANA selection:
*
      TRKMOM(1) = 0.1
      TRKLEN(1) = 30.
      TRKRPH(1) = 5.
      TRKZET(1) = 10.
      TRCCOS(1) = 0.94
      TRKERR(1) = 1.
      TRNCOS(1) = 0.98
      TRKMAX(1) = 1.E+10
      EHPC(1)   = 1.E-10
      EFEMC(1)  = 1.E-10
      EHAC(1)   = 1.E-10
      ESTIC(1)  = 1.E-10
      RECCAL(1) = 1.E+10
      VDONLY(1) = 0
      IDVDWZ(1) = 0
      IHADRJ(1) = 0
      ISTOEL(1) = 0
*
*--   May 98 tuning for 97 data
*
      TRKMOM(2) = 0.2
      TRKLEN(2) = 0.
      TRKRPH(2) = 4.
      TRKZET(2) = 4.
      TRCCOS(2) = 1.
      TRKMAX(2) = 1.5
      TRKERR(2) = 1.
      TRNCOS(2) = 1.
      EHPC(2)   = 0.5
      EFEMC(2)  = 0.4
      EHAC(2)   = 0.9
      ESTIC(2)  = 0.3
      RECCAL(2) = 5.
      VDONLY(2) = 2
      IDVDWZ(2) = 1
      IHADRJ(2) = 0
      ISTOEL(2) = 0
*
*--   April 99 tuning for 98 data
*
      TRKMOM(3) = 0.1
      TRKLEN(3) = 30.
      TRKRPH(3) = 4.
      TRKZET(3) = 4.
      TRCCOS(3) = 0.94
      TRKMAX(3) = 1.5
      TRKERR(3) = 1.
      TRNCOS(3) = 0.98
      EHPC(3)   = 0.3
      EFEMC(3)  = 0.4
      EHAC(3)   = 0.
      ESTIC(3)  = 0.3
      RECCAL(3) = 5.0
      VDONLY(3) = 1
      IDVDWZ(3) = 1
      IHADRJ(3) = 2
      ISTOEL(3) = 1
*
*     Print the chosen flag values and track selection cuts:
*
      WRITE (*,1000) IFLTRA,IFLODR,IFLVEC,IFLSTR,IFLCUT,
     +               IFLRVR,IFLSIM,IFLBSP,IFLBTG,IFLPVT,
     +               IFLVDR,IFLFCT,IFLRNQ,IFLBHP,IFLUTE,
     +               IFLVDH,IFLMUO,IFLECL,IFLELE,IFLEMC,
     +               IFLPHO,IFLPHC,IFLSTC,IFLHAC,IFLHAD,
     +               IFLRV0,IFLJET,IFLENR

*
      IF (IFLCUT.LT.1. .OR. IFLCUT.GT.NCUT) THEN
        IFLCUT = 3
        WRITE (*,1002) IFLCUT
      ENDIF
      IF (IFLSTR.GT.0) THEN
        WRITE (*,1001) TRKMOM(IFLCUT),TRKMAX(IFLCUT),TRKERR(IFLCUT),
     +                 TRKRPH(IFLCUT),TRKZET(IFLCUT),TRKLEN(IFLCUT),
     +                 TRCCOS(IFLCUT),VDONLY(IFLCUT),IDVDWZ(IFLCUT),
     +                 IHADRJ(IFLCUT),ISTOEL(IFLCUT),
     +                 EHPC(IFLCUT)  ,EFEMC(IFLCUT) ,EHAC(IFLCUT),
     +                 ESTIC(IFLCUT) ,TRNCOS(IFLCUT),
     +                 RECCAL(IFLCUT)

      ELSE
       WRITE(*,*) 'No Track Selection applied'
      ENDIF
*
*
*--   Read the energy correction  2000
      IF ( IFLENR .GT. 0 ) CALL PSENRG
*
*
*--   Read the bad 1997 HPC event list
      IF ( IFLBHP .GT. 0 ) CALL PSBHPC
*
*
*--   Change the default acceptance conditions for
*--   detectors/triggers of the "RUNQUALITY" files
*
      IF ( IFLRNQ .GT. 0 ) THEN
*
*--      MVX_A and MVX_C
         IRQDMN(1) = 1
         IRQDMN(2) = 1
*
*--      TPC_0 and TPC_1
         IRQDMN(5) = 7
         IRQDMN(6) = 7
*
*--      Read the runquality files
         CALL PSRUNQ(0)
      ENDIF
*
*-----------------------------------------------------------------------
 1000 FORMAT(/,1X,'%PSMAIN-I-USER00, ','SKELANA flags for this run:',/,
     +       /,1X,'IFLTRA = ',I3,
     +       /,1X,'IFLODR = ',I3,
     +       /,1X,'IFLVEC = ',I3,
     +       /,1X,'IFLSTR = ',I3,
     +       /,1X,'IFLCUT = ',I3,
     +       /,1X,'IFLRVR = ',I3,
     +       /,1X,'IFLSIM = ',I3,
     +       /,1X,'IFLBSP = ',I3,
     +       /,1X,'IFLBTG = ',I3,
     +       /,1X,'IFLPVT = ',I3,
     +       /,1X,'IFLVDR = ',I3,
     +       /,1X,'IFLFCT = ',I3,
     +       /,1X,'IFLRNQ = ',I3,
     +       /,1X,'IFLBHP = ',I3,
     +       /,1X,'IFLUTE = ',I3,
     +       /,1X,'IFLVDH = ',I3,
     +       /,1X,'IFLMUO = ',I3,
     +       /,1X,'IFLECL = ',I3,
     +       /,1X,'IFLELE = ',I3,
     +       /,1X,'IFLEMC = ',I3,
     +       /,1X,'IFLPHO = ',I3,
     +       /,1X,'IFLPHC = ',I3,
     +       /,1X,'IFLSTC = ',I3,
     +       /,1X,'IFLHAC = ',I3,
     +       /,1X,'IFLHAD = ',I3,
     +       /,1X,'IFLRV0 = ',I3,
     +       /,1X,'IFLJET = ',I3,
     +       /,1X,'IFLENR = ',I3,
     +       /)

 1001 FORMAT(/,1X,'%PSMAIN-I-USER00, '
     &  ,'track selection cuts for this run:',/,
     +       /,1X,'TRKMOM = ',E10.4,
     +       /,1X,'TRKMAX = ',E10.4,
     +       /,1X,'TRKERR = ',E10.4,
     +       /,1X,'TRKRPH = ',E10.4,
     +       /,1X,'TRKZET = ',E10.4,
     +       /,1X,'TRKLEN = ',E10.4,
     +       /,1X,'TRCCOS = ',E10.4,
     +       /,1X,'VDONLY = ',I3,
     +       /,1X,'IDVDWZ = ',I3,
     +       /,1X,'IHADRJ = ',I3,
     +       /,1X,'ISTOEL = ',I3,
     +       /,1X,'EHPC   = ',E10.4,
     +       /,1X,'EFEMC  = ',E10.4,
     +       /,1X,'EHAC   = ',E10.4,
     +       /,1X,'ESTIC  = ',E10.4,
     +       /,1X,'TRNCOS = ',E10.4,
     +       /,1X,'RECCAL = ',E10.4,
     +  /)
 1002 FORMAT(/,1X,'%PSMAIN-E-USER00, '
     &  ,'unknown track selection cuts,'
     &  ,' set ',I3,' as default',/)
      END
*
CDECK  ID>, USER01.
      SUBROUTINE USER01(NEED)
************************************************************************
*                                                                      *
*     Name           :  USER01                                         *
*     Called by      :  PHNEED                                         *
*     Date of coding :  Jan 18, 1992                                   *
*     Last update    :  Mar 15, 1994                                   *
*     Task           :  General event analysis                         *
*                                                                      *
*     Output         :  NEED = 1 the event is     needed               *
*                       NEED = 0 the event is NOT needed               *
*                                                                      *
************************************************************************
      IMPLICIT NONE
*
*-----------------------------------------------------------------*
*       I/O LUNs:                                                 *
*          LUNDST -- input channel                                *
*          LUNSTR -- output channel                               *
*          LUNLOG -- LOG-information                              *
*          LUNTTY -- TTY-information                              *
*          LUNHST -- user's channel to write histograms etc       *
*          LUNHBO -- user's channel to write histograms etc       *
*          LUNPDL -- channel to read PDL-file                     *
*          LUNDSC -- channel to read Description File             *
*          LUNPTR -- the "file pointer" returned by               *
*                    the C library routine "fopen" ( for UNIX )   *
*          LUNFAT -- LUN for the FATMEN RZ file                   *
*          LUNFAN -- LUN for the FATMEN nicknames file            *
*          LUNZIP -- LUN for "memory" medium for data compression *
*-----------------------------------------------------------------*
      INTEGER        LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR, LUNFAT, LUNFAN, LUNZIP
      COMMON /PHLUN/ LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR(0:9), LUNFAT, LUNFAN, LUNZIP
*
*     (from ZEBRA)
      INTEGER         INFLUN, INFSTA, INFOFZ
      COMMON /FZSTAT/ INFLUN, INFSTA, INFOFZ(40)
*
*
*--------------------------------------*
*       ZEBRA links, storage etc.      *
*--------------------------------------*
      INTEGER  LTOP,LINKS(10),LUX(12)
      EQUIVALENCE    ( LTEMP(1) , LUX(1) )
      INTEGER  MAXPIL,MXPBUF,NPILTS,IPILTS,NBPLTS
      INTEGER  NBMIDS
*
*- Dimension of UX store + working space
      INTEGER     NSIZEQ           , NSIZWS
      PARAMETER ( NSIZEQ = 1400000 , NSIZWS = 350000 )
      INTEGER        IQUEST
      COMMON /QUEST/ IQUEST(100)
*
      INTEGER     NLNKMX
      PARAMETER ( NLNKMX = 30 )
      REAL     Q(NSIZEQ)
      INTEGER IQ(NSIZEQ) , LQ(NSIZEQ)
      REAL     UXFENC
      INTEGER  LUXSTR , LUXREF
      EQUIVALENCE    ( LQ(1) , LUXSTR(1) ) , ( Q(1) , IQ(1) , LQ(9) )
      COMMON /UXCOM/ UXFENC(10) , LUXSTR(NLNKMX) , LUXREF(NLNKMX)
*
*
      INTEGER        LTOP1, LTOP2, LTOP3, LTOP4, LTOP5
      INTEGER        LTOP6, LTOP7, LTOP8, LTOP9
      EQUIVALENCE    ( LQ(1), LTOP1, LTOP, LUXSTR )
      EQUIVALENCE    ( LQ(2), LTOP2 )
      EQUIVALENCE    ( LQ(3), LTOP3 )
      EQUIVALENCE    ( LQ(4), LTOP4 )
      EQUIVALENCE    ( LQ(5), LTOP5 )
      EQUIVALENCE    ( LQ(6), LTOP6 )
      EQUIVALENCE    ( LQ(7), LTOP7 )
      EQUIVALENCE    ( LQ(8), LTOP8 )
      EQUIVALENCE    ( LQ(9), LTOP9 )
      EQUIVALENCE    (LUXREF(2),LINKS)
*
*     IXSTOR -- main ZEBRA store
*     IXDIV  -- ZEBRA division for data
*     IXRDIV -- ZEBRA division for run information (SOR,COR etc)
*
      INTEGER        IXSTOR, IXDIV, IXRDIV
      COMMON /PHDIV/ IXSTOR, IXDIV, IXRDIV
*
*     MAXPIL -- maximum pilot length
*     MXPBUF -- maximum number of pilots buffered
*     NBMIDS -- LQ(.) is a link to temporary minidst structure
*     NPILOT -- current pilot length
*     IPILOT -- current pilot
*     NPILTS -- array of lengths of the pilots buffered
*     IPILTS -- pilots buffered
*     NBPLTS -- number of pilots buffered
*
      PARAMETER      ( MAXPIL = 1024, MXPBUF = 10, NBMIDS = 9 )
      COMMON /PHPIL/ NPILTS(MXPBUF),IPILTS(MAXPIL,MXPBUF), NBPLTS
*
      INTEGER         LTEMP  ,LRTOP  ,LSTOP  ,LTTOP  ,LITOP  ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
      COMMON /UXLINK/ LTEMP(2) ,LRTOP ,LSTOP ,LTTOP ,LITOP ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
* 
      INTEGER    NCPTMX
      PARAMETER (NCPTMX=100)
      INTEGER          LRUNLK  ,LSTRUN,LEORUN,LCORUN,LCPT,
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP,
     +                          LORSTR,LOREOR,LORCOR,LORCPT
      COMMON /UXLRUN/ LRUNLK(2),LSTRUN,LEORUN,LCORUN,LCPT(NCPTMX),
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP(NCPTMX),
     +                          LORSTR,LOREOR,LORCOR,LORCPT(NCPTMX)
*
      INTEGER  NWPXMA,NWPILT,IUPILT 
      PARAMETER (NWPXMA=1024)
      COMMON /PXCHDR/NWPILT,IUPILT(NWPXMA)
*
*
*--   Make equivalence between PHDST and PXDST pilots
      INTEGER  NPILOT, IPILOT(MAXPIL)
      EQUIVALENCE ( NPILOT, NWPILT )
      EQUIVALENCE ( IPILOT(1), IUPILT(1) )
*
*
*---------------------------------------------------------------------*
*       General information about event processing:
*       INTRCT = .TRUE. if job is INTeRaCTive
*       NFZFIL = Number of files(mediums) processed including
*                current
*       NFZPIL = Number of pilots read for all files
*       NFZPIX = Number of pilots read for current file
*       NFZGET = Number of data's read for all files
*       NFZGEX = Number of data's read for current file
*       NEvent = Number of investigated events for all files
*       NEvenX = Number of investigated events for current file
*       NGOODS = Number of events selected for all files
*       NGOODX = Number of events selected for current file
*       NFILOU = Number of files written
*       NDSSAV(1..9) -- Number of d/s saved on each stream
*       NDSSAV(10) == NEVOUT   -- Total number of d/s saved
*       TIMTOT = Approximate job time limit ( sec )
*       TIMEND = Time needed for termination of the job ( sec )
*       MAXPST = Maximum number of prestage command
*       LIMPSF = Limit on number of prestaged files in one prestage command
*       LAPSTG = Lapse in sec between PRESTAGE and STAGE commands
*       PACKED = .TRUE. if MiniDST used
*       IVPACK = Version of PHMINI used
*       FILIMT = Size limit of an output file (in mega-words)
*       STGPUTW = .TRUE. -- stageput will be executed with wait option
*       SGNEXIT != 0 -- OS signal came asking to terminate the job
*--------------------------------------------------------------------*
      INTEGER        PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              NFILOU,NEVOUT,NDSSAV
     +,              MAXPST,LIMPSF,LAPSTG,IVPACK,SGNEXIT,PHGLST
      LOGICAL        INTRCT,PACKED,FILACC, STGPUTW
      REAL           TIMTOT,TIMEND, FILIMT
      COMMON/PHGEN/  PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              INTRCT, TIMTOT,TIMEND, MAXPST, LIMPSF, LAPSTG
     +,              NFILOU, NDSSAV(10)
     +,              PACKED, IVPACK, FILIMT, FILACC, STGPUTW, SGNEXIT
     +,              PHGLST
      EQUIVALENCE ( NDSSAV(10), NEVOUT )
*
*
*---------------------------------------------------------------------*
*       General information about event from pilot
*                 DAS
*      IIIEXP --  5.Experiment number
*      IIIRUN --  6.Run number
*      IIFILE --  7.File sequence number
*      IIIEVT --  9.Event number
*      IIIDAT -- 10.Event date ('yymmdd')
*      IIITIM -- 11.Event time ('hhmmss')
*                 LEP
*      IIFILL --  6.Fill number
*---------------------------------------------------------------------*
      INTEGER         IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
      COMMON /PHCIII/ IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
*
*
*     FOR EVENT LIST
*
*     EVLPHV -- PHDST version used to write the Event List
*     NRUNEL -- run   number as it is read from Event List
*     NEVTEL -- event number as it is read from Event List
*     NUW    -- number of user words for the current event
*     IUWORD -- array containing the user words
*
      INTEGER     NUWMAX
      PARAMETER ( NUWMAX = 100 )
      INTEGER         EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD
      COMMON /PHEVLC/ EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD(NUWMAX)

*
*     FOR RANDOM ACCESS TO DISK FZ-FILES
*
*     IRABSZ -- sizes of banks for random access
*     IRABDL -- quantum to increment the size of bank for random access
*     NMASKS -- number of masks for current criterion
*     MASKEV -- masks of criterion
*     JDSA1  -- number of the physical record in which
*               the current input d/s starts
*     JDSA2  -- off-set within this record
*     IRSTOR -- store index
*     IRDIV1 -- division index
*     LDIRAT -- links to the DaT banks
*     IQRAN  -- store for the DaT banks
*     NRANUS -- numbers of words of the DaT banks used
*     0:9    -- mediums
*
      INTEGER     ISZRAN         , NMSKMX
      PARAMETER ( ISZRAN = 250000, NMSKMX = 50 )
      INTEGER         IRABSZ, IRABDL, NMASKS, MASKEV, IRSTOR, IRDIV1
      INTEGER         NRANUS, FENRAN, LQRAN, JDSA1, JDSA2
      COMMON /PHRNDM/ IRABSZ(9), IRABDL
     +              , NMASKS, MASKEV(4,NMSKMX)
     +              , JDSA1, JDSA2, IRSTOR, IRDIV1
     +              , NRANUS(0:9), FENRAN(10), LQRAN(ISZRAN)
      INTEGER LDIRAT(0:9), IQRAN(ISZRAN)
      EQUIVALENCE ( LDIRAT(0), LQRAN(1) ), ( IQRAN(1), LQRAN(9) )

CC 27-MAR-1996
CC+KEEP, PHYEAR.
CC*
CC*     IYEAR -- year of DELANA processing
CC*
CC      INTEGER        IYEAR, NMULAY
CC      COMMON /PHYEAR/IYEAR, NMULAY
CC+SEQ, PHYEAR.
 
*
*     ISVER  -  Mini/short DST version number
*     IHAD4  -  Hadronic tag (team4 criteria)
*     NCTR4  -  Charged multiplicity (team 4)
*     NCTRK  -  Charged multiplicity (total)
*     NNTRK  -  Neutral multiplicity
*     ECMAS  -  Center of mass energy
*     ECHAR  -  Total charged energy
*     EMNEU  -  Total electromagnetic neutral energy
*     EHNEU  -  Total hadronic neutral energy
*     CDTYPE -  FullDST/shortDST type (from DSTQID)
*     LABO   -  Laboratory identifier (see TANAGRA manual)
*
*
      INTEGER ISVER,IHAD4
      INTEGER NCTR4,NCTRK,NNTRK
      REAL    ECMAS,ECHAR,EMNEU,EHNEU
      CHARACTER*4 CDTYPE,LABO
*
      COMMON /PSCEVT/ ISVER,IHAD4,NCTR4,NCTRK,NNTRK
     +,               ECMAS,ECHAR,EMNEU,EHNEU
      COMMON /PSCEVC/ CDTYPE,LABO
*
*
*     Flags to fill the COMMONs: 0 - do NOT fill the commons
*                                1 - filling from  the tapes
*                                2 - filling trough packages
*
*      flag    fill the COMMON's with        values  default
*     ------   ----------------------        ------  -------
*     IFLTRA - Track      Information        0 1        1
*              0 - COMMOM not filled
*              1 - COMMON filled
*
*     IFLODR - Re-order tracks               0 1        1
*              0 - order of DST PAs (DO NOT USE WITH OLD CODES!)
*              1 - chged then neutral
*     
*     IFLVEC - VECP vector filling           0 -22      22
*              0 - no VECP vector filling
*              1 - fill all tracks except the "new incoming" ones 
*                    and PAs in REMCLU clusters
*             11 - fill all tracks,  lock the "new incoming" ones 
*              2 - fill all tracks except the "charged outgoing" ones 
*                    and PAs in REMCLU clusters
*             22 - fill all tracks,  lock the "charged outgoing" ones
*              3 - fill all tracks except the "charged outgoing" ones
*                    and ignore the REMCLU PAs, using the old original ones
*                    if ISVER.LT.108 is set equivalent to IFLVEC = 2
*
*     IFLSTR - Track selection :             0 -11      11
*              0 - no selection applied
*              1 - track selection applied , rejected tracks removed
*             11 - track selection applied , rejected tracks flagged
*                    in LVLOCK,LVSELE
*
*     IFLCUT - Track selection tuning        0 - 3      3      
*              1 - Old SKELANA selection
*              2 - May 98 tuning for 97 data
*              3 - April 99 tuning for 98 data (SKELANA/XSDST 1.07)
*
*     IFLRVR - Recovery routine              0-111        111
*              0 - recovery routine not applied
*             >0 - routine applied, overwrites VECP, TRAC bank  
*            ..1 - high momentum track refit with PV constraint
*            .1. - mammoth recovery
*            1.. - recover charged tracks as neutrals
* (e.g. 111- run all three, 011 - don't use netral recovery)
*              
*     IFLSIM - Simulation Information        0 1        1
*              0 - COMMOM not filled
*              1 - COMMON filled
*
*     IFLBSP - Beam Spot  Information        0 1 2      2
*              0 - COMMON not filled
*              1 - Filled from DST bank
*              2 - Read from Beamspot file
*
*     IFLBTG - B tagging  Information        0 1 2      2
*              0 - COMMON not filled
*              1 - Filled from XDST bank, or recalculate for FullDST
*              2 - recalculate with AABTAG
*
*     IFLPVT - Primary vertex treatment      0 - 1      1
*              0 - DELANA primary vertex
*              1 - Btagging primary vertex (if b-tagging used)
*    
*     IFLVDR - refit VD with Z tracks with PV constraint 0-1 1
*              0 - Inactive
*              1 - Active
*
*     IFLFCT - refit FCA/FCB (RIF)  with PV constraint 0-1 1
*              0 - Inactive
*              1 - Active
*
*     IFLRNQ - Run quality selection         0 1        0 
*              0 - Inactive
*              1 - Read Runquality file and apply selection
*
*     IFLBHP - Skip the bad 1997 HPC events  0 1        1
*              0 - Yes
*              1 - No
*
*     IFLUTE - Unassociated TE banks           0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLVDH - Vertex     Detector hits      0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLMUO - Muon       Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLECL - Electromagnetic cluster Information      0 -22      2
*              0 - no REMCLU
*              1 - from the DST, fill VECP with clusters
*              2 - rerun, fill VECP with clusters
*             11 - from the DST, fill PSCECL COMMON only
*             22 - rerun, fill PSCECL COMMON only
*
*     IFLELE - ELEPHANT Electron Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLEMC - Electromagnetic Calorimetry        0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLPHO - Photon     Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLPHC - Photon     Conversion         0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLSTC - STIC information              0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XSDST
*
*     IFLHAC - Hadron     Calorimetry        0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLHAD - Hadron     Identification     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled
*
*     IFLRV0 - V0         Reconstruction     0 1        1
*              0 - COMMON not filled
*              1 - COMMON filled for XShortDST
*
*     IFLJET - Jet reconstruction algorithm  0 - 3      0
*              0 - no reconstruction
*              1 - LUCLUS (standard)
*              2 - JADE  scaled inv. mass
*              3 - JADE  fixed  inv. mass
*
*     IFLFIX - dummy (unused)                0          0

      INTEGER IFLTRA,IFLFIX,IFLRNQ,IFLSTR,IFLJET
      INTEGER IFLSIM,IFLBSP,IFLBTG,IFLEMC,IFLHAC
      INTEGER IFLSTC,IFLELE,IFLPHO,IFLMUO,IFLHAD
      INTEGER IFLVDH,IFLRV0,IFLUTE,IFLPHC,IFLVEC
      INTEGER IFLBHP,IFLECL,IFLRVR,IFLODR,IFLPVT
      INTEGER IFLCUT,IFLVDR,IFLFCT,IFLENR
*
      COMMON /PSCFLG/ IFLTRA,IFLFIX,IFLRNQ,IFLSTR,IFLJET
     +,               IFLSIM,IFLBSP,IFLBTG,IFLEMC,IFLHAC
     +,               IFLSTC,IFLELE,IFLPHO,IFLMUO,IFLHAD
     +,               IFLVDH,IFLRV0,IFLUTE,IFLPHC,IFLVEC
     +,               IFLBHP,IFLECL,IFLRVR,IFLODR,IFLPVT
     +,               IFLCUT,IFLVDR,IFLFCT,IFLENR
*
*
*     NEVBAD         - Total   number  of bad HPC events
*     MEVBAD         - Maximum number  of bad HPC events
*     IRNBAD(MEVBAD) - Run numbers of the bad HPC events
*     IEVBAD(MEVBAD) - Event  "    of the bad HPC events

      INTEGER    MEVBAD
      PARAMETER (MEVBAD = 50000)
*
      INTEGER NEVBAD,IRNBAD,IEVBAD    
*
      COMMON /PSCBHP/ NEVBAD,IRNBAD(MEVBAD),IEVBAD(MEVBAD)
*
*
*--   arguments
      INTEGER NEED
*
*--   functions
      INTEGER IPHPIC,JBIT
      CHARACTER*4 PHRTY
*
*--   local variables
      INTEGER     IDENT
      CHARACTER*4 RECTYP
      INTEGER IRSEL
      INTEGER I
*
      NEED   = 0
*
*--   Check the record type
      RECTYP = PHRTY()
*
*--   Skip the unwanted records
      IF     ( RECTYP .EQ. 'RAW ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'TAN ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'SOR ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'COR ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'EOR ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'SOS ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'EOS ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'BOF ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. 'CPT ' ) THEN
         GO TO 99
      ELSE IF( RECTYP .EQ. '0072' ) THEN
         GO TO 99
      ENDIF
*
      IF ( RECTYP .NE. 'DST ' ) THEN
         WRITE(*,1000) RECTYP,IIIRUN,IIIEVT,NEVENT
         GO TO 99
      ENDIF
*
*--   Maximum  number of events
*      IF ( NEVENT .GT. 10 ) THEN
*         NEED = -3
*         GO TO 99
*      ENDIF

*
      IF ( RECTYP .EQ. 'DST ' ) THEN
*
*--      Pilot record information
         IDENT = IPHPIC ('IDEN',1)
         IF ( IDENT .GE. 0 ) THEN
*--         Hadronic selection
            IHAD4 = 0
            IF(JBIT(IPILOT(IDENT+6),1) .EQ. 1  .AND.
     +         JBIT(IPILOT(IDENT+6),2) .EQ. 1) IHAD4 = 1
         ENDIF
*
*--      Run quality selection
         IF ( IFLRNQ .GT. 0  .AND.
     +        IIIRUN .GT. 0 ) THEN
            CALL PSRUNS(IRSEL)
         ENDIF
*
*--      Skip the bad 97 HPC events
         IF ( IFLBHP .GT. 0 ) THEN
            IF ( IIIRUN .GE. IRNBAD(     1)  .AND.
     +           IIIRUN .LE. IRNBAD(NEVBAD) ) THEN
               DO I = 1, NEVBAD
                  IF ( IIIRUN .EQ. IRNBAD(I)  .AND.
     +                 IIIEVT .EQ. IEVBAD(I) ) THEN
                     NEED = 0
                     GO TO 99
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
*--      Event selection (an example)
CCC      IF ( IRSEL .EQ. 1  .AND.
CCC  +        IHAD4 .NE. 0 ) NEED = 1
*
      NEED = 1
*
      ENDIF
*
   99 CONTINUE
*-----------------------------------------------------------------------
 1000 FORMAT(/,1X,'%PSMAIN-E-USER01, ','Event not in any DST format'
     +      ,/,1X,'                  ','Record type',A4
     +      ,/,1X,'                  ','Run',I7,', Event',I6,'/',I6,/)
      END
*
CDECK  ID>, USER02.
      SUBROUTINE USER02
************************************************************************
*                                                                      *
*     Name           :  USER02                                         *
*     Called by      :  PHPROC                                         *
*     Date of coding :  Nov 18, 1993                                   *
*     Last update    :  Mar 07, 1995                                   *
*     Task           :  To access the event for analysis               *
*                                                                      *
************************************************************************
      IMPLICIT NONE
*
*-----------------------------------------------------------------*
*       I/O LUNs:                                                 *
*          LUNDST -- input channel                                *
*          LUNSTR -- output channel                               *
*          LUNLOG -- LOG-information                              *
*          LUNTTY -- TTY-information                              *
*          LUNHST -- user's channel to write histograms etc       *
*          LUNHBO -- user's channel to write histograms etc       *
*          LUNPDL -- channel to read PDL-file                     *
*          LUNDSC -- channel to read Description File             *
*          LUNPTR -- the "file pointer" returned by               *
*                    the C library routine "fopen" ( for UNIX )   *
*          LUNFAT -- LUN for the FATMEN RZ file                   *
*          LUNFAN -- LUN for the FATMEN nicknames file            *
*          LUNZIP -- LUN for "memory" medium for data compression *
*-----------------------------------------------------------------*
      INTEGER        LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR, LUNFAT, LUNFAN, LUNZIP
      COMMON /PHLUN/ LUNDST, LUNSTR, LUNLOG, LUNTTY
     +             , LUNHST, LUNHBO, LUNPDL, LUNDSC
     +             , LUNPTR(0:9), LUNFAT, LUNFAN, LUNZIP
*
*     (from ZEBRA)
      INTEGER         INFLUN, INFSTA, INFOFZ
      COMMON /FZSTAT/ INFLUN, INFSTA, INFOFZ(40)
*
*
*--------------------------------------*
*       ZEBRA links, storage etc.      *
*--------------------------------------*
      INTEGER  LTOP,LINKS(10),LUX(12)
      EQUIVALENCE    ( LTEMP(1) , LUX(1) )
      INTEGER  MAXPIL,MXPBUF,NPILTS,IPILTS,NBPLTS
      INTEGER  NBMIDS
*
*- Dimension of UX store + working space
      INTEGER     NSIZEQ           , NSIZWS
      PARAMETER ( NSIZEQ = 1400000 , NSIZWS = 350000 )
      INTEGER        IQUEST
      COMMON /QUEST/ IQUEST(100)
*
      INTEGER     NLNKMX
      PARAMETER ( NLNKMX = 30 )
      REAL     Q(NSIZEQ)
      INTEGER IQ(NSIZEQ) , LQ(NSIZEQ)
      REAL     UXFENC
      INTEGER  LUXSTR , LUXREF
      EQUIVALENCE    ( LQ(1) , LUXSTR(1) ) , ( Q(1) , IQ(1) , LQ(9) )
      COMMON /UXCOM/ UXFENC(10) , LUXSTR(NLNKMX) , LUXREF(NLNKMX)
*
*
      INTEGER        LTOP1, LTOP2, LTOP3, LTOP4, LTOP5
      INTEGER        LTOP6, LTOP7, LTOP8, LTOP9
      EQUIVALENCE    ( LQ(1), LTOP1, LTOP, LUXSTR )
      EQUIVALENCE    ( LQ(2), LTOP2 )
      EQUIVALENCE    ( LQ(3), LTOP3 )
      EQUIVALENCE    ( LQ(4), LTOP4 )
      EQUIVALENCE    ( LQ(5), LTOP5 )
      EQUIVALENCE    ( LQ(6), LTOP6 )
      EQUIVALENCE    ( LQ(7), LTOP7 )
      EQUIVALENCE    ( LQ(8), LTOP8 )
      EQUIVALENCE    ( LQ(9), LTOP9 )
      EQUIVALENCE    (LUXREF(2),LINKS)
*
*     IXSTOR -- main ZEBRA store
*     IXDIV  -- ZEBRA division for data
*     IXRDIV -- ZEBRA division for run information (SOR,COR etc)
*
      INTEGER        IXSTOR, IXDIV, IXRDIV
      COMMON /PHDIV/ IXSTOR, IXDIV, IXRDIV
*
*     MAXPIL -- maximum pilot length
*     MXPBUF -- maximum number of pilots buffered
*     NBMIDS -- LQ(.) is a link to temporary minidst structure
*     NPILOT -- current pilot length
*     IPILOT -- current pilot
*     NPILTS -- array of lengths of the pilots buffered
*     IPILTS -- pilots buffered
*     NBPLTS -- number of pilots buffered
*
      PARAMETER      ( MAXPIL = 1024, MXPBUF = 10, NBMIDS = 9 )
      COMMON /PHPIL/ NPILTS(MXPBUF),IPILTS(MAXPIL,MXPBUF), NBPLTS
*
      INTEGER         LTEMP  ,LRTOP  ,LSTOP  ,LTTOP  ,LITOP  ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
      COMMON /UXLINK/ LTEMP(2) ,LRTOP ,LSTOP ,LTTOP ,LITOP ,LRTEMP ,
     +                LRWTMP ,LRAWUX ,LBKTOP ,LORTOP ,LRTINT ,LDTOP
* 
      INTEGER    NCPTMX
      PARAMETER (NCPTMX=100)
      INTEGER          LRUNLK  ,LSTRUN,LEORUN,LCORUN,LCPT,
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP,
     +                          LORSTR,LOREOR,LORCOR,LORCPT
      COMMON /UXLRUN/ LRUNLK(2),LSTRUN,LEORUN,LCORUN,LCPT(NCPTMX),
     +                          LSTTMP,LENTMP,LCOTMP,LCPTMP(NCPTMX),
     +                          LORSTR,LOREOR,LORCOR,LORCPT(NCPTMX)
*
      INTEGER  NWPXMA,NWPILT,IUPILT 
      PARAMETER (NWPXMA=1024)
      COMMON /PXCHDR/NWPILT,IUPILT(NWPXMA)
*
*
*--   Make equivalence between PHDST and PXDST pilots
      INTEGER  NPILOT, IPILOT(MAXPIL)
      EQUIVALENCE ( NPILOT, NWPILT )
      EQUIVALENCE ( IPILOT(1), IUPILT(1) )
*
*
*---------------------------------------------------------------------*
*       General information about event processing:
*       INTRCT = .TRUE. if job is INTeRaCTive
*       NFZFIL = Number of files(mediums) processed including
*                current
*       NFZPIL = Number of pilots read for all files
*       NFZPIX = Number of pilots read for current file
*       NFZGET = Number of data's read for all files
*       NFZGEX = Number of data's read for current file
*       NEvent = Number of investigated events for all files
*       NEvenX = Number of investigated events for current file
*       NGOODS = Number of events selected for all files
*       NGOODX = Number of events selected for current file
*       NFILOU = Number of files written
*       NDSSAV(1..9) -- Number of d/s saved on each stream
*       NDSSAV(10) == NEVOUT   -- Total number of d/s saved
*       TIMTOT = Approximate job time limit ( sec )
*       TIMEND = Time needed for termination of the job ( sec )
*       MAXPST = Maximum number of prestage command
*       LIMPSF = Limit on number of prestaged files in one prestage command
*       LAPSTG = Lapse in sec between PRESTAGE and STAGE commands
*       PACKED = .TRUE. if MiniDST used
*       IVPACK = Version of PHMINI used
*       FILIMT = Size limit of an output file (in mega-words)
*       STGPUTW = .TRUE. -- stageput will be executed with wait option
*       SGNEXIT != 0 -- OS signal came asking to terminate the job
*--------------------------------------------------------------------*
      INTEGER        PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              NFILOU,NEVOUT,NDSSAV
     +,              MAXPST,LIMPSF,LAPSTG,IVPACK,SGNEXIT,PHGLST
      LOGICAL        INTRCT,PACKED,FILACC, STGPUTW
      REAL           TIMTOT,TIMEND, FILIMT
      COMMON/PHGEN/  PHGFST,NFZFIL,NFZPIL,NFZGET,NFZPIX,NFZGEX
     +,              NEVENT,NGOODS,NEVENX,NGOODX
     +,              INTRCT, TIMTOT,TIMEND, MAXPST, LIMPSF, LAPSTG
     +,              NFILOU, NDSSAV(10)
     +,              PACKED, IVPACK, FILIMT, FILACC, STGPUTW, SGNEXIT
     +,              PHGLST
      EQUIVALENCE ( NDSSAV(10), NEVOUT )
*
*
*---------------------------------------------------------------------*
*       General information about event from pilot
*                 DAS
*      IIIEXP --  5.Experiment number
*      IIIRUN --  6.Run number
*      IIFILE --  7.File sequence number
*      IIIEVT --  9.Event number
*      IIIDAT -- 10.Event date ('yymmdd')
*      IIITIM -- 11.Event time ('hhmmss')
*                 LEP
*      IIFILL --  6.Fill number
*---------------------------------------------------------------------*
      INTEGER         IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
      COMMON /PHCIII/ IIIEXP,IIIRUN,IIFILE,IIIEVT,IIIDAT,IIITIM,IIFILL
*
*
*     FOR EVENT LIST
*
*     EVLPHV -- PHDST version used to write the Event List
*     NRUNEL -- run   number as it is read from Event List
*     NEVTEL -- event number as it is read from Event List
*     NUW    -- number of user words for the current event
*     IUWORD -- array containing the user words
*
      INTEGER     NUWMAX
      PARAMETER ( NUWMAX = 100 )
      INTEGER         EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD
      COMMON /PHEVLC/ EVLPHV, NRUNEL, NEVTEL, NUW, IUWORD(NUWMAX)

*
*     FOR RANDOM ACCESS TO DISK FZ-FILES
*
*     IRABSZ -- sizes of banks for random access
*     IRABDL -- quantum to increment the size of bank for random access
*     NMASKS -- number of masks for current criterion
*     MASKEV -- masks of criterion
*     JDSA1  -- number of the physical record in which
*               the current input d/s starts
*     JDSA2  -- off-set within this record
*     IRSTOR -- store index
*     IRDIV1 -- division index
*     LDIRAT -- links to the DaT banks
*     IQRAN  -- store for the DaT banks
*     NRANUS -- numbers of words of the DaT banks used
*     0:9    -- mediums
*
      INTEGER     ISZRAN         , NMSKMX
      PARAMETER ( ISZRAN = 250000, NMSKMX = 50 )
      INTEGER         IRABSZ, IRABDL, NMASKS, MASKEV, IRSTOR, IRDIV1
      INTEGER         NRANUS, FENRAN, LQRAN, JDSA1, JDSA2
      COMMON /PHRNDM/ IRABSZ(9), IRABDL
     +              , NMASKS, MASKEV(4,NMSKMX)
     +              , JDSA1, JDSA2, IRSTOR, IRDIV1
     +              , NRANUS(0:9), FENRAN(10), LQRAN(ISZRAN)
      INTEGER LDIRAT(0:9), IQRAN(ISZRAN)
      EQUIVALENCE ( LDIRAT(0), LQRAN(1) ), ( IQRAN(1), LQRAN(9) )

CC 27-MAR-1996
CC+KEEP, PHYEAR.
CC*
CC*     IYEAR -- year of DELANA processing
CC*
CC      INTEGER        IYEAR, NMULAY
CC      COMMON /PHYEAR/IYEAR, NMULAY
CC+SEQ, PHYEAR.
 
*
*      NVECP               - Number of particles in VECP
*     NCVECP               - Number of charged particles
*     NNVECP               - Number of neutral particles
*     NVECMC               - Number of simulated tracks
*     NJET                 - Number of jets
*       VECP(10, 3*MTRACK) - Real    vector of particle info
*      IVECP(10, 3*MTRACK) - Integer vector of particle info
*      LVECP    (3*MTRACK) - LPA/LST links    of the tracks
*     INVECP    (3*MTRACK) - LPA/LST indecies of the tracks
*     LVLOCK    (3*MTRACK) - Bitted word per track
*
*      VECP( 1,I) - P(x)
*      VECP( 2,I) - P(y)
*      VECP( 3,I) - P(z)
*      VECP( 4,I) - E
*      VECP( 5,I) - Mass
*      VECP( 6,I) - |P|
*      VECP( 7,I) - Charge
*      VECP( 8,I) - Mass code
*     IVECP( 9,I) - Mass identification
*     IVECP(10,I) - Jet number
*
*+KEEP,MTRACK.               from VECSUB
*      INTEGER MTRACK
*      PARAMETER (MTRACK = 300)
*
*+KEEP,PUCPPP.               from VECSUB
*+CDE, MTRACK.
*      COMMON /PUCPPP/ VECP(10, 3*MTRACK)
*      REAL VECP
*      INTEGER IVECP(10, 3*MTRACK)
*      DOUBLE PRECISION PVECP(5, 3*MTRACK)
*      EQUIVALENCE ( VECP(1,1), PVECP(1,1))
*      EQUIVALENCE (IVECP(1,1),  VECP(1,1))
*
*+KEEP,PUCLLL.               from VECSUB
*      COMMON /PUCLLL/ LVECP(3*MTRACK),LVECB0(3*MTRACK),
*     + LVECBN(MXVECB,3*MTRACK),NVECBN(MXVECB,3*MTRACK)
*      INTEGER LVECP
*
*  Common block used by the VECSUB package
*  The size can be enlarged from MTRACK to whatever you want
      INTEGER MTRACK
      PARAMETER (MTRACK=300)
      COMMON /PUCPPP/VECP(10, 3*MTRACK)
      REAL VECP
      INTEGER IVECP(10, 3*MTRACK)
      DOUBLE PRECISION PVECP(5, 3*MTRACK)
      EQUIVALENCE ( VECP(1, 1), PVECP(1, 1))
      EQUIVALENCE (IVECP(1, 1),  VECP(1, 1))
*  Common block used to store PA links for the tracks
*  If you increase MXVECB, make sure MXBLCK in DSTANA
*  is also changed!
      INTEGER MXVECB
      PARAMETER(MXVECB=40)
      COMMON /PUCLLL/LVECP(3*MTRACK),LVECB0(3*MTRACK),
     &LVECBN(MXVECB,3*MTRACK),NVECBN(MXVECB,3*MTRACK)
      INTEGER LVECP,LVECB0,LVECBN,NVECBN
*
      INTEGER NVECP,NCVECP,NNVECP,NVECMC,NJET,LVLOCK,INVECP
      COMMON /PSCVEC/ NVECP,NCVECP,NNVECP,NVECMC,NJET
     +,               LVLOCK(3*MTRACK),INVECP(3*MTRACK)
*
*
*     ISVER  -  Mini/short DST version number
*     IHAD4  -  Hadronic tag (team4 criteria)
*     NCTR4  -  Charged multiplicity (team 4)
*     NCTRK  -  Charged multiplicity (total)
*     NNTRK  -  Neutral multiplicity
*     ECMAS  -  Center of mass energy
*     ECHAR  -  Total charged energy
*     EMNEU  -  Total electromagnetic neutral energy
*     EHNEU  -  Total hadronic neutral energy
*     CDTYPE -  FullDST/shortDST type (from DSTQID)
*     LABO   -  Laboratory identifier (see TANAGRA manual)
*
*
      INTEGER ISVER,IHAD4
      INTEGER NCTR4,NCTRK,NNTRK
      REAL    ECMAS,ECHAR,EMNEU,EHNEU
      CHARACTER*4 CDTYPE,LABO
*
      COMMON /PSCEVT/ ISVER,IHAD4,NCTR4,NCTRK,NNTRK
     +,               ECMAS,ECHAR,EMNEU,EHNEU
      COMMON /PSCEVC/ CDTYPE,LABO
*
*                         ( PA extra-module TRAC (8) )
*
*     NTRAC                - Number of tracks
*     LENTRA               - Length of track information
*     QTRAC(LENTRA,MTRACK) - Real    array of  TRAC info
*     KTRAC(LENTRA,MTRACK) - Integer array of  TRAC info
*
*     KTRAC( 1,I) - Index of the origin vertex                  - s l
*     KTRAC( 2,I) - Index of the decay  vertex                  - s l
*     KTRAC( 3,I) - Index of the simulated track                m s l
*     QTRAC( 4,I) - Impact parameter in R_Phi                   m s l
*     QTRAC( 5,I) - Impact parameter in Z                       m s l
*     QTRAC( 6,I) - Theta angle at the perigee                  m s l
*     QTRAC( 7,I) - Phi   angle at the perigee                  m s l
*     QTRAC( 8,I) - 1/R. (curvature with sign at the perigee)   m s l
*     QTRAC( 9,I) - )
*     . . . . . . - ) Weight matrix                             m s l
*     QTRAC(23,I) - )
*     QTRAC(24,I) - Track lenght (negative for tracks NOT
*                   participating in the primary vertex fit)    m s l
*     KTRAC(25,I) - Detectors used for the reconstruction       m s l
*                   (bits 1-VD,2-ID,3-TPC,4-OD,5-FCA,6-FCB)
*     QTRAC(26,I) - Radius of the first measured point          m s l
*     QTRAC(27,I) - Z coor.of the first measured point          m s l
*     QTRAC(28,I) - Chi2 of the track fit without VD            m s l
*     QTRAC(29,I) - Chi2 of the track fit with    VD            m s l
*     KTRAC(30,I) - Number of d.o.f. of track fit without VD    m s l
*     KTRAC(31,I) - Number of d.o.f. of track fit with    VD    m s l
*     KTRAC(32,I) - Number of R_Phi ass. hits in 3 VD layers    m s l
*                   (bits 1:2-layer1, 3:4-layer2, 5:6-layer3)
*     KTRAC(33,I) - Number of R_Z   ass. hits in 2 VD layers    m s l
*                   (bits 1:2-layer1, 3:4-layer2)
*     QTRAC(34,I) - R_Phi residual between the track
*                   and the first measured point                m s l
*     QTRAC(35,I) - Error of the R_Phi residual                 m s l
*     QTRAC(36,I) - R_Z  residual between the track
*                   and the first measured point                m s l
*     QTRAC(37,I) - Error of the R_Z  residual                  m s l
*     QTRAC(38,I) - Imp.parameter with geom.sign to prim.vertex m s l
*     QTRAC(39,I) - Z imp. parameter with geometrical sign
*                   to the primary vertex                       m s l
*     QTRAC(40,I) - Imp.parameter with geom.sign to beam spot   m s l
*     QTRAC(41,I) - Energy error (for the team4 cut)            m s l
*     QTRAC(42,I) - Chi2 of VD hits associated with the track   m s l
*
*
      INTEGER    LENTRA
      PARAMETER (LENTRA =  42)
*
      INTEGER NTRAC,KTRAC
      REAL    QTRAC(LENTRA,MTRACK)
*
      COMMON /PSCTRA/ NTRAC,KTRAC(LENTRA,MTRACK)
      EQUIVALENCE (QTRAC,KTRAC)
*
*                            ( PA extra-module EMNC (22) )
*
*     NEMF                - Number of tracks with EMF shower
*     LENEMF              - Length of EMF module information
*     QEMF(LENEMF,MTRACK) - Real    array of EMF information
*     KEMF(LENEMF,MTRACK) - Integer array of EMF information
*
*     QEMF( 1,I) - Energy of the shower
*     QEMF( 2,I) - Theta  of the shower
*     QEMF( 3,I) - Phi    of the shower
*     KEMF( 4,I) - Identification code
*     KEMF( 5,I) - Number of glasses
*     KEMF( 6,I) - Glass hit pattern
*     KEMF( 7,I) - Number of ass. showers
*     QEMF( 8,I) - Total shower energy
*
*
      INTEGER    LENEMF
      PARAMETER (LENEMF = 8)
*
      INTEGER NEMF,KEMF
      REAL    QEMF(LENEMF,MTRACK)
*
      COMMON /PSCEMF/ NEMF,KEMF(LENEMF,MTRACK)
      EQUIVALENCE (QEMF,KEMF)
*
*                            ( PA extra-module EMNC (22) )
*
*     NHPC                - Number of tracks with HPC shower
*     LENHPC              - Length of HPC information
*     QHPC(LENHPC,MTRACK) - Real    array of HPC information
*     KHPC(LENHPC,MTRACK) - Integer array of HPC information
*
*     QHPC( 1,I) - Energy of the shower
*     QHPC( 2,I) - Theta  of the shower
*     QHPC( 3,I) - Phi    of the shower
*     KHPC( 4,I) - Identification code
*     KHPC( 5,I) - Number of layers
*     KHPC( 6,I) - Layer hit pattern
*     KHPC( 7,I) - Number of ass. showers
*     QHPC( 8,I) - Total shower energy
*
*
      INTEGER    LENHPC
      PARAMETER (LENHPC = 8)
*
      INTEGER NHPC,KHPC
      REAL    QHPC(LENHPC,MTRACK)
*
      COMMON /PSCHPC/ NHPC,KHPC(LENHPC,MTRACK)
      EQUIVALENCE (QHPC,KHPC)
*
*
*     NHPLAY              - Number of HPC layers
*     HPCLAY(NL,MTRACK)   - Real array of HPC layer energy
*
*     HPCLAY( 1,I) - Energy deposit in the 1-st layer 
*     HPCLAY( 2,I) - Energy deposit in the 2-nd layer 
*     ...            ...
*     HPCLAY(10,I) - Energy deposit in the last layer 
*
      INTEGER    NHPLAY
      PARAMETER (NHPLAY = 10)
*
      REAL HPCLAY(NHPLAY,MTRACK)
*
      COMMON /PSCHPL/ HPCLAY
*
*                            ( PA extra-module SSTC (33) )
*
*     NSTIC                - Number of tracks with STIC shower  - s f
*     LENSTC               - Length of STIC information
*     QSTIC(LENSTC,MTRACK) - Real    array of STIC information
*     KSTIC(LENSTC,MTRACK) - Integer array of STIC information
*
*     QSTIC( 1,I) - Energy of the shower                        - s f
*     QSTIC( 2,I) - Theta  of the shower                        - s f
*     QSTIC( 3,I) - Phi    of the shower                        - s f
*     KSTIC( 4,I) - Number of towers in the shower              - s f
*     KSTIC( 5,I) - Charged tag from the large veto :           - s f
*                    0 - no information available
*                    1 - hard photon
*                    2 - soft photon
*                    3 - electron
*     KSTIC( 6,I) - Charged tag from the combined veto :        - s f
*                    0 - no information available
*                    1 - hard photon
*                    2 - soft photon
*                    3 - very soft photon
*                    4 - electron
*     KSTIC( 7,I) - Veto multiplicity side A                    - s f
*     KSTIC( 8,I) - Veto multiplicity side B                    - s f
*     KSTIC( 9,I) - Silicon strip vertex position               - s f
*
*
      INTEGER    LENSTC
      PARAMETER (LENSTC = 9)
*
      INTEGER NSTIC,KSTIC
      REAL    QSTIC(LENSTC,MTRACK)
*
      COMMON /PSCSTC/ NSTIC,KSTIC(LENSTC,MTRACK)
      EQUIVALENCE (QSTIC,KSTIC)
*
*                            ( PA extra-module HCNC (23) )
*
*     NHAC                - Number of tracks with HAC shower
*     LENHAC              - Length of HAC information
*     QHAC(LENHAC,MTRACK) - Real    array of HAC information
*     KHAC(LENHAC,MTRACK) - Integer array of HAC information
*
*     QHAC( 1,I) - Energy of the shower
*     QHAC( 2,I) - Theta  of the shower
*     QHAC( 3,I) - Phi    of the shower
*     KHAC( 4,I) - Identification code
*     KHAC( 5,I) - Number of towers
*     KHAC( 6,I) - Tower hit pattern (8 bits per layer)
*     KHAC( 7,I) - Number of ass. showers
*     QHAC( 8,I) - Total shower energy
*
*
      INTEGER    LENHAC
      PARAMETER (LENHAC = 8)
*
      INTEGER NHAC,KHAC
      REAL    QHAC(LENHAC,MTRACK)
*
      COMMON /PSCHAC/ NHAC,KHAC(LENHAC,MTRACK)
      EQUIVALENCE (QHAC,KHAC)
*
*
*     NHCLAY              - Number of HAC layers
*     HACLAY(NL,MTRACK)   - Real array of HAC layer energy
*
*     HACLAY( 1,I) - Energy deposit in the 1-st layer 
*     HACLAY( 2,I) - Energy deposit in the 2-nd layer 
*     ...            ...
*     HACLAY( 4,I) - Energy deposit in the last layer 
*
      INTEGER    NHCLAY
      PARAMETER (NHCLAY = 4)
*
      REAL HACLAY(NHCLAY,MTRACK)
*
      COMMON /PSCHCL/ HACLAY
*
*
*     NP      - number of particles in current event
*
*     KP(I,1) - status code KS of the current particle
*     KP(I,2) - particle code KF (Particle Data Group)
*     KP(I,3) - index of parent particle, or jet, or 0
*     KP(I,4) - index of the first daughter, if none 0
*     KP(I,5) - index of the last  daughter, if none 0
*
*     PP(I,1) - Px, momentum in the x direction, in GeV/c
*     PP(I,2) - Py, momentum in the y direction, in GeV/c
*     PP(I,3) - Pz, momentum in the z direction, in GeV/c
*     PP(I,4) - E , energy, in GeV
*     PP(I,5) - m , mass, in GeV/c**2
*
*     VP(I,1) - x position of production vertex, in mm
*     VP(I,2) - y position of production vertex, in mm
*     VP(I,3) - z position of production vertex, in mm
*     VP(I,4) - time of production
*     VP(I,5) - proper lifetime of the particle, or 0.
*
      INTEGER    NPMAX
      PARAMETER (NPMAX = 4000)
*
      INTEGER NP,KP
      REAL    PP,VP
*
      COMMON /PSCLUJ/ NP,KP(NPMAX,5),PP(NPMAX,5),VP(NPMAX,5)
*
*                         LUJETS fragmentation and decay
*
*     ZF(I)   - z of fragmentaion
*     DM(I)   - decay mode in LUND
*
      REAL    ZF,DM
*
      COMMON /PSCLUF/ ZF(NPMAX),DM(NPMAX)
*
*
*                            ( PA extra-module MUID (24) )
*
*     NMUID                - Number of tracks with MUID info
*     LENMUD               - Length of the Muon Id information
*     QMUID(LENMUD,MTRACK) - Real    array of muon information
*     KMUID(LENMUD,MTRACK) - Integer array of muon information
*
*     KMUID(1,I) - Muon tag from MUCAL2
*     QMUID(2,I) - Global chi2 of very loose muon refit
*     KMUID(3,I) - Hit pattern with inefficiencies
*
*
      INTEGER    LENMUD
      PARAMETER (LENMUD = 3)
*
      INTEGER NMUID,KMUID
      REAL    QMUID(LENMUD,MTRACK)
*
      COMMON /PSCMUD/ NMUID,KMUID(LENMUD,MTRACK)
      EQUIVALENCE (QMUID,KMUID)
*
*                            ( PA extra-module ELID (25) )
*
*     NELID                - Number of tracks with ELID info    m s l
*     LENELD               - Length of information per electron
*     QELID(LENELD,MTRACK) - Real    array of elec. information
*     KELID(LENELD,MTRACK) - Integer array of elec. information
*
*     KELID(1,I) - Electron tag from ELECID :                   m s l
*                  0 - no identification run
*                  1 - identified NOT to be an electron
*                  2 - very loose electron
*                  3 - loose      electron
*                  4 - standard   electron
*                  5 - tight      electron
*     KELID(2,I) - Gamma conversion tag from ELECID (veto)      m s l
*                  1 - loose      electron
*                  2 - standard   electron
*                  3 - tight      electron
*     QELID(3,I) - Best estimate of electron Px at vertex       m s l
*     QELID(4,I) - Best estimate of electron Py at vertex       m s l
*     QELID(5,I) - Best estimate of electron Pz at vertex       m s l
*
*
      INTEGER    LENELD
      PARAMETER (LENELD = 5)
*
      INTEGER NELID,KELID
      REAL    QELID(LENELD,MTRACK)
*
      COMMON /PSCELD/ NELID,KELID(LENELD,MTRACK)
      EQUIVALENCE (QELID,KELID)
*
*
*     NPA       -  Size of the IPAST       table                m s l
*     NST       -  Size of the ISTPA,ISTLU tables               m s l
*     NLU       -  Size of the ILUST       table                m s l
*
*
*            Tables of correspondance PA <==> PV 
*
*     IPAPV(1,I) - Index of origin vertex of I-th PA            m s l
*     IPAPV(2,I) - Index of decay  vertex of I-th PA            m s l
*
*            Tables of correspondance ST <==> PA
*
*     ISTPA(I)  -  PA indices ( ST -> PA table )                m s l
*     IPAST(I)  -  ST indices ( PA -> ST table )                m s l
*
*            Tables of correspondance LU <==> ST
*
*     ILUST(I)  -  ST indices ( LU -> ST table )                m s l
*     ISTLU(I)  -  LU indices ( ST -> LU table )                m s l
*
*            Tables of correspondance ST <==> VX (vertex)
*
*     ISTVX(1,I) - Index of origin vertex of I-th ST            m s l
*     ISTVX(2,I) - Index of decay  vertex of I-th ST            m s l
*
*
      INTEGER    NVMAX
      PARAMETER (NVMAX = 511)
*
      INTEGER NPA,NST,NSH,NLU
      INTEGER ISTSH,ISHST
      INTEGER IPAST,ISTPA
      INTEGER ILUSH,ISHLU
      INTEGER ISTLU,ILUST
      INTEGER ISTVX,IPAPV
*
      COMMON /PSCTBL/ NPA,NST,NSH,NLU
     +,               ISHST(NVMAX),ISTSH(NVMAX)
     +,               ISTPA(NVMAX),IPAST(NVMAX)
     +,               ISHLU(NVMAX),ILUSH(NVMAX)
     +,               ILUST(NVMAX),ISTLU(NVMAX)
     +,               IPAPV(2,NVMAX)
     +,               ISTVX(2,NVMAX)
*
*
*
*     NVTX                  - Number of reconstr. vertices
*     NVTXMC                - Number of simulated vertices
*     NVTXMX                - Maximum number  of  vertices
*     LENVTX                - Length of vertex information
*     QVTX(LENVTX,2*NVTXMX) - Real    array  of VTX info
*     KVTX(LENVTX,2*NVTXMX) - Integer array  of VTX info
*     LVTX       (2*NVTXMX) - LPV/LSP links for vertices
*
*     KVTX( 1,I) - Index of the first outgoing particle
*     KVTX( 2,I) - Index of the incomming particle
*     KVTX( 3,I) - Nb of outgoing particles (multiplicity)
*     KVTX( 4,I) - Nb of degree of freedom of the vertex fit
*     KVTX( 5,I) - Mass code of the origin particle
*     QVTX( 6,I) - X
*     QVTX( 7,I) - Y    coordinates of the vertex
*     QVTX( 8,I) - Z
*     QVTX( 9,I) - Chi2 of the vertex fit
*     QVTX(10,I) - XX
*     QVTX(11,I) - XY
*     QVTX(12,I) - YY   Error matrix
*     QVTX(13,I) - XZ
*     QVTX(14,I) - YZ
*     QVTX(15,I) - ZZ
*     KVTX(16,I) - Error flag
*     KVTX(17,I) - Vertex status bits :
*                  bit 1 set on if dummy vertex
*                  bit 2 set on if secondary vertex
*                  bit 3 set on if secondary hadronic vertex
*                  bit 4 set on if vertex with simulation data
*
*
      INTEGER    LENVTX
      PARAMETER (LENVTX =  17)
*
      INTEGER    NVTXMX
      PARAMETER (NVTXMX = 150)
*
      INTEGER NVTX,NVTXMC,KVTX,LVTX
      REAL    QVTX(LENVTX,2*NVTXMX)
*
      COMMON /PSCVTX/ NVTX,NVTXMC
     +,               LVTX(       2*NVTXMX)
     +,               KVTX(LENVTX,2*NVTXMX)
      EQUIVALENCE (QVTX,KVTX)
*

*
      INTEGER I, IERR
*
*
*--- (re)fill skelana commons
      CALL PSBEG

*---  Dump event information as plain text
      PRINT *, "--------------------------"
      WRITE(*,98)      "HAPPY CHECK: EVENT:",IIIRUN, IIIEVT
      WRITE(*,97)      "CHECK: ECM:",ECMAS
      WRITE(*, 99)     "CHECK: TRACKS:", NVECP, NELID, NMUID
      DO I=1, NVECP
         WRITE(*, *)  I, VECP(7,I), VECP(1,I),VECP(2,I),
     $        VECP(3,I), VECP(4,I), QEMF(8,I), QHPC(8,I),
     $        QHAC(8,I), QSTIC(1,I), LVLOCK(I), QTRAC(4,I),
     $        QTRAC(5,I), QTRAC(24,I), KELID(1, I), KELID(2, I),
     $        KMUID(1, I)
      ENDDO
      WRITE(*, 104)     "CHECK: GEN:", NP
      DO I=1, NP
         WRITE(*, *)  I, KP(I,1), KP(I,2), PP(I,1), PP(I,2),
     $        PP(I,3), PP(I,4), PP(I,5) 
      ENDDO
      WRITE(*, 105)     "CHECK: SIM:", NVECMC
      DO I=1, NVECMC
         WRITE(*, *)  I, KP(ISTSH(I), 1), KP(ISTSH(I), 2), 
     $        VECP(1,300+I), VECP(2,300+I), VECP(3,300+I),
     $        VECP(4,300+I), VECP(5,300+I), VECP(7,300+I),
     $        ISTVX(1, I), ISTVX(2, I)
      ENDDO
*-      WRITE(*, 105)     "CHECK: VTX:", NVTXMC
*-      DO I=1, NVTXMC
*-         WRITE(*, *)  150+I, KVTX(1,150+I), KVTX(2,150+I),
*-     $        QVTX(6,150+I), QVTX(7,150+I), QVTX(8, 150+I)
*-      ENDDO
 97   FORMAT(A,F10.2)
 98   FORMAT(A,I10,I10)
 99   FORMAT(A,I10,I10,I10)
 104  FORMAT(A,I10)
 105  FORMAT(A,I10)
* 105  FORMAT(A,I10,F12.6, F12.6, F12.6, F12.6, F12.6, F12.6,I10)
* 106  FORMAT(A,"(",F12.6,",",F12.6,",",F12.6,",",F12.6,")")
      END
*
CDECK  ID>, USER99.
      SUBROUTINE USER99
************************************************************************
*                                                                      *
*     Name           :  USER99                                         *
*     Called by      :  PHEND                                          *
*     Date of coding :  Aug 07, 1993                                   *
*     Last update    :  Mar 25, 1994                                   *
*     Task           :  To be called at the end of the run             *
*                                                                      *
************************************************************************
      INTEGER LUNMU1,LUNEL1,LUNLP2
      PARAMETER (LUNMU1 = 21)
      PARAMETER (LUNEL1 = 22)
      PARAMETER (LUNLP2 = 23)
*
*
*--   Close the event list files
      CLOSE(LUNMU1)
      CLOSE(LUNEL1)
      CLOSE(LUNLP2)
*
      END
*
