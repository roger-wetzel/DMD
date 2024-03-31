; SPREADPOINT
; DEPECHE'S MODE DEMO (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2024 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o dmd -nosym dmd.s

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

inviswidth	equ	20 ; *8 = 160 px
pwidth		equ	44+inviswidth ; 352+160=512 to get rid of multiplication
logowidth	equ	44 ; bytes
logoheight	equ	36 ; px
planetwidth	equ	44 ; bytes
center		equ	22 ; bytes
pheight		equ	256 ; px
psize		equ	pheight*pwidth
safezone	equ	100*pwidth
yfillmargin	equ	60 ; px

voradjust	equ	50*pwidth

numplanes	equ	2
numorbits	equ	6
numcoverrows	equ	7
numcovercols	equ	27
coverrowheight	equ	13

tsize		equ	16*planetwidth
numtbuffers	equ	4

mccoywidth	equ	40
mccoyheight	equ	256
mccoypsize	equ	mccoywidth*mccoyheight
mccoycenter	equ	mccoypsize/2+(mccoywidth/2)
numboingsteps	equ	14
boingheight	equ	98
boingpsize	equ	mccoywidth*boingheight

; variants
blue		equ	0

; profiling
numbers		equ	0

availablemem	equ	0
timing		equ	0
testing		equ	0
textinfo	equ	0
musicpos	equ	0

; DMACON
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bit plane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA
SPREN		equ	1<<5		; Sprite DMA


*------	MACROS ----------------------------------------------------------------*

	macro STARTACTOR
	add.l	#1<<\1,v_actors(a5)	;
	endm

	macro STOPACTOR
	sub.l	#1<<\1,v_actors(a5)	;
	endm


*------	ALLOCATE MEMORY AND SAVE STATE ----------------------------------------*

base	movem.l	a0-a6/d0-d7,-(a7)	;
	bsr	alloc			;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	lea	vars(pc),a5		;
	move.l	d0,v_number(a5)		; free (available) chip memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;
	bsr	waitblitter		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$96(a6)		; disable all DMAs

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	INIT ------------------------------------------------------------------*

	lea	vars(pc),a5		;

	move.l	v_sky(a5),v_skydatap(a5) ;

	lea	playcmds(pc),a0		;
	move.l	a0,v_cmdspointer(a5)	;
	STARTACTOR actor_player		;

	move.l	b_coverdata(pc),v_coverdatap(a5) ;

	move.w	#1*pheight<<6+(pwidth-inviswidth)>>1,v_bltsize(a5) ; size = 1 bitplane

	move.w	#$0168,v_distance(a5)	;
	move.w	#$0124,v_a(a5)		;
	move.w	#pheight/2*pwidth+center-voradjust,v_yoffset(a5) ;

	move.l	b_boing(pc),v_boingp(a5) ;
	
	lea	person(pc),a0		;
	move.l	a0,v_personp(a5)	;
	move.w	#2,v_coladdsub(a5)	; start with fading in
	move.w	#fcolorpend-fcolorp,v_colstop(a5) ;

	STARTACTOR actor_mccoy		; start with mcCoy tribute

	bsr	LSP_MusicInit		;

	move.l	b_clist3(pc),a2		;
	lea	LSP_State+m_dmaconPatch2(pc),a1 ;
	add.w	#lspdmacon3-clist3+3,a2	;
	move.l	a2,(a1)			;

	move.l	b_clist1(pc),a2		;
	lea	LSP_State+m_dmaconPatch3(pc),a1 ;
	add.w	#lspdmacon1-clist1+3,a2	;
	move.l	a2,(a1)			;

	move.l	b_clistcw1(pc),a2	;
	lea	LSP_State+m_dmaconPatch4(pc),a1 ;
	add.w	#lspdmacon-clistcw1+3,a2 ;
	move.l	a2,(a1)			;

	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	move.l	b_clist2(pc),a0		; init clist
	move.l	a0,$80(a6)		;
	lea	LSP_State+m_dmaconPatch(pc),a1 ;
	add.w	#lspdmacon2-clist2+3,a0	;
	move.l	a0,(a1)			;

	move.l	b_clistcw2(pc),$84(a6)	; init clist 2

	bsr	waitraster		; avoid flickering (?)
	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN+SPREN,$96(a6) ;

	move.w	#$c030,$9a(a6)		; enable coper and vertb interrupts

*------	IDLE LOOP -------------------------------------------------------------*

	bsr	precalc			;
	
*------	RESTORE STATE AND EXIT ------------------------------------------------*

	bsr	waitblitter		;
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		;
	
	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	
	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,a0-a6/d0-d7	;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	VARS ------------------------------------------------------------------*

; # = do not change order

	rsreset
v_doquit	rs.b	1	; signal quit
v_wait		rs.b	1

v_actors	rs.l	1

v_datapointer	rs.l	1

v_cmdspointer	rs.l	1

v_matrix	rs.w	3*3	; 3D rotation matrix
v_a		rs.w	1
v_b		rs.w	1

v_c1		rs.w	1	; #
v_c2		rs.w	1	; #
v_c3		rs.w	1	; #

v_c1offset	rs.w	1	; #
v_c2offset	rs.w	1	; #
v_c3offset	rs.w	1	; #

v_c4		rs.w	1	; "hook" and rotate only stuff
v_c4offset	rs.w	1	;
v_prev		rs.w	1	;

v_distance	rs.w	1

v_yoffset	rs.w	1

v_orbits	rs.l	numorbits*2 ; start, end
v_orbitindex	rs.w	1

v_voronoi	rs.l	1

v_bitplanes	rs.l	1

v_bltsize	rs.w	1	; see cls

v_logocolindex	rs.w	1	; logo fading

v_db		rs.l	0	; (v_db is a marker only)
v_dbplane1a	rs.l	1	; #
v_dbplane2a	rs.l	1	; #
v_dbplane1b	rs.l	1	; #
v_dbplane2b	rs.l	1	; #

v_db1a		rs.l	1	; #
v_db1b		rs.l	1	; #

v_coverdatap	rs.l	1	; pointer to (un)cover data

v_morphindex	rs.w	1	; actor morph data index

; text
v_textindex	rs.w	1
v_produce	rs.w	1
v_show		rs.w	1
v_waiting	rs.w	1
v_textplaneoffset	rs.w	1
v_showtextploffset	rs.w	1
v_usedtbuffers	rs.w	1
v_triggertext	rs.w	1

	if numbers
v_frame		rs.w	1	; frame counter
v_number	rs.l	1	; test value
v_waitcount	rs.w	1
	endif

v_2d		rs.w	30*2	; max 30 vertices
v_2dframe	rs.w	4*2	; 4 vertices

v_skydatap	rs.l	1	; pointer to sky data
v_boingp	rs.l	1
v_personp	rs.l	1
v_colindex	rs.w	1
v_colstop	rs.w	1
v_coladdsub	rs.w	1
v_mccoy		rs.l	1
v_sky		rs.l	1

v_ball		rs.l	1
v_dbcwplane1a	rs.l	1	; #
v_dbcwplane1b	rs.l	1	; #
v_cwplanetext	rs.l	1
v_animstep	rs.w	1	; circ-wave animation step (0,1,2,...)
v_clcolptr	rs.l	1	; circ-wave copper list color table ptr

sizeofvars	rs.w	0

vars	ds.b	sizeofvars
	even


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	v_actors(a5),d7		; process actors
	btst	#actor_mccoy,d7		; mccoy?
	beq	.circwave		;
	moveq	#mccoywidth,d2		;
	move.l	v_db1a(a5),d6		;
	beq	.stop			;
	bra	.print			;

.circwave
	move.l	v_actors(a5),d7		; process actors
	btst	#actor_circwave,d7	; circ-wave?
	beq	.voronoi		;
	move.l	v_dbcwplane1a(a5),d6	;
	beq	.stop			;
	moveq	#cwpwidth,d2		;
	bra	.print			;

.voronoi
	moveq	#pwidth,d2		;
	move.l	v_dbplane1a(a5),d6	; db buffers are swapped here already
	beq	.stop			; (could be zero)

.print	move.l	d6,a0			;
	add.w	d1,a0			;
 	moveq	#8-1,d7			; 8 digits
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.l	a0,a2			;
	rept 5
	move.b	(a1)+,(a2)		; print digit
	add.w	d2,a2			; next line
	endr
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
.stop	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3

	endif


*------	WAIT BLITTER ----------------------------------------------------------*

waitblitter
	move.w	#$8400,$96(a6)			; BLTPRI on
	btst.b	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst.b	#14-8,$02(a6)			;
	bne	.wait				;
	move.w	#$0400,$96(a6)			; BLTPRI off
	rts					;


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
.wait	move.l	$04(a6),d0			; h0ffman's variant
	and.l	#$0001ff00,d0			;
	cmp.l	#$30<<8,d0			;
	bne	.wait				;
	rts					;

	rem
	cmp.b	#1,$06(a6)			; Photon's variant
	bne	waitraster			;
	btst	#0,$05(a6)			;
	bne	waitraster			;
	rts					;
	erem


*------	PRECALCULATION --------------------------------------------------------*

precalc
.idle	move.l	v_actors(a5),d7			; process actors
	btst	#actor_gen_text,d7		; buffer (venus) available?
	beq	.no				;
	bsr	handletextbuffers		;
	bra	.done				;
.no	bsr	handlepreparemain		;
	bsr	handlepreparecircwave		;
.done	tst.b	v_doquit(a5)			;
	beq	.idle				;
	rts					;


*------	COPER -----------------------------------------------------------------*

coper	moveq	#$0010,d0			; delete coper request bit
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;

	if timing
	move.w	#$0f00,$180(a6)			;
	endif

	bsr	LSP_MusicPlayTick		;

	movem.l	(a7)+,a0-a6/d0-d7		;

	if timing
	move.w	#$0000,$180(a6)			;
	endif

	rte					;


*------	IRQ3 ------------------------------------------------------------------*

irq3	movem.l	a0-a6/d0-d7,-(a7)		;
	lea	vars(pc),a5			;
	lea	custom,a6			;
	move.w	$1e(a6),d0			; read interrupt request bits
	btst	#4,d0				;
	bne	coper				;

	moveq	#$0030,d0			; delete vertb and coper request bit
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;
		
	move	#$2200,sr			; allow other (coper) level 3 interrupts

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_player,d7		;
	beq	.noplay				;
	bsr	play				;
.noplay	move.l	v_actors(a5),d7			; process actors
	btst	#actor_mccoy,d7			;
	beq	.act2				;
	bsr	tributetomccoy			;
	movem.l	v_db1a(a5),d0/d1		; double buffering
	exg	d0,d1				; v_db1a <-> v_db1b
	movem.l	d0/d1,v_db1a(a5)		;
	bra	.done				;

.act2	move.l	v_actors(a5),d7			; process actors
	btst	#actor_circwave,d7		;
	beq	.act4				;
	bsr	circwave			;
	movem.l	v_dbcwplane1a(a5),d0/d1		; double buffering
	exg	d0,d1				; v_dbcwplane1a <-> v_dbcwplane1b
	movem.l	d0/d1,v_dbcwplane1a(a5)		;
	bra	.done				;

.act4	move.l	v_actors(a5),d7			; process actors
	btst	#actor_voronoi3d,d7		;
	beq	.done				;

	move.l	v_dbplane1a(a5),d0		; bitplane pointers
	move.l	v_actors(a5),d7			; process actors
	btst	#actor_grey,d7			;
	beq	.clist1				;
	move.l	b_clist3(pc),a1			;
	move.w	d0,bplvor3-clist3+2+4(a1)	;
	swap	d0				;	
	move.w	d0,bplvor3-clist3+2(a1)		;
	bsr	textplane			;
;	bra	.next				; DON'T branch to .next - transition
	move.l	v_dbplane1a(a5),d0		;
.clist1	move.l	v_dbplane2a(a5),d2		;
	bsr	setbitplanepointersclist1	; (parameters d0, d2)
.next	bsr	cls				; mtx and applymtx run simultaneously

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_logo_fading,d7		;
	beq	.act5				;
	bsr	logofading			;
.act5	move.l	v_actors(a5),d7			; process actors
	btst	#actor_orbits,d7		;
	beq	.act0				;
	bsr	moveorbits			;
.act0	move.l	v_actors(a5),d7			;
	btst	#actor_morph,d7			;
	beq	.act3				;
	move.w	v_morphindex(a5),d0		;

	lea	zoomdata(pc),a0			;
	move.w	(a0,d0.w),v_distance(a5)	;

	lea	yposdata(pc),a0			;
	move.w	(a0,d0.w),d1			;
	add.w	d1,v_yoffset(a5)		; move voronoi down

	lea	tiltdata(pc),a0			;
	move.w	(a0,d0.w),v_a(a5)		;
	addq.w	#2,v_morphindex(a5)		;
	cmp.w	#tiltdataend-tiltdata,v_morphindex(a5) ;
	bne	.act3				;
	STOPACTOR actor_morph			;
.act3	bsr	drawvoronoi			;

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_uncover_venus,d7		;
	beq	.act1				;
	bsr	uncover				;
	bra	.db				;
.act1	btst	#actor_cover_venus,d7		;
	beq	.db				;
	bsr	cover				;
.db	movem.l	v_db(a5),d0-d3			; double buffering
	exg	d0,d2				; v_dbplane1a <-> v_dbplane1b
	exg	d1,d3				; v_dbplane2a <-> v_dbplane2b (3d only)
	movem.l	d0-d3,v_db(a5)			;

.done
	if timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif
	
	if numbers
;	moveq	#0,d0				; value
	move.w	v_frame(a5),d0			; value
	asl.l	#8,d0				;
	move.b	v_waitcount(a5),d0		;
	asl.l	#8,d0				;
	move.b	v_wait(a5),d0			;
	
	
	moveq	#8-1,d1				; pos
	bsr	printnumber			;

	move.l	v_number(a5),d0			; value
;	move.l	#safezone+4*psize,d0		;
;	move.w	v_c2(a5),d0			;
;	move.w	v_distance(a5),d0		;

	if musicpos
	moveq	#0,d0				;
	bsr	LSP_MusicGetPos			;
	move.w	d0,v_number(a5)			;
	endif

	move.w	#10*pwidth+8-1,d1		; pos (vs mccoywidth)
	bsr	printnumber			;

	addq.w	#1,v_frame(a5)			; advance frame number
	endif

	btst	#6,$bfe001			; left mouse button pressed?
	seq	v_doquit(a5)			; thanks MnemoTroN ;-)

	if timing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	movem.l	(a7)+,a0-a6/d0-d7		;
	rte					;


*------	TRIBUTE TO MCCOY ------------------------------------------------------*

tributetomccoy
	move.l	v_db1a(a5),a0			;
	move.l	a0,$e0(a6)			; bitplane running person
	add.w	#128*mccoywidth,a0		;
	move.l	a0,$e4(a6)			;

	move.l	v_boingp(a5),d0			; boing
	move.l	b_boing(pc),a0			;
	add.l	#numboingsteps*boingpsize,a0	;
	cmp.l	a0,d0				;
	bne	.noreset			;
	move.l	b_boing(pc),d0			; reset boing animation
.noreset
	move.l	b_clist2(pc),a1			;
	move.w	d0,bplmccoy1-clist2+2+4(a1)	; 1st plane (bitplane 2)
	swap	d0				;
	move.w	d0,bplmccoy1-clist2+2(a1)	;
	swap	d0				;
	move.l	d0,d1				;
	add.l	#numboingsteps*boingpsize,d1	; 2nd plane (bitplane 3)
	move.w	d1,bplmccoy1-clist2+2+8+4(a1)	;
	swap	d1				;
	move.w	d1,bplmccoy1-clist2+8+2(a1)	;
	
	move.l	d0,d1				;
	add.l	#boingpsize/2,d1		;
	move.w	d1,bplmccoy3-clist2+2+4(a1)	; 1st plane lower half
	swap	d1				;
	move.w	d1,bplmccoy3-clist2+2(a1)	;
	swap	d1				;
	add.l	#numboingsteps*boingpsize,d1	; 2nd plane lower half
	move.w	d1,bplmccoy3-clist2+2+8+4(a1)	;
	swap	d1				;
	move.w	d1,bplmccoy3-clist2+8+2(a1)	;
	
	add.l	#boingpsize,d0			; next frame
	move.l	d0,v_boingp(a5)			;

	bsr	waitblitter			; clear running person screen
	move.w	#0,$66(a6)			; modulo D
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	v_db1b(a5),$54(a6)		; destination D
	move.w	#mccoyheight<<6+mccoywidth>>1,$58(a6) ; bltsize and start

	bsr	fading				;
	bsr	skyplayer			;


*------	SLOW LINE DRAWER FOR FILLED VECTORS -----------------------------------*

	move.l	v_personp(a5),a1		;
	move.b	(a1)+,d0			; num vertices
	move.l	a1,a2				;
	addq.b	#1,d0				;
	ext.w	d0				;
	add.w	d0,d0				;
	add.w	d0,a2				; a2 = line connections

	move.l	v_db1b(a5),a0			; bitplane buffer
;	add.w	#mccoycenter,a0			;
	add.w	#mccoycenter+(128*mccoywidth),a0 ;

	lea	$52(a6),a6			;
	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#mccoywidth,$60-$52(a6)		;
	move.w	#mccoywidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask
	
	move.b	(a2)+,d5			; num lines
	ext.w	d5				;
.loop	moveq	#0,d7				;
	move.b	(a2)+,d7			; index p1
	move.b	(a1,d7.w),d0			; x1
	ext.w	d0				;
	move.b	1(a1,d7.w),d1			; y1
	ext.w	d1				;

	move.b	(a2)+,d7			; index p2
	move.b	(a1,d7.w),d2			; x2
	ext.w	d2				;
	move.b	1(a1,d7.w),d3			; y2
	ext.w	d3				;
	
	cmp.w	d3,d1				; compare y
	bgt	.noswap				;
	exg	d0,d2				; swap -> second larger
	exg	d1,d3				;
.noswap	moveq	#4,d7				; clears upper word too
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0b4a,d7			; $0bca=or
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; read octant from table

	move.w	d1,d6				;
	asl.w	#5,d1				; *32
	asl.w	#3,d6				; *8
	add.w	d6,d1				; = *40 (mccoy pwidth)

	move.l	a0,a4				; bitplane
	add.w	d1,a4				;
	moveq	#7,d1				;
	sub.w	d0,d1				; see bchg below
	asr.w	#3,d0				; allow neg x coords
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;

	move.l	d3,$62-$52(a6)			; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; start address
	bchg	d1,(a4)				; flip pixel (important for filling)
	move.w	d2,(a3)				; start
	dbf	d5,.loop			;
	bra	next				;

; +2 = SING Single bit per horizontal line for use with subsequent area fill
.octs	dc.b	0*4+1+2, 2*4+1+2, 1*4+1+2, 3*4+1+2
	dc.b	4*4+1+2, 5*4+1+2, 6*4+1+2, 7*4+1+2
	dc.b	0*4+65+2, 2*4+65+2, 1*4+65+2, 3*4+65+2
	dc.b	4*4+65+2, 5*4+65+2, 6*4+65+2, 7*4+65+2


*------	FILL RUNNING PERSON ---------------------------------------------------*

next	moveq	#0,d0				; modulos
	moveq	#-1,d1				;
	move.l	v_db1b(a5),a0			;
	add.w	#mccoypsize-2,a0		;

	btst	#14-8,$02-$52(a6)		;
.waitblitter3
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter3			;

	move.l	d0,$64-$52(a6)			; modulo A and D
	move.l	#$09f00012,$40-$52(a6)		; 09f00012 exlusive, 09f0000a inclusive
	move.l	d1,$44-$52(a6)			; first/last word mask
	move.l	a0,$50-$52(a6)			; source A
	move.l	a0,$54-$52(a6)			; destination D
	move.w	#(mccoyheight/2)<<6+mccoywidth>>1,$58-$52(a6) ; bltsize and start


*------	SLOW LINE DRAWER ------------------------------------------------------*

	move.l	v_personp(a5),a1		;
	move.b	(a1)+,d0			; num vertices
	move.l	a1,a2				;
	addq.b	#1,d0				;
	ext.w	d0				;
	add.w	d0,d0				;
	add.w	d0,a2				; a2 = line connections

	move.l	v_db1b(a5),a0			; bitplane buffer
	add.w	#mccoycenter,a0			;

	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#mccoywidth,$60-$52(a6)		;
	move.w	#mccoywidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask
	
	move.b	(a2)+,d5			; num lines
	ext.w	d5				;
.loop	moveq	#0,d7				;
	move.b	(a2)+,d7			; index p1
	move.b	(a1,d7.w),d0			; x1
	ext.w	d0				;
	move.b	1(a1,d7.w),d1			; y1
	ext.w	d1				;

	move.b	(a2)+,d7			; index p2
	move.b	(a1,d7.w),d2			; x2
	ext.w	d2				;
	move.b	1(a1,d7.w),d3			; y2
	ext.w	d3				;

	cmp.w	d3,d1				; compare y
	bgt	.noswap				; (draw the same way as the "filled" lines)
	exg	d0,d2				; swap -> second larger
	exg	d1,d3				;
.noswap	moveq	#4,d7				; note: moveq clears d7's upper word
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0bca,d7			;
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; octant

	move.w	d1,d6				;
	asl.w	#5,d1				; *32
	asl.w	#3,d6				; *8
	add.w	d6,d1				; = *40 (mccoy pwidth)

	lea	(a0,d1.w),a4			;
	asr.w	#3,d0				;
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; set starting address
	move.w	d2,(a3)				; start
	dbf	d5,.loop			;

	lea	personend(pc),a0		; handle animation
	cmp.l	a2,a0				;
	bne	.neod				;
	lea	person(pc),a2			; reset animation
.neod	move.l	a2,v_personp(a5)		;

	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;

.octs	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------	DATA RUNNING PERSON ---------------------------------------------------*

s	equ	274
t	equ	256
d	equ	320
dx	equ	-7
dy	equ	-71

	; frame 1
person	dc.b	30-1
	dc.b	(20+dx)*s/d,(-71+dy)*t/d, (33+dx)*s/d,(-64+dy)*t/d, (28+dx)*s/d,(-52+dy)*t/d, (32+dx)*s/d,(-51+dy)*t/d
	dc.b	(25+dx)*s/d,(-40+dy)*t/d, (30+dx)*s/d,(-34+dy)*t/d, (47+dx)*s/d,(-34+dy)*t/d, (51+dx)*s/d,(-19+dy)*t/d
	dc.b	(23+dx)*s/d,(-14+dy)*t/d, (25+dx)*s/d,(-6+dy)*t/d, (37+dx)*s/d,(19+dy)*t/d, (45+dx)*s/d,(53+dy)*t/d
	dc.b	(40+dx)*s/d,(60+dy)*t/d, (23+dx)*s/d,(60+dy)*t/d, (14+dx)*s/d,(27+dy)*t/d, (5+dx)*s/d,(7+dy)*t/d
	dc.b	(-7+dx)*s/d,(28+dy)*t/d, (-16+dx)*s/d,(33+dy)*t/d, (-47+dx)*s/d,(24+dy)*t/d, (-47+dx)*s/d,(10+dy)*t/d
	dc.b	(-37+dx)*s/d,(6+dy)*t/d, (-22+dx)*s/d,(9+dy)*t/d, (-13+dx)*s/d,(-7+dy)*t/d, (-12+dx)*s/d,(-17+dy)*t/d
	dc.b	(-17+dx)*s/d,(-17+dy)*t/d, (-22+dx)*s/d,(-25+dy)*t/d, (-18+dx)*s/d,(-32+dy)*t/d, (-5+dx)*s/d,(-46+dy)*t/d
	dc.b	(11+dx)*s/d,(-61+dy)*t/d, (15+dx)*s/d,(-59+dy)*t/d
	dc.b	30-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 28*2,29*2, 0*2,29*2, 27*2,28*2

	; frame 2
	dc.b	29-1
	dc.b	(14+dx)*s/d,(-59+dy)*t/d, (20+dx)*s/d,(-71+dy)*t/d, (34+dx)*s/d,(-64+dy)*t/d, (28+dx)*s/d,(-52+dy)*t/d
	dc.b	(31+dx)*s/d,(-50+dy)*t/d, (25+dx)*s/d,(-40+dy)*t/d, (30+dx)*s/d,(-33+dy)*t/d, (47+dx)*s/d,(-34+dy)*t/d
	dc.b	(51+dx)*s/d,(-18+dy)*t/d, (23+dx)*s/d,(-13+dy)*t/d, (37+dx)*s/d,(20+dy)*t/d, (41+dx)*s/d,(53+dy)*t/d
	dc.b	(36+dx)*s/d,(61+dy)*t/d, (19+dx)*s/d,(59+dy)*t/d, (14+dx)*s/d,(28+dy)*t/d, (5+dx)*s/d,(8+dy)*t/d
	dc.b	(-5+dx)*s/d,(30+dy)*t/d, (-14+dx)*s/d,(35+dy)*t/d, (-46+dx)*s/d,(25+dy)*t/d, (-46+dx)*s/d,(11+dy)*t/d
	dc.b	(-35+dx)*s/d,(7+dy)*t/d, (-21+dx)*s/d,(10+dy)*t/d, (-13+dx)*s/d,(-7+dy)*t/d, (-12+dx)*s/d,(-16+dy)*t/d
	dc.b	(-16+dx)*s/d,(-17+dy)*t/d, (-22+dx)*s/d,(-24+dy)*t/d, (-18+dx)*s/d,(-31+dy)*t/d, (-5+dx)*s/d,(-45+dy)*t/d
	dc.b	(11+dx)*s/d,(-60+dy)*t/d
	dc.b	29-1
	dc.b	1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2, 8*2,9*2
	dc.b	9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2, 16*2,17*2
	dc.b	17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2, 24*2,25*2
	dc.b	25*2,26*2, 26*2,27*2, 0*2,28*2, 0*2,1*2, 27*2,28*2

	; frame 3
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-70+dy)*t/d, (34+dx)*s/d,(-63+dy)*t/d, (27+dx)*s/d,(-51+dy)*t/d, (31+dx)*s/d,(-49+dy)*t/d
	dc.b	(24+dx)*s/d,(-38+dy)*t/d, (29+dx)*s/d,(-32+dy)*t/d, (47+dx)*s/d,(-31+dy)*t/d, (50+dx)*s/d,(-17+dy)*t/d
	dc.b	(23+dx)*s/d,(-13+dy)*t/d, (36+dx)*s/d,(20+dy)*t/d, (36+dx)*s/d,(55+dy)*t/d, (31+dx)*s/d,(62+dy)*t/d
	dc.b	(15+dx)*s/d,(58+dy)*t/d, (14+dx)*s/d,(28+dy)*t/d, (6+dx)*s/d,(11+dy)*t/d, (-1+dx)*s/d,(33+dy)*t/d
	dc.b	(-10+dx)*s/d,(39+dy)*t/d, (-43+dx)*s/d,(27+dy)*t/d, (-42+dx)*s/d,(12+dy)*t/d, (-32+dx)*s/d,(9+dy)*t/d
	dc.b	(-19+dx)*s/d,(12+dy)*t/d, (-13+dx)*s/d,(-6+dy)*t/d, (-12+dx)*s/d,(-15+dy)*t/d, (-15+dx)*s/d,(-15+dy)*t/d
	dc.b	(-21+dx)*s/d,(-22+dy)*t/d, (-17+dx)*s/d,(-30+dy)*t/d, (-5+dx)*s/d,(-45+dy)*t/d, (11+dx)*s/d,(-60+dy)*t/d
	dc.b	(14+dx)*s/d,(-58+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 27*2,28*2, 0*2,28*2, 26*2,27*2
	
	; frame 4
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-68+dy)*t/d, (33+dx)*s/d,(-62+dy)*t/d, (27+dx)*s/d,(-50+dy)*t/d, (30+dx)*s/d,(-49+dy)*t/d
	dc.b	(24+dx)*s/d,(-37+dy)*t/d, (28+dx)*s/d,(-30+dy)*t/d, (47+dx)*s/d,(-29+dy)*t/d, (49+dx)*s/d,(-14+dy)*t/d
	dc.b	(23+dx)*s/d,(-11+dy)*t/d, (39+dx)*s/d,(21+dy)*t/d, (33+dx)*s/d,(53+dy)*t/d, (27+dx)*s/d,(61+dy)*t/d
	dc.b	(11+dx)*s/d,(55+dy)*t/d, (16+dx)*s/d,(27+dy)*t/d, (8+dx)*s/d,(14+dy)*t/d, (4+dx)*s/d,(37+dy)*t/d
	dc.b	(-3+dx)*s/d,(43+dy)*t/d, (-38+dx)*s/d,(32+dy)*t/d, (-38+dx)*s/d,(16+dy)*t/d, (-27+dx)*s/d,(12+dy)*t/d
	dc.b	(-16+dx)*s/d,(15+dy)*t/d, (-12+dx)*s/d,(-5+dy)*t/d, (-12+dx)*s/d,(-12+dy)*t/d, (-20+dx)*s/d,(-20+dy)*t/d
	dc.b	(-16+dx)*s/d,(-27+dy)*t/d, (-4+dx)*s/d,(-42+dy)*t/d, (-5+dx)*s/d,(-46+dy)*t/d, (10+dx)*s/d,(-58+dy)*t/d
	dc.b	(14+dx)*s/d,(-57+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 27*2,28*2, 0*2,28*2, 26*2,27*2

	; frame 5
	dc.b	31-1
	dc.b	(20+dx)*s/d,(-67+dy)*t/d, (33+dx)*s/d,(-61+dy)*t/d, (27+dx)*s/d,(-49+dy)*t/d, (30+dx)*s/d,(-47+dy)*t/d
	dc.b	(23+dx)*s/d,(-35+dy)*t/d, (26+dx)*s/d,(-28+dy)*t/d, (46+dx)*s/d,(-24+dy)*t/d, (47+dx)*s/d,(-10+dy)*t/d
	dc.b	(39+dx)*s/d,(-8+dy)*t/d, (23+dx)*s/d,(-9+dy)*t/d, (42+dx)*s/d,(21+dy)*t/d, (29+dx)*s/d,(52+dy)*t/d
	dc.b	(22+dx)*s/d,(60+dy)*t/d, (7+dx)*s/d,(51+dy)*t/d, (11+dx)*s/d,(42+dy)*t/d, (5+dx)*s/d,(46+dy)*t/d
	dc.b	(-33+dx)*s/d,(35+dy)*t/d, (-33+dx)*s/d,(19+dy)*t/d, (-22+dx)*s/d,(15+dy)*t/d, (-12+dx)*s/d,(17+dy)*t/d
	dc.b	(-13+dx)*s/d,(-5+dy)*t/d, (-12+dx)*s/d,(-11+dy)*t/d, (-17+dx)*s/d,(-16+dy)*t/d, (-14+dx)*s/d,(-24+dy)*t/d
	dc.b	(14+dx)*s/d,(-56+dy)*t/d, (13+dx)*s/d,(20+dy)*t/d, (16+dx)*s/d,(27+dy)*t/d, (13+dx)*s/d,(35+dy)*t/d
	dc.b	(-6+dx)*s/d,(-46+dy)*t/d, (-4+dx)*s/d,(-41+dy)*t/d, (10+dx)*s/d,(-57+dy)*t/d
	dc.b	31-1
	dc.b	25*2,26*2, 26*2,27*2, 25*2,27*2, 0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2
	dc.b	5*2,6*2, 6*2,7*2, 7*2,8*2, 8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2
	dc.b	13*2,14*2, 14*2,15*2, 15*2,16*2, 16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2
	dc.b	21*2,22*2, 22*2,23*2, 0*2,24*2, 23*2,29*2, 28*2,29*2, 28*2,30*2, 24*2,30*2

	; frame 6
	dc.b	26-1
	dc.b	(21+dx)*s/d,(-66+dy)*t/d, (33+dx)*s/d,(-60+dy)*t/d, (27+dx)*s/d,(-47+dy)*t/d, (31+dx)*s/d,(-46+dy)*t/d
	dc.b	(22+dx)*s/d,(-30+dy)*t/d, (25+dx)*s/d,(-26+dy)*t/d, (45+dx)*s/d,(-20+dy)*t/d, (45+dx)*s/d,(-5+dy)*t/d
	dc.b	(38+dx)*s/d,(-4+dy)*t/d, (24+dx)*s/d,(-5+dy)*t/d, (43+dx)*s/d,(22+dy)*t/d, (24+dx)*s/d,(52+dy)*t/d
	dc.b	(18+dx)*s/d,(58+dy)*t/d, (4+dx)*s/d,(47+dy)*t/d, (6+dx)*s/d,(45+dy)*t/d, (-26+dx)*s/d,(40+dy)*t/d
	dc.b	(-27+dx)*s/d,(23+dy)*t/d, (-17+dx)*s/d,(18+dy)*t/d, (-9+dx)*s/d,(19+dy)*t/d, (-13+dx)*s/d,(-3+dy)*t/d
	dc.b	(-12+dx)*s/d,(-12+dy)*t/d, (-11+dx)*s/d,(-22+dy)*t/d, (-4+dx)*s/d,(-35+dy)*t/d, (-6+dx)*s/d,(-45+dy)*t/d
	dc.b	(9+dx)*s/d,(-56+dy)*t/d, (14+dx)*s/d,(-54+dy)*t/d
	dc.b	26-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 24*2,25*2
	dc.b	0*2,25*2, 23*2,24*2
	
	; frame 7
	dc.b	23-1
	dc.b	(21+dx)*s/d,(-65+dy)*t/d, (34+dx)*s/d,(-58+dy)*t/d, (28+dx)*s/d,(-47+dy)*t/d, (31+dx)*s/d,(-45+dy)*t/d
	dc.b	(21+dx)*s/d,(-25+dy)*t/d, (43+dx)*s/d,(-14+dy)*t/d, (40+dx)*s/d,(0+dy)*t/d, (34+dx)*s/d,(1+dy)*t/d
	dc.b	(26+dx)*s/d,(-2+dy)*t/d, (44+dx)*s/d,(23+dy)*t/d, (14+dx)*s/d,(58+dy)*t/d, (1+dx)*s/d,(45+dy)*t/d
	dc.b	(-19+dx)*s/d,(44+dy)*t/d, (-22+dx)*s/d,(27+dy)*t/d, (-12+dx)*s/d,(22+dy)*t/d, (-4+dx)*s/d,(21+dy)*t/d
	dc.b	(-12+dx)*s/d,(4+dy)*t/d, (-12+dx)*s/d,(-2+dy)*t/d, (-8+dx)*s/d,(-24+dy)*t/d, (-4+dx)*s/d,(-32+dy)*t/d
	dc.b	(-6+dx)*s/d,(-46+dy)*t/d, (14+dx)*s/d,(-53+dy)*t/d, (8+dx)*s/d,(-55+dy)*t/d
	dc.b	23-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,22*2, 21*2,22*2, 0*2,21*2

	; frame 8
	dc.b	27-1
	dc.b	(21+dx)*s/d,(-64+dy)*t/d, (34+dx)*s/d,(-57+dy)*t/d, (28+dx)*s/d,(-47+dy)*t/d, (32+dx)*s/d,(-44+dy)*t/d
	dc.b	(22+dx)*s/d,(-25+dy)*t/d, (21+dx)*s/d,(-19+dy)*t/d, (41+dx)*s/d,(-8+dy)*t/d, (35+dx)*s/d,(6+dy)*t/d
	dc.b	(30+dx)*s/d,(6+dy)*t/d, (43+dx)*s/d,(25+dy)*t/d, (35+dx)*s/d,(33+dy)*t/d, (37+dx)*s/d,(36+dy)*t/d
	dc.b	(30+dx)*s/d,(43+dy)*t/d, (23+dx)*s/d,(43+dy)*t/d, (10+dx)*s/d,(56+dy)*t/d, (1+dx)*s/d,(46+dy)*t/d
	dc.b	(-12+dx)*s/d,(48+dy)*t/d, (-17+dx)*s/d,(32+dy)*t/d, (-8+dx)*s/d,(27+dy)*t/d, (0+dx)*s/d,(25+dy)*t/d
	dc.b	(-10+dx)*s/d,(7+dy)*t/d, (-12+dx)*s/d,(-1+dy)*t/d, (-10+dx)*s/d,(-17+dy)*t/d, (-5+dx)*s/d,(-29+dy)*t/d
	dc.b	(15+dx)*s/d,(-53+dy)*t/d, (8+dx)*s/d,(-54+dy)*t/d, (-6+dx)*s/d,(-46+dy)*t/d
	dc.b	27-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 0*2,24*2
	dc.b	23*2,26*2, 25*2,26*2, 24*2,25*2
	
	; frame 9
	dc.b	26-1
	dc.b	(21+dx)*s/d,(-63+dy)*t/d, (34+dx)*s/d,(-56+dy)*t/d, (28+dx)*s/d,(-45+dy)*t/d, (33+dx)*s/d,(-43+dy)*t/d
	dc.b	(23+dx)*s/d,(-25+dy)*t/d, (21+dx)*s/d,(-13+dy)*t/d, (36+dx)*s/d,(-1+dy)*t/d, (30+dx)*s/d,(9+dy)*t/d
	dc.b	(42+dx)*s/d,(28+dy)*t/d, (40+dx)*s/d,(30+dy)*t/d, (42+dx)*s/d,(33+dy)*t/d, (36+dx)*s/d,(38+dy)*t/d
	dc.b	(23+dx)*s/d,(43+dy)*t/d, (6+dx)*s/d,(55+dy)*t/d, (1+dx)*s/d,(49+dy)*t/d, (-5+dx)*s/d,(52+dy)*t/d
	dc.b	(-12+dx)*s/d,(35+dy)*t/d, (-5+dx)*s/d,(30+dy)*t/d, (4+dx)*s/d,(26+dy)*t/d, (-9+dx)*s/d,(8+dy)*t/d
	dc.b	(-12+dx)*s/d,(0+dy)*t/d, (-11+dx)*s/d,(-13+dy)*t/d, (-7+dx)*s/d,(-26+dy)*t/d, (-6+dx)*s/d,(-47+dy)*t/d
	dc.b	(15+dx)*s/d,(-53+dy)*t/d, (7+dx)*s/d,(-54+dy)*t/d
	dc.b	26-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 0*2,24*2
	dc.b	23*2,25*2, 24*2,25*2

	; frame 10
	dc.b	21-1
	dc.b	(21+dx)*s/d,(-63+dy)*t/d, (34+dx)*s/d,(-56+dy)*t/d, (28+dx)*s/d,(-46+dy)*t/d, (33+dx)*s/d,(-43+dy)*t/d
	dc.b	(23+dx)*s/d,(-19+dy)*t/d, (22+dx)*s/d,(-3+dy)*t/d, (29+dx)*s/d,(4+dy)*t/d, (27+dx)*s/d,(6+dy)*t/d
	dc.b	(29+dx)*s/d,(12+dy)*t/d, (44+dx)*s/d,(27+dy)*t/d, (39+dx)*s/d,(35+dy)*t/d, (0+dx)*s/d,(54+dy)*t/d
	dc.b	(-8+dx)*s/d,(39+dy)*t/d, (8+dx)*s/d,(27+dy)*t/d, (-9+dx)*s/d,(9+dy)*t/d, (-12+dx)*s/d,(0+dy)*t/d
	dc.b	(-10+dx)*s/d,(-12+dy)*t/d, (-13+dx)*s/d,(-16+dy)*t/d, (-5+dx)*s/d,(-48+dy)*t/d, (14+dx)*s/d,(-52+dy)*t/d
	dc.b	(6+dx)*s/d,(-54+dy)*t/d
	dc.b	21-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,20*2, 19*2,20*2, 0*2,19*2
	
	; frame 11
	dc.b	22-1
	dc.b	(21+dx)*s/d,(-63+dy)*t/d, (34+dx)*s/d,(-56+dy)*t/d, (28+dx)*s/d,(-47+dy)*t/d, (34+dx)*s/d,(-43+dy)*t/d
	dc.b	(26+dx)*s/d,(-27+dy)*t/d, (29+dx)*s/d,(1+dy)*t/d, (24+dx)*s/d,(2+dy)*t/d, (47+dx)*s/d,(25+dy)*t/d
	dc.b	(41+dx)*s/d,(30+dy)*t/d, (5+dx)*s/d,(55+dy)*t/d, (0+dx)*s/d,(51+dy)*t/d, (-2+dx)*s/d,(52+dy)*t/d
	dc.b	(-9+dx)*s/d,(39+dy)*t/d, (7+dx)*s/d,(27+dy)*t/d, (7+dx)*s/d,(23+dy)*t/d, (-8+dx)*s/d,(9+dy)*t/d
	dc.b	(-11+dx)*s/d,(0+dy)*t/d, (-10+dx)*s/d,(-10+dy)*t/d, (-15+dx)*s/d,(-19+dy)*t/d, (-4+dx)*s/d,(-50+dy)*t/d
	dc.b	(14+dx)*s/d,(-53+dy)*t/d, (6+dx)*s/d,(-54+dy)*t/d
	dc.b	22-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 0*2,20*2, 19*2,21*2, 20*2,21*2
	
	; frame 12
	dc.b	21-1
	dc.b	(21+dx)*s/d,(-65+dy)*t/d, (34+dx)*s/d,(-59+dy)*t/d, (29+dx)*s/d,(-48+dy)*t/d, (35+dx)*s/d,(-44+dy)*t/d
	dc.b	(28+dx)*s/d,(-32+dy)*t/d, (28+dx)*s/d,(-24+dy)*t/d, (35+dx)*s/d,(-1+dy)*t/d, (30+dx)*s/d,(2+dy)*t/d
	dc.b	(25+dx)*s/d,(3+dy)*t/d, (46+dx)*s/d,(21+dy)*t/d, (9+dx)*s/d,(56+dy)*t/d, (1+dx)*s/d,(48+dy)*t/d
	dc.b	(-6+dx)*s/d,(51+dy)*t/d, (-13+dx)*s/d,(37+dy)*t/d, (4+dx)*s/d,(27+dy)*t/d, (3+dx)*s/d,(18+dy)*t/d
	dc.b	(-2+dx)*s/d,(13+dy)*t/d, (-5+dx)*s/d,(14+dy)*t/d, (-18+dx)*s/d,(-24+dy)*t/d, (-1+dx)*s/d,(-56+dy)*t/d
	dc.b	(14+dx)*s/d,(-55+dy)*t/d
	dc.b	21-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 0*2,20*2, 19*2,20*2

	; frame 13
	dc.b	24-1
	dc.b	(21+dx)*s/d,(-68+dy)*t/d, (34+dx)*s/d,(-61+dy)*t/d, (28+dx)*s/d,(-50+dy)*t/d, (35+dx)*s/d,(-47+dy)*t/d
	dc.b	(31+dx)*s/d,(-36+dy)*t/d, (30+dx)*s/d,(-26+dy)*t/d, (39+dx)*s/d,(-5+dy)*t/d, (36+dx)*s/d,(0+dy)*t/d
	dc.b	(27+dx)*s/d,(1+dy)*t/d, (47+dx)*s/d,(17+dy)*t/d, (14+dx)*s/d,(57+dy)*t/d, (-1+dx)*s/d,(45+dy)*t/d
	dc.b	(-9+dx)*s/d,(50+dy)*t/d, (-18+dx)*s/d,(37+dy)*t/d, (0+dx)*s/d,(24+dy)*t/d, (0+dx)*s/d,(13+dy)*t/d
	dc.b	(-5+dx)*s/d,(8+dy)*t/d, (-15+dx)*s/d,(9+dy)*t/d, (-20+dx)*s/d,(-30+dy)*t/d, (0+dx)*s/d,(-59+dy)*t/d
	dc.b	(14+dx)*s/d,(-57+dy)*t/d, (24+dx)*s/d,(-7+dy)*t/d, (20+dx)*s/d,(-5+dy)*t/d, (25+dx)*s/d,(0+dy)*t/d
	dc.b	24-1
	dc.b	21*2,22*2, 21*2,23*2, 22*2,23*2, 0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2
	dc.b	5*2,6*2, 6*2,7*2, 7*2,8*2, 8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2
	dc.b	13*2,14*2, 14*2,15*2, 15*2,16*2, 16*2,17*2, 17*2,18*2, 18*2,19*2, 0*2,20*2, 19*2,20*2
	
	; frame 14
	dc.b	26-1
	dc.b	(21+dx)*s/d,(-71+dy)*t/d, (34+dx)*s/d,(-64+dy)*t/d, (29+dx)*s/d,(-54+dy)*t/d, (36+dx)*s/d,(-49+dy)*t/d
	dc.b	(32+dx)*s/d,(-40+dy)*t/d, (33+dx)*s/d,(-28+dy)*t/d, (43+dx)*s/d,(-11+dy)*t/d, (40+dx)*s/d,(-2+dy)*t/d
	dc.b	(31+dx)*s/d,(-1+dy)*t/d, (21+dx)*s/d,(-16+dy)*t/d, (20+dx)*s/d,(-6+dy)*t/d, (48+dx)*s/d,(12+dy)*t/d
	dc.b	(28+dx)*s/d,(49+dy)*t/d, (20+dx)*s/d,(56+dy)*t/d, (5+dx)*s/d,(46+dy)*t/d, (13+dx)*s/d,(28+dy)*t/d
	dc.b	(-13+dx)*s/d,(49+dy)*t/d, (-22+dx)*s/d,(37+dy)*t/d, (-5+dx)*s/d,(21+dy)*t/d, (-4+dx)*s/d,(5+dy)*t/d
	dc.b	(-7+dx)*s/d,(-6+dy)*t/d, (-10+dx)*s/d,(4+dy)*t/d, (-24+dx)*s/d,(3+dy)*t/d, (-22+dx)*s/d,(-34+dy)*t/d
	dc.b	(0+dx)*s/d,(-62+dy)*t/d, (14+dx)*s/d,(-60+dy)*t/d
	dc.b	26-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 0*2,25*2
	
	; frame 15
	dc.b	30-1
	dc.b	(20+dx)*s/d,(-73+dy)*t/d, (34+dx)*s/d,(-67+dy)*t/d, (28+dx)*s/d,(-56+dy)*t/d, (36+dx)*s/d,(-52+dy)*t/d
	dc.b	(33+dx)*s/d,(-43+dy)*t/d, (35+dx)*s/d,(-31+dy)*t/d, (44+dx)*s/d,(-18+dy)*t/d, (43+dx)*s/d,(-7+dy)*t/d
	dc.b	(34+dx)*s/d,(-5+dy)*t/d, (21+dx)*s/d,(-20+dy)*t/d, (19+dx)*s/d,(-11+dy)*t/d, (47+dx)*s/d,(6+dy)*t/d
	dc.b	(37+dx)*s/d,(47+dy)*t/d, (30+dx)*s/d,(55+dy)*t/d, (13+dx)*s/d,(47+dy)*t/d, (19+dx)*s/d,(21+dy)*t/d
	dc.b	(9+dx)*s/d,(12+dy)*t/d, (6+dx)*s/d,(21+dy)*t/d, (-11+dx)*s/d,(38+dy)*t/d, (-20+dx)*s/d,(43+dy)*t/d
	dc.b	(-29+dx)*s/d,(32+dy)*t/d, (-13+dx)*s/d,(14+dy)*t/d, (-5+dx)*s/d,(-7+dy)*t/d, (-11+dx)*s/d,(-20+dy)*t/d
	dc.b	(-14+dx)*s/d,(-3+dy)*t/d, (-18+dx)*s/d,(0+dy)*t/d, (-31+dx)*s/d,(-4+dy)*t/d, (-24+dx)*s/d,(-39+dy)*t/d
	dc.b	(0+dx)*s/d,(-64+dy)*t/d, (14+dx)*s/d,(-62+dy)*t/d
	dc.b	30-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 28*2,29*2, 0*2,29*2
	
	; frame 16
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-76+dy)*t/d, (34+dx)*s/d,(-69+dy)*t/d, (29+dx)*s/d,(-59+dy)*t/d, (37+dx)*s/d,(-54+dy)*t/d
	dc.b	(34+dx)*s/d,(-46+dy)*t/d, (36+dx)*s/d,(-35+dy)*t/d, (45+dx)*s/d,(-25+dy)*t/d, (46+dx)*s/d,(-12+dy)*t/d
	dc.b	(35+dx)*s/d,(-10+dy)*t/d, (23+dx)*s/d,(-25+dy)*t/d, (19+dx)*s/d,(-13+dy)*t/d, (46+dx)*s/d,(3+dy)*t/d
	dc.b	(46+dx)*s/d,(46+dy)*t/d, (40+dx)*s/d,(53+dy)*t/d, (21+dx)*s/d,(49+dy)*t/d, (21+dx)*s/d,(19+dy)*t/d
	dc.b	(7+dx)*s/d,(8+dy)*t/d, (2+dx)*s/d,(17+dy)*t/d, (-27+dx)*s/d,(36+dy)*t/d, (-34+dx)*s/d,(24+dy)*t/d
	dc.b	(-16+dx)*s/d,(8+dy)*t/d, (-6+dx)*s/d,(-9+dy)*t/d, (-10+dx)*s/d,(-32+dy)*t/d, (-19+dx)*s/d,(-8+dy)*t/d
	dc.b	(-24+dx)*s/d,(-6+dy)*t/d, (-37+dx)*s/d,(-11+dy)*t/d, (-25+dx)*s/d,(-44+dy)*t/d, (0+dx)*s/d,(-68+dy)*t/d
	dc.b	(14+dx)*s/d,(-65+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2

	; frame 17
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-78+dy)*t/d, (34+dx)*s/d,(-71+dy)*t/d, (28+dx)*s/d,(-61+dy)*t/d, (38+dx)*s/d,(-55+dy)*t/d
	dc.b	(34+dx)*s/d,(-48+dy)*t/d, (37+dx)*s/d,(-37+dy)*t/d, (45+dx)*s/d,(-30+dy)*t/d, (47+dx)*s/d,(-17+dy)*t/d
	dc.b	(35+dx)*s/d,(-14+dy)*t/d, (24+dx)*s/d,(-28+dy)*t/d, (19+dx)*s/d,(-15+dy)*t/d, (45+dx)*s/d,(3+dy)*t/d
	dc.b	(52+dx)*s/d,(45+dy)*t/d, (47+dx)*s/d,(53+dy)*t/d, (27+dx)*s/d,(51+dy)*t/d, (21+dx)*s/d,(19+dy)*t/d
	dc.b	(5+dx)*s/d,(6+dy)*t/d, (-2+dx)*s/d,(12+dy)*t/d, (-33+dx)*s/d,(28+dy)*t/d, (-39+dx)*s/d,(15+dy)*t/d
	dc.b	(-19+dx)*s/d,(4+dy)*t/d, (-7+dx)*s/d,(-11+dy)*t/d, (-9+dx)*s/d,(-39+dy)*t/d, (-22+dx)*s/d,(-12+dy)*t/d
	dc.b	(-28+dx)*s/d,(-10+dy)*t/d, (-40+dx)*s/d,(-18+dy)*t/d, (-26+dx)*s/d,(-48+dy)*t/d, (0+dx)*s/d,(-69+dy)*t/d
	dc.b	(13+dx)*s/d,(-66+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2

	; frame 18
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-78+dy)*t/d, (34+dx)*s/d,(-71+dy)*t/d, (28+dx)*s/d,(-61+dy)*t/d, (38+dx)*s/d,(-56+dy)*t/d
	dc.b	(35+dx)*s/d,(-49+dy)*t/d, (38+dx)*s/d,(-39+dy)*t/d, (45+dx)*s/d,(-35+dy)*t/d, (48+dx)*s/d,(-20+dy)*t/d
	dc.b	(36+dx)*s/d,(-18+dy)*t/d, (25+dx)*s/d,(-31+dy)*t/d, (18+dx)*s/d,(-15+dy)*t/d, (44+dx)*s/d,(4+dy)*t/d
	dc.b	(55+dx)*s/d,(45+dy)*t/d, (49+dx)*s/d,(53+dy)*t/d, (30+dx)*s/d,(54+dy)*t/d, (21+dx)*s/d,(20+dy)*t/d
	dc.b	(4+dx)*s/d,(5+dy)*t/d, (-1+dx)*s/d,(14+dy)*t/d, (-36+dx)*s/d,(23+dy)*t/d, (-40+dx)*s/d,(10+dy)*t/d
	dc.b	(-18+dx)*s/d,(3+dy)*t/d, (-7+dx)*s/d,(-12+dy)*t/d, (-9+dx)*s/d,(-41+dy)*t/d, (-25+dx)*s/d,(-16+dy)*t/d
	dc.b	(-32+dx)*s/d,(-14+dy)*t/d, (-42+dx)*s/d,(-22+dy)*t/d, (-26+dx)*s/d,(-51+dy)*t/d, (-1+dx)*s/d,(-70+dy)*t/d
	dc.b	(13+dx)*s/d,(-66+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2

	; frame 19
	dc.b	31-1
	dc.b	(19+dx)*s/d,(-78+dy)*t/d, (33+dx)*s/d,(-71+dy)*t/d, (28+dx)*s/d,(-61+dy)*t/d, (38+dx)*s/d,(-55+dy)*t/d
	dc.b	(35+dx)*s/d,(-48+dy)*t/d, (39+dx)*s/d,(-40+dy)*t/d, (44+dx)*s/d,(-37+dy)*t/d, (48+dx)*s/d,(-23+dy)*t/d
	dc.b	(34+dx)*s/d,(-18+dy)*t/d, (25+dx)*s/d,(-31+dy)*t/d, (18+dx)*s/d,(-13+dy)*t/d, (41+dx)*s/d,(8+dy)*t/d
	dc.b	(53+dx)*s/d,(49+dy)*t/d, (48+dx)*s/d,(56+dy)*t/d, (28+dx)*s/d,(57+dy)*t/d, (18+dx)*s/d,(23+dy)*t/d
	dc.b	(3+dx)*s/d,(8+dy)*t/d, (-1+dx)*s/d,(15+dy)*t/d, (-9+dx)*s/d,(19+dy)*t/d, (-36+dx)*s/d,(20+dy)*t/d
	dc.b	(-39+dx)*s/d,(7+dy)*t/d, (-30+dx)*s/d,(4+dy)*t/d, (-16+dx)*s/d,(3+dy)*t/d, (-6+dx)*s/d,(-11+dy)*t/d
	dc.b	(-9+dx)*s/d,(-41+dy)*t/d, (-26+dx)*s/d,(-17+dy)*t/d, (-35+dx)*s/d,(-15+dy)*t/d, (-43+dx)*s/d,(-25+dy)*t/d
	dc.b	(-27+dx)*s/d,(-51+dy)*t/d, (0+dx)*s/d,(-69+dy)*t/d, (13+dx)*s/d,(-66+dy)*t/d
	dc.b	31-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 28*2,29*2, 29*2,30*2, 0*2,30*2

	; frame 20
	dc.b	31-1
	dc.b	(19+dx)*s/d,(-76+dy)*t/d, (33+dx)*s/d,(-69+dy)*t/d, (29+dx)*s/d,(-59+dy)*t/d, (38+dx)*s/d,(-54+dy)*t/d
	dc.b	(36+dx)*s/d,(-47+dy)*t/d, (39+dx)*s/d,(-39+dy)*t/d, (43+dx)*s/d,(-38+dy)*t/d, (48+dx)*s/d,(-23+dy)*t/d
	dc.b	(33+dx)*s/d,(-18+dy)*t/d, (26+dx)*s/d,(-30+dy)*t/d, (17+dx)*s/d,(-11+dy)*t/d, (35+dx)*s/d,(15+dy)*t/d
	dc.b	(48+dx)*s/d,(53+dy)*t/d, (43+dx)*s/d,(61+dy)*t/d, (24+dx)*s/d,(63+dy)*t/d, (13+dx)*s/d,(28+dy)*t/d
	dc.b	(2+dx)*s/d,(13+dy)*t/d, (0+dx)*s/d,(17+dy)*t/d, (-8+dx)*s/d,(21+dy)*t/d, (-36+dx)*s/d,(19+dy)*t/d
	dc.b	(-38+dx)*s/d,(6+dy)*t/d, (-29+dx)*s/d,(3+dy)*t/d, (-15+dx)*s/d,(3+dy)*t/d, (-7+dx)*s/d,(-10+dy)*t/d
	dc.b	(-9+dx)*s/d,(-40+dy)*t/d, (-27+dx)*s/d,(-18+dy)*t/d, (-35+dx)*s/d,(-17+dy)*t/d, (-43+dx)*s/d,(-25+dy)*t/d
	dc.b	(-26+dx)*s/d,(-50+dy)*t/d, (-1+dx)*s/d,(-67+dy)*t/d, (13+dx)*s/d,(-64+dy)*t/d
	dc.b	31-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 28*2,29*2, 29*2,30*2, 0*2,30*2

	; frame 21
	dc.b	28-1
	dc.b	(19+dx)*s/d,(-74+dy)*t/d, (33+dx)*s/d,(-67+dy)*t/d, (28+dx)*s/d,(-57+dy)*t/d, (37+dx)*s/d,(-52+dy)*t/d
	dc.b	(35+dx)*s/d,(-45+dy)*t/d, (39+dx)*s/d,(-36+dy)*t/d, (43+dx)*s/d,(-37+dy)*t/d, (47+dx)*s/d,(-22+dy)*t/d
	dc.b	(33+dx)*s/d,(-17+dy)*t/d, (27+dx)*s/d,(-30+dy)*t/d, (17+dx)*s/d,(-8+dy)*t/d, (44+dx)*s/d,(56+dy)*t/d
	dc.b	(38+dx)*s/d,(65+dy)*t/d, (19+dx)*s/d,(67+dy)*t/d, (1+dx)*s/d,(19+dy)*t/d, (-8+dx)*s/d,(24+dy)*t/d
	dc.b	(-37+dx)*s/d,(19+dy)*t/d, (-37+dx)*s/d,(6+dy)*t/d, (-28+dx)*s/d,(3+dy)*t/d, (-14+dx)*s/d,(4+dy)*t/d
	dc.b	(-7+dx)*s/d,(-8+dy)*t/d, (-9+dx)*s/d,(-39+dy)*t/d, (-27+dx)*s/d,(-17+dy)*t/d, (-35+dx)*s/d,(-16+dy)*t/d
	dc.b	(-44+dx)*s/d,(-25+dy)*t/d, (-27+dx)*s/d,(-48+dy)*t/d, (13+dx)*s/d,(-62+dy)*t/d, (-1+dx)*s/d,(-65+dy)*t/d
	dc.b	28-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 0*2,26*2, 25*2,27*2, 26*2,27*2

	; frame 22
	dc.b	29-1
	dc.b	(19+dx)*s/d,(-71+dy)*t/d, (33+dx)*s/d,(-65+dy)*t/d, (29+dx)*s/d,(-55+dy)*t/d, (38+dx)*s/d,(-49+dy)*t/d
	dc.b	(36+dx)*s/d,(-43+dy)*t/d, (39+dx)*s/d,(-34+dy)*t/d, (42+dx)*s/d,(-35+dy)*t/d, (47+dx)*s/d,(-20+dy)*t/d
	dc.b	(33+dx)*s/d,(-15+dy)*t/d, (27+dx)*s/d,(-28+dy)*t/d, (17+dx)*s/d,(-6+dy)*t/d, (30+dx)*s/d,(23+dy)*t/d
	dc.b	(40+dx)*s/d,(60+dy)*t/d, (34+dx)*s/d,(68+dy)*t/d, (16+dx)*s/d,(69+dy)*t/d, (1+dx)*s/d,(23+dy)*t/d
	dc.b	(-7+dx)*s/d,(27+dy)*t/d, (-37+dx)*s/d,(20+dy)*t/d, (-36+dx)*s/d,(6+dy)*t/d, (-27+dx)*s/d,(2+dy)*t/d
	dc.b	(-12+dx)*s/d,(6+dy)*t/d, (-7+dx)*s/d,(-5+dy)*t/d, (-9+dx)*s/d,(-36+dy)*t/d, (-27+dx)*s/d,(-15+dy)*t/d
	dc.b	(-36+dx)*s/d,(-14+dy)*t/d, (-43+dx)*s/d,(-23+dy)*t/d, (-26+dx)*s/d,(-46+dy)*t/d, (-1+dx)*s/d,(-62+dy)*t/d
	dc.b	(13+dx)*s/d,(-60+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2
	
	; frame 23
	dc.b	30-1
	dc.b	(19+dx)*s/d,(-69+dy)*t/d, (33+dx)*s/d,(-63+dy)*t/d, (28+dx)*s/d,(-53+dy)*t/d, (38+dx)*s/d,(-47+dy)*t/d
	dc.b	(36+dx)*s/d,(-41+dy)*t/d, (39+dx)*s/d,(-32+dy)*t/d, (42+dx)*s/d,(-33+dy)*t/d, (47+dx)*s/d,(-18+dy)*t/d
	dc.b	(33+dx)*s/d,(-12+dy)*t/d, (26+dx)*s/d,(-25+dy)*t/d, (17+dx)*s/d,(-5+dy)*t/d, (32+dx)*s/d,(24+dy)*t/d
	dc.b	(35+dx)*s/d,(62+dy)*t/d, (29+dx)*s/d,(70+dy)*t/d, (11+dx)*s/d,(67+dy)*t/d, (8+dx)*s/d,(34+dy)*t/d
	dc.b	(3+dx)*s/d,(25+dy)*t/d, (-5+dx)*s/d,(31+dy)*t/d, (-35+dx)*s/d,(21+dy)*t/d, (-34+dx)*s/d,(8+dy)*t/d
	dc.b	(-25+dx)*s/d,(3+dy)*t/d, (-11+dx)*s/d,(7+dy)*t/d, (-7+dx)*s/d,(-4+dy)*t/d, (-9+dx)*s/d,(-35+dy)*t/d
	dc.b	(-27+dx)*s/d,(-13+dy)*t/d, (-35+dx)*s/d,(-12+dy)*t/d, (-43+dx)*s/d,(-21+dy)*t/d, (-26+dx)*s/d,(-45+dy)*t/d
	dc.b	(-1+dx)*s/d,(-60+dy)*t/d, (13+dx)*s/d,(-58+dy)*t/d
	dc.b	30-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 28*2,29*2, 0*2,29*2
	
	; frame 24
	dc.b	29-1
	dc.b	(19+dx)*s/d,(-67+dy)*t/d, (32+dx)*s/d,(-61+dy)*t/d, (28+dx)*s/d,(-52+dy)*t/d, (37+dx)*s/d,(-46+dy)*t/d
	dc.b	(36+dx)*s/d,(-39+dy)*t/d, (38+dx)*s/d,(-31+dy)*t/d, (43+dx)*s/d,(-29+dy)*t/d, (47+dx)*s/d,(-15+dy)*t/d
	dc.b	(33+dx)*s/d,(-10+dy)*t/d, (26+dx)*s/d,(-24+dy)*t/d, (18+dx)*s/d,(-3+dy)*t/d, (33+dx)*s/d,(26+dy)*t/d
	dc.b	(31+dx)*s/d,(64+dy)*t/d, (24+dx)*s/d,(72+dy)*t/d, (6+dx)*s/d,(66+dy)*t/d, (8+dx)*s/d,(35+dy)*t/d
	dc.b	(6+dx)*s/d,(32+dy)*t/d, (0+dx)*s/d,(36+dy)*t/d, (-32+dx)*s/d,(23+dy)*t/d, (-30+dx)*s/d,(10+dy)*t/d
	dc.b	(-21+dx)*s/d,(6+dy)*t/d, (-9+dx)*s/d,(10+dy)*t/d, (-9+dx)*s/d,(-33+dy)*t/d, (-27+dx)*s/d,(-10+dy)*t/d
	dc.b	(-35+dx)*s/d,(-9+dy)*t/d, (-43+dx)*s/d,(-19+dy)*t/d, (-26+dx)*s/d,(-42+dy)*t/d, (-1+dx)*s/d,(-58+dy)*t/d
	dc.b	(13+dx)*s/d,(-56+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2

	; frame 25
	dc.b	30-1
	dc.b	(19+dx)*s/d,(-66+dy)*t/d, (33+dx)*s/d,(-60+dy)*t/d, (29+dx)*s/d,(-50+dy)*t/d, (37+dx)*s/d,(-44+dy)*t/d
	dc.b	(35+dx)*s/d,(-38+dy)*t/d, (39+dx)*s/d,(-29+dy)*t/d, (43+dx)*s/d,(-25+dy)*t/d, (48+dx)*s/d,(-11+dy)*t/d
	dc.b	(33+dx)*s/d,(-7+dy)*t/d, (25+dx)*s/d,(-21+dy)*t/d, (18+dx)*s/d,(-4+dy)*t/d, (19+dx)*s/d,(0+dy)*t/d
	dc.b	(38+dx)*s/d,(26+dy)*t/d, (27+dx)*s/d,(61+dy)*t/d, (20+dx)*s/d,(70+dy)*t/d, (3+dx)*s/d,(61+dy)*t/d
	dc.b	(9+dx)*s/d,(40+dy)*t/d, (-26+dx)*s/d,(27+dy)*t/d, (-24+dx)*s/d,(13+dy)*t/d, (-16+dx)*s/d,(9+dy)*t/d
	dc.b	(-4+dx)*s/d,(12+dy)*t/d, (-9+dx)*s/d,(-2+dy)*t/d, (-9+dx)*s/d,(-30+dy)*t/d, (-12+dx)*s/d,(-27+dy)*t/d
	dc.b	(-26+dx)*s/d,(-8+dy)*t/d, (-34+dx)*s/d,(-6+dy)*t/d, (-43+dx)*s/d,(-15+dy)*t/d, (-27+dx)*s/d,(-40+dy)*t/d
	dc.b	(-1+dx)*s/d,(-57+dy)*t/d, (13+dx)*s/d,(-55+dy)*t/d
	dc.b	30-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 28*2,29*2, 0*2,29*2
	
	; frame 26
	dc.b	29-1
	dc.b	(19+dx)*s/d,(-65+dy)*t/d, (33+dx)*s/d,(-58+dy)*t/d, (29+dx)*s/d,(-49+dy)*t/d, (37+dx)*s/d,(-44+dy)*t/d
	dc.b	(35+dx)*s/d,(-37+dy)*t/d, (38+dx)*s/d,(-28+dy)*t/d, (44+dx)*s/d,(-21+dy)*t/d, (48+dx)*s/d,(-8+dy)*t/d
	dc.b	(32+dx)*s/d,(-4+dy)*t/d, (25+dx)*s/d,(-20+dy)*t/d, (19+dx)*s/d,(-6+dy)*t/d, (20+dx)*s/d,(3+dy)*t/d
	dc.b	(40+dx)*s/d,(27+dy)*t/d, (23+dx)*s/d,(60+dy)*t/d, (15+dx)*s/d,(68+dy)*t/d, (0+dx)*s/d,(57+dy)*t/d
	dc.b	(8+dx)*s/d,(39+dy)*t/d, (-19+dx)*s/d,(32+dy)*t/d, (-18+dx)*s/d,(16+dy)*t/d, (-10+dx)*s/d,(13+dy)*t/d
	dc.b	(-3+dx)*s/d,(14+dy)*t/d, (-9+dx)*s/d,(-1+dy)*t/d, (-9+dx)*s/d,(-29+dy)*t/d, (-25+dx)*s/d,(-5+dy)*t/d
	dc.b	(-32+dx)*s/d,(-3+dy)*t/d, (-43+dx)*s/d,(-11+dy)*t/d, (-27+dx)*s/d,(-38+dy)*t/d, (-1+dx)*s/d,(-56+dy)*t/d
	dc.b	(14+dx)*s/d,(-54+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 27*2,28*2, 0*2,28*2

	; frame 27
	dc.b	28-1
	dc.b	(20+dx)*s/d,(-65+dy)*t/d, (33+dx)*s/d,(-57+dy)*t/d, (29+dx)*s/d,(-48+dy)*t/d, (37+dx)*s/d,(-43+dy)*t/d
	dc.b	(34+dx)*s/d,(-36+dy)*t/d, (37+dx)*s/d,(-26+dy)*t/d, (45+dx)*s/d,(-18+dy)*t/d, (47+dx)*s/d,(-4+dy)*t/d
	dc.b	(33+dx)*s/d,(-1+dy)*t/d, (23+dx)*s/d,(-18+dy)*t/d, (18+dx)*s/d,(-5+dy)*t/d, (24+dx)*s/d,(9+dy)*t/d
	dc.b	(40+dx)*s/d,(28+dy)*t/d, (11+dx)*s/d,(67+dy)*t/d, (-3+dx)*s/d,(55+dy)*t/d, (7+dx)*s/d,(38+dy)*t/d
	dc.b	(-11+dx)*s/d,(36+dy)*t/d, (-13+dx)*s/d,(21+dy)*t/d, (-5+dx)*s/d,(17+dy)*t/d, (-1+dx)*s/d,(17+dy)*t/d
	dc.b	(-8+dx)*s/d,(8+dy)*t/d, (-9+dx)*s/d,(-26+dy)*t/d, (-23+dx)*s/d,(-1+dy)*t/d, (-30+dx)*s/d,(1+dy)*t/d
	dc.b	(-41+dx)*s/d,(-7+dy)*t/d, (-26+dx)*s/d,(-35+dy)*t/d, (0+dx)*s/d,(-55+dy)*t/d, (14+dx)*s/d,(-53+dy)*t/d
	dc.b	28-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 0*2,27*2
	
	; frame 28
	dc.b	27-1
	dc.b	(20+dx)*s/d,(-64+dy)*t/d, (34+dx)*s/d,(-57+dy)*t/d, (29+dx)*s/d,(-47+dy)*t/d, (36+dx)*s/d,(-43+dy)*t/d
	dc.b	(33+dx)*s/d,(-35+dy)*t/d, (36+dx)*s/d,(-25+dy)*t/d, (45+dx)*s/d,(-13+dy)*t/d, (46+dx)*s/d,(-1+dy)*t/d
	dc.b	(33+dx)*s/d,(1+dy)*t/d, (22+dx)*s/d,(-15+dy)*t/d, (19+dx)*s/d,(-5+dy)*t/d, (37+dx)*s/d,(21+dy)*t/d
	dc.b	(40+dx)*s/d,(31+dy)*t/d, (6+dx)*s/d,(65+dy)*t/d, (-7+dx)*s/d,(52+dy)*t/d, (3+dx)*s/d,(40+dy)*t/d
	dc.b	(-4+dx)*s/d,(40+dy)*t/d, (-8+dx)*s/d,(25+dy)*t/d, (0+dx)*s/d,(21+dy)*t/d, (-8+dx)*s/d,(8+dy)*t/d
	dc.b	(-10+dx)*s/d,(-24+dy)*t/d, (-20+dx)*s/d,(2+dy)*t/d, (-26+dx)*s/d,(4+dy)*t/d, (-38+dx)*s/d,(-1+dy)*t/d
	dc.b	(-25+dx)*s/d,(-33+dy)*t/d, (0+dx)*s/d,(-55+dy)*t/d, (14+dx)*s/d,(-52+dy)*t/d
	dc.b	27-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 0*2,26*2
	
	; frame 29
	dc.b	28-1
	dc.b	(20+dx)*s/d,(-63+dy)*t/d, (34+dx)*s/d,(-56+dy)*t/d, (28+dx)*s/d,(-47+dy)*t/d, (36+dx)*s/d,(-43+dy)*t/d
	dc.b	(33+dx)*s/d,(-34+dy)*t/d, (35+dx)*s/d,(-23+dy)*t/d, (44+dx)*s/d,(-10+dy)*t/d, (44+dx)*s/d,(1+dy)*t/d
	dc.b	(34+dx)*s/d,(3+dy)*t/d, (22+dx)*s/d,(-14+dy)*t/d, (19+dx)*s/d,(-5+dy)*t/d, (40+dx)*s/d,(16+dy)*t/d
	dc.b	(46+dx)*s/d,(26+dy)*t/d, (41+dx)*s/d,(32+dy)*t/d, (35+dx)*s/d,(33+dy)*t/d, (2+dx)*s/d,(64+dy)*t/d
	dc.b	(-9+dx)*s/d,(50+dy)*t/d, (1+dx)*s/d,(40+dy)*t/d, (-4+dx)*s/d,(29+dy)*t/d, (3+dx)*s/d,(26+dy)*t/d
	dc.b	(-9+dx)*s/d,(8+dy)*t/d, (-10+dx)*s/d,(-16+dy)*t/d, (-16+dx)*s/d,(4+dy)*t/d, (-20+dx)*s/d,(8+dy)*t/d
	dc.b	(-33+dx)*s/d,(4+dy)*t/d, (-24+dx)*s/d,(-31+dy)*t/d, (0+dx)*s/d,(-54+dy)*t/d, (14+dx)*s/d,(-53+dy)*t/d
	dc.b	28-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 0*2,27*2
	
	; frame 30
	dc.b	25-1
	dc.b	(20+dx)*s/d,(-63+dy)*t/d, (34+dx)*s/d,(-57+dy)*t/d, (28+dx)*s/d,(-47+dy)*t/d, (36+dx)*s/d,(-43+dy)*t/d
	dc.b	(32+dx)*s/d,(-34+dy)*t/d, (33+dx)*s/d,(-23+dy)*t/d, (43+dx)*s/d,(-6+dy)*t/d, (41+dx)*s/d,(3+dy)*t/d
	dc.b	(32+dx)*s/d,(5+dy)*t/d, (50+dx)*s/d,(22+dy)*t/d, (44+dx)*s/d,(27+dy)*t/d, (34+dx)*s/d,(31+dy)*t/d
	dc.b	(36+dx)*s/d,(34+dy)*t/d, (30+dx)*s/d,(40+dy)*t/d, (-2+dx)*s/d,(62+dy)*t/d, (-13+dx)*s/d,(48+dy)*t/d
	dc.b	(2+dx)*s/d,(36+dy)*t/d, (0+dx)*s/d,(32+dy)*t/d, (3+dx)*s/d,(30+dy)*t/d, (-10+dx)*s/d,(7+dy)*t/d
	dc.b	(-13+dx)*s/d,(11+dy)*t/d, (-27+dx)*s/d,(8+dy)*t/d, (-23+dx)*s/d,(-28+dy)*t/d, (0+dx)*s/d,(-54+dy)*t/d
	dc.b	(15+dx)*s/d,(-53+dy)*t/d
	dc.b	25-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	0*2,24*2
	
	; frame 31
	dc.b	19-1
	dc.b	(21+dx)*s/d,(-64+dy)*t/d, (34+dx)*s/d,(-57+dy)*t/d, (29+dx)*s/d,(-47+dy)*t/d, (36+dx)*s/d,(-43+dy)*t/d
	dc.b	(31+dx)*s/d,(-32+dy)*t/d, (32+dx)*s/d,(-22+dy)*t/d, (40+dx)*s/d,(-3+dy)*t/d, (36+dx)*s/d,(6+dy)*t/d
	dc.b	(52+dx)*s/d,(19+dy)*t/d, (31+dx)*s/d,(33+dy)*t/d, (33+dx)*s/d,(35+dy)*t/d, (-6+dx)*s/d,(61+dy)*t/d
	dc.b	(-17+dx)*s/d,(46+dy)*t/d, (2+dx)*s/d,(33+dy)*t/d, (-7+dx)*s/d,(11+dy)*t/d, (-19+dx)*s/d,(11+dy)*t/d
	dc.b	(-21+dx)*s/d,(-26+dy)*t/d, (0+dx)*s/d,(-54+dy)*t/d, (14+dx)*s/d,(-53+dy)*t/d
	dc.b	19-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 0*2,18*2
	
	; frame 32
	dc.b	19-1
	dc.b	(21+dx)*s/d,(-65+dy)*t/d, (34+dx)*s/d,(-57+dy)*t/d, (29+dx)*s/d,(-47+dy)*t/d, (35+dx)*s/d,(-44+dy)*t/d
	dc.b	(29+dx)*s/d,(-32+dy)*t/d, (29+dx)*s/d,(-23+dy)*t/d, (37+dx)*s/d,(-1+dy)*t/d, (33+dx)*s/d,(2+dy)*t/d
	dc.b	(51+dx)*s/d,(17+dy)*t/d, (16+dx)*s/d,(49+dy)*t/d, (13+dx)*s/d,(46+dy)*t/d, (-10+dx)*s/d,(60+dy)*t/d
	dc.b	(-20+dx)*s/d,(45+dy)*t/d, (0+dx)*s/d,(33+dy)*t/d, (-7+dx)*s/d,(14+dy)*t/d, (-10+dx)*s/d,(14+dy)*t/d
	dc.b	(-18+dx)*s/d,(-24+dy)*t/d, (0+dx)*s/d,(-54+dy)*t/d, (15+dx)*s/d,(-54+dy)*t/d
	dc.b	19-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 0*2,18*2, 17*2,18*2
	
	; frame 33
	dc.b	19-1
	dc.b	(21+dx)*s/d,(-66+dy)*t/d, (34+dx)*s/d,(-59+dy)*t/d, (29+dx)*s/d,(-49+dy)*t/d, (34+dx)*s/d,(-46+dy)*t/d
	dc.b	(27+dx)*s/d,(-31+dy)*t/d, (27+dx)*s/d,(-21+dy)*t/d, (32+dx)*s/d,(0+dy)*t/d, (51+dx)*s/d,(15+dy)*t/d
	dc.b	(19+dx)*s/d,(49+dy)*t/d, (12+dx)*s/d,(43+dy)*t/d, (-15+dx)*s/d,(59+dy)*t/d, (-23+dx)*s/d,(43+dy)*t/d
	dc.b	(-4+dx)*s/d,(31+dy)*t/d, (-11+dx)*s/d,(1+dy)*t/d, (-11+dx)*s/d,(-10+dy)*t/d, (-17+dx)*s/d,(-23+dy)*t/d
	dc.b	(4+dx)*s/d,(-57+dy)*t/d, (14+dx)*s/d,(-55+dy)*t/d, (-1+dx)*s/d,(-56+dy)*t/d
	dc.b	19-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 16*2,17*2
	dc.b	0*2,17*2, 15*2,18*2, 16*2,18*2
	
	; frame 34
	dc.b	21-1
	dc.b	(21+dx)*s/d,(-69+dy)*t/d, (35+dx)*s/d,(-61+dy)*t/d, (28+dx)*s/d,(-51+dy)*t/d, (33+dx)*s/d,(-48+dy)*t/d
	dc.b	(24+dx)*s/d,(-27+dy)*t/d, (26+dx)*s/d,(-8+dy)*t/d, (51+dx)*s/d,(12+dy)*t/d, (22+dx)*s/d,(49+dy)*t/d
	dc.b	(10+dx)*s/d,(39+dy)*t/d, (-19+dx)*s/d,(57+dy)*t/d, (-28+dx)*s/d,(43+dy)*t/d, (-9+dx)*s/d,(28+dy)*t/d
	dc.b	(-12+dx)*s/d,(-6+dy)*t/d, (-10+dx)*s/d,(-17+dy)*t/d, (-14+dx)*s/d,(-22+dy)*t/d, (-4+dx)*s/d,(-55+dy)*t/d
	dc.b	(5+dx)*s/d,(-59+dy)*t/d, (14+dx)*s/d,(-59+dy)*t/d, (15+dx)*s/d,(14+dy)*t/d, (16+dx)*s/d,(25+dy)*t/d
	dc.b	(21+dx)*s/d,(19+dy)*t/d
	dc.b	21-1
	dc.b	18*2,20*2, 19*2,20*2, 18*2,19*2, 0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2
	dc.b	5*2,6*2, 6*2,7*2, 7*2,8*2, 8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2
	dc.b	13*2,14*2, 14*2,15*2, 15*2,16*2, 16*2,17*2, 0*2,17*2
	
	; frame 35
	dc.b	22-1
	dc.b	(21+dx)*s/d,(-71+dy)*t/d, (34+dx)*s/d,(-64+dy)*t/d, (28+dx)*s/d,(-53+dy)*t/d, (33+dx)*s/d,(-50+dy)*t/d
	dc.b	(24+dx)*s/d,(-32+dy)*t/d, (21+dx)*s/d,(-18+dy)*t/d, (32+dx)*s/d,(-6+dy)*t/d, (52+dx)*s/d,(8+dy)*t/d
	dc.b	(35+dx)*s/d,(43+dy)*t/d, (28+dx)*s/d,(48+dy)*t/d, (14+dx)*s/d,(39+dy)*t/d, (25+dx)*s/d,(16+dy)*t/d
	dc.b	(11+dx)*s/d,(8+dy)*t/d, (8+dx)*s/d,(30+dy)*t/d, (-22+dx)*s/d,(56+dy)*t/d, (-33+dx)*s/d,(43+dy)*t/d
	dc.b	(-15+dx)*s/d,(25+dy)*t/d, (-10+dx)*s/d,(-20+dy)*t/d, (-11+dx)*s/d,(-22+dy)*t/d, (-5+dx)*s/d,(-56+dy)*t/d
	dc.b	(6+dx)*s/d,(-61+dy)*t/d, (15+dx)*s/d,(-60+dy)*t/d
	dc.b	22-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 0*2,21*2

	; frame 36
	dc.b	25-1
	dc.b	(21+dx)*s/d,(-74+dy)*t/d, (34+dx)*s/d,(-67+dy)*t/d, (28+dx)*s/d,(-55+dy)*t/d, (33+dx)*s/d,(-52+dy)*t/d
	dc.b	(23+dx)*s/d,(-33+dy)*t/d, (22+dx)*s/d,(-25+dy)*t/d, (38+dx)*s/d,(-14+dy)*t/d, (35+dx)*s/d,(-7+dy)*t/d
	dc.b	(52+dx)*s/d,(3+dy)*t/d, (42+dx)*s/d,(42+dy)*t/d, (36+dx)*s/d,(47+dy)*t/d, (20+dx)*s/d,(40+dy)*t/d
	dc.b	(28+dx)*s/d,(14+dy)*t/d, (8+dx)*s/d,(2+dy)*t/d, (-1+dx)*s/d,(25+dy)*t/d, (-21+dx)*s/d,(45+dy)*t/d
	dc.b	(-30+dx)*s/d,(50+dy)*t/d, (-40+dx)*s/d,(37+dy)*t/d, (-23+dx)*s/d,(19+dy)*t/d, (-12+dx)*s/d,(-10+dy)*t/d
	dc.b	(-11+dx)*s/d,(-22+dy)*t/d, (-6+dx)*s/d,(-36+dy)*t/d, (-6+dx)*s/d,(-57+dy)*t/d, (7+dx)*s/d,(-64+dy)*t/d
	dc.b	(14+dx)*s/d,(-62+dy)*t/d
	dc.b	25-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 23*2,24*2, 0*2,24*2
	dc.b	22*2,23*2
	
	; frame 37
	dc.b	26-1
	dc.b	(21+dx)*s/d,(-76+dy)*t/d, (34+dx)*s/d,(-69+dy)*t/d, (28+dx)*s/d,(-56+dy)*t/d, (32+dx)*s/d,(-54+dy)*t/d
	dc.b	(23+dx)*s/d,(-38+dy)*t/d, (22+dx)*s/d,(-32+dy)*t/d, (42+dx)*s/d,(-21+dy)*t/d, (38+dx)*s/d,(-7+dy)*t/d
	dc.b	(52+dx)*s/d,(0+dy)*t/d, (51+dx)*s/d,(39+dy)*t/d, (46+dx)*s/d,(46+dy)*t/d, (28+dx)*s/d,(41+dy)*t/d
	dc.b	(28+dx)*s/d,(13+dy)*t/d, (8+dx)*s/d,(0+dy)*t/d, (-6+dx)*s/d,(21+dy)*t/d, (-27+dx)*s/d,(37+dy)*t/d
	dc.b	(-38+dx)*s/d,(41+dy)*t/d, (-46+dx)*s/d,(27+dy)*t/d, (-27+dx)*s/d,(13+dy)*t/d, (-12+dx)*s/d,(-12+dy)*t/d
	dc.b	(-11+dx)*s/d,(-23+dy)*t/d, (-4+dx)*s/d,(-41+dy)*t/d, (-6+dx)*s/d,(-58+dy)*t/d, (1+dx)*s/d,(-63+dy)*t/d
	dc.b	(8+dx)*s/d,(-66+dy)*t/d, (14+dx)*s/d,(-64+dy)*t/d
	dc.b	26-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 0*2,25*2
	
	; frame 38
	dc.b	24-1
	dc.b	(21+dx)*s/d,(-77+dy)*t/d, (34+dx)*s/d,(-71+dy)*t/d, (28+dx)*s/d,(-59+dy)*t/d, (32+dx)*s/d,(-56+dy)*t/d
	dc.b	(22+dx)*s/d,(-37+dy)*t/d, (45+dx)*s/d,(-28+dy)*t/d, (43+dx)*s/d,(-13+dy)*t/d, (33+dx)*s/d,(-11+dy)*t/d
	dc.b	(50+dx)*s/d,(-1+dy)*t/d, (57+dx)*s/d,(38+dy)*t/d, (52+dx)*s/d,(44+dy)*t/d, (34+dx)*s/d,(44+dy)*t/d
	dc.b	(29+dx)*s/d,(13+dy)*t/d, (6+dx)*s/d,(-2+dy)*t/d, (-10+dx)*s/d,(17+dy)*t/d, (-43+dx)*s/d,(33+dy)*t/d
	dc.b	(-50+dx)*s/d,(19+dy)*t/d, (-29+dx)*s/d,(7+dy)*t/d, (-12+dx)*s/d,(-14+dy)*t/d, (-12+dx)*s/d,(-23+dy)*t/d
	dc.b	(-3+dx)*s/d,(-44+dy)*t/d, (-6+dx)*s/d,(-58+dy)*t/d, (14+dx)*s/d,(-66+dy)*t/d, (9+dx)*s/d,(-67+dy)*t/d
	dc.b	24-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 0*2,22*2, 21*2,23*2, 22*2,23*2
	
	; frame 39
	dc.b	27-1
	dc.b	(21+dx)*s/d,(-78+dy)*t/d, (35+dx)*s/d,(-71+dy)*t/d, (28+dx)*s/d,(-60+dy)*t/d, (32+dx)*s/d,(-57+dy)*t/d
	dc.b	(23+dx)*s/d,(-42+dy)*t/d, (25+dx)*s/d,(-39+dy)*t/d, (47+dx)*s/d,(-32+dy)*t/d, (46+dx)*s/d,(-18+dy)*t/d
	dc.b	(39+dx)*s/d,(-15+dy)*t/d, (26+dx)*s/d,(-16+dy)*t/d, (48+dx)*s/d,(0+dy)*t/d, (59+dx)*s/d,(38+dy)*t/d
	dc.b	(55+dx)*s/d,(45+dy)*t/d, (37+dx)*s/d,(46+dy)*t/d, (28+dx)*s/d,(15+dy)*t/d, (6+dx)*s/d,(-3+dy)*t/d
	dc.b	(-11+dx)*s/d,(16+dy)*t/d, (-20+dx)*s/d,(21+dy)*t/d, (-46+dx)*s/d,(28+dy)*t/d, (-51+dx)*s/d,(13+dy)*t/d
	dc.b	(-29+dx)*s/d,(6+dy)*t/d, (-12+dx)*s/d,(-15+dy)*t/d, (-11+dx)*s/d,(-30+dy)*t/d, (-2+dx)*s/d,(-46+dy)*t/d
	dc.b	(-5+dx)*s/d,(-57+dy)*t/d, (15+dx)*s/d,(-66+dy)*t/d, (10+dx)*s/d,(-68+dy)*t/d
	dc.b	27-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	0*2,25*2, 24*2,26*2, 25*2,26*2

	; frame 40
	dc.b	29-1
	dc.b	(21+dx)*s/d,(-77+dy)*t/d, (34+dx)*s/d,(-70+dy)*t/d, (28+dx)*s/d,(-59+dy)*t/d, (31+dx)*s/d,(-57+dy)*t/d
	dc.b	(24+dx)*s/d,(-44+dy)*t/d, (27+dx)*s/d,(-39+dy)*t/d, (46+dx)*s/d,(-34+dy)*t/d, (48+dx)*s/d,(-20+dy)*t/d
	dc.b	(42+dx)*s/d,(-18+dy)*t/d, (23+dx)*s/d,(-18+dy)*t/d, (47+dx)*s/d,(4+dy)*t/d, (57+dx)*s/d,(41+dy)*t/d
	dc.b	(53+dx)*s/d,(47+dy)*t/d, (34+dx)*s/d,(49+dy)*t/d, (27+dx)*s/d,(16+dy)*t/d, (7+dx)*s/d,(-2+dy)*t/d
	dc.b	(-10+dx)*s/d,(19+dy)*t/d, (-18+dx)*s/d,(23+dy)*t/d, (-47+dx)*s/d,(26+dy)*t/d, (-50+dx)*s/d,(11+dy)*t/d
	dc.b	(-41+dx)*s/d,(7+dy)*t/d, (-27+dx)*s/d,(6+dy)*t/d, (-13+dx)*s/d,(-14+dy)*t/d, (-12+dx)*s/d,(-20+dy)*t/d
	dc.b	(-18+dx)*s/d,(-24+dy)*t/d, (-3+dx)*s/d,(-48+dy)*t/d, (-6+dx)*s/d,(-56+dy)*t/d, (10+dx)*s/d,(-68+dy)*t/d
	dc.b	(15+dx)*s/d,(-66+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 27*2,28*2, 0*2,28*2, 26*2,27*2
	
	; frame 41
	dc.b	30-1
	dc.b	(20+dx)*s/d,(-75+dy)*t/d, (34+dx)*s/d,(-69+dy)*t/d, (28+dx)*s/d,(-57+dy)*t/d, (30+dx)*s/d,(-55+dy)*t/d
	dc.b	(24+dx)*s/d,(-44+dy)*t/d, (28+dx)*s/d,(-38+dy)*t/d, (47+dx)*s/d,(-35+dy)*t/d, (50+dx)*s/d,(-21+dy)*t/d
	dc.b	(41+dx)*s/d,(-18+dy)*t/d, (23+dx)*s/d,(-18+dy)*t/d, (23+dx)*s/d,(-14+dy)*t/d, (45+dx)*s/d,(9+dy)*t/d
	dc.b	(53+dx)*s/d,(45+dy)*t/d, (49+dx)*s/d,(52+dy)*t/d, (31+dx)*s/d,(52+dy)*t/d, (23+dx)*s/d,(20+dy)*t/d
	dc.b	(6+dx)*s/d,(1+dy)*t/d, (-8+dx)*s/d,(22+dy)*t/d, (-17+dx)*s/d,(27+dy)*t/d, (-47+dx)*s/d,(25+dy)*t/d
	dc.b	(-49+dx)*s/d,(10+dy)*t/d, (-39+dx)*s/d,(6+dy)*t/d, (-24+dx)*s/d,(6+dy)*t/d, (-12+dx)*s/d,(-12+dy)*t/d
	dc.b	(-12+dx)*s/d,(-20+dy)*t/d, (-20+dx)*s/d,(-27+dy)*t/d, (-4+dx)*s/d,(-48+dy)*t/d, (-5+dx)*s/d,(-53+dy)*t/d
	dc.b	(11+dx)*s/d,(-66+dy)*t/d, (15+dx)*s/d,(-64+dy)*t/d
	dc.b	30-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 26*2,27*2, 28*2,29*2, 0*2,29*2, 27*2,28*2
	
	; frame 42
	dc.b	29-1
	dc.b	(20+dx)*s/d,(-73+dy)*t/d, (35+dx)*s/d,(-67+dy)*t/d, (28+dx)*s/d,(-55+dy)*t/d, (30+dx)*s/d,(-53+dy)*t/d
	dc.b	(25+dx)*s/d,(-43+dy)*t/d, (29+dx)*s/d,(-36+dy)*t/d, (47+dx)*s/d,(-35+dy)*t/d, (50+dx)*s/d,(-21+dy)*t/d
	dc.b	(41+dx)*s/d,(-17+dy)*t/d, (23+dx)*s/d,(-16+dy)*t/d, (24+dx)*s/d,(-11+dy)*t/d, (41+dx)*s/d,(14+dy)*t/d
	dc.b	(49+dx)*s/d,(48+dy)*t/d, (45+dx)*s/d,(55+dy)*t/d, (27+dx)*s/d,(56+dy)*t/d, (19+dx)*s/d,(23+dy)*t/d
	dc.b	(6+dx)*s/d,(3+dy)*t/d, (-8+dx)*s/d,(25+dy)*t/d, (-17+dx)*s/d,(29+dy)*t/d, (-48+dx)*s/d,(24+dy)*t/d
	dc.b	(-48+dx)*s/d,(9+dy)*t/d, (-38+dx)*s/d,(5+dy)*t/d, (-23+dx)*s/d,(8+dy)*t/d, (-13+dx)*s/d,(-10+dy)*t/d
	dc.b	(-13+dx)*s/d,(-18+dy)*t/d, (-22+dx)*s/d,(-27+dy)*t/d, (-5+dx)*s/d,(-49+dy)*t/d, (11+dx)*s/d,(-64+dy)*t/d
	dc.b	(15+dx)*s/d,(-62+dy)*t/d
	dc.b	29-1
	dc.b	0*2,1*2, 1*2,2*2, 2*2,3*2, 3*2,4*2, 4*2,5*2, 5*2,6*2, 6*2,7*2, 7*2,8*2
	dc.b	8*2,9*2, 9*2,10*2, 10*2,11*2, 11*2,12*2, 12*2,13*2, 13*2,14*2, 14*2,15*2, 15*2,16*2
	dc.b	16*2,17*2, 17*2,18*2, 18*2,19*2, 19*2,20*2, 20*2,21*2, 21*2,22*2, 22*2,23*2, 23*2,24*2
	dc.b	24*2,25*2, 25*2,26*2, 27*2,28*2, 0*2,28*2, 26*2,27*2
personend
	even


*------	COLOR FADING ----------------------------------------------------------*

fading	move.l	v_actors(a5),d7			; process actors
	btst	#actor_mccoy_cols,d7		;
	beq	.no				;
	move.l	b_clist2(pc),a1			;
	move.w	v_colindex(a5),d1		;

	lea	fcolorp(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,cp1-clist2+2(a1)		;
	move.w	d0,cp2-clist2+2(a1)		;
	move.w	d0,cp3-clist2+2(a1)		;
	move.w	d0,cp4-clist2+2(a1)		;
	move.w	d0,cp5-clist2+2(a1)		;
	move.w	d0,cp6-clist2+2(a1)		;
	move.w	d0,cp7-clist2+2(a1)		;
	move.w	d0,cp8-clist2+2(a1)		;
	move.w	d0,csd1-clist2+2(a1)		;

	lea	fcolort(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,ct1-clist2+2(a1)		;
	move.w	d0,ct2-clist2+2(a1)		;
	move.w	d0,ct3-clist2+2(a1)		;
	move.w	d0,csc1-clist2+2(a1)		;

	lea	fcolorsa(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,csa1-clist2+2(a1)		;
	move.w	d0,csa2-clist2+2(a1)		;

	lea	fcolorb1(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,cb1a-clist2+2(a1)		;
	move.w	d0,cb1b-clist2+2(a1)		;

	lea	fcolorb2(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,cb2a-clist2+2(a1)		;
	move.w	d0,cb2b-clist2+2(a1)		;
	move.w	d0,csb1-clist2+2(a1)		;
	move.w	d0,csb2-clist2+2(a1)		;

	lea	fcolorb3(pc),a0			;
	move.w	(a0,d1.w),d0			;
	move.w	d0,cb3a-clist2+2(a1)		;
	move.w	d0,cb3b-clist2+2(a1)		;

	move.w	v_coladdsub(a5),d0		;
	add.w	d0,v_colindex(a5)		;
	move.w	v_colstop(a5),d0		;
	cmp.w	v_colindex(a5),d0		;
	bne	.notfinished			;
	STOPACTOR actor_mccoy_cols		;
	subq.w	#2,v_colindex(a5)		;
	move.w	#-2,v_colstop(a5)		; prepare fade out
	neg.w	v_coladdsub(a5)			;
.notfinished	
.no	rts					;

fcolorp	dc.w	$0000,$0111,$0122,$0222,$0233,$0234,$0344,$0355
	dc.w	$0456,$0466,$0467,$0578,$0588,$0589,$069a,$06aa
	dc.w	$07ab,$07bb,$07bc,$08cd,$08dd,$09de,$09ef,$09ef
fcolorpend

fcolort	dc.w	$0000,$0111,$0111,$0122,$0222,$0233,$0234,$0344
	dc.w	$0345,$0455,$0456,$0466,$0567,$0577,$0578,$0688
	dc.w	$0689,$0699,$079a,$079a,$07ab,$08ab,$08bc,$08bc

fcolorsa
	dc.w	$0000,$0011,$0011,$0111,$0112,$0122,$0122,$0123
	dc.w	$0123,$0133,$0133,$0134,$0144,$0144,$0145,$0145
	dc.w	$0155,$0156,$0156,$0156,$0166,$0167,$0167,$0167

fcolorb1
	dc.w	$0000,$0111,$0112,$0122,$0223,$0233,$0244,$0344
	dc.w	$0355,$0355,$0366,$0467,$0477,$0478,$0588,$0589
	dc.w	$0599,$069a,$06ab,$06ab,$06bc,$07bc,$07cd,$07cd

fcolorb2
	dc.w	$0000,$0111,$0111,$0122,$0122,$0123,$0133,$0233
	dc.w	$0244,$0244,$0245,$0255,$0256,$0356,$0366,$0367
	dc.w	$0377,$0378,$0478,$0489,$0489,$049a,$049a,$049a

fcolorb3
	dc.w	$0000,$0001,$0011,$0011,$0011,$0011,$0011,$0011
	dc.w	$0011,$0012,$0012,$0012,$0012,$0022,$0022,$0022
	dc.w	$0022,$0023,$0023,$0023,$0023,$0023,$0023,$0023


*------	APPLY MTX -------------------------------------------------------------*

z	equ 0

; a2 = 2d data destination
; a3 = 3d data source
; d7 num vertices
applymtx
	move.w	v_distance(a5),d3		;
	lea	v_matrix(a5),a0			;
.loop	move.l	a0,a1				;

	move.b	(a3)+,d1			; x
	ext.w	d1				;
	move.b	(a3)+,d2			; y
	ext.w	d2				;
	
	if z
	moveq	#0,d0				; z
	endif

	move.w	d1,d4				;
	muls	(a1)+,d4			;
	move.w	d2,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;

	if z
	move.w	d0,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	else
	addq.w	#2,a1				;
	endif

	move.w	d1,d5				;
	muls	(a1)+,d5			;
	move.w	d2,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	
	if z
	move.w	d0,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	else
	addq.w	#2,a1				;
	endif
	
	muls	(a1)+,d1			;
	muls	(a1)+,d2			;
	
	if z
	muls	(a1)+,d0			;
	add.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
;	move.w	d0,d6				; (no need for z value)
	else
	addq.w	#2,a1				;
	move.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
	endif

	add.w	d3,d0				; distance
	divs	d0,d4				;
	divs	d0,d5				;
	
	move.w	d4,(a2)+			; x
	move.w	d5,(a2)+			; y

	dbf	d7,.loop			;
	rts					;


*------	MTX -------------------------------------------------------------------*

mtx	move.l	b_sintab(pc),a0			; sin
	lea	512(a0),a1			; cos

	move.w	#$07fe,d5			;
	move.w	v_a(a5),d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d0			; sin a
	move.w	(a1,d6.w),d3			; cos a

	move.w	v_b(a5),d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d1			; sin b
	move.w	(a1,d6.w),d4			; cos b

	move.w	v_c1(a5),d2			;
	and.w	d5,d2				;
	move.w	(a0,d2.w),d2			; 512
;	add.w	d2,d2				; double range of motion
	move.w	d2,d6				;

	move.w	v_c2(a5),d2			;
	and.w	d5,d2				;
	move.w	(a0,d2.w),d2			; 512
	add.w	d2,d6				;

	move.w	v_c3(a5),d2			;
	and.w	d5,d2				;
	move.w	(a0,d2.w),d2			; 512
	add.w	d2,d6				;

	move.w	v_c4offset(a5),d2		;
	sub.w	d2,v_c4(a5)			;

speed	equ	4

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_hook,d7			;
	beq	.nohook				;
	move.w	v_prev(a5),d2			;
	sub.w	d6,d2				;
	move.w	d6,v_prev(a5)			;
	cmp.w	#speed,d2			;
	bne	.nohook				;
	clr.w	v_c1offset(a5)			;
	clr.w	v_c2offset(a5)			;
	clr.w	v_c3offset(a5)			;
	STOPACTOR actor_hook			;
	move.w	#speed,v_c4offset(a5)		; keep rotating

.nohook	add.w	v_c4(a5),d6			;
	and.w	d5,d6				;

	btst	#actor_freeze,d7		;
	beq	.nofreeze			;
	move.w	d6,d2				;
	and.w	#$07fc,d2			; $07fc vs $07fe see compensation below
;	move.w	d2,v_number(a5)			; test compensation
;	tst.w	d2				; test compensation
	bne	.nofreeze			;
	subq.w	#2,v_c4(a5)			; compenstate speed $07fc
	clr.w	v_c4offset(a5)			; stop rotating
	STOPACTOR actor_freeze			;
.nofreeze
	move.w	(a0,d6.w),d2			; sin c
	move.w	(a1,d6.w),d5			; cos c

	lea	v_matrix(a5),a0			;
	move.w	d0,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d6,a1				;
	move.w	d3,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	move.w	d7,a2				;
	muls	d5,d6				;
	asr.l	#8,d6				;
	sub.w	d7,d6				;
	move.w	d6,6(a0)			;
	move.w	d3,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	move.w	d7,a3				;
	move.w	a1,d6				;
	muls	d2,d6				;
	asr.l	#8,d6				;
	add.w	d7,d6				;
	move.w	d6,8(a0)			;
	move.w	a3,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	add.w	d7,d6				;
	move.w	d6,12(a0)			;
	move.w	a2,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	sub.w	d7,d6				;
	move.w	d6,14(a0)			;
	muls	d4,d5				;
	asr.l	#8,d5				;
	move.w	d5,(a0)				;
	muls	d4,d2				;
	asr.l	#8,d2				;
	move.w	d2,2(a0)			;
	muls	d4,d0				;
	asr.l	#8,d0				;
	move.w	d0,10(a0)			;
	muls	d4,d3				;
	asr.l	#8,d3				;
	move.w	d3,16(a0)			;
	neg.w	d1				;
	move.w	d1,4(a0)			;
	rts					;
	
	
*------	UNCOVER ---------------------------------------------------------------*

uncover	move.l	v_coverdatap(a5),a0		;
	move.l	(a0)+,d0			; pattern		

	move.l	v_dbplane2a(a5),a1		; fill cover plane (black voronoi plane)
	move.l	v_dbplane2b(a5),a2		;
	add.w	#((pheight-logoheight)-(numcoverrows*coverrowheight))*pwidth,a1 ;
	add.w	#((pheight-logoheight)-(numcoverrows*coverrowheight))*pwidth,a2 ;

	move.w	(a0)+,d1			;
	add.w	d1,a1				;
	add.w	d1,a2				;
	move.w	(a0)+,d1			;
	move.w	.offset(pc,d1.w),d1		;
	add.w	d1,a1				;
	add.w	d1,a2				;

	moveq	#coverrowheight-1,d7		;
.loop	and.l	d0,(a1)				;
	and.l	d0,(a2)				;
	add.w	#pwidth,a1			;
	add.w	#pwidth,a2			;
	dbf	d7,.loop			;

	move.l	b_coverdata(pc),a3		;
	add.w	#coverdatasize,a3		;
	cmp.l	a0,a3				; done?
	beq	.stop				;

	move.l	a0,v_coverdatap(a5)		;
	rts					;

.stop	STOPACTOR actor_uncover_venus		;
	move.w	#2*pheight<<6+(pwidth-inviswidth)>>1,v_bltsize(a5) ; size = 2 bitplanes
	move.l	b_coverdata(pc),v_coverdatap(a5) ; used later (actor_cover_venus)
	rts					;

.offset	dc.w	0*coverrowheight*pwidth
	dc.w	1*coverrowheight*pwidth
	dc.w	2*coverrowheight*pwidth
	dc.w	3*coverrowheight*pwidth
	dc.w	4*coverrowheight*pwidth
	dc.w	5*coverrowheight*pwidth
	dc.w	6*coverrowheight*pwidth


*------	COVER -----------------------------------------------------------------*

cover	move.l	v_coverdatap(a5),a0		;
	move.l	(a0)+,d0			; pattern		
	move.w	(a0)+,d1			; x
	move.l	b_planet(pc),a1			; clear planet bitplane data
	add.w	d1,a1				;
	move.w	(a0)+,d1			; row (y)
	add.w	.offset(pc,d1.w),a1		;

	moveq	#coverrowheight-1,d7		;
.loop	and.l	d0,(a1)				; bitplane 1
	and.l	d0,1*planetsize/3(a1)		; bitplane 2
	and.l	d0,2*planetsize/3(a1)		; bitplane 2
	add.w	#planetwidth,a1			;
	dbf	d7,.loop			;

	move.l	b_coverdata(pc),a3		;
	add.w	#coverdatasize,a3		;
	cmp.l	a0,a3				; done?
	beq	.stop				;

	move.l	a0,v_coverdatap(a5)		;
	rts					;

.stop	STOPACTOR actor_cover_venus		;
	rts					;

; note: special offsets (-x) because Venus = 87 px (not 91) high
	
.offset	dc.w	0*coverrowheight*planetwidth
	dc.w	1*coverrowheight*planetwidth
	dc.w	2*coverrowheight*planetwidth
	dc.w	(3*coverrowheight-1)*planetwidth
	dc.w	(4*coverrowheight-2)*planetwidth
	dc.w	(5*coverrowheight-3)*planetwidth
	dc.w	(6*coverrowheight-4)*planetwidth ; 78-4+13=87


*------	COVER DATA ------------------------------------------------------------*
	
coverdata
	dc.l	%11111111111110000000000000000000
	dc.w	0

	dc.l	%00000000000001111111111111000000
	dc.w	0

	dc.l	%00000000001111111111111000000000
	dc.w	2

	dc.l	%00000001111111111111000000000000
	dc.w	4

	dc.l	%00001111111111111000000000000000
	dc.w	6

	dc.l	%00000000000000000111111111111100
	dc.w	6

	dc.l	%00000000000000111111111111100000
	dc.w	8

	dc.l	%00000000000111111111111100000000
	dc.w	10

	dc.l	%00000000111111111111100000000000
	dc.w	12

	dc.l	%00000111111111111100000000000000
	dc.w	14

	dc.l	%00000000000000000011111111111110
	dc.w	14

	dc.l	%00000000000000011111111111110000
	dc.w	16

	dc.l	%00000000000011111111111110000000
	dc.w	18

	dc.l	%00000000011111111111110000000000
	dc.w	20

	dc.l	%00000011111111111110000000000000
	dc.w	22

	dc.l	%00011111111111110000000000000000
	dc.w	24

	dc.l	%00000000000000001111111111111000
	dc.w	24

	dc.l	%00000000000001111111111111000000
	dc.w	26

	dc.l	%00000000001111111111111000000000
	dc.w	28

	dc.l	%00000001111111111111000000000000
	dc.w	30

	dc.l	%00001111111111111000000000000000
	dc.w	32

	dc.l	%01111111111111000000000000000000
	dc.w	34

	dc.l	%00000000000000111111111111100000
	dc.w	34

	dc.l	%00000000000111111111111100000000
	dc.w	36

	dc.l	%00000000111111111111100000000000
	dc.w	38

	dc.l	%00000111111111111100000000000000
	dc.w	40

	dc.l	%00000000000000000011111111111111
	dc.w	40
coverdataend


*------	ORBIT -----------------------------------------------------------------*

moveorbits
	move.l	b_spritedata(pc),a1		;
	move.l	b_clist1(pc),a4			;

	lea	v_orbits(a5),a0			;
	moveq	#numorbits-1,d7			;
.loop	move.l	(a0),a3				; orbit data
	
	moveq	#0,d0				;
	move.b	(a3)+,d0			; x
	moveq	#0,d1				;
	move.b	(a3)+,d1			; y
;	add.w	#298-255,d1			; orig
	add.w	#298-5-255,d1			; -5 = adjust for "pole stars"

	moveq	#0,d2				;
	move.b	(a3)+,d2			; x extra bit and color
	btst	#4,d2				; x extra bit set?
	beq	.notset				;
	bset	#8,d0				;
.notset	and.w	#$000f,d2			; mask out color bits

	movem.l	d2/d7/a0/a1,-(a7)		; note: this part is pretty lame
	asl.w	#5,d2				;
;	moveq	#0,d2				; testing
	lea	sprdat(pc),a0			;
	addq.w	#4,a1				; skip header
	
	btst	#0,d7				; even or odd sprite? -> sprite color
	bne	.odd				;
	addq.w	#2,a1				;	
.odd	add.w	d2,a0				;
	
	moveq	#16-1,d7			; sprite height
.copy	move.w	(a0)+,(a1)			;
	addq.w	#4,a1				;
	dbf	d7,.copy			;
	movem.l	(a7)+,d2/d7/a0/a1		;
	
	add.w	d2,d2				; word index
	move.w	.grey(pc,d2.w),spritecolors+2-clist1(a4) ;
;	move.w	#$0ddd,spritecolors+2-clist1(a4) ; testing

	add.w	#112,d0				; adjust x

;	move.w	#260,d0				; testing
;	moveq	#107,d1				; testing

	cmp.l	#$19052900,(a1)			; dried out?
	beq	.driedout			;
	moveq	#16,d2				; sprite height
	bsr	setspritepos			;
.driedout
	cmp.l	4(a0),a3			; end of orbit data?
	bne	.neod				;

	move.w	v_orbitindex(a5),d0		;
	addq.w	#1,v_orbitindex(a5)		;
	cmp.w	#(orbitsend-orbits)/8,v_orbitindex(a5) ;
	bne	.good				;
	clr.w	v_orbitindex(a5)		;
.good	asl.w	#3,d0				; *8 index to data
	lea	orbits(pc),a3			;
	move.l	(a3,d0.w),(a0)			; data start
	move.l	4(a3,d0.w),4(a0)		; data end

	move.l	v_actors(a5),d6			; process actors
	btst	#actor_dry_out_orbits,d6	;
	beq	.notdry				;

	move.l	#$19052900,(a1)			; invisible position $19+16 = $29
.notdry	bra	.reset				;
.neod	move.l	a3,(a0)				;
.reset	addq.w	#8,a0				; next orbit
	addq.w	#4,a4				; next sprite color in clist
	add.w	#sprite1data-sprite0data,a1	; next sprite data
	dbf	d7,.loop			;
	rts					;

.grey	dc.w	$0111,$0222,$0322,$0333
	dc.w	$0444,$0555,$0666,$0777
	dc.w	$0888,$0999,$0aaa,$0bbb
	dc.w	$0ccc,$0ddd,$0eee,$0fff

sprdat	dc.w	%0000000000000000 ; index 0: 1 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 1: 2 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 2: 3 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000001110000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 3: 4 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000001110000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 4: 5 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000011111000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 5: 6 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000011111000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 6: 7 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000111111100000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 7: 8 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000111111100000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 8: 9 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0001111111110000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 9: 10 px
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0001111111110000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 10: 11 px
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0011111111111000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 11: 12 px
	dc.w	%0000000000000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0011111111111000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 12: 13 px
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0111111111111100
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000 ; index 13: 14 px
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0111111111111100
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000100000000 ; index 14: 15 px
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%1111111111111110
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000

	dc.w	%0000000100000000 ; index 15: 16 px
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%1111111111111110
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000100000000
	dc.w	%0000000000000000


*------ SET BITPLANE POINTERS CLIST 1 -----------------------------------------*

; d0: bitplane1 d2: bitplane2
setbitplanepointersclist1
	move.l	b_clist1(pc),a1			;
	move.l	d0,d1				;
	move.w	d0,bplvor-clist1+2+4(a1)	;
	swap	d0				;	
	move.w	d0,bplvor-clist1+2(a1)		;
	add.l	#133*pwidth,d1			; overlap Venus
	move.w	d1,bplvor2-clist1+2+4(a1)	;
	swap	d1				;	
	move.w	d1,bplvor2-clist1+2(a1)		;

	move.l	d2,d1				;
	move.w	d2,bplvor-clist1+2+4+8(a1)	;
	swap	d2				;	
	move.w	d2,bplvor-clist1+2+8(a1)	;

	add.l	#133*pwidth,d1			; overlap Venus
	move.w	d1,bplvor2-clist1+2+4+8(a1)	;
	swap	d1				;	
	move.w	d1,bplvor2-clist1+2+8(a1)	;
	rts					;


*------	SET SPRITE POSITION ---------------------------------------------------*

; 0 VSTART  1 HSTART
; xxxxxxxx  xxxxxxxx
;
; 2 VSTOP   3 MISC/CTRL
; xxxxxxxx  A----xxx
;                |||
;                VSTART high bit
;                 ||
;                 VSTOP high bit
;                  |
;                  HSTART low bit

; must not alter d7, a1
setspritepos
	move.w	d0,d3				; process x
	move.w	#550,d5				; was 450
	cmp.w	d5,d3				; outside of visible area?
	bcs	.lower				;
	move.w	d5,d3				;
.lower	moveq	#0,d5				; init CTRL
	asr.w	#1,d3				;
	bcc	.2				; was bvc (omg! 2^5 years)
	moveq	#1,d5				; set HSTART low bit
.2	move.b	d3,1(a1)			; write HSTART
	move.w	d1,d3				; process y
	move.b	d3,(a1)				; write VSTART
	clr.b	d3				;
	tst.w	d3				;
	beq	.3				;
	addq.b	#4,d5				; set VSTART high bit
.3	move.w	d1,d3				; process height VSTOP
	add.w	d2,d3				;
	move.b	d3,2(a1)			; write VSTOP
	clr.b	d3				;
	tst.w	d3				;
	beq	.4				;
	addq.b	#2,d5				; set VSTOP high bit
.4	move.b	d5,3(a1)			; write CTRL
	rts					;

	
*------ SET SPRITE POINTERS ---------------------------------------------------*

; a0: clist d0: sprite data
setspritepointers
	swap	d0				; sprite0
	move.w	d0,(a0)				;
	swap	d0				;
	move.w	d0,4(a0)			;
	add.l	#sprite1data-sprite0data,d0	; sprite1
	swap	d0				;
	move.w	d0,8(a0)			;
	swap	d0				;
	move.w	d0,12(a0)			;
	add.l	#sprite2data-sprite1data,d0	; sprite2
	swap	d0				;
	move.w	d0,16(a0)			;
	swap	d0				;
	move.w	d0,20(a0)			;
	add.l	#sprite3data-sprite2data,d0	; sprite3
	swap	d0				;
	move.w	d0,24(a0)			;
	swap	d0				;
	move.w	d0,28(a0)			;
	add.l	#sprite4data-sprite3data,d0	; sprite4
	swap	d0				;
	move.w	d0,32(a0)			;
	swap	d0				;
	move.w	d0,36(a0)			;
	add.l	#sprite5data-sprite4data,d0	; sprite5
	swap	d0				;
	move.w	d0,40(a0)			;
	swap	d0				;
	move.w	d0,44(a0)			;
	add.l	#sprite6data-sprite5data,d0	; sprite6
	swap	d0				;
	move.w	d0,48(a0)			;
	swap	d0				;
	move.w	d0,52(a0)			;
	add.l	#sprite7data-sprite6data,d0	; sprite7
	swap	d0				;
	move.w	d0,56(a0)			;
	swap	d0				;
	move.w	d0,60(a0)			;
	rts					;


*------ SPRITE DATA -----------------------------------------------------------*

spritedata
sprite0data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0110000000000000,%0000000000000000 ; mind the "color" word
	endr
	dc.w	$0000,$0000
	
sprite1data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0000000000000000,%0110000000000000 ; mind the "color word"
	endr
	dc.w	$0000,$0000

sprite2data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0110000000000000,%0000000000000000
	endr
	dc.w	$0000,$0000

sprite3data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0000000000000000,%0110000000000000
	endr
	dc.w	$0000,$0000

sprite4data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0110000000000000,%0000000000000000
	endr
	dc.w	$0000,$0000

sprite5data
	dc.w	$1905,$1b00
	rept	16
	dc.w	0,0
;	dc.w	%0000000000000000,%0110000000000000
	endr
	dc.w	$0000,$0000

sprite6data
	dc.w	$1905,$1a00	; 1px high sprite in the top-leftmost valid position
	dc.w	$0000,$0000	; blank pixel data
	dc.w	$0000,$0000	; end of sprite

sprite7data
	dc.w	$1905,$1a00	; 1px high sprite in the top-leftmost valid position
	dc.w	$0000,$0000	; blank pixel data
	dc.w	$0000,$0000	; end of sprite
spritedataend


*------	MORPH DATA ------------------------------------------------------------*

tiltdata
	dc.w	290,290,290,288,286,284,282,280,278,274
	dc.w	272,268,266,262,258,254,248,244,240,234
	dc.w	228,224,218,212,206,198,192,184,178,170
	dc.w	162,154,146,138,130,122,114,108,100,94
	dc.w	86,80,74,68,64,58,52,48,44,38
	dc.w	34,30,26,24,20,18,14,12,10,8
	dc.w	6,4,2,2,0
tiltdataend

zoomdata
	dc.w	360,360,358,358,358,358,356,356,354,354
	dc.w	352,352,350,350,348,346,344,344,342,340
	dc.w	338,336,334,332,330,326,324,322,320,316
	dc.w	314,310,308,306,302,300,296,294,292,290
	dc.w	286,284,282,280,278,276,274,272,272,270
	dc.w	268,266,266,264,264,262,262,260,260,258
	dc.w	258,258,258,256,256

yposdata
; 36 + 14 = 50?
	dc.w	pwidth,pwidth,pwidth,pwidth,pwidth,pwidth,pwidth,pwidth,pwidth,pwidth
	dc.w	pwidth,pwidth,pwidth,pwidth,0,0,0,0,0,0
	dc.w	0,0,0,0,0,0,0,0,0,0
	dc.w	0,0,0,0,0,0,0,0,0,0
	dc.w	0,0,0,0,0,0,0,0,0,0
	dc.w	0,0,0,0,0,0,0,0,0,0
	dc.w	0,0,0,0,0


*------	SKY PLAYER ------------------------------------------------------------*

skyplayer
	move.l	v_skydatap(a5),a0	;
	move.l	a0,a2			;
	move.l	b_clist2(pc),a1		; sprites clist 2
	add.w	#spritepointersclist2+2-clist2,a1 ;
	
	moveq	#8-1,d2			; 8 sprites
.set	move.l	a2,a3			;
	add.w	(a0)+,a3		;
	move.l	a3,d1			;
	move.w	d1,4(a1)		;
	swap	d1			;
	move.w	d1,(a1)			;
	addq.w	#8,a1			;
	dbf	d2,.set			;

	add.w	(a0)+,a2		;
	cmp.w	#$1994,(a2)		; end of data? nice magic word. #TLB1994
	bne	.noreset		;
	move.l	v_sky(a5),a2		;
.noreset
	move.l	a2,v_skydatap(a5)	;
	rts				;


*------	PLAYER ----------------------------------------------------------------*

play	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
	rts				;

.donotwait
	move.l	v_cmdspointer(a5),a0	;
.loop	move.b	(a0)+,d0		; cmd_wait (0)?
	bne	.1			;
	move.b	(a0)+,v_wait(a5)	; duration
	move.l	a0,v_cmdspointer(a5)	;

	if numbers
	addq.b	#1,v_waitcount(a5)	; sync helper
	endif

	rts				;

.1	subq.b	#1,d0			; cmd_actor_start (1)?
	bne	.2			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bset	d1,d2			;
	move.l	d2,v_actors(a5)		;
	bra	.loop			;

.2	subq.b	#1,d0			; cmd_actor_stop (2)?
	bne	.3			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bclr	d1,d2			;
	move.l	d2,v_actors(a5)		;
	bra	.loop			;

.3	subq.b	#1,d0			; cmd_rewind (3)?
	bne	.4			;
	lea	playrewind(pc),a0	;
	bra	.loop			;

.4	subq.b	#1,d0			; cmd_show_logo (4)?
	bne	.5			;
	move.l	b_clist1(pc),a1		;
	move.w	#$4200,bplc01-clist1+2(a1) ;
	move.w	#cfill,d1		;
	move.w	d1,cfill1-clist1+2(a1)	;
	move.w	d1,cfill2-clist1+2(a1)	;
	move.w	#clines,clines1-clist1+2(a1) ;
	bra	.loop			;

.5	subq.b	#1,d0			; cmd_color (5)?
	bne	.6			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)

	move.l	b_clist1(pc),a1		;
	move.w	d1,cfill1-clist1+2(a1)	;
	move.w	d1,cfill2-clist1+2(a1)	;
	bra	.loop			;
	
.6	subq.b	#1,d0			; cmd_set_offset (6)?
	bne	.7			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; address/offset v_c
	move.b	(a0)+,d2		;
	ext.w	d2			; value
	move.w	d2,v_c1offset(a5,d1.w)	;
	bra	.loop			;

.7	subq.b	#1,d0			; cmd_grey (7)?
	bne	.8			;
	move.l	b_clist3(pc),$80(a6)	;
	STARTACTOR actor_grey		;
	move.w	#pheight/2*pwidth+center-voradjust+(logoheight*pwidth),v_yoffset(a5) ;
	bra	.loop			;

.8	subq.b	#1,d0			; cmd_bgcolor (8)?
	bne	.9			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)
	move.l	b_clist1(pc),a1		;
	move.w	d1,bgcl1-clist1+2(a1)	;
	move.l	b_clist3(pc),a1		;
	move.w	d1,bgcl3-clist3+2(a1)	;
	bra	.loop			;

.9	subq.b	#1,d0			; cmd_triggertext (9)?
	bne	.10			;
	st	v_triggertext(a5)	;
	bra	.loop			;
	
.10	subq.b	#1,d0			; cmd_main (10)?
	bne	.11			;
	move.l	b_clist1(pc),$80(a6)	;
	STARTACTOR actor_voronoi3d	;
	if testing
	move.w	v_frame(a5),v_number+2(a5) ;
	endif
	bra	.loop			;

.11	subq.b	#1,d0			; cmd_circwave (11)?
	bne	.12			;
	move.l	b_clistcw1(pc),$80(a6)	;
	STARTACTOR actor_circwave	;
	if testing
	move.w	v_frame(a5),v_number+2(a5) ;
	endif
	bra	.loop			;

.12	subq.b	#1,d0			; cmd_cw_cols (12)?
	bne	.13			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; combinded bg fg colors %bbbbffff

	move.l	b_clistcw1(pc),a1	;
	move.l	b_clistcw2(pc),a2	;

	move.l	d1,d4			; bg
	and.b	#%11110000,d4		;
	asr.w	#4,d4			;
	move.l	d4,d2			;
	asl.w	#4,d4			;
	or.w	d4,d2			;
	asl.w	#4,d4			;
	or.w	d4,d2			;
	move.w	d2,colscw1-clistcw1+2(a1) ;
	move.w	d2,colscw2-clistcw2+2(a2) ;

	move.l	d1,d4			; fg
	and.b	#%00001111,d4		;
	move.l	d4,d2			;
	asl.w	#4,d4			;
	or.w	d4,d2			;
	asl.w	#4,d4			;
	or.w	d4,d2			;
	move.w	d2,colscw1-clistcw1+4+2(a1) ;
	move.w	d2,colscw2-clistcw2+4+2(a2) ;
	bra	.loop			;

.13	subq.b	#1,d0			; cmd_cw_text (13)?
	bne	.14			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		;
	ext.w	d1			;
	move.l	b_clistcw2(pc),a1	;
	move.l	v_cwplanetext(a5),d2	; bitplane text
	add.l	#(textypos-(2*textheight))*cwpwidth,d2 ;
.cue	add.l	#textheight*cwpwidth,d2	; cue to text line #terracrestastyle
	dbf	d1,.cue			;
	move.w	d2,bplcw2-clistcw2+2+4(a1) ;
	swap	d2			;	
	move.w	d2,bplcw2-clistcw2+2(a1) ;
.14	bra	.loop			;


; notes
;	move.w	#$0100,v_distance(a5)	; zoomdata does this (checked with numbers v_distance)
;	move.w	#$0156,v_distance(a5)	; max size while rotating

; commands
cmd_wait	equ 	0
cmd_actor_start	equ	1
cmd_actor_stop	equ	2
cmd_rewind	equ	3
cmd_show_logo	equ	4
cmd_color	equ	5
cmd_set_offset	equ	6
cmd_grey	equ	7
cmd_bgcolor	equ	8
cmd_triggertext	equ	9
cmd_main	equ	10
cmd_circwave	equ	11
cmd_cw_cols	equ	12
cmd_cw_text	equ	13

; actor bits
actor_orbits		equ	0
actor_plateau		equ	1
actor_voronoi		equ	2
actor_uncover_venus	equ	3
actor_dry_out_orbits	equ	4
actor_cover_venus	equ	5
actor_fill		equ	6
actor_grey		equ	7
actor_player		equ	8
actor_hook		equ	9
actor_morph		equ	10
actor_freeze		equ	11
actor_mccoy		equ	12
actor_mccoy_cols	equ	13
actor_prepare_main	equ	14
actor_voronoi3d		equ	15
actor_prepare_circwave	equ	16
actor_circwave		equ	17
actor_fx_circwave	equ	18
actor_gen_text		equ	19
actor_logo_fading	equ	20

playcmds
	dc.b	cmd_wait,50
	dc.b	cmd_actor_start,actor_mccoy_cols
	dc.b	cmd_wait,200
	dc.b	cmd_wait,220
	dc.b	cmd_actor_start,actor_mccoy_cols
	dc.b	cmd_wait,26 ; wait for fade out

	dc.b	cmd_actor_stop,actor_mccoy
	dc.b	cmd_actor_start,actor_prepare_circwave
	dc.b	cmd_wait,50 ; CHECKED - no not change this
	dc.b	cmd_cw_text,0 ; MUST follow after prepare circwave
	dc.b	cmd_wait,0
	dc.b	cmd_circwave

	dc.b	cmd_cw_cols,$00,cmd_wait,3
	dc.b	cmd_cw_cols,$11,cmd_wait,3
	dc.b	cmd_cw_cols,$22,cmd_wait,3
	dc.b	cmd_cw_cols,$33,cmd_wait,3
	dc.b	cmd_cw_cols,$44,cmd_wait,3
	dc.b	cmd_cw_cols,$55,cmd_wait,3
	dc.b	cmd_cw_cols,$66,cmd_wait,3
	dc.b	cmd_cw_cols,$67,cmd_wait,3
	dc.b	cmd_cw_cols,$68,cmd_wait,3
	dc.b	cmd_cw_cols,$69,cmd_wait,3
	dc.b	cmd_cw_cols,$6a,cmd_wait,3
	dc.b	cmd_cw_cols,$6b,cmd_wait,3
	dc.b	cmd_cw_cols,$6c,cmd_wait,3
	dc.b	cmd_cw_cols,$6d
	dc.b	cmd_actor_start,actor_fx_circwave

	dc.b	cmd_wait,250
	dc.b	cmd_wait,200
	dc.b	cmd_cw_text,1 ; SPREADPOINT
	dc.b	cmd_wait,150
	dc.b	cmd_cw_text,0 ; (void)
	dc.b	cmd_wait,50
	dc.b	cmd_cw_text,2 ; PRESENTS
	dc.b	cmd_wait,150-10
	dc.b	cmd_cw_text,0 ; (void)
	dc.b	cmd_wait,130+10
	dc.b	cmd_actor_stop,actor_fx_circwave
	dc.b	cmd_wait,100

	dc.b	cmd_cw_cols,$6c,cmd_wait,3
	dc.b	cmd_cw_cols,$6b,cmd_wait,3
	dc.b	cmd_cw_cols,$6a,cmd_wait,3
	dc.b	cmd_cw_cols,$69,cmd_wait,3
	dc.b	cmd_cw_cols,$68,cmd_wait,3
	dc.b	cmd_cw_cols,$67,cmd_wait,3
	dc.b	cmd_cw_cols,$66,cmd_wait,3
	dc.b	cmd_cw_cols,$55,cmd_wait,3
	dc.b	cmd_cw_cols,$44,cmd_wait,3
	dc.b	cmd_cw_cols,$33,cmd_wait,3
	dc.b	cmd_cw_cols,$22,cmd_wait,3
	dc.b	cmd_cw_cols,$11,cmd_wait,3
	dc.b	cmd_cw_cols,$00

	dc.b	cmd_actor_stop,actor_circwave
	dc.b	cmd_wait,0
	dc.b	cmd_actor_start,actor_prepare_main
	dc.b	cmd_wait,80 ; CHECKED - do not change this
	dc.b	cmd_main

	dc.b	cmd_wait,50
	dc.b	cmd_actor_start,actor_uncover_venus
	
	dc.b	cmd_wait,200

	dc.b	cmd_actor_stop,actor_uncover_venus
	dc.b	cmd_actor_start,actor_orbits
	dc.b	cmd_wait,250

	dc.b	cmd_show_logo, cmd_actor_start,actor_logo_fading
	dc.b	cmd_wait,250
	dc.b	cmd_wait,190+30

	dc.b	cmd_actor_start,actor_fill, cmd_actor_start,actor_plateau

	dc.b	cmd_color,$0e,$ee, cmd_wait,1
	dc.b	cmd_color,$0d,$dd, cmd_wait,1
	dc.b	cmd_color,$0c,$cc, cmd_wait,1
	dc.b	cmd_color,$0b,$bb, cmd_wait,1
	dc.b	cmd_color,$0a,$aa, cmd_wait,1
	dc.b	cmd_color,$09,$99, cmd_wait,1
	dc.b	cmd_color,$08,$88, cmd_wait,1
	dc.b	cmd_color,$07,$77, cmd_wait,1
	dc.b	cmd_color,$06,$66, cmd_wait,1
	dc.b	cmd_color,$05,$55, cmd_wait,1
	dc.b	cmd_color,$04,$44, cmd_wait,1
	dc.b	cmd_color,$03,$33, cmd_wait,1
	dc.b	cmd_color,$02,$22, cmd_wait,1
	dc.b	cmd_color,$01,$11

	dc.b	cmd_wait,50-30
	dc.b	cmd_set_offset,0,2
	dc.b	cmd_wait,30
	dc.b	cmd_set_offset,0,4
	dc.b	cmd_wait,40-10
	dc.b	cmd_set_offset,0,6
	dc.b	cmd_wait,50-20
	dc.b	cmd_set_offset,0,8

	dc.b	cmd_wait,200+20+10

	dc.b	cmd_actor_start,actor_voronoi

	dc.b	cmd_color,$0e,$ee, cmd_wait,1
	dc.b	cmd_color,$0d,$dd, cmd_wait,1
	dc.b	cmd_color,$0c,$cc, cmd_wait,1
	dc.b	cmd_color,$0b,$bb, cmd_wait,1
	dc.b	cmd_color,$0a,$aa, cmd_wait,1
	dc.b	cmd_color,$09,$99, cmd_wait,1
	dc.b	cmd_color,$08,$88, cmd_wait,1
	dc.b	cmd_color,$07,$77, cmd_wait,1
	dc.b	cmd_color,$06,$66, cmd_wait,1
	dc.b	cmd_color,$05,$55, cmd_wait,1
	dc.b	cmd_color,$04,$44, cmd_wait,1
	dc.b	cmd_color,$03,$33, cmd_wait,1
	dc.b	cmd_color,$02,$22, cmd_wait,1
	dc.b	cmd_color,$01,$11
	
	dc.b	cmd_wait,200

	dc.b	cmd_set_offset,4,2
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,4,4
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,4,6
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,4,8
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,4,10

	dc.b	cmd_set_offset,2,2
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,2,4
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,2,6
	dc.b	cmd_wait,50
	dc.b	cmd_set_offset,2,8
	dc.b	cmd_wait,76
	dc.b	cmd_set_offset,2,6

	dc.b	cmd_wait,200-50-76
	dc.b	cmd_actor_start,actor_dry_out_orbits

	dc.b	cmd_wait,180
	dc.b	cmd_actor_start,actor_cover_venus

	dc.b	cmd_wait,100
		
	dc.b	cmd_actor_start,actor_hook
	dc.b	cmd_wait,255
	
	dc.b	cmd_actor_stop,actor_fill, cmd_bgcolor,$01,$11, cmd_wait,0
	dc.b	cmd_bgcolor,$01,$11, cmd_wait,0
	dc.b	cmd_grey, cmd_bgcolor,$02,$22, cmd_wait,1
	dc.b	cmd_bgcolor,$03,$33, cmd_wait,1
	dc.b	cmd_bgcolor,$04,$44, cmd_wait,1
	dc.b	cmd_bgcolor,$05,$55, cmd_wait,1
	dc.b	cmd_bgcolor,$06,$66

	dc.b	cmd_wait,50

	dc.b	cmd_actor_start,actor_gen_text
	dc.b	cmd_actor_start,actor_morph
	dc.b	cmd_wait,100

	dc.b	cmd_actor_start,actor_freeze
	dc.b	cmd_wait,200

playrewind
	dc.b	cmd_triggertext
	dc.b	cmd_wait,33	; text
	dc.b	cmd_triggertext
	dc.b	cmd_wait,6	; empty text
	dc.b	cmd_rewind

	even


*------	COPPER INSTRUCTION LIST 1 ---------------------------------------------*

clist1	dc.w	$1007,$fffe ; chance for player to alter clist in time

spritepointersclist1
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c71 ; DIWSTRT normal=$2c81
	dc.w	$0090,$2cd1 ; DIWSTOP normal=$2cc1
; http://amiga-dev.wikidot.com/hardware:ddfstrt
	dc.w	$0092,$0030 ; DDFSTRT normal=$38 wide=$30
	dc.w	$0094,$00d8 ; DDFSTOP normal=$d0 wide=$d8

bpllogo	dc.w	$00e0,0,$00e2,0 ; bitplane 1
	dc.w	$00e4,0,$00e6,0 ; bitplane 2
	dc.w	$00e8,0,$00ea,0 ; bitplane 3
	dc.w	$00ec,0,$00ee,0 ; bitplane 4

bplc01	dc.w	$0100,$0200 ; later set to $4200
	dc.w	$0102,$0000
	dc.w	$0104,$0044 ; PF2 (Voroni) sprites PF1 (Venus)
	dc.w	$0108,$0000
	dc.w	$010a,$0000
	
bgcl1	dc.w	$0180,0

logocl1	dc.w	$0182,0,$0184,0,$0186,0
	dc.w	$0188,0,$018a,0,$018c,0,$018e,0
	dc.w	$0190,0,$0192,0,$0194,0,$0196,0
	dc.w	$0198,0,$019a,0,$019c,0,$019e,0

spritecolors
	dc.w	$01a2,0,$01a4,0 ; sprite 0 and 1
	dc.w	$01aa,0,$01ac,0 ; sprite 2 and 3
	dc.w	$01b2,0,$01b4,0 ; sprite 4 and 5
	
	; not used (not available)
	dc.w	$01ba,$0000,$01bc,$0000 ; sprite 6 and 7
	
	dc.w	$01a0,$0000,$01a6,$0000 ; sprite 0 and 1
	dc.w	$01a8,$0000,$01ae,$0000 ; sprite 2 and 3
	dc.w	$01b0,$0000,$01b6,$0000 ; sprite 4 and 5
	dc.w	$01b8,$0000,$01be,$0000 ; sprite 6 and 7

; sprite 7 unusable due to wide screen
; sprite 6 is "corrupted"
; https://eab.abime.net/showthread.php?t=89276

lspline	equ $30
	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe

	if timing
	dc.w	$0180,$00f0
	endif
lspdmacon1
	dc.b	$00,$96,$80,0
;	dc.w	$0180,$0000

; Voronoi
	dc.b	$2c+logoheight,$07,$ff,$fe	; must no be in conflict with lspline ($2c+36=$50)

	dc.w	$0108,inviswidth
	dc.w	$010a,inviswidth

bplvor	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0

	dc.w	$0100,$2200

clines 	equ $0eee
cfill	equ $0111

	dc.w	$0182,clines
cfill1	dc.w	$0184,$0000	; changed later
	dc.w	$0186,clines	; same as $0182

clines1	dc.w	$0192,$0000	; changed later to value clines
cfill2	dc.w	$0194,$0000	; changed later
	dc.w	$0196,clines	; same as $0192

; Venus
	dc.w	$d501,$fffe
	dc.w	$0100,$5400 ; dual playfield
bplplanet
	dc.w	$00e0,0,$00e2,0 ; bitplane 1
	dc.w	$00e8,0,$00ea,0 ; bitplane 3
	dc.w	$00f0,0,$00f2,0 ; bitplane 5
bplvor2	dc.w	$00e4,0,$00e6,0 ; bitplane 2
	dc.w	$00ec,0,$00ee,0 ; bitplane 4

	dc.w	$0108,$0000 ; no modulo for playfield 1 (planet/Venus)

	if blue

	dc.w	$0182,$0148,$0184,$0147,$0186,$0136,$0188,$0024,$018a,$0012,$018c,$0135,$018e,$0001
	dc.b	$d6,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$0012,$0188,$0148,$018a,$0001,$018c,$0023,$018e,$0135
	dc.b	$d7,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$0149,$0188,$0159,$018a,$016a,$018c,$026b,$018e,$027b
	dc.b	$d8,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$016b,$0186,$015a,$0188,$016a,$018a,$0148,$018c,$0159,$018e,$027b
	dc.b	$d9,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0138,$0186,$016b,$0188,$0148,$018a,$016a,$018c,$0137,$018e,$0149
	dc.b	$da,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$0138,$0188,$0148,$018a,$016b,$018c,$0149,$018e,$0137
	dc.b	$db,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0148,$018a,$0138,$018c,$0159,$018e,$0137
	dc.b	$dc,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016a,$0188,$0138,$018a,$016b,$018c,$0148,$018e,$0159
	dc.b	$dd,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0159,$0188,$0148,$018a,$016a,$018c,$016b,$018e,$0137
	dc.b	$de,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0148,$0188,$0159,$018a,$0138,$018c,$016a,$018e,$016b
	dc.b	$df,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0148,$0188,$0159,$018a,$016b,$018c,$0138,$018e,$016a
	dc.b	$e0,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016a,$0188,$0138,$018a,$0148,$018c,$016b,$018e,$0159
	dc.b	$e1,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016b,$0188,$016a,$018a,$0159,$018c,$0138,$018e,$0137
	dc.b	$e2,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0137,$0188,$016a,$018a,$027b,$018c,$0159,$018e,$0148
	dc.b	$e3,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0148,$0188,$0138,$018a,$016b,$018c,$0159,$018e,$016a
	dc.b	$e4,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0137,$018a,$027b,$018c,$0148,$018e,$0138
	dc.b	$e5,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$0148,$018a,$016a,$018c,$016b,$018e,$0137
	dc.b	$e6,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016a,$0188,$027b,$018a,$016b,$018c,$0148,$018e,$0137
	dc.b	$e7,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016a,$0188,$016b,$018a,$0148,$018c,$027b,$018e,$026b
	dc.b	$e8,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$0148,$018e,$0159
	dc.b	$e9,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$0159,$018e,$0138
	dc.b	$ea,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$0148,$018e,$0159
	dc.b	$eb,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$0159,$018c,$0137,$018e,$016a
	dc.b	$ec,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$0159,$018a,$016a,$018c,$027b,$018e,$027c
	dc.b	$ed,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$016a,$018a,$027b,$018c,$0159,$018e,$0137
	dc.b	$ee,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$016b,$018a,$0137,$018c,$0159,$018e,$027b
	dc.b	$ef,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$0149,$0188,$016b,$018a,$027b,$018c,$0159,$018e,$0137
	dc.b	$f0,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$0149,$0188,$0137,$018a,$027b,$018c,$016b,$018e,$0159
	dc.b	$f1,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$0149,$0188,$0138,$018a,$0148,$018c,$016b,$018e,$027b
	dc.b	$f2,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0137,$0188,$016a,$018a,$0138,$018c,$016b,$018e,$0159
	dc.b	$f3,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0137,$018a,$0148,$018c,$0159,$018e,$016b
	dc.b	$f4,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$016b,$018a,$0137,$018c,$0159,$018e,$0138
	dc.b	$f5,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0159,$0188,$016a,$018a,$016b,$018c,$028c,$018e,$0138
	dc.b	$f6,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016b,$0188,$016a,$018a,$0159,$018c,$0148,$018e,$0138
	dc.b	$f7,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$027b,$018a,$0137,$018c,$0148,$018e,$0159
	dc.b	$f8,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0138,$018a,$0137,$018c,$016b,$018e,$0148
	dc.b	$f9,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0148,$018a,$0138,$018c,$0159,$018e,$016b
	dc.b	$fa,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0138,$0188,$016a,$018a,$016b,$018c,$0159,$018e,$0148
	dc.b	$fb,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0138,$0188,$0148,$018a,$027b,$018c,$016b,$018e,$016a
	dc.b	$fc,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0138,$0188,$0148,$018a,$027b,$018c,$0159,$018e,$016a
	dc.b	$fd,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0159,$0188,$0138,$018a,$0148,$018c,$016b,$018e,$0137
	dc.b	$fe,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0148,$0188,$0138,$018a,$0159,$018c,$0137,$018e,$016a
	dc.b	$ff,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$0159,$0188,$016a,$018a,$0148,$018c,$016b,$018e,$0138
	dc.b	$ff,$df,$ff,$fe ; $00,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0159,$0188,$016a,$018a,$016b,$018c,$0148,$018e,$0138
	dc.b	$01,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$0159,$0188,$016a,$018a,$0138,$018c,$0148,$018e,$016b
	dc.b	$02,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016a,$0188,$0138,$018a,$0159,$018c,$0148,$018e,$016b
	dc.b	$03,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$016a,$018a,$0148,$018c,$0159,$018e,$0138
	dc.b	$04,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$0159,$018a,$0138,$018c,$016b,$018e,$0148
	dc.b	$05,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$016b,$018a,$027b,$018c,$0148,$018e,$0138
	dc.b	$06,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016a,$0188,$016b,$018a,$0137,$018c,$0148,$018e,$027b
	dc.b	$07,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0137,$0186,$016b,$0188,$0149,$018a,$027b,$018c,$016a,$018e,$0148
	dc.b	$08,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$016b,$0188,$0149,$018a,$027b,$018c,$0137,$018e,$0138
	dc.b	$09,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$0137,$0188,$016b,$018a,$0149,$018c,$027b,$018e,$0148
	dc.b	$0a,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016b,$0186,$027b,$0188,$016a,$018a,$0137,$018c,$0149,$018e,$0148
	dc.b	$0b,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$016a,$0188,$0137,$018a,$027b,$018c,$0149,$018e,$0036
	dc.b	$0c,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$027b,$0186,$0137,$0188,$015a,$018a,$016a,$018c,$0149,$018e,$0138
	dc.b	$0d,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$0137,$0188,$016a,$018a,$027b,$018c,$0138,$018e,$0149
	dc.b	$0e,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$0137,$0188,$0149,$018a,$016a,$018c,$027b,$018e,$0138
	dc.b	$0f,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0137,$0186,$016b,$0188,$016a,$018a,$0149,$018c,$027b,$018e,$0138
	dc.b	$10,$07,$ff,$fe
	dc.w	$0182,$016b,$0184,$015a,$0186,$016a,$0188,$0149,$018a,$0137,$018c,$027b,$018e,$0138
	dc.b	$11,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016b,$0188,$016a,$018a,$027b,$018c,$0137,$018e,$0138
	dc.b	$12,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$0137,$018e,$0138
	dc.b	$13,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$016b,$018a,$0138,$018c,$0137,$018e,$027c
	dc.b	$14,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$0148,$018e,$0138
	dc.b	$15,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$027b,$0188,$016b,$018a,$0137,$018c,$016a,$018e,$028c
	dc.b	$16,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$0137,$018e,$027c
	dc.b	$17,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$027c,$018e,$0137
	dc.b	$18,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$016b,$0186,$027b,$0188,$015a,$018a,$016a,$018c,$0148,$018e,$0138
	dc.b	$19,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$0137,$018e,$0148
	dc.b	$1a,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$027c,$018e,$0148
	dc.b	$1b,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$016b,$0186,$015a,$0188,$027b,$018a,$016a,$018c,$0159,$018e,$027c
	dc.b	$1c,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$028c,$018e,$0159
	dc.b	$1d,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$016b,$0188,$027b,$018a,$028c,$018c,$027c,$018e,$016a
	dc.b	$1e,$07,$ff,$fe
	dc.w	$0182,$0149,$0184,$015a,$0186,$016b,$0188,$027b,$018a,$016a,$018c,$028c,$018e,$027c
	dc.b	$1f,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$028c,$018a,$016a,$018c,$027c,$018e,$016b
	dc.b	$20,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$0149,$0186,$027b,$0188,$016b,$018a,$016a,$018c,$028c,$018e,$027c
	dc.b	$21,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$0149,$018a,$016a,$018c,$028c,$018e,$027c
	dc.b	$22,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$016a,$018a,$0149,$018c,$027c,$018e,$028c
	dc.b	$23,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$016a,$018a,$0149,$018c,$028c,$018e,$027c
	dc.b	$24,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016b,$0186,$027b,$0188,$0149,$018a,$016a,$018c,$027c,$018e,$028c
	dc.b	$25,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$016a,$018a,$0149,$018c,$027c,$018e,$0159
	dc.b	$26,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$027c,$018a,$016a,$018c,$0149,$018e,$028c
	dc.b	$27,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$027b,$0186,$016b,$0188,$016a,$018a,$028c,$018c,$0149,$018e,$027c
	dc.b	$28,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016b,$0186,$016a,$0188,$027b,$018a,$027c,$018c,$028c,$018e,$0149
	dc.b	$29,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016a,$0186,$016b,$0188,$027b,$018a,$028c,$018c,$0149,$018e,$027c
	dc.b	$2a,$07,$ff,$fe
	dc.w	$0182,$027b,$0184,$015a,$0186,$016b,$0188,$028c,$018a,$016a,$018c,$027c,$018e,$0149
	dc.b	$2b,$07,$ff,$fe
	dc.w	$0182,$015a,$0184,$016b,$0186,$027b,$0188,$028c,$018a,$027c,$018c,$016a,$018e,$0149
 
 	else

	dc.w	$0182,$0841,$0184,$0741,$0186,$0631,$0188,$0420,$018a,$0210,$018c,$0531,$018e,$0100
	dc.b	$d6,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0210,$0188,$0841,$018a,$0100,$018c,$0320,$018e,$0531
	dc.b	$d7,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0941,$0188,$0951,$018a,$0a61,$018c,$0b62,$018e,$0b72
	dc.b	$d8,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0b61,$0186,$0a51,$0188,$0a61,$018a,$0841,$018c,$0951,$018e,$0b72
	dc.b	$d9,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0831,$0186,$0b61,$0188,$0841,$018a,$0a61,$018c,$0731,$018e,$0941
	dc.b	$da,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0831,$0188,$0841,$018a,$0b61,$018c,$0941,$018e,$0731
	dc.b	$db,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0841,$018a,$0831,$018c,$0951,$018e,$0731
	dc.b	$dc,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0a61,$0188,$0831,$018a,$0b61,$018c,$0841,$018e,$0951
	dc.b	$dd,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0951,$0188,$0841,$018a,$0a61,$018c,$0b61,$018e,$0731
	dc.b	$de,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0841,$0188,$0951,$018a,$0831,$018c,$0a61,$018e,$0b61
	dc.b	$df,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0841,$0188,$0951,$018a,$0b61,$018c,$0831,$018e,$0a61
	dc.b	$e0,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0a61,$0188,$0831,$018a,$0841,$018c,$0b61,$018e,$0951
	dc.b	$e1,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b61,$0188,$0a61,$018a,$0951,$018c,$0831,$018e,$0731
	dc.b	$e2,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0731,$0188,$0a61,$018a,$0b72,$018c,$0951,$018e,$0841
	dc.b	$e3,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0841,$0188,$0831,$018a,$0b61,$018c,$0951,$018e,$0a61
	dc.b	$e4,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0731,$018a,$0b72,$018c,$0841,$018e,$0831
	dc.b	$e5,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0841,$018a,$0a61,$018c,$0b61,$018e,$0731
	dc.b	$e6,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0a61,$0188,$0b72,$018a,$0b61,$018c,$0841,$018e,$0731
	dc.b	$e7,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0a61,$0188,$0b61,$018a,$0841,$018c,$0b72,$018e,$0b62
	dc.b	$e8,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0841,$018e,$0951
	dc.b	$e9,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0951,$018e,$0831
	dc.b	$ea,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0841,$018e,$0951
	dc.b	$eb,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0951,$018c,$0731,$018e,$0a61
	dc.b	$ec,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0951,$018a,$0a61,$018c,$0b72,$018e,$0c72
	dc.b	$ed,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0a61,$018a,$0b72,$018c,$0951,$018e,$0731
	dc.b	$ee,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0b61,$018a,$0731,$018c,$0951,$018e,$0b72
	dc.b	$ef,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0941,$0188,$0b61,$018a,$0b72,$018c,$0951,$018e,$0731
	dc.b	$f0,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0941,$0188,$0731,$018a,$0b72,$018c,$0b61,$018e,$0951
	dc.b	$f1,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0941,$0188,$0831,$018a,$0841,$018c,$0b61,$018e,$0b72
	dc.b	$f2,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0731,$0188,$0a61,$018a,$0831,$018c,$0b61,$018e,$0951
	dc.b	$f3,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0731,$018a,$0841,$018c,$0951,$018e,$0b61
	dc.b	$f4,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0b61,$018a,$0731,$018c,$0951,$018e,$0831
	dc.b	$f5,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0951,$0188,$0a61,$018a,$0b61,$018c,$0c82,$018e,$0831
	dc.b	$f6,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b61,$0188,$0a61,$018a,$0951,$018c,$0841,$018e,$0831
	dc.b	$f7,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0b72,$018a,$0731,$018c,$0841,$018e,$0951
	dc.b	$f8,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0831,$018a,$0731,$018c,$0b61,$018e,$0841
	dc.b	$f9,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0841,$018a,$0831,$018c,$0951,$018e,$0b61
	dc.b	$fa,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0831,$0188,$0a61,$018a,$0b61,$018c,$0951,$018e,$0841
	dc.b	$fb,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0831,$0188,$0841,$018a,$0b72,$018c,$0b61,$018e,$0a61
	dc.b	$fc,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0831,$0188,$0841,$018a,$0b72,$018c,$0951,$018e,$0a61
	dc.b	$fd,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0951,$0188,$0831,$018a,$0841,$018c,$0b61,$018e,$0731
	dc.b	$fe,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0841,$0188,$0831,$018a,$0951,$018c,$0731,$018e,$0a61
	dc.b	$ff,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0951,$0188,$0a61,$018a,$0841,$018c,$0b61,$018e,$0831
	dc.b	$ff,$df,$ff,$fe ; $00,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0951,$0188,$0a61,$018a,$0b61,$018c,$0841,$018e,$0831
	dc.b	$01,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0951,$0188,$0a61,$018a,$0831,$018c,$0841,$018e,$0b61
	dc.b	$02,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0a61,$0188,$0831,$018a,$0951,$018c,$0841,$018e,$0b61
	dc.b	$03,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0a61,$018a,$0841,$018c,$0951,$018e,$0831
	dc.b	$04,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0951,$018a,$0831,$018c,$0b61,$018e,$0841
	dc.b	$05,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0b61,$018a,$0b72,$018c,$0841,$018e,$0831
	dc.b	$06,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0a61,$0188,$0b61,$018a,$0731,$018c,$0841,$018e,$0b72
	dc.b	$07,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0731,$0186,$0b61,$0188,$0941,$018a,$0b72,$018c,$0a61,$018e,$0841
	dc.b	$08,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0b61,$0188,$0941,$018a,$0b72,$018c,$0731,$018e,$0831
	dc.b	$09,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0731,$0188,$0b61,$018a,$0941,$018c,$0b72,$018e,$0841
	dc.b	$0a,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b61,$0186,$0b72,$0188,$0a61,$018a,$0731,$018c,$0941,$018e,$0841
	dc.b	$0b,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0a61,$0188,$0731,$018a,$0b72,$018c,$0941,$018e,$0630
	dc.b	$0c,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0b72,$0186,$0731,$0188,$0a51,$018a,$0a61,$018c,$0941,$018e,$0831
	dc.b	$0d,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0731,$0188,$0a61,$018a,$0b72,$018c,$0831,$018e,$0941
	dc.b	$0e,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0731,$0188,$0941,$018a,$0a61,$018c,$0b72,$018e,$0831
	dc.b	$0f,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0731,$0186,$0b61,$0188,$0a61,$018a,$0941,$018c,$0b72,$018e,$0831
	dc.b	$10,$07,$ff,$fe
	dc.w	$0182,$0b61,$0184,$0a51,$0186,$0a61,$0188,$0941,$018a,$0731,$018c,$0b72,$018e,$0831
	dc.b	$11,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b61,$0188,$0a61,$018a,$0b72,$018c,$0731,$018e,$0831
	dc.b	$12,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0731,$018e,$0831
	dc.b	$13,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0b61,$018a,$0831,$018c,$0731,$018e,$0c72
	dc.b	$14,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0841,$018e,$0831
	dc.b	$15,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b72,$0188,$0b61,$018a,$0731,$018c,$0a61,$018e,$0c82
	dc.b	$16,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0731,$018e,$0c72
	dc.b	$17,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0c72,$018e,$0731
	dc.b	$18,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0b61,$0186,$0b72,$0188,$0a51,$018a,$0a61,$018c,$0841,$018e,$0831
	dc.b	$19,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0731,$018e,$0841
	dc.b	$1a,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0c72,$018e,$0841
	dc.b	$1b,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0b61,$0186,$0a51,$0188,$0b72,$018a,$0a61,$018c,$0951,$018e,$0c72
	dc.b	$1c,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0c82,$018e,$0951
	dc.b	$1d,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b61,$0188,$0b72,$018a,$0c82,$018c,$0c72,$018e,$0a61
	dc.b	$1e,$07,$ff,$fe
	dc.w	$0182,$0941,$0184,$0a51,$0186,$0b61,$0188,$0b72,$018a,$0a61,$018c,$0c82,$018e,$0c72
	dc.b	$1f,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0c82,$018a,$0a61,$018c,$0c72,$018e,$0b61
	dc.b	$20,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0941,$0186,$0b72,$0188,$0b61,$018a,$0a61,$018c,$0c82,$018e,$0c72
	dc.b	$21,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0941,$018a,$0a61,$018c,$0c82,$018e,$0c72
	dc.b	$22,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0a61,$018a,$0941,$018c,$0c72,$018e,$0c82
	dc.b	$23,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0a61,$018a,$0941,$018c,$0c82,$018e,$0c72
	dc.b	$24,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b61,$0186,$0b72,$0188,$0941,$018a,$0a61,$018c,$0c72,$018e,$0c82
	dc.b	$25,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0a61,$018a,$0941,$018c,$0c72,$018e,$0951
	dc.b	$26,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0c72,$018a,$0a61,$018c,$0941,$018e,$0c82
	dc.b	$27,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b72,$0186,$0b61,$0188,$0a61,$018a,$0c82,$018c,$0941,$018e,$0c72
	dc.b	$28,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b61,$0186,$0a61,$0188,$0b72,$018a,$0c72,$018c,$0c82,$018e,$0941
	dc.b	$29,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0a61,$0186,$0b61,$0188,$0b72,$018a,$0c82,$018c,$0941,$018e,$0c72
	dc.b	$2a,$07,$ff,$fe
	dc.w	$0182,$0b72,$0184,$0a51,$0186,$0b61,$0188,$0c82,$018a,$0a61,$018c,$0c72,$018e,$0941
	dc.b	$2b,$07,$ff,$fe
	dc.w	$0182,$0a51,$0184,$0b61,$0186,$0b72,$0188,$0c82,$018a,$0c72,$018c,$0a61,$018e,$0941
	endif

	dc.w	$ffff,$fffe
clist1end


*------	COPPER INSTRUCTION LIST 3 ---------------------------------------------*

clist3	dc.w	$1007,$fffe ; chance for player to alter clist in time

spritepointersclist3
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c71 ; DIWSTRT normal=$2c81
	dc.w	$0090,$2cd1 ; DIWSTOP normal=$2cc1
	dc.w	$0092,$0030 ; DDFSTRT normal=$0038 wide=$0030
	dc.w	$0094,$00d8 ; DDFSTOP normal=$00d0 wide=$00d8

	dc.w	$0100,$1200
	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,inviswidth
	dc.w	$010a,$0000 ; text bitplane

bplvor3	dc.w	$00e0,0,$00e2,0
textplanep
	dc.w	$00e4,0,$00e6,0

bgcl3	dc.w	$0180,$0666
	dc.w	$0182,clines
	dc.w	$0184,$0fff
	dc.w	$0186,$0fff

	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe
lspdmacon3
	dc.b	$00,$96,$80,0

	dc.w	$8f07,$fffe
	dc.w	$0100,$2200
	dc.w	$9f07,$fffe
	dc.w	$0100,$1200

	dc.w	$ffff,$fffe
clist3end


*------	COPPER INSTRUCTION LIST 2 ---------------------------------------------*

; boing colors
colorb1	equ	$0000 ; $07cd
colorb2	equ	$0000 ; $049a shared with csb2*
colorb3	equ	$0000 ; $0023

colort	equ	$0000 ; text $08bc
colorp	equ	$0000 ; person $09ef shared with csd*

clist2	dc.w	$1007,$fffe ; chance for player to alter clist in time
	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,$0000
	dc.w	$010a,$0000

spritepointersclist2
	dc.w	$013c,0,$013e,0,$0138,0,$013a,0,$0134,0,$0136,0
	dc.w	$0130,0,$0132,0,$012c,0,$012e,0,$0128,0,$012a,0
	dc.w	$0124,0,$0126,0,$0120,0,$0122,0

	; sprite (stars) colors
	dc.w	$01a0,$0000 ; transparent
csa1	dc.w	$01a2,$0000 ; $0167
csb1	dc.w	$01a4,$0000 ; $049a
csc1	dc.w	$01a6,$0000 ; $08bc
	dc.w	$01a8,$0000 ; transparent
csa2	dc.w	$01aa,$0000 ; $0167
csb2	dc.w	$01ac,$0000 ; $049a
	dc.w	$01ae,$0000 ; not used
	dc.w	$01b0,$0000 ; transparent
csd1	dc.w	$01b2,$0000 ; $09ef
	dc.w	$01b4,$0000 ; not used
	dc.w	$01b6,$0000 ; not used
	dc.w	$01b8,$0000 ; transparent
	dc.w	$01ba,$0000 ; not used
	dc.w	$01bc,$0000 ; not used
	dc.w	$01be,$0000 ; not used

	rem
	dc.w	$01a0,$0000,$01a2,$0334,$01a4,$0556,$01a6,$0778
	dc.w	$01a8,$0000,$01aa,$0334,$01ac,$0556,$01ae,$0f00

	dc.w	$01b0,$0000,$01b2,$099a,$01b4,$00f0,$01b6,$0f00
	dc.w	$01b8,$0000,$01ba,$00f0,$01bc,$00f0,$01be,$0f00
	erem

	dc.w	$0100,$2200
bplmccoy1
	dc.w	$00e8,0,$00ea,0 ; bitplane 3
	dc.w	$00ec,0,$00ee,0 ; bitplane 4

	dc.w	$0180,$0000
cp1	dc.w	$0182,colorp
	dc.w	$0184,$0000
cp2	dc.w	$0186,colorp

cb3a	dc.w	$0188,colorb3 ; boing (test with $0f00)
cp3	dc.w	$018a,colorp
	dc.w	$018c,$0000
cp4	dc.w	$018e,colorp

cb1a	dc.w	$0190,colorb1 ; boing (test with $0ff0)
cp5	dc.w	$0192,colorp
	dc.w	$0194,$0000 ; overlapping legs
cp6	dc.w	$0196,colorp

cb2a	dc.w	$0198,colorb2 ; boing small stripe (test with $00f0)
cp7	dc.w	$019a,colorp
	dc.w	$019c,$0000 ; overlapping legs
cp8	dc.w	$019e,colorp

	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe
lspdmacon2
	dc.b	$00,$96,$80,0

	dc.w	$7b07,$fffe ; boing with overlapping person
	dc.w	$0100,$4200

	dc.b	$7b+boingheight/2,$07,$ff,$fe ; boing lower half
bplmccoy3
	dc.w	$00e0,0,$00e2,0 ; bitplane 1
	dc.w	$00e4,0,$00e6,0 ; bitplane 2
	dc.w	$0100,$2200
cb3b	dc.w	$0182,colorb3
cb1b	dc.w	$0184,colorb1
cb2b	dc.w	$0186,colorb2

	dc.b	$7b+boingheight,$07,$ff,$fe ; BLANK between boing and text
	dc.w	$0100,$1200
	dc.w	$0108,-mccoywidth
	dc.w	$0182,$0000
	dc.w	$0104,$0024 ; sprites have priority

	dc.w	$e207,$fffe ; SPREADING THE NEWS
bplmccoy2
	dc.w	$00e0,0,$00e2,0 ; bitplane 1
	dc.w	$0108,$0000
	dc.w	$0104,$0000
ct1	dc.w	$0182,colort
	dc.b	$e2+14,$07,$ff,$fe

	dc.w	$0108,-mccoywidth ; BLANK
	dc.w	$0182,$0000
	dc.w	$0104,$0024

	dc.b	$e2+14+5,$07,$ff,$fe ; AROUND THE WORLD
ct2	dc.w	$0182,colort
	dc.w	$0108,$0000

	dc.w	$ffdf,$fffe
	dc.b	($e2+14+5+14)&$ff,$07,$ff,$fe ; BLANK
	dc.w	$0108,-mccoywidth
	dc.w	$0182,$0000
	dc.w	$0104,$0024

	dc.b	($e2+14+5+14+5)&$ff,$07,$ff,$fe ; MCCOY
ct3	dc.w	$0182,colort
	dc.w	$0108,$0000
	
	dc.b	($e2+14+5+14+5+5)&$ff,$07,$ff,$fe ; BLANK
	dc.w	$0108,-mccoywidth
	dc.w	$0182,$0000
	dc.w	$0104,$0024

	dc.w	$ffff,$fffe
clist2end


*------	COPPER INSTRUCTION CIRC-WAVE 2 ----------------------------------------*

textline	equ	$08
textypos	equ	220
textheight	equ	13
clistcw2
	dc.w	$fe07,$fffe
	dc.w	$0100,$0200
colscw2	dc.w	$0180,0,$0182,0
	dc.w	$ffdf,$fffe
	dc.b	textline,$07,$ff,$fe
bplcw2	dc.w	$00e0,0,$00e2,0 ; text
	dc.w	$0100,$1200
	dc.b	textline+textheight,$07,$ff,$fe
	dc.w	$0100,$0200
	dc.w	$ffff,$fffe
clistcw2end


*------	VORONOI ---------------------------------------------------------------*

drawvoronoi
	move.l	v_actors(a5),d7			; process actors
	btst	#actor_plateau,d7		;
	beq	.done				;

	bsr	mtx				;
	movem.w	v_c1offset(a5),d0-d2		;
	add.w	d0,v_c1(a5)			;
	add.w	d1,v_c2(a5)			;
	add.w	d2,v_c3(a5)			;

	move.l	v_datapointer(a5),a3		; draw voronoi
	moveq	#0,d7				;
	move.b	(a3)+,d7			; a3 = base of points

	movem.l	a3/d7,-(a7)			;
	asr.w	#1,d7				;
	subq.w	#1,d7				; adjust for dbf
	lea	v_2d(a5),a2			; destination
	bsr	applymtx			; a3 = 3d data, d7 = num vertices

	lea	frame(pc),a3			;
	moveq	#4-1,d7				; 4 vertices
	lea	v_2dframe(a5),a2		; destination
	bsr	applymtx			; a3 = 3d data, d7 = num vertices

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_fill,d7			;
	beq	.nofill				;
	moveq	#4,d5				; draw frame with 4 lines
	lea	frameindices(pc),a2		;
	lea	v_2dframe(a5),a1		;
	move.l	v_dbplane2b(a5),a0		;
	add.w	#pheight/2*pwidth+center-voradjust,a0 ; center
	bsr	flines				;
	move.l	v_dbplane2b(a5),a0		;
	add.w	#psize-inviswidth-2-voradjust,a0 ; fills from bottom to top (why -2)
	bsr	fill				;
	bsr	waitblitter			;
.nofill	moveq	#4,d5				; stroke frame with 4 lines
	lea	frameindices(pc),a2		;
	lea	v_2dframe(a5),a1		;
	bsr	lines				;

	movem.l	(a7)+,a2/d5			;

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_voronoi,d7		;
	beq	.done				;

	add.l	d5,a2				; skip points: go to connections
	move.b	(a2)+,d5			; num connections (line index pairs)
	lea	v_2d(a5),a1			;
	bsr	lines				;

	move.l	v_voronoi(a5),a3		;
	add.l	#voronoidecrunchedsize,a3	;
	cmp.l	a3,a2				;
	blt	.noteod				;
	move.l	v_voronoi(a5),a2		; end of data - reset
.noteod	move.l	a2,v_datapointer(a5)		;
.done	rts					;


*------	SLOW LINE DRAWER ------------------------------------------------------*

; a1 = 2d data
; a2 = indices
; d5 = num lines
lines	move.l	v_dbplane1b(a5),a0		;
	add.w	v_yoffset(a5),a0		;

	lea	$52(a6),a6			;
	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

.loop	moveq	#0,d6				;
	move.b	(a2)+,d6			; index p1
	movem.w	(a1,d6.w),d0/d1			; x1 y1
	move.b	(a2)+,d6			; index p2
	movem.w	(a1,d6.w),d2/d3			; x2 y2

	cmp.w	d3,d1				; compare y
	bgt	.noswap				; (draw the same way as the "filled" lines)
	exg	d0,d2				; swap -> second larger
	exg	d1,d3				;
.noswap	moveq	#4,d7				; moveq clears d7's upper word
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0bca,d7			;
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; octant
	asl.w	#6,d1				; = muls #pwidth,d1

	lea	(a0,d1.w),a4			;
	asr.w	#3,d0				;
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; set starting address
	move.w	d2,(a3)				; start

	subq.b	#1,d5				;
	bne	.loop				;

	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;

.octs	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------	SLOW LINE DRAWER FOR FILLED VECTORS -----------------------------------*

; a1 = 2d data
; a2 = indices
flines	lea	$52(a6),a6			;
	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

.loop	moveq	#0,d6				;
	move.b	(a2)+,d6			; index p1
	movem.w	(a1,d6.w),d0/d1			; x1 y1
	move.b	(a2)+,d6			; index p2
	movem.w	(a1,d6.w),d2/d3			; x2 y2
	
	cmp.w	d3,d1				; compare y
	bgt	.noswap				;
	exg	d0,d2				; swap -> second larger
	exg	d1,d3				;
.noswap	moveq	#4,d7				; clears upper word too
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0b4a,d7			; $0bca=or
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; read octant from table
	asl.w	#6,d1				; = muls #pwidth,d1

	move.l	a0,a4				; bitplane
	add.w	d1,a4				;
	moveq	#7,d1				;
	sub.w	d0,d1				; see bchg below
	asr.w	#3,d0				; allow neg x coords
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;

	move.l	d3,$62-$52(a6)			; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; start address
	bchg	d1,(a4)				; flip pixel (important for filling)
	move.w	d2,(a3)				; start

	subq.b	#1,d5				;
	bne	.loop				;

	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;

; +2 = SING Single bit per horizontal line for use with subsequent area fill
.octs	dc.b	0*4+1+2, 2*4+1+2, 1*4+1+2, 3*4+1+2
	dc.b	4*4+1+2, 5*4+1+2, 6*4+1+2, 7*4+1+2
	dc.b	0*4+65+2, 2*4+65+2, 1*4+65+2, 3*4+65+2
	dc.b	4*4+65+2, 5*4+65+2, 6*4+65+2, 7*4+65+2


*------	CLEAR SCREEN ----------------------------------------------------------*

; note: clears 2 bitplanes (1 and 2)
cls	bsr	waitblitter			;
	move.w	#inviswidth,$66(a6)		; modulo D
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	v_dbplane1b(a5),$54(a6)		; destination D
	move.w	v_bltsize(a5),$58(a6)		; bltsize and start
	rts					;


*------	FILL ------------------------------------------------------------------*

fillsub	equ	8
fill	subq.w	#fillsub/2,a0			; fill plateau only
	moveq	#inviswidth+fillsub,d0		;
	moveq	#-1,d1				;
	move.w	#(pheight-yfillmargin)<<6+(pwidth-inviswidth-fillsub)>>1,d2 ;
	bsr	waitblitter			;
	move.w	d0,$64(a6)			; modulo A
	move.w	d0,$66(a6)			; modulo D
	move.l	#$09f00012,$40(a6)		; 09f00012 exlusive, 09f0000a inclusive
	move.l	d1,$44(a6)			; first/last word mask
	move.l	a0,$50(a6)			; source A
	move.l	a0,$54(a6)			; destination D
	move.w	d2,$58(a6)			; bltsize and go
	rts					;

	rem
fill	moveq	#inviswidth,d0			;
	moveq	#-1,d1				;
	move.w	#(pheight-yfillmargin)<<6+(pwidth-inviswidth)>>1,d2 ;
	bsr	waitblitter			;
	move.w	d0,$64(a6)			; modulo A
	move.w	d0,$66(a6)			; modulo D
	move.l	#$09f00012,$40(a6)		; 09f00012 exlusive, 09f0000a inclusive
	move.l	d1,$44(a6)			; first/last word mask
	move.l	a0,$50(a6)			; source A
	move.l	a0,$54(a6)			; destination D
	move.w	d2,$58(a6)			; bltsize and go
	rts					;
	erem


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clist1size	equ	clist1end-clist1
clist2size	equ	clist2end-clist2
clist3size	equ	clist3end-clist3
lspbanksize	equ	lspbankend-lspbank
logosize	equ	logoend-logo
planetsize	equ	planetend-planet
spritedatasize	equ	spritedataend-spritedata
coverdatasize	equ	(coverdataend-coverdata)/6*8*numcoverrows
boingsize	equ	boingend-boing
crunchsize	equ	crunchend-crunch
skysize		equ	skyend-sky
mccoysize	equ	mccoyend-mccoy
voronoisize	equ	voronoiend-voronoi
voronoidecrunchedsize	equ	89522
clistcw2size	equ	clistcw2end-clistcw2

memtable
b_clist1	dc.l	0,MEMF_CHIP,clist1size
b_clist2	dc.l	0,MEMF_CHIP,clist2size
b_clist3	dc.l	0,MEMF_CHIP,clist3size
b_lspbank	dc.l	0,MEMF_CHIP,lspbanksize
b_logo		dc.l	0,MEMF_CHIP,logosize
b_planet	dc.l	0,MEMF_CHIP,planetsize
b_spritedata	dc.l	0,MEMF_CHIP,spritedatasize
b_boing		dc.l	0,MEMF_CHIP,boingsize
b_crunch	dc.l	0,MEMF_CHIP,crunchsize
b_clistcw2	dc.l	0,MEMF_CHIP,clistcw2size

memtable2
;b_bitplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,safezone+4*psize ; = $11900 (71936) -> reusing (b_)boing (109760)
b_sintab	dc.l	0,BESTMEMORY,2560
b_coverdata	dc.l	0,BESTMEMORY,coverdatasize
b_mccoyplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,2*mccoypsize
b_clistcw1	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clistcw1size+clistcw1extrasize

;b_testoutofmem	dc.l	0,MEMF_CHIP,600000
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

alloc	lea	clist1(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	clist1(pc),a0			; mark data that is in chipmen already
	lea	b_clist1(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist2(pc),a0			;
	lea	b_clist2(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist3(pc),a0			;
	lea	b_clist3(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;

	lea	logo(pc),a0			;
	lea	b_logo(pc),a1			;
	move.l	a0,(a1)				;
	
	lea	planet(pc),a0			;
	lea	b_planet(pc),a1			;
	move.l	a0,(a1)				;

	lea	spritedata(pc),a0		;
	lea	b_spritedata(pc),a1		;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#boing-base,a0			;
	lea	b_boing(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#crunch-base,a0			;
	lea	b_crunch(pc),a1			;
	move.l	a0,(a1)				;
	
	lea	clistcw2(pc),a0			;
	lea	b_clistcw2(pc),a1		;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	initmemory			;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

initmemory
	lea	vars(pc),a5			;
	
	lea	base(pc),a0			; copy "sky", "voronoi", "mccoy"
	add.l	#crunch-base,a0			;
	move.l	b_crunch(pc),a1			;
	move.l	a1,a2				;
	move.l	a2,v_sky(a5)			;
	add.l	#skysize,a2			;
	move.l	a2,v_voronoi(a5)		;
	add.l	#voronoisize,a2			;
	move.l	a2,v_mccoy(a5)			;
	
	move.l	#crunchsize,d7			;
.copyc	move.b	(a0)+,(a1)+			;
	subq.l	#1,d7				;
	bne	.copyc				;

	move.l	b_mccoyplanes(pc),a0		;
	lea	v_db1a(a5),a1			;
	move.l	a0,(a1)+			;
	add.w	#mccoypsize,a0			;
	move.l	a0,(a1)				;

	lea	base(pc),a1			; init punks
	move.l	a1,d1				;
	lea	punks(pc),a0			;
	moveq	#(punksend-punks)/4-1,d7	;
.ploop	add.l	d1,(a0)+			;
	dbf	d7,.ploop			;

	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;

	lea	clistcw1(pc),a0			; copy clist cw 1 to chip memory
	move.l	b_clistcw1(pc),a1		;
	moveq	#clistcw1size/2-1,d7		;
.cpcl	move.w	(a0)+,(a1)+			;
	dbf	d7,.cpcl			;
	move.l	#$008a0000,(a1)+		; valid end of clist cw 1

	lea	clistcw2(pc),a0			; copy clist cw 2 to chip memory
	move.l	b_clistcw2(pc),a1		;
	moveq	#clistcw2size/2-1,d7		;
.cpcl2	move.w	(a0)+,(a1)+			;
	dbf	d7,.cpcl2			;

	; init common clist crap

	lea	clist1(pc),a2			;
	lea	logo(pc),a0			; copy logo to chip memory
	move.l	b_logo(pc),a1			; and init logo bitplanes
	move.l	a1,d0				;
	move.w	#logosize-1,d7			;
.copyl	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyl			;
	add.w	#bpllogo-clist1+2+4,a2		; init logo bitplanes
	moveq	#4-1,d7				; 4 bitplanes
.initbpl
	move.w	d0,(a2)				;
	swap	d0				;
	move.w	d0,-4(a2)			;
	swap	d0				;
	add.l	#logosize/4,d0			;
	addq.w	#8,a2				;
	dbf	d7,.initbpl			;

	lea	clist1(pc),a2			;
	lea	planet(pc),a0			; copy planet to chip memory
	move.l	b_planet(pc),a1			; and init planet bitplanes
	move.l	a1,d0				;
	move.w	#planetsize-1,d7		;
.copyp	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyp			;
	add.w	#bplplanet-clist1+2+4,a2	; init planet bitplanes
	moveq	#3-1,d7				; 3 bitplanes
.initbpl2
	move.w	d0,(a2)				;
	swap	d0				;
	move.w	d0,-4(a2)			;
	swap	d0				;
	add.l	#planetsize/3,d0		;
	addq.w	#8,a2				;
	dbf	d7,.initbpl2			;

	lea	clist1(pc),a0			; copy clist 1 to chip memory
	move.l	b_clist1(pc),a1			;
	move.w	#clist1size-1,d7		;
.copyc1	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc1			;

	lea	clist2(pc),a0			; copy clist 2 to chip memory
	move.l	b_clist2(pc),a1			;
	move.w	#clist2size-1,d7		;
.copyc2	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc2			;

	lea	clist3(pc),a0			; copy clist 3 to chip memory
	move.l	b_clist3(pc),a1			;
	move.w	#clist3size-1,d7		;
.copyc3	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc3			;

	lea	spritedata(pc),a0		; copy sprite data to chip memory
	move.l	b_spritedata(pc),a1		;
	move.w	#spritedatasize-1,d7		;
.copysd	move.b	(a0)+,(a1)+			;
	dbf	d7,.copysd			;

	lea	base(pc),a0			; copy boing
	add.l	#boing-base,a0			;
	move.l	b_boing(pc),a1			;
	move.l	#boingsize,d7			;
.copyb	move.b	(a0)+,(a1)+			;
	subq.l	#1,d7				;
	bne	.copyb				;

	move.l	v_mccoy(a5),d0			;
	move.l	b_clist2(pc),a1			; init bitplane pointer
	move.w	d0,bplmccoy2-clist2+2+4(a1)	;
	swap	d0				;	
	move.w	d0,bplmccoy2-clist2+2(a1)	;

	move.l	b_clist1(pc),a0			; sprites clist 1
	add.w	#spritepointersclist1+2-clist1,a0 ;
	move.l	b_spritedata(pc),d0		;
	bsr	setspritepointers		;

	move.l	b_clist3(pc),a0			; sprites clist 3
	add.w	#spritepointersclist3+2-clist3,a0 ;
	move.l	b_spritedata(pc),d0		;
	bsr	setspritepointers		;

	move.l	b_clistcw1(pc),a0		; sprites clist cw 1
	add.w	#spritepointersclistcw+2-clistcw1,a0 ;
	move.l	b_spritedata(pc),d0		;
	bsr	setspritepointers		;

	bsr	generatesintab			; sintab

	lea	orbits(pc),a0			; init orbits
	lea	v_orbits(a5),a1			;
	moveq	#0,d1				;
	moveq	#numorbits-1,d7			;
.inito	move.l	(a0)+,d0			;
	add.l	d1,d0				;
	move.l	d0,(a1)+			; data start
	move.l	(a0)+,(a1)+			; data end
;	add.l	#8*3,d1				; offset/delay
	dbf	d7,.inito			;

	bsr	initcoverdata			;
	rts					;


*------	HANDLE PREPARE MAIN ---------------------------------------------------*

handlepreparemain
	move.l	v_actors(a5),d7			;
	btst	#actor_prepare_main,d7		;
	beq	.no				;

	move.l	v_voronoi(a5),a0		; crunched voronoi data
	move.l	b_boing(pc),a1			;
	bsr	lzodecrunch			;
	move.l	b_boing(pc),v_voronoi(a5)	; uncrunched voronoi data

	move.l	v_voronoi(a5),v_datapointer(a5)	;

	move.l	b_crunch(pc),v_bitplanes(a5)	; reusing sky and voronoi crunched data as bitplane memory

	move.l	v_bitplanes(a5),a0		; init db bitplane pointers
	add.w	#safezone,a0			;
	lea	v_dbplane1a(a5),a1		;
	moveq	#numplanes*2-1,d7		;
.initplanes
	move.l	a0,(a1)+			;
	add.w	#psize,a0			;
	dbf	d7,.initplanes			;

	move.l	v_bitplanes(a5),a1		; clear bitplanes
	move.w	#(safezone+4*psize)/4-1,d7	;
	moveq	#0,d0				;
.clear	move.l	d0,(a1)+			;
	dbf	d7,.clear			;

	move.l	v_dbplane2a(a5),a1		; fill cover plane (black voronoi plane)
	move.l	v_dbplane2b(a5),a2		;
	add.w	#((pheight-logoheight)-(numcoverrows*coverrowheight))*pwidth,a1 ;
	add.w	#((pheight-logoheight)-(numcoverrows*coverrowheight))*pwidth,a2 ;
	moveq	#-1,d0				; data ("black")
	move.w	#numcoverrows*coverrowheight*pwidth/4-1,d7
.fill	move.l	d0,(a1)+			;
	move.l	d0,(a2)+			;
	dbf	d7,.fill			;

	if testing
	move.w	v_frame(a5),v_number(a5)	;
	endif

	STOPACTOR actor_prepare_main		;
.no	rts					;


*------	CIRC WAVE BY BIG BROTHER ----------------------------------------------*

	include	"circ-wave/circ-wave.s"
	even


*------	HANDLE PREPARE CIRC-WAVE ----------------------------------------------*

handlepreparecircwave
	move.l	v_actors(a5),d7			;
	btst	#actor_prepare_circwave,d7	;
	beq	.no				;

	move.l	b_boing(pc),a1			; reuse boing chip memory

	move.l	a1,v_ball(a5)			; set bob gfx address
	move.w	#%0111000000000000,d0		; init bob gfx
	move.w	#%1111100000000000,d1		;
	move.w	d0,(a1)+			;
	move.w	d1,(a1)+			;
	move.w	d1,(a1)+			;
	move.w	d1,(a1)+			;
	move.w	d0,(a1)+			;

	move.l	a1,v_dbcwplane1a(a5)		; db bitplane pointers
	move.l	a1,v_cwplanetext(a5)		;
	move.l	a1,a2				;
	add.w	#cwpsize,a1			;
	move.l	a1,v_dbcwplane1b(a5)		;

	move.w	#2*cwpsize/4-1,d7		; clear bitplanes
	moveq	#0,d0				;
.clrbp	move.l	d0,(a2)+			;
	dbf	d7,.clrbp			;

	lea	copperylst(pc),a0		; init ptr to copper colors y pos
	move.l	a0,v_clcolptr(a5)		;

	lea	base(pc),a0			; init arches list pointers
	add.l	#archlst-base,a0		;
	lea	arch00(pc),a1			;
	move.l	a1,d1				;
	moveq	#nrarches-1,d0			;
.arloop	add.l	d1,(a0)+			;
	dbf	d0,.arloop			;

	lea	even_idx(pc),a0			;
	move.l	v_dbcwplane1a(a5),a4		; a4 = param for drawbob
	bsr	drawfullimage			; draw arches for even frames nbs
	lea	odd_idx(pc),a0			;
	move.l	v_dbcwplane1b(a5),a4		; a4 = param for drawbob
	bsr	drawfullimage			; draw arch for the odd frame nbs

	; now active plane is filled with data from even and buffer contains data from odd

	moveq	#cwpwidth,d5			;

	move.l	v_cwplanetext(a5),a1		; text SPREADPOINT
	add.w	#textypos*cwpwidth+11,a1	;
	moveq	#%100000,d4			;
	bsr	printtext			;

	move.l	v_cwplanetext(a5),a1		; text PRESENTS
	add.w	#(textypos+textheight)*cwpwidth+13,a1 ;
	moveq	#%100000,d4			;
	bsr	printtext			;

	if testing
	move.w	v_frame(a5),v_number(a5)	;
	endif

	STOPACTOR actor_prepare_circwave	;
.no	rts					;


*------	BIFAT'S LZO DECRUNCHER ------------------------------------------------*

;	raw LZO Decruncher for MC68000
;	2008, 2010, 2015 by bifat/tek
;
;	Note that the cruncher produces a 4 byte header, $b0 followed by the
;	24bit length of the decrunched data. Strip off or skip these first
;	four bytes from the crunched data when passing them to this routine.
;
;	In-place decrunching: Align the crunched data to the end of the
;	destination area PLUS overhead.
;	 _________________________________________________
;	|                   |                           : |
;	|    destination    |       crunched data       : |
;	|___________________|___________________________:_|
;	^-- destptr (a1)    ^-- srcptr (a0)              ^- overhead
;

		; a0	inp
		; a1	outp

lzodecrunch
;		movem.l	d2-d6/a2-a3,-(a7)

		moveq	#0,d3
		st	d3		; 000000ff
		moveq	#15,d4
		moveq	#3,d5
		moveq	#63,d6
		lea	$4000.w,a3
		
		moveq	#0,d0
		moveq	#0,d1
		moveq	#17,d2
		
		move.b	(a0)+,d1
		cmp.w	d2,d1
		ble.b	.loop2
		
		sub.w	d2,d1
		cmp.w	#4,d1
		blt.b	.match_next

		subq.w	#1,d1
.cop1
		move.b	(a0)+,(a1)+		; data
		dbf	d1,.cop1
		bra.b	.litrun
.zup
		tst.w	d1
		bne.b	.no0

		moveq	#15,d1
		move.b	(a0)+,d2
		bne.b	.ok1
.lop1	
		add.w	d3,d1
		move.b	(a0)+,d2
		beq.b	.lop1
.ok1
		add.w	d2,d1
.no0
		; copy d1+3 bytes
		moveq	#7,d0			; 4
		and.w	d1,d0			; 4
		neg.w	d0			; 4
		add.w	d0,d0			; 4
		lsr.w	#3,d1			; 10
		jmp	.copl(pc,d0.w)		; 14	=> 40
		move.b	(a0)+,(a1)+		; data
		move.b	(a0)+,(a1)+
.lop41
		move.b	(a0)+,(a1)+
		move.b	(a0)+,(a1)+
		move.b	(a0)+,(a1)+
		move.b	(a0)+,(a1)+
		move.b	(a0)+,(a1)+
.copl
		move.b	(a0)+,(a1)+
		move.b	(a0)+,(a1)+		; 12
		move.b	(a0)+,(a1)+
		dbf	d1,.lop41		; 10(14)
.litrun	
		moveq	#0,d1			; !!
		move.b	(a0)+,d1
		cmp.w	d4,d1
		bgt.b	.match

		lea	-$801(a1),a2
		move.w	d1,d0
		lsr.w	#2,d0
		sub.l	d0,a2
		move.b	(a0)+,d0
		lsl.w	#2,d0
		sub.l	d0,a2
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
.match_done
		and.w	d5,d1
		beq.b	.loop
.match_next
		move.b	(a0)+,(a1)+		; data
		subq.w	#1,d1
		bne.b	.match_next

		move.b	(a0)+,d1
		bra.b	.match
.loop
		move.b	(a0)+,d1
	if testing
		move.w	d1,$180(a6)
	endif
.loop2
		cmp.w	d4,d1	; #15
		ble.b	.zup
.match
		cmp.w	d6,d1	; #63
		ble.b	.no1
		
		lea	-1(a1),a2
		moveq	#31,d0
		and.w	d1,d0
		lsr.w	#2,d0
		sub.l	d0,a2
		move.b	(a0)+,d0
		lsl.w	#3,d0
		sub.l	d0,a2
		move.w	d1,d2
		lsr.w	#5,d2
		subq.w	#1,d2
		bra.b	.copy_match
.no1
		moveq	#31,d2
		cmp.w	d2,d1
		ble.b	.no2

		and.w	d1,d2
		bne.b	.no11
		
		moveq	#31,d2
		move.b	(a0)+,d1
		bne.b	.ok2
.lop2
		add.w	d3,d2
		move.b	(a0)+,d1
		beq.b	.lop2
.ok2
		add.w	d1,d2
.no11
		lea	-1(a1),a2
		moveq	#0,d1
		move.b	(a0)+,d1
		move.w	d1,d0
		lsr.w	#2,d0
		sub.l	d0,a2
		move.b	(a0)+,d0
		lsl.w	#6,d0
		sub.l	d0,a2
.copy_match
		; copy d2+2 bytes:
		move.b	(a2)+,(a1)+
		
		moveq	#7,d0
		and.w	d2,d0
		neg.w	d0
		add.w	d0,d0
		lsr.w	#3,d2
		jmp	.copl2(pc,d0.w)
.lop42
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+
.copl2
		move.b	(a2)+,(a1)+
		dbf	d2,.lop42
		moveq	#0,d2
		bra	.match_done
.no2
		cmp.w	d4,d1
		ble.b	.no3

		move.l	a1,a2
		moveq	#8,d0
		and.w	d1,d0
		ror.w	#5,d0
		and.w	#$f800,d0
		sub.l	d0,a2
		moveq	#7,d2
		and.w	d1,d2
		bne.b	.no21

		move.b	(a0)+,d1
		bne.b	.ok3
.lop3
		add.w	d3,d2
		move.b	(a0)+,d1
		beq.b	.lop3
.ok3
		add.w	d1,d2
		addq.w	#7,d2
.no21
		moveq	#0,d1
		move.b	(a0)+,d1
		move.w	d1,d0
		lsr.w	#2,d0
		sub.l	d0,a2
		move.b	(a0)+,d0
		lsl.w	#6,d0
		sub.l	d0,a2
		cmp.l	a2,a1
		beq.b	.end

		sub.l	a3,a2
		bra.b	.copy_match
.no3
		lea	-1(a1),a2
		move.w	d1,d0
		lsr.w	#2,d0
		sub.l	d0,a2
		move.b	(a0)+,d0
		lsl.w	#2,d0
		sub.l	d0,a2
		move.b	(a2)+,(a1)+
		move.b	(a2),(a1)+
		bra	.match_done
.end	
;		movem.l	(a7)+,d2-d6/a2-a3
		rts


*------	GENERATE SINE TABLE ---------------------------------------------------*

; sine table, 1024 angle steps, factor 256

generatesintab
	lea	.sinb(pc),a0			;
	move.l	b_sintab(pc),a1			;
	move.w	#246-1,d7			;
.gensin	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	move.w	d0,(a1)+			;
	dbf	d7,.gensin			;

	move.l	a1,a0				; used for cos
	moveq	#10+11-1,d7			; 10 values for sin, 11 values for cos
.fill256
	move.w	#$0100,(a1)+			;
	dbf	d7,.fill256			;

	move.w	#245-1,d7			; cos
.gencos	move.w	-(a0),(a1)+			;
	dbf	d7,.gencos			;

	move.w	#512-1,d7			;
	move.l	b_sintab(pc),a0			;
.genneg	move.w	(a0)+,d0			;
	neg.w	d0				;
	move.w	d0,(a1)+			;
	dbf	d7,.genneg			;

	move.l	b_sintab(pc),a0			;
	move.w	#256-1,d7			;
.gensin2
	move.w	(a0)+,(a1)+			;
	dbf	d7,.gensin2			;
	rts					;

.sinb	dc.b	$00,$02,$03,$05,$06,$08,$09,$0b
	dc.b	$0d,$0e,$10,$11,$13,$14,$16,$18
	dc.b	$19,$1b,$1c,$1e,$1f,$21,$22,$24
	dc.b	$26,$27,$29,$2a,$2c,$2d,$2f,$30
	dc.b	$32,$33,$35,$37,$38,$3a,$3b,$3d
	dc.b	$3e,$40,$41,$43,$44,$46,$47,$49
	dc.b	$4a,$4c,$4d,$4f,$50,$52,$53,$55
	dc.b	$56,$58,$59,$5b,$5c,$5e,$5f,$61
	dc.b	$62,$63,$65,$66,$68,$69,$6b,$6c
	dc.b	$6d,$6f,$70,$72,$73,$75,$76,$77
	dc.b	$79,$7a,$7b,$7d,$7e,$80,$81,$82
	dc.b	$84,$85,$86,$88,$89,$8a,$8c,$8d
	dc.b	$8e,$90,$91,$92,$93,$95,$96,$97
	dc.b	$99,$9a,$9b,$9c,$9e,$9f,$a0,$a1
	dc.b	$a2,$a4,$a5,$a6,$a7,$a8,$aa,$ab
	dc.b	$ac,$ad,$ae,$af,$b1,$b2,$b3,$b4
	dc.b	$b5,$b6,$b7,$b8,$b9,$ba,$bc,$bd
	dc.b	$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5
	dc.b	$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd
	dc.b	$ce,$cf,$cf,$d0,$d1,$d2,$d3,$d4
	dc.b	$d5,$d6,$d7,$d7,$d8,$d9,$da,$db
	dc.b	$dc,$dc,$dd,$de,$df,$e0,$e0,$e1
	dc.b	$e2,$e3,$e3,$e4,$e5,$e5,$e6,$e7
	dc.b	$e7,$e8,$e9,$e9,$ea,$eb,$eb,$ec
	dc.b	$ed,$ed,$ee,$ee,$ef,$ef,$f0,$f1
	dc.b	$f1,$f2,$f2,$f3,$f3,$f4,$f4,$f5
	dc.b	$f5,$f5,$f6,$f6,$f7,$f7,$f8,$f8
	dc.b	$f8,$f9,$f9,$f9,$fa,$fa,$fa,$fb
	dc.b	$fb,$fb,$fc,$fc,$fc,$fd,$fd,$fd
	dc.b	$fd,$fd,$fe,$fe,$fe,$fe,$fe,$ff
	dc.b	$ff,$ff,$ff,$ff,$ff,$ff		; 246 values 


*------	INIT COVER DATA -------------------------------------------------------*

initcoverdata
	move.l	b_coverdata(pc),a1		;
	moveq	#0,d0				; index to row offset (y)
	moveq	#numcoverrows-1,d7		;
.loop	lea	coverdata(pc),a0		;
	moveq	#numcovercols-1,d6		;
.copy	move.l	(a0)+,d1			; pattern
	not.l	d1				; prepare for "and"
	move.l	d1,(a1)+			;
	move.w	(a0)+,(a1)+			; x offset
	move.w	d0,(a1)+			;
	dbf	d6,.copy			;
	addq.w	#2,d0				;
	dbf	d7,.loop			;
;	rts					; fallthru


*------	SHUFFLE COVER DATA ----------------------------------------------------*

shufflecoverdata
	move.l	b_coverdata(pc),a1		; (A) source
	move.l	a1,a2				; (B) destination

	lea	base(pc),a0			; "random" data
	move.l	b_lspbank(pc),a3		; "random" data
	
	move.w	#numcoverrows*numcovercols-1,d7	;
.loop	moveq	#0,d0				; create "random" number
	move.b	(a0)+,d0			;
	add.b	(a0)+,d0			;
;	add.b	(a0)+,d0			;
	add.b	(a3)+,d0			;
	add.b	(a3)+,d0			;

	cmp.b	#numcoverrows*numcovercols,d0	;
	blo	.inrange			;
	sub.b	#numcoverrows*numcovercols,d0	;
.inrange
	asl.w	#3,d0				; *8

	move.l	(a1),d1				; swap A <-> B
	move.l	4(a1),d2			;
	move.l	(a2,d0.w),(a1)+			;
	move.l	4(a2,d0.w),(a1)+		;
	move.l	d1,(a2,d0.w)			;
	move.l	d2,4(a2,d0.w)			;
	
	dbf	d7,.loop			;
	rts					;


*------ TEXT PLANE ------------------------------------------------------------*

textplane
	tst.w	v_waiting(a5)			;
	bne	.waiting			;

	tst.w	v_show(a5)			;
	beq	.ringb				;

	clr.w	v_show(a5)			;
	clr.w	v_produce(a5)			;
	clr.w	v_waiting(a5)			;
	subq.w	#1,v_usedtbuffers(a5)		;

	add.w	#tsize,v_showtextploffset(a5)	; ring buffer
	cmp.w	#numtbuffers*tsize,v_showtextploffset(a5) :
	bne	.ringb				;
	clr.w	v_showtextploffset(a5)		;
.ringb	tst.w	v_triggertext(a5)		;
	beq	.notrigger			;
	clr.w	v_triggertext(a5)		;
	st	v_produce(a5)			;
	st	v_waiting(a5)			; semaphore
.notrigger

.waiting
	move.l	b_planet(pc),a0			;
	add.w	v_showtextploffset(a5),a0	;
	move.l	a0,d0				;
	move.l	b_clist3(pc),a1			;
	add.w	#textplanep+2-clist3,a1		;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	rts					;


*------ HANDLE TEXT BUFFERS ---------------------------------------------------*

handletextbuffers
	cmp.w	#numtbuffers,v_usedtbuffers(a5)	;
	beq	.nofreebuffer			;

	lea	text(pc),a0			;
	add.w	v_textindex(a5),a0		;
	lea	textend(pc),a1			;
	cmp.l	a0,a1				;
	bne	.gottext			;
	STOPACTOR actor_player			; stop player - that's it
;	clr.w	v_textindex(a5)			;
;	lea	text(pc),a0			;
.gottext
	bsr	textwidth			; returns d0, d4

	move.l	b_planet(pc),a1			;
	add.w	v_textplaneoffset(a5),a1	;

	move.w	#tsize/4-1,d7			;
	move.l	a1,a2				;
.clear	clr.l	(a2)+				;
	dbf	d7,.clear			;

	add.l	d0,a1				;
	moveq	#planetwidth,d5			;
	bsr	printtext			;

	addq.w	#1,v_usedtbuffers(a5)		;
	add.w	#tsize,v_textplaneoffset(a5)	; ring buffer
	cmp.w	#numtbuffers*tsize,v_textplaneoffset(a5) ;
	bne	.done				;
	clr.w	v_textplaneoffset(a5)		;
.done
.nofreebuffer
	tst.w	v_produce(a5)			;
	beq	.nogive				;
	cmp.w	#2,v_usedtbuffers(a5)		;
	bcs	.nobufferready			;
	clr.w	v_produce(a5)			; no skipping
	st	v_show(a5)			; vertb will consume this
	clr.w	v_waiting(a5)			;

.nobufferready
.noshow
.nogive	rts					;


*------ TEXT WIDTH ------------------------------------------------------------*

; input a0: text
; output d0, d4
textwidth
	lea	prop-" "(pc),a1			;
	moveq	#0,d2				; sum up width
.loop	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	beq	.done				;
	moveq	#0,d1				;
	move.b	(a1,d0.w),d1			;
	addq.b	#2,d1				; padding
	add.l	d1,d2				;
	bra	.loop				;
.done
	if textinfo
	move.l	d2,d1				; create info longword:
	beq	.zero				;
	lea	text(pc),a1			; WWWWC1C2
	add.w	v_textindex(a5),a1		; (width, char 1, char 2)
	asl.l	#8,d1				;
	move.b	(a1)+,d1			;
	asl.l	#8,d1				;
	move.b	(a1)+,d1			;
	move.l	d1,v_number(a5)			;
.zero	endif

	if testing
	cmp.w	#planetwidth*8+2,d2		; too large? (352+2)
	ble	.inrange			;
	st	v_doquit(a5)			; -> signal quit
.inrange
	endif

	move.l	#planetwidth*8+3,d0		; 352+2 + center adj (last padding)
	sub.l	d2,d0				;
	asr.l	#1,d0				; /2 (half)
	move.l	d0,d2				;
	asr.l	#3,d0				; /8 (byte)
	and.l	#%111,d2			;
	moveq	#0,d4				;
	move.b	.startmasks(pc,d2.w),d4		;
	rts					;

.startmasks
	dc.b	%10000000
	dc.b	%01000000
	dc.b	%00100000
	dc.b	%00010000
	dc.b	%00001000
	dc.b	%00000100
	dc.b	%00000010
	dc.b	%00000001


*------ PRINT TEXT ------------------------------------------------------------*

; param a1 bitplane, d4 xpos, d5 width
printtext
.char	lea	text(pc),a0			;
	add.w	v_textindex(a5),a0		;

	moveq	#0,d0				;
	move.b	(a0),d0				;
	beq	.done				;

	sub.b	#" ",d0				;
	moveq	#0,d1				;
	move.b	prop(pc,d0.w),d1		;
	subq.w	#1,d1				;

	asl.w	#5,d0				; *32 (data of 1 char)
	lea	font(pc),a2			;
	add.w	d0,a2				;

	moveq	#15,d3				; bit 15
.col	movem.l	a1-a2,-(a7)			;
	moveq	#fheight-1,d7			;
.row	move.w	(a2),d0				;
	btst.l	d3,d0				;
	beq	.no				;
	or.b	d4,(a1)				; draw pixel
.no	addq.w	#2,a2				; next row of char
	add.w	d5,a1				;
	dbf	d7,.row				;

	movem.l	(a7)+,a1-a2			;
	subq.w	#1,d3				; next bit to test in char col

	ror.b	#1,d4				; x++
	bcc	.x1				;
	addq.w	#1,a1				; next byte
.x1	dbf	d1,.col				;

	ror.b	#1,d4				; x++
	bcc	.space1				;
	addq.w	#1,a1				; next byte
.space1	ror.b	#1,d4				; x++
	bcc	.space2				;
	addq.w	#1,a1				; next byte
.space2	addq.w	#1,v_textindex(a5)		;
	bra	.char				;

.done	addq.w	#1,v_textindex(a5)		; skip 0
	rts					;

; net values, 0 = unused char
prop	dc.b	12	; space
	dc.b	2	; !
	dc.b	8	; "
	dc.b	0	; #
	dc.b	0	; $
	dc.b	0	; %
	dc.b	0	; &
	dc.b	3	; '
	dc.b	7	; (
	dc.b	7	; )
	dc.b	0	; *
	dc.b	0	; +
	dc.b	0	; ,
	dc.b	12	; -
	dc.b	3	; .
	dc.b	9	; /
	dc.b	0,4,12,0,12,0,0,0,0,12	; 0-9
	dc.b	3	; :
	dc.b	0	; ;
	dc.b	0	; <
	dc.b	0	; =
	dc.b	0	; >
	dc.b	0	; ?
	dc.b	13	; @ (used as 13px wide space)
	dc.b	12,12,12,12	; ABCD
	dc.b	12,12,12,12	; EFGH
	dc.b	3,12,12,12	; IJKL
	dc.b	13,12,12,12	; MNOP
	dc.b	12,12,12,11	; QRST
	dc.b	12,13,13,12	; UVWX
	dc.b	13,12		; YZ


*------ TEXT ------------------------------------------------------------------*

text	dc.b	"SPREADPOINT",0
	dc.b	"PRESENTS",0
	dc.b	"",0
	dc.b	"",0
	dc.b	"DEPECHE'S",0
	dc.b	"DEPECHE'S",0
	dc.b	"DEPECHE'S",0
	dc.b	"DEPECHE'S",0
	dc.b	"",0
	dc.b	"",0
	dc.b	"MODE@ ",0
	dc.b	"MODE@ ",0
	dc.b	"MODE@ ",0
	dc.b	"MODE@ ",0
	dc.b	"@ DEMO",0
	dc.b	"@ DEMO",0
	dc.b	"@ DEMO",0
	dc.b	"@ DEMO",0
	dc.b	"",0
	dc.b	"",0
	dc.b	"",0
	dc.b	"",0

	dc.b	"",0
;	dc.b	"",0

	dc.b	"SWISS CRACKING ASSOCIATION",0 ; $148 (width - see textinfo flag)
	dc.b	"",0
	dc.b	"THE ELECTRONIC KNIGHTS",0 ; $11f
	dc.b	"",0
	dc.b	"FIVE FINGER PUNCH",0 ; $dd
	dc.b	"",0
	dc.b	"ABYSS-CONNECTION",0 ; $d7
	dc.b	"",0
	dc.b	"THE BLACK LOTUS",0 ; $d0
	dc.b	"",0
	dc.b	"THE TWITCH ELITE",0 ; $cb
	dc.b	"",0
	dc.b	"KESTRA BITWORLD",0 ; $c8
	dc.b	"",0
	dc.b	"ATTENTIONWHORE",0 ; $b9
	dc.b	"",0
	dc.b	"BATMAN GROUP",0 ; $a8
	dc.b	"",0
	dc.b	"SPACEBALLS",0 ; $8c
	dc.b	"",0
	dc.b	"DEADLINERS",0 ; $83
	dc.b	"",0
	dc.b	"SLIPSTREAM",0 ; $83
	dc.b	"",0
	dc.b	"RESISTANCE",0 ; $82
	dc.b	"",0
	dc.b	"ANDROMEDA",0 ; $7f
	dc.b	"",0
	dc.b	"DEKADENCE",0 ; $7e
	dc.b	"",0
	dc.b	"NAH-KOLOR",0 ; $7e
	dc.b	"",0
	dc.b	"GHOSTOWN",0 ; $70
	dc.b	"",0
	dc.b	"UP ROUGH",0 ; $70
	dc.b	"",0
	dc.b	"ALCATRAZ",0 ; $6f
	dc.b	"",0
	dc.b	"AMIGABILL",0 ; $6d
	dc.b	"",0
	dc.b	"ARTSTATE",0 ; $6d
	dc.b	"",0
	dc.b	"FAIRLIGHT",0 ; $6b
	dc.b	"",0
	dc.b	"DEMOZOO",0 ; $63
	dc.b	"",0
	dc.b	"SCOOPEX",0 ; $62
	dc.b	"",0
	dc.b	"HAUJOBB",0 ; $62
	dc.b	"",0
	dc.b	"LOONIES",0 ; $59
	dc.b	"",0
	dc.b	"COCOON",0 ; $54
	dc.b	"",0
	dc.b	"REBELS",0 ; $54
	dc.b	"",0
	dc.b	"LEMON.",0 ; $4c
	dc.b	"",0
	dc.b	"DESIRE",0 ; $4b
	dc.b	"",0
	dc.b	"ALTAIR",0 ; $4a
	dc.b	"",0
	dc.b	"LEMMY",0 ; $49
	dc.b	"",0
	dc.b	"MCCOY",0 ; $48
	dc.b	"",0
	dc.b	"ABYSS",0 ; $47
	dc.b	"",0
	dc.b	"MELON",0 ; $47
	dc.b	"",0
	dc.b	"TPOLM",0 ; $46
	dc.b	"",0
	dc.b	"POUET",0 ; $45
	dc.b	"",0
	dc.b	"AFWD",0 ; $39
	dc.b	"",0
	dc.b	"VOID",0 ; $30
	dc.b	"",0
	dc.b	"TRSI",0 ; $2e
	dc.b	"",0

	rept 4
	dc.b	"",0
	endr
	rept 4
	dc.b	"MUSIC: VIRGILL",0
	endr
	rept 4
	dc.b	"",0
	endr
	rept 4
	dc.b	"LOGO REWORK: SANDER/FOCUS",0
	endr
	rept 4
	dc.b	"",0
	endr
	rept 4
	dc.b	"CODE: BIG BROTHER",0
	endr
	rept 4
	dc.b	"",0
	endr
	rept 4
	dc.b	"CODE AND GFX: DEPECHE",0
	endr

	rept 8
	dc.b	"",0
	endr
	rept 5
	dc.b	"REVISION 2O24",0
	endr

	rept 2*numtbuffers
	dc.b	"",0 ; important
	endr
textend
	rept 2*numtbuffers
	dc.b	"",0 ; important
	endr


*------ DMD FONT --------------------------------------------------------------*

fheight equ 16
	even

font	ds.w	16	; space
	ds.w	16	; !

	dc.w	%1110011100000000	; "
	dc.w	%1110011100000000
	dc.w	%1110011100000000
	dc.w	%1110011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; #
	ds.w	16	; $
	ds.w	16	; %
	ds.w	16	; &

	dc.w	%1110000000000000	; '
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111110000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%0111110000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111110000000000	; )
	dc.w	%0111111000000000	; extra padding at beginning
	dc.w	%0111111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0111111000000000
	dc.w	%0111111000000000
	dc.w	%0111110000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; *
	ds.w	16	; +
	ds.w	16	; ,

	dc.w	%0000000000000000	; -
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; .
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000011100000000	; /
	dc.w	%0000011100000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0011100000000000
	dc.w	%0011100000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 0 (use O instead)

	dc.w	%0111000000000000	; 1
	dc.w	%0111000000000000	; extra padding at beginning
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000	; 2
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 3

	dc.w	%1110000001110000	; 4
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 5
	ds.w	16	; 6
	ds.w	16	; 7
	ds.w	16	; 8

	dc.w	%0111111111100000	; 9
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; :
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; ;
	ds.w	16	; <
	ds.w	16	; =
	ds.w	16	; >
	ds.w	16	; ?

	dc.w	%0000000000000000	; @ (used as 13px wide space)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000	; A
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111001110000
	dc.w	%1111111001110000
	dc.w	%1111111001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110011111100000
	dc.w	%1110011111100000
	dc.w	%1110011111100000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1111000001111000
	dc.w	%1111100011111000
	dc.w	%1111110111111000
	dc.w	%1111111111111000
	dc.w	%1110111110111000
	dc.w	%1110011100111000
	dc.w	%1110001000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	
	dc.w	%1110000001110000
	dc.w	%1111000001110000
	dc.w	%1111100001110000
	dc.w	%1111110001110000
	dc.w	%1111111001110000
	dc.w	%1110111101110000
	dc.w	%1110011111110000
	dc.w	%1110001111110000
	dc.w	%1110000111110000
	dc.w	%1110000011110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	
	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000


	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011100000000
	dc.w	%1110001110000000
	dc.w	%1110000111000000
	dc.w	%1110000011100000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1111000001111000
	dc.w	%0111100011110000
	dc.w	%0011110111100000
	dc.w	%0001111111000000
	dc.w	%0000111110000000
	dc.w	%0000011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110001000111000
	dc.w	%1110011100111000
	dc.w	%1110111110111000
	dc.w	%1111111111111000
	dc.w	%1111110111111000
	dc.w	%1111100011111000
	dc.w	%1111000001111000
	dc.w	%1110000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0111111111100000
	dc.w	%0111111111100000
	dc.w	%0111111111100000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1111111111111000
	dc.w	%1111111111111000
	dc.w	%0111111111110000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000111100000
	dc.w	%0000001111000000
	dc.w	%0000011110000000
	dc.w	%0000111100000000
	dc.w	%0001111000000000
	dc.w	%0011110000000000
	dc.w	%0111100000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000


*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.13 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;*****************************************************************

;------------------------------------------------------------------
;
;	LSP_MusicInit
;
;		In:	a0: LSP music data(any memory)
;			a1: LSP sound bank(chip memory)
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;------------------------------------------------------------------
LSP_MusicInit
	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),d1		; a1: sound bank data (chip mem)

	lea	LSP_State(pc),a3
	move.l	a0,a4				; relocation flag ad
	addq.w	#2,a0				; skip relocation flag
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.w	(a0)+,m_escCodeGetPos(a3)
	move.l	(a0)+,-(a7)			; music len in frame ticks
	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (win 2 cycles in insane player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a0,a1				; keep relocated flag
.relocLoop
	tst.b	(a4)				; relocation guard
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes table size
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0

	; read sequence timing infos (if any)
	move.w	(a0)+,m_seqCount(a3)
	beq	.noSeq
	move.l	a0,m_seqTable(a3)
	clr.w	m_currentSeq(a3)
	move.w	m_seqCount(a3),d0
	moveq	#0,d1
	move.w	d0,d1
	lsl.w	#3,d1				; 8 bytes per entry
	add.w	#12,d1				; add 3 last 32bits (word stream size, byte stream loop, word stream loop)
	add.l	a0,d1				; word stream data address
	subq.w	#1,d0
.seqRel	tst.b	(a4)
	bne	.skipRel
	add.l	d1,(a0)
	add.l	d1,4(a0)
.skipRel
	addq.w	#8,a0
	dbf	d0,.seqRel

.noSeq	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	st	(a4)				; mark this music score as "relocated"
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks
	rts


;------------------------------------------------------------------
;
;	LSP_MusicPlayTick
;
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;------------------------------------------------------------------
LSP_MusicPlayTick
	lea	LSP_State(pc),a1
	move.l	(a1),a0				; byte stream
	move.l	m_codeTableAddr(a1),a2		; code table
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
.cmdExec
	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	.resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch2-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch3-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch4-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			


.noInst	move.l	a0,(a1)				; store word stream (or byte stream if coming from early out)
	rts

.cextended
	add.w	#$100,d0
	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm
	cmp.w	m_escCodeGetPos(a1),d0
	bne	.cmdExec
.r_setPos
	move.b	(a0)+,(m_currentSeq+1)(a1)
	bra	.process

.r_rewind	
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm
	move.b	(a0)+,(m_currentBpm+1)(a1)	; BPM
	bra	.process

.resetv	dc.l	0,0,0,0


	if musicpos

;------------------------------------------------------------------
;
;	LSP_MusicSetPos
;
;		In: d0: seq position (from 0 to last seq of the song)
;		Out:None
;
;	Force the replay pointer to a seq position. If music wasn't converted
;	using -setpos option, this func does nothing
;
;------------------------------------------------------------------
LSP_MusicSetPos
	lea	LSP_State(pc),a3
	move.w	m_seqCount(a3),d1
	beq	.noTimingInfo
	cmp.w	d1,d0
	bge	.noTimingInfo
	move.w	d0,m_currentSeq(a3)
	move.l	m_seqTable(a3),a0
	lsl.w	#3,d0
	add.w	d0,a0
	move.l	(a0)+,m_wordStream(a3)
	move.l	(a0)+,m_byteStream(a3)
.noTimingInfo
	rts


;------------------------------------------------------------------
;
;	LSP_MusicGetPos
;
;		In: None
;		Out: d0:  seq position (from 0 to last seq of the song)
;
;	Get the current seq position. If music wasn't converted with
;	-getpos option, this func just returns 0
;
;------------------------------------------------------------------
LSP_MusicGetPos
	move.w	LSP_State+m_currentSeq(pc),d0
	rts
	
	endif


	rsreset	
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_seqCount		rs.w	1
m_seqTable		rs.l	1
m_currentSeq		rs.w	1
m_escCodeGetPos		rs.w	1

m_dmaconPatch2		rs.l	1	; added
m_dmaconPatch3		rs.l	1	; added
m_dmaconPatch4		rs.l	1	; added

sizeof_LSPVars		rs.w	0

LSP_State	ds.b	sizeof_LSPVars
	even


*------	LOGO ------------------------------------------------------------------*

logo	incbin	"logo"
logoend
	even
	
	
*------	LOGO COLOR FADING -----------------------------------------------------*

logofading
	move.w	v_logocolindex(a5),d0	;
	btst	#0,d0			; every second frame only
	bne	.skip			;

	move.l	b_clist1(pc),a1		;
	add.w	#logocl1-clist1+2,a1	;
	lea	logocols(pc,d0.w),a0	;
	moveq	#numlogocolors-1,d7	;
.loop	move.w	(a0),(a1)		;
	add.w	#logocol1end-logocol1,a0 ;
	addq.w	#4,a1			; next color in clist
	dbf	d7,.loop		;

.skip	addq.w	#1,v_logocolindex(a5)	;
	cmp.w	#logocol1end-logocol1,v_logocolindex(a5) ;
	bne	.nf			;
	STOPACTOR actor_logo_fading
.nf	rts				;

numlogocolors	equ	15

logocols
logocol1
	dc.w	$0000,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111
logocol1end
	dc.w	$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0111,$0222,$0222,$0222,$0222,$0222,$0222,$0222
	dc.w	$0111,$0111,$0111,$0111,$0111,$0222,$0222,$0222,$0222,$0222,$0333,$0333,$0333,$0333,$0333
	dc.w	$0111,$0111,$0111,$0111,$0222,$0222,$0222,$0333,$0333,$0333,$0333,$0444,$0444,$0444,$0444
	dc.w	$0111,$0111,$0111,$0222,$0222,$0222,$0333,$0333,$0333,$0444,$0444,$0444,$0555,$0555,$0555
	dc.w	$0111,$0111,$0222,$0222,$0222,$0333,$0333,$0444,$0444,$0444,$0555,$0555,$0666,$0666,$0666
	dc.w	$0111,$0111,$0222,$0222,$0333,$0333,$0444,$0444,$0555,$0555,$0666,$0666,$0666,$0777,$0777
	dc.w	$0111,$0111,$0222,$0333,$0333,$0444,$0444,$0555,$0555,$0666,$0666,$0777,$0777,$0888,$0888
	dc.w	$0111,$0222,$0222,$0333,$0333,$0444,$0555,$0555,$0666,$0666,$0777,$0888,$0888,$0999,$0999
	dc.w	$0111,$0222,$0222,$0333,$0444,$0444,$0555,$0666,$0666,$0777,$0888,$0888,$0999,$0aaa,$0aaa
	dc.w	$0111,$0222,$0333,$0333,$0444,$0555,$0666,$0666,$0777,$0888,$0888,$0999,$0aaa,$0bbb,$0bbb
	dc.w	$0111,$0222,$0333,$0444,$0444,$0555,$0666,$0777,$0888,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ccc
	dc.w	$0111,$0222,$0333,$0444,$0555,$0666,$0666,$0777,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ddd,$0ddd
	dc.w	$0111,$0222,$0333,$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ddd,$0ddd,$0eee
	dc.w	$0111,$0222,$0333,$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ddd,$0eee,$0fff


*------	PLANET ----------------------------------------------------------------*

planet	incbin	"planet"
planetend


*------	VORONOI FRAME ---------------------------------------------------------*

frame	dc.b	-120,-120
	dc.b	120,-120
	dc.b	120,120
	dc.b	-120,120

frameindices
	dc.b	0*4,1*4,1*4,2*4,2*4,3*4,3*4,0*4

punks
orbits	dc.l	orbit0-base, orbit0end-base
	dc.l	orbit2-base, orbit2end-base
	dc.l	orbit4-base, orbit4end-base
	dc.l	orbit6-base, orbit6end-base
	dc.l	orbit8-base, orbit8end-base
	dc.l	orbit10-base, orbit10end-base
	dc.l	orbit12-base, orbit12end-base
	dc.l	orbit14-base, orbit14end-base
	dc.l	orbit15-base, orbit15end-base
	dc.l	orbit13-base, orbit13end-base
	dc.l	orbit11-base, orbit11end-base
	dc.l	orbit9-base, orbit9end-base
	dc.l	orbit7-base, orbit7end-base
	dc.l	orbit5-base, orbit5end-base
	dc.l	orbit3-base, orbit3end-base
	dc.l	orbit1-base, orbit1end-base
orbitsend
punksend

orbit0	dc.b	$01,$ef,$06, $03,$eb,$06, $04,$e8,$06, $06,$e4,$06
	dc.b	$08,$e1,$06, $0a,$dd,$06, $0c,$da,$06, $0f,$d7,$06
	dc.b	$11,$d3,$06, $13,$d0,$06, $16,$cd,$06, $18,$ca,$06
	dc.b	$1a,$c7,$06, $1d,$c4,$06, $20,$c1,$06, $22,$be,$06
	dc.b	$25,$bb,$06, $28,$b8,$06, $2b,$b5,$06, $2e,$b3,$06
	dc.b	$31,$b0,$06, $34,$ad,$07, $37,$ab,$07, $3a,$a8,$07
	dc.b	$3d,$a6,$07, $40,$a4,$07, $43,$a1,$07, $47,$9f,$07
	dc.b	$4a,$9d,$07, $4d,$9b,$07, $51,$99,$07, $54,$97,$07
	dc.b	$58,$95,$07, $5b,$93,$07, $5f,$92,$07, $62,$90,$07
	dc.b	$66,$8f,$07, $69,$8d,$07, $6d,$8c,$07, $71,$8a,$07
	dc.b	$75,$89,$07, $78,$88,$07, $7c,$87,$07, $80,$86,$07
	dc.b	$84,$85,$07, $87,$84,$07, $8b,$83,$07, $8f,$82,$07
	dc.b	$93,$82,$07, $97,$81,$07, $9b,$81,$07, $9f,$80,$07
	dc.b	$a3,$80,$07, $a7,$80,$07, $aa,$80,$07, $ae,$80,$07
	dc.b	$b1,$80,$08, $b5,$80,$08, $b9,$80,$08, $bd,$80,$08
	dc.b	$c1,$80,$08, $c5,$81,$08, $c9,$81,$08, $cd,$82,$08
	dc.b	$d1,$82,$08, $d4,$83,$08, $d8,$84,$08, $dc,$85,$08
	dc.b	$e0,$86,$08, $e4,$87,$08, $e7,$88,$08, $eb,$89,$08
	dc.b	$ef,$8a,$08, $f3,$8c,$08, $f6,$8d,$08, $fa,$8e,$08
	dc.b	$fe,$90,$08, $01,$92,$18, $05,$93,$18, $08,$95,$18
	dc.b	$0c,$97,$18, $0f,$99,$18, $13,$9b,$18, $16,$9d,$18
	dc.b	$19,$9f,$18, $1d,$a1,$18, $20,$a3,$18, $23,$a6,$18
	dc.b	$26,$a8,$18, $29,$ab,$18, $2c,$ad,$18, $2f,$b0,$19
	dc.b	$32,$b2,$19, $35,$b5,$19, $38,$b8,$19, $3b,$bb,$19
	dc.b	$3d,$bd,$19, $40,$c0,$19, $43,$c3,$19, $45,$c6,$19
	dc.b	$48,$c9,$19, $4a,$cd,$19, $4d,$d0,$19, $4f,$d3,$19
	dc.b	$51,$d6,$19, $53,$da,$19, $56,$dd,$19, $58,$e0,$19
	dc.b	$5a,$e4,$19, $5b,$e7,$19, $5d,$eb,$19, $5f,$ee,$19
	
	dc.b	$70,$ee,$10 ; invisible (extra invisible dry out frame)
orbit0end

orbit1	dc.b	$0c,$ec,$05, $0e,$e8,$05, $10,$e5,$05, $12,$e1,$05
	dc.b	$14,$de,$05, $16,$da,$05, $18,$d7,$05, $1a,$d4,$05
	dc.b	$1c,$d1,$05, $1e,$cd,$05, $20,$ca,$05, $23,$c7,$05
	dc.b	$25,$c4,$05, $28,$c1,$05, $2a,$be,$05, $2d,$bb,$05
	dc.b	$2f,$b8,$05, $32,$b6,$05, $35,$b3,$05, $37,$b0,$05
	dc.b	$3a,$ae,$06, $3d,$ab,$06, $40,$a9,$06, $43,$a6,$06
	dc.b	$46,$a4,$06, $49,$a2,$06, $4c,$a0,$06, $4f,$9d,$06
	dc.b	$52,$9b,$06, $56,$99,$06, $59,$97,$06, $5c,$96,$06
	dc.b	$5f,$94,$06, $63,$92,$06, $66,$90,$06, $6a,$8f,$06
	dc.b	$6d,$8d,$06, $70,$8c,$06, $74,$8b,$06, $77,$89,$07
	dc.b	$7b,$88,$07, $7f,$87,$07, $82,$86,$07, $86,$85,$07
	dc.b	$89,$84,$07, $8d,$83,$07, $91,$83,$07, $94,$82,$07
	dc.b	$98,$81,$07, $9b,$81,$07, $9f,$80,$07, $a3,$80,$07
	dc.b	$a7,$80,$07, $aa,$80,$07, $ae,$80,$07, $b1,$80,$08
	dc.b	$b4,$80,$08, $b8,$80,$08, $bc,$80,$08, $bf,$80,$08
	dc.b	$c3,$81,$08, $c7,$81,$08, $ca,$82,$08, $ce,$82,$08
	dc.b	$d2,$83,$08, $d5,$84,$08, $d9,$85,$08, $dd,$86,$08
	dc.b	$e0,$87,$08, $e4,$88,$08, $e7,$89,$08, $eb,$8a,$08
	dc.b	$ee,$8b,$09, $f2,$8d,$09, $f5,$8e,$09, $f8,$90,$09
	dc.b	$fc,$91,$09, $ff,$93,$09, $03,$95,$19, $06,$97,$19
	dc.b	$09,$99,$19, $0c,$9b,$19, $0f,$9d,$19, $13,$9f,$19
	dc.b	$16,$a1,$19, $19,$a3,$19, $1c,$a5,$19, $1f,$a8,$19
	dc.b	$22,$aa,$19, $25,$ad,$19, $27,$af,$19, $2a,$b2,$1a
	dc.b	$2d,$b5,$1a, $30,$b7,$1a, $32,$ba,$1a, $35,$bd,$1a
	dc.b	$37,$c0,$1a, $3a,$c3,$1a, $3c,$c6,$1a, $3f,$c9,$1a
	dc.b	$41,$cc,$1a, $43,$cf,$1a, $45,$d3,$1a, $48,$d6,$1a
	dc.b	$4a,$d9,$1a, $4c,$dc,$1a, $4e,$e0,$1a, $4f,$e3,$1a
	dc.b	$51,$e7,$1a, $53,$ea,$1a, $55,$ee,$1a, $56,$f1,$1a
	dc.b	$58,$f5,$1a, $59,$f9,$1a, $5b,$fc,$1a
	
	dc.b	$70,$ee,$10
orbit1end

orbit2	dc.b	$2a,$d1,$04, $2c,$ce,$04, $2f,$cb,$04, $31,$c8,$04
	dc.b	$33,$c5,$04, $35,$c2,$04, $37,$bf,$04, $3a,$bc,$04
	dc.b	$3c,$b9,$04, $3e,$b6,$04, $41,$b3,$05, $43,$b1,$05
	dc.b	$46,$ae,$05, $48,$ac,$05, $4b,$a9,$05, $4e,$a7,$05
	dc.b	$50,$a4,$05, $53,$a2,$05, $56,$a0,$05, $59,$9e,$05
	dc.b	$5b,$9c,$05, $5e,$9a,$05, $61,$98,$05, $64,$96,$05
	dc.b	$67,$94,$06, $6a,$92,$06, $6d,$91,$06, $70,$8f,$06
	dc.b	$73,$8e,$06, $76,$8c,$06, $79,$8b,$06, $7d,$89,$06
	dc.b	$80,$88,$06, $83,$87,$06, $86,$86,$06, $89,$85,$06
	dc.b	$8d,$84,$07, $90,$83,$07, $93,$83,$07, $96,$82,$07
	dc.b	$9a,$81,$07, $9d,$81,$07, $a0,$80,$07, $a4,$80,$07
	dc.b	$a7,$80,$07, $aa,$80,$07, $ae,$80,$07, $b0,$80,$08
	dc.b	$b3,$80,$08, $b7,$80,$08, $ba,$80,$08, $bd,$80,$08
	dc.b	$c1,$81,$08, $c4,$81,$08, $c7,$82,$08, $ca,$82,$08
	dc.b	$ce,$83,$08, $d1,$84,$08, $d4,$84,$08, $d7,$85,$09
	dc.b	$db,$86,$09, $de,$87,$09, $e1,$89,$09, $e4,$8a,$09
	dc.b	$e7,$8b,$09, $eb,$8d,$09, $ee,$8e,$09, $f1,$90,$09
	dc.b	$f4,$91,$09, $f7,$93,$09, $fa,$95,$09, $fd,$96,$0a
	dc.b	$00,$98,$1a, $03,$9a,$1a, $05,$9c,$1a, $08,$9e,$1a
	dc.b	$0b,$a1,$1a, $0e,$a3,$1a, $11,$a5,$1a, $13,$a7,$1a
	dc.b	$16,$aa,$1a, $18,$ac,$1a, $1b,$af,$1a, $1e,$b2,$1a
	dc.b	$20,$b4,$1b, $22,$b7,$1b, $25,$ba,$1b, $27,$bd,$1b
	dc.b	$29,$bf,$1b, $2c,$c2,$1b, $2e,$c5,$1b, $30,$c9,$1b
	dc.b	$32,$cc,$1b, $34,$cf,$1b, $36,$d2,$1b, $38,$d5,$1b
	dc.b	$3a,$d9,$1b, $3c,$dc,$1b, $3d,$df,$1b, $3f,$e3,$1b
	dc.b	$41,$e6,$1b, $42,$ea,$1b, $44,$ed,$1b, $45,$f1,$1c
	dc.b	$47,$f4,$1c, $48,$f8,$1c, $49,$fc,$1c
	
	dc.b	$70,$ee,$10
orbit2end

orbit3	dc.b	$4b,$bc,$03, $4d,$b9,$04, $4f,$b7,$04, $51,$b4,$04
	dc.b	$53,$b1,$04, $55,$af,$04, $58,$ac,$04, $5a,$aa,$04
	dc.b	$5c,$a7,$04, $5e,$a5,$04, $61,$a2,$04, $63,$a0,$04
	dc.b	$65,$9e,$04, $68,$9c,$05, $6a,$9a,$05, $6d,$98,$05
	dc.b	$6f,$96,$05, $72,$94,$05, $74,$93,$05, $77,$91,$05
	dc.b	$79,$8f,$05, $7c,$8e,$05, $7f,$8c,$06, $81,$8b,$06
	dc.b	$84,$8a,$06, $87,$88,$06, $89,$87,$06, $8c,$86,$06
	dc.b	$8f,$85,$06, $92,$84,$06, $94,$84,$06, $97,$83,$06
	dc.b	$9a,$82,$07, $9d,$81,$07, $a0,$81,$07, $a2,$81,$07
	dc.b	$a5,$80,$07, $a8,$80,$07, $ab,$80,$07, $ae,$80,$07
	dc.b	$b0,$80,$07, $b2,$80,$08, $b5,$80,$08, $b8,$80,$08
	dc.b	$bb,$80,$08, $be,$80,$08, $c0,$81,$08, $c3,$81,$08
	dc.b	$c6,$82,$08, $c9,$83,$09, $cc,$83,$09, $ce,$84,$09
	dc.b	$d1,$85,$09, $d4,$86,$09, $d7,$87,$09, $d9,$88,$09
	dc.b	$dc,$8a,$09, $df,$8b,$09, $e1,$8c,$09, $e4,$8e,$0a
	dc.b	$e7,$8f,$0a, $e9,$91,$0a, $ec,$93,$0a, $ee,$94,$0a
	dc.b	$f1,$96,$0a, $f3,$98,$0a, $f6,$9a,$0a, $f8,$9c,$0a
	dc.b	$fb,$9e,$0b, $fd,$a0,$0b, $ff,$a2,$0b, $02,$a5,$1b
	dc.b	$04,$a7,$1b, $06,$a9,$1b, $08,$ac,$1b, $0b,$ae,$1b
	dc.b	$0d,$b1,$1b, $0f,$b4,$1b, $11,$b6,$1b, $13,$b9,$1b
	dc.b	$15,$bc,$1c, $17,$bf,$1c, $19,$c2,$1c, $1b,$c5,$1c
	dc.b	$1c,$c8,$1c, $1e,$cb,$1c, $20,$ce,$1c, $22,$d1,$1c
	dc.b	$23,$d5,$1c, $25,$d8,$1c, $26,$db,$1c, $28,$df,$1c
	dc.b	$29,$e2,$1c, $2b,$e6,$1c, $2c,$e9,$1c, $2e,$ed,$1d
	dc.b	$2f,$f0,$1d, $30,$f4,$1d, $31,$f8,$1d, $32,$fb,$1d
	
	dc.b	$70,$ee,$10
orbit3end

orbit4	dc.b	$69,$af,$03, $6a,$ac,$03, $6c,$aa,$03, $6e,$a8,$04
	dc.b	$70,$a5,$04, $71,$a3,$04, $73,$a1,$04, $75,$9e,$04
	dc.b	$77,$9c,$04, $79,$9a,$04, $7b,$98,$04, $7d,$96,$04
	dc.b	$7f,$95,$05, $81,$93,$05, $83,$91,$05, $85,$90,$05
	dc.b	$87,$8e,$05, $89,$8d,$05, $8b,$8b,$05, $8d,$8a,$05
	dc.b	$8f,$89,$06, $91,$87,$06, $94,$86,$06, $96,$85,$06
	dc.b	$98,$84,$06, $9a,$84,$06, $9c,$83,$06, $9e,$82,$06
	dc.b	$a1,$82,$07, $a3,$81,$07, $a5,$81,$07, $a7,$80,$07
	dc.b	$a9,$80,$07, $ac,$80,$07, $ae,$80,$07, $b0,$80,$07
	dc.b	$b1,$80,$08, $b4,$80,$08, $b6,$80,$08, $b8,$80,$08
	dc.b	$ba,$80,$08, $bc,$81,$08, $bf,$81,$08, $c1,$82,$09
	dc.b	$c3,$83,$09, $c5,$83,$09, $c7,$84,$09, $ca,$85,$09
	dc.b	$cc,$86,$09, $ce,$87,$09, $d0,$88,$09, $d2,$89,$0a
	dc.b	$d4,$8b,$0a, $d6,$8c,$0a, $d8,$8e,$0a, $da,$8f,$0a
	dc.b	$dc,$91,$0a, $de,$92,$0a, $e0,$94,$0a, $e2,$96,$0b
	dc.b	$e4,$98,$0b, $e6,$9a,$0b, $e8,$9c,$0b, $ea,$9e,$0b
	dc.b	$ec,$a0,$0b, $ee,$a2,$0b, $f0,$a4,$0b, $f2,$a7,$0b
	dc.b	$f3,$a9,$0c, $f5,$ac,$0c, $f7,$ae,$0c, $f8,$b1,$0c
	dc.b	$fa,$b3,$0c, $fc,$b6,$0c, $fd,$b9,$0c, $ff,$bc,$0c
	dc.b	$00,$bf,$1c, $02,$c1,$1c, $03,$c4,$1d, $05,$c8,$1d
	dc.b	$06,$cb,$1d, $08,$ce,$1d, $09,$d1,$1d, $0a,$d4,$1d
	dc.b	$0c,$d8,$1d, $0d,$db,$1d, $0e,$de,$1d, $0f,$e2,$1d
	dc.b	$10,$e5,$1d, $11,$e9,$1d, $12,$ec,$1d, $13,$f0,$1d
	dc.b	$14,$f3,$1e, $15,$f7,$1e, $16,$fb,$1e, $17,$fe,$1e
	
	dc.b	$70,$ee,$10
orbit4end

orbit5	dc.b	$82,$a8,$03, $84,$a6,$03, $85,$a3,$03, $86,$a1,$03
	dc.b	$87,$9f,$04, $89,$9d,$04, $8a,$9b,$04, $8b,$99,$04
	dc.b	$8d,$97,$04, $8e,$95,$04, $8f,$93,$04, $91,$91,$04
	dc.b	$92,$90,$05, $94,$8e,$05, $95,$8d,$05, $96,$8b,$05
	dc.b	$98,$8a,$05, $99,$89,$05, $9b,$88,$05, $9c,$87,$06
	dc.b	$9e,$86,$06, $9f,$85,$06, $a1,$84,$06, $a2,$83,$06
	dc.b	$a4,$82,$06, $a5,$82,$06, $a7,$81,$07, $a8,$81,$07
	dc.b	$aa,$80,$07, $ab,$80,$07, $ad,$80,$07, $ae,$80,$07
	dc.b	$b0,$80,$07, $b1,$80,$08, $b2,$80,$08, $b4,$80,$08
	dc.b	$b5,$80,$08, $b7,$80,$08, $b8,$81,$08, $ba,$81,$08
	dc.b	$bb,$82,$09, $bd,$82,$09, $be,$83,$09, $c0,$84,$09
	dc.b	$c1,$85,$09, $c3,$86,$09, $c4,$87,$09, $c6,$88,$0a
	dc.b	$c7,$89,$0a, $c9,$8b,$0a, $ca,$8c,$0a, $cb,$8d,$0a
	dc.b	$cd,$8f,$0a, $ce,$90,$0a, $d0,$92,$0b, $d1,$94,$0b
	dc.b	$d2,$95,$0b, $d4,$97,$0b, $d5,$99,$0b, $d6,$9b,$0b
	dc.b	$d8,$9d,$0b, $d9,$9f,$0c, $da,$a2,$0c, $dc,$a4,$0c
	dc.b	$dd,$a6,$0c, $de,$a9,$0c, $df,$ab,$0c, $e0,$ae,$0c
	dc.b	$e2,$b0,$0c, $e3,$b3,$0c, $e4,$b6,$0d, $e5,$b8,$0d
	dc.b	$e6,$bb,$0d, $e7,$be,$0d, $e8,$c1,$0d, $e9,$c4,$0d
	dc.b	$ea,$c7,$0d, $eb,$ca,$0d, $ec,$cd,$0d, $ed,$d0,$0d
	dc.b	$ee,$d4,$0e, $ef,$d7,$0e, $f0,$da,$0e, $f0,$de,$0e
	dc.b	$f1,$e1,$0e, $f2,$e5,$0e, $f3,$e8,$0e, $f3,$ec,$0e
	dc.b	$f4,$ef,$0e, $f5,$f3,$0e, $f5,$f6,$0e, $f6,$fa,$0e
	dc.b	$f7,$fe,$0e
	
	dc.b	$70,$ee,$10
orbit5end

orbit6	dc.b	$99,$a8,$03, $9a,$a6,$03, $9a,$a4,$03, $9b,$a1,$03
	dc.b	$9b,$9f,$03, $9c,$9d,$03, $9d,$9b,$04, $9d,$99,$04
	dc.b	$9e,$97,$04, $9f,$95,$04, $a0,$93,$04, $a0,$92,$04
	dc.b	$a1,$90,$04, $a2,$8f,$05, $a2,$8d,$05, $a3,$8c,$05
	dc.b	$a4,$8a,$05, $a5,$89,$05, $a5,$88,$05, $a6,$87,$05
	dc.b	$a7,$86,$06, $a8,$85,$06, $a8,$84,$06, $a9,$83,$06
	dc.b	$aa,$82,$06, $ab,$82,$06, $ab,$81,$06, $ac,$81,$07
	dc.b	$ad,$80,$07, $ae,$80,$07, $af,$80,$07, $af,$80,$07
	dc.b	$b0,$80,$07, $b0,$80,$08, $b1,$80,$08, $b1,$80,$08
	dc.b	$b2,$80,$08, $b3,$80,$08, $b4,$81,$08, $b5,$81,$09
	dc.b	$b5,$82,$09, $b6,$82,$09, $b7,$83,$09, $b8,$84,$09
	dc.b	$b8,$85,$09, $b9,$86,$09, $ba,$87,$0a, $bb,$88,$0a
	dc.b	$bb,$89,$0a, $bc,$8a,$0a, $bd,$8c,$0a, $be,$8d,$0a
	dc.b	$be,$8f,$0a, $bf,$90,$0b, $c0,$92,$0b, $c0,$93,$0b
	dc.b	$c1,$95,$0b, $c2,$97,$0b, $c3,$99,$0b, $c3,$9b,$0b
	dc.b	$c4,$9d,$0c, $c5,$9f,$0c, $c5,$a1,$0c, $c6,$a4,$0c
	dc.b	$c6,$a6,$0c, $c7,$a8,$0c, $c8,$ab,$0c, $c8,$ad,$0c
	dc.b	$c9,$b0,$0d, $ca,$b2,$0d, $ca,$b5,$0d, $cb,$b8,$0d
	dc.b	$cb,$bb,$0d, $cc,$be,$0d, $cc,$c1,$0d, $cd,$c4,$0d
	dc.b	$cd,$c7,$0d, $ce,$ca,$0e, $ce,$cd,$0e, $cf,$d0,$0e
	dc.b	$cf,$d3,$0e, $d0,$d6,$0e, $d0,$da,$0e, $d1,$dd,$0e
	dc.b	$d1,$e1,$0e, $d1,$e4,$0e, $d2,$e7,$0e, $d2,$eb,$0e
	dc.b	$d2,$ef,$0f, $d3,$f2,$0f, $d3,$f6,$0f, $d3,$f9,$0f
	dc.b	$d4,$fd,$0f
	
	dc.b	$70,$ee,$10
orbit6end

orbit7	dc.b	$b0,$a6,$03, $b0,$a4,$03, $b0,$a2,$03, $b0,$9f,$03
	dc.b	$b0,$9d,$03, $b0,$9b,$03, $b0,$99,$04, $b0,$97,$04
	dc.b	$b0,$95,$04, $b0,$94,$04, $b0,$92,$04, $b0,$90,$04
	dc.b	$b0,$8f,$04, $b0,$8d,$05, $b0,$8c,$05, $b0,$8a,$05
	dc.b	$b0,$89,$05, $b0,$88,$05, $b0,$87,$05, $b0,$86,$05
	dc.b	$b0,$85,$06, $b0,$84,$06, $b0,$83,$06, $b0,$82,$06
	dc.b	$b0,$82,$06, $b0,$81,$06, $b0,$81,$07, $b0,$80,$07
	dc.b	$b0,$80,$07, $b0,$80,$07, $b0,$80,$07, $b0,$80,$07
	dc.b	$b0,$80,$08, $b0,$80,$08, $b0,$80,$08, $b0,$80,$08
	dc.b	$b0,$80,$08, $b0,$81,$08, $b0,$81,$09, $b0,$82,$09
	dc.b	$b0,$82,$09, $b0,$83,$09, $b0,$84,$09, $b0,$85,$09
	dc.b	$b0,$86,$09, $b0,$87,$0a, $b0,$88,$0a, $b0,$89,$0a
	dc.b	$b0,$8a,$0a, $b0,$8b,$0a, $b0,$8d,$0a, $b0,$8e,$0b
	dc.b	$b0,$90,$0b, $b0,$91,$0b, $b0,$93,$0b, $b0,$95,$0b
	dc.b	$b0,$97,$0b, $b0,$99,$0b, $b0,$9b,$0c, $b0,$9d,$0c
	dc.b	$b0,$9f,$0c, $b0,$a1,$0c, $b0,$a3,$0c, $b0,$a6,$0c
	dc.b	$b0,$a8,$0c, $b0,$aa,$0c, $b0,$ad,$0d, $b0,$af,$0d
	dc.b	$b0,$b2,$0d, $b0,$b5,$0d, $b0,$b7,$0d, $b0,$ba,$0d
	dc.b	$b0,$bd,$0d, $b0,$c0,$0d, $b0,$c3,$0d, $b0,$c6,$0e
	dc.b	$b0,$c9,$0e, $b0,$cc,$0e, $b0,$cf,$0e, $b0,$d3,$0e
	dc.b	$b0,$d6,$0e, $b0,$d9,$0e, $b0,$dd,$0e, $b0,$e0,$0e
	dc.b	$b0,$e3,$0e, $b0,$e7,$0f, $b0,$ea,$0f, $b0,$ee,$0f
	dc.b	$b0,$f2,$0f, $b0,$f5,$0f, $b0,$f9,$0f, $b0,$fd,$0f
	
	dc.b	$70,$ee,$10
orbit7end

orbit8	dc.b	$c7,$a7,$03, $c6,$a4,$03, $c5,$a2,$03, $c5,$a0,$03
	dc.b	$c4,$9e,$03, $c3,$9c,$03, $c3,$9a,$04, $c2,$98,$04
	dc.b	$c1,$96,$04, $c1,$94,$04, $c0,$92,$04, $bf,$91,$04
	dc.b	$bf,$8f,$04, $be,$8d,$05, $bd,$8c,$05, $bc,$8b,$05
	dc.b	$bc,$89,$05, $bb,$88,$05, $ba,$87,$05, $b9,$86,$06
	dc.b	$b9,$85,$06, $b8,$84,$06, $b7,$83,$06, $b6,$83,$06
	dc.b	$b6,$82,$06, $b5,$81,$06, $b4,$81,$07, $b3,$80,$07
	dc.b	$b2,$80,$07, $b2,$80,$07, $b1,$80,$07, $b0,$80,$07
	dc.b	$b0,$80,$08, $b0,$80,$08, $af,$80,$08, $ae,$80,$08
	dc.b	$ad,$80,$08, $ac,$81,$08, $ac,$81,$08, $ab,$82,$09
	dc.b	$aa,$82,$09, $a9,$83,$09, $a9,$84,$09, $a8,$84,$09
	dc.b	$a7,$85,$09, $a6,$86,$0a, $a6,$87,$0a, $a5,$89,$0a
	dc.b	$a4,$8a,$0a, $a3,$8b,$0a, $a3,$8d,$0a, $a2,$8e,$0a
	dc.b	$a1,$90,$0b, $a0,$91,$0b, $a0,$93,$0b, $9f,$95,$0b
	dc.b	$9e,$96,$0b, $9e,$98,$0b, $9d,$9a,$0b, $9c,$9c,$0c
	dc.b	$9c,$9e,$0c, $9b,$a1,$0c, $9a,$a3,$0c, $9a,$a5,$0c
	dc.b	$99,$a8,$0c, $98,$aa,$0c, $98,$ac,$0c, $97,$af,$0d
	dc.b	$97,$b2,$0d, $96,$b4,$0d, $96,$b7,$0d, $95,$ba,$0d
	dc.b	$94,$bd,$0d, $94,$c0,$0d, $93,$c3,$0d, $93,$c6,$0d
	dc.b	$92,$c9,$0e, $92,$cc,$0e, $91,$cf,$0e, $91,$d2,$0e
	dc.b	$90,$d5,$0e, $90,$d9,$0e, $90,$dc,$0e, $8f,$df,$0e
	dc.b	$8f,$e3,$0e, $8e,$e6,$0e, $8e,$ea,$0e, $8e,$ed,$0f
	dc.b	$8d,$f1,$0f, $8d,$f5,$0f, $8d,$f8,$0f, $8c,$fc,$0f
	
	dc.b	$70,$ee,$10
orbit8end

orbit9	dc.b	$de,$a9,$03, $dd,$a7,$03, $dc,$a5,$03, $db,$a2,$03
	dc.b	$d9,$a0,$03, $d8,$9e,$04, $d7,$9c,$04, $d5,$9a,$04
	dc.b	$d4,$98,$04, $d3,$96,$04, $d1,$94,$04, $d0,$93,$04
	dc.b	$cf,$91,$04, $cd,$8f,$05, $cc,$8e,$05, $ca,$8c,$05
	dc.b	$c9,$8b,$05, $c8,$8a,$05, $c6,$88,$05, $c5,$87,$05
	dc.b	$c3,$86,$06, $c2,$85,$06, $c0,$84,$06, $bf,$83,$06
	dc.b	$bd,$83,$06, $bc,$82,$06, $ba,$81,$06, $b9,$81,$07
	dc.b	$b7,$80,$07, $b6,$80,$07, $b4,$80,$07, $b3,$80,$07
	dc.b	$b1,$80,$07, $b0,$80,$08, $af,$80,$08, $ad,$80,$08
	dc.b	$ac,$80,$08, $aa,$80,$08, $a9,$81,$08, $a7,$81,$08
	dc.b	$a6,$81,$09, $a4,$82,$09, $a3,$83,$09, $a1,$84,$09
	dc.b	$a0,$84,$09, $9e,$85,$09, $9d,$86,$09, $9b,$87,$0a
	dc.b	$9a,$88,$0a, $98,$8a,$0a, $97,$8b,$0a, $95,$8c,$0a
	dc.b	$94,$8e,$0a, $93,$8f,$0a, $91,$91,$0b, $90,$93,$0b
	dc.b	$8e,$94,$0b, $8d,$96,$0b, $8c,$98,$0b, $8a,$9a,$0b
	dc.b	$89,$9c,$0b, $88,$9e,$0b, $87,$a0,$0c, $85,$a2,$0c
	dc.b	$84,$a5,$0c, $83,$a7,$0c, $82,$aa,$0c, $80,$ac,$0c
	dc.b	$7f,$af,$0c, $7e,$b1,$0c, $7d,$b4,$0c, $7c,$b7,$0d
	dc.b	$7b,$b9,$0d, $7a,$bc,$0d, $79,$bf,$0d, $77,$c2,$0d
	dc.b	$76,$c5,$0d, $75,$c8,$0d, $75,$cb,$0d, $74,$ce,$0d
	dc.b	$73,$d2,$0d, $72,$d5,$0e, $71,$d8,$0e, $70,$dc,$0e
	dc.b	$6f,$df,$0e, $6e,$e2,$0e, $6e,$e6,$0e, $6d,$e9,$0e
	dc.b	$6c,$ed,$0e, $6c,$f0,$0e, $6b,$f4,$0e, $6a,$f8,$0e
	dc.b	$6a,$fb,$0e
	
	dc.b	$70,$ee,$10
orbit9end

orbit10	dc.b	$f9,$b1,$03, $f7,$af,$03, $f6,$ac,$03, $f4,$aa,$03
	dc.b	$f2,$a7,$04, $f0,$a5,$04, $ee,$a3,$04, $ed,$a0,$04
	dc.b	$eb,$9e,$04, $e9,$9c,$04, $e7,$9a,$04, $e5,$98,$04
	dc.b	$e3,$96,$04, $e1,$95,$05, $df,$93,$05, $dd,$91,$05
	dc.b	$db,$8f,$05, $d9,$8e,$05, $d7,$8d,$05, $d5,$8b,$05
	dc.b	$d3,$8a,$05, $d1,$89,$06, $ce,$87,$06, $cc,$86,$06
	dc.b	$ca,$85,$06, $c8,$84,$06, $c6,$84,$06, $c4,$83,$06
	dc.b	$c1,$82,$06, $bf,$82,$07, $bd,$81,$07, $bb,$81,$07
	dc.b	$b9,$80,$07, $b6,$80,$07, $b4,$80,$07, $b2,$80,$07
	dc.b	$b0,$80,$07, $af,$80,$08, $ac,$80,$08, $aa,$80,$08
	dc.b	$a8,$80,$08, $a6,$80,$08, $a3,$81,$08, $a1,$81,$08
	dc.b	$9f,$82,$09, $9d,$83,$09, $9b,$83,$09, $99,$84,$09
	dc.b	$96,$85,$09, $94,$86,$09, $92,$87,$09, $90,$88,$09
	dc.b	$8e,$89,$0a, $8c,$8b,$0a, $8a,$8c,$0a, $88,$8e,$0a
	dc.b	$86,$8f,$0a, $83,$91,$0a, $81,$92,$0a, $7f,$94,$0a
	dc.b	$7d,$96,$0b, $7b,$98,$0b, $7a,$9a,$0b, $78,$9c,$0b
	dc.b	$76,$9e,$0b, $74,$a0,$0b, $72,$a2,$0b, $70,$a4,$0b
	dc.b	$6e,$a7,$0b, $6d,$a9,$0c, $6b,$ac,$0c, $69,$ae,$0c
	dc.b	$67,$b1,$0c, $66,$b3,$0c, $64,$b6,$0c, $63,$b9,$0c
	dc.b	$61,$bc,$0c, $5f,$bf,$0c, $5e,$c2,$0c, $5c,$c5,$0d
	dc.b	$5b,$c8,$0d, $5a,$cb,$0d, $58,$ce,$0d, $57,$d1,$0d
	dc.b	$56,$d4,$0d, $54,$d8,$0d, $53,$db,$0d, $52,$de,$0d
	dc.b	$51,$e2,$0d, $50,$e5,$0d, $4f,$e9,$0d, $4e,$ec,$0d
	dc.b	$4d,$f0,$0d, $4c,$f3,$0e, $4b,$f7,$0e, $4a,$fb,$0e

	dc.b	$70,$ee,$10
orbit10end

orbit11	dc.b	$16,$bd,$13, $14,$ba,$13, $12,$b7,$14, $0f,$b5,$14
	dc.b	$0d,$b2,$14, $0b,$af,$14, $09,$ad,$14, $07,$aa,$14
	dc.b	$05,$a8,$14, $02,$a5,$14, $00,$a3,$14, $fe,$a1,$04
	dc.b	$fb,$9f,$04, $f9,$9d,$05, $f6,$9b,$05, $f4,$99,$05
	dc.b	$f2,$97,$05, $ef,$95,$05, $ec,$93,$05, $ea,$91,$05
	dc.b	$e7,$90,$05, $e5,$8e,$05, $e2,$8d,$05, $df,$8b,$06
	dc.b	$dd,$8a,$06, $da,$89,$06, $d7,$88,$06, $d5,$87,$06
	dc.b	$d2,$86,$06, $cf,$85,$06, $cc,$84,$06, $ca,$83,$06
	dc.b	$c7,$82,$07, $c4,$82,$07, $c1,$81,$07, $be,$81,$07
	dc.b	$bc,$80,$07, $b9,$80,$07, $b6,$80,$07, $b3,$80,$07
	dc.b	$b0,$80,$07, $ae,$80,$08, $ac,$80,$08, $a9,$80,$08
	dc.b	$a6,$80,$08, $a3,$80,$08, $a0,$81,$08, $9e,$81,$08
	dc.b	$9b,$82,$08, $98,$83,$08, $95,$83,$09, $92,$84,$09
	dc.b	$90,$85,$09, $8d,$86,$09, $8a,$87,$09, $87,$88,$09
	dc.b	$85,$89,$09, $82,$8b,$09, $7f,$8c,$09, $7d,$8d,$0a
	dc.b	$7a,$8f,$0a, $78,$90,$0a, $75,$92,$0a, $72,$94,$0a
	dc.b	$70,$96,$0a, $6d,$97,$0a, $6b,$99,$0a, $68,$9b,$0a
	dc.b	$66,$9d,$0a, $64,$a0,$0b, $61,$a2,$0b, $5f,$a4,$0b
	dc.b	$5d,$a6,$0b, $5a,$a9,$0b, $58,$ab,$0b, $56,$ae,$0b
	dc.b	$54,$b0,$0b, $52,$b3,$0b, $50,$b6,$0b, $4e,$b8,$0b
	dc.b	$4c,$bb,$0c, $4a,$be,$0c, $48,$c1,$0c, $46,$c4,$0c
	dc.b	$44,$c7,$0c, $42,$ca,$0c, $40,$cd,$0c, $3f,$d1,$0c
	dc.b	$3d,$d4,$0c, $3c,$d7,$0c, $3a,$da,$0c, $38,$de,$0c
	dc.b	$37,$e1,$0c, $36,$e5,$0c, $34,$e8,$0c, $33,$ec,$0d
	dc.b	$32,$ef,$0d, $30,$f3,$0d, $2f,$f7,$0d, $2e,$fa,$0d
	dc.b	$2d,$fe,$0d
	
	dc.b	$70,$ee,$10
orbit11end

orbit12	dc.b	$35,$d0,$14, $33,$cd,$14, $31,$c9,$14, $2e,$c6,$14
	dc.b	$2c,$c3,$14, $2a,$c0,$14, $28,$bd,$14, $26,$bb,$14
	dc.b	$23,$b8,$14, $21,$b5,$14, $1e,$b2,$15, $1c,$b0,$15
	dc.b	$19,$ad,$15, $17,$ab,$15, $14,$a8,$15, $11,$a6,$15
	dc.b	$0f,$a3,$15, $0c,$a1,$15, $09,$9f,$15, $06,$9d,$15
	dc.b	$03,$9b,$15, $01,$99,$15, $fe,$97,$05, $fb,$95,$05
	dc.b	$f8,$93,$06, $f5,$92,$06, $f2,$90,$06, $ef,$8e,$06
	dc.b	$eb,$8d,$06, $e8,$8c,$06, $e5,$8a,$06, $e2,$89,$06
	dc.b	$df,$88,$06, $dc,$87,$06, $d8,$86,$06, $d5,$85,$06
	dc.b	$d2,$84,$07, $cf,$83,$07, $cb,$82,$07, $c8,$82,$07
	dc.b	$c5,$81,$07, $c2,$81,$07, $be,$80,$07, $bb,$80,$07
	dc.b	$b8,$80,$07, $b4,$80,$07, $b1,$80,$07, $af,$80,$08
	dc.b	$ab,$80,$08, $a8,$80,$08, $a5,$80,$08, $a1,$80,$08
	dc.b	$9e,$81,$08, $9b,$81,$08, $97,$82,$08, $94,$82,$08
	dc.b	$91,$83,$08, $8e,$84,$08, $8a,$85,$09, $87,$86,$09
	dc.b	$84,$87,$09, $81,$88,$09, $7e,$89,$09, $7a,$8a,$09
	dc.b	$77,$8c,$09, $74,$8d,$09, $71,$8f,$09, $6e,$90,$09
	dc.b	$6b,$92,$09, $68,$93,$09, $65,$95,$0a, $62,$97,$0a
	dc.b	$5f,$99,$0a, $5c,$9b,$0a, $59,$9d,$0a, $57,$9f,$0a
	dc.b	$54,$a1,$0a, $51,$a4,$0a, $4e,$a6,$0a, $4c,$a8,$0a
	dc.b	$49,$ab,$0a, $47,$ad,$0a, $44,$b0,$0a, $41,$b3,$0a
	dc.b	$3f,$b5,$0b, $3d,$b8,$0b, $3a,$bb,$0b, $38,$be,$0b
	dc.b	$36,$c1,$0b, $33,$c4,$0b, $31,$c7,$0b, $2f,$ca,$0b
	dc.b	$2d,$cd,$0b, $2b,$d0,$0b, $29,$d3,$0b, $27,$d7,$0b
	dc.b	$25,$da,$0b, $24,$dd,$0b, $22,$e1,$0b, $20,$e4,$0b
	dc.b	$1f,$e8,$0b, $1d,$eb,$0b, $1b,$ef,$0b, $1a,$f2,$0c
	dc.b	$19,$f6,$0c, $17,$fa,$0c, $16,$fd,$0c
	
	dc.b	$70,$ee,$10
orbit12end

orbit13	dc.b	$54,$eb,$15, $52,$e8,$15, $50,$e4,$15, $4e,$e1,$15
	dc.b	$4c,$de,$15, $4a,$da,$15, $48,$d7,$15, $46,$d4,$15
	dc.b	$44,$d0,$15, $42,$cd,$15, $3f,$ca,$15, $3d,$c7,$15
	dc.b	$3b,$c4,$15, $38,$c1,$15, $36,$be,$15, $33,$bb,$15
	dc.b	$30,$b8,$15, $2e,$b5,$15, $2b,$b3,$15, $28,$b0,$15
	dc.b	$25,$ae,$16, $23,$ab,$16, $20,$a9,$16, $1d,$a6,$16
	dc.b	$1a,$a4,$16, $17,$a2,$16, $14,$9f,$16, $10,$9d,$16
	dc.b	$0d,$9b,$16, $0a,$99,$16, $07,$97,$16, $04,$95,$16
	dc.b	$00,$94,$16, $fd,$92,$06, $fa,$90,$06, $f6,$8f,$06
	dc.b	$f3,$8d,$06, $ef,$8c,$06, $ec,$8a,$07, $e8,$89,$07
	dc.b	$e5,$88,$07, $e1,$87,$07, $de,$86,$07, $da,$85,$07
	dc.b	$d6,$84,$07, $d3,$83,$07, $cf,$82,$07, $cc,$82,$07
	dc.b	$c8,$81,$07, $c4,$81,$07, $c1,$80,$07, $bd,$80,$07
	dc.b	$b9,$80,$07, $b5,$80,$07, $b2,$80,$07, $af,$80,$08
	dc.b	$ab,$80,$08, $a8,$80,$08, $a4,$80,$08, $a0,$80,$08
	dc.b	$9d,$81,$08, $99,$81,$08, $95,$82,$08, $92,$82,$08
	dc.b	$8e,$83,$08, $8a,$84,$08, $87,$85,$08, $83,$86,$08
	dc.b	$80,$87,$08, $7c,$88,$08, $79,$89,$08, $75,$8a,$08
	dc.b	$72,$8b,$09, $6e,$8d,$09, $6b,$8e,$09, $67,$90,$09
	dc.b	$64,$92,$09, $60,$93,$09, $5d,$95,$09, $5a,$97,$09
	dc.b	$57,$99,$09, $53,$9b,$09, $50,$9d,$09, $4d,$9f,$09
	dc.b	$4a,$a1,$09, $47,$a3,$09, $44,$a6,$09, $41,$a8,$09
	dc.b	$3e,$aa,$09, $3b,$ad,$09, $38,$b0,$09, $36,$b2,$0a
	dc.b	$33,$b5,$0a, $30,$b8,$0a, $2e,$ba,$0a, $2b,$bd,$0a
	dc.b	$28,$c0,$0a, $26,$c3,$0a, $24,$c6,$0a, $21,$c9,$0a
	dc.b	$1f,$cc,$0a, $1d,$d0,$0a, $1a,$d3,$0a, $18,$d6,$0a
	dc.b	$16,$d9,$0a, $14,$dd,$0a, $12,$e0,$0a, $10,$e4,$0a
	dc.b	$0f,$e7,$0a, $0d,$eb,$0a, $0b,$ee,$0a, $0a,$f2,$0a
	dc.b	$08,$f5,$0a, $07,$f9,$0a, $05,$fd,$0a
	
	dc.b	$70,$ee,$10
orbit13end

orbit14	dc.b	$5e,$ec,$16, $5c,$e8,$16, $5a,$e5,$16, $58,$e1,$16
	dc.b	$56,$de,$16, $54,$db,$16, $52,$d7,$16, $50,$d4,$16
	dc.b	$4d,$d1,$16, $4b,$ce,$16, $49,$ca,$16, $46,$c7,$16
	dc.b	$44,$c4,$16, $41,$c1,$16, $3e,$be,$16, $3c,$bb,$16
	dc.b	$39,$b9,$16, $36,$b6,$16, $33,$b3,$16, $30,$b1,$16
	dc.b	$2d,$ae,$17, $2a,$ab,$17, $27,$a9,$17, $24,$a7,$17
	dc.b	$21,$a4,$17, $1e,$a2,$17, $1a,$a0,$17, $17,$9e,$17
	dc.b	$14,$9b,$17, $10,$99,$17, $0d,$98,$17, $09,$96,$17
	dc.b	$06,$94,$17, $02,$92,$17, $ff,$91,$07, $fb,$8f,$07
	dc.b	$f7,$8d,$07, $f4,$8c,$07, $f0,$8b,$07, $ec,$89,$07
	dc.b	$e9,$88,$07, $e5,$87,$07, $e1,$86,$07, $dd,$85,$07
	dc.b	$d9,$84,$07, $d6,$83,$07, $d2,$83,$07, $ce,$82,$07
	dc.b	$ca,$81,$07, $c6,$81,$07, $c2,$80,$07, $be,$80,$07
	dc.b	$ba,$80,$07, $b6,$80,$07, $b3,$80,$07, $b0,$80,$08
	dc.b	$ac,$80,$08, $a8,$80,$08, $a4,$80,$08, $a0,$80,$08
	dc.b	$9c,$81,$08, $98,$81,$08, $94,$82,$08, $90,$82,$08
	dc.b	$8c,$83,$08, $89,$84,$08, $85,$85,$08, $81,$85,$08
	dc.b	$7d,$86,$08, $79,$88,$08, $76,$89,$08, $72,$8a,$08
	dc.b	$6e,$8b,$08, $6b,$8d,$08, $67,$8e,$08, $63,$90,$08
	dc.b	$60,$91,$08, $5c,$93,$08, $59,$95,$08, $55,$97,$08
	dc.b	$52,$98,$08, $4e,$9a,$08, $4b,$9c,$08, $48,$9f,$08
	dc.b	$44,$a1,$08, $41,$a3,$08, $3e,$a5,$08, $3b,$a8,$08
	dc.b	$38,$aa,$08, $34,$ad,$08, $31,$af,$09, $2e,$b2,$09
	dc.b	$2c,$b4,$09, $29,$b7,$09, $26,$ba,$09, $23,$bd,$09
	dc.b	$20,$c0,$09, $1e,$c3,$09, $1b,$c6,$09, $19,$c9,$09
	dc.b	$16,$cc,$09, $14,$cf,$09, $12,$d2,$09, $0f,$d6,$09
	dc.b	$0d,$d9,$09, $0b,$dc,$09, $09,$e0,$09, $07,$e3,$09
	dc.b	$05,$e7,$09, $03,$ea,$09, $01,$ee,$09
	
	dc.b	$70,$ee,$10
orbit14end

orbit15	dc.b	$5e,$e5,$18, $5c,$e2,$18, $5a,$df,$18, $58,$db,$18
	dc.b	$55,$d8,$18, $53,$d5,$18, $51,$d1,$18, $4e,$ce,$18
	dc.b	$4c,$cb,$18, $49,$c8,$18, $47,$c5,$18, $44,$c2,$18
	dc.b	$42,$bf,$18, $3f,$bc,$18, $3c,$b9,$18, $39,$b6,$18
	dc.b	$36,$b4,$18, $33,$b1,$18, $30,$ae,$18, $2d,$ac,$18
	dc.b	$2a,$a9,$18, $27,$a7,$18, $23,$a5,$18, $20,$a2,$18
	dc.b	$1d,$a0,$18, $19,$9e,$18, $16,$9c,$18, $13,$9a,$18
	dc.b	$0f,$98,$18, $0c,$96,$18, $08,$94,$18, $04,$92,$18
	dc.b	$01,$91,$18, $fd,$8f,$08, $f9,$8e,$08, $f6,$8c,$08
	dc.b	$f2,$8b,$08, $ee,$8a,$08, $ea,$88,$08, $e6,$87,$08
	dc.b	$e3,$86,$08, $df,$85,$08, $db,$84,$08, $d7,$83,$08
	dc.b	$d3,$83,$08, $cf,$82,$08, $cb,$81,$08, $c7,$81,$08
	dc.b	$c3,$80,$08, $bf,$80,$08, $bb,$80,$08, $b7,$80,$08
	dc.b	$b3,$80,$08, $b0,$80,$08, $ac,$80,$08, $a8,$80,$08
	dc.b	$a4,$80,$08, $a0,$80,$08, $9c,$81,$08, $98,$81,$08
	dc.b	$94,$81,$08, $90,$82,$08, $8c,$83,$08, $88,$84,$08
	dc.b	$85,$84,$08, $81,$85,$08, $7d,$86,$08, $79,$87,$08
	dc.b	$75,$89,$08, $71,$8a,$08, $6e,$8b,$08, $6a,$8c,$08
	dc.b	$66,$8e,$08, $62,$8f,$08, $5f,$91,$08, $5b,$93,$08
	dc.b	$57,$94,$08, $54,$96,$08, $50,$98,$08, $4d,$9a,$08
	dc.b	$49,$9c,$08, $46,$9e,$08, $43,$a0,$08, $3f,$a3,$08
	dc.b	$3c,$a5,$08, $39,$a7,$08, $36,$aa,$08, $33,$ac,$08
	dc.b	$2f,$af,$08, $2c,$b1,$08, $29,$b4,$08, $27,$b7,$08
	dc.b	$24,$ba,$08, $21,$bc,$08, $1e,$bf,$08, $1b,$c2,$08
	dc.b	$19,$c5,$08, $16,$c8,$08, $14,$cb,$08, $11,$cf,$08
	dc.b	$0f,$d2,$08, $0c,$d5,$08, $0a,$d8,$08, $08,$dc,$08
	dc.b	$06,$df,$08, $04,$e3,$08, $02,$e6,$08
	
	dc.b	$70,$ee,$10
orbit15end
	even


*------ DATA CIRC-WAVE BY BIG BROTHER ----------------------------------------*

	;; anim step, ypos1, ypos2, ..., yposN
copperylst
	include	"circ-wave/copper-color-pos.inc"
copperylstend
	even

coppercolors	; brightest to darkest - last remains unused!
	dc.w	$0fff,$0eee,$0ddd,$0ccc,$0bbb,$0aaa,$0999,$0888,$0777,$0666
coppercolorsend

	;; number of colors used in the copper list effect, this must be aligned
	;; with the number of columns in the copperylst table! Currently not
	;; checked.
numshades	equ	(coppercolorsend-coppercolors)>>1

	even
	; Don't change order and spacing of even_deltas and odd_deltas, as
	; these two are used to compute the number of elements in the list.
even_deltas
	include "circ-wave/even-deltas.inc"
odd_deltas
	include "circ-wave/odd-deltas.inc"
even_idx
	include "circ-wave/even-start.inc"
odd_idx	
	include "circ-wave/odd-start.inc"

arch00	include "circ-wave/arch-00.inc"
arch01	include "circ-wave/arch-01.inc"
arch02	include "circ-wave/arch-02.inc"
arch03	include "circ-wave/arch-03.inc"
arch04	include "circ-wave/arch-04.inc"
arch05	include "circ-wave/arch-05.inc"
arch06	include "circ-wave/arch-06.inc"
arch07	include "circ-wave/arch-07.inc"
arch08	include "circ-wave/arch-08.inc"
arch09	include "circ-wave/arch-09.inc"
arch10	include "circ-wave/arch-10.inc"
arch11	include "circ-wave/arch-11.inc"
arch12	include "circ-wave/arch-12.inc"
arch13	include "circ-wave/arch-13.inc"
arch14	include "circ-wave/arch-14.inc"
arch15	include "circ-wave/arch-15.inc"
arch16	include "circ-wave/arch-16.inc"
arch17	include "circ-wave/arch-17.inc"
arch18	include "circ-wave/arch-18.inc"
arch19	include "circ-wave/arch-19.inc"
arch20	include "circ-wave/arch-20.inc"
arch21	include "circ-wave/arch-21.inc"

archlst	dc.l	arch00-arch00
	dc.l	arch01-arch00
	dc.l	arch02-arch00
	dc.l 	arch03-arch00
	dc.l 	arch04-arch00
	dc.l 	arch05-arch00
	dc.l 	arch06-arch00
	dc.l	arch07-arch00
	dc.l	arch08-arch00
	dc.l	arch09-arch00
	dc.l	arch10-arch00
	dc.l	arch11-arch00
	dc.l	arch12-arch00
	dc.l	arch13-arch00
	dc.l	arch14-arch00
	dc.l	arch15-arch00
	dc.l	arch16-arch00
	dc.l	arch17-arch00
	dc.l	arch18-arch00
	dc.l	arch19-arch00
	dc.l	arch20-arch00
	dc.l	arch21-arch00
archlstend

nrarches 	equ	(archlstend-archlst)>>2
deltalstlen	equ	(odd_deltas-even_deltas)/nrarches


*------	MUSIC -----------------------------------------------------------------*

lspbank	incbin	"virgill-dmd2.lsbank"
lspbankend
	even	; very important

lspmusic
	incbin	"virgill-dmd2.lsmusic",10 ; skip header (10 bytes)
	even


*------	SKY -------------------------------------------------------------------*

crunch ; start of special memory section for crunched data - do not change order
sky	incbin	"sky"
skyend
	even


*------	VORONOI DATA (CRUNCHED) -----------------------------------------------*

voronoi	incbin	"voronoic",4 ; skip header (4 bytes)
voronoiend
	even


*------	PICTURE - SPREADING THE NEWS AROUND THE WORLD MCCOY -------------------*

mccoy	incbin	"mccoy"
mccoyend
crunchend ; end of special memory section for crunched data
	even


*------	BOING -----------------------------------------------------------------*

boing	incbin	"boing"
boingend
	even
