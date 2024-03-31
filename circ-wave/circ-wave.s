; CIRC-WAVE
; (C) 2024 BIG BROTHER

cwincwviswidth	equ	24		; bytes
cwviswidth	equ	40		; bytes
cwpwidth	equ	cwviswidth+cwincwviswidth ; Intentionally adds up to 64
cwpheight	equ	256 		; px
cwpsize		equ	cwpwidth*cwpheight ; bytes

DMACONR		equ 	$002		; DMA control (and blitter status) read
INTENAR		equ	$01C		; Interrupt enable bits read
IRQ3VEC		equ	$06c		; Interrupt level 3 vector
INTENA		equ	$09A		; Interrupt enable bits (clr/set)
INTREQ		equ	$09C		; Interrupt request bits (clr/set)
DMACON		equ	$096		; DMA control (clr/set)
COP1L 		equ	$080		; Copper list 1
COLOR00		equ	$180		; Color 0 (background)
COLOR01		equ	$182		; Color 1

BLTAMOD		equ	$064		; Blitter modulo for source A
BLTBMOD		equ	$062		; Blitter modulo for source B
BLTCMOD		equ	$060		; Blitter modulo for source C
BLTDMOD		equ	$066		; Blitter modulo for destination D
BLTCON0		equ	$040		; Blitter control register 0
BLTCON1		equ	$042		; Blitter control register 1
BLTAPT		equ	$050		; Blitter pointer to source A
BLTBPT		equ	$04c		; Blitter pointer to source B
BLTCPT		equ	$048		; Blitter pointer to source C
BLTDPT		equ	$054		; Blitter pointer to destination D
BLTAFWM		equ	$044		; Blitter first word mask for source A
BLTALWM		equ	$046		; Blitter last word mask for source A
BLTSIZE		equ	$058		; Blitter start and size (window width,height)
DIWSTRT		equ	$08E		; Display window start (upper left vert-horiz position)
DIWSTOP		equ	$090		; Display window stop (lower right vert.-horiz. position)
DDFSTRT		equ	$092		; Display bitplane data fetch start (horiz. position)
DDFSTOP		equ	$094		; Display bitplane data fetch stop (horiz. position)
BPLCON0		equ	$100		; Bitplane control register (misc. control bits)
BPLCON1		equ	$102		; Bitplane control reg. (scroll value PF1, PF2)
BPLCON2		equ	$104		; Bitplane control reg. (priority control)
BPLCON3		equ	$106		; Bitplane control (enhanced features)
BPL1MOD		equ	$108		; Bitplane modulo (odd planes)
BPL2MOD		equ	$10A		; Bitplane modulo (even planes)
BPL1PT		equ	$0E0		; Bitplane 1 pointer


*------ CIRC WAVE ---------------------------------------------------------*

circwave
	move.l	v_dbcwplane1a(a5),d0		; bitplane pointer
	move.l	b_clistcw1(pc),a1		;
	move.w	d0,bplcw-clistcw1+2+4(a1)	;
	swap	d0				;	
	move.w	d0,bplcw-clistcw1+2(a1)		;
	bsr	updateclist			; must happen before rasterline $2c
	move.l	v_dbcwplane1b(a5),a4		;
	bsr	updateframe			; incrementally update frame content
	rts					;


*------ Draw the initial image -------------------------------------------*
; Input:
; a0	pointer to table with nr. bobs to plot per arch
; a4	bitplane
; a5	pointer to variables
; a6	custom chip address
drawfullimage
;	lea	archlst(pc),a1
	lea	base(pc),a1
	add.l	#archlst-base,a1
	
	moveq	#nrarches-1,d7
.loop0	move.w	(a0)+,d0		; number of bobs to blit
	move.l	(a1)+,a2		; arch data: (x,y) coords list
.loop1	movem.w	(a2)+,d1/d2
	move.w	#%1111110000000000,d4 	; OR blitting
	bsr	drawbob
	dbf	d0,.loop1
	dbf	d7,.loop0
	rts

; a4	bitplane
; a5	pointer to variables
; a6	must point to $dff000
; Incrementally update a frame by adding/removing bobs.
updateframe
	moveq	#0,d0			; moveq is always .l
	move.w	v_animstep(a5),d0 	; even/odd frame names are based on R's
	btst	#0,d0			; 1-based indexing scheme. Here we are 0
	beq	.oddfrm			; based, exactly the other way round
	lea	base(pc),a0		; current list of indices
	add.l	#even_idx-base,a0
	lea	even_deltas(pc),a1	; increments table
	bra	.start
.oddfrm	lea	base(pc),a0
	add.l	#odd_idx-base,a0
	lea	odd_deltas(pc),a1
.start	lea	base(pc),a2
	add.l	#archlst-base,a2

	lsr.w	#1,d0			; offset calc: 2 tables, even and odd
	mulu	#nrarches,d0		; nr of arch indices per delta line, bytes
	moveq	#0,d7			; arch index 2*(0,1,...,nrarches-1)
	moveq	#0,d6			; delta for arch
	;moveq	#0,d5			; clear circle coordinate table offset
.arloop	move.l	(a2)+,a3		; get coordinate table for curr arch
	move.b	(a1,d0.w),d6		; increment/decrement for arch (delta)
	beq	.nojob			; increment of 0 - nothing to do
	blt	.rmbob			; decrement, remove bobs
.addbob	addq.w	#1,(a0,d7.w)		; pre-increment coord index, a0 fixed
	move.w	(a0,d7.w),d5		; offset in circles coordinate table
	lsl.w	#2,d5			; offset from word pairs (x,y) to bytes
	movem.w	(a3,d5.w),d1/d2		;
	move.w	#%1111110000000000,d4	; minterm bits, lower byte 0. OR mask
	bsr	drawbob			; blit new bob
	subq.b	#1,d6			; one bob fewer to plot
	bne	.addbob
	bra	.nojob
.rmbob	;moveq	#0,d5
	move.w	(a0,d7.w),d5		; coordinate index
	lsl.w	#2,d5			; 2 coordinate words, 4 bytes offset
	movem.w	(a3,d5.w),d1/d2
	move.w	#%0000110000000000,d4 	; minterm bits for erasing the shape
	bsr	drawbob
	subq.w	#1,(a0,d7.w)		; update coordinate index (one bob fewer)
	addq.b	#1,d6			; one bob fewer to plot (negative nrs)
	bne	.rmbob
	move.w	(a0,d7.w),d5		; repaint the current bob, as we have
	lsl.w	#2,d5			; bitten off parts before. The code is
	movem.w	(a3,d5.w),d1/d2
	move.w	#%1111110000000000,d4	; minterm bits, lower byte 0. OR mask
	bsr	drawbob			; blit new bob
.nojob	addq.w	#2,d7			; next entry in index table (.w)
	addq.w	#1,a1			; next entry in delta table (.b, d0 fixed)
	cmp.w	#nrarches<<1,d7		; are we done with all arches?
	bne	.arloop
	move.w	v_animstep(a5),d0	; increase animation counter and take
	addq.w	#1,d0			; care of overflow by resetting counter
	cmp.w	#deltalstlen<<1,d0	; steps: length even and odd delta list
	bne	.nreset
	moveq	#0,d0
.nreset	move.w	d0,v_animstep(a5)
	rts


*------ draw the bob -----------------------------------------------------*
; Input:
; d1.l	x pos
; d2.l	y pos
; d4.w	Bits 15-8 minterm LF7-0, assuming sources A and B, destination D.
; a4    bitplane
; a5	pointer to vars
; a6	custom chip base address ($dff000)
; registers modified: d1.l-d4.l
;
; A  B  C  D  BLTCON0 bit
; -  -  -  - ------------
; 0  0  0  0         0
; 0  0  1  0         1
; 0  1  0  1         2
; 0  1  1  1         3
; 1  0  0  0         4
; 1  0  1  0         5
; 1  1  0  0         6
; 1  1  1  0         7
;
; LF = 11111100 for OR blit and LF = 00001100 for REMOVE blit
; LF, logic function minterm bits; ASH, shift value A source; use, channel use
;         LF bits  ASH use channel
;         765432103210ABCD
drawbob	move.b	#%00001101,d4		; upper byte is LF minterm, swapped later
	move.l	d1,d3			; d1.l = xpos
	asl.l	#4,d3			; shift to suitable ASH bit position
	or.b	d3,d4			; becomes ASH3-0 after next ror.w
	ror.w	#8,d4			; align such that LF7 is bit 15.
	asr.l	#3,d1			; calc byte pos (signed division by 8)
	add.l	a4,d1			;

	asl.w	#6,d2			; = muls #cwpwidth,d1
	add.l	d1,d2			; final byte to start blitting to

	bsr	waitblitter		; XXX could turn this into a macro!
	moveq	#cwpwidth-4,d1		;
	move.w	d1,BLTBMOD(a6)		; modulo B
	move.w	#-2,BLTAMOD(a6)		; modulo A
	move.w	d1,BLTDMOD(a6)		; modulo D
	move.w	d4,BLTCON0(a6)		; bltcon0
	move.l	#$ffff0000,BLTAFWM(a6)	; source A mask first (.w) and last (.w)
	move.w	#0,BLTCON1(a6)		; bltcon1

	move.l	v_ball(a5),BLTAPT(a6)	; source A, fixed: ball
	move.l	d2,BLTDPT(a6)		; destination D
	move.l	d2,BLTBPT(a6)		; source B
	move.w	#(ballsize>>1)<<6+2,BLTSIZE(a6)	; h9-h0,w5-w0 - access starts blitter
	rts


; a5	pointer to variables
; a6	must point to $dff000
; Update copper list to create flash effect if the desired animation frames are
; reached.
updateclist
	move.l	b_clistcw1(pc),a1
	add.w	#clarchcolors-clistcw1,a1 ; next byte will be: WAIT command y pos

	move.l	v_clcolptr(a5),a0
	moveq	#0,d0
	move.b	(a0)+,d0		; animation step nr for copper action
	cmp.w	v_animstep(a5),d0
	bne	.done

	move.l	v_actors(a5),d7		; process actors
	btst	#actor_fx_circwave,d7	;
	beq	.nofx			;

	lea	coppercolors(pc),a2	; table with color RGB values
	moveq	#numshades-1,d7		; number of colors
.loop	move.b	(a0)+,d0		; copper WAIT y pos
	beq	.nochg			; 0: jump to next color, no change
	add.b	#$2c,d0			; first visible line is $2c = 44
	cmp.b	#$fd,d0			; safety check
	bcc	.nochg			;
	move.b	d0,(a1)+		; WAIT command
	move.b	#$07,(a1)+
	move.l	#$fffe<<16+COLOR01,(a1)+	
	move.w	(a2),(a1)+		; color from lookup table
.nochg	addq.w	#2,a2			; next color
	dbf	d7,.loop

	lea	copperylstend(pc),a2
	cmp.l	a0,a2
	bne	.nores
	lea	copperylst(pc),a0
.nores	move.l	a0,v_clcolptr(a5)
.nofx	move.l	#$008a0000,(a1)+	; jump to clist cw 2
.done	rts


*------	COPPER INSTRUCTION LIST ------------------------------------------*

clistcw1
	dc.w	$1007,$fffe ; chance for player to update clist in time
	dc.w	DIWSTRT,$2c81 		; display window start
	dc.w	DIWSTOP,$2cc1 		; display window stop
	dc.w	DDFSTRT,$0038		; display bitplane data fetch start
	dc.w	DDFSTOP,$00d0		; display bitplane data fetch stop

	dc.w	BPLCON0,$1200		; bitplane control register (one bitplane)
	dc.w	BPLCON1,$0000		; bitplane control register (scroll values)
	dc.w	BPLCON2,$0000 		; do not have priority
	dc.w	BPL1MOD,cwincwviswidth 	; bitplane modulo (odd planes)
	dc.w	BPL2MOD,cwincwviswidth	; bitplane modulo (even planes)

bplcw	dc.w	$00e0,0,$00e2,0
colscw1	dc.w	$0180,0,$0182,0

spritepointersclistcw
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

lsplinecw	equ $13
	dc.b	lsplinecw,$07,$ff,$fe
	dc.w	INTREQ,$8010 		; trigger coper (lsp)
	dc.b	lsplinecw+11,$07,$ff,$fe

	if timing
	dc.w	COLOR00,$00f0
	endif
lspdmacon
	dc.w	DMACON
	dc.b	$80,0

	;; Space for flash effect. Recall that the copper list inserted here
	;; varies in size.
clarchcolors
clistcw1end

ballsize		equ	5*2
clistcw1size		equ	clistcw1end-clistcw1
clistcw1extrasize	equ	numshades*8+64 ; 64 extra space (end of clist and stuff)
