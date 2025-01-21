#include once "default_types_v1.bas"
#include once "string_list_v1.bas"
#include once "entry_list_v1.bas"
#include once "word_range_list_v1.bas"
#include "string.bi"

const ROW_DIM = 1
const COL_DIM = 2

const MAX_LEN = 24

type word_placement
	dim as isz ori, row, col ', idx
end type

type wordmap_type
	dim as any ptr pHandle 'thread handle
	dim as isz done = 0
	dim as isz cntBA, cntAB, cntCC, cntCP, cntNC, cntOK, cntALL
	dim as isz solvedCount = 0, saveCount = 0
	dim as isz numCrossings = 0, maxCrossings = 0
	dim as isz ubr, ubc 'maps bounds
	'for statistics
	dim as isz tryCount, placeCount, loopCount
	dim as isz cntSkipRowNoFit, cntSkipColNoFit
	dim as isz cntSkipRowNoCon, cntSkipColNoCon
	'for optimised search
	dim as wr_list sdRowList(any), sdColList(any)
	dim as u32 rowBitMask(any), colBitMask(any)
	'main working data
	dim as entry_list_type entries 'local copied version
	dim as u8 wordMap(any, any) 'y/row, x/col 2d-map
	dim as word_placement wpList(any) 'list to save + for optimised search
	dim as word_placement wpBest(any)
	'subroutines
	declare sub init(rows as isz, cols as isz, entries as entry_list_type)
	declare sub show(showWords as isz)
	declare sub saveAsText(fileName as string)
	declare sub saveAsImage(fileNameBmp as string)
	declare sub saveAsTask(fileNameClues as string)
	declare function countCrossings() as isz
	declare function testWordPos(byref wp as word_placement, word as string) as isz
	declare function copyToMap(wp as word_placement, word as string) as isz
	declare function copyToMapSave(wp as word_placement, word as string, erasedStr as string) as isz
	declare sub processSolved(depth as isz)
	declare sub processShow()
	declare function recursiveBase(depth as isz) as isz
	declare function recursiveOpt(depth as isz) as isz
	declare function recursiveSmart0() as isz
	declare function recursiveSmart1(depth as isz) as isz
end type

sub wordmap_type.init(rows as isz, cols as isz, entries as entry_list_type)
	redim wordMap(0 to rows - 1, 0 to cols - 1)
	ubr = ubound(wordMap, ROW_DIM)
	ubc = ubound(wordMap, COL_DIM)
	for row as isz = 0 to ubr
		for col as isz = 0 to ubc
			wordMap(row, col) = asc(".") 'int(rnd * 26) + 65 'A..Z
		next
	next
	redim rowBitMask(ubr)
	redim colBitMask(ubc)
	redim sdRowList(ubr)
	redim sdColList(ubc)
	this.entries = entries 'create copy
	redim wpList(ubound(entries.entry))
	redim wpBest(ubound(entries.entry))
end sub

sub wordmap_type.show(showWords as isz)
	'dim as isz dotPosOffset = 0
	dim as isz x0 = 300
	dim as isz y0 = 100
	line(x0-10,y0-7)-step((ubc + 1) * 24+3,(ubr + 1) * 24+3),&h808080, bf
	for row as isz = 0 to ubr
		for col as isz = 0 to ubc
			dim as u8 char = wordMap(row, col)
			dim as isz x = x0 + col * 24
			dim as isz y = y0 + row * 24
			if char <> asc(".") then
				line(x-7, y-4)-step(21,21),&hffffff,bf
				line(x-8, y-5)-step(23,23),&h000000,b
				line(x-9, y-6)-step(25,25),&h000000,b
				if showWords <> 0 then
					draw string (x, y), chr(char), &h000000
				end if
			end if
		next
	next
end sub

sub wordmap_type.saveAsText(fileName as string)
	if open(fileName for output as #1) <> 0 then
		print "Error opening file: "; fileName
	else
		for row as isz = 0 to ubr
			for col as isz = 0 to ubc
				dim as u8 char = wordMap(row, col)
				print #1, chr(char);
			next
			print #1, ""
		next
		close #1
	end if
end sub

sub wordmap_type.saveAsImage(fileNameBmp as string)
	bsave(fileNameBmp, 0) 'save screen
end sub

sub wordmap_type.saveAsTask(fileNameClues as string)
	dim as string_list vert, horz
	'split vert / horz
	for i as isz = 0 to ubound(wpList)
		if wpList(i).ori = 0 then
			horz.add(entries.entry(i).clue)
		else
			vert.add(entries.entry(i).clue)
		end if
	next
	'scramble, else long to short
	horz.shuffle()
	vert.shuffle()
	'now save
	if open(fileNameClues for output as #1) <> 0 then
		print "Error opening file: "; fileNameClues
	else
		print #1, "=== horizontal clues ==="
		for i as isz = 0 to ubound(horz.list)
			print #1, horz.list(i)
		next
		print #1, "=== vertical clues ==="
		for i as isz = 0 to ubound(vert.list)
			print #1, vert.list(i)
		next
		close #1
	end if
end sub


'count up if cross, tee or corner
function wordmap_type.countCrossings() as isz
	dim as isz count = 0
	for row as isz = 0 to ubr
		for col as isz = 0 to ubc
			if wordMap(row, col) <> asc(".") then
				dim as isz flagH = 0, flagV = 0
				if row > 0 then if wordMap(row - 1, col) <> asc(".") then flagV += 1
				if row < ubr then if wordMap(row + 1, col) <> asc(".") then flagV += 1
				if col > 0 then if wordMap(row, col - 1) <> asc(".") then flagH += 1
				if col < ubc then if wordMap(row, col + 1) <> asc(".") then flagH += 1
				if (flagV > 0) and (flagH > 0) then count += 1
			end if
		next
	next
	return count
end function

'does word not collide & not direct neighbour to other words?
'steps:
'1. check for no character before and after word
'2a. check char collision & characters parallel to word 
'2b. characters parallel to word
'2c. check for at least 1 connection

'cntBA = before or after (horz)
'cntAB = above or below (vert)
'cntCC = char collisiom
'cntCP = char parallel
'cntNC = not connected
'cntOK = placement OK

function wordmap_type.testWordPos(byref wp as word_placement, word as string) as isz
	dim as isz count, ori = wp.ori, col = wp.col, row = wp.row
	dim as isz lastChar = len(word) - 1
	cntALL += 1
	'1. voor en achter woord mag niks
	if ori = 0 then 'horz
		if col > 0 then
			if wordMap(row, col - 1) <> asc(".") then cntBA += 1 : return -1
		end if
		dim as isz lastCol = col + lastChar
		if lastCol < ubc then
			if wordMap(row, lastCol + 1) <> asc(".") then cntBA += 1 : return -1
		end if
	else
		if row > 0 then
			if wordMap(row - 1, col) <> asc(".") then cntAB += 1 : return -1
		end if
		dim as isz lastRow = row + lastChar
		if lastRow < ubr then
			if wordMap(lastRow + 1, col) <> asc(".") then cntAB += 1 : return -1
		end if
	end if
	'2. zijwaarts mag niks tenzij kruising
	if ori = 0 then 'horz
		for i as isz = 0 to lastChar
			if wordMap(row, col + i) = asc(".") then 'skip check above/under if character present = kruising
				'check above and below
				if row > 0 then if wordMap(row - 1, col + i) <> asc(".") then cntCP += 1 : return -1
				if row < ubr then if wordMap(row + 1, col + i) <> asc(".") then cntCP += 1 : return -1
				count += 1
			else
				if wordMap(row, col + i) <> word[i] then cntCC += 1 : return -1 'char collision
			end if
		next
	else 'vert
		for i as isz = 0 to lastChar
			if wordMap(row + i, col) = asc(".") then
				'check left and right? 
				if col > 0 then if wordMap(row + i, col - 1) <> asc(".") then cntCP += 1 : return -1
				if col < ubc then if wordMap(row + i, col + 1) <> asc(".") then cntCP += 1 : return -1
				count += 1
			else
				if wordMap(row + i, col) <> word[i] then cntCC += 1 : return -1 'char collision
			end if
		next
	end if
	'check for no connection at all
	if count = len(word) then cntNC += 1 : return -1 'all "."
	cntOK += 1
	return 0 'ok
end function

function wordmap_type.copyToMap(wp as word_placement, word as string) as isz
	'if testToMapBounds(wordMap(), row, col, ori, word) < 0 then return -1
	'if testToMapCollide(wordMap(), row, col, ori, word) < 0 then return -1
	dim as isz row = wp.row
	dim as isz col = wp.col
	if wp.ori = 0 then 'horz
		for i as isz = 0 to len(word) - 1
			wordMap(row, col + i) = word[i]
		next
	else 'vert
		for i as isz = 0 to len(word) - 1
			wordMap(row + i, col) = word[i]
		next
	end if
	return 0 'ok
end function

function wordmap_type.copyToMapSave(wp as word_placement, word as string, erasedStr as string) as isz
	dim as isz row = wp.row
	dim as isz col = wp.col
	erasedStr = space(len(word))
	if wp.ori = 0 then 'horz
		for i as isz = 0 to len(word) - 1
			erasedStr[i] = wordMap(row, col + i)
			wordMap(row, col + i) = word[i]
		next
	else 'vert
		for i as isz = 0 to len(word) - 1
			erasedStr[i] = wordMap(row + i, col)
			wordMap(row + i, col) = word[i]
		next
	end if
	return 0 'ok
end function

sub wordmap_type.processSolved(depth as isz)
	dim as string fileName
	solvedCount += 1
	numCrossings = countCrossings()
	if numCrossings > maxCrossings then 'only save better versions
		maxCrossings = numCrossings
		'save best placement
		for iwp as isz = 0 to ubound(wpList)
			wpBest(iwp) = wpList(iwp)
		next
		'CLS 'DEBUG
		'~ line(50-10,50-2)-step(200,100), &h00202020, bf
		'~ draw string (50, 50), "depth:        " + str(depth)
		'~ draw string (50, 70), "solvedCount:  " + str(solvedCount)
		'~ draw string (50, 90), "numCrossings: " + str(numCrossings)
		'~ draw string (50,110), "tryCount:     " + str(tryCount)
		'~ draw string (50,130), "placeCount:   " + str(placeCount)
		'~ 'draw string (50,110), "numChar:      " + str(countChar(wordMap()))
		'~ 'list word placement numbers
		'~ 'for iw as isz = 0 to depth
		'~ '	draw string (50, 130 + iw * 20), str(wpList(iw).ori) & "," & str(wpList(iw).row) & "," & str(wpList(iw).col)
		'~ 'next
		'~ 'if numCrossings >= 40 then
		'~ if saveCount <= 100 then 'limit files
			'~ 'fileName = "output\solve_" & format(saveCount, "0000")
			'~ 'show(0) 'without text
			'~ 'saveAsImage(fileName & ".bmp")
			'~ fileName = "output\solved_" & format(saveCount, "0000")
			'~ show(1) 'with text
			'~ 'saveAsImage(fileName & ".bmp")
			'~ 'saveAsTask(fileName & ".txt")
			'~ 'getkey()
		'~ end if
		saveCount += 1
	end if
	sleep 1,1
end sub

sub wordmap_type.processShow()
	this.show(1)
	line(45, 150)-step(130, 25*16), &h00202020, bf
	'dim as f64 t = timer
	'draw string (450,150), "tc " + str(int( (tryCount / (t-t0)) * 1e-6 )) + " M"
	'draw string (450,170), "pc " + str(int( (placeCount / (t-t0)) * 1e-3 )) + " k"
	'draw string (50,190), "cntBA " + str(cntBA \ 1000000)
	'draw string (50,210), "cntBA " + str(cntAB \ 1000000)
	'draw string (50,230), "cntCC " + str(cntCC \ 1000000)
	'draw string (50,250), "cntCP " + str(cntCP \ 1000000)
	'draw string (50,270), "cntNC " + str(cntNC \ 1000000)
	'draw string (50,290), "cntOK " + str(cntOK \ 1000000)
	'draw string (50,310), "cntALL " + str(cntALL \ 1000000)
	'for i as integer = 0 to 24
	'	draw string (50, 150 + i*16), str(i) + " " + str(depthCount(i))
	'next
	for iRow as integer = 0 to ubr
		'draw string (50, 190 + iRow * 16), str(rowCharCount(iRow))
		'draw string (50, 190 + iRow * 16), hex(rowBitMask(iRow), 7)
	next
	for iCol as integer = 0 to ubc
		'draw string (120, 190 + iCol * 16), str(colCharCount(iCol))
		'draw string (120, 190 + iCol * 16), hex(colBitMask(iCol), 7)
	next
	sleep 1
end sub

'-------------------------------------------------------------------------------

function wordmap_type.recursiveBase(depth as isz) as isz
	dim as isz row, col
	dim as string word = entries.entry(depth).word
	dim as word_placement wp = type(0, 0, 0)
	dim as string * MAX_LEN erasedStr
	wp.ori = 0 'horizontal
	for row = 0 to ubr
		wp.row = row
		for col = 0 to ubc - (len(word) - 1)
			wp.col = col
			if depth = 0 orelse testWordPos(wp, word) = 0 then
				copyToMapSave(wp, word, erasedStr) 'place word & save overwritten chars
				wpList(depth) = wp
				'processBoardShow(wordMap()) : getkey()
				if depth < entries.num - 1 then
					recursiveBase(depth + 1)
				else 'all placed
					processSolved(depth)
				end if
				copyToMap(wp, erasedStr) 'remove word again (restore grid to previous state)
			end if
		next
	next
	wp.ori = 1 'vertical
	for col = 0 to ubc
		wp.col = col
		for row = 0 to ubr - (len(word) - 1)
			wp.row = row
			if depth = 0 orelse testWordPos(wp, word) = 0 then
				copyToMapSave(wp, word, erasedStr) 'place word & save overwritten chars
				wpList(depth) = wp
				'processBoardShow(wordMap()) : getkey()
				if depth < entries.num - 1 then
					recursiveBase(depth + 1)
				else 'all placed
					processSolved(depth)
				end if
				copyToMap(wp, erasedStr) 'remove word again (restore grid to previous state)
			end if
		next
	next
	if depth = 1 then sleep 1 : print ".";
	return 0 'no more positions to try
end function

'-------------------------------------------------------------------------------

function wordmap_type.recursiveOpt(depth as isz) as isz
	dim as isz row, col, rowMax, colMax
	dim as string word = entries.entry(depth).word
	dim as u32 wordBitMask = entries.wordBitMask(depth)
	dim as isz wordLen = len(word)
	dim as word_placement wp = type(0, 0, 0)
	dim as string * MAX_LEN erasedStr
	dim as u32 rowBitMaskPrev(ubr)
	dim as u32 colBitMaskPrev(ubc)
	'depthCount(depth) += 1

	wp.ori = 0 'horizontal
	rowMax = ubr 'ok
	colMax = ubc - (wordLen - 1)
	'colMax = ubound(wordMap, COL_DIM)
	for row = 0 to rowMax
		wp.row = row
		if depth > 0 then 'skip test for depth = 0
			if sdRowList(row).maxGap(ubc) <= wordLen then cntSkipRowNoFit += 1 : continue for 'does not fit
			'if rowCharCount(row) >= colMax then cntSkipRowNoFit += 1 : continue for 'does not fit
			if (wordBitMask and rowBitMask(row)) = 0 then cntSkipRowNoCon +=1 : continue for 'no matching chars
		end if
		for col = 0 to colMax
			wp.col = col
			'tryCount += 1
			if depth = 0 orelse testWordPos(wp, word) = 0 then
				'placeCount += 1
				'------------------------------------------------
				rowBitMaskPrev(row) = rowBitMask(row) 'save row bit mask
				rowBitMask(row) or= wordBitMask 'set row bit mask
				for iChar as isz = 0 to wordLen - 1 
					dim as isz iCol = col + iChar
					colBitMaskPrev(iCol) = colBitMask(iCol) 'save col bit masks
					dim as isz bitNum = word[iChar] - asc("A")
					colBitMask(iCol) or= (1 shl bitNum) 'set col bit masks
				next
				'------------------------------------------------
				'rowCharCount(row) += wordLen
				sdRowList(row).insert(word, col)
				copyToMapSave(wp, word, erasedStr) 'place word & save overwritten chars
				wpList(depth) = wp
				'---
				'processBoardShow(wordMap()) : getkey()
				'---
				if depth < entries.num - 1 then
					recursiveOpt(depth + 1)
				else 'all placed
					processSolved(depth)
				end if
				copyToMap(wp, erasedStr) 'remove word again (restore grid to previous state)
				'rowCharCount(row) -= wordLen
				sdRowList(row).remove(col)
				'------------------------------------------------
				rowBitMask(row) = rowBitMaskPrev(row) 'restore row bit mask
				for iChar as isz = 0 to wordLen - 1 
					dim as isz iCol = col + iChar
					colBitMask(iCol) = colBitMaskPrev(iCol) 'restore col bit masks
				next
				'------------------------------------------------
			end if
		next
	next
	
	wp.ori = 1 'vertical
	colMax = ubc 'ok
	rowMax = ubr - (wordLen - 1)
	for col = 0 to colMax
		wp.col = col
		if depth > 0 then 'skip test for depth = 0
			if sdColList(col).maxGap(ubr) <= wordLen then cntSkipColNoFit +=1 : continue for 'does not fit
			'if colCharCount(col) >= rowMax then cntSkipColNoFit +=1 : continue for 'does not fit
			if (wordBitMask and colBitMask(col)) = 0 then cntSkipColNoCon += 1 : continue for 'no matching chars
		end if
		for row = 0 to rowMax
			wp.row = row
			'tryCount += 1
			if depth = 0 orelse testWordPos(wp, word) = 0 then
				'placeCount += 1
				'------------------------------------------------
				colBitMaskPrev(col) = colBitMask(col) 'save col bit mask
				colBitMask(col) or= wordBitMask 'set col bit mask
				for iChar as isz = 0 to wordLen - 1 
					dim as isz iRow = row + iChar
					rowBitMaskPrev(iRow) = rowBitMask(iRow) 'save col bit masks
					dim as isz bitNum = word[iChar] - asc("A")
					rowBitMask(iRow) or= (1 shl bitNum) 'set col bit masks
				next
				'------------------------------------------------
				'colCharCount(col) += wordLen
				sdColList(col).insert(word, row)
				copyToMapSave(wp, word, erasedStr) 'place word & save overwritten chars
				wpList(depth) = wp
				'---
				'processBoardShow(wordMap()) : getkey()
				'---
				if depth < entries.num - 1 then
					recursiveOpt(depth + 1)
				else 'all placed
					processSolved(depth)
				end if
				copyToMap(wp, erasedStr) 'remove word again (restore grid to previous state)
				'colCharCount(col) -= wordLen
				sdColList(col).remove(row)
				'------------------------------------------------
				colBitMask(col) = colBitMaskPrev(col) 'restore row bit mask
				for iChar as isz = 0 to wordLen - 1 
					dim as isz iRow = row + iChar
					rowBitMask(iRow) = rowBitMaskPrev(iRow) 'restore col bit masks
				next
				'------------------------------------------------
			end if
		next
	next

	if depth = 1 then sleep 1 : print ".";
	'if (placeCount and &h0ffff) = 0 then processBoardShow(wordMap())

	return 0 'no more positions to try
end function

'-------------------------------------------------------------------------------

function wordmap_type.recursiveSmart0() as isz
	dim as isz depth = 0
	dim as word_placement wp '= type(0, 0, 0)
	dim as string word = entries.entry(depth).word
	dim as string erasedStr
	wp.ori = 0 'horizontal
	for row as isz = 0 to ubr
		wp.row = row
		for col as isz = 0 to ubc - (len(word) - 1)
			wp.col = col
			'sleep 1 : print ".";
			sleep 1,1 : loopCount += 1
			copyToMapSave(wp, word, erasedStr)
			wpList(depth) = wp
			recursiveSmart1(depth + 1)
			copyToMap(wp, erasedStr) 'undo
		next
	next
	wp.ori = 1 'vertical
	for col as isz = 0 to ubc
		wp.col = col
		for row as isz = 0 to ubr - (len(word) - 1)
			wp.row = row
			'sleep 1 : print ".";
			sleep 1,1 : loopCount += 1
			copyToMapSave(wp, word, erasedStr)
			wpList(depth) = wp
			recursiveSmart1(depth + 1)
			copyToMap(wp, erasedStr) 'undo
		next
	next
	done = 1
	return 0 'no more positions to try
end function

function wordmap_type.recursiveSmart1(depth as isz) as isz
	dim as string word = entries.entry(depth).word
	dim as isz wordLenMin1 = len(word) - 1
	dim as u32 wordBitMask = entries.wordBitMask(depth)
	dim as word_placement wpn, wpb '= type(0, 0, 0) 'wp new' & 'wp board'
	dim as string erasedStr, wordPlaced
	'
	for ipw as isz = 0 to depth - 1 'previous place words
		if wordBitMask and entries.wordBitMask(ipw) = 0 then continue for 'no matching chars, next word
		wordPlaced = entries.entry(ipw).word
		for ichpw as isz = 0 to len(wordPlaced) - 1 'loop chars placed words
			dim as u8 wpch = wordPlaced[ichpw]
			'dim as isz bitNum = wpch - asc("A")
			'if wordBitMask and (1 shl bitNum) = 0 then continue for 'char not in new word, next char
			for ichw as isz = 0 to wordLenMin1 'loop chars new word
				if wpch = word[ichw] then
					wpb = wpList(ipw)
					'wpn.ori = (wpb.ori + 1) and 1 '0->1, 1->0
					if wpb.ori = 1 then
						wpn.ori = 0 'horz
						wpn.row = wpb.row + ichpw
						wpn.col = wpb.col - ichw
						if wpn.col < 0 then continue for
						if wpn.col + wordLenMin1 > ubc then continue for
					else
						wpn.ori = 1 'vert
						wpn.row = wpb.row - ichw
						wpn.col = wpb.col + ichpw
						if wpn.row < 0 then continue for
						if wpn.row + wordLenMin1 > ubr then continue for
					end if
					tryCount += 1
					if testWordPos(wpn, word) = 0 then 'ok
						placeCount += 1
						'place word & save overwritten chars
						copyToMapSave(wpn, word, erasedStr)
						wpList(depth) = wpn
						if depth < entries.num - 1 then
							recursiveSmart1(depth + 1)
						else 'all placed
							processSolved(depth)
						end if
						copyToMap(wpn, erasedStr) 'undo
					end if
				end if
			next
		next
	next
	return 0 'no more positions to try
end function

