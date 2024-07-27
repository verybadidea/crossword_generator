#include "crt/stdlib.bi"
#include "crt/string.bi"
#include "string.bi"

#define u8 ubyte
#define i32 long
#define isz integer
#define f64 double

type entry_type
	dim as string word, clue
end type

type word_placement
	dim as isz ori, row, col ', idx
end type

dim shared as isz numEntries 'acces needed by recursiveMagic()
dim shared as entry_type entry(any) 'acces needed by recursiveMagic()
dim shared as isz solvedCount = 0
dim shared as isz numCrossings = 0
dim shared as isz maxDepth = 0
dim shared as isz maxCrossings = 0
dim shared as word_placement wpList(any)

'dim shared as u8 debugMap(any, any) 'y/row, x/col

'-------------------------------------------------------------------------------

function split2(inputStr as string, delChar as string, splitStr1 as string, splitStr2 as string) as isz
	dim as isz delPos = instr(inputStr, delChar)
	splitStr1 = trim(mid(inputStr, 1, delPos - 1))
	splitStr2 = trim(mid(inputStr, delPos + 1))
	return 0
end function

'-------------------------------------------------------------------------------

'read data lines
function readFromData(entry() as entry_type) as isz
	dim as string wordStr, clueStr
	do
		read wordStr, clueStr
		if wordStr = "EOD" then
			exit do
		else
			dim as isz ub = ubound(entry) + 1
			redim preserve entry(ub)
			entry(ub).word = ucase(wordStr)
			entry(ub).clue = clueStr
		end if
	loop
	return ubound(entry) + 1
end function

'read file
function readFromFile(entry() as entry_type, wordFile as string) as isz
	dim as string textStr, wordStr, clueStr
	if open(wordFile, for input, as #1) <> 0 then
		print "Error opening: "; wordFile
		return -1
	else
		while not eof(1)
			line input #1, textStr
			if textStr = "" then exit while
			split2(textStr, ":", wordStr, clueStr)
			dim as isz ub = ubound(entry) + 1
			redim preserve entry(ub)
			entry(ub).word = ucase(wordStr)
			entry(ub).clue = clueStr
		wend
		close #1
	end if
	return ubound(entry) + 1
end function

'list words + description
sub listEntries(entry() as entry_type)
	for i as isz = 0 to ubound(entry)
		print entry(i).word, entry(i).clue
	next
end sub

'sort longest to shortest word
function entryCmp cdecl(byval pAny1 as const any ptr, byval pAny2 as const any ptr) as i32
	'cast to proper type
	dim as entry_type ptr pProb1 = cast(entry_type ptr, pAny1)
	dim as entry_type ptr pProb2 = cast(entry_type ptr, pAny2)
	'Compare the values
	if len(pProb1->word) < len(pProb2->word) then
		return +1
	elseif len(pProb1->word) > len(pProb2->word) then
		return -1
	else
		return 0
	end if
end function

sub sortEntries(entry() as entry_type)
	qsort(@entry(0), ubound(entry) + 1, sizeof(entry_type), @entryCmp)
end sub

'-------------------------------------------------------------------------------

const ROW_DIM = 1
const COL_DIM = 2

'~ sub allocMap(wordMap() as u8, rows as isz, cols as isz
	'~ redim wordMap(0 to rows-1, 0 to cols-1)
'~ end sub 

sub initMap(wordMap() as u8, rows as isz, cols as isz)
	redim wordMap(0 to rows - 1, 0 to cols - 1)
	for row as isz = 0 to ubound(wordMap, ROW_DIM)
		for col as isz = 0 to ubound(wordMap, COL_DIM)
			wordMap(row, col) = asc(".") 'int(rnd * 26) + 65 'A..Z
		next
	next
end sub

sub copyMap(srcMap() as u8, destMap() as u8)
	dim as isz ubr = ubound(srcMap, ROW_DIM)
	dim as isz ubc = ubound(srcMap, COL_DIM)
	'dim as isz nBytes = sizeof(srcMap) * (ubRow + 1) * (ubCol + 1)
	redim destMap(0 to ubr, 0 to ubc)
	'memcpy(@destMap(0,0), @srcMap(0,0), nBytes)
	for r as isz = 0 to ubr
		for c as isz = 0 to ubc
			destMap(r, c) = srcMap(r, c)
		next
	next
end sub

sub showMap(wordMap() as u8, showWords as isz)
	'dim as isz dotPosOffset = 0
	dim as isz x0 = 200
	dim as isz y0 = 100
	dim as isz ubr = ubound(wordMap, ROW_DIM)
	dim as isz ubc = ubound(wordMap, COL_DIM)
	line(200-10,100-7)-step((ubc + 1) * 24+3,(ubr + 1) * 24+3),&h808080, bf
	for row as isz = 0 to ubr
		for col as isz = 0 to ubc
			dim as u8 char = wordMap(row, col)
			dim as isz x = 200 + col * 24
			dim as isz y = 100 + row * 24
			if char <> asc(".") then
				'if debugMap(row, col) = 1 then
				'	line(x-7, y-4)-step(21,21),&hffffBf,bf
				'else
					line(x-7, y-4)-step(21,21),&hffffff,bf
				'end if
				line(x-8, y-5)-step(23,23),&h000000,b
				line(x-9, y-6)-step(25,25),&h000000,b
				if showWords <> 0 then
					draw string (x, y), chr(char), &h000000
				end if
			end if
		next
	next
end sub

sub saveMap1(wordMap() as u8, fileName as string)
	if open(fileName for output as #1) <> 0 then
		print "Error opening file: "; fileName
	else
		for row as isz = 0 to ubound(wordMap, ROW_DIM)
			for col as isz = 0 to ubound(wordMap, COL_DIM)
				dim as u8 char = wordMap(row, col)
				print #1, chr(char);
			next
			print #1, ""
		next
		close #1
	end if
end sub

sub saveMap2(wordMap() as u8, entry() as entry_type, wpList() as word_placement, fileNameBmp as string, fileNameClues as string)
	bsave(fileNameBmp, 0) 'save screen
	if open(fileNameClues for output as #1) <> 0 then
		print "Error opening file: "; fileNameClues
	else
		print #1, "=== horizontal clues ==="
		for i as isz = 0 to ubound(wpList)
			if wpList(i).ori = 0 then print #1, entry(i).clue
		next
		print #1, "=== vertical clues ==="
		for i as isz = 0 to ubound(wpList)
			if wpList(i).ori = 1 then print #1, entry(i).clue
		next
		close #1
	end if
end sub


'count up if cross, tee or corner
function countCrossings(wordMap() as u8) as isz
	dim as isz count = 0
	dim as isz ubr = ubound(wordMap, ROW_DIM)
	dim as isz ubc = ubound(wordMap, COL_DIM)
	for row as isz = 0 to ubr
		for col as isz = 0 to ubc
			if wordMap(row, col) <> asc(".") then
				'debugMap(row, col) = 0
				dim as isz flagH = 0, flagV = 0
				if row > 0 then if wordMap(row - 1, col) <> asc(".") then flagV += 1
				if row < ubr then if wordMap(row + 1, col) <> asc(".") then flagV += 1
				if col > 0 then if wordMap(row, col - 1) <> asc(".") then flagH += 1
				if col < ubc then if wordMap(row, col + 1) <> asc(".") then flagH += 1
				if (flagV > 0) and (flagH > 0) then
					count += 1
					'debugMap(row, col) = 1
				end if
			end if
		next
	next
	return count
end function

'does word fit completely inside grid layout?
'~ function testToMapBounds(wordMap() as u8, byref wp as word_placement, word as string) as isz
	'~ if wp.col < 0 then return -1
	'~ if wp.row < 0 then return -1
	'~ if wp.ori = 0 then 'horz
		'~ if wp.row > ubound(wordMap, ROW_DIM) then return -1
		'~ if wp.col + len(word) - 1 > ubound(wordMap, COL_DIM) then return -1
	'~ else 'vert
		'~ if wp.col > ubound(wordMap, COL_DIM) then return -1
		'~ if wp.row + len(word) - 1 > ubound(wordMap, ROW_DIM) then return -1
	'~ end if
	'~ return 0 'ok
'~ end function

'does word not collide & not direct neighbour other words?
function testToMapCollide(wordMap() as u8, byref wp as word_placement, word as string) as isz
	dim as isz count, ori = wp.ori, col = wp.col, row = wp.row
	'voor en achter mag niks
	if ori = 0 then 'horz
		if col > 0 then
			if wordMap(row, col - 1) <> asc(".") then return -1
		end if
		dim as isz lastCol = col + len(word) - 1
		if lastCol < ubound(wordMap, COL_DIM) then
			if wordMap(row, lastCol + 1) <> asc(".") then return -1
		end if
	else
		if row > 0 then
			if wordMap(row - 1, col) <> asc(".") then return -1
		end if
		dim as isz lastRow = row + len(word) - 1
		if lastRow < ubound(wordMap, ROW_DIM) then
			if wordMap(lastRow + 1, col) <> asc(".") then return -1
		end if
	end if
	'zijwaarts mag niks tenzij kruising
	if ori = 0 then 'horz
		for i as isz = 0 to len(word) - 1
			if wordMap(row, col + i) = asc(".") then
				'check above and below
				if row > 0 then if wordMap(row - 1, col + i) <> asc(".") then return -1
				if row < ubound(wordMap, ROW_DIM) then if wordMap(row + 1, col + i) <> asc(".") then return -1
				count += 1
			end if
		next
	else 'vert
		for i as isz = 0 to len(word) - 1
			if wordMap(row + i, col) = asc(".") then
				'check above and below
				if col > 0 then if wordMap(row + i, col - 1) <> asc(".") then return -1
				if col < ubound(wordMap, COL_DIM) then if wordMap(row + i, col + 1) <> asc(".") then return -1
				count += 1
			end if
		next
	end if
	'check for no connection at all
	if count = len(word) then return -1 'all "."
	'check for char collision
	for i as isz = 0 to len(word) - 1
		if wordMap(row, col) <> asc(".") then
			if wordMap(row, col) <> word[i] then return -1 'char collision
		end if
		if ori = 0 then col += 1 else row += 1
	next
	'note: row or col now modified
	return 0 'ok
end function

function copyToMap(wordMap() as u8, wp as word_placement, word as string) as isz
	'if testToMapBounds(wordMap(), row, col, ori, word) < 0 then return -1
	'if testToMapCollide(wordMap(), row, col, ori, word) < 0 then return -1
	dim as isz row = wp.row
	dim as isz col = wp.col
	for i as isz = 0 to len(word) - 1
		wordMap(row, col) = word[i]
		if wp.ori = 0 then col += 1 else row += 1
	next
	return 0 'ok
end function

'nextpos function
function nextPos(wordMap() as u8, byref wp as word_placement, word as string) as isz
	if wp.ori = 0 then 'horz
		if wp.col + (len(word) - 1) < ubound(wordMap, COL_DIM) then
			wp.col += 1
		else
			wp.col = 0
			if wp.row < ubound(wordMap, ROW_DIM) then
				wp.row += 1
			else
				wp.row = 0
				wp.ori = 1
			end if
		end if
	else 'vert
		if wp.col < ubound(wordMap, COL_DIM) then
			wp.col += 1
		else
			wp.col = 0
			if wp.row + (len(word) - 1) < ubound(wordMap, ROW_DIM) then
				wp.row += 1
			else
				wp.row = 0
				wp.ori = 0
				return -1 'end of scan
			end if
		end if
	end if
	return 0
end function

function recursiveMagic(wordMap() as u8, depth as isz) as isz
	dim as u8 copiedMap(any, any)
	dim as string word = entry(depth).word
	dim as word_placement wp = type(0, 0, 0)
	do
		if depth = 0 orelse testToMapCollide(wordMap(), wp, word) = 0 then 'no character collision
			copyMap(wordMap(), copiedMap())
			copyToMap(copiedMap(), wp, word)
			wpList(depth) = wp

			if depth < numEntries - 1 then
				recursiveMagic(copiedMap(), depth + 1)
			else
				'all placed
				solvedCount += 1
				numCrossings = countCrossings(copiedMap())
				if numCrossings >= maxCrossings then
					maxCrossings = numCrossings
					CLS 'DEBUG
					draw string (50,70), str(solvedCount)
					draw string (50,50), str(depth)
					draw string (50,90), str(numCrossings)
					showMap(copiedMap(), 0)
					for iw as isz = 0 to depth
						draw string (50, 130 + iw * 20), str(wpList(iw).ori) & "," & str(wpList(iw).row) & "," & str(wpList(iw).col)
					next
					dim as string fileName = "output\solve_" & format(solvedCount, "0000")
					saveMap2(copiedMap(), entry(), wpList(), fileName & ".bmp", fileName & ".txt")
					'save to file
					'if solvedCount <= 10 then 'limit files
						'saveMap(copiedMap(), "output\solve_" & format(solvedCount, "0000") & ".txt")
					'end if
				end if
				sleep 1
			end if
			erase copiedMap
			if depth = 0 then print ".";
		end if
	loop while nextPos(wordMap(), wp, word) = 0 'succes
	return 0 'no more positions to try
end function

'-------------------------------------------------------------------------------

const SW = 800, SH = 600
screenres SW, SH, 32
width SW \ 8, SH \ 16

'numEntries = readFromFile(entry(), "words.txt")
numEntries = readFromData(entry())
sortEntries(entry())
listEntries(entry())

sleep 500

dim as u8 wordMap(any, any) 'y/row, x/col
initMap(wordMap(), 17, 16) 'rows, cols. Must not be smaller then longest word!
'redim debugMap(0 to 17 - 1, 0 to 16 - 1)
redim wpList(0 to numEntries - 1)
dim as f64 t0 = timer
recursiveMagic(wordMap(), 0)
dim as f64 t1 = timer
print "End, " & solvedCount & " solutions found in " & format(t1 - t0, "#.000") & "s"

getkey()

'-------------------------------------------------------------------------------

data "Continue", ""
data "Deallocate", ""
data "Erase", ""
data "FileDateTime", ""
data "GetMouse", ""
data "HiWord", ""
data "ImageDestroy", ""
data "Kill", ""
data "Locate", ""
data "MutexLock", ""
data "Next", ""
data "Overload", ""
data "Paint", ""
data "Random", ""
data "ScreenLock", ""
data "Then", ""
data "UBound", ""
data "Varptr", ""
data "While", ""
data "Xor", ""
data "Year", ""
data "ZString", ""
data "EOD", "End of data"
