#include "crt/stdlib.bi"
#include once "default_types_v1.bas"

'-------------------------------------------------------------------------------

function split2(inputStr as string, delChar as string, splitStr1 as string, splitStr2 as string) as isz
	dim as isz delPos = instr(inputStr, delChar)
	splitStr1 = trim(mid(inputStr, 1, delPos - 1))
	splitStr2 = trim(mid(inputStr, delPos + 1))
	return 0
end function

function stringBitMask(word as string) as u32
	dim as u32 bitMask
	for iChar as isz = 0 to len(word) - 1
		dim as isz bitNum = word[iChar] - asc("A") 'A -> 0, B -> 1 ... Z => 25
		bitMask or= (1 shl bitNum)
	next
	return bitMask
end function

function countBits32(v as u32) as isz
	dim as isz count = 0
	for i as isz = 0 to 31
		count += (v and 1)
		v shr= 1
	next
	return count
end function

'-------------------------------------------------------------------------------

const SORT_NONE = 0
const SORT_LENGTH = 1
const SORT_UNICHR = 2
const SORT_CHRSCR = 3

type entry_type
	dim as string word, clue
end type

'sort longest to shortest word
function entryCmpLen cdecl(byval pAny1 as const any ptr, byval pAny2 as const any ptr) as i32
	'cast to proper type
	dim as entry_type ptr pEntry1 = cast(entry_type ptr, pAny1)
	dim as entry_type ptr pEntry2 = cast(entry_type ptr, pAny2)
	'Compare the values
	dim as isz diff = len(pEntry2->word) - len(pEntry1->word)
	if diff > 0 then
		return +1
	elseif diff < 0 then
		return -1
	else
		return 0
	end if
end function

'sort most unique characters
function entryCmpUniChar cdecl(byval pAny1 as const any ptr, byval pAny2 as const any ptr) as i32
	'cast to proper type
	dim as entry_type ptr pEntry1 = cast(entry_type ptr, pAny1)
	dim as entry_type ptr pEntry2 = cast(entry_type ptr, pAny2)
	'Compare the values
	dim as isz diff = countBits32(stringBitMask(pEntry2->word)) - countBits32(stringBitMask(pEntry1->word))
	if diff > 0 then
		return +1
	elseif diff < 0 then
		return -1
	else
		return 0
	end if
end function

'-------------------------------------------------------------------------------

type entry_list_type
	dim as isz num 'acces needed by recursiveMagic()
	dim as entry_type entry(any) 'acces needed by recursiveMagic()
	dim as u32 wordBitMask(any) 'acces needed by recursiveMagic()
	declare function readFromData() as isz
	declare function readFromFile(wordFile as string) as isz
	declare sub list()
	declare sub sort(method as isz)
	declare sub rev()
	declare sub shuffle(numSwaps as isz, limit as isz)
	declare sub createBitMask()
	declare sub listBitMask()
	declare sub sortCharScore() 'for another way of sorting words
end type

'read data lines
function entry_list_type.readFromData() as isz
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
	num = ubound(entry) + 1
	return ubound(entry) + 1
end function

'read file
function entry_list_type.readFromFile(wordFile as string) as isz
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
	num = ubound(entry) + 1
	return ubound(entry) + 1
end function

'list words + description
sub entry_list_type.list()
	for i as isz = 0 to ubound(entry)
		print entry(i).word, entry(i).clue', countBits32(stringBitMask(entry(i).word))
	next
end sub

sub entry_list_type.sort(method as isz)
	select case method
	case SORT_LENGTH
		qsort(@entry(0), ubound(entry) + 1, sizeof(entry_type), @entryCmpLen)
	case SORT_UNICHR
		qsort(@entry(0), ubound(entry) + 1, sizeof(entry_type), @entryCmpUniChar)
	case SORT_CHRSCR
		sortCharScore()
	end select
end sub

sub entry_list_type.rev()
	dim as entry_type tempEntry = Entry(0)
	dim as isz ub = ubound(entry)
	for i as isz = 0 to ub \ 2 
		swap entry(i), entry(ub - i)
	next
end sub

sub entry_list_type.shuffle(numSwaps as isz, limit as isz)
	if limit > ubound(entry) then limit = ubound(entry)
	for i as isz = 1 to numSwaps
		swap entry(int(rnd*limit)), entry(int(rnd*limit))
	next
end sub

sub entry_list_type.createBitMask() 'entry() as entry_type, wordBitMask() as u32)
	dim as isz ub = ubound(entry)
	redim wordBitMask(ub)
	for iWord as isz = 0 to ub
		wordBitMask(iWord) = stringBitMask(entry(iWord).word)
	next
end sub

sub entry_list_type.listBitMask() 'entry() as entry_type, wordBitMask() as u32)
	for i as isz = 0 to ubound(entry)
		'print entry(i).word, bin(wordBitMask(i), 26)
		print entry(i).word, hex(wordBitMask(i), 7)
	next
end sub

sub entry_list_type.sortCharScore() 'for another way of sorting words
	dim as isz charCount(0 to 25) 'A...Z
	redim as u32 charScore(ubound(entry))
	'determine total char count
	for iWord as isz = 0 to num - 1
		dim as string word = entry(iWord).word
		for iChar as isz = 0 to len(word) - 1
			dim as isz charNum = word[iChar] - asc("A")
			charCount(charNum) += 1
		next
	next
	'calculate char score per word
	for iWord as isz = 0 to num - 1
		dim as isz score = 0
		dim as string word = entry(iWord).word
		for iChar as isz = 0 to len(word) - 1
			dim as isz charNum = word[iChar] - asc("A")
			score += charCount(charNum)
		next
		charScore(iWord) = score
	next
	'sort charScore + entry
	for i as isz = 0 to num - 1
		dim as isz maxIndex = i
		dim as isz maxValue = charScore(i)
		for j as isz = i + 1 to num - 1
			if charScore(j) > maxValue then
				maxIndex = j
				maxValue = charScore(j)
			end if
		next
		if maxIndex <> i then
			swap charScore(i), charScore(maxIndex)
			swap entry(i), entry(maxIndex)
		end if
	next
	'list (debug)
	'~ for i as isz = 0 to num - 1
		'~ print entry(i).word, charScore(i)
	'~ next
end sub
