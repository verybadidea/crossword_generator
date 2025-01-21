#include "crt/stdlib.bi"
#include "crt/string.bi"

#include once "default_types_v1.bas"

#define max(a, b) (iif((a) > (b), (a), (b)))

const MAX_WORDS = 40

#include once "entry_list_v1.bas"
dim as entry_list_type entries_

#include "wordmap_v1.bas"
const N_WORDMAP = 2 'is number of threads
dim as wordmap_type wordMap(N_WORDMAP-1)
dim as isz sortOrder(N_WORDMAP-1, 0 to 2) '= {{1,2,3},{1,3,2},{2,1,3},{2,3,1},{3,1,2},{3,2,1}}

'-------------------------------------------------------------------------------

'const SW = 1000, SH = 800
'screenres SW, SH, 32
'width SW \ 8, SH \ 16

randomize timer
entries_.readFromData()
entries_.createBitMask()

if len(entries_.entry(0).word) >= MAX_LEN then
	print "Abort: Word len >= MAX_LEN: " & MAX_LEN
	getkey() : end
end if
entries_.list()

print "numEntries: " + str(entries_.num)
'print "Press any key to start" 
'getkey()
sleep 1000
cls
'end

for iwm as isz = 0 to ubound(wordMap)
	wordMap(iwm).init(16, 15, entries_) 'rows, cols. Must not be smaller then longest word!
	wordMap(iwm).entries.sort(1)
	'use different sort order for each thread
	'for isort as isz = 0 to 2
		'wordMap(iwm).entries.sort(sortOrder(iwm, isort))
	'next
next

'launch threads
dim as f64 t0 = timer
for i as isz = 0 to ubound(wordMap)
	wordMap(i).pHandle = threadCreate(cptr(any ptr, @wordmap_type.recursiveSmart0), @wordMap(i))
next

'poll thread status and wait for completion
dim as boolean allDone
do
	allDone = true
	locate 2,1
	for i as isz = 0 to ubound(wordMap)
		print "thread(" & i & "): " & wordMap(i).done & " - " & wordMap(i).loopCount
		if wordMap(i).done = 0 then allDone = false
	next
	sleep 250, 1
loop until allDone

for i as integer = 0 to ubound(wordMap)
	threadWait(wordMap(i).pHandle)
next

dim as f64 t1 = timer
print "End: " & format(t1 - t0, "#.000") & "s"
'print wordMap(0).solvedCount & " solutions found"
getkey()
end

data "aardbei", ""
data "abrikoos", ""
data "ananas", ""
data "appel", ""
data "avocado", ""
data "banaan", ""
data "bosbes", ""
data "citroen", ""
data "druif", ""
data "framboos", ""
'data "grapefruit", ""
data "kers", ""
data "kiwi", ""
data "kokosnoot", ""
data "mandarijn", ""
data "meloen", ""
data "nectarine", ""
'data "papaja", ""
data "peer", ""
data "perzik", ""
data "pruim", ""
'data "rozenbottel", ""
data "sinaasappel", ""
data "tomaat", ""
data "vlierbes", ""
data "EOD", "End of data"

'entries_.readFromFile("crossword_fusion/words_fusion.txt")
'wordMap.init(17, 16, entries_) 'for fusion data set
