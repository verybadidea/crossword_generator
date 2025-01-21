'sd_list = word_pos_list (string with start position)
'sorted dynamic list (wp cannot be used)

type wr_list 'word range list
	dim as string word(any)
	dim as integer pos_(any)
	declare function size() as integer
	declare function empty() as integer
	declare function insert(textstr as string, textpos as integer) as integer
	declare function remove(textpos as integer) as integer
	declare function endPos(i as integer) as integer
	'declare function maxGap(maxLen as integer) as integer
	declare function maxGap(lastPos as integer) as integer
	declare sub printAll() 
end type

function wr_list.size() as integer
	return ubound(word) + 1
end function

function wr_list.empty() as integer
	erase(word)
	return 0
end function

function wr_list.insert(textstr as string, textpos as integer) as integer
	dim as integer ub = ubound(word)
	dim as integer insPos = ub + 1
	'grow list by one
	redim preserve word(insPos)
	redim preserve pos_(insPos)
	for i as integer = 0 to ub 'skips on empty list
		if textpos < pos_(i) then 'move the rest down
			for j as integer = ub to i step -1
				word(j + 1) = word(j)
				pos_(j + 1) = pos_(j)
			next
			insPos = i
			exit for
		end if
	next
	'in case of empty OR append list OR insert list
	word(insPos) = textstr
	pos_(insPos) = textpos
	return insPos
end function

'remove by position key
function wr_list.remove(textpos as integer) as integer
	dim as integer ub = ubound(word)
	for i as integer = 0 to ub
		if textpos = pos_(i) then
			'copy remaining up
			for j as integer = i to ub - 1
				word(j) = word(j + 1)
				pos_(j) = pos_(j + 1)
			next
			if ub = 0 then
				erase word, pos_
			else
				redim preserve word(ub - 1)
				redim preserve pos_(ub - 1)
			end if
			return i
		end if
	next
	return -1 'error, not found
end function

function wr_list.endPos(i as integer) as integer
	return pos_(i) + (len(word(i)) - 1)
end function

'~ 'note maxLen = 0...maxIndex
'~ function sd_list.maxGap(maxLen as integer) as integer
	'~ dim as integer gap, max = 0
	'~ dim as integer ub = ubound(word)
	'~ if ub < 0 then 'list empty
		'~ return maxLen 'full width
	'~ else
		'~ if pos_(0) > max then max = pos_(0)
		'~ for i as integer = 0 to ub - 1
			'~ gap = pos_(i + 1) - (pos_(i) + len(word(i))) 'endPos(i)
			'~ if gap > max then max = gap
		'~ next
		'~ gap = maxLen - (pos_(ub) + len(word(ub))) 'space after last word
		'~ if gap > max then max = gap
		'~ return max
	'~ end if
'~ end function

function wr_list.maxGap(lastPos as integer) as integer
	dim as integer gap, max = 0
	dim as integer ub = ubound(word)
	if ub < 0 then 'list empty
		return lastPos + 1 'full width
	else
		if pos_(0) > max then max = pos_(0)
		for i as integer = 0 to ub - 1
			gap = (pos_(i + 1) - (pos_(i) + len(word(i)))) - 1
			'gap = pos_(i + 1) - (pos_(i) + len(word(i))) 'endPos(i)
			'gap -= 1 'correct for in-bewteen word spacing
			if gap > max then max = gap
		next
		gap = (lastPos + 1) - (pos_(ub) + len(word(ub))) 'space after last word
		if gap > max then max = gap
		return max
	end if
end function

sub wr_list.printAll()
	for i as integer = 0 to ubound(word)
		print i & " - " & word(i) & " : " & pos_(i) & "..." & endPos(i)
	next
end sub

'test code
'dim as sd_list wpList

'print "insert: " & wpList.insert("aaa", 16)
'print "insert: " & wpList.insert("ccc", 3)
'print "insert: " & wpList.insert("xxx", 10)
'wpList.printAll()
'print "maxGap: " & wpList.maxGap(16)

'print "remove: " & wpList.remove(10)
'print "remove: " & wpList.remove(3)
'print "remove: " & wpList.remove(5)
'wpList.printAll()
'print "maxGap: " & wpList.maxGap(16)

'print wpList.insert("bbb", 2)
'print wpList.insert("ddd", 7)
