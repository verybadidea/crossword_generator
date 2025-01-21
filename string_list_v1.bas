type string_list
	dim as string list(any)
	declare function size() as integer
	declare function empty() as integer
	declare function add(text as string) as integer
	declare sub scramble()
	declare sub shuffle()
	declare sub printAll() 
end type

function string_list.size() as integer
	return ubound(list) + 1
end function

function string_list.empty() as integer
	erase(list)
	return 0
end function

function string_list.add(text as string) as integer
	dim as integer ub = ubound(list)
	redim preserve list(ub + 1)
	list(ub + 1) = text
	return ub + 1
end function

'simple inefficient low quality scrambler
sub string_list.scramble()
	for i as integer = 0 to ubound(list)
		dim as integer j = int(rnd * size())
		dim as integer k = int(rnd * size())
		swap list(j), list(k)
		'print list(j), list(k)
	next
end sub

'Fisher-Yates shuffle
sub string_list.shuffle()
	for i as integer = ubound(list) to 1 step -1
		dim as integer j = int(rnd * (i+1))
		if i <> j then swap list(i), list(j)
	next
end sub

sub string_list.printAll()
	for i as integer = 0 to ubound(list)
		print i & " - " & list(i)
	next
end sub
