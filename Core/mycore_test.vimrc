"string util heler functions for text
"Appends a line to list if it's not empty
"
:function! Add_IfNotEmpty(Lines, LineToAdd)
	:if len(a:Lines) > 0
		:call add(a:Lines, a:LineToAdd)
	:endif
:endfunction
"Appends a set of lines to list, if it's NOT empty
:function! Extend_IfNotEmpty(Lines, LinesToAdd)
	:if len(a:Lines) > 0
		:call extend(a:Lines, a:LinesToAdd)
	:endif
:endfunction
"Append a line to list, and separates with a blank line if NOT empty
:function! Add_WithBlank(Lines, LineToAdd)
	:call Add_IfNotEmpty(a:Lines, "")
	:call add(a:Lines, a:LineToAdd)
:endfunction
"Append a line to list, and separates with a blank line if NOT empty
:function! Extend_WithBlank(Lines, LineToAdd)
	:call Add_IfNotEmpty(a:Lines, "")
	:call extend(a:Lines, a:LinesToAdd)
:endfunction
