"basic vimrc declarations for any language

:set nowrap
:set number


"******* Debugging
:let g:mycore_echo_debug = 0
:function! DebugEcho(Msg)
	if g:mycore_echo_debug
		echo a:Msg
	endif
:endfunction
:function! BoolNot(Val)
	:if a:Val
		return 0
	:else
		return 1
	:endif
:endfunction

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
	:if len(a:Lines) > 0 && len(a:LinesToAdd) > 0
		:call extend(a:Lines, a:LinesToAdd)
	:endif
:endfunction
"Append a line to list, and separates with a blank line if NOT empty
:function! Add_WithBlank(Lines, LineToAdd)
	:call Add_IfNotEmpty(a:Lines, "")
	:call add(a:Lines, a:LineToAdd)
:endfunction
"Append list to list, and separates with a blank line if NOT empty
:function! Extend_WithBlank(Lines, LinesToAdd)
	:if len(a:LinesToAdd) > 0
		:call Add_IfNotEmpty(a:Lines, "")
		:call extend(a:Lines, a:LinesToAdd)
	:endif
:endfunction
:function! IdentLine(Line, Num)
	let l:result_line = ""
	let l:i = 0
	:while l:i < a:Num
		let l:result_line = l:result_line . "\t"
		let l:i = l:i + 1
	:endwhile
	let l:result_line = l:result_line . a:Line
	return l:result_line
:endfunction
:function! IdentBlock(Lines, Num)
	let l:i = 0
	:while l:i < len(a:Lines)
		let a:Lines[l:i] = IdentLine(a:Lines[l:i], a:Num)
		let l:i = l:i + 1
	:endwhile
:endfunction

"function to be used for providing Not-yet-impl statements
:function! GetLines_NotImplFunc_Default(ClassName, TemplParams, FuncName, Msg, OptionString)
	let l:lines = []
	":TODO
	return l:lines	
:endfunction

:function! GetLines_NotImplRetFunc_Default(ClassName, TemplParams, FuncName, Msg, RetVal, OptionString)
	let l:lines = []
	":TODO
	return l:lines	
:endfunction

:function! GetLines_NotImplFuncSevere_Default(ClassName, TemplParams, FuncName, Msg, OptionString)
	let l:lines = []
	":TODO
	return l:lines	
:endfunction

:function! GetLines_NotImplRetFuncSevere_Default(ClassName, TemplParams, FuncName, Msg, RetVal, OptionString)
	let l:lines = []
	":TODO
	return l:lines	
:endfunction

:function! MyCore_NotImplFunc(ClassName, TemplParams, FuncName, Msg, OptionString)
	return GetLines_NotImplFunc_Default(a:ClassName, a:TemplParams, a:FuncName, a:Msg, a:OptionString)
:endfunction
:function! MyCore_NotImplRetFunc(ClassName, TemplParams, FuncName, Msg, RetVal, OptionString)
	return GetLines_NotImplRetFunc_Default(a:ClassName, a:TemplParams, a:FuncName, a:Msg, a:RetVal, a:OptionString)
:endfunction
:function! MyCore_NotImplFuncSevere(ClassName, TemplParams, FuncName, Msg, OptionString)
	return GetLines_NotImplFuncSevere_Default(a:ClassName, a:TemplParams, a:FuncName, a:Msg, a:OptionString)
:endfunction
:function! MyCore_NotImplRetFuncSevere(ClassName, TemplParams, FuncName, Msg, RetVal, OptionString)
	return GetLines_NotImplRetFuncSevere_Default(a:ClassName, a:TemplParams, a:FuncName, a:Msg, a:RetVal, a:OptionString)
:endfunction
"Returns a set of lines, representing current NOT_YET_IMPL instruction call
"IsSevere 1 => hard assert (instead of silent)
:function! GetLines_NotImpl(IsSevere, ClassName, TemplParams, FuncName, Msg, RetVal, OptionString)
	let l:lines = []
	":TODO
	return l:lines	
:endfunction

"Add idented code lines of SINGLE file and automaticall perform necessary identation (for example, when
"inside namespace, etc.)
:function! AddIndentedCodeLinesAt(LineNumber, Lines)
	let l:IndentedLines = deepcopy(a:Lines)
	"TODO: Indent block if necessary
	:call append(a:LineNumber, l:IndentedLines)
:endfunction

"Add idented code lines of SINGLE file and automaticall perform necessary identation (for example, when
"inside namespace, etc.)
:function! AddIndentedCodeLines(Lines)
	let l:LineNumber = line('.')
	:call AddIndentedCodeLinesAt(l:LineNumber, a:Lines)
:endfunction

"Jumps cursor right after the inserted lines as if the given lines where inserted at the given position of
"the cursor
"Returns: New cursor position
:function! JumpAfterAt(LineNumber, Lines)
	:let newpos = a:LineNumber + len(a:Lines) 
	:call cursor(newpos, 1)
	return newpos
:endfunction

"Returns: New cursor position
:function! JumpAfter(Lines)
	:let newpos=JumpAfterAt(line('.'), a:Lines)
	return newpos
:endfunction

"Searches for the given pattern in range of lines
"Returns found line index
:function! SearchRange(StartLineIdx, StopLineIdx, Pattern, SearchFlags, Options)
	:call cursor(a:StartLineIdx, 1)
	let pos = search(a:Pattern, a:SearchFlags, a:StopLineIdx)
	return pos
:endfunction

"Searchs for the given search pattern
"Returns: New cursor position
:function! JumpWhereRange(StartLineIdx, StopLineIdx, Pattern, Options)
	let SearchFlags = []
	let pos = SearchRange(a:StartLineIdx, a:StopLineIdx, a:Pattern, SearchFlags, a:Options)
	:call cursor(newpos)
	return newpos
:endfunction

"Returns: list of lines of the given buffer
:function! GetBufLinesRangeAt(StartLineIdx, EndLineIdx, Options)
	return getbufline(bufnr('%'), a:StartLineIdx, a:EndLineIdx)
:endfunction

:function! GetBufLinesRangeAtMiddle_ByPattern(MiddleLineIdx, StartPattern, EndPattern, Options)
	let l:end_line_index = line('$')
	let l:startLineIdx = SearchRange(1, a:MiddleLineIdx+1, a:StartPattern, "", a:Options)
	let l:endLineIdx = SearchRange(a:MiddleLineIdx, l:end_line_index+1, a:EndPattern, "", a:Options)
	"TODO: Remove debug
	echo "StartLine: ".l:startLineIdx." EndLine: ".l:endLineIdx
	return GetBufLinesRangeAt(l:startLineIdx, l:endLineIdx, a:Options)
:endfunction

"Append both declaration and definition 
" - based on current context (lines, buffer etc.) and options
" - Automatically should perform identation (for example, when inside namespace
" etc.)
" - Does NOT automatically jumps cursor at the end!
:function! AddCode(PublicLines, PrivateLines, Options)
	let l:AddPublic = (a:Options !~ "NoPublic;")
	let l:AddPriv = (a:Options !~ "NoPriv;") 
	"Adding public code
	let l:PublicLineNumber = line('.')
	:call AddIndentedCodeLinesAt(l:PublicLineNumber, a:PublicLines)
	"TODO: Add private lines
:endfunction
