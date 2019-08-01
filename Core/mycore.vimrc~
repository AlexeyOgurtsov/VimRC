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

:function! CoreEcho(IsError, Line, EchoOptions)
	if a:IsError
		echoerr(a:Line)
	else
		echo(a:Line)
	endif
:endfunction

:function! EchoBlock(IsError, Lines, EchoOptions)
	:for Line in a:Lines
		:call CoreEcho(a:IsError, Line, a:EchoOptions)
	:endfor
:endfunction

:function! BoolNot(Val)
	:if a:Val
		return 0
	:else
		return 1
	:endif
:endfunction

"Returns: Count of lines that satisfy the given pattern
:function! CountLinesByPattern(Lines, Pattern)
	let l:count = 0
	let i = 0
	while i < len(a:Lines)
		if (a:Lines[i] =~# a:Pattern)
			let l:count += 1
		endif

		let i += 1
	endwhile
	return l:count
:endfunction

"Returns: Index of the given line to search
:function! FindFirstFrom(StartLineIndex, LineToSearch)
	let i = a:StartLineIndex
	:while (i <= line('$'))
		if getline(i) =~# a:LineToSearch
			return i
		endif
		let i += 1
	:endwhile
	return -1
:endfunction

"Calculate the identation from the given line
"NOTE: Right now identation param is number of tabs
:function! GetLineIndentationParam(Line)
	let i = 0
	while i < len(a:Line)
		if(strpart(a:Line, i, 1) !~# "\t" )
			return i
		endif
		let i += 1
	endwhile
	return len(a:Line)
:endfunction

"Checks if the given line is meaningful
"WARNING! Does not automatically checks comments!
:function! IsCoreLineMeaningful(Line)
	return a:Line =~ '\a\+'
:endfunction

"Returns true if the given set of lines contains at least one meaningful
"character
"WARNING! Override in the corresponding module to support comments!
:function! IsLineMeaningful(Line)
	return IsCoreLineMeaningful(a:Line)
:endfunction

"Returns true if the given set of lines contains at least one meaningful line
"-Not whitespaced;
"-Not empty line
"WARNING!!! Comments are NOT recognized automatically!
:function! AreCoreLinesMeaningful(Lines)
	let i = 0
	:while i < len(a:Lines)	
		let line = a:Lines[i]
		if IsLineMeaningful(line)
			return 1
		endif
		let i += 1
	:endwhile
	return 0
:endfunction


"Returns true if the given set of lines contains at least one meaningful line
"WARNING! Override in the corresponding module to support comments!
:function! AreLinesMeaningful(Lines)
	return AreCoreLinesMeaningful(a:Lines)	
:endfunction

:function! UpdateStringOpenState(OpenState, Line, OpenStr, CloseStr)
	let l:CountToClose = a:OpenState["CountToOpen"]
	let l:IsInitiallyOpened = a:OpenState["IsInitiallyOpened"]
	if(a:Line) == ""
		return l:CountToClose
	endif
	let l:i = len(a:Line) - 1
	while l:i >= 0
		if(stridx(a:Line, a:OpenStr, l:i) == l:i)
			"Open str
			if (l:CountToClose == 0)
				let l:IsInitiallyOpened = 1
			endif
			let l:CountToClose -= 1
		elseif (stridx(a:Line, a:CloseStr, l:i) == l:i)
			let l:CountToClose += 1
		else
			"here we have no open str nor close str
		endif
		let l:i -= 1
	endwhile
	let a:OpenState["CountToOpen"] = l:CountToClose
	let a:OpenState["IsInitiallyOpened"] = l:IsInitiallyOpened
:endfunction

:function! GetOrDefault(L, Index, DefaultValue)
	if(a:Index < len(a:L))
		return a:L[a:Index]
	else
		return a:DefaultValue
	endif
:endfunction 

"Gets key value of Dictionary type or returns {} if not specified
:function! GetKey_DictType(Dict, Key)
	if(has_key(a:Dict, a:Key))
		return a:Dict[a:Key]
	else
		return {}
	endif
:endfunction

:function! GetKey_ListType(Dict, Key)
	if(has_key(a:Dict, a:Key))
		return a:Dict[a:Key]
	else
		return []
	endif
:endfunction

"Gets key value of string type or returns "" if not specified
:function! GetKey_StringType(Dict, Key)
	if(has_key(a:Dict, a:Key))
		return a:Dict[a:Key]
	else
		return ""
	endif
:endfunction

"Gets key value of int type or returns 0 if not specified
:function! GetKey_IntType(Dict, Key)
	if(has_key(a:Dict, a:Key))
		return a:Dict[a:Key]
	else
		return 0
	endif
:endfunction

"Cmd base arg context in the dictionary of base arguments
:function! GetCmdBase_Context(Dict)
	return GetKey_DictType(a:Dict, "Context")
:endfunction

"Adds one dict to another
:function! CombineDict(Dict, OtherDict)
	:for Key in keys(a:OtherDict)
		let a:Dict[Key] = a:OtherDict[Key]
	:endfor
:endfunction

:function! ClearList(List)
	:while len(a:List) > 0
		:call remove(a:List, 0)
	:endwhile
:endfunction

:function! ClearDict(Dict)
	:for Key in keys(a:Dict)
		:call remove(a:Dict, Key)
	:endfor
:endfunction

:function! ResetDict(Dict, NewValueDict)
	:call ClearDict(a:Dict)
	:call CombineDict(a:Dict, a:NewValueDict)
:endfunction

"Returns true if element with specified index does NOT exist within the given List
:function! NoArg(IsError, ArgList, ArgName, Index)
	if a:Index >= len(a:ArgList)	
		:call CoreEcho(a:IsError, 'Argument "'.a:ArgName.'"'.' (index='.a:Index.') not specified', "")
		return 1
	endif 

	return 0
:endfunction

"If no basic args or they are of wrong type,
"returns true and display msg if no base args provided by the command
"NOTE! Basic args must be the FIRST (i.e.g with zero index) element 
"of the argument list
:let g:BaseArgsIndex = 0
:function! GetBaseArgsChecked(ArgList, OutBaseArgs)
	if(len(a:ArgList) == 0)
		echoerr "Base args must be provided as first argument of Dictionary type (specify {} if no basic args to be passed)"
		return 1
	else
		:let l:base_args = a:ArgList[0]
		if(type(l:base_args) != v:t_dict)
			echoerr "Base args must be of Dictionary type (specify {} if no basic args to be passed)"
			return 1
		endif
		:call extend(a:OutBaseArgs, l:base_args)
	endif
	return 0
:endfunction

"Append char if not empty
:function! GetWithChar_IfNotEmpty(S, Char)
	if(len(a:S) > 0)
		return Char.a:S
	endif
	return a:S
:endfunction

"Append space if not empty
:function! GetSpaced_IfNotEmpty(S)
	return GetWithChar_IfNotEmpty(a:S, " ")
:endfunction

"Append tab if not empty
:function! GetTabbed_IfNotEmpty(S)
	return GetWithChar_IfNotEmpty(a:S, "\t")
:endfunction

"Append newline if not empty
:function! GetNewLined_IfNotEmpty(S)
	return GetWithChar_IfNotEmpty(a:S, "\r")
:endfunction

"
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

:function! PrependBlock(Lines, PrependStr)
	let l:i = 0
	while l:i < len(a:Lines)
		let a:Lines[l:i] = a:PrependStr.a:Lines[l:i]
		let l:i += 1
	endwhile
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
"-DOES automaticall jumps after end of the inserted block by itself
:function! AddIndentedCodeLinesAt(LineNumber, Lines, Options)
	let l:IndentedLines = deepcopy(a:Lines)
	"TODO: Indent block if necessary
	if(a:Options =~# "PrepLineBefore;")
		:call insert(l:IndentedLines, "", 0)
	endif
	if(a:Options =~# "PrepLineAfter;")
		:call add(l:IndentedLines, "")
	endif
	:call append(a:LineNumber, l:IndentedLines)
	if(a:Options !~# "LockCur;")
		:call JumpAfterAt(a:LineNumber, a:Lines)
	endif
:endfunction

"Add idented code lines of SINGLE file and automaticall perform necessary identation (for example, when
"inside namespace, etc.)
:function! AddIndentedCodeLines(Lines, Options)
	let l:LineNumber = line('.')
	:call AddIndentedCodeLinesAt(l:LineNumber, a:Lines, a:Options)
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

"Context types
let g:ContextType_Unknown = 0 "We do NOT know where we are now
let g:ContextType_Enum = 1 "Inside enum or enum class etc.
let g:ContextType_Class = 2 "Inside class or struct.
let g:ContextType_Function = 3 "Inside function
let g:ContextType_Global = 4 "Inside global or namespace 
let g:ContextType_NUM = 5


" Returns Core context with the given line index
" Members:
" Line at which context was taken
" WARNING!!! Use GetContextAt whenever possible!)
let g:Context_Line = "Line"
let g:Context_Type = "ContextType"
"Start line number of the inner context entity (class, enum, function etc.): 
" (Always line where the HEADER's FIRST LINE: 
" enum class, class)
" WARNING! Never includes template<class, ... > prefix if it's on a separate
" line above!
" For global context - zero line 
let g:Context_StartLine = "ContextStartLine"
let g:Context_OpenBraceLine = "ContextOpenBraceLine"
"End line number of the inner context entity (class, enum, function etc.)
"Line RIGHT BEFORE the } line
"For global context - last line of the file
let g:Context_EndLine = "ContextEndLine"
"Lines inside body
"All lines right after "{" abd just before "}"
"(NOT determined for Unknown or GLOBAL!!!)
let g:Context_LinesInsideBody = "ContextLinesInsideBody"
"Indentation param of the heade line (for global - indentation of the inner
"NAMESPACE is returned)
let g:Context_IndentationParam = "ContextIndentationParam"
"Enums
let g:Context_NumEnumLiterals = "ContextNumEnumLiterals"
:function! GetCoreContextAt(LineIndex)
	let l:res = {}
	"Line
	let l:res[g:Context_Line] = a:LineIndex
	let l:res[g:Context_Type] = g:ContextType_Unknown "TODO

	let l:res[g:Context_StartLine] = 0
	let l:res[g:Context_OpenBraceLine] = 0
	let l:res[g:Context_EndLine] = line('$')
	let l:res[g:Context_LinesInsideBody] = []

	let l:res[g:Context_IndentationParam] = 0

	let l:res[g:Context_NumEnumLiterals] = 0
	"TODO
	return l:res
:endfunction

"Warning! Override this function in the concrete files (for cpp, for example),
"So correct context is used
:function! GetContextAt(LineIndex)
	return GetCoreContextAt(a:LineIndex)
:endfunction

:function! GetContextLine(Context)
	return a:Context[g:Context_Line]
:endfunction

:function! GetContextStartLine(Context)
	return a:Context[g:Context_StartLine]
:endfunction

:function! GetContextOpenBraceLine(Context)
	return a:Context[g:Context_OpenBraceLine]
:endfunction

:function! GetContextEndLine(Context)
	return a:Context[g:Context_EndLine]
:endfunction

:function! GetContextLinesInsideBody(Context)
	return a:Context[g:Context_LinesInsideBody]
:endfunction

:function! GetContextType(Context)
	return a:Context[g:Context_Type]
:endfunction

:function! GetContextIndentationParam(Context)
	return a:Context[g:Context_IndentationParam]
:endfunction

:function! GetContextNumEnumLiterals(Context)
	return a:Context[g:Context_NumEnumLiterals]
:endfunction

:function! GetCoreEchoContextLines(IsError, Msg, Context, EchoOptions)
	let l:lines = []
	let l:header_line = ""
	if a:IsError
		let l:header_line .= "Error! "
	endif
	let l:header_line .= a:Msg
	:call add(l:lines, l:header_line)
	"Context lines
	:call add(l:lines, "line: ".GetContextLine(a:Context))
	return l:lines
:endfunction

"Returns set of lines of the current context 
"WARNING! override in concrete layers (.cpp etc.)
:function! GetEchoContextLines(IsError, Msg, Context, EchoOptions)
	return GetCoreEchoContextLines(a:IsError, a:Msg, a:Context, a:EchoOptions)
:endfunction

:function! EchoContext(IsError, Msg, Context, EchoOptions)
	let l:lines = GetEchoContextLines(a:IsError, a:Msg, a:Context, a:EchoOptions)
	:call EchoBlock(a:IsError, l:lines, a:EchoOptions)
:endfunction

"If passed context is empty, uses the default context
:function! GetBestContext(Context)
	if a:Context == {}
		return GetContextAt(line('.'))
	else
		return a:Context
	endif
:endfunction

:function! GetCurrContext(Options)
	return GetContextAt(line('.'))
:endfunction

"Returns given context (or current), honors options
:function! ContextOrCurr(Context, Options)
	return GetBestContext(GetCurrContext(a:Options))
:endfunction

"Append both declaration and definition 
" - based on the given public context (lines, buffer etc.) and options
" - Automatically should perform identation (for example, when inside namespace
" etc.)
" - Do automatically jumps cursor at the end (if not specified "LockCur")!
" (because some commands need that cursor is jumped elsewhere)
" Returns:
" 	Index of line, where public lines where inserted

:function! AddCodeAt(Context, PublicLines, PrivateLines, Options)
	let l:AddPublic = (a:Options !~ "NoPublic;")
	let l:AddPriv = (a:Options !~ "NoPriv;") 
	"Adding public code
	let l:BestContext = GetBestContext(a:Context)
	let l:PublicLineNumber = GetContextLine(l:BestContext)

	:call AddIndentedCodeLinesAt(l:PublicLineNumber, a:PublicLines, a:Options)
	"TODO: Add private lines
	return l:PublicLineNumber
:endfunction

"Append both declaration and definition at the given context at CURRENT line
" Returns:
" 	Index of line, where public lines where inserted
:function! AddCode(PublicLines, PrivateLines, Options)
	let l:Context = GetContextAt(line('.'))
	return AddCodeAt(l:Context, a:PublicLines, a:PrivateLines, a:Options)
:endfunction

