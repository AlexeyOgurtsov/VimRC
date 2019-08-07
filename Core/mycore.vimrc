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

:function! IsUpperCharAt(S, Index)
	if(a:Index >= len(a:S))
	       	return 0
	endif
	let CharStr = strpart(a:S, a:Index, 1)
	if(toupper(CharStr) == CharStr)
		return 1
	endif
	return 0
:endfunction

:function! IsLowerCharAt(S, Index)
	if(a:Index >= len(a:S))
	       	return 0
	endif
	let CharStr = strpart(a:S, a:Index, 1)
	if(tolower(CharStr) == CharStr)
		return 1
	endif
	return 0
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

"Tries to search the given line, so that it placed Before the given one
"Returns: 
"	Index of the given line to search, 
"	Index of the given line, if the After line does NOT exist
"	-1 if After placed before or does NOT exist
:function! FindFirstFromAndBefore(StartLineIndex, First, After)
	let l:BeforeIndex = FindFirstFrom(a:StartLineIndex, a:First)
	if (l:BeforeIndex < 0 )
	       	return -1 
	endif
	let l:AfterIndex = FindFirstFrom(a:StartLineIndex, a:After)
	if (l:AfterIndex < 0)
		return l:BeforeIndex
	endif
	if(l:BeforeIndex == l:AfterIndex)
		"TODO: Not impl: check order within the line itself
	endif
	return l:BeforeIndex <= l:AfterIndex
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

"Elements of the list at the given range are to be converted into a string
"with the given separator using .= operator
"WARNING!!! If indices are beyond range, empty result is returned
"EndIndex - NON-inclusive (if -1, then to end)
:function! JoinListRange(Args, Sep, StartIndex, EndIndex)
	let i = a:StartIndex	
	let res = ''
	while( ( i < a:EndIndex ) && (i < len(a:Args) ) )
		if(len(res) > 0)
			let res .= a:Sep
		endif

		let res .= a:Args[i]

		let i += 1	
	endwhile
	return res
:endfunction

"Accumulate all elements from the given start index using .= operator
"WARNING! If index is beyond range, empty result is used
:function! JoinRestList(L, Sep, StartIndex)
	return JoinListRange(a:L, a:Sep, a:StartIndex, len(a:L))
:endfunction

"Like JoinRestList, but uses ' ' separator by default
:function! ListRangeAsString(L, Sep, StartIndex, EndIndex)
	return JoinRestList(a:L, ' ', a:StartIndex, a:EndIndex)	
:endfunction
"Like JoinRestList, but uses ' ' separator by default
:function! ListRestAsString(L, StartIndex)
	return JoinRestList(a:L, ' ', a:StartIndex)
:endfunction

"Makes argument string for the given command
:function! GetWithAppendedArgString_FromDictOfList_ByKey(SourceStr, ArgDict, Key)
	let l:s = a:SourceStr
	let val_list = a:ArgDict[a:Key]
	if len(val_list) > 0 
		if(len(l:s) > 0)
			let l:s .= ' '
		endif
		let l:s .= a:Key
		"WARNING! We use double separator here (first opens, second -
		"closes)
		let l:s .= JoinRestList(val_list, a:Key.a:Key, 0)
	endif	
	return l:s
:endfunction

:function! ArgString_FromDictOfList(ArgDict)
	let l:s = ''
	if(has_key(a:ArgDict, '') && len(a:ArgDict['']) > 0) 
		let l:s = GetWithAppendedArgString_FromDictOfList_ByKey(l:s, a:ArgDict, '')
	endif
	for Key in keys(a:ArgDict)
		if(Key != '')
			let l:s = GetWithAppendedArgString_FromDictOfList_ByKey(l:s, a:ArgDict, Key)
		endif
	endfo	
	return l:s
:endfunction

"Returns arg string: main string and body string"
:function! MakeArgString_MainAndBody(MainArgDict, BodyString)
	let l:s = ''
		let l:MainArgDictString = ArgString_FromDictOfList(a:MainArgDict)
		if(len(l:MainArgDictString) > 0)
			let l:s .= l:MainArgDictString
		endif

		if(len(a:BodyString) > 0)
			if(len(l:s) > 0)
				let l:s .= ' '
			endif
			let l:s .= '@'
			let l:s .= a:BodyString
		endif
	return l:s
:endfunction

"Make argument for function-style command (for function command or variable
"command)
:function! MakeCommandArgs_FunctionStyle(Name, MainArgDict, BodyString)
	return ' ; '.a:Name.' '.MakeArgString_MainAndBody(a:MainArgDict, a:BodyString)	
:endfunction

let g:VariableGenArgIndex_Name = 0
let g:VariableGenArgIndex_RetType = 1
let g:VariableGenArgIndex_Initializer = 2
let g:VariableGenArgIndex_Ops = 3
let g:VariableGenArgIndex_Category = 4
let g:VariableGenArgIndex_CommentTextLines = 5

:function! MakeVariableGenArgs(Name, RetType, Initializer, Ops, Category, CommentTextLines)
	return [a:Name, a:RetType, deepcopy(a:Initializer), a:Ops, a:Category, a:CommentTextLines]
:endfunction:

:function! EchoVariableGenArgs(Args)
	let lines = []
	:call add(lines, 'Name:'.GetVariableName(a:Args))
	:call add(lines, 'RetType:'.GetVariableRetType(a:Args))
	:call add(lines, 'Initializer:'.GetVariableInitializer(a:Args))
	:call add(lines, 'Category:'.GetVariableCategory(a:Args))
	:call add(lines, 'Ops:'.GetVariableOps(a:Args))
	:call add(lines, 'Comment:'.GetVariableCommentTextLines(a:Args))
	:call EchoBlock(0, lines, '')
:endfunction

:function! GetVariableName(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_Name]
:endfunction

:function! SetVariableName(VariableGenList, NewValue)
	let a:VariableGenList[g:VariableGenArgIndex_Name] = a:NewValue 
:endfunction

:function! GetVariableRetType(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_RetType]
:endfunction

:function! SetVariableRetType(VariableGenList, NewValue)
	let a:VariableGenList[g:VariableGenArgIndex_RetType] = a:NewValue
:endfunction

:function! GetVariableOps(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_Ops]
:endfunction

:function! AddVariableOps(VariableGenList, NewOps)
	let a:VariableGenList[g:VariableGenArgIndex_Ops] .= ';' . a:NewOps
:endfunction

:function! GetVariableInitializer(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_Initializer]
:endfunction

:function! SetVariableInitializer(VariableGenList, NewValue)
	let a:VariableGenList[g:VariableGenArgIndex_Initializer] = deepcopy(a:NewValue)
:endfunction

:function! GetVariableCategory(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_Category]
:endfunction

:function! SetVariableCategory(VariableGenList, NewValue)
	let a:VariableGenList[g:VariableArgIndex_Category] = a:NewValue
:endfunction

:function! GetVariableCommentTextLines(VariableGenList)
	return a:VariableGenList[g:VariableGenArgIndex_CommentTextLines]
:endfunction

:function! SetVariableCommentTextLines(VariableGenList, NewValue)
	let a:VariableGenList[g:VariableArgIndex_CommentTextLines] = deepcopy(a:NewValue)
:endfunction


let g:FuncGenArgIndex_Name = 0
let g:FuncGenArgIndex_ArgString = 1
let g:FuncGenArgIndex_RetType = 2
let g:FuncGenArgIndex_Ops = 3
let g:FuncGenArgIndex_BodyList = 4 "List of body line strings
let g:FuncGenArgIndex_Category = 5 "Category
let g:FuncGenArgIndex_CommentTextLines =6 "Comment lines

:function! MakeFuncGenArgs(Name, ArgString, RetType, Ops, BodyList, Category, CommentTextLines)
	return [a:Name, a:ArgString, a:RetType, a:Ops, deepcopy(a:BodyList), a:Category, a:CommentTextLines]
:endfunction:

:function! GetFuncName(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_Name]
:endfunction

:function! SetFuncName(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncGenArgIndex_Name] = a:NewValue 
:endfunction

:function! GetFuncArgString(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_ArgString]
:endfunction

:function! SetFuncArgString(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncGenArgIndex_ArgString] = a:NewValue
:endfunction

:function! GetFuncRetType(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_RetType]
:endfunction

:function! SetFuncRetType(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncGenArgIndex_RetType] = a:NewValue
:endfunction

:function! GetFuncOps(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_Ops]
:endfunction

:function! AddFuncOps(FuncGenList, NewOps)
	let a:FuncGenList[g:FuncGenArgIndex_Ops] .= ';' . a:NewOps
:endfunction

:function! GetFuncBody(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_BodyList]
:endfunction

:function! SetFuncBody(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncGenArgIndex_BodyList] = deepcopy(a:NewValue)
:endfunction

:function! GetFuncCategory(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_Category]
:endfunction

:function! SetFuncCategory(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncArgIndex_Category] = a:NewValue
:endfunction

:function! GetFuncCommentTextLines(FuncGenList)
	return a:FuncGenList[g:FuncGenArgIndex_CommentTextLines]
:endfunction

:function! SetFuncCommentTextLines(FuncGenList, NewValue)
	let a:FuncGenList[g:FuncArgIndex_CommentTextLines] = deepcopy(a:NewValue)
:endfunction

"Returns name with upper-cased first letter
:function! GetFixedName(Name)
	if(a:Name == '') 
		return ''
	endif
	return toupper(strpart(a:Name, 0, 1)) . strpart(a:Name, 1)
:endfunction

"Return list of form [MainString, ReturnValueString],
"Where ReturnValueString is all string beyond the FUNC_RETURN_VALUE char
"(FUNC_RETURN_VALUE char is NOT included in any of the strings!)
:function! SplitFuncArgs_MainAndResult(S)
	if(len(a:S) > 0)
		if(a:S =~# '^@')
			return ['', strpart(a:S, 1)]
		else
			return split(a:S, '@')
		endif
	else
		return ['','']
	endif
:endfunction


:function! GetFuncBodyLines_FromMainAndBodyList(MainAndBodyList)
	let l:BodyString = GetOrDefault(a:MainAndBodyList, 1, "")
	if(IsEmptyOrSpaced(l:BodyString))
		return []
	else
		return [ l:BodyString ]
	endif
:endfunction

"Extracts arguments from the given string 
"Each argument is prefixed with the given SepString
"Default argument - when none of the provided SepStrings are encounted yet in
"the string)
"If the SepString is encountered the second time, it's to be treated like
"the given argument is closed and we are providing the default argument now
"Returns: Dictionary mapping SepString to argument value LIST
"Examples:
"	Separators:
"		! - Category
"		; - Option
"		: - return value
"
"	!Input|Category;ops:bool:int* p    
"		":" -> [ "bool" ]
"		"!" -> [ "Input|Category" ]
"		"" -> [ "int* p" ]
"		";" -> [ "ops" ]
"
"	!Input|Category;ops:bool:int* p:int:test
"		":" -> [ "bool", "int" ]
"		"!" -> [ "Input|Category" ]
"		"" -> [ "int* p" ]
"		";" -> [ "ops", "test" ]

:function! ExtractArgumentsFromString(S, SepList)
	let ResDict = {}

	" Create one list for each key (separator) of the result dictionary)
	let ResDict[''] = []
	let SepIndex = 0
	while SepIndex < len(a:SepList)
		let ResDict[a:SepList[SepIndex]] = []
		let SepIndex += 1
	endwhile

	"Index of current separator
	"(if negative, the default argument is current)
	let CurrSepIndex = -1
	let sep_value_start_index = 0
	let sep_value_end_index = 0 "Past the end value of the separator
	let SepFound = 0
	let FoundSepIndex = -1 "Index of separator we found
	let i = 0
	while 1
		" Append the extracted argument value corresponding the given
		" separator
		if(SepFound || (i >= len(a:S)))
			if(i >= len(a:S) && BoolNot(SepFound))
				let sep_value_end_index = len(a:S)
			endif	

			if(CurrSepIndex >= 0)
				let SepStr = a:SepList[CurrSepIndex]
			else
				let SepStr = ""
			endif
			let len_argument = sep_value_end_index - sep_value_start_index
			"We should never add empty string for default argument
			"at the start of the line!
			if(BoolNot((CurrSepIndex < 0) && (i == 1) && (len_argument == 0)))
				:call add(ResDict[SepStr], strpart(a:S, sep_value_start_index, len_argument))
			endif

			let sep_value_start_index = i "Note: here we already skipped the separator
			let sep_value_end_index = sep_value_start_index "Does Not makes sense to assign it - it's to be updated at the end of each iteration

			if(CurrSepIndex == FoundSepIndex)
				"Last separator is the same as we
				"found, so we close the current
				"argument and use the default
				let CurrSepIndex = -1
			else
				let CurrSepIndex = FoundSepIndex
			endif
		endif


		"Exit we gained the end of string
		if(i >= len(a:S))
			break
		endif

		"Find the separator
		let SepFound = 0
		let SepIndex = 0
		let FoundSepIndex = -1
		while (SepIndex < len(a:SepList)) && BoolNot(SepFound)
			if(stridx(a:S, a:SepList[SepIndex], i) == i)
				"We found the separator
				let SepFound = 1
				let FoundSepIndex = SepIndex
			endif

			let SepIndex += 1
		endwhile
			
		if(SepFound)
			let sep_value_end_index = i
			let i += len(a:SepList[FoundSepIndex])
		else
			let i += 1
		endif
	endwhile

	return ResDict
:endfunction

:function! EchoDictOfList(DictOfList)
	for key in keys(a:DictOfList)
		echo 'key='.key.' value='.join(a:DictOfList[key], ';')
	endfor
:endfunction


"Extracts default arguments + custom arguments from string
:function! ExtractDefaultArguments(S, ExtraSeps)
	let Seps = [ '!', ':', ';', '//']
	:call extend(Seps, a:ExtraSeps)

	return ExtractArgumentsFromString(a:S, Seps)
:endfunction

"Is line of the following forms:
" '// ~ SectionName Begin
" '// ~ SectionName Beg
" '// ~ SectionName {
" '// ~ SectionName // WARNING!!! NO End or } here!
:function! IsSectionStartLine(Line, SectionName)
	let Pattern = '^\s*//\s*\~\s*'
	let Pattern .= a:SectionName
	let Pattern .= '\s*\(\|{\|Beg\|Begin\)\s*$'
	return a:Line =~ Pattern
:endfunction

"Is line of the following forms:
" '// ~ SectionName End
" '// ~ SectionName Beg
" '// ~ SectionName {'
:function! IsSectionEndLine(Line, SectionName)
	let Pattern = '^\s*//\s*\~\s*'
	let Pattern .= a:SectionName
	let Pattern .= '\s*\(}\|End\)\s*$'
	return a:Line =~ Pattern
:endfunction


"Find section start line
"Returns: -1 if not found
"Arguments:
"	StartLine - index of line (in buffer space) where to start the search
"	EndLine - index of last line to search
:function! FindSectionStartLine_InRange(StartLine, LastLine, SectionName)
	let LineIndex = a:StartLine
	while(LineIndex <= a:LastLine)
		if(IsSectionStartLine(getline(LineIndex), a:SectionName))
			return LineIndex
		endif
		let LineIndex += 1
	endwhile
	return -1
:endfunction

"Find section end line
"Returns: -1 if not found
"Arguments:
"	StartLine - index of line (in buffer space) where to start the search
"	EndLine - index of last line to search
:function! FindSectionEndLine_InRange(StartLine, LastLine, SectionName)
	let LineIndex = a:StartLine
	while(LineIndex <= a:LastLine)
		if(IsSectionStartLine(getline(LineIndex), a:SectionName))
			return LineIndex
		endif
		let LineIndex += 1
	endwhile
	return -1
:endfunction
"Returns name of value from options string
"(returns empty string if value name is NOT specified)
:function! GetValueByPrefixFromOptions(Options, Prefix)
	let idx_value = stridx(a:Options, a:Prefix)
	if(idx_value < 0)
		return ''
	elseif (idx_value == (len(a:Options) - len(a:Prefix)))
		"Now name specified after the section char
		return ''
	else
		"Skipping the prefix
		let idx_value += len(a:Prefix)
	endif
	let idx_option_sep_after_value = stridx(a:Options, ';', idx_value)
	if(idx_option_sep_after_value >= 0)
		return strpart(a:Options, idx_value, (idx_option_sep_after_value - idx_value))
	else
		return strpart(a:Options, idx_value)
	endif

	"Should not get here
	return ''
:endfunction

:function! GetSectionNameFromOptions(Options)
	return GetValueByPrefixFromOptions(a:Options, '$')
:endfunction

:function! GetClassNameFromOptions(Options)
	return GetValueByPrefixFromOptions(a:Options, '^')
:endfunction

"Joins all elements corresponding the given key into one str
:function! GetJoinedArgument_FromDict(Dict, Key, JoinStr)
	let Str = ''

	let Elems = GetKey_ListType(a:Dict, a:Key)

	let i = 0
	while (i < len(Elems))
		if(len(Str) > 0)
			let Str .= a:JoinStr
		endif
		let Str .= GetSpacesTrimmed(Elems[i])
		let i += 1
	endwhile
	return Str
:endfunction

:function! StringOrDefaultIfEmpty(S, Def)
	if(a:S == '')
		return a:Def
	else 
		return a:S
	endif
:endfunction

:function! GetSection_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, '~', ''))
:endfunction

:function! GetJoinedCategories_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, '!', '|'))
:endfunction

:function! GetJoinedOps_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, ';', ';'))
:endfunction

:function! GetFunctionArguments_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, '', ''))
:endfunction

:function! GetComment_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, '//', ' '))
:endfunction

:function! GetReturnType_FromExtractedDict(Dict)
	return GetSpacesTrimmed(GetJoinedArgument_FromDict(a:Dict, ':', ' '))
:endfunction

:function! InvalidFunctionArgs(Dict)
	let ReturnType = GetKey_ListType(a:Dict, ':')

	if(len(ReturnType) > 1)
		echoerr "Only one return type can be specified for function"
		return 1
	endif

	return 0
:endfunction

"Removes all spaces chars from start of the string
:function! GetLeftSpacesTrimmed(S)
	let i = 0
	let spaces_len = 0
	while(i < len(a:S))
		if(strpart(a:S, i, 1) =~# '\s')
			let spaces_len += 1
		else
			break
		endif
		let i+=1
	endwhile
	"echo "DEBUG: spaces_len: ".spaces_len
	return strpart(a:S, spaces_len)
:endfunction

"Removes all spaces chars from end of the string
:function! GetRightSpacesTrimmed(S)
	if(a:S == '') 
		return a:S
	endif
	let i = len(a:S) - 1
	let spaces_len = 0
	while(i >= 0)
		if(strpart(a:S, i, 1) =~# '\s')
			let spaces_len += 1
		else
			break
		endif
		let i-=1
	endwhile
	"echo "DEBUG: spaces_len: ".spaces_len
	return strpart(a:S, 0, (len(a:S) - spaces_len))
:endfunction

"Removes all spaces chars from end and start of the string
:function! GetSpacesTrimmed(S)
	let res = GetRightSpacesTrimmed(GetLeftSpacesTrimmed(a:S))
	return res
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
:let g:OpsArgIndex = 1
:let g:NumCommonArgs = 2
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

:function! GetNameWithFixedPrefix(Name, DesiredPrefix)
	let LowerPrefix = tolower(a:DesiredPrefix)
	lockvar LowerPrefix
	let Prefix = toupper(a:DesiredPrefix)
	lockvar Prefix
	
	let FixedName = copy(a:Name)

	let IsLowerF = (FixedName[0] ==# LowerPrefix)

	if(len(FixedName) > 1)
		let IsUpperAfterF = (toupper(FixedName[1]) == FixedName[1])
	else
		let IsUpperAfterF = 0
	endif

	if IsLowerF && IsUpperAfterF
		"When upper character right after lower F is specified,
		"then we typically already included F as the prefix

	else
		"Appending F at the begging if not started with it
		if ((FixedName[0] !=# Prefix))
			let FixedName = Prefix.FixedName
		endif
	endif

	let FixedName_TwoChars = strpart(FixedName, 0, 2)
	let FixedName_Rest = strpart(FixedName, 2)

	"Upper-casing first character after F if NOT upper-cased yet
	let FixedName_TwoChars = toupper(FixedName_TwoChars)
	let FixedName = FixedName_TwoChars.FixedName_Rest 

	return FixedName
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


"Jumps cursor right after the inserted lines as if the given lines where inserted at the given position of
"the cursor
"Returns: New cursor position
:function! JumpAfterAt(LineNumber, Lines)
	let IsDebug = 0
	:let newpos = a:LineNumber + len(a:Lines)
	:call setpos('.', [0, newpos, 1, 0])
	":call cursor(newpos, 1)
	if (IsDebug)
		echo 'LineNumber='.a:LineNumber
		echo 'len(a:Lines)='.len(a:Lines)
		echo 'newpos='.newpos
	endif
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

"Access types"
let g:AccessType_Unknown = 0
let g:AccessType_Public = 1
let g:AccessType_Protected = 2
let g:AccessType_Private = 3

:function! GetAccessTypeFromOptions(Options)
	if(a:Options =~# '+')
		return g:AccessType_Public
	elseif(a:Options =~# '#')
		return g:AccessType_Protected
	elseif(a:Options =~# '-')
		return g:AccessType_Private
	endif

	return g:AccessType_Unknown
:endfunction

:function! GetAccessTypeString(AccessType)
	if(a:AccessType == g:AccessType_Unknown)
		return "Unknown"
	elseif(a:AccessType == g:AccessType_Public)
		return "Public"
	elseif(a:AccessType == g:AccessType_Protected)
		return "Protected"
	elseif(a:AccessType == g:AccessType_Private)
		return "Private"
	else
		return "{WRONG}"
	endif
:endfunction


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
let g:Context_HeaderLineIndex = "ContextHeaderLineIndex"
"End line number of the inner context entity (class, enum, function etc.)
"Line RIGHT BEFORE the } line
"For global context - last line of the file
let g:Context_EndLine = "ContextEndLine"
"Lines inside body
"All lines right after "{" abd just before "}"
"(NOT determined for Unknown or GLOBAL!!!)
let g:Context_LinesInsideBody = "ContextLinesInsideBody"
"Is current class a struct (0 - for other contexts)"
let g:Context_IsStruct = "ContextIsStruct"
"Access type: what is current access type at the given context line?
"Valid classes (including structs)
"When struct: global is public
"When class: global is private
let g:Context_ClassAccessType = "ContextAccessType"
"First public line of class
"-1 if context is not class context or line does not exist
"(for struct - index of first line after AFTER_LINE)
let g:Context_ClassPublicLineIndex = "ContextClassPublicLineIndex"
let g:Context_ClassPrivateLineIndex = "ContextClassPrivateLineIndex"
let g:Context_ClassProtectedLineIndex = "ContextClassProtectedLineIndex"
"Base name of the current context's class
let g:Context_ClassBaseName = "ContextClassBaseName"
"Class where are we inside now (no matter, whether we are inside class context
"or inside other context"
"(empty means we are NOT inside class)
let g:Context_ClassName = "ContextClassName"
"Path to the most inner namespace we are currently in
"(no matter what context we are currently in"
"(does NOT include the classname!")
"Empty for global namespace or unknown context
let g:Context_NamespacePath = "ContextNamespacePath" 
"Indentation param of the heade line (for global - indentation of the inner
"NAMESPACE is returned)
let g:Context_IndentationParam = "ContextIndentationParam"
"Enums
let g:Context_NumEnumLiterals = "ContextNumEnumLiterals"
let g:Context_MinimalEnumFlag_LineIndex = "ContextMinimalEnumFlagLineIndex"
let g:Context_MinimalEnumFlagValue = "ContextMinimalEnumFlagValue"
let g:Context_MaxEnumFlag_LineIndex = "ContextMaxEnumFlagLineIndex"
let g:Context_MaxEnumFlagValue = "ContextMaxEnumFlagValue"
"Enum flag value that is not assigned yet
let g:Context_EnumFlagHole_LineIndex = "ContextEnumFlagHoleLineIndex"
let g:Context_EnumFlagHoleValue = "ContextEnumFlagHoleValue"
:function! GetCoreExactContextAt(LineIndex, Options)
	let l:res = {}
	"Line
	let l:res[g:Context_Line] = a:LineIndex
	let l:res[g:Context_Type] = g:ContextType_Unknown

	let l:res[g:Context_StartLine] = 0
	let l:res[g:Context_HeaderLineIndex] = 0
	let l:res[g:Context_OpenBraceLine] = 0
	let l:res[g:Context_EndLine] = line('$')
	let l:res[g:Context_LinesInsideBody] = []

	"Class
	let l:res[g:Context_IsStruct] = 0
	let l:res[g:Context_ClassAccessType] = g:AccessType_Unknown
	let l:res[g:Context_ClassPublicLineIndex] = -1
	let l:res[g:Context_ClassPrivateLineIndex] = -1
	let l:res[g:Context_ClassProtectedLineIndex] = -1

	"Class/Namespace"
	let l:res[g:Context_ClassBaseName] = ''
	let l:res[g:Context_ClassName] = ''
	let l:res[g:Context_NamespacePath] = ''

	"Formatting
	let l:res[g:Context_IndentationParam] = 0

	"Enums
	let l:res[g:Context_NumEnumLiterals] = 0

	let l:res[g:Context_MinimalEnumFlag_LineIndex] = -1
	let l:res[g:Context_MinimalEnumFlagValue] = -1
	let l:res[g:Context_MaxEnumFlag_LineIndex] = -1
	let l:res[g:Context_MaxEnumFlagValue] = -1
	let l:res[g:Context_EnumFlagHole_LineIndex] = -1
	let l:res[g:Context_EnumFlagHoleValue] = -1
	"TODO
	
	return l:res
:endfunction

"Override this function and return new line index for the context
"(return the same line if context should not be updated)
:function! GetUpdatedContextLine(Context, Options)
	return GetContextLine(a:Context)
:endfunction

"Finds new context based on current options
"(for example, when specified +/-/# - go to private/public/protected section)
:function! GetUpdatedContext_FromOptions(Context, Options)
	let NewLine = GetUpdatedContextLine(a:Context, a:Options)

	let NewOps = a:Options.';ExactContext;'
	let NewContext = GetContextAt(NewLine, NewOps)
	return NewContext
:endfunction

"Warning! Override this function in the concrete files (for cpp, for example),
"So correct context is used
:function! GetExactContextAt(LineIndex, Options)
	return GetCoreExactContextAt(a:LineIndex, a:Options)
:endfunction

"Warning! NEVER Override this function! Override GetExactContextAt instead!
:function! GetContextAt(LineIndex, Options)
	let l:res = GetExactContextAt(a:LineIndex, a:Options)

	"Finding best context:
	if(a:Options !~# ';ExactContext;')
		let l:res = GetUpdatedContext_FromOptions(l:res, a:Options)
		:call cursor(GetContextLine(l:res), 1)
	endif

	return l:res
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

:function! GetContextHeaderLineIndex(Context)
	return a:Context[g:Context_HeaderLineIndex]
:endfunction

:function! GetContextEndLine(Context)
	return a:Context[g:Context_EndLine]
:endfunction

:function! GetContextEntityName(Context)
	let ContextType = GetContextType(a:Context)
	if (ContextType == g:ContextType_Class)
		return GetContextClassName(a:Context)
	else
		"TODO
		return ''
	endif
:endfunction

:function! GetContextLinesInsideBody(Context)
	return a:Context[g:Context_LinesInsideBody]
:endfunction

:function! GetContextIsStruct(Context)
	return a:Context[g:Context_IsStruct]
:endfunction

:function! GetContextClass_PublicLine(Context)
	return a:Context[g:Context_ClassPublicLineIndex]
:endfunction

:function! GetContextClass_PrivateLine(Context)
	return a:Context[g:Context_ClassPrivateLineIndex]
:endfunction

:function! GetContextClass_ProtectedLine(Context)
	return a:Context[g:Context_ClassProtectedLineIndex]
:endfunction

:function! GetContextClass_BaseName(Context)
	return a:Context[g:Context_ClassBaseName]
:endfunction

:function! GetContextClass_AccessType(Context)
	return a:Context[g:Context_ClassAccessType]
:endfunction

:function! GetContextClassName(Context)
	return a:Context[g:Context_ClassName]
:endfunction

:function! GetContextNamespacePath(Context)
	return a:Context[g:Context_NamespacePath]
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

:function! GetContext_MinimalEnumFlag_LineIndex(Context)
	return a:Context[g:Context_MinimalEnumFlag_LineIndex]
:endfunction

:function! GetContext_MinimalEnumFlag_Value(Context)
	return a:Context[g:Context_MinimalEnumFlagValue]
:endfunction

:function! GetContext_MaxEnumFlag_LineIndex(Context)
	return a:Context[g:Context_MaxEnumFlag_LineIndex]
:endfunction

:function! GetContext_MaxEnumFlag_Value(Context)
	return a:Context[g:Context_MaxEnumFlagValue]
:endfunction

:function! GetContext_EnumFlagHole_LineIndex(Context)
	return a:Context[g:Context_EnumFlagHole_LineIndex]
:endfunction

:function! GetContext_EnumFlagHole_Value(Context)
	return a:Context[g:Context_EnumFlagHoleValue]
:endfunction

"Transform line index of the context from local (relative to first brace) into
"global line index (relative to buffer's line)
:function! LineIndex_LocToBuf(Context, InsideBody_LineIndex)
	if(a:InsideBody_LineIndex < 0)
		return -1
	endif
	let l:FirstBodyLine_LineIndex_InBuffer = GetContextOpenBraceLine(a:Context) + 1
	return l:FirstBodyLine_LineIndex_InBuffer + a:InsideBody_LineIndex
:endfunction

"Should we add +1 for indentation when adding into the given context?
:function! ContextRequiresExtraIndentation(Context, Options)
	let ContextType = GetContextType(a:Context)
	if ((ContextType == g:ContextType_Global) || (ContextType == g:ContextType_Unknown))
		"TODO: Here check for namespaces
		return 0
	else
		return 1
	endif
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
	"Line
	:call add(l:lines, "Line: ".GetContextLine(a:Context))
	let ContextType = GetContextType(a:Context)
	:call add(l:lines, "ContextType: ".ContextType)
	if (ContextType == g:ContextType_Unknown)
		:call add(l:lines, "(Unknown)")
	elseif (ContextType == g:ContextType_Enum)
		:call add(l:lines, "(Enum)")
	elseif (ContextType == g:ContextType_Class)
		:call add(l:lines, "(Class)")
	elseif (ContextType == g:ContextType_Function)
		:call add(l:lines, "(Function)")
	else
		:call add(l:lines, "(Wrong context value)")
	endif

	"Class name and namespace
	:call add(l:lines, "ClassName: ".string(GetContextClassName(a:Context)))
	:call add(l:lines, "ClassBaseName: ".string(GetContextClass_BaseName(a:Context)))
	:call add(l:lines, "NamespacePath: ".string(GetContextNamespacePath(a:Context)))

	if(ContextType == g:ContextType_Class)
		:call add(l:lines, "IsStruct: ".string(GetContextIsStruct(a:Context)))
		:call add(l:lines, "ClassAccessType: ".GetAccessTypeString(GetContextClass_AccessType(a:Context)))
		:call add(l:lines, "PublicLineIndex: ".string(GetContextClass_PublicLine(a:Context)))
		:call add(l:lines, "PrivateLineIndex: ".string(GetContextClass_PrivateLine(a:Context)))
		:call add(l:lines, "ProtectedLineIndex: ".string(GetContextClass_ProtectedLine(a:Context)))
	endif

	:call add(l:lines, "IndentationParam: ".GetContextIndentationParam(a:Context))
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
:function! GetBestContext(Context, Options)
	if a:Context == {}
		return GetContextAt(line('.'), a:Options)
	else
		return a:Context
	endif
:endfunction

:function! GetCurrContext(Options)
	return GetContextAt(line('.'), a:Options)
:endfunction

"Returns given context (or current), honors options
:function! ContextOrCurr(Context, Options)
	return GetBestContext(GetCurrContext(a:Options), a:Options)
:endfunction

"Returns true if the given line consists of whitespaces only or completely
"empty
:function! IsEmptyOrSpaced(Line)
	return a:Line =~# '^\s*$'
:endfunction

"Do we have empty line before (if no line before - false!)
:function! EmptyOrSpacedLineBefore(LineIndex)
	if(a:LineIndex <= 1)
		return 0
	endif

	return IsEmptyOrSpaced(getline(a:LineIndex - 1))
:endfunction

"Add idented code lines of SINGLE file and automaticall perform necessary identation (for example, when
"inside namespace, etc.)
"-DOES automaticall jumps after end of the inserted block by itself
"Returns:
"	Index of the last line of inserted code (EXCLUDING extra newlines!) 
"	(or insert line index, if empty block is inserted)
:function! AddIndentedCodeLinesAt(Context, Lines, Options)
	let IsDebug = 0
	if (IsDebug)
		echo 'DEBUG: AddIndentedCodeLinesAt'
	endif 

	let l:LineNumber = GetContextLine(a:Context)
	lockvar l:LineNumber
	let l:IndentedLines = deepcopy(a:Lines)
	let IndentParam = GetContextIndentationParam(a:Context)

	"Identation and formatting
	if(a:Options =~? ";PrepLineBefore;")
		:call insert(l:IndentedLines, "", 0)
	endif

	"Calculate EndLineIndex
	if (len(l:IndentedLines) > 0)
		let EndLineIndex = l:LineNumber + len(l:IndentedLines) - 1
	else
		let EndLineIndex = l:LineNumber
	endif

	"Warning! We must prepend line after AFTER we initialized EndLineIndex
	"(because we need to return the last MEANINGFUL line index)
	if(a:Options =~? ";PrepLineAfter;")
		:call add(l:IndentedLines, "")
	endif

	if(a:Options !~? ";NoDefaultIndent;")
		let IndentParam = GetContextIndentationParam(a:Context)
		"By default we ident on the level of the context PLUS one
		"However, when a special argument is given, we indent on level
		"of the context
		if ((a:Options !~? ";NoPlusOneIndent;") && ContextRequiresExtraIndentation(a:Context, a:Options))
			let IndentParam += 1
		endif
		:call IdentBlock(l:IndentedLines, IndentParam)
	endif

	" Appending to buffer
	let OldEnd = line('$')
	"WARNING: We must count ONLY start index!
	let CountLinesBeyondEnd = (l:LineNumber - OldEnd)
	if (IsDebug)
		echo "CountLinesBeyondEnd: ".CountLinesBeyondEnd
	endif
	if(CountLinesBeyondEnd > 0)
		let i = 0
		:while i < CountLinesBeyondEnd
			:call append(line('$'), [''])
			let i += 1
		:endwhile
	endif

	"Cursor
	"if(a:Options !~? ";LockCur;")
	"	WHY the HELL it does NOt work?
		:call JumpAfterAt(l:LineNumber, l:IndentedLines)
		:redraw!
		":call cursor(l:LineNumber + len(l:IndentedLines), 1)

		"echo "AddIndentedCodeLinesAt: TEST: Here"
	
		"WHY the HELL it does NOt work?
		":call cursor(5, 1)
		":call cursor(2, 1)
	"endif

	:call append(l:LineNumber, l:IndentedLines)
	:redraw!

	if IsDebug
		let debug_lines = []
		:call add(debug_lines, 'DEBUG: AddIndentedCodeLines')
		:call add(debug_lines, 'Options:'.a:Options)
		:call add(debug_lines, 'LineNumber:'.LineNumber)
		:call add(debug_lines, 'len(IndentedLines):'.len(l:IndentedLines))
		:call add(debug_lines, 'IndentParam:'.IndentParam)
		:call add(debug_lines, 'EndLineIndex:'.EndLineIndex)
		:call EchoBlock(0, debug_lines, '')
	endif

	return EndLineIndex
:endfunction

:function! GetSectionName_RightHereForEntity(Context)
	let EntityName = GetContextEntityName(a:Context)
	return 'RightHere_'.EntityName
:endfunction
 

"Prepares the buffer code for inserting the private code RIGHT HERE
"Returns: line where to  insert private lines
:function! PrepareLines_ForRightHereCode_ReturnInsertLine(PublicContext, EndPublicLineIndex, PrivateLines, Options)
		let IsDebug = 0

		"Find section if it exists
		let SectionName = GetSectionName_RightHereForEntity(a:PublicContext)
		let StartLine = GetContextLine(a:PublicContext)
		let FoundSectionIndex = FindSectionStartLine_InRange(StartLine, line('$'), SectionName)

		if(IsDebug)
			echo "DEBUG: PrepareLines_ForRightHereCode: FoundSectionIndex=".FoundSectionIndex
		endif

		"If section not found, create it
		if(FoundSectionIndex < 0)
			let l:Comment = 'Adhoc impl lines (WARNING! move to .cpp file, otherwise will fail to compile!)'
			let l:Category = ''
			let l:Ops = ''
			let SectionInsertLineIndex = (GetContextEndLine(a:PublicContext)) + 1
			:call AddSectionBracketLinesAt(GetContextAt(SectionInsertLineIndex, ''), SectionName, l:Comment, l:Category, l:Ops)

			if (IsDebug)
				echo 'DEBUG: PrepareLines_ForRightHereCode: SectionInsertLineIndex='.SectionInsertLineIndex
			endif


		endif
		let InsertLine = FindSectionEndLine_InRange(StartLine, line('$'), SectionName) 

		if (IsDebug)
			echo 'DEBUG: PrepareLines_ForRightHereCode: InsertLine='.l:InsertLine
		endif

		return l:InsertLine
:endfunction

"Adds private implementation when right-here mode is used
:function! AddPrivateCode_RightHere(PublicContext, EndPublicLineIndex, PrivateLines, Options)
		"TODO: Find best insert line
		let l:InsertLine = PrepareLines_ForRightHereCode_ReturnInsertLine(a:PublicContext, a:EndPublicLineIndex, a:PrivateLines, a:Options)

		let l:Context = GetContextAt(l:InsertLine, a:Options)
		:call AddIndentedCodeLinesAt(l:Context, a:PrivateLines, a:Options)
:endfunction

"**** Appends private part of the code 
"(part that is typically to be included in the .cpp file)
"
" Can insert code either into the current file (based on options)
" or switch/load other buffer
" Automatically restores context (cursor position, current buffer) 
" based on options
"
" Arguments:
" 	Public context: context where do we inserted public lines
" 	End public line index: the LAST line, where public code was inserted
" 	(EXCLUDING extra newlines)
:function! AddPrivateCode_IfShould(PublicContext, EndPublicLineIndex, PrivateLines, Options)
	let l:AddPriv = (a:Options !~ ";NoPriv;") 
	if BoolNot(l:AddPriv)
		return -1
	endif


	" New Options
	let l:NewOptions = a:Options
	let l:AddSepLine = 1
	if(l:AddSepLine)
		let l:NewOptions .= ";PrepLineBefore;"
	endif

	"Should we insert private lines right after the public code
	let l:InsertPrivateHere = (a:Options =~ ";PrivHere;")
	if(l:InsertPrivateHere)
		:call AddPrivateCode_RightHere(a:PublicContext, a:EndPublicLineIndex, a:PrivateLines, l:NewOptions)
	endif
	
:endfunction

"Add idented code lines of SINGLE file and automaticall perform necessary identation (for example, when
"inside namespace, etc.)
:function! AddIndentedCodeLines(Lines, Options)
	let l:Context = ContextOrCurr({}, a:Options)
	let l:LineNumber = GetContextLine(l:Context)
	:call AddIndentedCodeLinesAt(l:LineNumber, a:Lines, a:Options)
:endfunction

:function! GetUpdatedContext(Context, Options)
	return GetContextAt(GetContextLine(a:Context), a:Options)
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
	let IsDebug = 0
	let l:PublicLineNumber = GetContextLine(a:Context)
	let l:AddPublic = (a:Options !~ ";NoPublic;")

	"Adding public code
	let l:BestPublicContext = ContextOrCurr(a:Context, a:Options)
	let l:EndPublicLineIndex = AddIndentedCodeLinesAt(l:BestPublicContext, a:PublicLines, a:Options)

	if(IsDebug)
		echo 'DEBUG AddCodeAt'
		echo 'PublicLineNumber: '.l:PublicLineNumber
		echo 'EndPublicLineIndex: '.l:EndPublicLineIndex
	endif

	"Why the hell it does not work ever here?
	":call cursor(l:EndPublicLineIndex, 1)

	"Adding private code"
	"WARNING!!! We must recalculate context after public lines are added
	"(as lines are shifted)!
	let l:BestPublicContext = GetUpdatedContext(l:BestPublicContext, a:Options)
	:call AddPrivateCode_IfShould(l:BestPublicContext, l:EndPublicLineIndex, a:PrivateLines, a:Options)
	return l:PublicLineNumber
:endfunction

"Append both declaration and definition at the given context at CURRENT line
" Returns:
" 	Index of line, where public lines where inserted
:function! AddCode(PublicLines, PrivateLines, Options)
	let l:Context = GetContextAt(line('.'), a:Options)
	return AddCodeAt(l:Context, a:PublicLines, a:PrivateLines, a:Options)
:endfunction

"*** Comment
"Arguments:
"	InputLines - set of lines passed initially to make comment
"
"	IsAsterisk - will use /**/ comment 
"	(otherwise will use // line comment)
:function! GetLines_CppComment(IsAsterisk, InputLines, Options)
	"Ever if we have no lines, and skip comment behaviour is disabled,
	"we should add comment
	let l:ShouldSkipComment = (a:Options =~# "SkipEmptyCom;")
	if (l:ShouldSkipComment && (len(a:InputLines) == 0))
		return []	
	endif

	let l:SingleLine = (len(a:InputLines) <= 1) && BoolNot(a:Options =~# "MultiCom;")
	
	"Building comment
	let l:res_lines = deepcopy(a:InputLines)
	if a:IsAsterisk
	
		"Here building /**/ comment
		if l:SingleLine
			if (len(a:InputLines) == 0)
				"We add empty line if comment is empty to force
				"comment generation
				:call add(l:res_lines, "")	
			endif
			let l:l = '/** '.l:res_lines[0].'*/'
			let l:res_lines[0] = l:l
		else
			"Multiline comment here
			:call PrependBlock(l:res_lines, "* ")
			:call insert(l:res_lines, '/**', 0)
			:call add(l:res_lines, '*/')
		endif
	else
		"Here building // comment
		if (len(a:InputLines) == 0)
			"We add empty line if comment is empty to force
			"comment generation
			:call add(l:res_lines, "")	
		endif
		:call PrependBlock(l:res_lines, '// ')
	endif
	return l:res_lines
:endfunction

" Adds a new comment code at the given context, if it should be done according
" to options
" Returns: Index of line, where comment was inserted
:function! AddCode_CppComment_IfShould(Context, IsAsterisk, InputLines, Options)
	let l:lines = GetLines_CppComment_IfShould(a:IsAsterisk, a:InputLines, a:Options)
	if len(l:lines) > 0
		let l:res = AddCodeAt(GetBestContext(a:Context, a:Options), l:lines, [], a:Options)
	endif
	return GetContextLine(a:Context)
:endfunction

:function! GetLines_CppComment_IfShould(IsAsterisk, InputLines, Options)
	if (a:Options !~# "NoCom;")
		let l:lines = GetLines_CppComment(a:IsAsterisk, a:InputLines, a:Options)
		return l:lines
	endif
	return []
:endfunction

:function! GetLines_DefaultNamedComment_IfShould(IsAsterisk, Name, CommentString, Ops)
	let l:CommentTextLines = GetLines_DefaultNamedCommentString_Text(a:Name, a:CommentString, a:Ops)
	let l:CommentLines = GetLines_CppComment_IfShould(a:IsAsterisk, l:CommentTextLines, a:Ops)
	return l:CommentLines
:endfunction

:function! GetLines_DefaultNamedCommentString_Text(Name, CommentString, OptionString)
	let l:comm_lines = [ a:CommentString ] "TODO: Split by string
	let l:lines = []

	let ShouldUseName = (a:CommentString == '')
	"At this time we either use a comment or just name
	let ShouldUseCommentStr = BoolNot(ShouldUseName)

	if ShouldUseName
		"Use name if comment lines are empty
		:call add(l:lines, a:Name)
	endif
	if ShouldUseCommentStr
		:call extend(l:lines, l:comm_lines)
	endif
	return l:lines
:endfunction

"Extract common arguments of the command, that are typically specified
"Arguments to be extracted:
"	BaseArgs - must be specified as the first argument of Dictionary Type
"	Ops - must be specified as the SECOND string
"	(according to default ExtractOptions)
"Returns: true if failed when extacting arguments
"Options are returned as a list, where zero element is the passed string of options
"Custom args - the rest of all unprocessed arguments
let g:MaxCount_BaseCmdArgs = 2
:function! ExtractCmdArgs_TrueOnFail(ExtractOptions, ArgList, OutContext, OutBaseArgs, OutOpsList, OutCustomArgs)
	"Basic arguments"	
	let l:no_base_args = GetBaseArgsChecked(a:ArgList, a:OutBaseArgs)
	if l:no_base_args
		return
	endif

	"Options
	if len(a:ArgList) > 1
		"Options string is specified
		let l:OpsString = a:ArgList[1]
	else
		"Options string is not specified
		let l:OpsString = ""
	endif
	
	"Add options string to the list of options
	if len(a:OutOpsList) == 0
		:call add(a:OutOpsList, l:OpsString)
	else
		let a:OutOpsList[0] = l:OpsString
	endif

	"Extracting custom args
	if len(a:ArgList) >= g:MaxCount_BaseCmdArgs
		:call extend(a:OutCustomArgs, a:ArgList[g:MaxCount_BaseCmdArgs:])
	endif
		
	"Current context
	:call ResetDict(a:OutContext, ContextOrCurr(GetCmdBase_Context(a:OutBaseArgs), l:OpsString))
:endfunction

:function! GetLines_SectionComment_IfShould(Name, CommentString, Category, Ops)
	let IsAsterisk = 0
	if(a:CommentString == '')
		return []
	endif
	return GetLines_DefaultNamedComment_IfShould(IsAsterisk, a:Name, a:CommentString, a:Ops)
:endfunction

:function! GetSectionStartLine(SectionName)
	return '// ~ '.a:SectionName.' Begin'
:endfunction

:function! GetSectionEndLine(SectionName)
	return '// ~ '.a:SectionName.' End'
:endfunction

"Returns a pair of lines at the given location
"// ~ SectionName Begin
"// ~ SectionName End
:function! GetSectionBracketLines(SectionName, Comment, Category, Options)
	let lines = []
	:call add(lines, GetSectionStartLine(a:SectionName))
	:call extend(lines, GetLines_SectionComment_IfShould(a:SectionName, a:Comment, a:Category, a:Options))
	:call add(lines, GetSectionEndLine(a:SectionName))
	return lines
:endfunction

:function! AddSectionBracketLinesAt(Context, SectionName, Comment, Category, Options)
	let lines = GetSectionBracketLines(a:SectionName, a:Comment, a:Category, a:Options)
	let NewOps = a:Options.';PrepLineBefore;'
	let IsDebug = 0
	if (IsDebug)
		echo 'DEBUG: AddSectionBracketLinesAt'
		echo 'GetContextLine: '.GetContextLine(a:Context)
	endif
	return AddIndentedCodeLinesAt(a:Context, lines, NewOps)
:endfunction

:function! AddSectionBracketLinesAtLine(LineNumber, SectionName, Comment, Category, Options)
	 return AddSectionBracketLinesAt(GetContextAt(a:LineNumber, a:Options), a:SectionName, a:Comment, a:Category, a:Options)
:endfunction

:function! AddSectionBracketLines(SectionName, Comment, Category, Options)
	return AddSectionBracketLinesAt(GetCurrContext(a:Options), a:SectionName, a:Comment, a:Category, a:Options)
:endfunction

:function! CmdFunc_AddCode_Section(...)
	let l:ShowDebugLines = 0

	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]
	let l:ContextType = GetContextType(l:Context)

	":call EchoContext(0, "Debug context: CmdFunc_AddCode_CppSection", l:Context, "")

	"Checking custom args
	let l:Name_ArgIndex = 0
	let l:RestArgs_ArgIndex = 1
	if NoArg(1, l:MyArgs, "Name", l:Name_ArgIndex)
		return 0
	endif
	let l:Name = l:MyArgs[l:Name_ArgIndex]
	let RestOfString = GetOrDefault(l:MyArgs, l:RestArgs_ArgIndex, '')
	let l:RestArgs_Dict = ExtractDefaultArguments(RestOfString, [])

	let l:Comment = GetComment_FromExtractedDict(l:RestArgs_Dict)
	let l:ExtraOps = GetJoinedOps_FromExtractedDict(l:RestArgs_Dict)
	let l:Category = GetJoinedCategories_FromExtractedDict(l:RestArgs_Dict)

	"Update the global ops with extra ops:
	let l:Ops .= ';'.l:ExtraOps.';'

	"DEBUG {
		if l:ShowDebugLines
			let l:DebugLines = []
			:call add(l:DebugLines, "DEBUG CmdFunc_AddCode_Section: ")
			:call add(l:DebugLines, "Comment=".l:Comment)
			:call EchoBlock(0, l:DebugLines, "")
		endif
	"DEBUG }
	"
	
	:call AddSectionBracketLines(l:Name, l:Comment, l:Category, l:Ops)

	return 1
:endfunction

"Add section of form // ~ Section Name Begin
:command! -nargs=* Sect :call CmdFunc_AddCode_Section({}, <f-args>)

:source D:/!DV_SCRIPT/VIM/CONFIG/Core/mycore_keys.vimrc
