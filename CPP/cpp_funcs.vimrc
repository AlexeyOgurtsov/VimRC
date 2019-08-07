"C++ functions for adding C++ stuff

:function! UpdateStringOpenState_Curly(OpenState, Line)
	return UpdateStringOpenState(a:OpenState, a:Line, "{", "}")
:endfunction

:function! GetExactContextAt(LineIndex, Options)
	return GetCppExactContextAt(a:LineIndex, a:Options)
:endfunction

"Determines, whether the given line string is cpp function header
:function! IsCppFunctionHeaderLine(Line, OutDecl)
	let l:IsFunction = (a:Line =~# '^\s*\(.*\)\(\w*\)\s*(.*)\s*\(.*\)\s*{*\s*$')
	return l:IsFunction
:endfunction

"Determines, whether the given line string is cpp class/struct header
:function! IsCppClassHeaderLine(Line, OutDecl)
	let l:IsClass = (a:Line =~# '^\s*\(class\|struct\)\s*\(\w\+\)\s*\(\|:\s*\w\+\s*\(\w\+\)\)\s*{*\s*$')
	return l:IsClass
:endfunction

"Returns base class from the C++ class header line
"(returns "" if no base class specified)
:function! GetCppBaseClassName_FromHeader(HeaderLine)
	let lexems = split(a:HeaderLine)	
	let i = 0
	while i < len(lexems)	
		if( lexems[i] == ':' )
			let access_i = i + 1
			while access_i < len(lexems)
				if(lexems[access_i] == 'public' || lexems[access_i] == 'private' || lexems[access_i] == 'protected')
					break
				endif
				let access_i += 1
			endwhile
			if(access_i) >= len(lexems)
				break
			endif
			let class_base_name_index = access_i + 1
			if(class_base_name_index) >= len(lexems)
				break
			endif
			let class_base_name = lexems[class_base_name_index]
			return class_base_name
		endif

		let i += 1
	endwhile
	return ""
:endfunction


"Return Cpp Class name from C++ class header
"WARNING!!! The header must be valid!!!
:function! GetCppClassName_FromHeader(HeaderLine)
	let lexems = split(a:HeaderLine)	
	let i = 0
	while i < len(lexems)	
		if(lexems[i] == 'class' || lexems[i] == 'struct')
			let class_name_index = i + 1
			let class_name = lexems[class_name_index]
			return class_name
		endif

		let i += 1
	endwhile
	return ""
:endfunction

"Returns whether IsStruct class header
"WARNING!!! The header must be valid class header!!!
:function! GetIsStruct_FromHeader(HeaderLine)
	let lexems = split(a:HeaderLine)	
	let i = 0
	while i < len(lexems)	
		if(lexems[i] == 'struct')
			return 1
		endif

		if(lexems[i] == 'class')
			return 0
		endif

		let i += 1
	endwhile
	return ""
:endfunction

:function! IsCppAccessSpecLine(Line)
	"TODO: Skip C++ comments at the end
	return (a:Line =~# '^\s*public:') || (a:Line =~# '^\s*private:') || (a:Line =~# '^\s*protected:')
:endfunction

"Find line index of form "Spec:" in the given lines
"(return -1 if not found)
:function! FindAccessSpec_LineIndex(Lines, Spec)
	let i = 0
	while i < len(a:Lines)
		let line = GetLeftSpacesTrimmed(a:Lines[i])

		if(line =~# ('^'.a:Spec.':'))
			return i
		endif

		let i += 1
	endwhile	
	return -1
:endfunction

:function! FindPrivate_LineIndex(Lines)
	return FindAccessSpec_LineIndex(a:Lines, "private")
:endfunction

:function! FindPublic_LineIndex(Lines)
	return FindAccessSpec_LineIndex(a:Lines, "public")
:endfunction

:function! FindProtected_LineIndex(Lines)
	return FindAccessSpec_LineIndex(a:Lines, "protected")
:endfunction

:function! ComputeCppClassContext(OutContext, HeaderLine, LinesInsideBody, Options)
	let IsDebugEcho = 0

	"TODO! Body of class may be splitted into a SET of lines!
	
	"TODO: We should compute cpp class name generally (ever if context is
	"not  class)
	let a:OutContext[g:Context_ClassName] = GetCppClassName_FromHeader(a:HeaderLine)
	let a:OutContext[g:Context_ClassBaseName] = GetCppBaseClassName_FromHeader(a:HeaderLine)

	let a:OutContext[g:Context_IsStruct] = GetIsStruct_FromHeader(a:HeaderLine)
	let a:OutContext[g:Context_ClassPublicLineIndex] = FindPublic_LineIndex(a:LinesInsideBody)
	let a:OutContext[g:Context_ClassPrivateLineIndex] = FindPrivate_LineIndex(a:LinesInsideBody)
	let a:OutContext[g:Context_ClassProtectedLineIndex] = FindProtected_LineIndex(a:LinesInsideBody)

	if IsDebugEcho
		echo "DEBUG: ComputeCppClassContext: "
		echo "HeaderLine: ".a:HeaderLine
		:call EchoContext(0, "Context", a:OutContext, a:Options)
	endif
:endfunction

"Determines, whether the given line string is cpp enum class header
:function! IsCppEnumClassHeaderLine(Line, OutDecl)
	let l:IsClassHeader = (a:Line =~# '^\s*enum\s*\(class\|\)\s*\(\w\+\)\s*\(\|:\s*\(\w\+\)\)\s*{*\s*$')
	return l:IsClassHeader
:endfunction

:function! IsLineMeaningful(Line)
	"TODO: Override to remove comments
	return IsCoreLineMeaningful(a:Line)
:endfunction

:function! AreLinesMeaningful(Lines)
	"TODO: Override to remove comments
	return AreCoreLinesMeaningful(a:Lines)
:endfunction

" Returns string assigned to the given enum flag literal string
" Returns "" (empty string) if not the enum flag literal
" (strings that contain "1 << N" are considered yet literal strings)
:function! GetEnumFlagValueStr(Line)
	let l:CommaPattern = '\s*\(\|,\)\s*'
	let l:NamePattern = '\w\+'
	let l:AnyPattern = '.*'
	let l:Pattern = '^'.l:CommaPattern
	let l:Pattern .= l:NamePattern
	let l:Pattern .= '\s*=\s*'
	let l:Pattern .= '\s*1\s*'
	let l:Pattern .= '\s*<<\s*'
	let l:Pattern .= '\s*\w\+\s*'
	let l:Pattern .= l:AnyPattern
	let l:Pattern .= l:CommaPattern
	let l:Pattern .= '$'

	let l:IsFlagLiteral = (a:Line =~# l:Pattern)
	"echo "GetEnumFlagValueStr: DEBUG: IsFlagLiteral=".l:IsFlagLiteral

	if BoolNot(l:IsFlagLiteral)
		return ""

	else
		"Here we have the enum flag literal
		
		let AfterShiftIndex = strridx(a:Line, "<<") + len("<<")

		"Lets find the last from end digit or alpha numeric - it's our
		"expression start
		let expr_start_idx = -1
		"And the first from end digit or alpha-numeri
		" -it's our expression end
		let expr_end_idx = -1
		let l:i = AfterShiftIndex "OLD: len(a:Line) - 1
			

		while ((expr_start_idx < 0) || (expr_end_idx < 0)) && (l:i < len(a:Line))
			"echo "DEBUG: i=".l:i." val=".strpart(a:Line, l:i, 1)

			"Here we found character, that can be a part of the
			"string we search for
			if(strpart(a:Line, l:i, 1) =~ '\w')
				if(expr_start_idx < 0)
					"Start is not found yet
					let expr_start_idx = l:i
				endif
			else
				"Curr character is not part of the string we
				"search for
				if(expr_start_idx >= 0)
					let expr_end_idx = l:i - 1
				endif
			endif

			let l:i += 1
		endwhile
		if(l:expr_end_idx < 0)
			let l:expr_end_idx = len(a:Line) - 1
		endif

		let l:FlagLiteralValue = strpart(a:Line, expr_start_idx, (expr_end_idx - expr_start_idx) + 1)
		return l:FlagLiteralValue
	endif
	return "" "NOT literal
:endfunction

"Find line indices of all enum literals with assigned values of form "1 << N"
:function! FindLineIndices_EnumFlagLiterals(Lines)
	let l:LineIndices = []
	let l:i = 0
	while l:i < len(a:Lines)
		if (GetEnumFlagValueStr(a:Lines[l:i]) != "")
			:call add(l:LineIndices, l:i)
		endif
		let l:i += 1
	endwhile
	return l:LineIndices
:endfunction

:function! CalculateNumEnumLiterals(Lines)
	:if BoolNot(AreLinesMeaningful(a:Lines) )
		return 0	
	:endif

	let CountByPattern = CountLinesByPattern(a:Lines, '\s*\,\s*')
	return CountByPattern + 1 

:endfunction

:function! ComputeCppEnumContext(OutContext, HeaderLine, LinesInsideBody, Options)
	let IsDebug = 0

	let a:OutContext[g:Context_NumEnumLiterals] = CalculateNumEnumLiterals(a:LinesInsideBody)
	if(IsDebug)
		echo 'DEBUG: ComputeCppEnumContext: NumLiterals='.a:OutContext[g:Context_NumEnumLiterals]
	endif

	let EnumFlag_LineIndices  = FindLineIndices_EnumFlagLiterals(a:LinesInsideBody)

	let l:MinimalEnumFlag_LineIndex = -1
	let l:MaxEnumFlag_LineIndex = -1
	let l:EnumFlag_FirstHoleIndex = -1

	let l:MinimalEnumFlag_Value = 0
	let l:MaxEnumFlag_Value = 0
	let l:FirstHole_Value = 0

	let l:i = 0
	while l:i < len(EnumFlag_LineIndices)
		let LineIndex = EnumFlag_LineIndices[l:i]
		let line = a:LinesInsideBody[LineIndex]
		let EnumFlagShiftValue = GetEnumFlagValueStr(line)

		let IsNumberFlagShift = (EnumFlagShiftValue =~# '^\d\+$')
		if IsNumberFlagShift
			let NumberFlagShift = str2nr(EnumFlagShiftValue)

			if(l:MinimalEnumFlag_LineIndex < 0 || (l:MinimalEnumFlag_Value > NumberFlagShift))
				let l:MinimalEnumFlag_LineIndex = LineIndex
				let l:MinimalEnumFlag_Value = NumberFlagShift
			endif

			if(l:MaxEnumFlag_LineIndex < 0 || (l:MaxEnumFlag_Value < NumberFlagShift))
				let l:MaxEnumFlag_LineIndex = LineIndex
				let l:MaxEnumFlag_Value = NumberFlagShift
			endif

			"TODO Hole calculation
		endif
		
		let l:i += 1
	endwhile

	"DEBUG {
		"echo "ComputeCppEnumContext: DEBUG: MinimalEnumFlag_LineIndex=".l:MinimalEnumFlag_LineIndex
		"echo "ComputeCppEnumContext: DEBUG: MinimalEnumFlag_Value=".l:MinimalEnumFlag_Value

		"echo "ComputeCppEnumContext: DEBUG: MaxEnumFlag_LineIndex=".l:MaxEnumFlag_LineIndex
		"echo "ComputeCppEnumContext: DEBUG: MaxEnumFlag_Value=".l:MaxEnumFlag_Value
	"DEBUG }

	let a:OutContext[g:Context_MinimalEnumFlag_LineIndex] = l:MinimalEnumFlag_LineIndex
	if(l:MinimalEnumFlag_LineIndex > 0)
		let a:OutContext[g:Context_MinimalEnumFlagValue] = l:MinimalEnumFlag_Value
	endif
	let a:OutContext[g:Context_MaxEnumFlag_LineIndex] = l:MaxEnumFlag_LineIndex
	if(l:MaxEnumFlag_LineIndex > 0)
		let a:OutContext[g:Context_MaxEnumFlagValue] = l:MaxEnumFlag_Value
	endif
	let a:OutContext[g:Context_EnumFlagHole_LineIndex] = l:EnumFlag_FirstHoleIndex
	if(l:EnumFlag_FirstHoleIndex > 0)
		let a:OutContext[g:Context_EnumFlagHoleValue] = ""
	endif

:endfunction

:function! ComputeLinesInsideContextBody(OpenBraceLineIndex, ContextType, LinesInsideBody)
	let Lines = []
	:call ClearList(a:LinesInsideBody)
	
	let line_index = a:OpenBraceLineIndex
	let EndBracketFound = 0
	
	let NumOpenedBrackets = 0
	:while (line_index <= line('$')) && BoolNot(EndBracketFound)
		let l = getline(line_index)
		"echo "line_index=".line_index." ;NumOpenedBrackets=".NumOpenedBrackets

		let char_i = 0
		while char_i < len(l)
			if stridx(l, "{", char_i) == char_i
				let NumOpenedBrackets += 1
			elseif stridx(l, "}", char_i) == char_i
				let NumOpenedBrackets -= 1
			endif
			let char_i += 1
		endwhile

		"We must check line indices from after-first line only
		if NumOpenedBrackets == 0
			let EndBracketFound = 1
		else
			let line_index += 1
			"echo "NEXT: line_index=".line_index." ;NumOpenedBrackets=".NumOpenedBrackets
			let l = getline(line_index)
			:call add(a:LinesInsideBody, l)
		endif
	:endwhile

	if (len(a:LinesInsideBody) > 0)
		:call remove(a:LinesInsideBody, len(a:LinesInsideBody) - 1)
	endif

	return Lines
:endfunction

"Determine cpp context lines and its type
:function! ExtractCppContextLines(OutCppContext)
	let IsDebug = 0

	let l:ContextType = g:ContextType_Unknown

	let l:ContextLine = GetContextLine(a:OutCppContext)
	lockvar l:ContextLine

	"Searching up until the first inner entity (class, func etc.) is found
	"or start of file is reached

	"Count of cpp-style '}' unopened operator brackets found
	let BracketsToOpen = 0 
	let InitiallyOpened = 0
	let OpenState = {"CountToOpen":BracketsToOpen, "IsInitiallyOpened":InitiallyOpened}
	let IsContextHeaderFound = 0
	let LineIndex = l:ContextLine
	while BoolNot(IsContextHeaderFound) && ( LineIndex > 0 )
		let CurrLine = getline(LineIndex)

		"Count brackets
		:call UpdateStringOpenState_Curly(OpenState, CurrLine)
		let BracketsToOpen = OpenState["CountToOpen"]
		let InitiallyOpened = OpenState["IsInitiallyOpened"]

		let l:IsReallyOpened = (InitiallyOpened && (BracketsToOpen <= 0)) || (LineIndex == l:ContextLine) 
		let l:OpenBracketExists = (FindFirstFromAndBefore(LineIndex, "{", "}") > 0)

		" DEBUG {
		"let l:debug_lines = [ "******* ExtractCppContextLines: DEBUG: " ]
		":call add(l:debug_lines, "LineIndex=".LineIndex)
		":call add(l:debug_lines, "; Line=".CurrLine)
		":call add(l:debug_lines, "; IsContextHeaderFound=".IsContextHeaderFound)
		":call add(l:debug_lines, "; BracketsToOpen=".BracketsToOpen)
		":call add(l:debug_lines, "; InitallyOpened=".InitiallyOpened)
		":call add(l:debug_lines, "; IsReallyOpened=".l:IsReallyOpened)
		":call add(l:debug_lines, "; OpenBracketExists=".l:OpenBracketExists)
		":call EchoBlock(0, l:debug_lines, "")
		" DEBUG }

		if ( l:IsReallyOpened && l:OpenBracketExists )
			let l:Decl = {}
			if (IsCppEnumClassHeaderLine(CurrLine, l:Decl))
				let l:ContextType = g:ContextType_Enum
				let IsContextHeaderFound = 1
			elseif (IsCppClassHeaderLine(CurrLine, l:Decl))
				let l:ContextType = g:ContextType_Class
				let IsContextHeaderFound = 1
			elseif (IsCppFunctionHeaderLine(CurrLine, l:Decl))
				let l:ContextType = g:ContextType_Function
				let IsContextHeaderFound = 1
			else
			"TODO: Always check when add new types
			endif
		endif

		let LineIndex -= 1 
	endwhile

	"Assign context type as early as you can
	let a:OutCppContext[g:Context_Type] = l:ContextType

	if IsContextHeaderFound
		let l:HeaderLineIndex =  (l:LineIndex + 1)
		let l:HeaderLine = getline(l:HeaderLineIndex)

		let l:StartLine = l:HeaderLineIndex
		let a:OutCppContext[g:Context_StartLine] = l:StartLine

		let OpenBraceLineIndex = FindFirstFrom(l:StartLine, '{')

		"Lines inside body (not including { and } braces )
		let LinesInsideBody = []
		:call ComputeLinesInsideContextBody(OpenBraceLineIndex, l:ContextType, LinesInsideBody)
		let a:OutCppContext[g:Context_LinesInsideBody] = LinesInsideBody

		"Compute end line index based on the lines inside body
		let a:OutCppContext[g:Context_HeaderLineIndex] = HeaderLineIndex
		let a:OutCppContext[g:Context_OpenBraceLine] = OpenBraceLineIndex
		let a:OutCppContext[g:Context_EndLine] = OpenBraceLineIndex + len(LinesInsideBody)

		let a:OutCppContext[g:Context_IndentationParam] = GetLineIndentationParam(getline(l:StartLine))
		"echo "Debug: StartLine=".l:StartLine." LineIndentationParam=".GetLineIndentationParam(getline(l:StartLine))
		"echo "Debug: StartLine=".l:StartLine." OutCppContext[IndentationParam]=".a:OutCppContext[g:Context_IndentationParam]

		if l:ContextType == g:ContextType_Enum
			:call ComputeCppEnumContext(a:OutCppContext, l:HeaderLine, LinesInsideBody, "")
		elseif l:ContextType == g:ContextType_Class
			:call ComputeCppClassContext(a:OutCppContext, l:HeaderLine, LinesInsideBody, "")
		elseif l:ContextType == g:ContextType_Function
			"Other type
		endif
	else
		"No context header found, so we are inside the global
		let a:OutCppContext[g:Context_StartLine] = 0
		let a:OutCppContext[g:Context_OpenBraceLine] = 0
		let a:OutCppContext[g:Context_EndLine] = line('$')

		let a:OutCppContext[g:Context_IndentationParam] = 0 "TODO: Calculate based on the current namespace
	endif

	if IsDebug
		echo 'ExtractCppContextLines'
		:call EchoContext(0, 'Context', a:OutCppContext, '')
	endif

	return l:ContextType
:endfunction

" Cpp Context at the given line
" Members:
" -All from Core
:function! GetCppExactContextAt(LineIndex, Options)
	let l:res = {}
	:call extend(l:res, GetCoreExactContextAt(a:LineIndex, a:Options))
	let l:ContextType = ExtractCppContextLines(l:res)
	return l:res
:endfunction

"Find the appropriate for insertion Private/Protected/Public section line index 
"with the given access type 
"Section may be explicitly or implicitly specified (for example, as public
"section for struct)
"returns: found line index in buffer space
"(or - 1 if there's not section with the given access type)
:function! FindAccessSection(Context, AccessType, Options)
	let IsDebug = 0

	let PublicLineIndex = LineIndex_LocToBuf(a:Context, GetContextClass_PublicLine(a:Context))
	let PrivateLineIndex = LineIndex_LocToBuf(a:Context, GetContextClass_PrivateLine(a:Context))
	let ProtectedLineIndex = LineIndex_LocToBuf(a:Context, GetContextClass_ProtectedLine(a:Context))

	let IsStruct = GetContextIsStruct(a:Context)
	let OpenBraceLineIndex = GetContextOpenBraceLine(a:Context)

	if(IsDebug)
		let debug_lines = []
		:call add(debug_lines, 'AccessType: '.GetAccessTypeString(a:AccessType))
		:call add(debug_lines, 'IsStruct: '.IsStruct)
		:call add(debug_lines, 'HeaderLineIndex: '.GetContextHeaderLineIndex(a:Context))
		:call add(debug_lines, 'OpenBraceLineIndex: '.OpenBraceLineIndex)
		:call add(debug_lines, 'PublicLineIndex: '.PublicLineIndex)
		:call add(debug_lines, 'PrivateLineIndex: '.PrivateLineIndex)
		:call add(debug_lines, 'ProtectedLineIndex: '.ProtectedLineIndex)
		:call EchoBlock(0, debug_lines, "")
	endif

	if(a:AccessType == g:AccessType_Protected)
		return ProtectedLineIndex

	elseif(a:AccessType == g:AccessType_Public)
		if(IsStruct)
			return OpenBraceLineIndex
		else
			return PublicLineIndex
		endif

	elseif(a:AccessType == g:AccessType_Private)
		if(PrivateLineIndex >= 0)
			return PrivateLineIndex
		endif

		"Here private line is not specified and we have class, so use
		"the default namespace
		if(BoolNot(IsStruct))
			return OpenBraceLineIndex
		endif
	endif

	return -1
:endfunction

"Find next line of section (no matter, whether it's a next section or out of
"scope of the given class)
:function! FindSectionNextLine(Context, StartLine, Options)
	let IsDebug = 0

	"Index of line of end of class ('}' line)
	let ClassEndLineIndex = (GetContextEndLine(a:Context) + 1)

	let IsFound = 0 "Is end of section found
	let LineIndex = (1 + a:StartLine) "line index in the current space
	while(BoolNot(IsFound) && (LineIndex < ClassEndLineIndex))
		if(IsCppAccessSpecLine(getline(LineIndex)))
			let IsFound = 1
			break
		endif
		let LineIndex += 1
	endwhile

	if (IsDebug)
		echo "DEBUG: FindexSectionNextLine"
		echo "StartLine=".a:StartLine
		echo "ClassEndLineIndex=".ClassEndLineIndex
		echo "LineIndex=".LineIndex
		echo "IsFound=".IsFound
	endif

	return LineIndex
:endfunction
"Searches for the last line of the given section
:function! FindSectionLastLine(Context, StartLine, Options)
	return (FindSectionNextLine(a:Context, a:StartLine, a:Options) - 1)
:endfunction

:function! SkipEmptyLinesUp(LineIndex, MaxCountEmptyLinesToKeep)
	let i = a:LineIndex
	let CountEmptyLinesFound = 0
	while(i >= 1 && IsEmptyOrSpaced(getline(i)))
		let i -= 1
		let CountEmptyLinesFound += 1
	endwhile
	if(CountEmptyLinesFound >= a:MaxCountEmptyLinesToKeep)
		let i += a:MaxCountEmptyLinesToKeep
	endif
	return i
:endfunction

"Returns best insert position for the given section
"StartLine - start line index in buffer space
"Returns: best insert position for the given section
:function! FindSectionInsertPosition(Context, StartLine, Options)
	let l:ResultLine = a:StartLine
	let l:SectionLastLine = FindSectionLastLine(a:Context, a:StartLine, a:Options)
	if(l:SectionLastLine >= 0)
		let l:InsertPosition = SkipEmptyLinesUp(l:SectionLastLine, 1)
		let l:ResultLine = l:InsertPosition
	endif
	return l:ResultLine
:endfunction

"Overriding getting context line from options for cpp
:function! GetUpdatedContextLine(Context, Options)
	let NewLineIndex = GetContextLine(a:Context)

	let DesiredAccessType = GetAccessTypeFromOptions(a:Options)
	if((DesiredAccessType != g:AccessType_Unknown))
		let SectionLineIndex = FindAccessSection(a:Context, DesiredAccessType, a:Options)
		if (SectionLineIndex >= 0)
			let NewLineIndex = FindSectionInsertPosition(a:Context, SectionLineIndex, a:Options)
		endif
	endif
	
	return NewLineIndex
:endfunction

:function! GetLines_GetVarOrFieldCommentText(IsField, TypeName, Name, InitExpr, OptionString)
	return GetLines_DefaultNamedCommentString_Text(a:Name, "", a:OptionString)
:endfunction

:function! GetLines_GetVarOrFieldComment_IfShould(IsField, TypeName, Name, InitExpr, OptionString)
	let l:CommentTextLines = GetLines_GetVarOrFieldCommentText(a:IsField, a:TypeName, a:Name, a:InitExpr, a:OptionString)
	let l:IsAsterisk = 1
	let l:CommentLines = GetLines_CppComment_IfShould(l:IsAsterisk, l:CommentTextLines, a:OptionString)
	return l:CommentLines
:endfunction

:function! GetLines_GetVarOrFieldComment_IfShould(IsField, TypeName, Name, InitExpr, OptionString)
	let l:CommentTextLines = GetLines_GetVarOrFieldCommentText(a:IsField, a:TypeName, a:Name, a:InitExpr, a:OptionString)
	let l:IsAsterisk = 1
	let l:CommentLines = GetLines_CppComment_IfShould(l:IsAsterisk, l:CommentTextLines, a:OptionString)
	return l:CommentLines
:endfunction

:function! GetLines_EnumClassComment_IfShould(Name, Ops)
	"TODO: ugly redirection
	return GetLines_GetVarOrFieldComment_IfShould(0, "", a:Name, "", a:Ops)
:endfunction

:function! GetLines_EnumLiteralComment_IfShould(Name, Ops)
	let IsAsterisk = 1
	let CommentString = "" "TODO: Take as argument
	return GetLines_DefaultNamedComment_IfShould(IsAsterisk, a:Name, CommentString, a:Ops)
:endfunction

:function! GetLines_FunctionComment_IfShould(Name, CommentString, Ops)
	let IsAsterisk = 1
	return GetLines_DefaultNamedComment_IfShould(IsAsterisk, a:Name, a:CommentString, a:Ops)
:endfunction

"*** #Include directive
"Include header:
"-Appends .h automatically (if not specified)
:function! GetLine_CppIncludeAt_General(Filename, IsSystem)
	let l:include_arg = a:Filename 
	:if a:IsSystem
		let l:left_quote = '<'
		let l:right_quote = '>'
	:else
		:if a:Filename !~ '.h$'
			let l:include_arg = a:Filename.'.h'
		:endif
		let l:left_quote = '"'
		let l:right_quote = '"'
	:endif
	let l:directive_line = '#include '.l:left_quote.l:include_arg.l:right_quote
	return l:directive_line
:endfunction
:function! CppIncludeAt_General(LineNumber, Filename, IsSystem)
	let l:directive_line = GetLine_CppIncludeAt_General(a:Filename, a:IsSystem)
	:call append(a:LineNumber, l:directive_line)
:endfunction
"Include header with the given header base path
"Include System
"*** Templated class names
"*** Variable definitions / declarations
"* Var: int x, float, etc.
"* Const var: const int x, const float x
"* Var: MySystem* pInSystem, MySystem** ppInSystem
"* ptr to Const: const MySystem* pInSystem
"* const ptr to Const: const MySystem* const pInSystem
"* with default arguments:
"* int x = 0
"*** Statement
"If statement
"Switch statement
"Template helpers
"* Returns string of form "template<class A, class B>" 
"* or "template<A, B>" (if IsWithClass is 0)
"*
"*Arguments: 
"*TemplParams - Dictionary mapping TemplParamName to type 
" (like "class" or "template<class> class ")
" Each name may be optionally prefixed with ORDER number
:function! GetTemplParamsString(TemplParams, IsWithClass)
	:if a:IsWithClass
		let l:line = "template"
	:else
		let l:line = ""
	:endif
	let l:OrderedTemplNames = keys(a:TemplParams)
	:call sort(l:OrderedTemplNames)
" TODO2:    remove the Number prefix
	:let l:ParamIndex = 0
	:if len(a:TemplParams) > 0
		let l:line = l:line . "<"
			:for TemplName in l:OrderedTemplNames
				"Appending comma if it's not the first argument
				:if l:ParamIndex > 0
					let l:line = l:line . ", "
				:endif
				" Forming parameter member string
				:if a:IsWithClass
					let l:line = l:line . a:TemplParams[TemplName] . " "
				:endif
				let l:line = l:line . TemplName
				"Increment param index
				let l:ParamIndex = l:ParamIndex + 1
			:endfor
		let l:line = l:line . ">"
	:endif
	return l:line
:endfunction
"Function member class prefix string
" WARNING! DOES include ::
:function! GetCppClassMemberPrefix(ClassName, TemplParams)
	let l:IsWithClass = (len(a:ClassName) > 0)
	let l:Res = ""
	let l:IsWithTemplKeyword = 0 "Should we add Template keyword?
	if (l:IsWithClass)
		let l:Res .= a:ClassName
        	let l:Res .= GetTemplParamsString(a:TemplParams, l:IsWithTemplKeyword)
	       	let l:Res .= "::"
	endif
	return l:Res
:endfunction
"Generates string of form Name(ContentString) [const]
:function! GetLine_CppFunc_NameArgs_AndSpecs(Name, ContentString, OptionString)
	let l:res_s = a:Name.'('.a:ContentString.')'
	:if (a:OptionString =~? ";c;") || (a:OptionString =~? ";const;") || (a:OptionString =~? ";Get;")
		let l:res_s = l:res_s . " const"
	:endif
	return l:res_s
:endfunction
"*** Function declaration
"* Declaration of the following form:
"* {virtual} ReturnType FunctionName(ContentString)
"* WARNING ; not appended!
:function! GetLine_CppFuncDecl_General(Name, ContentString, ReturnType, OptionString)
	let l:declaration_string = ""
	"static if necessary"
	:if (a:OptionString =~? ";S;") || (a:OptionString =~? ";Stat;") || (a:OptionString =~? ";Static;")
		let l:declaration_string = l:declaration_string . "static "	
	:endif
	"Virtual if necessary"
	let l:IsPureVirtual = (a:OptionString =~? ";Pure;") || (a:OptionString =~? ";PureV;") || (a:OptionString =~? ";PureVirt;")
	let l:IsVirtual = (a:OptionString =~? ";Virt;") || (a:OptionString =~? ";V;") || (l:IsPureVirtual)

	:if l:IsVirtual
		let l:declaration_string = l:declaration_string . "virtual "	
	:endif
	"" Return type
	let l:declaration_string = l:declaration_string . a:ReturnType
	:if len(a:ReturnType) > 0
		let l:declaration_string = l:declaration_string . " "	
	:endif
	let l:declaration_string = l:declaration_string. GetLine_CppFunc_NameArgs_AndSpecs(a:Name, a:ContentString, a:OptionString)
	"Forming prefix
	:if ((a:OptionString =~? ";Over;") || (a:OptionString =~? ";Ov;"))
		let l:declaration_string = l:declaration_string . " override"
	:endif
	:if l:IsPureVirtual
		let l:declaration_string = l:declaration_string . " =0"	
	:endif
	:if a:OptionString =~? ";Def;"
		let l:declaration_string = l:declaration_string . " =default"	
	:endif
	:if (a:OptionString =~? ";Delete;") || (a:OptionString =~? ";Del;")
		let l:declaration_string = l:declaration_string . " =delete"	
	:endif
	:call DebugEcho("DEBUG: GetLine_CppFuncDecl_General: OptionString=". a:OptionString)
	:call DebugEcho("DEBUG: GetLine_CppFuncDecl_General: declaration_string=". l:declaration_string)
	return l:declaration_string
:endfunction

"* Appends function declaration with ; included at the end
:function! CppFuncDecl_General(LineNumber, Name, ContentString, ReturnType, OptionString)
	let l:IsImpl = 1
	let l:declaration_string = GetLine_CppFuncDecl_General(l:IsImpl, a:Name, a:ContentString, a:ReturnType, a:OptionString)
	let l:formatted_decl_line = l:declaration_string.';'
	:call append(a:LineNumber, l:formatted_decl_line)
:endfunction
"*** Calculates default return statemen lines for function (NOT indented)
:function! GetLines_ReturnStmt(ValueStr)
	let l:res_line = 'return'
	if(len(a:ValueStr) > 0)
		let l:res_line .= ' '
	endif
	let l:res_line .= a:ValueStr
	let l:res_line .= ';'
	return [ l:res_line ]
:endfunction
:function! GetLines_DefaultReturnStmt(Name, ClassName, TemplParams, ContentString, ReturnType, OptionString)
	let l:Lines = []

	"Handling special cases for custom operators first
	:if (a:Name==#"operator=")
		return GetLines_ReturnStmt('*this')
	:endif

	:if (a:ReturnType != "void") && (a:ReturnType != "")
		:call add(l:Lines, GetLines_ReturnStmt('{}'))
	:endif
	return l:Lines
:endfunction
:function! GetLines_CppFuncDefaultBody(ReturnStmt, ContentString, ReturnType, OptionString)
	let l:Lines = []
	let l:ReturnStmtLines = deepcopy(a:ReturnStmt)
	:call extend(l:Lines, l:ReturnStmtLines)
	return l:Lines
:endfunction
"*** Returns content string updated for implementing
:function! GetFuncImplContentString(DeclContentString)
	"TODO: Insert const for types
	return a:DeclContentString
:endfunction 

"*** Type string of form "Type*" or "const Type*"
:function! GetTypeString_Ptr(TypeName, IsConst)
	let Tp = a:TypeName . "*"
	if(a:IsConst)
		let Tp = "const " . Tp
	endif
	return Tp
:endfunction
"*** Is type string Const or Non-const ptr
:function! IsPtr(TypeStr)
	return a:TypeStr =~ "*"
:endfunction
"*** Is type string Ptr to Const 
:function! IsPtrToConst(TypeStr)
	let deref_index = strridx(a:TypeStr, "*")
	if deref_index < 0
		return 0
	endif
	let const_index = stridx(a:TypeStr, "const")
	if const_index < 0
		return 0
	endif
	return const_index < deref_index
:endfunction
"* Whether PTR, to MUTABLE (non-const) value
:function! IsPtrToMutable(TypeStr)
	return (IsPtr(a:TypeStr)) && BoolNot(IsPtrToConst(a:TypeStr))
:endfunction
"**** Is type string RValue Ref
:function! IsRValueRef(TypeStr)
	return (a:TypeStr =~ "&&")
:endfunction
"**** Is type string Const or non-const ref and NOT RValue Ref
:function! IsReference(TypeStr)
	return (a:TypeStr =~ "&") && BoolNot(IsRValueRef(a:TypeStr))
:endfunction
:function! IsReferenceToConst(TypeStr)
	if(BoolNot(IsReference(a:TypeStr)))
		return
	endif
	let ref_index = strridx(a:TypeStr, "&")
	if ref_index < 0
		return 0
	endif
	let const_index = stridx(a:TypeStr, "const")
	if const_index < 0
		return 0
	endif
	return const_index < ref_index
:endfunction
:function! IsReferenceToMutable(TypeStr)
	return (IsReference(a:TypeStr)) && BoolNot(IsReferenceToConst(a:TypeStr))
:endfunction
"*** Type string of form "Type&&"
:function! GetTypeString_RValueRef(TypeName)
	return a:TypeName . "&&"
:endfunction
"*** Type string of form "const Type&"
:function! GetTypeString_RefToConst(TypeName)
	return "const " . a:TypeName . "&"
:endfunction
"*** Type string of form "Type&"
:function! GetTypeString_Ref(TypeName)
	return a:TypeName . "&"
:endfunction
"*** Argument string of form "Type&& Name"
:function! GetArgumentString_RValueRef(TypeName, ArgName)
	return GetTypeString_RValueRef(a:TypeName) . " " . a:ArgName
:endfunction
"*** Argument string of form "const Type& Name"
:function! GetArgumentString_RefToConst(TypeName, ArgName)
	return GetTypeString_RefToConst(a:TypeName) . " " . a:ArgName
:endfunction

"*** Content string for copy/move ctor/assignment operators
:function! GetFuncImplContentString_CopyMove(IsMove, ClassName, TemplParams, OptionString)
	let l:ArgName = "rhs"
	:lockvar l:ArgName
	:if a:IsMove
		"Move operation
		let l:s = GetArgumentString_RValueRef(a:ClassName, l:ArgName)
	:else
		"Copy operation
		let l:s = GetArgumentString_RefToConst(a:ClassName, l:ArgName)
	:endif
	return l:s
:endfunction
"*** Function implementation header
:function! GetLines_CppFuncImplHeader(Name, ClassName, TemplParams, ContentString, ReturnType, OptionString)
	let l:Lines = []
	let l:TemplHeader = GetTemplParamsString(a:TemplParams, 1)
	"Form Func Header
	:if len(a:ReturnType) > 0
		let l:FuncHeader = a:ReturnType . " "
	:else
		let l:FuncHeader = ""
	:endif
	let l:ImplContent = GetFuncImplContentString(a:ContentString)
	let l:FuncHeader =  l:FuncHeader.GetCppClassMemberPrefix(a:ClassName, a:TemplParams).GetLine_CppFunc_NameArgs_AndSpecs(a:Name, l:ImplContent, a:OptionString)
	"Form up result
	:if len(a:TemplParams) > 0
		:call add(l:Lines, l:TemplHeader)
	:endif
	:call add(l:Lines, l:FuncHeader)
	return l:Lines
:endfunction


"Returns list of lines of body lines (default or custom)
"{} NOT added!
:function! GetLines_CppFunc_InnerBodyLines(FuncGenArgs, ClassName, TemplParams)
	let l:IsDebug = 0

	let l:FuncArgs = GetFuncArgString(a:FuncGenArgs)
	let l:Name = GetFuncName(a:FuncGenArgs)
	let l:RetType = GetFuncRetType(a:FuncGenArgs)
	let l:Ops = GetFuncOps(a:FuncGenArgs)
	let l:ProvidedBodyLines = GetFuncBody(a:FuncGenArgs)

	if(l:Ops =~? ";NoDefBody;")
		let l:func_body = []
	elseif(l:ProvidedBodyLines == [])
		"Using default body
		let l:ReturnStmt = GetLines_DefaultReturnStmt(l:Name, a:ClassName, a:TemplParams, l:FuncArgs, l:RetType, l:Ops)
		let l:func_body = GetLines_CppFuncDefaultBody(l:ReturnStmt, l:FuncArgs, l:RetType, l:Ops)
	else
		"Body is provided, use it as-is
		let l:func_body = deepcopy(l:ProvidedBodyLines)
	endif

	if(l:IsDebug)
		echo "DEBUG: GetLines_CppFunc_InnerBody"
		echo "len(l:func_body)=".len(l:func_body)
		:call EchoBlock(0, l:func_body, "")
	endif

	return l:func_body
:endfunction

"*** Function definition
"*** Function's both declaration and definition
"*** WARNING! Lines are NOT idented
:function! GetLines_CppFunc(OutDefinition, FuncGenArgs, ClassName, TemplParams)
	let l:IsDebug = 0
	if (l:IsDebug)
		echo "DEBUG: GetLine_CppFunc"
	endif

	let l:FuncArgs = GetFuncArgString(a:FuncGenArgs)
	let l:Name = GetFuncName(a:FuncGenArgs)
	let l:RetType = GetFuncRetType(a:FuncGenArgs)
	let l:Ops = GetFuncOps(a:FuncGenArgs)

	let l:decl_lines = []

	let l:header_line = GetLine_CppFuncDecl_General(l:Name, l:FuncArgs, l:RetType, l:Ops)

	"Body calculation {
	let l:body_inner_lines = GetLines_CppFunc_InnerBodyLines(a:FuncGenArgs, a:ClassName, a:TemplParams)
	:call IdentBlock(l:body_inner_lines, 1)
	let l:func_body = []
	:call add(l:func_body, '{')
	:call extend(l:func_body, l:body_inner_lines)
	:call add(l:func_body, '}')
	" Body calculation }

	let l:ShouldInline = (l:Ops =~? ";Inline;") || (l:Ops =~? ";In;") || (len(a:TemplParams) > 0)
	:call DebugEcho("Debug: GetLines_CppFunc: ShouldInline: ".l:ShouldInline)
	let l:ShouldNotInline = BoolNot(l:ShouldInline)
	:let l:GenImpl = (l:Ops !~? ";NoImpl;") && (l:Ops !~? ";Def;")
	:lockvar l:GenImpl
	"Debug
	:call DebugEcho("Debug: GetLines_CppFunc: GenImpl: ".l:GenImpl)
	:if BoolNot(l:GenImpl) || l:ShouldNotInline
		"Non-inlining or should not gen impl,
		"then we should not provide the body inside declaration
		let l:header_line = l:header_line . ";"
		:call add(l:decl_lines, l:header_line)
		"Generate function definition in cpp file
		:if l:GenImpl
			let l:impl_header = GetLines_CppFuncImplHeader(l:Name, a:ClassName, a:TemplParams, l:FuncArgs, l:RetType, l:Ops)
			:call extend(a:OutDefinition, l:impl_header)
			:call extend(a:OutDefinition, l:func_body)
		:endif
	:else
		"Here we should provide body inside declaration
		"Debug
		:call DebugEcho("Debug: GetLine_CppFunc: here we should provide body inside decl")
		:if l:GenImpl
			let l:ImplContent = GetFuncImplContentString(l:FuncArgs)
			"function header line inside implementation 
			"(inside .cpp file or inlined)
			let l:header_line_impl = GetLine_CppFuncDecl_General(l:Name, l:ImplContent, l:RetType, l:Ops)
		:call DebugEcho("Debug: GetLine_CppFunc: header line impl: ".l:header_line_impl)
			:call add(l:decl_lines, l:header_line_impl)
			"Inlining body
			:call extend(l:decl_lines, l:func_body)
		:endif
	:endif
	return l:decl_lines
:endfunction
"*
"*** Class/Struct definition
"* Builds class or struct definition.
"*
"* Return value:
"*	List of lines of DECLARATION + inline DEFINITION 
"*	(all this lines to be inserted into .h file)
"*	OutDefinition: list of lines of definition (to be inserted into .cpp file)
"*
"* Arguments:
"*	Name              -      name of the class (WITHOUT Template arguments!)
"*	IsStruct          -      struct if 1 (otherwise class)
"*	TemplParams       -      Dictionary of Name-To-Type pairs, for example T-to-class
"*
"*
"* 	OptionString      -      string of options in the following form "Opt1;Opt2;...;OptN"
"*	ExtraPrivateLinesAbove - (list of lines) see class declaration example
"*
"* The following options are available:
"* 	I - Interface class (Virt is implicitly specified)
"*
"*	NoDtor - never generate destructor
"* 	Virt - Class with virtual/pure virtual functions:
"*		1. Virtual destructor is automatically declared
"*	
"*
"*	ImplicitCopyMove (ImplCpMov) - do NOT define copy or move operations
"*	NoDefCtor - do NOT provide default ctor
"*	NoCopy (NoCp) - do NOT provide default copy/move operations
"*	MoveOnly - provide ONLY move operations
"*	CustomCopy (CustCp) - custom assign/copy operations to be specified
"*
"*	Eq - specify if you want the class to include operators == and != 
"*	Compare - compare operations (<, > etc.) to be generated
"*
"* Lines of class or struct definition of the following form:
"*
"* template<class TArgA, class TArgB>
"* class Name
"* {
"*	EXTRA_PRIVATE_ABOVE // For example for "GENERATED_BODY() in UE4"
"*
"* public:
"*	virtual ~Name() = default; // If OptionString contains "Virt" or "I"
"*	Name() = default; // If OptionString does NOT contain "NoDefCtor";
"*
"*	Name(const Name&) = default; // If OptionString does NOT contain "NoCp";
"*	Name& operator=(const Name&) = default;	
"*
"*	Name(Name&&) = default;
"*	Name& operator=(Name&&) = default;
"*
"*	bool Compare(const Name& rhs); // If OptionString contains "Compare"	
"*
"* private:
"* }; 
"*
"* template<class TArgA, class TArgB>
"* bool operator==(const Name<TArgA, TArgB>& lhs, const Name<TArgA, TArgB>& rhs);
"*
"* template<class TArgA, class TArgB>
"* inline bool operator!=(const Name<TArgA, TArgB>& lhs, const Name<TArgA,TArgB>& rhs)
"* {
"* 	return ! operator=(lhs, rhs);
"* }
"*
"* template<class TArgA, class TArgB>
"* inline bool operator<(const Name<TArgA, TArgB>& lhs, const Name<TArgA,
"TArgB>& rhs)
"* {
"*	return lhs.Compare(rhs) < 0;
"* }
"*
"* template<class TArgA, class TArgB>
"* inline bool operator>(const Name& lhs, const Name& rhs)
"* {
"*	return lhs.Compare(rhs) > 0;
"* }
"*
"* template<class TArgA, class TArgB>
"* inline bool operator<=(const Name& lhs, const Name& rhs) 
"* {
"*	return lhs.Compare(rhs) <= 0;
"* }
"*
"* template<class TArgA, class TArgB>
"* inline bool operator>=(const Name& lhs, const Name& rhs)
"* {
"*	return lhs.Compare(rhs) >= 0;
"* }
"*
"* struct Name
"* {
"*	Name() 
"	{
"	}
"* };
"* Returns string of form "class/struct Name" 
:function! GetClassNameOnly(Name, IsStruct)
	:if a:IsStruct
		let l:line = "struct"
	:else
		let l:line = "class"
	:endif
	let l:line = l:line . " " . a:Name
	return l:line
:endfunction
"* Returns string of form "template<...> class/struct Name"
:function! GetLines_CppClassHeader_General(Name, TemplParams, IsStruct, OptionString)
	let l:lines = []
	:if a:TemplParams != {}
		:call add(l:lines, GetTemplParamsString(a:TemplParams, 1))
	:endif 
	:call add(l:lines, GetClassNameOnly(a:Name, a:IsStruct))
	return l:lines
:endfunction
"* Add destruction function (based on options)
:function! GetLines_CppDestructorFunc(OutDefinition, ClassName, TemplParams, OptionString)
	"Virtual
	let l:IsVirtualDtor = (a:OptionString =~? ";VirtDtor;") || (a:OptionString =~? ";I;")
	"Default/Delete
	let l:IsDef = (a:OptionString !~? ";Cust;") && (a:OptionString !~? ";CustDtor;") && (a:OptionString !~? ";Ptr;")
	let l:IsDeleted = 0 "TODO
	let l:ShouldInline = (a:OptionString =~? ";Inline;") || (a:OptionString =~? ";InlineDtor;")
	let l:NoImpl = (a:OptionString =~? ";NoImpl;") || (a:OptionString =~? ";NoImplDtor;")
	"Forming result
	let l:PublicDeclaration = []
		let l:FunctionName = '~' .  a:ClassName "As we have destructor, function name is the same as class + ~
		:lockvar l:FunctionName
		let l:FunctionReturnType = "" "As we have destructor, function return type is empty
		:lockvar l:FunctionReturnType
		let l:FuncOptions = ""
		let l:FuncArgs = ""
		:lockvar l:FuncArgs
		:if l:IsVirtualDtor
			let l:FuncOptions = l:FuncOptions . ";Virt;"
		:endif
		:if l:IsDef
			let l:FuncOptions = l:FuncOptions . ";Def;"
		:endif
		:if l:IsDeleted
			let l:FuncOptions = l:FuncOptions . ";Delete;"
		:endif
		:if l:ShouldInline
			let l:FuncOptions = l:FuncOptions . ";Inline;"
		:endif
		:if l:NoImpl
			let l:FuncOptions = l:FuncOptions . ";NoImpl;"
		:endif
		let l:CommentTextLines = ""
		let l:Category = ""
		let FuncGenArgs = MakeFuncGenArgs(l:FunctionName, l:FuncArgs, l:FunctionReturnType, l:FuncOptions, [], l:Category, l:CommentTextLines)
		let l:decl = GetLines_CppFunc(a:OutDefinition, FuncGenArgs, a:ClassName, a:TemplParams)
		"TODO: Why we extend public always? How about private part?
		:call extend(l:PublicDeclaration, l:decl)
	return l:PublicDeclaration
:endfunction
"* Declare destructor if necessary (based on options)
"* Returns: 
"*	public declaration part
"*	OutDefinition             -     definition (into .cpp file)
"*	OutPrivateDeclaration     -     private declaration part
:function! GetLines_Destructor_General(OutDefinition, OutPrivateDeclaration, IsStruct, ClassName, TemplParams, OptionString)
	:if a:IsStruct 
		let l:ShouldGenerate = (a:OptionString =~ "ForceDtor;")
	:else
		" For classes, and if no ForceDtor:
		let l:ShouldGenerate = (a:OptionString !~ "NoDtor;") || (a:OptionString =~ "ForceDtor;")
	:endif
	:if ! l:ShouldGenerate
		return []
	:endif 
 	let l:decl_lines = GetLines_CppDestructorFunc(a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString)
	return l:decl_lines
:endfunction
"* Add default ctor function (based on options)
:function! GetLines_CppDefaultCtorFunc(OutDefinition, ClassName, TemplParams, OptionString)
	"Default/Delete
	let l:IsDef = (a:OptionString !~ "Cust;") && (a:OptionString !~ "CustDefCtor;") && (a:OptionString !~ "Ptr;")
	let l:IsDeleted = (a:OptionString =~ "DeleteDefCtor;")
	let l:ShouldInline = (a:OptionString =~ "Inline;") || (a:OptionString =~ "InlineDefCtor;")
	let l:NoImpl = (a:OptionString =~ "NoImpl;") || (a:OptionString =~ "NoImplDefCtor;")
	"Forming result
	let l:PublicDeclaration = []
		let l:FunctionName = a:ClassName "As we have ctor, function name is the same as class
		:lockvar l:FunctionName
		let l:FunctionReturnType = "" "As we have ctor, function return type is empty
		:lockvar l:FunctionReturnType
		let l:FuncOptions = ""
		let l:FunctionArgs = ""
		:lockvar l:FunctionArgs
		:if l:IsDef
			let l:FuncOptions = l:FuncOptions . "Def;"
		:endif
		:if l:IsDeleted
			let l:FuncOptions = l:FuncOptions . "Delete;"
		:endif
		:if l:ShouldInline
			let l:FuncOptions = l:FuncOptions . "Inline;"
		:endif
		:if l:NoImpl
			let l:FuncOptions = l:FuncOptions . "NoImpl;"
		:endif
		let l:CommentTextLines = ""
		let l:Category = ""
		let FuncGenArgs = MakeFuncGenArgs(l:FunctionName, l:FunctionArgs, l:FunctionReturnType, l:FuncOptions, [], l:Category, l:CommentTextLines)
		let l:decl = GetLines_CppFunc(a:OutDefinition, FuncGenArgs, a:ClassName, a:TemplParams)
		"TODO: Why we extend public always? How about private part?
		:call extend(l:PublicDeclaration, l:decl)
	return l:PublicDeclaration
:endfunction
"* Declare default ctor(s) if necessary (based on options)
"* Returns: 
"*	public declaration part
"*	OutDefinition             -     definition (into .cpp file)
"*	OutPrivateDeclaration     -     private declaration part
:function! GetLines_DefaultCtor_General(OutDefinition, OutPrivateDeclaration, IsStruct, ClassName, TemplParams, OptionString)
	let l:ShouldGenerate = (a:OptionString !~ "NoDefCtor;")
	:if ! l:ShouldGenerate
		return []
	:endif 
 	let l:decl_lines = GetLines_CppDefaultCtorFunc(a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString)
	return l:decl_lines
:endfunction
"Operation for copying or moving (constructor or assignment)
:function! GetLines_CopyMove_CppFunc(IsMove, IsAssignment, OutDefinition, ClassName, TemplParams, OptionString)
	"Default/Delete
	let l:IsDefBoth = (a:OptionString !~ "Cust;") && (a:OptionString !~ "CustCpMov;") && (a:OptionString !~ "Ptr;")
	:if a:IsMove
		"Move"
		let l:IsDef = l:IsDefBoth || (a:OptionString =~ "CustMov;")
	:else
		"Copy"
		let l:IsDef = l:IsDefBoth || (a:OptionString =~ "CustCp;")
	:endif
	let l:ForbidBoth = (a:OptionString =~ "ForbidCpMov;")
	:if a:IsMove
		"Move"
		let l:IsDeleted = l:ForbidBoth || (a:OptionString =~ "ForbidMov;")
	:else
		"Copy"
		let l:IsDeleted = l:ForbidBoth || (a:OptionString =~ "ForbidCp;")
	:endif
	let l:ShouldInlineBoth = (a:OptionString =~ "Inline;") || (a:OptionString =~ "InlineCpMov;")
	:if a:IsMove
		"Move"
		let l:ShouldInline = l:ShouldInlineBoth || (a:OptionString =~ "InlineMov;")
	:else
		"Copy"
		let l:ShouldInline = l:ShouldInlineBoth || (a:OptionString =~ "InlineCp;")
	:endif
	let l:NoImplBoth = (a:OptionString =~ "NoImpl;") || (a:OptionString =~ "NoImplCpMov;")
	:if a:IsMove
		"Move"
		let l:NoImpl = l:NoImplBoth || (a:OptionString =~ "NoImplMov;")
	:else
		"Copy"
		let l:NoImpl = l:NoImplBoth || (a:OptionString =~ "NoImplCp;")
	:endif
	"Forming result
	let l:PublicDeclaration = []
		:if a:IsAssignment
			"Assignment"
			let l:FunctionName = "operator=" "As we have operator=
			let l:FunctionReturnType = GetTypeString_Ref(a:ClassName) "As we have operator=
		:else
			"Ctor"
			let l:FunctionName = a:ClassName "As we have ctor, function name is the same as class
			let l:FunctionReturnType = "" "As we have ctor, function return type is empty
		:endif
		:lockvar l:FunctionName
		:lockvar l:FunctionReturnType

		let l:ArgString = GetFuncImplContentString_CopyMove(a:IsMove, a:ClassName, a:TemplParams, a:OptionString)
		:lockvar l:ArgString

		"Forming up func options"
		let l:FuncOptions = ""
		:if l:IsDef
			let l:FuncOptions = l:FuncOptions . "Def;"
		:endif
		:if l:IsDeleted
			let l:FuncOptions = l:FuncOptions . "Delete;"
		:endif
		:if l:ShouldInline
			let l:FuncOptions = l:FuncOptions . "Inline;"
		:endif
		:if l:NoImpl
			let l:FuncOptions = l:FuncOptions . "NoImpl;"
		:endif

		let l:Category = ""
		let l:CommentTextLines = ""
		let FuncGenArgs = MakeFuncGenArgs(l:FunctionName, l:ArgString, l:FunctionReturnType, l:FuncOptions, [], l:Category, l:CommentTextLines)
		let l:decl = GetLines_CppFunc(a:OutDefinition, FuncGenArgs, a:ClassName, a:TemplParams)
		"TODO: Why we extend public always? How about private part?
		:call extend(l:PublicDeclaration, l:decl)
	return l:PublicDeclaration
:endfunction

"Adds both ctor and assignment
:function! GetLines_CopyMove_CtorAndAssign(IsMove, OutDefinition, ClassName, TemplParams, OptionString)
	let l:public_lines = []
	"Ctor
	:call extend(l:public_lines, GetLines_CopyMove_CppFunc(a:IsMove, 0, a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString))
	"Assignment
	:call extend(l:public_lines, GetLines_CopyMove_CppFunc(a:IsMove, 1, a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString))
	return l:public_lines
:endfunction
"* Add copy operation function (based on options)
:function! GetLines_CopyFuncs(OutDefinition, ClassName, TemplParams, OptionString)
	"TODO
 	return GetLines_CopyMove_CtorAndAssign(0, a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString)
:endfunction
"* Add move operations function (based on options)
:function! GetLines_MoveFuncs(OutDefinition, ClassName, TemplParams, OptionString)
	"TODO
 	return GetLines_CopyMove_CtorAndAssign(1, a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString)
:endfunction
"* Declare copy/move operations if necessary (based on options)
"* Returns: 
"*	public declaration part
"*	OutDefinition             -     definition (into .cpp file)
"*	OutPrivateDeclaration     -     private declaration part
:function! GetLines_CopyMoveOperations_General(OutDefinition, OutPrivateDeclaration, IsStruct, ClassName, TemplParams, OptionString)
	let l:ForceCpMov = (a:OptionString =~ "ForceCpMov;")
	let l:ForceCpOnly = (a:OptionString =~ "ForceCp;")
	let l:ForceMovOnly = (a:OptionString =~ "ForceMov;")
	let l:ForceAnyOf = l:ForceCpMov || l:ForceCpOnly || l:ForceMovOnly 

	:if a:IsStruct 
		"Should we generate any at all?
		let l:ShouldGenerate = l:ForceAnyOf
	:else
		" For classes
		let l:ShouldGenerate = (a:OptionString !~ "NoCpMov;") || l:ForceAnyOf
	:endif
	:if ! l:ShouldGenerate
		return []
	:endif 

	"Result generation:
	let l:decl_lines = []

	"Generate Copy
	:if BoolNot(a:IsStruct) || l:ForceCpMov || l:ForceCpOnly
		:call extend(l:decl_lines, GetLines_CopyFuncs(a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString))
	:endif 
	
	"Generate Move
	:if BoolNot(a:IsStruct) || l:ForceCpMov || l:ForceMovOnly
		:call extend(l:decl_lines, GetLines_MoveFuncs(a:OutDefinition, a:ClassName, a:TemplParams, a:OptionString))
	:endif
	
	return l:decl_lines
:endfunction
"* See help above
:function! GetLines_CppClass_General(OutDefinition, Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove, ExtraLinesAbove)
	"Destructor
 	let l:destructor_definition = []
	let l:destructor_priv = []
	let l:destructor_public = GetLines_Destructor_General(l:destructor_definition, l:destructor_priv, a:IsStruct, a:Name, a:TemplParams, a:OptionString)
	"DefaultCtor/ArgumentCtor
 	let l:default_ctor_definition = []
	let l:default_ctor_priv = []
	let l:default_ctor_public = GetLines_DefaultCtor_General(l:default_ctor_definition, l:default_ctor_priv, a:IsStruct, a:Name, a:TemplParams, a:OptionString)
	"Copy/Move operations
 	let l:copy_move_definition = []
	let l:copy_move_priv = []
	let l:copy_move_public = GetLines_CopyMoveOperations_General(l:copy_move_definition, l:copy_move_priv, a:IsStruct, a:Name, a:TemplParams, a:OptionString)
	"Definition (.cpp)
	:call Extend_WithBlank(a:OutDefinition, l:destructor_definition) 
	:call Extend_WithBlank(a:OutDefinition, l:default_ctor_definition) 
	:call Extend_WithBlank(a:OutDefinition, l:copy_move_definition) 
	"Declaration result
	let l:lines = []
	"Header
	:call extend(l:lines, a:ExtraLinesAbove)
	:call extend(l:lines, GetLines_CppClassHeader_General(a:Name, a:TemplParams, a:IsStruct, a:OptionString))
	:call add(l:lines, "{")
	let l:ExtraPrivateLinesAbove_Idented = deepcopy(a:ExtraPrivateLinesAbove)
	:call IdentBlock(l:ExtraPrivateLinesAbove_Idented, 1)
	:call extend(l:lines, l:ExtraPrivateLinesAbove_Idented)
	"Public section
	:if BoolNot(a:IsStruct)
		:call add(l:lines, "public:")
	:endif
	:let l:public_lines = []
	:call Extend_WithBlank(l:public_lines, l:destructor_public)
	:call Extend_WithBlank(l:public_lines, l:default_ctor_public)
	:call Extend_WithBlank(l:public_lines, l:copy_move_public)
	:call IdentBlock(l:public_lines, 1)
	:call extend(l:lines, l:public_lines)
"TODO: append comparison/equality operators
	:call add(l:lines, "")
	"Protected section
	:let l:protected_lines = []
	"Should we include protected section
	:if (len(l:protected_lines) > 0) || (a:OptionString =~ "ForceProtected;")
		"We always should include protected section if at least one
		"protected line is generated
		let l:IncludeProtected = 1;
	:else
		"For classes we include protected section by default,
		"for structs - not included by default
		let l:IncludeProtected = BoolNot(a:IsStruct)
	:endif
	:if l:IncludeProtected
		:call add(l:lines, "protected:")
		:call IdentBlock(l:protected_lines, 1)
		:call extend(l:lines, l:protected_lines)
	:endif
	"Private section
	:let l:private_lines = []
	:call Extend_WithBlank(l:private_lines, l:destructor_priv)
	:call Extend_WithBlank(l:private_lines, l:default_ctor_priv)
	:call Extend_WithBlank(l:private_lines, l:copy_move_priv)
	"Should we include private section
	:if (len(l:private_lines) > 0) || (a:OptionString =~ "ForcePriv;")
		"We always should include private section if at least one
		"private line is generated
		let l:IncludePrivate = 1;
	:else
		"For classes we include private section by default,
		"for structs - not included by default
		let l:IncludePrivate = BoolNot(a:IsStruct)
	:endif
	:if l:IncludePrivate
		:call add(l:lines, "private:")
		:call IdentBlock(l:private_lines, 1)
		:call extend(l:lines, l:private_lines)
	:endif
	:call add(l:lines, "};")
	return l:lines
:endfunction
"General function for adding code of class
:function! AddCode_CppClass_General(Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove, ExtraLinesAbove)
	let IsDebug = 0
	"Calculate lines"
	let l:PrivLines = []
	let l:PublicLines = GetLines_CppClass_General(l:PrivLines, a:Name, a:TemplParams, a:IsStruct, a:OptionString, a:ExtraPrivateLinesAbove, a:ExtraLinesAbove)
	"Add code"
	let l:Context = ContextOrCurr({}, a:OptionString)

	let l:Ops = a:OptionString.";PrepLineAfter;"
	:call AddCodeAt(l:Context, l:PublicLines, l:PrivLines, l:Ops)

	"Cursor jumping
	let NewClassContext = GetContextAt(GetContextLine(l:Context), l:Ops)
	if IsDebug
		echo 'DEBUG: AddCode_CppClass_General'
		:call EchoContext(0, 'NewClassContext', NewClassContext, '')
	endif
	"Jump to end of class
	":call cursor(GetContextEndLine(NewClassContext), 1)
:endfunction
" Adds code of cpp struct with default options
:function! AddCode_CppClass_GeneralDefault(IsStruct, Name, TemplParams, OptionString, ExtraPrivateLinesAbove, ExtraLinesAbove)
	let l:Opts = a:OptionString
	if a:IsStruct
		let l:Opts = l:Opts . "Inline;"
		let l:Opts = l:Opts . "CustDefCtor;"
	endif
	:call AddCode_CppClass_General(a:Name, a:TemplParams, a:IsStruct, l:Opts, a:ExtraPrivateLinesAbove, a:ExtraLinesAbove)
:endfunction

"Argument indices
let g:AddCode_CppClass_ClassNameArgIndex = 2

:function! CmdFunc_AddCode_CppClass_Default(...)
	let args = a:000
	lockvar args
	let n = len(a:000) - 2
	lockvar n

	:call DebugEcho("Command with ".n." args called")

	if len(a:000) >= 2
		let IsStruct = a:000[0]
		lockvar IsStruct

		let ExtraLinesAbove = a:000[1]
		lockvar ExtraLinesAbove
	endif

	if n > 2 
		echoerr "Too many arguments for the command"
		return
	elseif n == 0
		echoerr "At least one argument (struct name) must be specified for the command"
		return
	else
		"here we have 1 or 2 arguments
		let StructName = args[g:AddCode_CppClass_ClassNameArgIndex]
		lockvar StructName
		if n >= 2
			let Ops = args[3]
		else
			let Ops = ""
		endif
		lockvar Ops

		"Performing struct/class insertion command
		:call AddCode_CppClass_Default(IsStruct, StructName, Ops, ExtraLinesAbove)
	endif
:endfunction

:function! TemplParams_ListToDict(InList)
	let res_dict = {}
	for ParamName in a:InList
		let res_dict[ParamName] = "class"
	endfor
	return res_dict
:endfunction

"Does NOT check that the given param exists!
"Returns template parameters argument DICTIONARY
:function! GetTemplParamsArgument(ArgList, ArgIndex)
		let TemplParams = eval(a:ArgList[a:ArgIndex])

		:if type(TemplParams) ==  v:t_dict
			let TemplParamsDictionary = TemplParams
		:elseif type(TemplParams) ==  v:t_list
			let TemplParamsList = TemplParams
			let TemplParamsDictionary = TemplParams_ListToDict(TemplParamsList)
		:else
			echoerr "Wrong type of template params list (must be either list or dictionary (name to type mapping))"
			return {}
		:endif
		return TemplParamsDictionary
:endfunction

:function! CmdFunc_AddCode_CppClass_TemplDefault(...)
	let args = a:000
	lockvar args
	let n = len(a:000) - 2
	lockvar n

	echo "Command with ".n." args called"

	if len(a:000) >= 2
		let IsStruct = a:000[0]
		lockvar IsStruct

		let ExtraLinesAbove = a:000[1]
		lockvar ExtraLinesAbove
	endif

	if n > 3 
		echoerr "Too many arguments for the command"
		return
	elseif n < 2
		echoerr "At least two arguments (struct name and template params list or dictinary (name to type (e.g. class))) must be specified for the command"
		return
	else
		"here we have 2 or 3 arguments
		let StructName = args[g:AddCode_CppClass_ClassNameArgIndex]
		lockvar StructName
		let TemplParams = GetTemplParamsArgument(args, 3)

		if n >= 3
			let Ops = args[4]
		else
			let Ops = ""
		endif
		lockvar Ops

		"Performing struct insertion command
		:call AddCode_CppClass_TemplDefault(IsStruct, StructName, TemplParamsDictionary, Ops, ExtraLinesAbove)
	endif
:endfunction

:function! AddCode_CppIncludeAt_General(LineNumber, Filename, IsSystem)
	let l = GetLine_CppIncludeAt_General(a:Filename, a:IsSystem)
	:call append(a:LineNumber, l)
:endfunction 
:function! CmdFunc_Inc(...)
	let args = a:000
	let n = len(args)

	if n < 1
		echoerr "Filename to include must be specified"
	else
		if n >= 2
			let IsSystem = args[1]
		else
			let IsSystem = 0
		endif

		"Here ok, at least one argument specified
		let Name = args[0]

		"Find pragma line index (we should include below it always!)
		let pragma_line = search('^\s*#pragma')
		let buffer_line_count = line('$')

		"We must append blank line if pragme line is the last,
		"otherwise append will fail
		if pragma_line >= buffer_line_count
			:call append(pragma_line, "")
		endif

		let insert_line = pragma_line + 1

		:call AddCode_CppIncludeAt_General(insert_line, Name, IsSystem)
	endif
:endfunction

"Argument indices
let g:AddCode_CppVarOrField_IsField_ArgIndex = 0
let g:AddCode_CppVarOrField_LinesAbove_ArgIndex = 1
let g:AddCode_CppVarOrField_OptionString_ArgIndex = 2
let g:AddCode_CppVarOrField_TypeName_ArgIndex = 3
let g:AddCode_CppVarOrField_Name_ArgIndex = 4
let g:AddCode_CppVarOrField_InitExpr_ArgIndex = 5

:function! GetDefaultVarInitExpr(OptionString, TypeName, InitExpr)
	let l:res = ""
	"TODO: default for init, refs, ptrs etc.
	return l:res
:endfunction

"Returns real initialization expr:
"- If empty line is used, uses default initializer for the given type 
"(according to options, of course)
:function! GetRealVarInitExpr(OptionString, TypeName, InitExpr)
	let l:res = a:InitExpr
	if (a:OptionString !~# "NoInit;")
		if a:InitExpr == ""
			let l:res = GetDefaultVarInitExpr(a:OptionString, a:TypeName, a:InitExpr)
		endif
	endif
	return l:res 
:endfunction

"Returns part of variable declaration part, to be located
"RIGHT after the class name part (ClassName::Var = )
"WARNING! Does NOT include ';'
:function! GetCppVarDecl_Right(OptionString, TypeName, Name, InitExpr)
	let l:res = a:Name
	let l:RealInitExpr = GetRealVarInitExpr(a:OptionString, a:TypeName, a:InitExpr)
	if len(l:RealInitExpr) > 0
		let l:res .= " = "
		let l:res .= l:RealInitExpr
	endif
	return l:res
:endfunction

"Returns declaration of cpp variable WITHOUT ;
"(for any reason - field, argument
"variable, local function variable etc.) 
:function! GetCppVarDecl(OptionString, TypeName, Name, InitExpr)
	let l:res = a:TypeName
	if(len(l:res) > 0)
		let l:res .= " "
	endif
	let l:res .= GetCppVarDecl_Right(a:OptionString, a:TypeName, a:Name, a:InitExpr)
	return l:res
:endfunction

" Returns the main line of the declaration (WARNING! Does NOT include ";")
:function! GetCppVar_MainLine(IsField, OptionString, TypeName, Name, InitExpr)
	return GetCppVarDecl(a:OptionString, a:TypeName, a:Name, a:InitExpr)
:endfunction

:function! GetLines_CppVarOrField(OutCppLines, IsField, LinesAbove, OptionString, TypeName, Name, InitExpr)
	let l:lines = []
	"add comment
	let l:CommentLines = GetLines_GetVarOrFieldComment_IfShould(a:IsField, a:TypeName, a:Name, a:InitExpr, a:OptionString)
	:call extend(l:lines, l:CommentLines)
	"Adding lines above
	:call extend(l:lines, a:LinesAbove)
	"Add the main line
	:call add(l:lines, GetCppVar_MainLine(a:IsField, a:OptionString, a:TypeName, a:Name, a:InitExpr).";")
	return l:lines
:endfunction

"Adds properly indented cpp variable of field text at current line
"(warning! Adds ONLY variable text, NO getters or setters etc.)
"Arguments: see the corresponding CmdFunc
:function! AddCode_CppVarOrField(IsField, Comment, Category, LinesAbove, OptionString, TypeName, Name, InitExpr)
	"Reserved for impl, for example initializers for static variables"
	let l:variable_cpp_lines = []
	"Variable declaration itself
	let l:variable_lines = GetLines_CppVarOrField(l:variable_cpp_lines, a:IsField, a:LinesAbove, a:OptionString, a:TypeName, a:Name, a:InitExpr)

	"Add code
	let l:NewOps = a:OptionString.";PrepLineAfter;"
	let Context = GetContextAt(line('.'), a:OptionString)
	let l:InsertionStartLine = AddCodeAt(Context, l:variable_lines, l:variable_cpp_lines, l:NewOps)

	"TODO: Add getter
:endfunction

:function! IsFieldContextType(ContextType)
	if(a:ContextType == g:ContextType_Class)
		return 1
	endif 	
	return 0
:endfunction

:function! IsVariableContextType(ContextType)
	if(a:ContextType == g:ContextType_Enum)
		return 0
	endif 	
	return 1
:endfunction

:function! InvalidVarArgs(ArgsDict)
	"TODO: Check that return type is explicitly set
	return InvalidFunctionArgs(a:ArgsDict)
:endfunction

:function! GetInitializer_FromArgs(MainAndInitializer)
	let l:InitializerLines = GetFuncBodyLines_FromMainAndBodyList(a:MainAndInitializer)
	if(len(l:InitializerLines) > 0)
		return l:InitializerLines[0]
	else
		return ''
	endif
:endfunction

	let g:CppVariableCmd_Name_ArgIndex = 0
	let g:CppVariableCmd_RestString_ArgIndex = 1
:function! GetUpdatedVariableArgs_FromRestOfArgs(Context, BaseArgs, MyArgs, RestArgsString, Options) 

	"Checking custom args
	if NoArg(1, a:MyArgs, "Name", g:CppVariableCmd_Name_ArgIndex)
		return []
	endif
	let IsDebug = 0
	let l:Ops = a:Options
	let l:ContextType = GetContextType(a:Context)

	let MainAndInitializer = SplitFuncArgs_MainAndResult(a:RestArgsString)
	let l:RestArgs = GetOrDefault(MainAndInitializer, 0, "")
	let l:InitializerStr = GetInitializer_FromArgs(MainAndInitializer)
	""Rest of args dictionary:
	""-Initializer (@)
	""-Extra ops (;)
	""-Categry (!)
	let l:RestArgs_Dict = ExtractDefaultArguments(l:RestArgs, [])

	"Check for return value and arguments validity
	if (InvalidVarArgs(l:RestArgs_Dict))
		return 0
	endif

	let l:TypeName = GetReturnType_FromExtractedDict(l:RestArgs_Dict)
	let l:Comment = GetComment_FromExtractedDict(l:RestArgs_Dict)
	let l:ExtraOps = GetJoinedOps_FromExtractedDict(l:RestArgs_Dict)
	let l:Category = GetJoinedCategories_FromExtractedDict(l:RestArgs_Dict)

	"Update the global ops with extra ops:
	let l:Ops .= ';'.l:ExtraOps.';'

	let l:FixedName = a:MyArgs[g:CppVariableCmd_Name_ArgIndex]
	lockvar l:FixedName

	let VariableGenArgList = MakeVariableGenArgs(l:FixedName, l:TypeName, l:Ops, l:InitializerStr, l:Category, l:Comment)
	return VariableGenArgList
:endfunction

"Add C++ variable
:function! CmdFunc_AddCode_CppVar(...)
	let IsDebug = 0

	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]
	let l:ContextType = GetContextType(l:Context)

	let RestOfArgs = ListRestAsString(l:MyArgs, g:CppVariableCmd_RestString_ArgIndex)
	let l:UpdatedArgs = GetUpdatedVariableArgs_FromRestOfArgs(l:Context, l:BaseArgs, l:MyArgs, RestOfArgs, l:Ops) 
	if(l:UpdatedArgs == [])
		return 0
	endif

	let l:IsField = GetKey_IntType(l:BaseArgs, 'IsField')
	let l:LinesAbove = GetKey_ListType(l:BaseArgs, "ExtraPrivateLinesAbove")

	if(IsDebug)
		echo 'DEBUG: AddClass'
		echo 'Ops: '.l:Ops
	endif
	"DEBUG {
	"echo "CmdFunc_AddCode_CppClass: DEBUG: ExtraPrivateLinesAbove: "
	":call EchoBlock(0, l:ExtraPrivateLinesAbove, "")
	"echo "CmdFunc_AddCode_CppClass: DEBUG: ClassLinesAbove: "
	":call EchoBlock(0, l:ExtraLinesAbove, "")
	"DEBUG }
	
	if(l:IsField && BoolNot(IsFieldContextType(l:ContextType)))
		let l:IsValidContext = 0
	elseif(BoolNot(l:IsField) && BoolNot(IsVariableContextType(l:ContextType)))
		let l:IsValidContext = 0
	else
		let l:IsValidContext = 1
	endif
	
	if l:IsValidContext
		"TODO Pass updated arguments instead!
		":call AddCode_CppVarOrField(l:IsField, l:Comment, l:Category, l:LinesAbove, l:Ops, l:TypeName, l:FixedName, l:InitializerStr)
	else
		"Unsupported context type here
		:call EchoContext(1, "Unsupported context for command", l:Context, "")
		return 0
	endif
	return 1
:endfunction

:function! GetLines_EnumClass(BaseArgs, Ops, Context, LinesAbove, LinesBelow, Name)
	let l:lines = []
	"Add comment
	let l:comment_lines = GetLines_EnumClassComment_IfShould(a:Name, a:Ops)
	:call extend(l:lines, l:comment_lines)
	"Add lines above
	:call extend(l:lines, a:LinesAbove)
	"Add main code
	let l:header_line = "enum class ".a:Name 
	:if a:Ops =~ "uint8;"
		let l:header_line .= " : uint8"
	:endif
	:call add(l:lines, l:header_line)
	:call add(l:lines, "{")
	:call add(l:lines, "};")
	"Add lines below
	:call extend(l:lines, a:LinesBelow)
	return l:lines
:endfunction

:function! GetEnumLiteralLine(BaseArgs, Ops, LineAfter, Name, ValueStr)
	let l:res = a:Name
	if(len(a:ValueStr) > 0)
		let l:res .= " = "
		let l:res .= a:ValueStr
	endif
	if(len(a:LineAfter) > 0)
		let l:res .= ' '.a:LineAfter
	endif
	return l:res
:endfunction

:function! GetLines_EnumLiteral(ShouldAddComma, BaseArgs, Ops, Context, LineAfter, Name, ValueStr)
	let l:lines = []
	"Add Comment
	let l:comment_lines = GetLines_EnumLiteralComment_IfShould(a:Name, a:Ops)
	:call extend(l:lines, l:comment_lines)
	"Add literal line
	let l:literal = ""
	:if a:ShouldAddComma
		let l:literal .= ', '
	:endif
	let l:literal .= GetEnumLiteralLine(a:BaseArgs, a:Ops, a:LineAfter, a:Name, a:ValueStr)
	:call add(l:lines, l:literal)
	return l:lines
:endfunction

:function! AddCode_EnumClass(BaseArgs, Ops, Context, LinesAbove, LinesBelow, Name)
	let l:FixedName = GetNameWithFixedPrefix(a:Name, "E")
	let l:lines = GetLines_EnumClass( a:BaseArgs, a:Ops, a:Context, a:LinesAbove, a:LinesBelow, l:FixedName)
	let l:NewOps = a:Ops.";PrepLineAfter;"

	let AddCodeResult = AddCodeAt(a:Context, l:lines, [], l:NewOps)
	"Hack: change cursor position
	":call cursor(LineIndex_LocToBuf(a:Context, 0), 1)

	return AddCodeResult
:endfunction

:function! AddCode_EnumLiteral(BaseArgs, Ops, Context, LineAfter, Name, ValueStr)
	"Do we have an empty line before the literal?
	let l:EmptyLineBefore = IsEmptyOrSpaced(getline(GetContextLine(a:Context)))

	"*** Is there at least one enum literal before the position we currently
	"insert text
	""TODO: Currently we just calculuate total number of literals,
	"not number of literals before the position
	let l:AfterLiteral = (GetContextNumEnumLiterals(a:Context) > 0)

	"Should we add an empty line before the literal?
	let l:PrependLineBefore = l:AfterLiteral && BoolNot(l:EmptyLineBefore)
	let l:PrependLineAfter = 0 "TODO: When we in the middle of enum, and no line after

	let l:ShouldAddComma = l:AfterLiteral
	let l:lines = []

	"Empty line before the literal
	if(l:PrependLineBefore)
		:call add(l:lines, "")
	endif
	
	"Main code
	:call extend(l:lines, GetLines_EnumLiteral(l:ShouldAddComma, a:BaseArgs, a:Ops, a:Context, a:LineAfter, a:Name, a:ValueStr))

	"Empty line after the literal
	if(l:PrependLineAfter)
		:call add(l:lines, "")
	endif

	" DEBUG {
		":call EchoContext(0, "Debug context", a:Context, "")
		"echo "AddCode_EnumLiteral: Debug: GetContextIndentationParam(a:Context): ".GetContextIndentationParam(a:Context)
		"echo "AddCode_EnumLiteral: Debug: IndentLevel: ".l:indent_level
	" DEBUG }

	let ResultCodeAt = AddCodeAt(a:Context, l:lines, [], a:Ops)

	"Hack: change cursor position
	:call JumpAfterAt(GetContextLine(a:Context), l:lines)

	return ResultCodeAt
:endfunction

:function! IsEnumLiteralContextType(ContextType)
	return (a:ContextType == g:ContextType_Enum)
:endfunction

:function! IsEnumClassContextType(ContextType)
	return (a:ContextType == g:ContextType_Global) || (a:ContextType == g:ContextType_Class) || (a:ContextType == g:ContextType_Unknown)
:endfunction

:function! IsClassContextType(ContextType)
	if(a:ContextType == g:ContextType_Enum)
		return 0
	endif
	return 1
:endfunction

"Context of ordinary function (NOT lambda)
:function! IsFunctionContextType(ContextType)
	if((a:ContextType == g:ContextType_Enum) || (a:ContextType == g:ContextType_Function))
		return 0
	endif
	return 1
:endfunction

"Arguments: see the corresponding command for arguments
:function! CmdFunc_AddCode_CppClass(...)
	let IsDebug = 0

	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]
	let l:ContextType = GetContextType(l:Context)

	let l:Name_ArgIndex = 0
	let l:RestString_ArgIndex = 1
	"Checking custom args
	if NoArg(1, l:MyArgs, "Name", l:Name_ArgIndex)
		return 0
	endif

	let RestOfArgs = ListRestAsString(l:MyArgs, l:RestString_ArgIndex)
	let l:RestArgs_Dict = ExtractDefaultArguments(RestOfArgs, [])

	let l:Comment = GetComment_FromExtractedDict(l:RestArgs_Dict)
	let l:ExtraOps = GetJoinedOps_FromExtractedDict(l:RestArgs_Dict)
	let l:Category = GetJoinedCategories_FromExtractedDict(l:RestArgs_Dict)

	"Update the global ops with extra ops:
	let l:Ops .= ';'.l:ExtraOps.';'

	if(IsDebug)
		echo 'DEBUG: AddClass'
		echo 'Ops: '.l:Ops
	endif

	"Is templated class"
	let l:IsTempl = GetKey_IntType(l:BaseArgs, "IsTempl")

	"Getting custom args
	lockvar l:TemplParamsArgIndex
	if(l:IsTempl)
		"Templ params arg index is ZERO
		let l:TemplParamsArgIndex = 0
		if NoArg(1, l:MyArgs, "TemplParams", l:TemplParamsArgIndex)
			return 0
		endif
		let l:NameArgIndex = l:TemplParamsArgIndex + 1
		let l:TemplParams = GetTemplParamsArgument(l:MyArgs, l:TemplParamsArgIndex)
		"Templ params name arg index is ONE (right after TemplParams)
	else
		"Templ params name arg index is ZERO
		let l:TemplParams = {}
		let l:NameArgIndex = 0
	endif
	let l:FixedName = l:MyArgs[l:NameArgIndex]
	let IsReallyTempl = l:TemplParams != {}
	if IsReallyTempl
		let l:FixedName = GetNameWithFixedPrefix(l:FixedName, "T")
	endif
	lockvar l:FixedName

	let l:IsStruct = GetKey_IntType(l:BaseArgs, "IsStruct")
	let l:ExtraPrivateLinesAbove = GetKey_ListType(l:BaseArgs, "ExtraPrivateLinesAbove")
	let l:ExtraLinesAbove = GetKey_ListType(l:BaseArgs, "ClassLinesAbove")

	"DEBUG {
	"echo "CmdFunc_AddCode_CppClass: DEBUG: ExtraPrivateLinesAbove: "
	":call EchoBlock(0, l:ExtraPrivateLinesAbove, "")
	"echo "CmdFunc_AddCode_CppClass: DEBUG: ClassLinesAbove: "
	":call EchoBlock(0, l:ExtraLinesAbove, "")
	"DEBUG }
	
	if IsClassContextType(l:ContextType)
		:call AddCode_CppClass_GeneralDefault(l:IsStruct, l:FixedName, l:TemplParams, l:Ops, l:ExtraPrivateLinesAbove, l:ExtraLinesAbove)
	else
		"Unsupported context type here
		:call EchoContext(1, "Unsupported context for command", l:Context, "")
		return 0
	endif
:endfunction

"Arguments: see the corresponding command for arguments
:function! CmdFunc_AddCode_EnumClassOrLiteral(...)
	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]

	":call EchoContext(0, "Debug context: AddCode_EnumClassOrLiteral", l:Context, "")

	"Checking custom args
	if NoArg(1, l:MyArgs, "Name", 0)
		return 0
	endif

	"Getting custom args
	let l:Name = l:MyArgs[0]
	let l:ValueStr = GetOrDefault(l:MyArgs, 1, "")
	"echo "DEBUG:EnumOrLiteral CMD: Name= ".l:Name."; ValueStr=".l:ValueStr
	
	let l:ContextType = GetContextType(l:Context)
	if IsEnumClassContextType(l:ContextType)
		return AddCode_EnumClass(l:BaseArgs, l:Ops, l:Context, GetKey_ListType(l:BaseArgs, "ClassLinesAbove"), GetKey_ListType(l:BaseArgs, "ClassLinesBelow"), l:Name)
	elseif IsEnumLiteralContextType(l:ContextType)
		let l:LiteralLineAfter = GetKey_StringType(l:BaseArgs, "LiteralLineAfter")
		return AddCode_EnumLiteral(l:BaseArgs, l:Ops, l:Context, l:LiteralLineAfter, l:Name, l:ValueStr)
	else
		"Unsupported context type here
		:call EchoContext(1, "Unsupported context for command", l:Context, "")
		return 0
	endif

	return 1
:endfunction

"CppFunction + advanced options (like, for example, ClassLinesAbove etc.)
"and parses context
:function! GetLines_CppFunction_Advanced(OutDefinition, Context, BaseArgs, FuncGenArgs)
	let l:IsDebug = 0

	let l:Name = GetFuncName(a:FuncGenArgs)
	let l:Comment = GetFuncCommentTextLines(a:FuncGenArgs)
	let l:Ops = GetFuncOps(a:FuncGenArgs)

	let l:ClassName = GetContextClassName(a:Context)
	let l:TemplParams = {} "TODO: Pass templ params


	"if (type(a:BaseArgs) != g:v_Dict)
	"	echoerr "GetLines_CppFunction_Advanced: BaseArgs must be dict"
	"endif

	if(l:IsDebug)
		echo "DEBUG: GetLines_CppFunction_Advanced"
		:call EchoBlock(0, a:FuncGenArgs, "")
		echo "type(Context): ".type(a:Context)
		echo "type(BaseArgs): ".type(a:BaseArgs)
		echo "type(FuncGenArgs): ".type(a:FuncGenArgs)
		echo "Ops: ".l:Ops
		echo "Name: ".l:Name
		echo "Comment: ".l:Comment
	endif

	let l:public_lines = []
	let l:priv_lines = []

	:call extend(l:public_lines, GetLines_FunctionComment_IfShould(l:Name, l:Comment, l:Ops))
	:call extend(l:public_lines, GetKey_ListType(a:BaseArgs, "LinesAbove"))
	:call extend(l:public_lines, GetLines_CppFunc(a:OutDefinition, a:FuncGenArgs, l:ClassName, l:TemplParams))

	return l:public_lines
:endfunction

:function! AddCode_CppFunction(Context, BaseArgs, FuncGenArgs)
	let l:IsDebug = 0

	let l:Ops = GetFuncOps(a:FuncGenArgs)


	if(l:IsDebug)
		echo "DEBUG: AddCode_CppFunction"
		echo "type(Context): ".type(a:Context)
		echo "type(BaseArgs): ".type(a:BaseArgs)
		echo "type(FuncGenArgs): ".type(a:FuncGenArgs)
		echo "Ops: ".l:Ops
	endif

	let l:public_lines = []
	let l:priv_lines = []

	let l:main_lines = GetLines_CppFunction_Advanced(l:priv_lines, a:Context, a:BaseArgs, a:FuncGenArgs)
	:call extend(l:public_lines, l:main_lines)

	let l:NewOps = l:Ops.';PrepLineAfter;'
	:call AddCodeAt(a:Context, l:public_lines, l:priv_lines, l:NewOps)
:endfunction

:function! IsCppAnyCtorName(ClassName, Name)
	return a:ClassName == a:Name
:endfunction

:function! IsCppDestructorName(ClassName, Name)
	return ('~'.a:ClassName) == a:Name
:endfunction

:function! IsCppInterfaceName(Name)
	return a:Name =~# ("^I.*")
:endfunction

:function! GetCppFunction_ReturnTypeOrDefault(Context, ReturnType, Name, Ops)
	let l:ContextType = GetContextType(a:Context)

	" Is this is a special function without return type?
	let l:NoReturnType = 0
	if l:ContextType == g:ContextType_Class
		let l:ClassName = GetContextClassName(a:Context)

		if(IsCppAnyCtorName(l:ClassName, a:Name) || IsCppDestructorName(l:ClassName, a:Name))
			let l:NoReturnType = 1
		endif
	endif

	if(l:NoReturnType || (a:ReturnType != ''))
		return a:ReturnType
	else
		return 'void'
	endif
:endfunction

"Searches for the context namespace for the given member name
:function! GetFixedName_MemberInContext(Context, MemberName, Ops)
	"TODO
	return GetFixedName(a:MemberName)
:endfunction

:function! GetUpdatedCppFunctionArgs_Getter(Context, MemberName, FuncArgs)
	let l:IsDebug = 0
	if l:IsDebug
		echo "GetUpdatedCppFunctionArgs_Getter"
		echo "FuncArgString: ".GetFuncArgString(a:FuncArgs)
		echo "ReturnType: ".GetFuncRetType(a:FuncArgs)
	endif

	let l:NewArgs = deepcopy(a:FuncArgs)

	if(a:MemberName == '')
		echoerr 'Getter member name must be non-empty'
		return []
	endif

	let l:FixedMemberName = GetFixedName_MemberInContext(a:Context, a:MemberName, GetFuncOps(l:NewArgs))
	let l:NewGetterName = 'Get'.l:FixedMemberName

	:call SetFuncName(l:NewArgs, l:NewGetterName)
	:call AddFuncOps(l:NewArgs, ';const;') "Getters are always constant
	:call AddFuncOps(l:NewArgs, ';inline;') "Getters are inline by default

	"Checking return type
	let l:OldRetType = GetFuncRetType(l:NewArgs)
	if(l:OldRetType == '' || l:OldRetType == 'void')
		"TODO: Fix return type 
		"(based on return type of the given var, if it exists)

		echoerr "Getter must have return type"
		return []
	endif

	
	"Fixing body: WARN: Must be done AFTER fixing return type!
	if(GetFuncBody(l:NewArgs) == [])
		let NewBody = GetLines_ReturnStmt(l:FixedMemberName)
		:call SetFuncBody(l:NewArgs, NewBody)	
	endif

	return l:NewArgs
:endfunction

"This function is TO BE overriden in the concrete module (u, for example)
"Called AFTER core arguments have been updated
"Returns: updated function args
:function! CmdFunc_Module_GetUpdatedCppFunctionArgs(Context, FuncArgs)
	let l:NewArgs = deepcopy(a:FuncArgs)
	return l:NewArgs
:endfunction


" Returns updated function args based on context
:function! CmdFunc_GetUpdatedCppFunctionArgs(Context, FuncArgs)
	let l:ContextType = GetContextType(a:Context)

	let l:NewArgs = deepcopy(a:FuncArgs)

	"Update options based on context
	let l:Name = GetFuncName(a:FuncArgs)
	if(l:Name =~# '^\.\w*')
		"Getter
		let l:MemberName = strpart(l:Name, 1)
		let l:NewArgs = GetUpdatedCppFunctionArgs_Getter(a:Context, l:MemberName, a:FuncArgs)
		if(l:NewArgs == [])
			return []
		endif

	elseif(l:Name =~# '^\.\.\w*')
		"Setter
		let l:MemberName = strpart(l:Name, 2)
	endif

	let l:NewArgs = CmdFunc_Module_GetUpdatedCppFunctionArgs(a:Context, l:NewArgs)

	return l:NewArgs
:endfunction

:function! CmdFunc_AddCode_CppFunction(...)
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

	":call EchoContext(0, "Debug context: CmdFunc_AddCode_CppFunction", l:Context, "")

	"Checking custom args
	let l:Name_ArgIndex = 0
	let l:RetValAndArgs_ArgIndex = 1
	if NoArg(1, l:MyArgs, "Name", l:Name_ArgIndex)
		return 0
	endif
	let l:Name = l:MyArgs[l:Name_ArgIndex]
	let RestOfString = ListRestAsString(l:MyArgs, l:RetValAndArgs_ArgIndex)
	let MainAndBody = SplitFuncArgs_MainAndResult(RestOfString)
	let l:RetValAndArgs = GetOrDefault(MainAndBody, 0, "")
	let l:BodyLines = GetFuncBodyLines_FromMainAndBodyList(MainAndBody)
	let l:RetValAndArgs_Dict = ExtractDefaultArguments(l:RetValAndArgs, [])


	"Check for return value and arguments validity
	if (InvalidFunctionArgs(l:RetValAndArgs_Dict))
		return 0
	endif

	let l:FunctionArgs = GetFunctionArguments_FromExtractedDict(l:RetValAndArgs_Dict)
	let l:Comment = GetComment_FromExtractedDict(l:RetValAndArgs_Dict)
	let l:ExtraOps = GetJoinedOps_FromExtractedDict(l:RetValAndArgs_Dict)
	let l:Category = GetJoinedCategories_FromExtractedDict(l:RetValAndArgs_Dict)

	"Update the global ops with extra ops:
	let l:Ops .= ';'.l:ExtraOps.';'


	"Calculate class name based on context
	if( l:ContextType == g:ContextType_Class )
		let l:ClassName = GetContextClassName(l:Context)
	else
		let l:ClassName = ''
	endif

	"Return type
	let l:RetType = GetCppFunction_ReturnTypeOrDefault(l:Context, GetReturnType_FromExtractedDict(l:RetValAndArgs_Dict), l:Name, l:Ops)

	"Update options based on context
	if( l:ContextType == g:ContextType_Class )
		let IsInterfaceClass = IsCppInterfaceName(l:ClassName)
		
		"For interface classes by default virtual functions are used
		if (IsInterfaceClass && (l:Ops !~#";NoVirt;" ))
			let l:Ops .= ';PureVirt;'
		endif
	endif

	"DEBUG {

		if l:ShowDebugLines
			let l:DebugLines = []
			:call add(l:DebugLines, "DEBUG CmdFunc_AddCode_CppFunction: ")
			:call add(l:DebugLines, "RetValAndArgs=".l:RetValAndArgs)
			:call add(l:DebugLines, "RetType=".l:RetType)
			:call add(l:DebugLines, "FunctionArgs=".l:FunctionArgs)
			:call add(l:DebugLines, "Comment=".l:Comment)
			:call EchoBlock(0, l:DebugLines, "")

			echo "BodyLines"
			:call EchoBlock(0, l:BodyLines, "")

			:call EchoDictOfList(l:RetValAndArgs_Dict)
		endif

	"DEBUG }

	"All arguments of function generation
	let l:FuncGenArgs = MakeFuncGenArgs(l:Name, l:FunctionArgs, l:RetType, l:Ops, l:BodyLines, l:Category, l:Comment)
	"Function arguments
	let l:FuncGenArgs = CmdFunc_GetUpdatedCppFunctionArgs(l:Context, l:FuncGenArgs)
	if (l:FuncGenArgs == [])
		return 0
	endif

	if( IsFunctionContextType(ContextType) )
		:call AddCode_CppFunction(l:Context, l:BaseArgs, l:FuncGenArgs)
	else
		"Unsupported context type here
		:call EchoContext(1, "Unsupported context for command", l:Context, "")
		return 0
	endif
	return 1
:endfunction

"Include:
"Args: Name (with or without .h) [IsSystem (0/1)]
:command! -nargs=* Inc :call CmdFunc_Inc(<f-args>)

"Add Cpp enum class or enum literal at current position of the document
"based on current context (inside enum or inside class)
"Arguments (when adding enum class):
"	Ops ClassName
"Arguments (when adding enum literal):
"	Ops LiteralName [Value]
:command! -nargs=* En :call CmdFunc_AddCode_EnumClassOrLiteral({}, <f-args>)

"Ars: Ops StructName 
:command! -nargs=* Stru :call CmdFunc_AddCode_CppClass({"IsStruct":1}, <f-args>)
"Ars: Ops ClassName 
:command! -nargs=* Cla :call CmdFunc_AddCode_CppClass({"IsStruct":0}, <f-args>)
"Ars: Ops TemplParams StructName 
:command! -nargs=* TStru :call CmdFunc_AddCode_CppClass({"IsStruct":1, "IsTempl":1}, <f-args>)
"Ars: Ops TemplParams ClassName 
:command! -nargs=* TCla :call CmdFunc_AddCode_CppClass({"IsStruct":0, "IsTempl":1}, <f-args>)

"Adds Cpp variable at current position of the document
"(inside function, or inside class)
"Args: OptionString Type Name [InitExpr]
":command! -nargs=* Va :call CmdFunc_AddCode_CppVarOrField(0, [], <f-args>)

"Args1: Ops Name [:type] [@initializer]
"Example calls:
"	:Va ; x :int @0 !category
:command! -nargs=* Va :call CmdFunc_AddCode_CppVar({'IsField':0, 'LinesAbove':[]}, <f-args>)

"Args: Ops Name [ArgsAndRetVal]
"ArgsAndRetval has the following format:
"Elements after the : (colon) char and before : (colon) or end of string is return value
"If return value is not specified, void is used
"Examples:
"	:F ; Sum int* A int* B :int		   - returns int + takes A,B
"	:F ; Sum :int:int* A int* B                - returns int + takes A,B
"	:F ; Msg int* A                            - returns void + takes A*
:command! -nargs=* F :call CmdFunc_AddCode_CppFunction({}, <f-args>)


"Helper function: Add Cpp class both definition and declaration
"with default name
":function! TestCppClass(TemplParams, IsStruct, OptionString)
"	:let l:DefaultName = "TestClass"
"	:let l:ExtraPrivateLinesAbove = [ "/*", "Line1", "Line2", "*/"]
"	:call CppClassAt_General(line("."), 0, l:DefaultName, a:TemplParams, a:IsStruct, a:OptionString, l:ExtraPrivateLinesAbove)
"	:call CppClassAt_General(line("."), 1, l:DefaultName, a:TemplParams, a:IsStruct, a:OptionString, l:ExtraPrivateLinesAbove)
":endfunction
"*** Enum definition
"*** Add class variables (with getters, default initialization)
