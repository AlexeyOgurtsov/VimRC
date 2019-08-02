"C++ functions for adding C++ stuff

:function! UpdateStringOpenState_Curly(OpenState, Line)
	return UpdateStringOpenState(a:OpenState, a:Line, "{", "}")
:endfunction

:function! GetContextAt(LineIndex)
	return GetCppContextAt(a:LineIndex)
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
	let l:Pattern = '^'.l:CommaPattern
	let l:Pattern .= l:NamePattern
	let l:Pattern .= '\s*=\s*'
	let l:Pattern .= '\s*1\s*'
	let l:Pattern .= '\s*<<\s*'
	let l:Pattern .= '\s*\w\+\s*'
	let l:Pattern .= l:CommaPattern
	let l:Pattern .= '$'

	let l:IsFlagLiteral = (a:Line =~# l:Pattern)
	"echo "GetEnumFlagValueStr: DEBUG: IsFlagLiteral=".l:IsFlagLiteral

	if BoolNot(l:IsFlagLiteral)
		return ""

	else
		"Here we have the enum flag literal

		"Lets find the last from end digit or alpha numeric - it's our
		"expression start
		let expr_start_idx = -1
		"And the first from end digit or alpha-numeri
		" -it's our expression end
		let expr_end_idx = -1
		let l:i = len(a:Line) - 1
			

		while (expr_start_idx < 0) || (expr_end_idx < 0)
			"echo "DEBUG: i=".l:i." val=".strpart(a:Line, l:i, 1)

			"Here we found character, that can be a part of the
			"string we search for
			if(strpart(a:Line, l:i, 1) =~ '\w')
				if(expr_end_idx < 0)
					"End is not found yet
					let expr_end_idx = l:i
				endif
			else
				"Curr character is not part of the string we
				"search for
				if(expr_end_idx >= 0)
					let expr_start_idx = l:i + 1
				endif
			endif

			let l:i -= 1
		endwhile

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

:function! CalculateNumEnumLiterals(OpenBraceLineIndex, Lines)
	:if BoolNot(AreLinesMeaningful(a:Lines) )
		return 0	
	:endif

	let CountByPattern = CountLinesByPattern(a:Lines, '\s*\,\s*')
	return CountByPattern + 1 

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

	if IsContextHeaderFound
		let l:StartLine = (l:LineIndex + 1)
		let a:OutCppContext[g:Context_StartLine] = l:StartLine

		let OpenBraceLineIndex = FindFirstFrom(l:StartLine, '{')

		"Lines inside body (not including { and } braces )
		let LinesInsideBody = []
		:call ComputeLinesInsideContextBody(OpenBraceLineIndex, l:ContextType, LinesInsideBody)
		let a:OutCppContext[g:Context_LinesInsideBody] = LinesInsideBody

		"Compute end line index based on the lines inside body
		let a:OutCppContext[g:Context_OpenBraceLine] = OpenBraceLineIndex
		let a:OutCppContext[g:Context_EndLine] = OpenBraceLineIndex + len(LinesInsideBody)

		let a:OutCppContext[g:Context_IndentationParam] = GetLineIndentationParam(getline(l:StartLine))
		"echo "Debug: StartLine=".l:StartLine." LineIndentationParam=".GetLineIndentationParam(getline(l:StartLine))
		"echo "Debug: StartLine=".l:StartLine." OutCppContext[IndentationParam]=".a:OutCppContext[g:Context_IndentationParam]

		if l:ContextType == g:ContextType_Enum
			let a:OutCppContext[g:Context_NumEnumLiterals] = CalculateNumEnumLiterals(OpenBraceLineIndex, LinesInsideBody)
		elseif l:ContextType == g:ContextType_Class
			"TODO
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

	let a:OutCppContext[g:Context_Type] = l:ContextType
	return l:ContextType
:endfunction

" Cpp Context at the given line
" Members:
" -All from Core
:function! GetCppContextAt(LineIndex)
	let l:res = {}
	:call extend(l:res, GetCoreContextAt(a:LineIndex))
	let l:ContextType = ExtractCppContextLines(l:res)
	return l:res
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
		let l:res = AddCodeAt(GetBestContext(a:Context), l:lines, [], a:Options)
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

:function! GetLines_GetVarOrFieldCommentText(IsField, TypeName, Name, InitExpr, OptionString)
	let l:lines = []
	:call add(l:lines, a:Name)
	return l:lines
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
	"TODO: ugly redirection
	return GetLines_GetVarOrFieldComment_IfShould(0, "", a:Name, "", a:Ops)
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
	let l:IsWithClass = 0
	return a:ClassName . GetTemplParamsString(a:TemplParams, l:IsWithClass) . "::"
:endfunction
"Generates string of form Name(ContentString) [const]
:function! GetLine_CppFunc_NameArgs_AndSpecs(Name, ContentString, OptionString)
	let l:res_s = a:Name.'('.a:ContentString.')'
	:if (a:OptionString =~ "Const;")
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
	"Virtual if necessary"
	:if a:OptionString =~ "Virt;"
		let l:declaration_string = l:declaration_string . "virtual "	
	:endif
	"" Return type
	let l:declaration_string = l:declaration_string . a:ReturnType
	:if len(a:ReturnType) > 0
		let l:declaration_string = l:declaration_string . " "	
	:endif
	let l:declaration_string = l:declaration_string. GetLine_CppFunc_NameArgs_AndSpecs(a:Name, a:ContentString, a:OptionString)
	:if a:OptionString =~ "Def;"
		let l:declaration_string = l:declaration_string . " =default"	
	:endif
	:if a:OptionString =~ "Delete;"
		let l:declaration_string = l:declaration_string . " =delete"	
	:endif
	"Forming prefix
	:if (a:OptionString =~ "Over;")
		let l:declaration_string = l:declaration_string . " override"
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
"*** Calculates default return statemen lines for function
:function! GetLines_DefaultReturnStmt(Name, ClassName, TemplParams, ContentString, ReturnType, OptionString)
	let l:Lines = []

	"Handling special cases for custom operators first
	:if (a:Name==#"operator=")
		return ["return *this;"]
	:endif

	:if (a:ReturnType != "void") && (a:ReturnType != "")
		let l:ReturnStmt = "return ".a:ReturnType."{};"
		:call add(l:Lines, l:ReturnStmt)
	:endif
	return l:Lines
:endfunction
:function! GetLines_CppFuncDefaultBody(ReturnStmt, ContentString, ReturnType, OptionString)
	let l:Lines = []
	let l:ReturnStmtLines = deepcopy(a:ReturnStmt)
	:call IdentBlock(l:ReturnStmtLines, 1)
	:call add(l:Lines, "{")
	:call extend(l:Lines, l:ReturnStmtLines)
	:call add(l:Lines, "}")
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
"*** Function definition
"*** Function's both declaration and definition
"*** WARNING! Lines are NOT idented
:function! GetLines_CppFunc(OutDefinition, Name, ClassName, TemplParams, ContentString, ReturnType, OptionString)
	let l:decl_lines = []
	let l:header_line = GetLine_CppFuncDecl_General(a:Name, a:ContentString, a:ReturnType, a:OptionString)
	let l:ReturnStmt = GetLines_DefaultReturnStmt(a:Name, a:ClassName, a:TemplParams, a:ContentString, a:ReturnType, a:OptionString)
	let l:func_body = GetLines_CppFuncDefaultBody(l:ReturnStmt, a:ContentString, a:ReturnType, a:OptionString)
	let l:ShouldInline = (a:OptionString =~ "Inline;") || (len(a:TemplParams) > 0)
	:call DebugEcho("Debug: GetLines_CppFunc: ShouldInline: ".l:ShouldInline)
	let l:ShouldNotInline = BoolNot(l:ShouldInline)
	:let l:GenImpl = (a:OptionString !~ "NoImpl;") && (a:OptionString !~ "Def;")
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
			let l:impl_header = GetLines_CppFuncImplHeader(a:Name, a:ClassName, a:TemplParams, a:ContentString, a:ReturnType, a:OptionString)
			:call extend(a:OutDefinition, l:impl_header)
			:call extend(a:OutDefinition, l:func_body)
		:endif
	:else
		"Here we should provide body inside declaration
		"Debug
		:call DebugEcho("Debug: GetLine_CppFunc: here we should provide body inside decl")
		:if l:GenImpl
			let l:ImplContent = GetFuncImplContentString(a:ContentString)
			"function header line inside implementation 
			"(inside .cpp file or inlined)
			let l:header_line_impl = GetLine_CppFuncDecl_General(a:Name, l:ImplContent, a:ReturnType, a:OptionString)
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
	let l:IsVirtualDtor = (a:OptionString =~ "VirtDtor;") || (a:OptionString =~ "I;")
	"Default/Delete
	let l:IsDef = (a:OptionString !~ "Cust;") && (a:OptionString !~ "CustDtor;") && (a:OptionString !~ "Ptr;")
	let l:IsDeleted = 0 "TODO
	let l:ShouldInline = (a:OptionString =~ "Inline;") || (a:OptionString =~ "InlineDtor;")
	let l:NoImpl = (a:OptionString =~ "NoImpl;") || (a:OptionString =~ "NoImplDtor;")
	"Forming result
	let l:PublicDeclaration = []
		let l:FunctionName = '~' .  a:ClassName "As we have destructor, function name is the same as class + ~
		:lockvar l:FunctionName
		let l:FunctionReturnType = "" "As we have destructor, function return type is empty
		:lockvar l:FunctionReturnType
		let l:FuncOptions = ""
		let l:ContentString = ""
		:lockvar l:ContentString
		:if l:IsVirtualDtor
			let l:FuncOptions = l:FuncOptions . "Virt;"
		:endif
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
		let l:decl = GetLines_CppFunc(a:OutDefinition, l:FunctionName, a:ClassName, a:TemplParams, l:ContentString, l:FunctionReturnType, l:FuncOptions)
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
		let l:ContentString = ""
		:lockvar l:ContentString
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
		let l:decl = GetLines_CppFunc(a:OutDefinition, l:FunctionName, a:ClassName, a:TemplParams, l:ContentString, l:FunctionReturnType, l:FuncOptions)
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

		let l:ContentString = GetFuncImplContentString_CopyMove(a:IsMove, a:ClassName, a:TemplParams, a:OptionString)
		:lockvar l:ContentString

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

		let l:decl = GetLines_CppFunc(a:OutDefinition, l:FunctionName, a:ClassName, a:TemplParams, l:ContentString, l:FunctionReturnType, l:FuncOptions)
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
	"Calculate lines"
	let l:PrivLines = []
	let l:PublicLines = GetLines_CppClass_General(l:PrivLines, a:Name, a:TemplParams, a:IsStruct, a:OptionString, a:ExtraPrivateLinesAbove, a:ExtraLinesAbove)
	"Add code"
	let l:Context = ContextOrCurr({}, a:OptionString)
	:call AddCodeAt(l:Context, l:PublicLines, l:PrivLines, a:OptionString)
	"TODO Cursor jumping
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
:function! AddCode_CppVarOrField(IsField, LinesAbove, OptionString, TypeName, Name, InitExpr)
	"Reserved for impl, for example initializers for static variables"
	let l:variable_cpp_lines = []
	"Variable declaration itself
	let l:variable_lines = GetLines_CppVarOrField(l:variable_cpp_lines, a:IsField, a:LinesAbove, a:OptionString, a:TypeName, a:Name, a:InitExpr)

	"Add code
	let l:InsertionStartLine = AddCodeAt(GetContextAt(line('.')), l:variable_lines, l:variable_cpp_lines, a:OptionString)

	"We should jump after the inserted lines
	:call JumpAfterAt(l:InsertionStartLine, l:variable_lines)
:endfunction

" Adds variable or field 
" (See command)
" Arguments:
" 1. IsField (1 - field)
" Fields are added to classes always (ever if we're currently inside function)
" 2. LinesAbove (List of strings)
" May include here UPROPERTY(), for example
" 3. OptionString  (string)
" Of form "Opt1;Opt2;" (string)
" 4. TypeName (String)
" 5. Name (String)
" 6. Initializer expression (optional, String)
:function! CmdFunc_AddCode_CppVarOrField(...)
	let args = a:000
	let n = len(args) - 3

	:call assert_true(len(args) >= g:AddCode_CppVarOrField_InitExpr_ArgIndex, "Argument count wrong for Cpp Var or Field addition function")

	let IsField = a:000[g:AddCode_CppVarOrField_IsField_ArgIndex]
	let LinesAbove = a:000[g:AddCode_CppVarOrField_LinesAbove_ArgIndex]
	let OptionString = a:000[g:AddCode_CppVarOrField_OptionString_ArgIndex]

	if n < 2
		echoerr "OptionString, TypeName and Name must be specified"
		return
	else
		let TypeName = args[g:AddCode_CppVarOrField_TypeName_ArgIndex]
		let Name = args[g:AddCode_CppVarOrField_Name_ArgIndex]

		if n >= 3
			let InitializerExpression = args[g:AddCode_CppVarOrField_InitExpr_ArgIndex]
		else
			let InitializerExpression = "" "TODO: Should we choose default initializer
		endif

		:call AddCode_CppVarOrField(IsField, LinesAbove, OptionString, TypeName, Name, InitializerExpression)
	endif
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
	return AddCodeAt(a:Context, l:lines, [], a:Ops)
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

	return AddCodeAt(a:Context, l:lines, [], a:Ops)
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

"Arguments: see the corresponding command for arguments
:function! CmdFunc_AddCode_CppClass(...)
	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]
	let l:ContextType = GetContextType(l:Context)

	"Checking custom args
	if NoArg(1, l:MyArgs, "Name", 0)
		return 0
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
:command! -nargs=* Va :call CmdFunc_AddCode_CppVarOrField(0, [], <f-args>)


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
