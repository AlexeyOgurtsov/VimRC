"Get USTRUCT, UCLASS etc.
"Arguments:
"	ULineType = USTRUCT, UCLASS etc.
"	Specs = specifiers before meta (BlueprintType etc.)
"	MetaLine = line to be specified after Meta=()
:function! GetULine(ULineType, Specs, MetaLine, Category)
	let between_scopes = a:Specs
		if len(a:MetaLine) > 0
			if len(between_scopes) > 0
				let between_scopes .= ", "
			endif
			let between_scopes .= "Meta=("
			let between_scopes .= a:MetaLine
		       	let between_scopes .= ")"
		endif
		if len(a:Category) > 0
			if len(between_scopes) > 0
				let between_scopes .= ", "
			endif
			let between_scopes .= "Category="
			let between_scopes .= a:Category
		endif
	let res = a:ULineType."(".between_scopes.")"
	return res
:endfunction
:function! GetUStructLine(Specs, MetaLine, Category)
	return GetULine("USTRUCT", a:Specs, a:MetaLine, a:Category)
:endfunction
:function! GetUClassLine(Specs, MetaLine, Category)
	return GetULine("UCLASS", a:Specs, a:MetaLine, a:Category)
:endfunction
:function! GetUEnumLine(Specs, MetaLine, Category)
	return GetULine("UENUM", a:Specs, a:MetaLine, a:Category)
:endfunction
:function! GetUMetaLine(Specs, MetaLine, Category)
	return GetULine("UMETA", a:Specs, a:MetaLine, a:Category)
:endfunction
:function! GetUPropertyLine(Specs, MetaLine, Category)
	return GetULine("UPROPERTY", a:Specs, a:MetaLine, a:Category)
:endfunction


" Fixed name of UClass/UStruct/UEnum
:function! GetFixedEnumOrClassName(ContextType, IsStruct, IsTempl, Name, Ops)
	let ShouldFixPrefix = 1

	if(a:ContextType == g:ContextType_Class)
		if a:IsTempl
			let DesiredPrefix = "T"
		else
			"Non templ"
			if a:IsStruct
				let DesiredPrefix = "F"
			else
				"If UClass, then we do NOT known by default its type
				"(maybe A, maybe U)
				"let DesiredPrefix = "U"
				let ShouldFixPrefix = 0
			endif
		endif
	elseif (a:ContextType == g:ContextType_Enum)
		let DesiredPrefix = "E"
	else
		"Unknown type or function
		let ShouldFixPrefix = 0
	endif

	if ShouldFixPrefix
		return GetNameWithFixedPrefix(a:Name, DesiredPrefix)
	else
		return a:Name
	endif
:endfunction

"Returns dictionary of context at the given line from the current buffer
"Members:
"- All members of GetContextDictAt
:function! GetUnrealContextAt(LineIndex)
	let l:Context = {}
	:call extend(l:Context, GetCppContextAt(a:LineIndex))
	return l:Context
:endfunction
:function! GetContextAt(LineIndex)
	return GetUnrealContextAt(a:LineIndex)
:endfunction

"Returns struct or class lines above from options
:function! GetClassLines_FromOptions(ClassLinesAboveHeader, IsStruct, IsTempl, Category, Ops)
	:if(a:IsTempl && (a:Ops !~# ";Force;"))
		"For now unreal engine 4 does not support template classes or
		"structs, so we should not add generated body etc.
		return []
	:endif

	"echo "GetClassLines_FromOptions: DEBUG: type(a:ClassLinesAboveHeader)=".type(a:ClassLinesAboveHeader)
	:call extend(a:ClassLinesAboveHeader, ["GENERATED_BODY()", ""])

	if(a:IsStruct)
		let ClassLinesAbove = [GetUStructLine("BlueprintType", "", a:Category)]
	else
		let l:Specs = ""
		let l:Meta = ""
		if (a:Ops =~# ";Abstract;")
			"l:Specs .= ""TODO
		endif	
		if (a:Ops =~# ";Comp;")
			"TODO
		endif
		let ClassLinesAbove = [GetUClassLine(l:Specs, l:Meta, a:Category)]
	endif
	return ClassLinesAbove
:endfunction!

:function! CmdFunc_AddCode_UClass(...)
	let l:EntityType = g:ContextType_Class

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
	"TODO: calc name argument index!
	if NoArg(1, l:MyArgs, "Name", 0)
		return 0
	endif

	"Is templated class"
	let l:IsTempl = GetKey_IntType(l:BaseArgs, "IsTempl")
	let l:IsStruct = GetKey_IntType(l:BaseArgs, "IsStruct")

	"WARNING! Arguments are shifted based on whether templ or NON-templ
	"command used!
	if l:IsTempl
		let l:NameArgIndex = 1 
	else
		"NON templ class
		let l:NameArgIndex = 0 
	endif
	let l:CategoryArgIndex = (l:NameArgIndex + 1)
	let l:Category = GetOrDefault(l:MyArgs, l:CategoryArgIndex, "Misc")

	let TemplParams = {} " TODO
	let IsReallyTempl = l:TemplParams != {} 
	if IsReallyTempl
		if ( l:Ops !~# ";Force;")
			echoerr "Template UStruct/UClass are not supported by Unreal Engine for now (use ;Force; to add)"	
			return
		endif
	endif

	"Updaing new arguments
	let NewArgs = deepcopy(a:000)

	"Update name	
	let FixedName = GetFixedEnumOrClassName(l:EntityType, l:IsStruct, l:IsTempl, l:MyArgs[NameArgIndex], l:Ops)
	"DEBUG {
	"echo "CmdFunc_AddCode_UClass: DEBUG: FixefName: ".FixedName
	"DEBUG }
	let l:NewArgs[g:NumCommonArgs + l:NameArgIndex] = FixedName

	"Update U-specific lines (header above and class above)
	let l:NewArgs[g:BaseArgsIndex]["ExtraPrivateLinesAbove"] = [] "We must always add a key
	let l:NewArgs[g:BaseArgsIndex]["ClassLinesAbove"] = GetClassLines_FromOptions(l:NewArgs[g:BaseArgsIndex]["ExtraPrivateLinesAbove"], l:IsStruct, l:IsTempl, l:Category, l:Ops)

	"Call the basic CppClass function
	:call call(function("CmdFunc_AddCode_CppClass"), NewArgs)
:endfunction

:function! GetUPropertyLine_ForVar(Context, OptionString, TypeStr, Name, Category)
	"TODO: Check correct meta line based on access (private, public etc.)
	let l:Specs = "EditAnywhere, BlueprintReadWrite"
	let l:MetaLine = "AllowPrivateAccess=true"
	return GetUPropertyLine(Specs, MetaLine, a:Category)
:endfunction

:function! CmdFunc_AddCode_UVarOrProp(...)
	let l:n = len(a:000)
	if l:n < 5
		echoerr "OptionString, Category, TypeStr and Name must be specified"
		return
	else
		"Here we have valid count of arguments supplied
		"Forming up arguments
		let l:IsField = a:000[0]
		let l:OptionString = a:000[1]
		let l:Category = a:000[2]
		let l:TypeStr = a:000[3]
		let l:Name = a:000[4]

		" If initializer list is specified
		if l:n >= 5
			let l:InitializerList = a:000[5]
		else
			let l:InitializerList = ""
		endif

		"TODO: Why the insert line is NOT specified explicitly inside
		"the command call?"
		let l:InsertLine = line('.')	

		"Context dictionary: includes info about the context:
		"(See help near the context function definitions)
		let l:Context = GetContextAt(l:InsertLine)

		"Calculating lines above (typically UPROPERTY())
		if (l:OptionString !~# "NoProp;")
			let l:LinesAbove = [ GetUPropertyLine_ForVar(l:Context, l:OptionString, l:TypeStr, l:Name, l:Category) ]
		else
			let l:LinesAbove = []
		endif

		"Calling the command
		let l:NewArgs = [l:IsField, l:LinesAbove, l:OptionString, l:TypeStr, l:Name, l:InitializerList]
		:call call(function("CmdFunc_AddCode_CppVarOrField"), l:NewArgs)
	endif
:endfunction

:function! GetUEnumClass_LinesAbove(IsFlags, OpsList, Name, Category)
	let l:Lines = []
	let l:Specs = "BlueprintType" "TODO
	let l:MetaLine = "" 
	if (a:IsFlags)
		let l:MetaLine .= "Bitflags"
	endif
	:call add(l:Lines, GetUEnumLine(Specs, MetaLine, a:Category))
	return l:Lines
:endfunction

:function! GetUEnumClass_LinesBelow(IsFlags, OpsList, Name, Category)
	let l:Lines = []
	if a:IsFlags
		:call add(l:Lines, 'ENUM_CLASS_FLAGS('.a:Name.');')
	endif
	return l:Lines
:endfunction

:function! IsUEnumSpecialHiddenLiteral(IsFlags, OpsList, Name, ValueStr, Specs)
	if(a:IsFlags)
		if(a:Name == "Default" || a:Name == "None")
			return 1
		endif

		if(a:ValueStr == "0")
			return 1
		endif
	endif

	return 0
:endfunction

:function! GetFlagEnumLiteralValueStr(Shift, Ops)
	return "1 << ".a:Shift
:endfunction

:function! GetUEnumLiteral_FixedValueStr(Context, IsFlags, Ops, Name, ValueStr)	
	"echo "DEBUG: ValueStr=".a:ValueStr
	let MaxEnumFlag_LineIndex = GetContext_MaxEnumFlag_LineIndex(a:Context)
	let MaxEnumFlagValue = GetContext_MaxEnumFlag_Value(a:Context)
	"echo "DEBUG: (Context)MaxEnumFlagValue=".MaxEnumFlagValue
	if a:ValueStr == ""
		if a:IsFlags
			if (MaxEnumFlag_LineIndex >= 0)
				"echo "DEBUG: Here"
				return GetFlagEnumLiteralValueStr(MaxEnumFlagValue + 1, a:Ops)
			else 
				"No flag defined yet, define the first one
				return GetFlagEnumLiteralValueStr(0, a:Ops)
			endif
		endif
	endif

	return a:ValueStr
:endfunction

:function! GetUEnumLiteral_LineAfter(IsFlags, OpsList, Name, ValueStr, Specs)
	let l:MetaLine = ""
	let l:Category = ""
	let l:RealSpecs = a:Specs

	if IsUEnumSpecialHiddenLiteral(a:IsFlags, a:OpsList, a:Name, a:ValueStr, a:Specs)
		let l:RealSpecs .= "Hidden"
	else
		"Not special hidden literal!"
		if (a:Name !~# ("NoDisplayName"))
			if len(l:RealSpecs) > 0
				let l:RealSpecs .= ", "
			endif
			let l:RealSpecs .= "DisplayName=\"".a:Name."\""
		:endif
	endif

	return GetUMetaLine(l:RealSpecs, l:MetaLine, l:Category)
:endfunction

:function! AddCode_UEnumFlags_DefaultMembers(Context, Ops)
	if(a:Ops =~# ";NoDef;")
		return
	endif

	"Line, where the start brace of the enum is located
	let l:EnumBraceLine = FindFirstFrom(GetContextLine(a:Context), "{")
	:call cursor(l:EnumBraceLine, 1)

	" DEBUG {
		"echo "DEBUG: AddCode_UEnumFlags_DefaultMembers: "
		"echo "l:EnumBraceLine=".l:EnumBraceLine
	" DEBUG }

	"Adding the None literal
	:UEn Flags; None 0
	let l:AfterNoneLocation = line('.')

	"Adding the Default literal
	:UEn Flags; Default None	

	"Jump to position right after the None literal, so we can add new
	"literals right below it just after we created the enum class
	:call cursor(l:AfterNoneLocation, 1)
:endfunction

"Arguments: see the corresponding command
:function! CmdFunc_AddCode_UEnumOrLiteral(...)
	let l:EntityType = g:ContextType_Enum

	let l:Context = {}
	let l:BaseArgs = {}
	let l:OpsList = []
	let l:MyArgs = [] "Custom args of this command
	if ExtractCmdArgs_TrueOnFail("", a:000, l:Context, l:BaseArgs, l:OpsList, l:MyArgs)
		return 0
	endif
	let l:Ops = l:OpsList[0]

	"TODO: Does it depend on somethif (name index)
	let l:NameArgIndex = 0

	"Checking custom args
	if NoArg(1, l:MyArgs, "Name", l:NameArgIndex)
		return 0
	endif


	"Checking args
	let l:Name = l:MyArgs[l:NameArgIndex]

	let l:IsFlags = (l:Ops =~# "Flags;")
	
	let l:ContextType = GetContextType(l:Context)
	let l:ClassLinesAbove = []
	let l:ClassLinesBelow = []
	let l:LiteralLineAfter = ""

	"Should we add literal (not enum class)
	if IsEnumClassContextType(l:ContextType)
		let l:AddLiteral = 0
		let l:Category = GetOrDefault(l:MyArgs, 1, "Misc")
		"Fixing name
		let FixedName = GetFixedEnumOrClassName(l:EntityType, 0, 0, l:Name, l:Ops)
		let l:ClassLinesAbove = GetUEnumClass_LinesAbove(l:IsFlags, l:OpsList, l:FixedName, l:Category)
		let l:ClassLinesBelow = GetUEnumClass_LinesBelow(l:IsFlags, l:OpsList, l:FixedName, l:Category)
	elseif IsEnumLiteralContextType(l:ContextType)
		let l:AddLiteral = 1
		let FixedName = l:Name "No name fixing required when adding a literal
		let l:LiteralSpecs = ""
		let l:LiteralValueStrArgIndex = 1
		" Literal value str
		let l:LiteralValueStr = GetUEnumLiteral_FixedValueStr(l:Context, l:IsFlags, l:Ops, l:Name, GetOrDefault(l:MyArgs, l:LiteralValueStrArgIndex, ""))
		let l:LiteralLineAfter = GetUEnumLiteral_LineAfter(l:IsFlags, l:Ops, l:Name, l:LiteralValueStr, l:LiteralSpecs)
	else
		"Unsupported context type here
		:call EchoContext(1, "Unsupported context for command", l:Context, "")
		return 0
	endif

	"Calling the cpp-level command
	:let l:NewArgs = deepcopy(a:000)
	:let l:NewArgs[g:NumCommonArgs + l:NameArgIndex] = FixedName
	if l:AddLiteral
		if(g:NumCommonArgs + l:LiteralValueStrArgIndex >= len(l:NewArgs))
			:call add(l:NewArgs, "")
		endif
		:let l:NewArgs[g:NumCommonArgs + l:LiteralValueStrArgIndex] = l:LiteralValueStr
	endif
	:let l:NewArgs[g:BaseArgsIndex]["ClassLinesAbove"] = l:ClassLinesAbove
	:let l:NewArgs[g:BaseArgsIndex]["ClassLinesBelow"] = l:ClassLinesBelow
	:let l:NewArgs[g:BaseArgsIndex]["LiteralLineAfter"] = l:LiteralLineAfter
	:call call(function("CmdFunc_AddCode_EnumClassOrLiteral"), l:NewArgs)

	"Post-command actions
	if(BoolNot(l:AddLiteral))
		if(l:IsFlags)
			:call AddCode_UEnumFlags_DefaultMembers(l:Context, l:Ops)
		endif
	endif

	return 1
:endfunction

"arguments for enum class
"	Options Name [category (default to misc)]
"arguments for enum literal
"	Options Name
:command! -nargs=* UEn :call CmdFunc_AddCode_UEnumOrLiteral({}, <f-args>)

:command! -nargs=* UStru :call CmdFunc_AddCode_UClass({"IsStruct":1}, <f-args>)
:command! -nargs=* UCla :call CmdFunc_AddCode_UClass({"IsStruct":0}, <f-args>)

":command! -nargs=* UStru :call CmdFunc_AddCode_UClass(1, ["GENERATED_BODY()",""], <f-args>)

"Add Unreal Property (with UPROPERTY or without, according to flags)
"Arguments:
"OptionString Category Type Name [InitExpr]
:command! -nargs=* UPr :call CmdFunc_AddCode_UVarOrProp(0, <f-args>)
