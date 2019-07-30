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

:function! GetFixedUStructName(Name)
	let FixedName = copy(a:Name)

	let IsLowerF = (FixedName[0] ==# 'f')

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
		if ((FixedName[0] !=# 'F'))
			let FixedName = 'F'.FixedName
		endif
	endif

	let FixedName_TwoChars = strpart(FixedName, 0, 2)
	let FixedName_Rest = strpart(FixedName, 2)

	"Upper-casing first character after F if NOT upper-cased yet
	let FixedName_TwoChars = toupper(FixedName_TwoChars)
	let FixedName = FixedName_TwoChars.FixedName_Rest 

	return FixedName
:endfunction

"Returns dictionary of context at the given line from the current buffer
"Members:
"- All members of GetContextDictAt
:function! GetUnrealContextAt(LineIndex)
	let l:Context = {}
	:call extend(l:Context, GetCppContextAt(a:LineIndex))
	return l:Context
:endfunction

:function! CmdFunc_AddCode_UClass(...)
	let l:new_args = copy(a:000)
	"Update arguments
	let l:new_args[g:AddCode_CppClass_ClassNameArgIndex] = GetFixedUStructName(l:new_args[g:AddCode_CppClass_ClassNameArgIndex])
	"ULines
	let ULines = [GetUStructLine("BlueprintType", "", "Misc")]
	:call AddIndentedCodeLines(ULines)
	:call JumpAfter(ULines)
	"Class itself
	:call call(function("CmdFunc_AddCode_CppClass_Default"), new_args)
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
		let l:Context = GetUnrealContextAt(l:InsertLine)

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

:command! -nargs=* UStru :call CmdFunc_AddCode_UClass(1, ["GENERATED_BODY()",""], <f-args>)

"Add Unreal Property (with UPROPERTY or without, according to flags)
"Arguments:
"OptionString Category Type Name [InitExpr]
:command! -nargs=* UPr :call CmdFunc_AddCode_UVarOrProp(0, <f-args>)
