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

:command! -nargs=* UStru :call CmdFunc_AddCode_UClass(1, ["GENERATED_BODY()",""], <f-args>)
