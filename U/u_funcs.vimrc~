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

:function! CmdFunc_AddCode_UClass(...)
	let l:new_args = copy(a:000)
	let ULines = [GetUStructLine("BlueprintType", "", "Misc")]
	:call AddIndentedCodeLines(ULines)
	:call JumpAfter(ULines)
	:call call(function("CmdFunc_AddCode_CppClass_Default"), new_args)
:endfunction

:command! -nargs=* UStru :call CmdFunc_AddCode_UClass(1, ["GENERATED_BODY()",""], <f-args>)
