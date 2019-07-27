"C++ functions for adding C++ stuff
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
"*** Function declaration
"* Declaration of the following form:
"* ReturnType FunctionName(ContentString)
"* WARNING ; not appended!
:function! GetLine_CppFuncDecl_General(Name, ContentString, ReturnType)
	let l:declaration_string = a:ReturnType.' '.a:Name.'('.a:ContentString.')'
	return l:declaration_string
:endfunction

"* Appends function declaration with ; included at the end
:function! CppFuncDecl_General(LineNumber, Name, ContentString, ReturnType)
	let l:declaration_string = GetLine_CppFuncDecl_General(a:Name, a:ContentString, a:ReturnType)
	let l:formatted_decl_line = l:declaration_string.';'
	:call append(a:LineNumber, l:formatted_decl_line)
:endfunction
"*** Function definition
"*** Class/Struct definition
"*** Class/Struct template definition
