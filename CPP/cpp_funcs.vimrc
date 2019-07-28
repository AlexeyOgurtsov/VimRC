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
:function! GetLines_CppClass_General(OutDefinition, Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove)
	let l:lines = []
	return l:lines
:endfunction


" Adds definition or declaration on the given line, based on arguments
:function! CppClass_General(LineNumber, IsDefinition, Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove)
	let l:definition_lines = []
	let l:class_lines = GetLines_CppClass_General(l:definition_lines, a:Name, a:TemplParams, a:IsStruct, a:OptionString, a:ExtraPrivateLinesAbove)
	:if a:IsDefinition
		:call append(a:LineNumber, l:definition_lines)
	else
		:call append(a:LineNumber, l:class_lines)
	:endif
:endfunction
"*** Class/Struct template definition
"*** Enum definition
