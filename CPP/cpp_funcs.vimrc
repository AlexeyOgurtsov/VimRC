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
	"Explicit inline specifier if necessary"
	:if a:OptionString =~ "Inline;"
		let l:declaration_string = l:declaration_string . "inline "	
	:endif
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
	echo "DEBUG: GetLine_CppFuncDecl_General: OptionString=". a:OptionString
	echo "DEBUG: GetLine_CppFuncDecl_General: declaration_string=". l:declaration_string
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
	:if (a:ReturnType != "void") && (a:ReturnType != "")
		let l:ReturnStmt = "return ReturnType{};"
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
"*** Type string of form "Type&&"
:function! GetTypeString_RValueRef(TypeName)
	return a:TypeName . "&&"
:endfunction
"*** Type string of form "const Type&"
:function! GetTypeString_RefToConst(TypeName)
	return "const " . a:TypeName . "&"
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
	let l:ShouldNotInline = (a:OptionString !~ "Inline;") && (len(a:TemplParams) == 0)
	:let l:GenImpl = (a:OptionString !~ "NoImpl;") && (a:OptionString !~ "Def;")
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
		:if l:GenImpl
			let l:ImplContent = GetFuncImplContentString(a:ContentString)
			"function header line inside implementation 
			"(inside .cpp file or inlined)
			let l:header_line_impl = GetLine_CppFuncDecl_General(a:Name, l:ImplContent, a:ReturnType, a:OptionString)
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
	let l:public_lines = []
	"TODO
	return l:public_lines
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
:function! GetLines_CppClass_General(OutDefinition, Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove)
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
" Adds definition or declaration on the given line, based on arguments
:function! CppClassAt_General(LineNumber, IsDefinition, Name, TemplParams, IsStruct, OptionString, ExtraPrivateLinesAbove)
	let l:definition_lines = []
	let l:class_lines = GetLines_CppClass_General(l:definition_lines, a:Name, a:TemplParams, a:IsStruct, a:OptionString, a:ExtraPrivateLinesAbove)
	:if a:IsDefinition
		:call append(a:LineNumber, l:definition_lines)
	:else
		:call append(a:LineNumber, l:class_lines)
	:endif
:endfunction
"helper for adding non-template class with default options
:function! CppClassAt(LineNumber, IsDefinition, Name)
	:call CppClassAt_General(a:LineNumber, a:IsDefinition, a:Name, [], 0, "", [])
:endfunction
"helper for adding non-template struct with default options
:function! CppStructAt(LineNumber, IsDefinition, Name)
	:call CppClassAt_General(a:LineNumber, a:IsDefinition, a:Name, [], 1, "", [])
:endfunction
"Helper function: Add Cpp class both definition and declaration
"with default name
:function! TestCppClass(TemplParams, IsStruct, OptionString)
	:let l:DefaultName = "TestClass"
	:let l:ExtraPrivateLinesAbove = [ "/*", "Line1", "Line2", "*/"]
	:call CppClassAt_General(line("."), 0, l:DefaultName, a:TemplParams, a:IsStruct, a:OptionString, l:ExtraPrivateLinesAbove)
	:call CppClassAt_General(line("."), 1, l:DefaultName, a:TemplParams, a:IsStruct, a:OptionString, l:ExtraPrivateLinesAbove)
:endfunction
"*** Enum definition
"*** Add class variables (with getters, default initialization)
