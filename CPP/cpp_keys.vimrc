"Key bindings for C++ vim
"Include
:map <F12> :Inc 
"Enum class or enum literal (based on context)
:map <F3> :En Op1;Op2; N
"Variable/Field
:map <F5> :Va Op1;Op2; int Var 0
"Struct/Class
:map <F2> :Stru F Op1;Op2;
:map <S-F2> :TStru F {"T":"class"} Op1;Op2;
:map <C-F2> :Class F Op1;Op2;
:map <S-C-F2> :TClass F {"T":"class"} Op1;Op2;

