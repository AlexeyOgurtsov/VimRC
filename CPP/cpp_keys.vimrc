"Key bindings for C++ vim
"
"Include
"TODO: Add options
:map <F12> :Inc 

"Enum class or enum literal (based on context)
":map <A-F3> :En ;Op1;Op2; N
:map <A-F3> :En ; 
:map <F3> :En ; 

"Variable/Field
":map <A-F5> :Va ;Op1;Op2; int Var 0
:map <A-F5> :Va ; 
:map <F5> :Va ; 

"Function
":map <A-F6> :F ; N
:map <A-F6> :F ; 
:map <F6> :F ; 
:map <C-A-F6> :F ;PrivHere; 
:map <C-F6> :F ;PrivHere; 

"Struct/Class
"Old command
":map <F2> :Stru F Op1;Op2;
":map <S-F2> :TStru F {"T":"class"} Op1;Op2;
":map <C-F2> :Class F Op1;Op2;
":map <S-C-F2> :TClass F {"T":"class"} Op1;Op2;

":map <A-F2> :Stru ;Op1;Op2; F
:map <A-F2> :Stru ; 
:map <F2> :Stru ; 

":map <A-S-F2> :TStru ;Op1;Op2; {"T":"class"} F
:map <A-S-F2> :TStru ; {"T":"class"} 
:map <S-F2> :TStru ; {"T":"class"} 

":map <A-C-F2> :Cla ;Op1;Op2; D
:map <A-C-F2> :Cla ; 
:map <C-F2> :Cla ; 

":map <A-S-C-F2> :TCla ;Op1;Op2; {"T":"class"} F
:map <A-S-C-F2> :TCla ; {"T":"class"} 
:map <S-C-F2> :TCla ; {"T":"class"}

