" Command: WARN! <args> will NOT work!
:command! -nargs=* TestCmd echo MySum(5, <f-args>) 

"WARN! Here we use arbitraty number of params (a,b) will NOT work with
" <f-args>!
:function! MySum(...)
	let l:res = 0
	let l:i = 0
	while l:i < len(a:000)
		echo "i:".l:i
		echo "arg: ".a:000[l:i]
		echo "res:".l:res
	       	let l:res = l:res + a:000[l:i]
		let l:i = l:i + 1
	endwhile
	return l:res
:endfunction

:function! TestFunc()
: echom "TestFunc"
:endfunction

:function! MakeStruct(Name)
: echom "struct " . a:Name
: echom "{"
: echom "};"
:endfunction

:function! TestAppend(Name)
:let l:lines=["a", "b"] "assign function local variable to array
:call append(line('.'), l:lines) "Appends set of lines, does NOT move cursor
:call cursor(line('.') + len(l:lines), 1) "Moves cursor on the last line of the inserted text
:endfunction

:map <F2> :echo "Test" \| echo "Test2" <CR>
:map <C-1> :echo "test CTRL 1" <CR>
:map <C-F2> :echo "test CTRL F2" <CR>
:map <^1> :echo "test ^(CTRL)1" <CR>

:map <F3> p #include ".h" <ESC>
:map <F4> p UPROPERTY(EditAnywhere, BlueprintReadWrite, Category=Misc) <ESC>

:map <F5> :call append(line('.'), "test\rtest2")<CR>
:map <F6> :call ':normal! A' . test\rtest2<CR>

:map <F7> :execute 'r D:\!DV_SCRIPT\VIM\CONFIG\struct.txt' <CR>
:map <F8> :call TestFunc() <CR>
