:- initialization(main).

main :-
	open('machpen1.txt',read, Stream),
    readLines(Stream, List_lines, 'res.txt'),
	display(List_lines),
	close(Stream),
    halt(0).

readLines(InStream,W,OutputFile):- 
        get_code(InStream,Char), 
        checkCharAndReadRest(Char,Chars,InStream,OutputFile), 
        atom_codes(W,Chars). 

checkCharAndReadRest(37,[],_,OutputFile):-  
        printErrorAndClose(OutputFile,"Error while parsing input file").
        
        % if char is # symbol for comment
checkCharAndReadRest(35,[],_,OutputFile):-  
        printErrorAndClose(OutputFile,"Error while parsing input file").
   
        % if at end of stream
checkCharAndReadRest(-1,[],_,_):-  !. 
        
        % if at end of file
checkCharAndReadRest(end_of_file,[],_,_):-  !. 
        
        % otherwise keep reading
checkCharAndReadRest(Char,[Char|Chars],InStream,OutputFile):- 
        get_code(InStream,NextChar), 
        checkCharAndReadRest(NextChar,Chars,InStream,OutputFile).

% Output if error message produced and close program
% ErrorMsg is a stream
printErrorAndClose(FileName,ErrorMsg):-
    open(FileName,write,OutputFileStream),
    write(OutputFileStream,ErrorMsg), nl(OutputFileStream),
    close(OutputFileStream).
    %halt. %Closes SWI-Prolog, but probably needed for final version