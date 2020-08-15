open Prolog

let main () = begin
        let lexbuf = Lexing.from_channel stdin in
        print_string "Welcome to my toy-prolog interpreter.\nYou should first input the name of prolog file of clauses.\nThe format should be [\"test.pl\"].\nNote that quotes and fullstop are needed.\n"; flush stdout;
        print_string "?- "; flush stdout;    
        let fname = Parser.file Lexer.token lexbuf in
	    let file = open_in fname in
	    let lexbuf = Lexing.from_channel file in
        let program = Parser.database Lexer.token lexbuf in
        print_endline "true."; print_newline(); flush stdout;
        let lexbuf = Lexing.from_channel stdin in
        let boo = ref true in
        let got = ref true in
        while !boo do
            try
                print_string "?- ";
                flush stdout;
                let goal = Parser.repl Lexer.token lexbuf in 
                let var_set = variableSet goal in 
                (* let goal_list = findFactList program goal in  *)
                let sol = solve program Sub.empty goal in 
                let checker = ref sol in                                                                            (* The name checker is misleading. It basically is a LazyList of remaining solutions. *)
                (* while (List.length !checker != 0) do
                    let print_sub v t = print_string((string_var v) ^ " = " ^ (string_term t) ^ "." ^ "\n") in Sub.iter print_sub (List.hd !checker); flush stdout; checker := List.tl !checker *)
                let flag = ref ";" in
                if (LL.is_empty !checker) then 
                    begin
                    got := false;
                    print_string "false.\n"; 
                    print_newline ();
                    flag := ".";
                    flush stdout
                    end
                else
                    got := true;
                    let print_sub v t = print_string((string_var v) ^ " = " ^ (string_term t) ^ "." ^ "\n") in Sub.iter print_sub (findNeededVar (LL.hd !checker) var_set); 
                    flush stdout; 
                    checker := LL.tl !checker; 
                    print_string "Press ; to continue or . for next query " ; 
                    flush stdout; 
                    flag := read_line ();             
                    while !flag = ";" do
                        if (LL.is_empty !checker) then 
                            begin
                            got := false;
                            print_string "false.\n"; 
                            print_newline ();
                            flag := ".";
                            flush stdout
                            end
                        else let print_sub v t = print_string((string_var v) ^ " = " ^ (string_term t) ^ "." ^ "\n") in Sub.iter print_sub (findNeededVar (LL.hd !checker) var_set); 
                        flush stdout; 
                        checker := LL.tl !checker; 
                        print_string "Press ; to continue or . for next query " ; 
                        flush stdout; 
                        flag := read_line ();
                    done;
                if !got then 
                    begin
                    print_string "."; 
                    print_newline (); 
                    flush stdout;
                    end
            with
                    _ -> boo := false;print_newline (); print_string "% halt"; print_newline (); flush stdout; 
        done
end;;

main();;