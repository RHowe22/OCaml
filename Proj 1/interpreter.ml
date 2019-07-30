type const = I of int | B of bool | Error | S of string | U| Name of string | Fun of ((string list)*((string*const) list)*bool)

(** No longer a BST but kept to make life easier *)
let rec treeInsert((key:string),(value:const),nameTree):((string*const) list)=
   match nameTree with
   | hd::tl->(match hd with | (curkey,curval) -> if(key< curkey) then (key,value)::nameTree
                                                 else if(key > curkey) then hd::(treeInsert(key,value,tl))
                                                 else (key,value)::tl)
   | [] ->[(key,value)]

let rec findVal((key:string),(nameTree:((string*const)list))) =
   match nameTree with
   | hd::tl->(match hd with | (curkey,curval) -> if(key = curkey) then Some(curval)
                                                 else findVal(key,tl))
   | [] -> None

let rec funDef((instrucList:string list),(funlist:string list),(funread: int)):((string list)*(string list)) =
   match(instrucList) with
   |"funEnd"::tl -> if (funread =0) then (funlist,tl) else funDef(tl,funlist@["funEnd"],funread-1)
   | elm::tl ->(match (String.split_on_char ' ' elm) with| "fun"::itl -> funDef(tl,funlist@[elm],funread+1) | _ ->  funDef(tl,funlist@[elm],funread))
   | _  -> (funlist,instrucList)

(* function to convert constant to string*)      
let string_of_const  (inConst: const): string =
      match inConst with      
      |I i -> string_of_int i 
      |B b -> ":"^(string_of_bool b)^":"
      |Error -> ":error:"
      |U -> ":unit:"
      |S str ->  str
      |Name n -> n
      |_-> "stuff"

let rec isName ((str:string),(letterRead:bool),(index:int)):bool =    
    if(index == String.length str) then letterRead
    else if((str.[index])=='_') then isName(str,letterRead,(index+1))
    else if( ((str.[index]>='A') &&(str.[index] <= 'Z')) || ( (str.[index] >='a') && (str.[index] <= 'z')))
        then isName (str,true,(index+1))
    else if(letterRead && (str.[index]>='0' && str.[index] <= '9'))  then isName(str,letterRead,(index+1))
    else false

let string_Op f stack varstack=
  match stack with
  |(S s1)::(S s2)::tl ->(((f s1 s2)@tl),varstack)
  |(S s)::(Name n)::tl ->( match (findVal (n,varstack)) with Some(S s2) ->(((f s s2)@tl),varstack) | _ ->(Error::stack,varstack) )
  |(Name n)::(S s)::tl ->( match (findVal (n,varstack)) with Some(S s2) ->(((f s2 s)@tl),varstack) | _ ->(Error::stack,varstack) )
  |(Name n1)::(Name n2)::tl->( match ((findVal (n1,varstack)),(findVal (n2,varstack))) with (Some(S s1),Some(S s2)) ->(((f s1 s2)@tl),varstack) | _ ->(Error::stack,varstack))
  |_-> (Error::stack,varstack)

  let arith_Op f stack varstack =
   match stack with
   |(I i1)::(I i2)::tl ->(match (f i1 i2) with (I res) -> (I(res)::tl,varstack) |(B b)->(B(b)::tl,varstack) | _ -> (Error::stack,varstack))
   |(I i)::(Name n)::tl ->( match (findVal (n,varstack)) with Some(I i2) ->(match (f i i2) with (I res) -> (I(res)::tl,varstack)
                                                                                                 | (B b)->(B(b)::tl,varstack)
                                                                                                 | _ -> (Error::stack,varstack)) 
                                                              | _ ->(Error::stack,varstack) )
   |(Name n)::(I i)::tl ->( match (findVal (n,varstack)) with Some(I i2) ->(match (f i2 i) with (I res) -> (I(res)::tl,varstack)       
                                                                                                 |(B b)->(B(b)::tl,varstack)
                                                                                                 | _ -> (Error::stack,varstack))
                                                               | _ ->(Error::stack,varstack) )
   |(Name n1)::(Name n2)::tl->( match ((findVal (n1,varstack)),(findVal (n2,varstack))) with 
                                                (Some(I i1),Some(I i2)) ->(match (f i1 i2) with (I res) -> (I(res)::tl,varstack)
                                                                                                 |(B b)->(B(b)::tl,varstack)
                                                                                                 | _ -> (Error::stack,varstack))
                                                | _ ->(Error::stack,varstack) )
   |_-> (Error::stack,varstack)

  let bool_Op f stack varstack =
   match stack with
   |(B b1)::(B b2)::tl ->(((f b1 b2)@tl),varstack)
   |(B b)::(Name n)::tl ->( match (findVal (n,varstack)) with Some(B b2) ->(((f b b2)@tl),varstack) | _ ->(Error::stack,varstack) )
   |(Name n)::(B b)::tl ->( match (findVal (n,varstack)) with Some(B b2) ->(((f b2 b)@tl),varstack) | _ ->(Error::stack,varstack) )
   |(Name n1)::(Name n2)::tl->( match ((findVal (n1,varstack)),(findVal (n2,varstack))) with (Some(B b1),Some(B b2)) ->(((f b1 b2)@tl),varstack) | _ ->(Error::stack,varstack) )
   |_-> (Error::stack,varstack)


let bind(stack,varstack) =
      match stack with
      | Error::tl-> (Error::stack,varstack)
      | (Name from)::(Name dest)::tl->( match (findVal (from, varstack)) with
                                        |Some value->(U::tl,treeInsert(dest,value,varstack))  
                                        |None ->(Error::stack,varstack))
      | value::(Name n)::tl->(U::tl,treeInsert(n,value,varstack))
      | _ -> (Error::stack,varstack)

let do_Op (stack, varstack, instruction) = 
   match (String.split_on_char ' ' instruction) with 
   | instruct::[] ->(
     (*operation*)
        match instruct with
        |"pop"-> ( match stack with | []-> (Error::stack,varstack)| hd::tl -> (tl,varstack))
        |"swap"->(match stack with | x::y::tl -> (y::x::tl,varstack) | _ ->(Error::stack,varstack))
        |"add"->(arith_Op (fun x y -> I(x+y)) stack varstack)
        |"sub"->(arith_Op (fun x y -> I(y-x)) stack  varstack)
        |"mul"->(arith_Op (fun x y -> I(x*y) ) stack varstack)
        |"div"->(arith_Op (fun x y -> try I(y / x) with Division_by_zero -> Error)  stack varstack)
        |"rem"->(arith_Op (fun x y -> try I(y mod x) with Division_by_zero -> Error) stack varstack)
        |"neg"->(match stack with (I i)::tl->((I(i*(-1)))::tl,varstack)
                                  | (Name n)::tl-> (match findVal(n,varstack) with Some (I i1)->(I(i1* -1)::tl,varstack)
                                                                                   | _ -> (Error::stack,varstack) )
                                  | _ -> (Error::stack,varstack))
        |"equal"-> (arith_Op ( fun x y ->B(x=y) ) stack varstack)
        |"lessThan" -> (arith_Op( fun x y -> B(y<x)) stack varstack)                          
        |"cat" ->(string_Op  (fun x y -> [S(y^x)]) stack varstack)
        |"and" ->(bool_Op (fun x y -> [B(x && y)] ) stack varstack)
        |"or"->(bool_Op (fun x y -> [B(x||y)]) stack varstack)
        |"not"->(match stack with (B b)::tl->(B(not b)::tl,varstack)
                                 |(Name n)::tl->(match findVal(n,varstack) with Some (B b)->(B(not b)::tl,varstack)
                                                                                | _ -> (Error::stack,varstack) )
                                 | _-> (Error::stack, varstack))
        |"bind"->(bind (stack, varstack))
        |"if" -> (match stack with | x::y::B(b)::tl->(if b then (x::tl,varstack) else (y::tl,varstack))
                                   | x::y::Name(n)::tl ->(match findVal(n,varstack) with 
                                                          | Some(B b)-> (if(b) then (x::tl,varstack) else (y::tl,varstack))
                                                          | _ -> (Error::stack,varstack))
                                   | _ -> (Error::stack,varstack))                           
        |_ -> (Error::stack,varstack)
    )(*operations below all take once argurment and push them onto the stack*)
    | instruct::stackval::rest ->(
      match(instruct, stackval, rest) with
      |("pushb",":true:",[]) -> ((B(true))::stack,varstack)
      |("pushb",":false:",[]) -> ((B(false))::stack,varstack)
      |("pushi", stackval,[] ) -> (try ((I(int_of_string stackval))::stack, varstack) with Failure "int_of_string" -> ((Error::stack),varstack))
      |("pushs", stackval, rest ) ->(let sval=(List.fold_left (fun x y -> x^" "^y ) stackval rest) in 
                                     if( (sval.[0]=='"') && (sval.[(String.length sval)-1]='"') )
                                       then ((S(String.sub sval 1 ((String.length sval)-2)) ::stack ),varstack)
                                     else (Error::stack,varstack) )
      |("pushn", stackval,[])-> (if isName(stackval,false,0) then (Name(stackval)::stack,varstack) else (Error::stack,varstack))
      |("push", ":error:", [])-> ((Error)::stack,varstack)
      |("push", ":unit:", []) -> ((U)::stack,varstack)
      |(_,_ ,_)->(Error::stack,varstack)
    )
    | _ -> (stack,varstack)


let interpreter ((input:string) , (output:string)): unit=
    let ic = open_in input in
    let oc = open_out output in
    (* following code taken from homework 1   *)
    let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try
          let l = input_line ic in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in
 let instrucList   = loop_read []  in

    let rec iterateInstruct  (bigstack:((const list)*(string*const)list) list) (instructlist:string list): ((const list)*((string*const) list)) =
      match instructlist with
      | [] ->  (match bigstack with (stack,varstack)::valtl-> ([],varstack) | _ -> ([],[]) )
      | hd::tl->( match (String.split_on_char ' ' hd) with
                 |"quit"::itl -> (match bigstack with (stack,varstack)::valtl-> (stack,varstack) | _ -> ([],[]) )
                 |"let"::itl ->(match bigstack with (stack,varstack)::valtl-> (iterateInstruct (([],varstack)::bigstack) tl)| _ -> ([],[]) )
                 |"end"::itl ->
                    (match bigstack with 
                        |(stack,varstack)::valtl->  
                           (match stack with 
                              |stackTop::bottom ->
                                 (match valtl with
                                    | (nxtST,nxtvarSt)::list3 -> (iterateInstruct ((stackTop::nxtST,nxtvarSt)::list3) tl)
                                    | _ -> ([],[])) (*case that if valtl is empty, cant happen because end cannot occur without prior let  *)
                              | _ -> (iterateInstruct valtl tl)) (**if stack was empty then continue executing with valtl as current biglist *)
                        | _ -> ([],[]) ) (*bigstack can never be emptpy so never happens*) 
               
               | "fun"::funname::arg::itl->(match (funDef(tl,[],0)) with (funinstruc,remainingInstruct) ->
                   (match bigstack with 
                     | (stack,varstack)::valtl->
                        (iterateInstruct(((U::stack),(treeInsert(funname,Fun((arg::funinstruc),varstack, false),varstack)))::valtl) remainingInstruct)
                  |_-> ([],[])))
                  
                | "inOutFun"::funname::arg::itl->(match (funDef(tl,[],0)) with (funinstruc,remainingInstruct) ->
                   (match bigstack with 
                     | (stack,varstack)::valtl->
                        (iterateInstruct(((U::stack),(treeInsert(funname,Fun((arg::funinstruc),varstack, true),varstack)))::valtl) remainingInstruct)
                  |_-> ([],[])))

               | "call"::itl -> (print_string "here\n";match bigstack with (stack,varstack)::valtl ->
                                    (match stack with 
                                    | arg::(Name funname)::calltl -> 
                                         ( match (arg,findVal(funname, varstack)) with
                                          | (Error, _ ) ->( iterateInstruct ((Error::stack,varstack)::valtl) tl)
                                          | ( _  , None) ->( iterateInstruct ((Error::stack,varstack)::valtl) tl)
                                          | ( _ , Some func) -> (match func with | Fun (argname::funcInstruct,funvarstack, b) ->
                                                                                    (match(arg) with | Name n ->( match findVal(n, varstack) with 
                                                                                                                  | Some value ->(iterateInstruct  ((match (iterateInstruct (([],treeInsert(argname,value,funvarstack))::[[],[]]) funcInstruct )with
                                                                                                                                                      | (retval,retvarstack)-> (if(b) then (retval@calltl,(match findVal(argname,retvarstack) with
                                                                                                                                                                                 | Some newbind -> treeInsert((string_of_const (arg)),newbind,varstack)
                                                                                                                                                                                 | None -> varstack))
                                                                                                                                                                               else ((retval@calltl),varstack)  ))::valtl) tl)
                                                                                                                  | None -> (iterateInstruct ((Error::stack,varstack)::valtl) tl))
                                                                                                      | _ ->( iterateInstruct ( (match (iterateInstruct (([],treeInsert(argname,arg,funvarstack))::[[],[]]) funcInstruct ) with
                                                                                                                                    |(retval,retvarstack) ->(retval@calltl,varstack)) ::valtl) tl))
                                                                                  | _ ->   ( iterateInstruct ((Error::stack,varstack)::valtl) tl)))
                                    | arg::(Fun (argname::funcInstruct,funvarstack, b))::calltl ->( match arg with
                                                                                                   | Error ->( iterateInstruct ((Error::stack,varstack)::valtl) tl)
                                                                                                   | Name n -> (match findVal(n,varstack) with
                                                                                                                  | Some value -> (iterateInstruct ((match (iterateInstruct (([],treeInsert(argname,value,funvarstack))::[[],[]]) funcInstruct )with
                                                                                                                                                      | (retval,retvarstack)-> (if(b) then (retval@calltl,(match findVal(argname,retvarstack) with
                                                                                                                                                                                                         | Some newbind -> treeInsert((string_of_const (arg)),newbind,varstack)
                                                                                                                                                                                                         | None -> varstack))
                                                                                                                                                                     else ((retval@calltl),varstack)  ) ) ::valtl) tl)
                                                                                                                  | None  -> ( iterateInstruct ((Error::stack,varstack)::valtl) tl)   )
                                                                                                   | _ ->( iterateInstruct(    (match (iterateInstruct (([],treeInsert(argname,arg,funvarstack))::[[],[]]) funcInstruct ) with
                                                                                                                                    |(retval,retvarstack) -> (retval@calltl,varstack))::valtl) tl))
                                    | _->(iterateInstruct ((Error::stack,varstack)::valtl) tl))
                                |_-> ([],[]))
               
               | "return"::itl -> (match bigstack with (retval::stack,varstack)::valtl -> (
                                          match retval with Name n-> (match findVal(n,varstack) with 
                                                                     | Some (value)-> (value::[],varstack)
                                                                     | None -> (Error::[],varstack) )
                                           | value -> (value::[],varstack)  )
                                 | _ -> ([],[])) 
               | _ -> (match bigstack with (stack,varstack)::valtl-> ( iterateInstruct ((do_Op (stack,varstack, hd))::valtl) tl) | _ -> ([],[]) ) 
      )
      
   in
   let stackpair= iterateInstruct [([],[])] instrucList in
   let getstack (x,y) = x in
   let finalStack = getstack stackpair in
   close_in ic;
   List.iter (fun x -> Printf.fprintf oc "%s\n" (string_of_const x)) finalStack;
   close_out oc  
;;


let _ = interpreter("part3/input/input25.txt","part3/myoutput/test25.txt")