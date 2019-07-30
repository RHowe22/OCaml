(* printer definitions DO NOT MODIFY THESE *)
type printable = B of bool   | U |
                 S of string | L of int list | P of printable * printable

let ex0 = B true
let ex1 = S "Schrute bucks"
let ex2 = U
let ex3 = L [1; 8; 5; 0; 3]
let ex4 = P (P (U, P (P (U, L [1; -5; 13]), P (L [0], B true))), S "Hello")
let ex5 = P (P (B true, P (P (L [-21; 53; 12], S "c"), P (L [0], B false))), S "Hello")
let ex6 = P (P (U, P (P (L [15; -15; 213], S "c"), P (P (U, U), S "false"))), S "Hello")
let ex7 = P (S "Stanley nickels", L [1000000])

(* Your code begins here *)
let rec count_u (p: printable) : int =
    match p with
    | U  -> 1
    | P(x,y) -> (count_u x) + (count_u y)
    | _ -> 0

let rec global_or (p : printable) : bool option =
    match p with
    | B b -> Some b
    | P (x,y) -> (match (global_or (x), global_or (y)) with
        | (Some lval, Some rval) -> Some (lval || rval)
        | (Some lval , None) -> Some lval
        | (None, Some rval ) -> Some rval
        | _ -> None)
    | _ -> None

let rec f_on_int_list (f : int-> int) (p : printable) : printable =
  match p with
  | L ls -> L (List.map f ls)
  | P (x,y) -> P(f_on_int_list(f)(x), f_on_int_list(f)(y))
  | B b -> B b
  | U -> U
  | S s -> S s

let rec sum_all_ints (p : printable) : int option =
  match p with
  | L ls -> (let rec sum lis =
      match lis with
      | [] -> None
      | hd::tl ->(  match (sum tl) with
          | Some tlval -> Some(tlval+hd)
          | None -> Some(hd) )
      in sum ls )
  | P (x,y) -> (let left =sum_all_ints(x) in
      let right = sum_all_ints(y) in
      match (left ,right) with
      | (Some lval, Some rval) -> Some (lval + rval)
      | (Some lval , None) -> Some lval
      | (None, Some rval ) -> Some rval
      | (None, None) -> None)
  | _ -> None

let rec tostring (p : printable) : string =
  match p with
  | B b -> string_of_bool b
  | S s -> s
  | U -> "U"
  | P (x,y) -> (tostring(x))^(tostring(y))
  | L ls ->( let rec string_of_intlist lis =
      match lis with
      | [] -> ""
      | hd::tl-> (string_of_int hd)^(string_of_intlist tl) in
      string_of_intlist ls
)
;;
