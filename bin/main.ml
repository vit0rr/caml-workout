(* write an function that multiplies 42 by 10 *)
let () = 42 * 10 |> string_of_int |> print_endline

(* Write an function that divides 3.14 by 2.0 *)
let () = 3.14 /. float_of_int 2 |> string_of_float |> print_endline

(* Write an function that computes 4.2 raised to the seventh power *)
let () = 4.2 ** 7.0 |> string_of_float |> print_endline

(* Write an function that compares 42 to 42 using structural equality. *)
let () = 42 = 42 |> string_of_bool |> print_endline

(* Write an function that compares "hi" to "hi" using structural equality *)
let () = "hi" = "hi" |> string_of_bool |> print_endline

(* Write an function that compares "hi" to "hi" using physical equality. *)
let () = "hi" == "hi" |> string_of_bool |> print_endline

(* Write an if expression that evaluates to 42 if 2 is greater than 1 and otherwise evaluates to 7. *)
let () =
  if 2 > 1 then 42 |> string_of_int |> print_endline
  else 7 |> string_of_int |> print_endline

let double x = x * 2
let () = double 7 |> string_of_int |> print_endline
let cube_float x = x ** 3.0
let () = cube_float 2.5 |> string_of_float |> print_endline
let sign x = if x > 0 then 1 else if x = 0 then 0 else -1
let () = sign 0 |> string_of_int |> print_endline
let () = sign 1 |> string_of_int |> print_endline
let () = sign (-1) |> string_of_int |> print_endline
let rms x y z = sqrt ((x *. x) +. (y *. y) +. (z *. z)) /. 2.
let () = rms 3.0 4.0 5.0 |> string_of_float |> print_endline

let is_date_valid d m : bool =
  let months_with_31_days =
    [ "Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec" ]
  in
  let months_with_30_days = [ "Apr"; "Jun"; "Sep"; "Nov" ] in

  let months_with_28_days = [ "Feb" ] in
  (List.mem m months_with_31_days && d > 0 && d <= 31)
  || (List.mem m months_with_30_days && d > 0 && d <= 30)
  || (List.mem m months_with_28_days && d > 0 && d <= 28)

let () = is_date_valid 31 "Jan" |> string_of_bool |> print_endline
let () = is_date_valid 31 "Feb" |> string_of_bool |> print_endline
let () = is_date_valid 31 "Mar" |> string_of_bool |> print_endline

(* with pattern matching *)
let is_date_valid_pm d m =
  match m with
  | "Feb" -> 1 <= d && d <= 28
  | "Apr" | "Jun" | "Sep" | "Nov" -> 1 <= d && d <= 30
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 1 <= d && d <= 31
  | _ -> false

let () = is_date_valid_pm 31 "Jan" |> string_of_bool |> print_endline
let () = is_date_valid_pm 31 "Feb" |> string_of_bool |> print_endline
let () = is_date_valid_pm 31 "Mar" |> string_of_bool |> print_endline

let rec fib n =
  if n = 1 then 1 else if n = 2 then 1 else fib (n - 1) + fib (n - 2)

let () = fib 5 |> string_of_int |> print_endline

let fib_fast n =
  let rec fib_helper a b count =
    if count = 0 then a else fib_helper b (a + b) (count - 1)
  in
  fib_helper 0 1 n

let () = fib_fast 50 |> string_of_int |> print_endline
