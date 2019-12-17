(************************ Problem #2 **************************)

(*
- this function takes it in two lists and returns the product 
of the first elements of the lists plus the product of the 
second elements plus the product of n elements until the lists are
empty
*)
let rec product l1 = function
| [] -> failwith "Matrix has no elements."
| [[]] -> 0
| [x] -> (List.head l1) * (List.head x)
|  x::xs -> 
    (List.head l1) * (List.head x) + product (List.tail l1) xs;;

(*
- this functions takes it two list of lists where the first represents the row
and the second represents the column to be multiplied
- returns the whole row (list) of the matrix multiplication
*)
let rec getRow l1 l2 =
    match (l1, l2) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> 
        if (List.length y) = 1 then [product x (y::ys)]
        else (product x (y::ys)) :: getRow (x::xs) (List.map List.tail (y::ys));;

(*
- this function takes it in the two matrices and returns the matrix product
- it does so by finding the first product row and adding it to the matrix, which
is solved recursively
*)
let rec multiplyhelp l1 l2 =
    match (l1, l2) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> 
        if (List.length (x::xs)) = 1 then [getRow (x::xs) (y::ys)]
        else getRow (x::xs) (y::ys) :: multiplyhelp xs (y::ys);;

(*
- this function only verifies that the two matrices inputted are 
eligible to be multiplied bc the number of rows of the first matrix
must be equal to the columns of the second matrix, therefore the order
is required
- once verified, it calls the function that actually returns the result
*)
let validate l1 l2 =
    if (List.length (List.head l1)) <> (List.length l2) 
    then failwith "Matrices cannot be multiplied."
    else multiplyhelp l1 l2;;

(*
- matrix multiplication
- takes two int list list
- returns int list, which is the matrix product
*)
let multiply l1 l2 = validate l1 l2;;

// Testing and Printing Calculations
let (x1, x2) = ([[2; 5; 7]; [8; 1; 0]], [[3; 4]; [1; 2]; [7; 10]]);;
let (y1, y2) = ([[3; 4]; [5; 7]; [1; 0]], [[5; 2; 1; 3]; [6; 1; 3; 4]]);;
let (z1, z2) = ([[10; 23; 4; 6; 7; 12]], [[2; 7]; [1; 6]; [3; 0]; [4; 5]; [0; 0]; [3; 2]]);;
let rx = multiply x1 x2;;
let ry = multiply y1 y2;;
let rz = multiply z1 z2;;

printfn "The matrix multiplication of %A and %A is %A" x1 x2 rx;;
printfn "The matrix multiplication of %A and %A is %A" y1 y2 ry;;
printfn "The matrix multiplication of %A and %A is %A" z1 z2 rz;;



