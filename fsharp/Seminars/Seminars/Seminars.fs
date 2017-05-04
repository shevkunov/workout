
open System
open System.Numerics
open System.Drawing
open System.Windows.Forms

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let rec rpt n f =
    if n = 0 then fun x->x
    else f >> rpt (n-1) f



let mandelf (c:Complex) (z:Complex) = z * z + c

let ismandel c =
    let mandelf' = mandelf c
    let m20 = rpt 20 mandelf'
    let zn = m20 Complex.Zero
    Complex.Abs(zn) < 1.0

let scale (x:float, y:float) (u,v) n = 
    float(n - u) / float(v - u) * (y - x) + x

let form = 
    let image = new Bitmap(400, 400)
    let lscale = scale(-1.2, 1.2) (0, image.Height - 1)
    for i=0 to (image.Height-1) do
        for j=0 to (image.Width-1) do
            let t = new Complex(lscale i, lscale j) in
            image.SetPixel(i, j, if ismandel t then Color.Black else Color.White)
            
    let temp = new Form()
    temp.Paint.Add(fun e -> e.Graphics.DrawImage(image, 0, 0))
    temp.Show()
    temp

(*
let rec range b e = 
    if b < e then range (b + 1) e (printfn "%d" b)
    else printfn "%d" b
*)
   
let rec take n l =
    if n = 0 then [] else (List.head l) :: (take (n - 1) (List.tail l))

let y::ys = [1..5] 

let mn = List.min ['a'..'z']
let x = List.exists (fun x -> x = 2) [1..10]

let rec sum list =
    let y::ys = list
    if ys = [] then y
    else y + (sum ys)

let rec sum xs = 
    match xs with
    | [] -> 0
    | y::ys -> y + sum ys


let rec max xs = 
    match xs with
    | [] -> int(infinity)
    | y::ys -> y + sum ys


let rec gen n =
    [1..n] |> List.map (fun x -> [x+1 .. x+n]) |> List.concat
// gen 3


// 11.03.17

let rec rem a b = 
    if (a < b) then a
    else rem (a - b) b

let quot a b =
    let rec quot' a b v = 
        if (a < b) then v
        else quot' (a - b) b (v + 1)
    quot' a b 0

// lec

let rem a b = 
    let rec rem' a b =
        match (a-b) with
        | c when c < 0 -> a
        | _ -> rem' (a - b) b
    sign a * sign b * rem' (abs a) (abs b)

//gcd

let max a b =
    if a > b then a
    else b

let min a b =
    if a < b then a
    else b

let gcd a b = 
    let rec gcd' a b =
        if (b = 0) then a
        else gcd' b (a % b)
    gcd' (max a b) (min a b)


let gcd a b = 
    let rec gcd' a b =
        if (b = 0) then a
        else gcd' b (a % b)
    gcd' (max a b) (min a b)

let replicate n v = 
    let rec replicate' n v l =
        if (n > 0) then replicate' (n-1) v  (v::l) 
        else l
    replicate' n v []

replicate 3000000 'a' // stack may crash here, but tail recursion is here

List.replicate 30000000 'a' // so much memory
Seq.replicate 30000000 'a' // OK


let rec zip l1 l2 =
    let l1f :: l1s = l1
    let l2f :: l2s = l2 
    if ((l1 = []) || (l2 = [])) then []
    else (l1f, l2f) :: (zip l1s l2s)

let rec zip a b =
    match (a, b) with 
    | (x::xs, y::ys) -> (x, y) :: (zip xs ys)
    | _ -> []

let rec append a b =
    match a with
    | [] -> b
    | (x :: xs) -> x :: append xs b


let reverse l =
    let rec rev' l1 l2 =
        match l1 with
        | [] -> l2
        | (x :: xs) -> rev' xs (x::l2)
    rev' l []


let reverse<'a> =
    let rec rev' l2 l1 =
        match l1 with
        | [] -> l2
        | (x :: xs) -> rev' (x::l2) xs
    rev' []

let fib n =
    let rec fib' n acc1 acc2 = 
        if n = 0 then acc1
        else fib' (n-1) (acc1 + acc2) acc1;
    fib' n 1 0

let explode (s:string) = [for c in s -> c]
explode "sfsdaf"

let rec exp (s:string) l n =
    exp s ((n, s.Chars n) :: l) (n-1)

let rec delete char  =
    function
    | [] -> []
    | head::tail when head = char -> delete char tail
    | x::xs -> (x :: (delete char xs))


// that works, but reversing
let delete char list = 
    let rec delete' char acc = function
        | [] -> acc
        | (x :: xs) when x=char -> delete' char acc xs
        | (x :: xs) -> delete' char (x::acc) xs
    delete' char [] list

let substitude char replace list = 
    let rec substitude' acc = function
        | [] -> acc
        | (x :: xs) when x=char -> substitude' (acc@[replace]) xs
        | (x :: xs) -> substitude' (acc@[x]) xs
    substitude' [] list

// bracket balance
let bal list =
    let rec bal' acc list =
        match list with
         | (x :: xs) when x = '(' -> bal' (acc + 1) xs
         | (x :: xs) when x = ')' -> bal' (acc - 1) xs
         | (x :: xs) -> bal' acc xs
         | _ -> acc
    bal' 0 []

let isball list = 
    let rec isbal' acc list =
        if acc < 0 then false
        else match list with
             | [] -> acc = 0
             | x :: xs when x = '(' -> isbal' (acc+1) xs
             | x :: xs when x = ')' -> isbal' (acc-1) xs
             | x :: xs -> isbal' acc xs
    isbal' 0 list

/// 25.03.2017

let elem a l =
    let rec elem' l n =
        match l with
        | l :: ls when (l = a) -> n
        | l :: ls -> elem' ls (n + 1)
        | _ -> -1
    elem' l 0

let uniq l =
    let rec insert item set = 
        match set with
        | s :: set when (s = item) -> (set @ [s])
        | s :: set -> ((insert item set) @ [s] )
        | _ -> [item]
    let rec uniq' l = 
        match l with 
        | l :: ls -> insert l (uniq' ls)
        | _ -> l
    uniq' l

// можно было использовать elem для проверки наличия

let freq l =
    let rec insert item set = 
        match set with
        | (s, f) :: set when (s = item) -> ((s, f + 1) :: set)
        | (s, f) :: set -> ((s, f) :: (insert item set) )
        | _ -> [(item, 1)]
    let rec freq' l = // надо было с аккумулятором
        match l with  
        | l :: ls -> insert l (freq' ls)
        | _ -> []
    freq' l

 
let freq l =
    let rec insert item set = 
        match set with
        | (s, f) :: set when (s = item) -> ((s, f + 1) :: set)
        | (s, f) :: set -> ((s, f) :: (insert item set) )
        | _ -> [(item, 1)]
    let rec freq' l acc = // норм
        match l with  
        | l :: ls -> freq' ls (insert l acc)
        | _ -> acc
    freq' l []


let zipWith f l1 l2 =
    let rec zipWith' l =
        match l with
        | (a, b) :: ls -> (f a b) :: zipWith' ls
    zipWith' (zip l1 l2)


let rec zipWith f a b =
    match (a, b) with 
    | (x::xs, y::ys) -> (f x y) :: (zipWith f xs ys)
    | _ -> []


let rec zipWith f a b acc =
    match (a, b) with 
    | (x::xs, y::ys) -> zipWith f xs ys (acc @ [(f x y)])
    | _ -> acc

let rec map f = function
  | [] -> []
  | (x::xs) -> f x :: map f xs

map (fun x -> x + 3) [1;5;3;1;6] 
map (replicate 3) [3..6]
map (map (fun x -> x ** 2.)) [[1.;2.];[3.;4.;5.;6.];[7.;8.]]
map fst [(1,2);(3,5);(6,3);(2,6);(2,5)]
map fib [0..10]

let rec filter f = function
  | [] -> []
  | (x::xs) when f x -> x :: filter f xs
  | (x::xs) -> filter f xs

filter (fun x -> x > 3) [1;5;3;2;1;6;4;3;2;1]
filter (fun x -> x = 3) [1;2;3;4;5]
filter (fun x -> x <> []) [[1;2;3];[];[3;4;5];[2;2];[];[];[]]
filter (fun x -> List.exists (fun y -> x = y) ['a'..'z']) (explode "u LaUgH aT mE BeCaUsE I aM diFfeRent")
filter (fun x -> List.exists (fun y -> x = y) ['A'..'Z']) (explode "i lauGh At You BecAuse u r aLL the Same")

let tr n = 
    n * (n + 1) / 2

let rec pr acc n = 
    if n <= 0 then acc
    else pr (acc + (tr n)) (n - 1)

let Tn n = map tr [1..n]
let Pn n = map (pr 0) [1..n]


let rec foldr f a list =
  match list with
  [] -> a
  | h::t -> f h (foldr f a t)


let f l = foldr (min) (List.item 0 l) l

let rec mapfoldr f l = foldr (fun x a -> (f x) :: a) []

let rec foldr1 f list =
    match list with
    | [a] -> a
    | x::xs -> f x (foldr1 f xs)


List.foldBack (+) [1..6] 0 /// !

List.fold (-) 0 [1..6] /// !

let foldl f a list =
  let rec loop a = function
    | [] -> a
    | h::t -> loop (f a h) t
  loop a list

let rec mapfoldl f list = foldl (fun x a -> (f x)::a) [] list 

List.scan (+) 0 [1..6]


let reverse l =
    let rec rev' l1 l2 =
        match l1 with
        | [] -> l2
        | (x :: xs) -> rev' xs (x::l2)
    rev' l []

let rec myscan f start list =
    let rec myscan' list acc = 
        match list with
        | [] -> acc
        | (x::xs) -> match acc with
                     | (a::acs) -> myscan' xs ((f a x) :: (a :: acs))
    reverse (myscan' list [start])


let rec myscanback f start list =
    let rec myscan' list acc = 
        match list with
        | [] -> acc
        | (x::xs) -> match acc with
                     | (a::acs) -> myscan' xs ((f x a) :: (a :: acs))
    myscan' (reverse list) [start]


let rec scan f u list = //good
    u ::
    (match list with
     | [] -> []
     | x::xs -> scan f (f u x) xs)
    

let rec scanback f u list =
    match list with
    | (l::ls) -> 
                let lback = scanback f u ls
                (f l lback.Head) :: lback
    | _ -> [u]


(* Генерация списков *)
let list1 = [1 .. 10]       // val it : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let list2 = [1 .. 2 .. 10]  // val it : int list = [1; 3; 5; 7; 9]
let list3 = ['a' .. 'g']    // val it : char list ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g';]

// Ключевая конструкция yield или -> для генерации списков
let list4 = [ for a in 1 .. 10 do yield (a * a) ] // val it : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

// Несколько элементов
let list5 = 
  [ for a in 1 .. 3 do
      for b in 3 .. 7 do
        yield (a, b) ]
// val it : (int * int) list = [(1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7); (3, 3); (3, 4); (3, 5); (3, 6); (3, 7)]

// Синтаксический сахар для замены do-yield: ->
let list6 = 
  [for a in 1..3 do
    for b in 4..6 ->
      (a, b) ]

// генерация списка с условием
let list7 = [ for a in 1 .. 100 do 
                if a % 3 = 0 && a % 5 = 1 then yield a]
// val it : int list = [6; 21; 36; 51; 66; 81; 96]

// для любых перечислимых типов
let list8 = [for a in ['a'.. 'f'] do yield [a; a; a] ]
// val it : char list list = [['a'; 'a'; 'a']; ['b'; 'b'; 'b']; ['c'; 'c'; 'c']; ['d'; 'd'; 'd']; ['e'; 'e'; 'e']; ['f'; 'f'; 'f']]

// yield! используется для генерации одновременно нескольких элементов
let list9 = [for a in 1 .. 5 do yield! [ a .. a + 3 ] ] // val it : int list = [1; 2; 3; 4; 2; 3; 4; 5; 3; 4; 5; 6; 4; 5; 6; 7; 5; 6; 7; 8]

// для генерации списка можно использовать различные возможности языка
let list10 = 
  [
    let thisIsIt = "!"
    for a in 1 .. 5 do
      match a with
      | 3 -> yield! ["hello"; "world"; thisIsIt] // will be appended
      | _ -> yield a.ToString()
  ]
// val it : string list = ["1"; "2"; "hello"; "world"; "!"; "4"; "5"]


/// LECTION 06.
type 't tree = Nil | Node of 't * 't tree * 't tree
let Leaf x = Node(x, Nil, Nil)
let t = Node(1, Node(2, Nil, Leaf(3)), Leaf(4))

let rec size t acc cont = match t with
    | Nil -> cont acc
    | Node(_, L, R) -> size L (1+acc) (fun sL -> size R sL cont)

let TreeSize t = size t 0 (fun x->x)


let derivative (dx:float) f = fun x -> (f(x+dx)-f(x))/dx
let deriv = derivative 0.01

let x = 1
let f z = x + z // f 5 -> 6
let x = 2 // f 5 -> 6


let mutable x = 1
let f z = x + z // f 5 -> 6
x <- 2 // f 5 -> 7


// Generators

let new_counter n = 
    let mutable x = n in
    fun () ->
        x <- x+1; x

let c = new_counter 0 // try c();; c();;


let generator f n = 
    let mutable x = n in
    fun () ->
        x <- f x; x

let numbers = generator ((+) 1) 0

let fib = generator (fun (u, v) -> (v, u+v)) (1,1)
let map f gen = fun () -> f(gen())

let fibs = fib |> map fst

let rec filter p g = 
    fun () ->
        let x = g()
        if p x then x
        else (filter p g)() 

let fibs3 = fibs |> filter (fun x-> x%3 = 0)

/// SEQ

let fib = Seq.unfold (fun (u, v) -> Some(u, (u+v, u))) (1,1)

// [1..100] |> List.map ((*)2)
// {1..100} |> Seq.map ((*)2) |> Seq.toList

let fib = Seq.unfold (fun (u, v) -> Some(u, (u+v))) (1, 1)
fib |> Seq.filter (fun x-> x%3 = 0)

let numbers = Seq.unfold (fun x -> Some(x, x+1)) 1
let numbers = seq {
    let mutable x = 1
    while true do
        yield x
        x <- x+1
}


let ones = seq {
    while true do yield 1
}

let rec ones = seq {
    yield 1
    yield! ones /// wow
}

let rec numbers = seq {
    yield 1
    yield! Seq.map ((+)1) numbers
}

let fact n = seq {1..n} |> Seq.reduce (*) //***)
let fact = (..) 1 >> Seq.reduce (*) //***)

open System.IO
let ReadLines fn = 
    seq { use inp = File.OpenText fn in
          while not(inp.EndOfStream) do
            yield (inp.ReadLine()) }

ReadLines @"input.txt"
|> Seq.map (fun s->s.Split([|' '; '!'; ','|]))
|> Seq.filter (fun s -> s.Length > 0)

open FSharp.Charting

ReadLines @"input.txt"
|> Seq.collect (fun s->s.Split([|' '; '!'; ','|]))
|> Seq.filter (fun s -> s.Length > 0)
//|> Seq.map (fun s -> s.ToLower())
|> Seq.groupBy (fun x-> x)
|> Seq.map (fun (u, s) -> (u, Seq.length s))
|> Seq.sortByDescending snd
|> Chart.Bar

let flip f x y = f y x
let readset f = 
    ReadLines f
    |> Seq.map (fun s->s.Trim())
    |> Seq.fold (flip Set.add) Set.empty
let pos = readset @"file1"
let neq = readset @"file1"
let wt (s:string) =
    s.Split([|' '; '!'; ','|])
    |> Seq.fold (fun acc s -> if Set.contains s pos then acc+1
                              else if Set.contains s neg then acc-1
                              else acc) 0

ReadLines @"text"
|> Seq.maxBy wt // gooddest sentense

//
//ReadLines @"text"
//|> Seq.filter(fun s->s<> "")
//|> Seq.scan (fun (acc,_) s -> if (s.Contains("CHAPTER")) then (s,s) else (acc, "")) (0, "")
//|> Seq.filter (fun (c,_) -> c <> "")
//|> Seq.groupBy fst
//|> Seq.map (fun (c, s) -> (c, s |> Seq.map snd |> Seq.map wt |> Seq.sum))
//|> Chart.Bar

// 08.04.17

let rec cc coins = function  // TODO
    | 0 -> 1
    | n when n < 0 -> 0
    | n -> match coins with
           | [] -> 0
           | x :: xs -> (cc coins (n - x)) +
                            + (cc xs n)


type Anniversary =
 | Birthday of string * int * int * int
 | Wedding of string * string * int * int * int
 | Death of string * int * int * int

Birthday ("someone", 2014, 5, 4)
// Birthday "someone" 2012 11 7
let today = Birthday ("someone", 2014, 5, 4)
// today
// Birthday "someone" 2012 11 7


let (kurtCobain : Anniversary) = Birthday ("Kurt Cobain", 1967, 2, 20)
let (kurtWedding : Anniversary) = Wedding ("Kurt Cobain", "Courtney Love", 1990, 1 ,12)
let anniversaries = [
    kurtCobain;
    kurtWedding;
    Death ("Kurt Cobain", 1994, 4, 5)
]


let showAnniversary = function
  Birthday (name, year, month, day) -> name + " born " + showDate year month day // синеньким
  | Wedding (name1, name2, year, month, day) ->
      name1 + " married " + name2 + " on " + showDate year month day
  | Death (name, year, month, day) -> name + " dead in " + showDate year month day

let who = function
 Birthday (him, _, _, _) -> him 
 | Wedding (him, _, _, _, _) -> him
 | Death (him, _, _, _) -> him

 let print_ann list n =
    let (l::ls) = list
    printf "%d) %s\n" n (showAnniversary l)
    print_ann ls (n+1)

Seq.zip (Seq.initInfinite (fun x -> x + 1)) (seq <| List.map showAnniversary anniversaries)
|> Seq.iter (fun (n, x) -> printf "%d %s\n" n x )


type Point = { x  : float; y : float }
              
let a = { x = 13.22 ; y = 8.99 }
let b = { a with y = 666.13 }
let absPoint a = sqrt (a.x*a.x + a.y*a.y)

Some 1
None
Some "str" // type?
Some 42
Some 42 :: [None]  // ?
Some 42 :: [Some "str"; None] // ?



type Set = int -> bool


let (a:Set) = (fun a -> true)

let contains (s:Set) (a:int) = s a
contains a 1

let singletonSet b = fun a -> a = b
let singletonSet (b:int) = fun (a:int) -> a = b
let singletonSet b = fun (a:int) -> a = b
let singletonSet (b:int) = fun a -> a = b
let singletonSet = fun b -> fun (a: int) -> a = b
let singletonSet b =
  let answer a = a=b
  in (answer:Set)

let b = singletonSet 5
let c = singletonSet 6
contains b 5
contains b 6

let rec union s1 s2 =
    fun x ->
        (s1 x) || (s2 x)

let rec intersect s1 s2 =
    fun x ->
        (s1 x) && (s2 x)

let rec diff s1 s2 =
    fun x ->
        (s1 x) && (!(s2 x))

contains (intersect b c) 5





type 'a Tree =
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec treeInsert x = function
  EmptyTree -> singleton x 
  | Node (a, left, right) -> 
    if x = a then Node (x, left, right) 
    else 
      if x < a then Node (a, (treeInsert x left), right) 
      else Node (a, left, (treeInsert x right))
// when 'a : comparsion

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list

// list2tree через fold


let flip f y x = f x y

let list2tree list = 
    List.fold (flip treeInsert) EmptyTree list

list2tree [12; 1; 6; 4; 90; 9]

let rec tree2list tree = 
    match tree with
    | EmptyTree -> []
    | Node (v, ltree, rtree) ->
        (tree2list ltree) @ (v :: tree2list rtree)

let treesort x = x |> list2tree |> tree2list

treesort [12; 1; 6; 4; 90; 9]

list2tree [12; 12; 12; 13; 13; 14]



type 'a Tree =
  EmptyTree
  | Node of 'a * int * 'a Tree * 'a Tree

let singleton x = Node (x, 1, EmptyTree, EmptyTree)  

let rec treeInsert x tree = 
    match tree with
    | EmptyTree -> singleton(x)
    | Node (a, cnt, ltree, rtree) ->
        if (a = x) then Node (a, cnt + 1, ltree, rtree)
        else
            if (x < a) then Node(a, cnt, (treeInsert x ltree), rtree)
            else Node(a, cnt, ltree, (treeInsert x rtree))

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list


list2tree [12; 1; 6; 4; 90; 9]

let rec tree2list tree = 
    match tree with
    | EmptyTree -> []
    | Node (v, 1, ltree, rtree) ->
        (tree2list ltree) @ (v :: tree2list rtree)

let treesort x = x |> list2tree |> tree2list

treesort [12; 1; 6; 4; 90; 9]

list2tree [12; 12; 12; 13; 13; 14]




type 'a Tree =
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec treeInsert x = function
  EmptyTree -> singleton x 
  | Node (a, left, right) -> 
    if x = a then Node (x, left, right) 
    else 
      if x < a then Node (a, (treeInsert x left), right) 
      else Node (a, left, (treeInsert x right))
// when 'a : comparsion

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list

let rec tree2list tree = 
    match tree with
    | EmptyTree -> []
    | Node (v, ltree, rtree) ->
        (tree2list ltree) @ (v :: tree2list rtree)



 let rec foldTree treeFunction listValue tree =
     match tree with
     | EmptyTree -> listValue
     | Node(a, ltree, rtree) ->
         treeFunction a (foldTree treeFunction listValue ltree) (foldTree treeFunction listValue rtree)


let sumTree = foldTree (fun x left right -> x + left + right) 0
[2;7;4;3;5;8] |> list2tree |> sumTree


let getMax tree nan = 
    let pick = function
        | EmptyTree -> nan
        | Node(a, ltree, tree) -> a
    foldTree (fun x l r -> max (max x l) r) (pick tree) tree
 
   


 let rec foldTree treeFunction listValue tree =
     match tree with
     | EmptyTree -> listValue
     | Node(a, ltree, rtree) ->
         treeFunction a (foldTree treeFunction listValue ltree) (foldTree treeFunction listValue rtree)


let getMax tree : int option = 
    match tree with
    | EmptyTree -> None
    | Node(a, ltree, rtree) ->
        Some(foldTree (fun x l r -> max (max x l) r) a tree)
 

let treeFlip tree =
    foldTree (fun x l r -> Node (x, r, l)) EmptyTree tree
     

let rec compareTree a_tree b_tree : bool =
    match (a_tree, b_tree) with
    | (EmptyTree, EmptyTree) -> true
    | (EmptyTree, Node(b, b_ltree, b_rtree)) -> false
    | (Node(a, a_ltree, a_rtree), EmptyTree) -> false
    | (Node(a, a_ltree, a_rtree), Node(b, b_ltree, b_rtree))
        -> and (compareTree a_ltree b_ltree) (compareTree a_rtree b_rtree)



 
/// 29.04


type 'a Tree =
  | EmptyTree
  | Leaf of 'a
  | Node of 'a * 'a Tree list

Node(1, [Leaf 3; EmptyTree; Node(2, [])])

// найти высоту дерева, EmptyTree не считаются

let height tr =
    let rec htree t =
        let rec max_height list = 
            match list with
            | [] -> 0
            | h :: t -> max (htree h) (max_height t)
        match t with 
        | EmptyTree -> 0
        | Leaf(_) -> 1
        | Node(_, []) -> 1
        | Node(_, list) -> 1 + max_height list
    htree tr



// map cannot be implemented

type FileInfo = {name:string; fileSize:int}
type DirectoryInfo = {name:string; dirSize:int}

type FileSystemItem = Tree<FileInfo,DirectoryInfo>

let fromFile (fileInfo:FileInfo) = 
    LeafNode fileInfo 

let fromDir (dirInfo:DirectoryInfo) subitems = 
    InternalNode (dirInfo,subitems)

let readme = fromFile {name="readme.txt"; fileSize=1}
let config = fromFile {name="config.json"; fileSize=2}
let build  = fromFile {name="build.sh"; fileSize=3}
let src = fromDir {name="src"; dirSize=10} [readme; config; build]
let bin = fromDir {name="bin"; dirSize=10} []
let root = fromDir {name="root"; dirSize=5} [src; bin]

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.fileSize
    let fDir acc (dir:DirectoryInfo)= 
        acc + dir.dirSize
    fold fFile fDir 0 fileSystemItem 

readme |> totalSize  
src |> totalSize     
root |> totalSize    

// largestFile : fileSystemItem:Tree<FileInfo,'a> -> FileInfo option
let largestFile fileSystemItem =
    let fFile (acc:Option<FileInfo>) (file:FileInfo) : Option<FileInfo> =
        if (acc <> None) then
                if (acc.Value.fileSize > file.fileSize) then
                        acc
                    else
                        Some(file)
            else
                Some(file)
    let fDir (acc:FileInfo Option) (dir:DirectoryInfo) = 
        acc
    fold fFile fDir None fileSystemItem 





open System
open System.IO

DirectoryInfo("/home/und/fsharp")

type FileSystemTree = Tree<FileInfo,DirectoryInfo>

let fromFile (fileInfo:FileInfo) = 
    LeafNode fileInfo 

let rec fromDir (dirInfo:DirectoryInfo) = 
    let subItems = seq {
        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
    }
    InternalNode (dirInfo,subItems)

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.Length
    let fDir acc (dir:DirectoryInfo)= 
        acc 
    fold fFile fDir 0L fileSystemItem 
   
let currentDir = fromDir (DirectoryInfo("/home/shevkunov/code"))

currentDir |> totalSize  

let largestFile fileSystemItem =
    let fFile (largestSoFarOpt:FileInfo option) (file:FileInfo) = 
        match largestSoFarOpt with
        | None -> 
            Some file                
        | Some largestSoFar -> 
            if largestSoFar.Length > file.Length then
                Some largestSoFar
            else
                Some file

    let fDir largestSoFarOpt dirInfo = 
        largestSoFarOpt

    fold fFile fDir None fileSystemItem

currentDir |> largestFile  


////
(**************
type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) : 'r = 
    let recurse = fold fLeaf fNode  
    match tree with
    | LeafNode leafInfo -> 
        fLeaf acc leafInfo 
    | InternalNode (nodeInfo, subtrees) -> 
        Seq.fold recurse (fNode acc nodeInfo) subtrees 

*********)

let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) : Tree<'LeafData, 'INodeData> = 
    let recurse = map fLeaf fNode  
    match tree with
    | LeafNode leafInfo -> 
        LeafNode(fLeaf leafInfo) 
    | InternalNode (nodeInfo, subtrees) -> 
        InternalNode ( (fNode nodeInfo), Seq.map recurse subtrees )

let rec map fFile fDir (tree:Tree<'LeafData, 'INodeData>) =
    match tree with
    |    LeafNode a -> LeafNode (fFile a)
    | InternalNode (a, subtrees) ->
        InternalNode (fDir a, Seq.map (map fFile fDir) subtrees)

///


//// JSON

let json = """
{
    "a":"bvgd",
    "c":{
        "d":[1,2]
        },
    "e":false
    "f":null
}
"""

open System 
let explode (s:string) = [for i in s -> i]
type Token = OpenBrace | CloseBrace | OpenBracket | Colon | Comma
            | Null | True | String of string | Number of int | Boolean of bool


let tokenize source = 
    let parseString acc t = function
        |  '"' :: t -> (acc, t) //"
        | '\' :: 'n' :: t -> parseString (acc + "\n") t
        | c :: t -> parseString (acc + c) t
            ...
        | _ -> failwith "string parser error"

    let tokenize' acc = function
        | w :: t when (Char.IsWhiteSpace w) -> tokenize' acc t
        | '{' :: t -> tokenize' (OpenBrace :: acc) t
        | '}' :: t -> tokenize' (CloseBrace :: acc) t
        | 'n'::'u'::'l'::'l'::t -> tokenize' (Null :: acc) t
            ...
        | '"'::t -> let (s, t') = parseString "" t //"
                    tokenize' (String s) t' 
    tokenize [] source


[<EntryPoint>]
let main argv = 
    form
    //let line = Console.ReadLine()
    //range 1 10
    //printfn "%A" argv
     0 // return an integer exit code


