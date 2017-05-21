open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System.Globalization
open System.Text.RegularExpressions


//*** BEGIN README ***//

/// Это вторая ревизия задания (первая была выслана ещё до
/// первого дедлайна - 06.05)
/// Изменения : Number, умеющий в int/double
/// добавлен более-менее парсер escape-последовательностей

/// Каждое подзадание выделено комментариями BEGIN - TEST - END 
/// Весь код между BEGIN и TEST можно запустить разом - все тесты
/// и вывод содержится между TEST и  END

/// Задание lab3 перемещено в конец. Обратите внимание,
/// что lab3 и lab3Recursive - это РАЗНЫЕ решение
/// т.к. я не понял, что требуется в условии - написал оба варианта.

//*** END README ***//

// почтовый адрес
let email = "howtodo@ya.ru"

let json = """
{
    "a":"bvgd",
    "c":{
        "d":[1,2]
        },
    "e":false,
    "f":null
}
"""

let json2 = """
{
    "a":"bvgd",
    "c":{
        "d":[1,
             2,
             {
                 "fname" : ".etcshadow",
                 "dd": 15
             }
            ]
        },
    "e":false,
    "f":null
}
"""

let json3 = """
{
    "float1" : 0.5,
    "float2" : -0.228,
    "float3" : 1e10
    "int1" : 17,
    "int2" : -1997,
    "strangestring" : "Tank \\ \"Mark-V\" \\"
}
"""

open System 

let explode (s:string) = [for i in s -> i]
let implode (list : char list) = 
    List.fold(fun acc c -> acc + (string c)) "" list

//*** BEGIN Parse ***//
type Number = Int of int | Float of float
type Token = OpenBrace | CloseBrace | OpenBracket | CloseBracket | Colon | Comma
            | Null | String of string | Number of Number | Boolean of bool

//*** Tokenizer ***//
let tokenize source = 
    let appendChar str char =
        sprintf "%s%c" str char

    let charToInt (char : char) =
        (int char) - (int '0')

    let rec parseString acc = function
        | '"' :: t -> (acc, t) //'"
        | '\\' :: 'n' :: t -> parseString (acc + "\n") t
        | '\\' :: 'r' :: t -> parseString (acc + "\r") t
        | '\\' :: 't' :: t -> parseString (acc + "\t") t
        | '\\' :: '\'' :: t -> parseString (acc + "\'") t
        | '\\' :: '\"' :: t -> parseString (acc + "\"") t
        | '\\' :: '\\' :: t -> parseString (acc + "\\") t //'"
        | '\\' :: '/' :: t -> parseString (acc + "/") t
        | '\\' :: 'b' :: t -> parseString (acc + "\b") t
        | '\\' :: 'f' :: t -> parseString (acc + "\u000C") t
        | '\\' :: 'u' :: a :: b :: c :: d :: t ->
            let s = implode('\\' :: 'u' :: a :: b :: c :: [d])
            parseString (appendChar acc (char (Int32.Parse(s.Substring(2), NumberStyles.HexNumber)))) t
        | c :: t -> parseString (appendChar acc c) t
        | _ -> failwith "string parser error"
        // What should we do if line brakes?

    let isNumberChar c =
        (Char.IsDigit c) || (c = '-') || (c = '+') || (c = '.')

    let rec parseNumber acc = function
        | digit :: t when (isNumberChar digit) || (digit = 'e') || (digit = 'E')  ->
                    parseNumber (appendChar acc  digit) t
        | list -> (acc, list)
       
    let strToNumber x =
        if not (Regex("\A[\-]?([0]|[1-9][0-9]*)([\.][0-9]+)?([eE][\+\-]?[0-9]+)?\z")
            .IsMatch(x)) then failwith ("Bad number" + x)
        else 
            if (x.Contains(".") || x.Contains("E") || x.Contains("e")) then
                Number.Float(float x)
            else
                Number.Int(int x)

    let rec tokenize' acc = function
        | w :: t when (Char.IsWhiteSpace w) -> tokenize' acc t
        | '{' :: t -> tokenize' (OpenBrace :: acc) t
        | '}' :: t -> tokenize' (CloseBrace :: acc) t
        | '[' :: t -> tokenize' (OpenBracket :: acc) t
        | ']' :: t -> tokenize' (CloseBracket :: acc) t
        | ':' :: t -> tokenize' (Colon :: acc) t
        | ',' :: t -> tokenize' (Comma :: acc) t
        | 'n'::'u'::'l'::'l'::t -> tokenize' (Null :: acc) t
        | '"'::t -> let (s, t') = parseString "" t //"
                    tokenize' (String s :: acc) t' 
        | digit :: t when (isNumberChar digit) -> 
                    let (n, t') = parseNumber "" (digit :: t)
                    tokenize' (Number (strToNumber n) :: acc) t'
        | 't'::'r'::'u'::'e'::t -> tokenize' (Boolean true :: acc) t
        | 'f'::'a'::'l'::'s'::'e'::t -> tokenize' (Boolean false :: acc) t
        | [] -> acc
        | _ -> failwith "tokenize error"

    List.rev (tokenize' [] source)

//*** End Tokenizer ***//

// type JSON = Object of (string * JSON) list

type JSON = 
    | JSON_Null
    | JSON_String of string
    | JSON_Number of Number
    | JSON_Boolean of bool
    | JSON_List of JSON list
    | JSON_Obj of (string * JSON) list
   
// let parse str = if str = "{}" then Object [] else failwith "Wrong JSON structure!"

//type Token = OpenBrace | CloseBrace | OpenBracket | CloseBracket | Colon | Comma
//            | Null | String of string | Number of int | Boolean of bool

let parse (str : string) =
    let tokens = tokenize [for c in str -> c]
    let rec parse' (tokens : Token list) : (JSON * Token list) =
        let rec parse_list (acc : JSON list) (tokens : Token list) : (JSON * Token list) =
            match tokens with
            | Comma :: ts -> 
                parse_list acc ts
            | CloseBracket :: ts ->
                (JSON_List (List.rev acc), ts)
            | ts -> let (obj, ts') = parse' ts
                    parse_list (obj :: acc) ts'

        let rec parse_obj (acc : (string * JSON) list) (tokens : Token list) : (JSON * Token list) = 
            match tokens with
            | String name :: Colon :: ts ->
                let (obj, ts') = parse' ts
                parse_obj ((name, obj) :: acc) ts'
            | Comma :: ts -> 
                parse_obj acc ts
            | CloseBrace :: ts ->
                (JSON_Obj (List.rev acc), ts)
            | _ -> failwith "parse_obj error"
             
        match tokens with
        | Null :: ts -> (JSON_Null, ts)
        | String s :: ts -> (JSON_String s, ts)
        | Number n :: ts -> (JSON_Number n, ts)
        | Boolean n :: ts -> (JSON_Boolean n, ts)
        | OpenBrace :: ts -> parse_obj [] ts
        | OpenBracket :: ts -> parse_list [] ts
        | _ -> failwith "json parse error"

    let (obj, ts) = parse' tokens
    match ts with
    | [] -> obj
    | _ -> failwith "json parse' error"
//*** TEST Parse ***//
tokenize (explode json) // Обьявление выше
tokenize (explode json2)
tokenize (explode json3)

parse json // Обьявление выше
parse json2
parse json3

/// Больше тестирований - в разделе генератора
//*** ENG Parse ***//



//*** BEGIN Stringification ***//

let rec tabbedStringify (prefix : string) (tab : string) (json : JSON) : string =
    let itself = tabbedStringify (prefix + tab) tab

    let print_char = function
        | '\"' -> "\\\"" 
        | '\\' -> "\\\\" //'" (syntax highlining broken : ( )
        | '/' -> "\\/" 
        | '\b' -> "\\b"
        | '\u000C' -> "\\f"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | c -> Convert.ToString c

    let s_name (s : string) : string =
        "\"" + String.collect print_char s + "\""

    let print_number (x : Number) : string =
        match x with
        | Float f -> sprintf "%f" f
        | Int i -> sprintf "%d" i

    let rec print_list (acc : string) (l : JSON list) : string = 
        match l with
        | [obj] -> acc + (itself obj)
        | obj :: objs -> print_list (acc + (itself obj) + ", ") objs
        | _ -> "[]"

    let rec print_obj (acc: string) (l : (string * JSON) list) : string =
        match l with
        | [(name, obj)] -> acc + (prefix + tab) + (s_name name) + ":" + itself obj
        | (name, obj) :: objs ->
            let line = (prefix + tab) + (s_name name) + ":" + itself obj + ",\n"
            print_obj (acc + line) objs
        | [] -> ""

    match json with
    | JSON_Null -> "null"
    | JSON_String s -> s_name s
    | JSON_Number n -> print_number n
    | JSON_Boolean b -> match b with
                        | true -> "true"
                        | _ -> "false"
    | JSON_List l -> "[" + (print_list "" l) + "]"
    | JSON_Obj o -> "{\n" + (print_obj "" o) + "\n" + prefix + "}"
   
let rec plainStringify (json : JSON) : string =
    let print_char = function
        | '\"' -> "\\\"" 
        | '\\' -> "\\\\" //'" (syntax highlining broken : ( )
        | '/' -> "\\/" 
        | '\b' -> "\\b"
        | '\u000C' -> "\\f"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | c -> Convert.ToString c

    let s_name (s : string) : string =
        "\"" + String.collect print_char s + "\""

    let print_number (x : Number) : string =
        match x with
        | Float f -> sprintf "%f" f
        | Int i -> sprintf "%d" i

    let rec print_list (acc : string) (l : JSON list) : string = 
        match l with
        | [obj] -> acc + (plainStringify obj)
        | obj :: objs -> print_list (acc + (plainStringify obj) + ", ") objs
        | _ -> "[]"

    let rec print_obj (acc: string) (l : (string * JSON) list) : string =
        match l with
        | [(name, obj)] -> acc + (s_name name) + ":" + plainStringify obj
        | (name, obj) :: objs ->
            let line = (s_name name) + ":" + plainStringify obj + ","
            print_obj (acc + line) objs
        | [] -> ""

    match json with
    | JSON_Null -> "null"
    | JSON_String s -> s_name s
    | JSON_Number n -> print_number n
    | JSON_Boolean b -> match b with
                        | true -> "true"
                        | _ -> "false"
    | JSON_List l -> "[" + (print_list "" l) + "]"
    | JSON_Obj o -> "{" + (print_obj "" o) + "}"

// MAIN VERSION WITH TABULATIONS
let stringify (json : JSON) : string = 
    tabbedStringify "" "  " json
//*** TEST Stringification ***//
printfn "%s" (plainStringify (parse json))
printfn "%s" (stringify (parse json))

printfn "%s" (plainStringify (parse json2))
printfn "%s" (stringify (parse json2))

printfn "%s" (plainStringify (parse json3))  
printfn "%s" (stringify (parse json3)) /// << see json3 defenition

printfn "%s" (stringify (parse (stringify (parse json3))))

//Больше тестирований - в разделе генератора
//*** END Stringification ***//

//*** BEGIN Random Generator ***//

let generate = fun() -> // this one generates only JSON_Obj,
    let MAX_FIELDS = 10
    let MAX_LIST_LEN = 5
    let MAX_NAME_LEN = 10
    let MAX_OBJ_DEPTH = 2
    let rnd = new Random()
    let rand ub = rnd.Next ub
    let genBoolean = 
        fun() -> (rand 2) = 1
    let genNumber =
        fun() -> if genBoolean() then
                    Int(rnd.Next())
                 else 
                    Float(rnd.NextDouble())
    let genChar = 
        fun() -> sprintf "%c" (char ((int 'a') + (rand 26)))
    let genStr len =
        let rec genStr' (acc : string) = function
            | l when l > 0 -> genStr' (acc + genChar()) (l - 1)
            | _ -> acc
        genStr' "" len
    let genName =
        fun() -> genStr ((rand MAX_NAME_LEN) + 1) 
    let rec genObj (maxDepth : int) = 
        let genList = fun () ->
            let rec genList' (acc : JSON list) (fCnt : int) : (JSON list) =
                match fCnt with
                | c when c > 0 -> genList' ((genObj (maxDepth - 1)) :: acc) (c - 1)
                | _ -> acc
            genList' [] ((rand MAX_LIST_LEN) + 1)
        let rec genObj' (acc : (string * JSON) list) (fCnt : int) : JSON = 
            let genField = fun() ->
                let genObj'' = fun() ->
                    let range = if maxDepth > 0 then 6 // all objects
                                               else 4 // only non-recursive
                    match rand range with
                    | 0 -> JSON_Null
                    | 1 -> JSON_Boolean (genBoolean())
                    | 2 -> JSON_Number (genNumber())
                    | 3 -> JSON_String (genName())
                    | 4 -> JSON_List (genList()) 
                    | _ -> ((genObj (maxDepth - 1)))
                (genName(), genObj''()) 
            match fCnt with
            | c when c > 0 -> genObj' ((genField()) :: acc) (c - 1)
            | _ -> JSON_Obj acc
        genObj' [] ((rand MAX_FIELDS) + 1)
    genObj MAX_OBJ_DEPTH

//*** TEST Random Generator ***//
let gen = generate() // содержимое смотреть из консоли; повторить многа_раз
printfn "%s" (stringify (parse (plainStringify (parse (stringify gen)))))
//*** END Random Generator ***//

//*** BEGIN LAB 3 ***//
// Вариант 6: Составить список из всех повторяющихся ключей двух JSON-объектов //

// Внимание: реализованы функции:
// lab3 - проверяющий только собственные ключи обьекта
// lab3Recursive - проверящий ключи полей (, полей, полей, полей... обьектов)
// (при этом он не смотрит в обьекты лежащие в списке - это исправляется вставкой двух строк и
// как-то совсем лишено смысла, по-моему)
//
// Также реализована функция projection, оставляющая обьекту поля только с разрешёнными ключами

let getKeys obj: (string list) =
    let rec getKeys' (acc : string list) = function
        | (name, _) :: fields -> getKeys' (name :: acc) fields
        | [] -> acc
    match obj with
    | JSON_Obj obj -> List.rev (getKeys' [] obj)
    | _ -> []

let getKeysRecursive obj: (string list) =
    let rec getKeysRecursive' (acc : string list) = function
        | (name, field) :: fields -> match field with
                                     | JSON_Obj fObj ->
                                        let recKeys = getKeysRecursive' [] fObj
                                        getKeysRecursive' (recKeys @ (name :: acc)) fields
                                     |_ -> getKeysRecursive' (name :: acc) fields
        | [] -> acc
    match obj with
    | JSON_Obj obj -> List.rev (getKeysRecursive' [] obj)
    | _ -> []

let intersect list1 list2 =
    let l1 = List.sortDescending list1
    let l2 = List.sortDescending list2
    let rec intersect' l1 l2 acc =
        match (l1, l2) with
        | ([], _) -> acc
        | (_, []) -> acc
        | (h1 :: t1, h2 :: t2) -> if h1 = h2 then 
                                      intersect' t1 t2 (h1 :: acc)
                                  else 
                                      if h1 > h2 then
                                          intersect' t1 (h2 :: t2) acc
                                      else
                                          intersect' (h1 :: t1) t2  acc
    intersect' l1 l2 []

let projection obj keys =
    let rec proj' acc (obj : (string * JSON) list) keys =
        match (obj, keys) with
        | ([], _) -> acc
        | (_, []) -> acc
        | ( (name, field) :: tail, hkey :: tkey) -> if (name = hkey) then
                                                        proj' ((name, field) :: acc) tail tkey
                                                    else
                                                        if (name < hkey) then
                                                            proj' acc tail (hkey :: tkey)
                                                        else
                                                            proj' acc ((name, field) :: tail) tkey
    match obj with
    | JSON_Obj obj -> 
        let sortedObj = List.sortWith (fun (n1, f1) (n2, f2) -> String.Compare(n1, n2)) obj 
        List.rev (proj' [] sortedObj (List.sort keys))
    | _ -> []

let lab3 obj1 obj2 =
    intersect (getKeys obj1) (getKeys obj2)

let lab3Recursive obj1 obj2 =
    intersect (getKeysRecursive obj1) (getKeysRecursive obj2)

//*** TEST LAB 3 ***//
let j1 = """
{
    "aba":"bvgd",
    "cappa":{
        "d":[1,2]
        },
    "elf":false,
    "f":null
}
"""

let j2 = """
{
    "elf":true,
    "f":5,
    "newfaq":{
        "faq":true,
        "new":{
            "aba":"v"
        } 
    }
}
"""

let j3 = """
{
    "newbuilts":"yellow",
    "newfaq":[1,4,5,9],
    "f":[8,9,7],
    "cappa":null,
    "aba":"baba"
}
"""


let jo1 = parse j1
let jo2 = parse j2
let jo3 = parse j3

getKeys jo1 // should - ["aba"; "cappa"; "elf"; "f"]
getKeys jo2 // should - ["elf"; "f"; "newfaq"]
getKeys jo3 // should - ["newbuilts"; "newfaq"; "f"; "cappa"; "aba"]

getKeysRecursive jo1 // should - ["aba"; "cappa"; "d"; "elf"; "f"]
getKeysRecursive jo2 // should - ["elf"; "f"; "newfaq"; "faq"; "new"; "aba"]
getKeysRecursive jo3 // should - ["newbuilts"; "newfaq"; "f"; "cappa"; "aba"]

lab3 jo1 jo2 // should recieve ["elf", "f"]
lab3 jo1 jo3 // should recieve ["aba"; "cappa"; "f"]
lab3 jo2 jo3 // should recieve ["f"; "newfaq"]

lab3Recursive jo1 jo2 // should recieve  ["aba"; "elf"; "f"]
lab3Recursive jo1 jo3 // should recieve ["aba"; "cappa"; "f"]
lab3Recursive jo2 jo3 // should recieve ["aba"; "f"; "newfaq"]

let lr = generate()
printfn "%s" (stringify lr)
lab3 lr lr // should recieve all root keys
lab3Recursive lr lr // should recieve all keys 

projection jo1 (getKeys jo2)
// should recieve [("f", JSON_Null); ("elf", JSON_Boolean false)]

projection jo2 (getKeys jo3)
// should recieve [("newfaq", JSON_Obj [("faq", JSON_Boolean true);
// ("new", JSON_Boolean false)]);("f", JSON_Number 5)]

projection jo3 (getKeys jo1)
// should recieve [("aba", JSON_String "baba"); ("cappa", JSON_Null);
//   ("f", JSON_List [JSON_Number 8; JSON_Number 9; JSON_Number 7])]

//*** END LAB 3 ***//
    

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
