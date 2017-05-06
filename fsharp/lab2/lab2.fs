open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = ""

//*** Tokenizer ***//

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

open System 

let explode (s:string) = [for i in s -> i]

type Token = OpenBrace | CloseBrace | OpenBracket | CloseBracket | Colon | Comma
            | Null | String of string | Number of int | Boolean of bool
            /// is True | False needed?

let tokenize source = 
    let appendChar str char =
        sprintf "%s%c" str char

    let charToInt (char : char) =
        (int char) - (int '0')

    let rec parseString acc = function
        |  '"' :: t -> (acc, t) //'"
        | '\\' :: 'n' :: t -> parseString (acc + "\n") t
        | c :: t -> parseString (appendChar acc c) t
        | _ -> failwith "string parser error"
        // What should we do if line brakes?

    let rec parseNumber acc = function
        | digit :: t when (Char.IsDigit digit) ->
                    parseNumber (acc * 10 + (charToInt digit)) t
        | char :: t -> (acc, char :: t)
        | [] -> (acc, [])

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
        | digit :: t when (Char.IsDigit digit) ->
                    let (n, t') = parseNumber (charToInt digit) t
                    tokenize' (Number n :: acc) t'
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
    | JSON_Number of int
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
            | Null :: ts ->
                parse_list ((JSON_Null) :: acc) ts
            | String s :: ts ->
                parse_list ((JSON_String s) :: acc) ts
            | Number n :: ts ->
                parse_list ((JSON_Number n) :: acc) ts
            | Boolean b :: ts ->
                parse_list ((JSON_Boolean b) :: acc) ts
            | OpenBrace :: ts ->
                let (obj, ts') = parse' (OpenBrace :: ts)
                parse_list ((obj) :: acc) ts'
            | OpenBracket :: ts ->
                let (lst, ts') = parse_list [] ts
                parse_list ((lst) :: acc) ts'
            | Comma :: ts -> 
                parse_list acc ts
            | CloseBracket :: ts ->
                (JSON_List (List.rev acc), ts)
            | _ -> failwith "parse_list error"

        let rec parse_obj (acc : (string * JSON) list) (tokens : Token list) : (JSON * Token list) = 
            match tokens with
            | String name :: Colon :: Null :: ts ->
                parse_obj ((string name, JSON_Null) :: acc) ts
            | String name :: Colon :: String s :: ts ->
                parse_obj ((string name, JSON_String s) :: acc) ts
            | String name :: Colon :: Number n :: ts ->
                parse_obj ((string name, JSON_Number n) :: acc) ts
            | String name :: Colon :: Boolean b :: ts ->
                parse_obj ((string name, JSON_Boolean b) :: acc) ts
            | String name :: Colon :: OpenBrace :: ts ->
                let (obj, ts') = parse_obj [] ts
                parse_obj ((string name, obj) :: acc) ts'
            | String name :: Colon :: OpenBracket :: ts ->
                let (lst, ts') = parse_list [] ts
                parse_obj ((string name, lst) :: acc) ts'
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




//*** Stringification ***//
(*****
let tokenToString = function
    | OpenBrace -> "{"
    | CloseBrace -> "}"
    | OpenBracket -> "["
    | CloseBracket -> "]"
    | Colon -> ":"
    | Comma -> ","
    | Null -> "null"
    | String str -> "\"" + str + "\""
    | Number num -> sprintf "%d" num
    | Boolean b -> match b with
                   | true -> "true"
                   | _ -> "false"
        
type JSON = 
    | JSON_Null
    | JSON_String of string
    | JSON_Number of int
    | JSON_Boolean of bool
    | JSON_List of JSON list
    | JSON_Obj of (string * JSON) list
   ******))))
     
let rec stringify (json : JSON) : string =
    let s_name (s : string) : string = "\"" + s + "\""

    let rec print_list (acc : string) (l : JSON list) : string = 
        match l with
        | [obj] -> acc + (stringify obj)
        | obj :: objs -> print_list (acc + (stringify obj) + ", ") objs
        | _ -> "[]"

    let rec print_obj (acc: string) (l : (string * JSON) list) : string =
        match l with
        | [(name, obj)] -> acc + (s_name name) + ":" + stringify obj
        | (name, obj) :: objs ->
            let line = (s_name name) + ":" + stringify obj + ",\n"
            print_obj (acc + line) objs
        | [] -> ""

    match json with
    | JSON_Null -> "null"
    | JSON_String s -> s_name s
    | JSON_Number n -> sprintf "%d" n
    | JSON_Boolean b -> match b with
                        | true -> "true"
                        | _ -> "false"
    | JSON_List l -> "[" + (print_list "" l) + "]"
    | JSON_Obj o -> "{\n" + (print_obj "" o) + "\n}"

//*** END Stringification ***//

let lab3 = function
  | Object list -> 0

let generate = 
  let rnd = new Random()
  match rnd.Next(42) with
    | 0 -> Object []
    | _ -> Object [("random", Object [])]

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
