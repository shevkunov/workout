// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System
open System.Net
open System.Threading

///
/// Модифицированное (вами, т.к. почти наверное на странице (7/7 случайных преподавателей не имели их)
/// преподавателя нет ссылок на соцсети) задание состоит в следующем:
/// Составить список из ФИО преподавателей
/// (http://wikimipt.org/wiki/%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:%D0%9F%D1%80%D0%B5%D0%BF%D0%BE%D0%B4%D0%B0%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%BB%D1%84%D0%B0%D0%B2%D0%B8%D1%82%D1%83)
/// и страниц, ссылки на которые ведут из вики-карточки,
/// при этом не являются внутренними (веду не на http://wikimipt.org/) 
///

(* Мой код с семинара для примеру
let hockey = HtmlDocument.Load("http://www.sport-express.ru/hockey/world/")
hockey.Descendants ["td"]
    |> Seq.filter (fun (td:HtmlNode) -> td.HasClass "t_left w_100p")
    |> Seq.map (fun x -> x.InnerText()) 
    |> Seq.toList    
 *)

let rootPageURL = "http://wikimipt.org/wiki/%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:%D0%9F%D1%80%D0%B5%D0%BF%D0%BE%D0%B4%D0%B0%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%BB%D1%84%D0%B0%D0%B2%D0%B8%D1%82%D1%83"
      
let getFirstLetterOnPageLinksAndNamesList (url:string) =
    let page = HtmlDocument.Load(url)
    let content = page.Descendants ["div"]
                    |> Seq.find (fun (x:HtmlNode) -> x.HasClass "mw-content-ltr")
                    // Выпарсили блок c содержимым
    let innerDiv = content.Descendants["div"] |> Seq.tryFind (fun x -> x.HasClass "mw-category-group")
    let parseOneLetterBlock (x:HtmlNode) =
        x.Descendants ["li"]
            |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
            |> Seq.map (fun x -> (x.InnerText(), x.AttributeValue("href"))) 
            |> Seq.toList 
    match innerDiv with
    | Some(liList) ->
        parseOneLetterBlock(liList)
    | None -> // ОНИ ПАРСЯТ СТРАНИЦУ С БУКВОЙ "Я" ПО-ОСОБЕННОМУ!! ЗАЧЕМЗАЧЕМЗАЧЕМ
        parseOneLetterBlock(content)
                    
// Например: (Alfabet list ниже)
// getFirstLetterOnPageLinksAndNamesList alfabetList.[1]
// (getFirstLetterOnPageLinksAndNamesList alfabetList.[1]).[51]
//  - Илья Игоревич
// http://wikimipt.org/wiki/%D0%91%D0%BE%D0%B3%D0%B4%D0%B0%D0%BD%D0%BE%D0%B2_%D0%98%D0%BB%D1%8C%D1%8F_%D0%98%D0%B3%D0%BE%D1%80%D0%B5%D0%B2%D0%B8%D1%87

let getPersonalDateFromPage (id : string * string) =
    let (name, url) = id
    let urlCover url =
        "http://wikimipt.org" + url
    let page = HtmlDocument.Load(urlCover url)
    let wikicard = page.Descendants ["table"]
                        |> Seq.find (fun (x:HtmlNode) -> x.HasClass "wikitable card")
    let linksPred (url:string) =
        ((url.StartsWith "/wiki") || (url.StartsWith "/index.php")) <> true
    let links = wikicard.Descendants ["a"]
                    |> Seq.map (fun x -> (x.InnerText(), x.AttributeValue("href"))) 
                    |> Seq.filter (fun (name, url) -> linksPred url) // Это ваше предложение по отбору ссылок
                    |> Seq.toList
    printfn "Parsed : %s" name
    (name, links)

// Например: (Alfabet list ниже)
// getPersonalDateFromPage (getFirstLetterOnPageLinksAndNamesList alfabetList.[1]).[51]
// - ссылка на страницу выше, выпаршивает две внешние ссылки.

let unique list =
    let rec unique' sourse dest =
        match (sourse, dest) with 
        | (s::ss, d::ds) -> if s = d then unique' ss (d::ds)
                            else unique' ss (s::d::ds)
        | ([], ds) -> ds
        | (s::ss, []) -> unique' ss [s]

    // потестить:
    // unique' [1;1;2;3;4;4;5;5;6;7;7;7;8;9;9] []
    // unique [4;1;2;3;4;5;5;6;7;7;8;9;1;9;7]
    match list with
    | [] -> []
    | _ -> unique' (List.sortDescending list) []
   
/////////////////////////////////////////////////////////////////
//// SINGLECORE MODE
////////////////////////////////////////////////////////////////
                               

let ts() = System.DateTime.Now.Ticks
let time_0 = ts()
printfn "Root Page loading..."
let listPage = HtmlDocument.Load(rootPageURL)
printfn "Alfabet List loading..."
let alfabetList = 
    listPage.Descendants ["table"]
        |> Seq.filter (fun (x:HtmlNode) -> x.HasClass "plainlinks")
        |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
        |> Seq.filter (fun (x:HtmlNode) -> (x.InnerText() <> "*")) // String.Compare
        // |> Seq.map(fun x -> x.InnerText()) // Мы выпарсили алфавит
        |> Seq.map (fun x -> x.AttributeValue("href")) 
        |> Seq.toList  
printfn "Professors IDs loading..."
let professorIds = alfabetList
                    |> List.collect getFirstLetterOnPageLinksAndNamesList
                    |> unique
                    // потому что ВНЕЗАПНО при переходе на букву "Ё" или "Й"
                    // показываются имена на "А" и "К".
                    // Возможно, какие-то ещё буквы испорчены
let loaded = professorIds
                |> List.map getPersonalDateFromPage
let time_1 = ts()
printfn "One Thread Total time: %s" (System.TimeSpan(time_1 - time_0).ToString())
///One Thread Total time: 00:10:37.1766870

/////////////////////////////////////////////////////////////////
//// MULTICORE MODE ON!!!!!!!!!!!!!!!!
////////////////////////////////////////////////////////////////

let ts() = System.DateTime.Now.Ticks
let time_0 = ts()
printfn "Root Page loading..."
let listPage = HtmlDocument.Load(rootPageURL)
printfn "Alfabet List loading..."
let alfabetList = 
    listPage.Descendants ["table"]
        |> Seq.filter (fun (x:HtmlNode) -> x.HasClass "plainlinks")
        |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
        |> Seq.filter (fun (x:HtmlNode) -> (x.InnerText() <> "*")) // String.Compare
        // |> Seq.map(fun x -> x.InnerText()) // Мы выпарсили алфавит
        |> Seq.map (fun x -> x.AttributeValue("href")) 
        |> Seq.toList  

printfn "Professors IDs loading..."
let async_getFirstLetterOnPageLinksAndNamesList x = async {
        return getFirstLetterOnPageLinksAndNamesList x
    }

let professorIds = alfabetList
                    |> List.map async_getFirstLetterOnPageLinksAndNamesList
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> Array.toList
                    |> List.concat
                    |> unique
                    // потому что ВНЕЗАПНО при переходе на букву "Ё" или "Й"
                    // показываются имена на "А" и "К".
                    // Возможно, какие-то ещё буквы испорчены

let async_getPersonalDateFromPage x = async {
        return getPersonalDateFromPage x
    }
let loaded = professorIds
                |> List.map async_getPersonalDateFromPage
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
let time_1 = ts()
printfn "Multi Thread Total time: %s" (System.TimeSpan(time_1 - time_0).ToString())
///Multi Thread Total time: 00:08:34.1118120 
/// Быстрее, но... Возможно у меня сеть - бутылочное горло системы.

/////////////////////////////////////////////////////////////////
// почтовый адрес
let email = ""

let lab3 () =
  let bases = HtmlDocument.Load("https://mipt.ru/diht/bases/")
  bases.Descendants ["ul"] 
    |> Seq.filter (fun x -> x.HasClass("right-menu")) 
    |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
    // для получения ссылок вместо InnerText нужно использовать методы TryGetAttribute, Attibute или AttributeValue
    // см. исходный код https://github.com/fsharp/FSharp.Data/blob/master/src/Html/HtmlOperations.fs
    |> Seq.map(fun x -> x.InnerText()) 
    |> Seq.toList

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
