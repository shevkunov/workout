module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес

let email = "howtodo@ya.ru"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10


// *** Первая часть
// f(x) = sh(x) = (e^(+x) - e^(-x)) / 2
// sh(x) ~ x^(2n - 1) / ((2n - 1)!)

let fTailor (x : float) = (exp(+x) - exp(-x)) / 2. // функция, которую раскладываем
let n, a, b = 20., 0., 1. // интервал

let tailorCoeff (n : int) =
    let rec rfac (n : int) (acc : Result) =
        match n with
        | 1 -> acc
        | _ -> rfac (n - 1) ((fst acc) / (float n), (snd acc) + 1)
    rfac n (1., 0)

let add (a : Result) (b : Result) =
    (fst a + fst b, (snd a) + (snd b) + 1)

let mul (a : Result) (b : Result) =
    ((fst a) * (fst b), (snd a) + (snd b) + 1)


let rec pow (x : float) (n : int) =
    match n with
    | 0 -> (1., 1)
    | 1 -> (x , 1)
    | _ -> let calc = pow x (n / 2)
           let v = fst calc
           let iter = snd calc
           if (n % 2 = 0) then (v * v, iter + 1)
           else (v * v * x, iter + 1)

// As you told to me, we evaluate count of OPERATIONS in first part

let tailor x : Result =
    let absolute = fTailor x // There are another way to evaluate the error
    let rec tailor' (acc : Result) (n : int) = 
        if (abs (fst acc - absolute) >= delta) then // First way to evaluate error
            let nextCoeff = tailorCoeff n // Stupid evaluation
            //printf "%f-%d\n" (fst acc) (snd acc)
            let term = mul (pow x n) nextCoeff // with O(n!) coeff and O(log) power eval
            tailor' (add acc term) (n + 2)
        else 
            acc
    tailor' (x, 1) 3


let tailorA x : Result =
    let absolute = fTailor x  // There are another way to evaluate the error
    let rec tailorA' (acc : Result) (lastCoeff : float) (lastX : float) (n : int) = 
        if (abs (fst acc - absolute) >= delta) then // First way to evaluate error
            let nextCoeff = (lastCoeff / (float (n - 1)) / (float n), 2)  // good evaluation
            let nextX = (lastX * x * x, 2) /// with O(1) evaluations
            let term = mul nextX nextCoeff
            //printf "%f-%d\n" (fst acc) (snd acc)
            tailorA' (add acc term) (fst nextCoeff) (fst nextX) (n + 2)
        else
            acc
    tailorA' (x, 1) 1. x 3 



let tailor2 x : Result =
    let rec tailor2' (acc : Result) (n : int) = 
        let nextCoeff = tailorCoeff n // Stupid evaluation
        let term = mul (pow x n) nextCoeff // with O(n!) coeff and O(log) power eval
        if (abs (fst term) >= delta) then // Second way to evaluate error
            tailor2' (add acc term) (n + 2)
        else 
            acc
    tailor2' (x, 1) 3


let tailorA2 x : Result =
    let rec tailorA2' (acc : Result) (lastCoeff : float) (lastX : float) (n : int) = 
        let nextCoeff = (lastCoeff / (float (n - 1)) / (float n), 2)  // good evaluation
        let nextX = (lastX * x * x, 2) /// with O(1) evaluations
        let term = mul nextX nextCoeff
        if (abs (fst term) >= delta) then  // Second way to evaluate error
            tailorA2' (add acc term) (fst nextCoeff) (fst nextX) (n + 2)
        else
            acc
    tailorA2' (x, 1) 1. x 3 




let printTailor () = // First way to evaluate the error
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

let printTailor2 () = // Second way to evaluate the error. In fact it is the same with this series.
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor2 x, tailorA2 x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

// *** Вторая часть

let fSolve6 = fun x -> x + cos((x ** 0.52) + 2.)
let phi6 = fun x -> x - (fSolve6 x) / 10.
let ab6 = (0.5, 1.) 
let test6 = (fSolve6, phi6, ab6)

let fSolve7 = fun x -> 3. * ((log x) ** 2.) + 6. * (log x) -  5.
let phi7 = fun x -> x - (fSolve7 x) / 10.
let ab7 = (1., 3.) 
let test7 = (fSolve7, phi7, ab7)
 
let fSolve8 = fun x -> 0.6 * (3. ** x) - 2.3 * x - 3.
let phi8 = fun x -> x - (fSolve8 x) / 10.
let ab8 = (2., 3.) 
let test8 = (fSolve8, phi8, ab8)


let newton f a b : Result = 
    let rec newtonA i (x1 : float) (x0 : float) : Result =
        let x2 = x1 - (f x1) * ((x1 - x0) / ((f x1) - (f x0)))
        // we may write here = x1 - (f x1) / (f' x1),
        // but i use modificated newton algo,
        // derrivative doesn't required
        if (abs (x2 - x1) >= delta) then
            newtonA (i + 1) x2 x1
        else
            (x2, i)
    newtonA 0 a b

let iter phi a b : Result =
    let rec iterA i (x : float) : Result =
        let xnew = phi x
        if (abs (xnew - x) >= delta) then
            iterA (i + 1) xnew
        else
            (xnew, i)
    iterA 0 ( a )

let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result =
        let c = (a + b) / 2.
        if (b - a >= delta) then
            if ((f a)*(f c) <= 0.) then
                dichotomyA (i + 1) f a c
            else
                dichotomyA (i + 1) f c b      
        else 
            (c, i)   
    dichotomyA 0 // чтобы воспользоваться каррированием

let solve test =
    let f, phi, (a, b) = test
    [("Iteration", iter phi a b);
     ("Newton", newton f a b);
     ("Dichotomy", dichotomy f a b)]

let solveAll() =
    [("test6", test6); ("test7", test7); ("test8", test8)]
    |> List.map (fun t -> ((fst t), solve (snd t)))


let printSolve () =
    (solveAll())
    |> List.iter (fun ((name : string), list) ->
                    printfn "---%s---\n" name

                    list
                    |> List.iter (fun ((met : string), value) ->
                        printf "%s :\n value = (%f), iters = (%d)\n" met (fst value) (snd value)))

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString