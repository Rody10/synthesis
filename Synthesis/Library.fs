module Synthesis

let abelar n =
    n > 12 && n < 3097 && n % 2 = 0

let area (b: float) (h: float) =
    match b < 0.0 || h < 0.0 with
    |true -> failwith "negative numbers are not allowed"
    |false ->  0.5 * b * h


let zollo n =
    match n > 0 with
    |true -> n + n
    |false -> (n - n) - n

let zolo n =
    match n > 0 with
    |true -> n
    |false -> (n - n) - n

let min a b =
    match a < b with 
    |true -> a
    |false -> b
    

let max a b =
    match a > b with
    |true -> a
    |false -> b

let ofTime hours minutes seconds =
    ((hours * 60) + minutes) * 60 + seconds

let toTime seconds =
    let h z =  z / 3600
    let m x = (x%3600)/60
    let s y =  seconds - ((h seconds)*3600 + (m seconds)*60)   
    match seconds < 0 with
    |true -> (0,0,0)
    |false -> (h seconds, m seconds, s seconds)
                        
    

let digits n =
    let rec findN k acc=
        match k <= 9  with
        |true -> acc
        |false -> findN (k/10) (acc + 1)
    findN (zolo n) 1
    

let minmax n =
    let a,b,c,d = n
    min(min a b)(min c d), max(max a b)(max c d)                         //min a,b |> min c|> min d , max a,b |> min c|> min d
    


let isLeap y =
    match y<1582 with
    |true -> failwith "enter years from 1582 upwards"
    |false -> match ((y%4=0&&y%100<>0) || ((y%4=0&&y%100<>0)&& y%400=0) || (y%4=0&&y%100=0)&& y%400=0) with
              |true -> true
              |false -> false


let month m =
    match (m<1 || m> 12) with
    |true -> failwith "enter values between 1 and 12"
    |false -> match m with
                |1 -> "January", 31
                |2 -> "February", 28
                |3 -> "March", 31
                |4 -> "April", 30
                |5 -> "May" ,31
                |6 -> "June", 30
                |7 -> "July" , 31
                |8 -> "August" , 31
                |9 -> "September", 30
                |10 -> "October" , 31
                |11 -> "November" , 30
                |12 -> "December" , 31



    

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"