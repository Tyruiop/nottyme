open Notty
open Notty_lwt
open Lwt.Infix

let square = "\xe2\x96\x88"
let delay = Some 1.0

let sign n =
    if n > 0 then 1 else if n < 0 then -1 else 0

let rec makelist n v =
    match n with
    | 0 -> []
    | a -> v :: (makelist (a - 1) v)

module Primitives : sig
    val draw : int -> int -> Notty.image
    val draw_text : string -> int -> int -> Notty.image
    val hline : int -> int -> int -> Notty.image
    val vline : int -> int -> int -> Notty.image
end = struct
    let draw x y =
        I.(string A.(fg green) square |> hpad x 0 |> vpad y 0)

    let draw_text s x y =
        I.(string A.(fg green) s |> hpad x 0 |> vpad y 0)

    let hline n x y =
        let s = String.concat "" (makelist n square) in
        I.(string A.(fg green) s |> hpad x 0 |> vpad y 0)

    let vline n x y =
        let rec s_vline n x y =
            if n > 1 then
                I.(s_vline (n - 1) x y <-> string A.(fg green) square)
            else I.(string A.(fg green) square)
        in
        I.(s_vline n x y |> hpad x 0 |> vpad y 0)
end

module Numbers : sig
    type t
    val numbers : int -> t
    val to_img : int -> int -> t -> Notty.image
end = struct
    open Primitives

    type t = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Err | Dots

    let numbers = function
        | 0 -> Zero
        | 1 -> One
        | 2 -> Two
        | 3 -> Three
        | 4 -> Four
        | 5 -> Five
        | 6 -> Six
        | 7 -> Seven
        | 8 -> Eight
        | 9 -> Nine
        | -1 -> Dots
        | _ -> Err

    let to_img x y = function
        | Zero      -> I.(hline 4 x y </> vline 7 x y </> hline 4 x (y + 6) </> vline 7 (x + 3) y)
        | One       -> I.(vline 7 (x + 3) y)
        | Two       -> I.(hline 3 x y </> vline 4 (x + 3) y </> hline 4 x (y + 3) </> vline 4 x (y + 3) </> hline 4 x (y + 6))
        | Three     -> I.(hline 3 x y </> vline 7 (x + 3) y </> hline 3 x (y + 3) </> hline 3 x (y + 6))
        | Four      -> I.(vline 4 x y </> vline 7 (x + 3) y </> hline 3 x (y + 3))
        | Five      -> I.(hline 4 x y </> vline 4 x y </> hline 3 x (y + 3) </> vline 4 (x + 3) (y + 3) </> hline 4 x (y + 6))
        | Six       -> I.(hline 4 x y </> vline 7 x y </> vline 4 (x + 3) (y + 3) </> hline 4 x (y+3) </> hline 4 x (y + 6))
        | Seven     -> I.(hline 4 x y </> vline 7 (x + 3) y)
        | Eight     -> I.(hline 4 x y </> vline 7 x y </> vline 7 (x + 3) y </> hline 4 x (y + 3) </> hline 4 x (y + 6))
        | Nine      -> I.(hline 4 x y </> vline 3 x y </> hline 4 x (y + 3) </> vline 7 (x + 3) y)
        | Dots      -> I.(draw (x + 1) (y + 3) </> draw (x + 1) (y + 6))
        | Err       -> I.(hline 4 x (y + 3))
end

type clock = {
    mutable x: int;
    mutable y: int;
}

let init_clock term =
    let w, h = Term.size term in
    { x = (w - 38)/2; y = (h - 7)/2; }

let update_clock c w h =
    c.x <- (w - 38)/2;
    c.y <- (h - 7)/2;

type ctstatus = Off | Writing | On

type countdown = {
    mutable status: ctstatus;
    mutable start: int;
    mutable target: int;
    mutable wtarget: int list;
}

let format_h n =
    let h = n/3600 in
    let m = (n - 3600*h)/60 in
    let s = n mod 60 in
    (h, m, s)

let unformat_h (h, m, s) = h*3600 + m*60 + s

let init_countdown () = { status = Off; start = 0; target = 0; wtarget = []; }

let trigger_countdown ct =
    let t = int_of_float (Unix.time ()) in
    ct.status <- On;
    ct.start <- t;
    ct.wtarget <- [];
    ct.target <- t + ct.target

let rec ( ** ) n = function
    | 0 -> 1
    | m -> if m < 0 then 1 else n * (( ** ) n (m - 1))

let wtarget_to_target ctw =
    let ctwa = Array.of_list (List.rev ctw) in
    let ctwas = Array.length ctwa in
    let s =
        if ctwas >= 2 then ctwa.(ctwas - 2)*10 + ctwa.(ctwas - 1)
        else if ctwas = 1 then ctwa.(0)
        else 0
    in
    let m =
        if ctwas >= 4 then ctwa.(ctwas - 4)*10 + ctwa.(ctwas - 3)
        else if ctwas > 2 then ctwa.(2)
        else 0
    in
    let h =
        if ctwas >= 6 then ctwa.(ctwas - 6)*10 + ctwa.(ctwas - 5)
        else if ctwas = 5 then ctwa.(4)
        else 0
    in
    (h, m, s)

let write_countdown ct =
    let (h,m,s) = wtarget_to_target ct.wtarget in
    let hs = if h < 10 then "0" ^ string_of_int h else string_of_int h in
    let ms = if m < 10 then "0" ^ string_of_int m else string_of_int m in
    let ss = if s < 10 then "0" ^ string_of_int s else string_of_int s in
    hs ^ ":" ^ ms ^ ":" ^ ss

let display_status t ct x y =
    let open Primitives in
    match ct.status with
    | Off -> I.(hline 28 x y </> draw_text "00:00:00" (x + 29) y)
    | Writing -> I.(hline 28 x y </> draw_text (write_countdown ct) (x + 29) y)
    | On -> let cur = ct.target - (int_of_float t) in
            let (h, m, s) = format_h cur in
            let l = ((float_of_int ct.target) -. t) /. (float_of_int (ct.target - ct.start)) in
            let n_s = int_of_float (28.0 *. l) in
            let hs = if h < 10 then "0" ^ string_of_int h else string_of_int h in
            let ms = if m < 10 then "0" ^ string_of_int m else string_of_int m in
            let ss = if s < 10 then "0" ^ string_of_int s else string_of_int s in
            let fs = String.concat "" [hs; ":"; ms; ":"; ss] in
            I.(hline n_s x y </> draw_text fs (x + 29) y)

let update_status ct x y = match ct.status with
    | Off -> display_status (-1.0) ct x y
    | Writing -> display_status (-1.0) ct x y
    | On ->
            let t = (Unix.time ()) in
            if ct.target < (int_of_float t) then ct.status <- Off;
            display_status t ct x y

let display_time x y =
    let t = Unix.localtime (Unix.time ()) in
    let h, m, s = (t.Unix.tm_hour, t.Unix.tm_min, t.Unix.tm_sec) in
    let h10, h1 = h/10, h mod 10 in
    let m10, m1 = m/10, m mod 10 in 
    let s10, s1 = s/10, s mod 10 in
    I.(Numbers.(to_img x y (numbers h10) </> to_img (x + 5) y (numbers h1) </> to_img (x + 10) y (numbers (-1)) </> to_img (x + 14) y (numbers m10) </> to_img (x + 19) y (numbers m1) </> to_img (x + 24) y (numbers (-1)) </> to_img (x + 28) y (numbers s10) </> to_img (x + 33) y (numbers s1)))

let timer () = Lwt_unix.sleep 1.0 >|= fun _ -> `Timer
let event term = Lwt_stream.get (Term.events term) >|= function
    | Some (`Resize _ | #Unescape.event as x) -> x
    | None -> `End

let render term c ct =
    let ic = display_time c.x c.y in
    let ict = update_status ct c.x (c.y + 8) in
    Term.image term I.(ic </> ict)

let input c ct = function
    | 99 -> let f = function
                | Off -> Writing
                | Writing ->
                        let (h, m, s) = wtarget_to_target ct.wtarget in
                        ct.target <- h*3600 + m*60 + s; trigger_countdown ct;
                        On
                | On -> Off
        in
        ct.status <- f ct.status
    | _ -> ()

let rec loop term c ct (e, t) =
    (e <?> t) >>= function
        | `End | `Key (`Enter, _) -> Lwt.return_unit
        | `Key (`Uchar u, _) ->
                (match u with
                    | 99 -> let f = function
                            | Off -> Writing
                            | Writing ->
                                let (h, m, s) = wtarget_to_target ct.wtarget in
                                ct.target <- h*3600 + m*60 + s; trigger_countdown ct;
                                On
                            | On -> ct.wtarget <- []; Off
                        in
                        ct.status <- f ct.status
                    | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 ->
                            if ct.status = Writing then ct.wtarget <- (u - 48) :: ct.wtarget
                    | _ -> ()
                );
                render term c ct >>= fun() ->
                    loop term c ct (event term, t)
        | `Timer ->
            render term c ct >>= fun () ->
                loop term c ct (e, timer ())
        | `Resize dim ->
            let w, h = Term.size term in
            c.x <- (w - 38)/2; c.y <- (h - 7)/2;
            render term c ct >>= fun () ->
                loop term c ct (event term, t)
        | _ -> loop term c ct (event term, timer ())

let main () =
    let term = Term.create() in
    let c = init_clock term in
    let ct = init_countdown () in
    render term c ct  >>= fun () ->
        loop term c ct (event term, timer ())

let () = Lwt_main.run @@ main ()
