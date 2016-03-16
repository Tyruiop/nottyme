open Notty
open Notty_lwt
open Lwt.Infix

let square = "\xe2\x96\x88"
let delay = Some 1.0
let wx = ref 0
let wy = ref 0

let draw (x,y) =
    I.(string A.(fg green) square |> hpad x 0 |> vpad y 0)

let sign n =
    if n > 0 then 1 else if n < 0 then -1 else 0

let rec makelist n v =
    match n with
    | 0 -> []
    | a -> v :: (makelist (a - 1) v)

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

module Numbers : sig
    type t
    val numbers : int -> t
    val to_img : int -> int -> t -> Notty.image
end = struct
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
        | Five       -> I.(hline 4 x y </> vline 4 x y </> hline 3 x (y + 3) </> vline 4 (x + 3) (y + 3) </> hline 4 x (y + 6))
        | Six      -> I.(hline 4 x y </> vline 7 x y </> vline 4 (x + 3) (y + 3) </> hline 4 x (y+3) </> hline 4 x (y + 6))
        | Seven     -> I.(hline 4 x y </> vline 7 (x + 3) y)
        | Eight     -> I.(hline 4 x y </> vline 7 x y </> vline 7 (x + 3) y </> hline 4 x (y + 3) </> hline 4 x (y + 6))
        | Nine      -> I.(hline 4 x y </> vline 3 x y </> hline 4 x (y + 3) </> vline 7 (x + 3) y)
        | Dots      -> I.(draw (x + 1, y + 3) </> draw (x + 1, y + 6))
        | Err       -> I.(hline 4 x (y + 3))
end

let display_time x y =
    let t = Unix.localtime (Unix.time ()) in
    let h, m, s = (t.Unix.tm_hour, t.Unix.tm_min, t.Unix.tm_sec) in
    let h10 = h/10 in
    let h1 = h mod 10 in
    let m10 = m/10 in
    let m1 = m mod 10 in 
    let s10 = s/10 in
    let s1 = s mod 10 in
    I.(Numbers.(to_img x y (numbers h10) </> to_img (x + 5) y (numbers h1) </> to_img (x + 10) y (numbers (-1)) </> to_img (x + 14) y (numbers m10) </> to_img (x + 19) y (numbers m1) </> to_img (x + 24) y (numbers (-1)) </> to_img (x + 28) y (numbers s10) </> to_img (x + 33) y (numbers s1)))

let timer () = Lwt_unix.sleep 1.0 >|= fun _ -> `Timer
let event term = Lwt_stream.get (Term.events term) >|= function
    | Some (`Resize _ | #Unescape.event as x) -> x
    | None -> `End

let rec loop term (e, t) =
    (e <?> t) >>= function
        | `End | `Key (`Enter, _) -> Lwt.return_unit
        | `Timer ->
                let i = display_time ((!wx - 38)/2) ((!wy - 7)/2) in
                Term.image term i >>= fun () -> loop term (e, timer ())
        | `Resize dim ->
                let twx, twy = Term.size term in
                wx := twx; wy := twy;
                let i = display_time ((!wx - 38)/2) ((!wy - 7)/2) in
                Term.image term i >>= fun () -> loop term (event term, timer ())
        | _ -> loop term (event term, timer ())

let main () =
    let term = Term.create() in
    let twx, twy = Term.size term in
    wx := twx; wy := twy;
    let i = display_time ((!wx - 38)/2) ((!wy - 7)/2) in
    Term.image term i >>= fun () ->
        loop term (event term, timer ())

let () = Lwt_main.run @@ main ()
