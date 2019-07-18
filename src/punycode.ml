(* RFC 3492: IDNA Punycode implementation *)

(* From pages 8 through 9 in RFC 3492:
   Although the only restriction Punycode imposes on the input integers
   is that they be nonnegative, these parameters are especially designed
   to work well with Unicode [UNICODE] code points, which are integers
   in the range 0..10FFFF (but not D800..DFFF, which are reserved for
   use by the UTF-16 encoding of Unicode).  The basic code points are
   the ASCII [ASCII] code points (0..7F), of which U+002D (-) is the
   delimiter, and some of the others have digit-values as follows:
      code points    digit-values
      ------------   ----------------------
      41..5A (A-Z) =  0 to 25, respectively
      61..7A (a-z) =  0 to 25, respectively
      30..39 (0-9) = 26 to 35, respectively
*)

open Rresult

(* (* below is an incomplete attempt at implementing a generic
      Bootstrap implementation functorized over the Bootstrap parameters.
   PRs welcome! :-)
   *)
module type Bootstring_parameters =
sig
  (* Integer functions *)
  type t
  val of_int : int -> t
  val of_char : char -> t
  val ( / ) : t -> t -> t (* division *)
  val ( + ) : t -> t -> t (* addition *)
  val ( - ) : t -> t -> t (* subtraction *)
  val ( * ) : t -> t -> t (* multiplication *)
  val ( % ) : t -> t -> t (* remainder / modulo *)
  val one : t
  val zero : t

  val base : t
  val tmin : t
  val tmax : t
  val skew : t
  val damp : t
  val initial_bias : t
  val initial_n : t
  (* val delimiter : t  --- if this API was constructed to work on lists of codepoints rather than octet strings *)
(*
   The initial value of n cannot be greater than the minimum non-basic
   code point that could appear in extended strings.

   The remaining five parameters (tmin, tmax, skew, damp, and the
   initial value of bias) need to satisfy the following constraints:

      0 <= tmin <= tmax <= base-1
      skew >= 1
      damp >= 2
      initial_bias mod base <= base - tmin
*)
end

module Punycode_bootstring : Bootstring_parameters =
struct
include Int32
let ( % ) = rem
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( / ) = div
let of_char c = int_of_char c |> of_int

let base = 36l
let tmin = 1l
let tmax = 26l
let skew = 38l
let damp = 700l
let initial_bias = 72l
let initial_n = 0x80l
end

module Bootstring
(* Bootstring uses little-endian ordering *)
: functor (Parameters : Bootstring_parameters) ->
sig
(* to_utf8 *)
(* to_ascii *)
val decode : string -> ((Parameters.t * Parameters.t) list, [ `Invalid_input | `No_xn_start | `Contains_complex_characters
| `Canonical_violation
| `No_dashes]) result

end = functor (Parameters : Bootstring_parameters) ->
struct

  let adapt delta numpoints (firsttime:bool) =
    let open Parameters in
    let delta =
      if firsttime
      then delta / Parameters.damp
      else delta / (of_int 2)
    in
    let delta = delta + (delta / numpoints) in
    let k = zero in
    let rec loop delta k =
      begin match delta > ((base - tmin) * tmax) / (of_int 2) with
      | false -> (delta, k)
      | true ->
        let delta = delta / (base - tmin) in
        let k = k + base in
        loop delta k
      end
    in
    let (delta,k) = loop delta k in
    k + (((base - tmin + one) * delta) / (delta + skew))

  (* used in [decode] *)
  let rec for_k ~bias ~complex ~char_ptr ~i ~w k =
    let open Parameters in
       if char_ptr >= String.length complex then None else
       let digit = of_char complex.[char_ptr] in (*TODO fix *)
       let new_char_ptr = Pervasives.(char_ptr + 1) in
       let new_i = i + (digit * w) in (* TODO "fail on overflow" *)
       let t = begin match () with
               | () when k <= bias + tmin -> tmin
               | () when k >= bias + tmax -> tmax
               | () -> k - bias
               end
       in
       if digit < t
       then Some (new_char_ptr, new_i)
       else for_k ~bias ~complex ~char_ptr:new_char_ptr ~i:(new_i)
                   ~w:(w*(base - t)) (* TODO "fail on overflow" *)
                   (k+base)

   let rec decode_loop ~complex ~bias ~char_ptr ~i ~n ~output_lst ~output_plain =
     let open Parameters in
     let output_plain_len = of_int @@ String.length output_plain in
     if char_ptr >= String.length complex then R.ok output_lst else

     let oldi = i in

     begin match for_k ~bias ~complex
                       ~char_ptr
                       ~i:oldi
                       ~w:one
                       base(*k=base to infinity in steps of base*)
     with
     | None -> R.error `Invalid_input
     | Some v -> R.ok v
     end
     >>= fun (char_ptr, i) ->
     let output_len = output_plain_len + (of_int @@ List.length output_lst) in
     let bias = adapt (i-oldi) (output_len + one) (oldi = zero) in
     let n = n + i / (output_len + one) in (* fail on overflow *)
     let i = i % (output_len + one) in
     (* if n is a basic codepoint then fail *)
     if n < (of_int 0x7f) then R.error `Canonical_violation else R.ok ()
     >>= fun () ->
     (* insert n into output at position i *)
     let output_lst = (i,n)::output_lst in
     (* increment i [[[ We do this at the beginning of the loop]]] *)
     decode_loop ~complex ~bias ~char_ptr ~i ~n ~output_lst ~output_plain

  let decode input_str =
   let open Parameters in
   let n = Parameters.initial_n in
   (*let i = zero in*)
   let bias = Parameters.initial_bias in
   (* consume all code points before the last delimiter (if there is one)
     and copy them to output, fail on any non-basic code point
   *)
   if not @@ String.for_all (function '\x80'..'\xff' -> false | '\x00'..'\x7f' -> true) input_str
   then R.error `Contains_complex_characters
   else R.ok ()
   >>= fun () ->
   begin match String.cut ~sep:"xn--" input_str with
   | Some ("", input_str) -> R.ok input_str
   | _ -> R.error `No_xn_start
   end
   >>= fun input_str ->
   begin match String.cut ~rev:true ~sep:"-" input_str with
   | None -> R.error `No_dashes
   | Some x -> R.ok x
   end
   >>= fun (output_plain,complex) ->
   let output_plain_len = of_int @@ String.length output_plain in
   let i = output_plain_len in

   (*
   if more than zero code points were consumed then consume one more
     (which will be the last delimiter)
   *)
   let i = if i <> zero then i + one else i in

   decode_loop ~output_plain ~complex ~bias
           ~char_ptr:0 (*char_ptr*)
           ~i
           ~n
           ~output_lst:[] (* output_lst *)

end

module Punycode2 = Bootstring(Punycode_bootstring)
                   *)

module Pervasives_Buffer = Buffer
(* keep reference after Astring.Buffer is opened *)
open Astring

type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

let pp_illegal_label fmt err =
  Format.fprintf fmt "Illegal label: %s"
    (match err with
     | Illegal_label_size str -> strf "Invalid size: %S" str
     | Label_contains_illegal_character str ->
       strf "Contains invalid char: %S" str
     | Label_starts_with_illegal_character char ->
       strf "Starts with invalid char: %C" char
     | Label_ends_with_hyphen str -> strf "Ends with hyphen/dash: %S" str
    )

type punycode_decode_error =
  | Overflow_error
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

let msg_of_decode_error = function
  | Overflow_error -> R.msg "Unspecified overflow error"
  | Invalid_domain_name str -> R.msgf "Invalid domain name: %S" str
  | Illegal_label label_error -> R.msgf "%a" pp_illegal_label label_error

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Invalid_domain_name of string
  | Illegal_label of illegal_ascii_label

let msg_of_encode_error = function
  | Malformed_utf8_input str -> R.msgf "Malformed UTF-8 input: %S" str
  | Overflow -> R.msg "Unexpected overflow"
  | Invalid_domain_name str -> R.msgf "Invalid domain name: %S" str
  | Illegal_label label_error ->
    R.msgf "Illegal ascii label: %a" pp_illegal_label label_error

let is_valid_ascii_label label
  : (string , illegal_ascii_label) Rresult.result =
  (* validate length and character set in ascii DNS label *)
  begin
    if String.(length label > 0 && length label <= 63)
    then R.ok ()
    else R.error @@ Illegal_label_size label
  end
  >>= fun () ->
  begin match label.[0] , label.[pred @@ String.length label] with
    | _ , '-' -> R.error @@ Label_ends_with_hyphen label
    | '0'..'9' , _
    | 'a'..'z' , _
    | '_' , _ (* TODO this is invalid for some types of dns records, but ok for others *)
    | 'A'..'Z' , _ -> R.ok ()
    | c , _ -> R.error @@ Label_starts_with_illegal_character c
  end
  >>= fun () ->
  begin match
      label
      |> String.for_all
        (function
          | 'a'..'z'
          | 'A'..'Z'
          | '0'..'9'
          | '-'
          | '_' (* TODO decide if we should notify the user of underscores *)
            -> true
          | _ -> false)
    with
    | true -> R.ok label
    | false -> R.error @@ Label_contains_illegal_character label
  end

(* constants from RFC 3492 *)
let initial_n = 0x80
let initial_bias = 72
let delimiter_int = 0x2D
let base = 36
let damp = 700
let tmin = 1
let tmax = 26
let skew = 38
let punycode_max_int = 0x7f_ff_ff_ff


let lst_to_string lst =
  let b = Pervasives_Buffer.create 64 in
  let () = List.iter (Uutf.Buffer.add_utf_8 b) lst in
  Pervasives_Buffer.contents b

let decode_digit cp =
  let cp_int = int_of_char cp in
  match cp with
  | '\x00' .. '\x39' (* 00..57 ported from cp - 48 < 10 *)
    -> cp_int - 22
  | '\x3a' .. '\x5a' (* 58..90 ported from cp - 65 < 26 *)
    -> cp_int - 65
  | '\x5b' .. '\x7a' (* 91..122 ported from cp - 97 < 26 *)
    -> cp_int - 97
  | '\x7b' .. '\xFF' (* 123..255 else / default case *)
    -> base

let encode_digit d uppercase_flag =
  (* d + 22 + 75 * (d < 26) - ((flag != 0) << 5) *)
  match uppercase_flag , char_of_int d with
  | false , '\x00' .. '\x19' -> (* 00..25: lowercase a..z *)
     d + 0x61 (* d + 'a' *)
  | true , '\x00' .. '\x19' -> (* 00..25: uppercase a..z *)
     d + 0x41 (* d + 'A' *)
  | false , '\x1a' .. '\x23' -> (* 26..35: lowercase 0..9 *)
     (d - 26) + 0x30 (* d-26 + '0' *)
  | _ , ch ->
    failwith (__LOC__ ^ ": trying to decode invalid digit: "
              ^ (Char.Ascii.escape_char ch))

let adapt delta numpoints firsttime =
  let delta = match firsttime with
    | true -> (delta / damp)
    | false -> (delta lsr 1)
  in
  let delta = delta + (delta / numpoints) in
  let delta , k =
    let rec foo ~delta ~k =
      if delta > (((base - tmin) * tmax) / 2) then
        let delta = delta / (base - tmin) in
        let k = k + base in
        foo ~delta ~k
      else
        delta , k
    in
    foo ~delta ~k:0
  in
  k + (base - tmin + 1) * delta / (delta + skew)

module ImmutArray : sig
  type backing = Uchar.t
  type 'a t constraint 'a = backing
  val get : backing t -> int -> backing
  val len : backing t -> int
  val fold_left : (int -> backing -> int) -> int -> backing t -> int
  val of_utf8 : (string -> 'malformed_error) -> string ->
    ('acc -> Uchar.t -> 'acc) -> 'acc ->
    (Uchar.t t * 'acc, 'malformed_error) Rresult.result
  val of_utf8_save_basic_codepoints : (string -> 'malformed_error) -> string ->
    (Uchar.t t * (Uchar.t list * int), 'malformed_error) Rresult.result
  (*val span : 'a t -> int -> 'a t * 'a t*)
  val insert_left : Uchar.t t -> int -> Uchar.t -> Uchar.t t
  (*val of_array : Uchar.t array -> Uchar.t t
    val to_array : 'a t -> 'a array*)
  val to_list : 'a t -> 'a list
  val to_utf8 : Uchar.t t -> string
end = struct
  type backing = Uchar.t
  type 'a t = {arr : Uchar.t array ; len: int} constraint 'a = backing
  (*let to_utf8 t =
    (* this turns out to be slower than lst_to_string (to_list t) ... *)
    let b = Pervasives_Buffer.create 64 in
    for i = 0 to t.len - 1 do
      Uutf.Buffer.add_utf_8 b t.arr.(i)
    done ;
    Pervasives_Buffer.contents b*)
  (*let of_array old_arr = {arr = Array.copy old_arr ; len = Array.length old_arr}
    let to_array {arr ; len} = Array.sub arr 0 len |> Array.copy*)
  let get t idx =
    if idx < t.len then t.arr.(idx)
    else failwith "ImmutArray: index out of bounds"
  let len {len ; _} = len
  let insert_left t left_off uchar =
    let fresh = Array.make (t.len+1) (Uchar.of_int 0) in
    Array.blit t.arr 0 fresh 0 left_off ;
    fresh.(left_off) <- uchar ;
    Array.blit t.arr left_off fresh (left_off+1) (t.len-left_off) ;
    { arr = fresh ; len = t.len+1 }
  let to_list {arr ; len } = Array.sub arr 0 len |> Array.to_list
  let fold_left cb initial_acc t =
    let acc = ref initial_acc in
    for i = 0 to t.len -1 do
      acc := cb !acc t.arr.(i)
    done ; !acc
  let of_utf8 malformed_error utf8_str callback initial_acc =
    let array = Array.make (String.length utf8_str) (Uchar.of_int 0) in
    let ptr = ref 0 in
    Uutf.String.fold_utf_8
      (fun acc _codepoint_idx -> function
         | `Malformed s -> R.error (malformed_error s)
         | `Uchar c ->
           acc >>= fun (callback_acc (*basic_codepoints, basic_len*)) ->
           array.(!ptr) <- c ;
           incr ptr ;
           Ok (callback callback_acc c)
      )
      (R.ok (initial_acc)) utf8_str
    >>| fun (callback_acc) ->
    {arr = array ; len = !ptr }, callback_acc
  let of_utf8_save_basic_codepoints malformed_error utf8_str =
    of_utf8 malformed_error utf8_str
      (fun (basic_codepoints, basic_len) c ->
         if Uchar.to_int c < 0x80
         then (c::basic_codepoints), succ basic_len
         else basic_codepoints, basic_len
      ) ([], 0)
  let to_utf8 t =
    let buf = Buffer.create (len t) in
    fold_left (fun () uchar -> Uutf.Buffer.add_utf_8 buf uchar) () t;
    Buffer.contents buf
end

let decode input (* preserveCase TODO *)
  : (Uchar.t ImmutArray.t, punycode_decode_error) Rresult.result =
  let basic_codepoints, complex_codepoints =
    (* string component after RIGHTMOST dash is the complex codepoints.
       If no dash is found, the label is assumed to be "basic". *)
    match String.cut ~rev:true ~sep:"-" input with
    | None -> input, ""
    | Some basic_complex_pair -> basic_complex_pair
  in
  let initial_value_output_len, initial_value_output =
    ImmutArray.of_utf8 (fun s -> s) basic_codepoints (fun () _b -> ()) ()
    |> R.get_ok |> fun (foo,()) -> ImmutArray.len foo, foo
  in
  let max_ic = String.length complex_codepoints in

  let rec f ~ic ~n ~i ~w ~k ~value_output ~value_output_len ~bias =
    if ic >= max_ic then R.ok value_output
    else
      let oldi = i in
      let rec ff_inner ~ic ~w ~k ~i = (* while ic < input.length *)
        if ic >= max_ic then R.error Overflow_error
        else
          let digit = decode_digit complex_codepoints.[ic] in
          let ic = ic + 1 in
          (* digit >= base && raise RangeError *)
          (* digit > Math.floor((punycode_max_int - i) / w) && raise Overflow *)
          let i = i + (digit * w) in
          let t = match k with
            (* t = k <= bias ? tmin : k >= bias + tmax ? tmax : k - bias *)
            | _ when k <= bias -> tmin
            | _ when k >= bias + tmax -> tmax
            | _ -> k - bias
          in
          if digit < t then R.ok (n , i , oldi, ic)
          else (* if not, keep looping over characters: *)
            (* if w > Math.floor(punycode_max_int / (base -t )) then raise Overflow *)
            let w = w * (base - t)
            and k = k + base in
            ff_inner ~ic ~w ~k ~i
      in
      ff_inner ~ic ~w ~k ~i
      >>= fun (n , i, oldi, ic) ->
      let out = value_output_len + 1 in (* "out" == new length of output *)
      let bias = adapt (i-oldi) out (oldi = 0) in
      let n = n + (i / out) in
      begin if n > punycode_max_int
        then R.error Overflow_error
        else R.ok ()
      end >>= fun () ->
      let i = i mod out in
      let value_output =
        let insertion_point = i in
        ImmutArray.insert_left value_output insertion_point (Uchar.of_int n) in
      f ~n ~i:(i+1) ~value_output ~value_output_len:out ~ic ~w ~k ~bias
  in
  f ~n:initial_n ~i:0 ~w:1 ~k:base ~ic:0
    ~value_output:initial_value_output
    ~value_output_len:initial_value_output_len
    ~bias:initial_bias

let encode input_utf8 : (string, punycode_encode_error) Rresult.result =
  ImmutArray.of_utf8_save_basic_codepoints
    (fun s -> Malformed_utf8_input s) input_utf8
  >>= fun (input, (basic_codepoints, basic_codepoints_len)) ->
  (* restore order after fold_utf_8 which reverses it: *)
  let initial_value_output : Uchar.t list =
    (* initial value of output is prefixed with the ASCII characters and '-'*)
    if basic_codepoints <> []
    then Uchar.of_int delimiter_int :: basic_codepoints
    else [] (* if the string only contains complex characters, no '-' prefix *)
  in
  (* main encoding loop: *)
  let rec f ~n ~h ~delta ~bias ~(value_output:Uchar.t list)
    : ('a list , punycode_encode_error) Rresult.result =
    if h >= ImmutArray.len input then R.ok value_output
    else
      let m = ImmutArray.fold_left (* find lowest codepoint that in input: *)
          (fun m -> fun uchar ->
             let char = Uchar.to_int uchar in
             if char >= n then min char m else m)
          punycode_max_int input in
      begin match m - n > ( (punycode_max_int - delta) / (h+1)) with
        | true -> R.error Overflow
        | false -> R.ok ()
      end
      >>= fun () ->
      let delta = delta + ( (m - n) * (h + 1) ) in
      let n : int = m in
      let rec f_inner ~ic ~delta ~bias ~(n:int) ~h
          ~(value_output:Uchar.t list) =
        (* input.each_with_index do |char, j| *)
        if ic >= ImmutArray.len input
        then R.ok ((delta , bias , n , h , value_output))
        else
        let char = ImmutArray.get input ic in
        begin match delta + 1 with
          | new_delta when Uchar.to_int char < n -> R.ok new_delta
          | new_delta when new_delta > punycode_max_int
            -> R.error Overflow
          | _ -> R.ok delta
        end
        >>= fun delta ->
        let value_output , bias , delta , h =
          begin
            if Uchar.to_int char = n then
              (* while true do *)
              let value_output =
                let rec fff ~(value_output:Uchar.t list) ~k ~q =
                  let t = match 0 with
                    | _ when k <= bias -> tmin
                    | _ when k >= bias + tmax -> tmax
                    | _ -> k - bias
                  in
                  if q < t
                  then
                    (Uchar.of_int @@ encode_digit q false)::value_output
                  else
                    let value_output =
                      ( Uchar.of_int @@
                        encode_digit (t + ((q-t) mod (base - t))) false
                        (*todo should be false?*)
                      )::value_output
                    in
                    let q = (q-t) / (base -t) in
                    let k = k + base in
                    fff ~value_output ~k ~q
                in
                fff ~value_output ~k:base ~q:delta
              in
              let bias = adapt delta (h + 1)
                  (h = basic_codepoints_len) in
              let delta = 0 in
              let h = h+1 in
              (value_output , bias , delta , h)
            else
              (value_output , bias , delta , h)
          end
        in
        f_inner ~ic:(ic+1) ~delta ~bias ~n ~h ~value_output
      in
      f_inner ~ic:0 ~delta ~bias ~n ~h ~value_output (* TODO *)
      >>= fun (delta , bias , n , h , value_output) ->
      f ~n:(n+1) ~h ~delta:(delta+1) ~bias ~value_output
  in
  f ~n:initial_n
    ~h:basic_codepoints_len
    ~delta:0
    ~bias:initial_bias
    ~value_output:initial_value_output
  >>| List.rev >>| lst_to_string

let internal_to_domain_name domain
  : (bool * [`raw] Domain_name.t, punycode_encode_error) Rresult.result =
  let for_each_label acc label =
    match acc , label with
    | Error _ , _ -> acc
    | Ok (0, acc) , "" ->
      Ok (0, ""::acc) (* allow empty label at the end*)
    | Ok _ , ""  -> (* domain name contains ".." somewhere *)
      Error (Illegal_label (Illegal_label_size "contains empty label"))
    | Ok (remaining_labels, acc) , _  ->
      if String.for_all (function '\x00' .. '\x7F' -> true | _ -> false) label
      then is_valid_ascii_label label
           |> R.reword_error (function e -> Illegal_label e)
        >>= fun _ -> Ok (pred remaining_labels, label :: acc)
      else
        begin
          match encode label with
          | (Error _) as err -> err
          | Ok encoded ->
            is_valid_ascii_label
              ("xn--" ^ encoded)
            |> R.reword_error (function e -> Illegal_label e)
            >>| fun encoded_str ->
            pred remaining_labels, encoded_str :: acc
        end
  in
  begin match String.length domain with
    | len when len > 0 && len <= 255*4 -> (* rough limit on utf-8 strings *)
      Ok ()
    | len when len = 0 -> R.error @@ Illegal_label (Illegal_label_size domain)
    | _ -> R.error @@ Invalid_domain_name domain
  end >>= fun () ->
  let labels = String.cuts ~sep:"." domain in
  List.fold_left (for_each_label) (Ok (List.length labels -1, [])) labels
  >>= fun (_, encoded_labels) ->
  let has_dot, encoded_labels = match encoded_labels with
    | ""::tl -> true, List.rev tl
    | tl -> false, List.rev tl in
  Domain_name.of_strings encoded_labels
  |> R.reword_error (fun (`Msg msg) -> Invalid_domain_name
                        (msg^": "^(String.concat ~sep:"."
                                     (List.map String.Ascii.escape_string
                                        encoded_labels ))))
  >>| fun domain -> has_dot, domain

let to_encoded_domain_name string =
  internal_to_domain_name string >>| snd

let to_ascii string : (string, punycode_encode_error) result =
  internal_to_domain_name string >>| fun (has_dot, domain) ->
  match Domain_name.to_string domain with
  | encoded_str when has_dot -> encoded_str ^ "."
  | encoded_str -> encoded_str

let of_domain_labels (labels:string list) =
  let for_each_label acc label =
    acc >>= fun acc ->
    if String.is_prefix ~affix:"xn--" label then begin
        ( is_valid_ascii_label label
        |> R.reword_error (function e ->
            (Illegal_label e :punycode_decode_error))
        >>| String.drop ~max:4 >>| fun stripped ->
        if not (String.is_infix ~affix:"-" stripped)
        then "-" ^ stripped (* this contains ONLY punycode-stuff *)
        else stripped ) >>= fun label ->
        begin match decode label with
          | (Error _) as err -> err
          | Ok decoded ->
            R.ok @@ decoded :: acc
        end
      end
    else (* not punycode-encoded: *)
      ImmutArray.of_utf8
        (fun s -> (Invalid_domain_name
                     ("Invalid ASCII label: " ^ String.Ascii.escape s)
                   : punycode_decode_error))
        label (fun () _ -> ()) () >>| fst
      >>| fun label -> label::acc
  in
  (((Domain_name.of_strings labels >>| Domain_name.to_strings
     |> R.reword_error (fun (`Msg msg) ->
         (Invalid_domain_name msg : punycode_decode_error)
       ) >>= List.fold_left for_each_label R.(ok []))
    >>= (fun fqdn ->
        let decoded_labels = match fqdn with
          | hd::decoded_labels when ImmutArray.len hd = 0 ->
            decoded_labels
          | decoded_labels -> decoded_labels
        in
        let len = List.fold_left
            (fun acc s -> acc + ImmutArray.len s) 0 decoded_labels in
        if 253 >= len && 0 < len
        then Ok (List.rev fqdn)
        else
          Error (Invalid_domain_name "decoded size invalid"
                 : punycode_decode_error))))

let of_domain_name (domain:_ Domain_name.t) =
  (Domain_name.(to_strings (raw domain)) |> of_domain_labels)
  >>| fun x -> List.map ImmutArray.to_list x

let to_utf8_list domain =
  (Domain_name.of_string domain
   |> R.reword_error (fun (`Msg x) ->
       (Invalid_domain_name x : punycode_decode_error))
  ) >>| Domain_name.to_strings >>= of_domain_labels
  >>| List.map ImmutArray.to_utf8

let to_utf8 domain =
  let domain, is_fqdn = match String.head ~rev:true domain with
    | Some '.' -> String.span ~rev:true ~max:1 domain |> fst, true
    | _ -> domain, false
  in
  to_utf8_list domain
  >>| (fun lst -> if is_fqdn then lst@[""] else lst)
  >>| String.concat ~sep:"."
