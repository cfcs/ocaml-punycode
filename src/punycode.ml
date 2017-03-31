(* RFC 3492: IDNA Punycode implementation *)

(* KNOWN bugs:
** - domain names with trailing dots are rejected

*)

module Pervasives_Buffer = Buffer (* keep reference after Astring.Buffer is opened *)
open Astring
open Rresult

type illegal_ascii_label =
  | Illegal_label_size of string
  | Label_contains_illegal_character of string
  | Label_starts_with_illegal_character of char
  | Label_ends_with_hyphen of string

type punycode_decode_error =
  | Illegal_basic_codepoints (* TODO this should be caught in is_valid_ascii_label ; ideally never used. *)
  | Overflow_error
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

type punycode_encode_error =
  | Malformed_utf8_input of string
  | Overflow
  | Domain_name_too_long of string
  | Illegal_label of illegal_ascii_label

let is_valid_ascii_label label : (string , illegal_ascii_label) Rresult.result =
  (* validate length and character set in ascii DNS label *)
  begin
    if String.(length label > 0 && length label < 63)
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
  let b = Pervasives_Buffer.create 0 in
  let () = List.(iter (Uutf.Buffer.add_utf_8 b) lst) in
  Pervasives_Buffer.to_bytes b |> Bytes.to_string
			 
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
  | _ , _ -> failwith "should not happen"

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

let decode input (* preserveCase *) : (int list, punycode_decode_error) Rresult.result =
  let basic_codepoints, complex_codepoints =
    let basic_codepoints_len =
      match String.find ~rev:true (function '-' -> true | _ -> false) input with
      | Some x -> x
      | None -> 0
    in
    String.span ~max:basic_codepoints_len input
  in
  let complex_codepoints = String.drop ~max:1 complex_codepoints in (* get rid of the '-' *)

    let f acc c =
      begin match c, acc with
      | 'A' .. 'Z' , Ok acc (* set uppercase flag*)
        -> R.ok @@ (true, int_of_char c)::acc
      | '\x00' .. '\x7F' , Ok acc
        -> R.ok @@ (false, int_of_char c)::acc
      | _ , Error _
      | '\x80' .. '\xFF' , Ok _
        -> R.error Illegal_basic_codepoints (* TODO this should be caught in is_valid_ascii_label ; consider refactoring *)
      end
    in String.fold_left f (R.ok []) basic_codepoints
  >>= fun filtered_basic_codepoints ->
    let _ (* uppercase_flags *) , basic_codepoints =
      List.split filtered_basic_codepoints
    in
    let rec f ~ic ~n ~i ~w ~k ~value_output ~bias =
      if ic >= String.length complex_codepoints then R.ok value_output
      else
      let oldi = i in
      let rec ff_inner ~ic ~w ~k ~i = (* while ic < input.length *)
	if ic >= String.length complex_codepoints then R.error Overflow_error
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
      let out = List.(length value_output) + 1 in
      let bias = adapt (i-oldi) out (oldi = 0) in
      begin
	if i / out > punycode_max_int - n
	then R.error Overflow_error
	else R.ok ()
      end
      >>= fun () ->
      let n = n + (i / out) in
      let i = i mod out in
      let value_output =
	let rec span i fst = function
	  | tl when i = 0 -> List.(rev fst) , tl
	  | [] -> List.(rev fst) , []
	  | hd::tl -> span (pred i) (hd::fst) tl
	in
	let fst , snd = span i [] value_output in
	fst @ [n] @ snd
      in
      let i = i +1 in
      f ~n ~i ~value_output ~ic ~w ~k ~bias
    in
    f ~n:initial_n ~i:0 ~w:1 ~k:base ~ic:0 ~value_output:List.(rev basic_codepoints) ~bias:initial_bias

let encode input_utf8 : ('a , punycode_encode_error) Rresult.result =
  let open Result in
  Uutf.String.fold_utf_8
   (function acc -> fun _ (*index*) -> fun c ->
     begin match acc , c with
   | Error _ , _ -> acc
   | Ok (input, basic_codepoints) , `Uchar c ->
      R.ok begin match c with
		 | _ when c < 0x80 ->
		    (c :: input) , (c::basic_codepoints)
		 | _ -> (c :: input) , basic_codepoints
	   end
   | Ok _ , `Malformed s -> R.error (Malformed_utf8_input s)
     end
   )
   (R.ok ([], [])) input_utf8
  >>= fun (input , basic_codepoints) ->
  let input = List.rev input in
  let basic_codepoints = List.rev basic_codepoints in
  (* main encoding loop: *)
  let rec f ~n ~h ~delta ~bias ~(value_output:Uutf.uchar list) : (Uutf.uchar list , punycode_encode_error) Rresult.result =
    if h >= List.(length input) then R.ok value_output
    else
      let m = List.fold_left
		(fun m -> fun char ->
			  if char >= n && char <= m
			  then char else m
		)
		punycode_max_int input in
      begin match m - n > ((punycode_max_int - delta) / (h+1)) with
	    | true -> R.error Overflow
	    | false -> R.ok ()
      end
      >>= fun () ->
      let delta = delta + ( (m - n) * (h + 1) ) in
      let n = m in
      let rec f_inner ~ic ~delta ~bias ~n ~h ~(value_output:Uutf.uchar list) =
	(* input.each_with_index do |char, j| *)
	if ic >= List.length input then R.ok ((delta , bias , n , h , value_output))
	else
        let char = List.nth input ic in (* TODO guard against overflow *)
	begin match delta + 1 with
	      | new_delta when char < n -> R.ok new_delta
	      | new_delta when new_delta > punycode_max_int
		-> R.error Overflow
	      | _ -> R.ok delta
	end
	>>= fun delta ->
	let value_output , bias , delta , h =
	  begin
	    if char = n then
	      (* while true do *)
	      let value_output =
		let rec fff ~value_output ~k ~q =
		  let t = match 0 with
		    | _ when k <= bias -> tmin
		    | _ when k >= bias + tmax -> tmax
		    | _ -> k - bias
		  in
		  if q < t
		  then
		    let value_output = value_output @ [encode_digit q false] in
		    value_output
		  else
		    let value_output = value_output @ [encode_digit (t + ((q-t) mod (base - t))) false (*todo should be false?*) ] in
		    let q = (q-t) / (base -t) in
		    let k = k + base in
		    fff ~value_output ~k ~q
		in
		fff ~value_output ~k:base ~q:delta
	      in
              let bias = adapt delta (h + 1) (h = List.length basic_codepoints) in
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
      let n = n + 1 in
      f ~n ~h ~delta:(delta+1) ~bias ~value_output
  in
  let value_output =
    (* initial value of output is prefixed with the ASCII characters and '-'*)
    if List.length basic_codepoints > 0
    then basic_codepoints @ [delimiter_int]
    else [] (* TODO if the string only contains complex characters, should it be prefixed with a '-' ? *)
  in
  f ~n:initial_n
    ~h:List.(length basic_codepoints)
    ~delta:0
    ~bias:initial_bias
    ~value_output

let to_ascii domain : (string, punycode_encode_error) Rresult.result =
  let for_each_label acc label =
    match acc , label with
    | Error _ , _ -> acc
    | Ok _ , ""   -> R.error @@ Malformed_utf8_input label
    | Ok acc , _  ->
       if String.for_all (function '\x00' .. '\x7F' -> true | _ -> false) label
       then is_valid_ascii_label label
	    |> R.reword_error (function e -> Illegal_label e)
	    >>= fun _ -> R.ok (label :: acc)
       else
       begin
	 match encode label with
	 | Ok encoded ->
	    is_valid_ascii_label (lst_to_string encoded)
	    |> R.reword_error (function e -> Illegal_label e)
	    >>= fun encoded_str ->
	    R.ok @@ ("xn--" ^ encoded_str) :: acc
	 | (Error _) as err -> err
       end
  in
  begin match String.length domain with
	| len when len > 0 && len <= 253*4 (* rough limit on utf-8 strings *)
	  -> R.ok ()
        | len when len = 0 -> R.error @@ Illegal_label (Illegal_label_size domain)
	| _ -> R.error @@ Domain_name_too_long domain
  end
  >>= fun () ->
  String.cuts ~sep:"." domain
  |> List.fold_left for_each_label R.(ok [])
  >>= fun encoded_labels ->
  begin match
      List.rev encoded_labels
      |> String.concat ~sep:"."
    with
    | s when String.length s <= 253 -> R.ok s (* make sure the output is sane *)
    | s -> R.error @@ Domain_name_too_long s
  end

let to_unicode domain =
  let for_each_label acc label =
    begin
      match acc with
      | Error _ -> acc
      | Ok acc when String.is_prefix ~affix:"xn--" label
	->
	 is_valid_ascii_label label
	 |> R.reword_error (function e -> (Illegal_label e :punycode_decode_error))
	 >>= fun _ ->
	 begin
	   match
	     (String.drop ~max:4 label |> decode)
	   with
	   | (Error _) as err -> err
	   | Ok decoded
	     -> R.ok @@ (lst_to_string decoded) :: acc
	 end
      | Ok acc -> R.ok (label::acc)
    end
  in
  begin if String.(length domain > 0 && length domain <= 253)
	then R.ok ()
	else R.error (Domain_name_too_long domain : punycode_decode_error)
  end
  >>= fun () ->
  String.cuts ~sep:"." domain
  |> List.fold_left for_each_label R.(ok [])
  >>= fun decoded_labels ->
  List.rev decoded_labels
  |> String.concat ~sep:"."
  |> R.ok
