(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

  type bits = {length : int; data : bytes}
  let length b = b.length
  external get : bits -> int -> bool = "cprim_bits_get_exn"
  external set : bits -> int -> bool -> unit = "cprim_bits_set_exn"
  external iteri: (int -> bool -> unit) -> bits -> unit = "cprim_bits_iteri_exn"
  external iteri_on_val: (int -> unit) -> bits -> bool -> unit = "cprim_bits_iteri_on_val_exn"
  external mapi_on_val_inplace: (int -> bool) -> bits -> bool -> bits = "cprim_bits_mapi_on_val_exn"
  external mapi_inplace: (int -> bool -> bool) -> bits -> bits = "cprim_bits_mapi_exn"
  external count_val: bits -> bool -> int = "cprim_bits_count_val" [@@noalloc]

  let empty = {length = 0; data = Bytes.empty}
  let copy b = {length = b.length; data = Bytes.copy b.data}

  let mapi f b = mapi_inplace f (copy b)
  let mapi_on_val f b v = mapi_on_val_inplace f (copy b) v
  let iter f b = iteri (fun _ v -> f v) b
  let map f b = mapi (fun _ v -> f v) b
  let create n = {length = n; data = Bytes.create ((n + 7) lsr 3) }
  let make n v = {length = n; data = Bytes.make ((n + 7) lsr 3) (char_of_int (if v then 255 else 0)) }
  let init n f = mapi_inplace (fun i _ -> f i) (create n)

  let of_string s = let d = Bytes.of_string s in
                    let len = Bytes.length d in
                    let last_char = Bytes.get d (len - 1) in
                    let rest_bits = (int_of_char last_char) - (int_of_char '0') in
                    {length = ((len - 2) lsl 3) + rest_bits; data = d}

  let to_string b = let s = Bytes.to_string (Bytes.sub b.data 0 ((b.length + 7) lsr 3)) in
                    let rest_bits = b.length land 7 in
                    let last_char = char_of_int ((int_of_char '0') + rest_bits) in
                    s ^ (String.make 1 last_char)

  external for_all_values : (int -> bool) -> bits -> bool -> bool = "cprim_bits_for_all_values_exn"
  external exists_for_values : (int -> bool) -> bits -> bool -> bool  = "cprim_bits_exists_for_values_exn"
  external index_no_exn : bits -> bool -> int = "cprim_bits_index" [@@noalloc]
  external rindex_no_exn : bits -> bool -> int = "cprim_bits_rindex" [@@noalloc]
  let index b v = let idx = index_no_exn b v in if (idx >= 0) then idx else raise Not_found
  let rindex b v = let idx = rindex_no_exn b v in if (idx >= 0) then idx else raise Not_found

  let extend b at_left at_right=
    match at_left, at_right with
    | 0, 0 -> b
    | 0, n when n > 0 ->
      let new_data = Bytes.extend b.data 0 ((n + 7 - ((8 - (b.length land 7)) land 7)) lsr 3) in
         let new_data_first_byte_offset = Bytes.length b.data in
           Bytes.fill new_data new_data_first_byte_offset ((Bytes.length new_data ) - new_data_first_byte_offset) (char_of_int 0);
           { length = b.length + n; data = new_data }
    | _, _ -> raise (Invalid_argument "Bits.extend")

  type t = bits

  (* TODO: equal is incorrect, because of arbitrary values of rest unused bits in last byte*)
  let equal b1 b2 = b1.length == b2.length &&
                    Bytes.equal b1.data b2.data
  let compare b1 b2 = if b1.length > b2.length then 1 else
                      if b1.length < b2.length then -1 else
                      Bytes.compare b1.data b2.data

  (* special functions *)
  external bits_op : bits -> bits -> int -> bits = "cprim_bits_op"
  let op b1 b2 n = bits_op (copy b1) b2 n
  let ( land ) b1 b2 = op b1 b2 0
  let ( lor ) b1 b2 = op b1 b2 1
  let ( lxor ) b1 b2 = op b1 b2 2

  let land_inplace b1 b2 = bits_op b1 b2 0
  let lor_inplace b1 b2 = bits_op b1 b2 1
  let lxor_inplace b1 b2 = bits_op b1 b2 2

  external lnot_inplace : bits -> bits = "cprim_bits_not" [@@noalloc]
  let lnot b = lnot_inplace (copy b)
