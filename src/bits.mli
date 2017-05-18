(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Library for manipulating of bit vectors

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bits} *)

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

type bits

val length : bits -> int
external get : bits -> int -> bool = "cprim_bits_get_exn"
external set : bits -> int -> bool -> unit = "cprim_bits_set_exn"
val create : int -> bits
val make : int -> bool -> bits
val init : int -> (int -> bool) -> bits
val empty : bits
val copy : bits -> bits
val of_string : string -> bits
val to_string : bits -> string
external iteri: (int -> bool -> unit) -> bits -> unit = "cprim_bits_iteri_exn"
external iteri_on_val: (int -> unit) -> bits -> bool -> unit = "cprim_bits_iteri_on_val_exn"
external mapi_inplace: (int -> bool -> bool) -> bits -> bits = "cprim_bits_mapi_exn"
external mapi_on_val_inplace: (int -> bool) -> bits -> bool -> bits = "cprim_bits_mapi_on_val_exn"
external count_val: bits -> bool -> int = "cprim_bits_count_val" [@@noalloc]
val mapi : (int -> bool -> bool) -> bits -> bits
val mapi_on_val : (int -> bool) -> bits -> bool -> bits
val iter : (bool -> unit) -> bits -> unit
val map : (bool -> bool) -> bits -> bits
external index_no_exn : bits -> bool -> int = "cprim_bits_index" [@@noalloc]
external rindex_no_exn : bits -> bool -> int = "cprim_bits_rindex" [@@noalloc]
val index : bits -> bool -> int
val rindex : bits -> bool -> int

val extend : bits -> int -> int -> bits

external for_all_values : (int -> bool) -> bits -> bool -> bool = "cprim_bits_for_all_values_exn"
external exists_for_values : (int -> bool) -> bits -> bool -> bool  = "cprim_bits_exists_for_values_exn"

type t = bits
val equal : t -> t -> bool
val compare : t -> t -> int
  (*
  from Bytes:

  val sub : bytes -> int -> int -> bytes
  --val sub_string : bytes -> int -> int -> string
  val extend : bytes -> int -> int -> bytes
  val fill : bytes -> int -> int -> char -> unit
  val blit : bytes -> int -> bytes -> int -> int -> unit
  --val blit_string : string -> int -> bytes -> int -> int -> unit
  val concat : bytes -> bytes list -> bytes
  val cat : bytes -> bytes -> bytes
  val trim : bytes -> bytes
  --val escaped : bytes -> bytes
  val index_from : bytes -> int -> char -> int
  val rindex_from : bytes -> int -> char -> int
  val contains : bytes -> char -> bool
  val contains_from : bytes -> int -> char -> bool
  val rcontains_from : bytes -> int -> char -> bool
  type t = bytes

  val unsafe_to_string : bytes -> string
  val unsafe_of_string : string -> bytes
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external unsafe_blit : bytes -> int -> bytes -> int -> int -> unit
    = "caml_blit_bytes" [@@noalloc]
  external unsafe_fill : bytes -> int -> int -> char -> unit
    = "caml_fill_bytes" [@@noalloc]
    *)

(** Next bitwise operations will work only on bit vectors of the same length.
    Each operation generates new bit vector as result.
    So arguments are immutable. *)

val ( land ) : bits -> bits -> bits
val ( lor ) : bits -> bits -> bits
val ( lxor ) : bits -> bits -> bits
val lnot : bits -> bits

(** Inplace variants of bitwise operations, first argument will be changed and
    returned as result. *)

val land_inplace : bits -> bits -> bits
val lor_inplace : bits -> bits -> bits
val lxor_inplace : bits -> bits -> bits
external lnot_inplace : bits -> bits = "cprim_bits_not" [@@noalloc]

external all_zeros : bits -> bool = "cprim_bits_all_zeros" [@@noalloc]
external all_ones : bits -> bool = "cprim_bits_all_ones" [@@noalloc]

(** subset a b - checks if a is subset of b in sense that all set bits in a
    also set in b *)
external subset : bits -> bits -> bool = "cprim_bits_subset" [@@noalloc]
