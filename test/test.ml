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

let open Core.Std in
  let open Core_bench.Std in
      Command.run (Bench.make_command [
        Bench.Test.create_indexed ~name:"Bits.create" ~args:[1;10;1000;10000;100000;1000000]
          (fun len ->
            Staged.stage
              (fun () -> ignore (Bits.create len)));
        Bench.Test.create_indexed ~name:"Bits.lnot_inplace" ~args:[1;10;1000;10000;100000;1000000]
          (fun len ->
            let b = Bits.create len in
              Staged.stage
               (fun () -> ignore (Bits.lnot_inplace b)));
        Bench.Test.create_indexed ~name:"Bits.lxor_inplace" ~args:[1;10;1000;10000;100000;1000000]
          (fun len ->
            let b,b1 = Bits.create len, Bits.create len in
              Staged.stage
               (fun () -> ignore (Bits.lxor_inplace b b1)));
      ])
