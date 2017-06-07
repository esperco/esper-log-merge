(*
   This merges multiple log files into one file, sorted by timestamp.

   The only different with `sort -m` is that the beginning of a merged
   item is a timestamp at the beginning of a line, which can be followed
   by more lines (that don't start with a timestamp).
*)

open Printf

(*
   Extract timestamp from lines formatted like "^[timestamp] ...$"
*)
let parse_timestamp line =
  try
    let subs = Pcre.exec ~pat:"^\\[(\\d[0-9T.:+-]*)\\]" line in
    Some (Util_time.of_string (Pcre.get_substring subs 1))
  with
  | Not_found -> None
  | Invalid_argument _ -> failwith ("Corrupt timestamp: " ^ line)

(*
   Inspect a line and determine if it's the beginning of a new item.
   Returns the key according to which the items are supposed to be sorted.
*)
let inspect_line line =
  match parse_timestamp line with
  | None -> None
  | Some x -> Some (Util_time.to_float x)

let items_of_lines stream =
  let rec read_item_lines t acc =
    match Stream.peek stream with
    | None ->
        Some (t, List.rev acc)
    | Some line ->
        match inspect_line line with
        | None ->
            Stream.junk stream;
            read_item_lines t (line :: acc)
        | Some _ ->
            Some (t, List.rev acc)
  in
  let rec read_item i =
    match Stream.peek stream with
    | None -> None
    | Some line ->
        match inspect_line line with
        | None ->
            (* ignore junk at beginning of file *)
            Stream.junk stream;
            read_item i
        | Some t ->
            Stream.junk stream;
            read_item_lines t [line]
  in
  Stream.from read_item

let items_of_file filename =
  let line_stream, close = Util_stream.stream_of_file filename in
  items_of_lines line_stream

(*
   Warning: there will be file descriptor leaks if this function
            raises an exception.
*)
let merge_files input_files opt_output_file =
  let kv_streams = BatList.map items_of_file input_files in
  let merged_stream = Util_stream.merge compare kv_streams in
  let oc, close =
    match opt_output_file with
    | None ->
        stdout, (fun () -> ())
    | Some file ->
        let oc = open_out file in
        oc, (fun () -> close_out_noerr oc)
  in
  Stream.iter (fun (t, lines) ->
    List.iter (fun line ->
      fprintf oc "%s\n" line
    ) lines
  ) merged_stream;
  close ()

module Test = struct
  let to_file filename lines =
    let oc = open_out filename in
    List.iter (fun s -> fprintf oc "%s\n" s) lines;
    close_out oc

  let of_file filename =
    let stream, close = Util_stream.stream_of_file filename in
    Util_stream.to_list stream

  let merge l =
    let input_files =
      List.map (fun lines ->
        let file = Filename.temp_file "test-log-merge-in-" "" in
        to_file file lines;
        file
      ) l
    in
    let output_file = Filename.temp_file "test-log-merge-out-" "" in
    merge_files input_files (Some output_file);
    let merged_lines = of_file output_file in
    List.iter Sys.remove input_files;
    Sys.remove output_file;
    merged_lines

  let line1 = "[1970-01-01T00:00:00.000-07:00] 1"
  let line2 = "[1970-01-01T00:00:00.000-07:00] 2"
  let line3 = "3"
  let line4 = "[1970-01-01T00:00:00.001-07:00] 4"
  let line5 = "[1970-01-01T00:00:00.002-07:00] 5"
  let line6 = "[1970-01-01T00:00:00.002-07:00] 6"
  let line7 = "7"

  let test () =
    assert (
      merge [] = []
    );

    assert (
      merge [ ["junk"; "more junk"] ] = []
    );

    assert (
      merge [
        ["junk"];
        ["other junk"];
      ] =
      []
    );

    assert (
      merge [
        ["junk"; line1];
        [line5];
        [line2; line3; line4];
        [line6; line7];
      ] =
      [line1; line2; line3; line4; line5; line6; line7]
    );
    true
end

let tests = [
  "log-merge", Test.test;
]
