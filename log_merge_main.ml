let main ~offset =
  let output_file = ref None in
  let options = [
    "-o", Arg.String (fun s -> output_file := Some s),
    "output file"
  ] in
  let input_files = Cmdline.parse ~offset options in
  Log_merge.merge_files input_files !output_file
