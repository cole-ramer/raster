open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun pixel ->
    let gray =
      (Pixel.red pixel + Pixel.blue pixel + Pixel.green pixel) / 3
    in
    gray, gray, gray)
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let%expect_test "check gray_scale" =
  let output_image =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  Image.compare ~output_image ~reference_image;
  [%expect {||}]
;;
