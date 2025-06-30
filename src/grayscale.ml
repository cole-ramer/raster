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
  let our_image =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  match
    Image.height our_image = Image.height reference_image
    && Image.width our_image = Image.width reference_image
  with
  | false -> print_endline "Dimensions of the pictures are not equal"
  | true ->
    let first_and_total_incorrect_pixels =
      Image.foldi
        ~init:(0, None)
        reference_image
        ~f:(fun ~x ~y error_information refernce_pixel ->
          match
            ( Pixel.equal refernce_pixel (Image.get our_image ~x ~y)
            , snd error_information )
          with
          | false, None -> fst error_information + 1, Some refernce_pixel
          | false, _ -> fst error_information, snd error_information
          | true, _ -> error_information)
    in
    (match first_and_total_incorrect_pixels with
     | 0, None | _, None | 0, _ -> ()
     | total_bad_pixels, Some first_bad_pixel ->
       print_endline
         ("Total pixels that are incorrect: "
          ^ Int.to_string total_bad_pixels);
       print_endline
         ("First incorrect pixel: location:  "
          ^ Pixel.to_string first_bad_pixel));
    [%expect {||}]
;;
