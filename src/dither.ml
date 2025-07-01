open Core

(* adds the adjustment to surrounding dither*)
let adjust_surrounding_pixels image ~x ~y ~error =
  let max_x = Image.width image - 1 in
  let max_y = Image.height image - 1 in
  (* We convert adjustment to float then round, then to int, to increase accuracy*)
  if x < max_x
  then (
    let adjustment =
      Int.to_float error *. 7. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y)
         (adjustment, adjustment, adjustment)));
  if x > 0 && y < max_y
  then (
    let adjustment =
      Int.to_float error *. 3. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x - 1) ~y:(y + 1))
         (adjustment, adjustment, adjustment)));
  if y < max_y
  then (
    let adjustment =
      Int.to_float error *. 5. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x ~y:(y + 1))
         (adjustment, adjustment, adjustment)));
  if x < max_x && y < max_y
  then (
    let adjustment =
      Int.to_float error *. 1. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y:(y + 1))
         (adjustment, adjustment, adjustment)))
;;

(* This should look familiar by now! *)
let transform image =
  let gray_image = Grayscale.transform image in
  let max_pixel_value = Image.max_val gray_image in
  Image.mapi gray_image ~f:(fun ~x ~y gray_pixel ->
    (* Gray scale so does not matter whether we pick red, green, or blue pixel*)
    match Pixel.red gray_pixel > max_pixel_value / 2 with
    | true ->
      let error = Pixel.red gray_pixel - max_pixel_value in
      adjust_surrounding_pixels gray_image ~x ~y ~error;
      Pixel.of_int max_pixel_value (* Pure white pixel *)
    | false ->
      let error = Pixel.red gray_pixel in
      adjust_surrounding_pixels gray_image ~x ~y ~error;
      Pixel.zero (* Pure black pixel *))
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let%expect_test "dither transform test" =
  let output_image =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  Image.compare ~output_image ~reference_image;
  [%expect {||}]
;;
