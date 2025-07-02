open Core

(* adds the adjustment to surrounding dither*)
let is_in_bounds image ~x ~y =
  match x >= 0, x < Image.width image, y >= 0, y < Image.height image with
  | true, true, true, true -> true
  | _, _, _, _ -> false
;;

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

module Error_adjustment_directions = struct
  type t =
    | Right
    | Bottom_left
    | Bottom
    | Bottom_right
end

module Color = struct
  type t =
    | Red
    | Green
    | Blue
end

let get_adjusted_value (color : Color.t) multiplyer error_pixel =
  match color with
  | Red ->
    Int.to_float (Pixel.red error_pixel) *. multiplyer /. 16.
    |> Float.round
    |> Float.to_int
  | Blue ->
    Int.to_float (Pixel.blue error_pixel) *. multiplyer /. 16.
    |> Float.round
    |> Float.to_int
  | Green ->
    Int.to_float (Pixel.green error_pixel) *. multiplyer /. 16.
    |> Float.round
    |> Float.to_int
;;

let get_adjusted_pixel multiplyer error_pixel =
  let adjusted_red = get_adjusted_value Red multiplyer error_pixel in
  let adjusted_green = get_adjusted_value Green multiplyer error_pixel in
  let adjusted_blue = get_adjusted_value Blue multiplyer error_pixel in
  adjusted_red, adjusted_blue, adjusted_green
;;

(* let get_adjusted_pixel_in_val_range
   ~pixel_getting_adjusted
   ~adjustment_pixel
   max_val
   =
   let adjusted_red_val =
   match Pixel.red adjustment_pixel > 0 with
   | true ->
   min
   (Pixel.red adjustment_pixel + Pixel.red pixel_getting_adjusted)
   max_val
   | false ->
   max (Pixel.red adjustment_pixel + Pixel.red pixel_getting_adjusted) 0
   in
   let adjusted_blue_bal =
   match Pixel.blue adjustment_pixel > 0 with
   | true ->
   min
   (Pixel.blue adjustment_pixel + Pixel.blue pixel_getting_adjusted)
   max_val
   | false ->
   max (Pixel.blue adjustment_pixel + Pixel.blue pixel_getting_adjusted) 0
   in
   let adjusted_green_val =
   match Pixel.green adjustment_pixel > 0 with
   | true ->
   min
   (Pixel.blue adjustment_pixel + Pixel.blue pixel_getting_adjusted)
   max_val
   | false ->
   max (Pixel.blue adjustment_pixel + Pixel.blue pixel_getting_adjusted) 0
   in
   adjusted_red_val, adjusted_green_val, adjusted_blue_bal
   ;; *)

let adjust_pixel
      image
      error_pixel
      (adjustment_direction : Error_adjustment_directions.t)
      ~x
      ~y
  =
  (* let max_val = Image.max_val image in *)
  match adjustment_direction with
  | Right ->
    let multiplyer = 7. in
    let adjustment_pixel = get_adjusted_pixel multiplyer error_pixel in
    if
      Pixel.blue (Image.get image ~x:(x + 1) ~y)
      + Pixel.blue adjustment_pixel
      > Image.max_val image
    then
      print_endline
        ("Rigth"
         ^ Int.to_string
             (Pixel.blue (Image.get image ~x:(x + 1) ~y)
              + Pixel.blue adjustment_pixel));
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + ) (Image.get image ~x:(x + 1) ~y) adjustment_pixel)
  | Bottom_left ->
    let multiplyer = 3. in
    let adjustment_pixel = get_adjusted_pixel multiplyer error_pixel in
    if
      Pixel.blue (Image.get image ~x:(x - 1) ~y:(y + 1))
      + Pixel.blue adjustment_pixel
      > Image.max_val image
    then
      print_endline
        ("Bottom_left_blue"
         ^ Int.to_string
             (Pixel.blue (Image.get image ~x:(x - 1) ~y:(y + 1))
              + Pixel.blue adjustment_pixel));
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + ) (Image.get image ~x:(x - 1) ~y:(y + 1)) adjustment_pixel)
    (* (get_adjusted_pixel_in_val_range
       ~pixel_getting_adjusted:(Image.get image ~x:(x - 1) ~y:(y + 1))
       ~adjustment_pixel
       max_val) *)
  | Bottom ->
    let multiplyer = 5. in
    let adjustment_pixel = get_adjusted_pixel multiplyer error_pixel in
    if
      Pixel.blue (Image.get image ~x ~y:(y + 1))
      + Pixel.blue adjustment_pixel
      > Image.max_val image
    then
      print_endline
        ("Bottom"
         ^ Int.to_string
             (Pixel.blue (Image.get image ~x ~y:(y + 1))
              + Pixel.blue adjustment_pixel));
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + ) (Image.get image ~x ~y:(y + 1)) adjustment_pixel)
    (* (get_adjusted_pixel_in_val_range
       ~pixel_getting_adjusted:(Image.get image ~x ~y:(y + 1))
       ~adjustment_pixel
       max_val) *)
  | Bottom_right ->
    let multiplyer = 1. in
    let adjustment_pixel = get_adjusted_pixel multiplyer error_pixel in
    if
      Pixel.blue (Image.get image ~x:(x + 1) ~y:(y + 1))
      + Pixel.blue adjustment_pixel
      > Image.max_val image
    then
      print_endline
        ("Bottom_right_blue"
         ^ Int.to_string
             (Pixel.blue (Image.get image ~x:(x + 1) ~y:(y + 1))
              + Pixel.blue adjustment_pixel));
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) (Image.get image ~x:(x + 1) ~y:(y + 1)) adjustment_pixel)
;;

(* (get_adjusted_pixel_in_val_range
   ~pixel_getting_adjusted:(Image.get image ~x:(x + 1) ~y:(y + 1))
   ~adjustment_pixel
   max_val) *)

let adjust_surrounding_pixels_color ~error_pixel ~x ~y image =
  if is_in_bounds image ~x:(x + 1) ~y
  then adjust_pixel image error_pixel Right ~x ~y;
  if is_in_bounds image ~x:(x - 1) ~y:(y + 1)
  then adjust_pixel image error_pixel Bottom_left ~x ~y;
  if is_in_bounds image ~x ~y:(y + 1)
  then adjust_pixel image error_pixel Bottom ~x ~y;
  if is_in_bounds image ~x:(x + 1) ~y:(y + 1)
  then adjust_pixel image error_pixel Bottom_right ~x ~y
;;

let transform_color image ~(number_of_colors : int) =
  let max_pixel_value = Image.max_val image in
  print_endline (Int.to_string max_pixel_value);
  let differnce_between_allowed_values =
    Int.to_float max_pixel_value /. Int.to_float (number_of_colors - 1)
  in
  Image.mapi image ~f:(fun ~x ~y image_pixel ->
    let new_red_val =
      Int.to_float (Pixel.red image_pixel)
      /. differnce_between_allowed_values
      |> Float.round
      |> Float.( * ) differnce_between_allowed_values
      |> Float.round
      |> Float.to_int
    in
    let new_green_val =
      Int.to_float (Pixel.green image_pixel)
      /. differnce_between_allowed_values
      |> Float.round
      |> Float.( * ) differnce_between_allowed_values
      |> Float.round
      |> Float.to_int
    in
    let new_blue_val =
      Int.to_float (Pixel.blue image_pixel)
      /. differnce_between_allowed_values
      |> Float.round
      |> Float.( * ) differnce_between_allowed_values
      |> Float.round
      |> Float.to_int
    in
    let old_red_val = Pixel.red image_pixel in
    let old_green_val = Pixel.green image_pixel in
    let old_blue_val = Pixel.blue image_pixel in
    let error_pixel : Pixel.t =
      ( old_red_val - new_red_val
      , old_green_val - new_green_val
      , old_blue_val - new_blue_val )
    in
    (* print_endline "before adjustment"; *)
    adjust_surrounding_pixels_color ~error_pixel ~x ~y image;
    if new_blue_val > max_pixel_value
    then print_endline ("new_blue_val" ^ Int.to_string new_blue_val);
    new_red_val, new_green_val, new_blue_val)
;;

let dither_command =
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

let dither_color_command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and number_of_colors =
        flag
          "number-of-colors"
          (required Command.Param.int)
          ~doc:"INT number of colors per channel for color_dither"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> transform_color ~number_of_colors
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let command =
  Command.group
    ~summary:"dither related command"
    [ "color", dither_color_command; "gray", dither_command ]
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
