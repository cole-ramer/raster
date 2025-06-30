open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _ ->
    let x_start =
      match x - radius < 0 with true -> 0 | false -> x - radius
    in
    let x_end =
      match x + radius >= Image.width image with
      | true -> Image.width image - 1
      | false -> x + radius
    in
    let y_start =
      match y - radius < 0 with true -> 0 | false -> y - radius
    in
    let y_end =
      match y + radius >= Image.height image with
      | true -> Image.height image - 1
      | false -> y + radius
    in
    Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let%expect_test "blur image transform" =
  let output_image =
    transform
      (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
      ~radius:3
  in
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  Image.compare ~output_image ~reference_image;
  [%expect {||}]
;;
