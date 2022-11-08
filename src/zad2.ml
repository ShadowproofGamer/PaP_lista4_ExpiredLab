let pole f a b dx =
  let rec listcreating x =
    if abs_float(x-.b)<=dx then []
    else ( abs_float(f x), abs_float(f (x+.dx)))::listcreating(x+.dx)
  in let list = listcreating a in List.fold_right (fun h acc -> acc +. (( fst h +. snd h)*.dx/.2.)) list 0. ;;

pole (fun x->x) (-.1.) 1. 0.0001;;
pole (fun x->(-.x)) 2. 3. 0.05;;


let pxp f a = (f (a+.1.0))+.1.0;;
pxp (fun x->x) 3.;;