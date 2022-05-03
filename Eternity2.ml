#load "graphics.cma"
open Graphics;;
open_graph " 960x960";;

type color ={r:int; g:int;b:int};; 
type piece ={posx:int;posy:int;top:int; rigth :int; bottom:int;left:int };; 
type gameTree = Vide |Noeud of  gameTree * piece *  gameTree ;;

let nbp=12;; 
let rouge={r=255;g=0;b=0};; 
let vert={r=0;g=255;b=0};;
let bleu={r=0;g=0;b=255};;
let cyan={r=0;g=255;b=255};;
let magenta={r=255;g=0;b=255};;
let jaune={r=255;g=255;b=0};;
let gris={r=100;g=100;b=100};;
let fauve={r=173;g=79;b=9};;
let rose={r=253;g=108;b=158};;
let haki={r=121;g=137;b=51};; 

let lesCouleur =[|rouge;vert;bleu;cyan;magenta;jaune;gris;fauve;rose;haki|];;

let plateau=Array.init nbp (function x-> Array.init nbp (function y-> {posx=(-1);posy=(-1);top=(-1);rigth=(-1); bottom=(-1);left=(-1)}));;
let voisinhaut piece = 
  if(piece.posy>0)
  then
    (plateau.(piece.posy -1).(piece.posx)).bottom
  else
    (plateau.(nbp -1).(piece.posx)).bottom 
;;

let voisindroit piece =
  if(piece.posx<nbp-1)
  then
    (plateau.(piece.posy).(piece.posx+1)).left
  else
    (plateau.(piece.posy).(0)).left 
;;

let voisinbas piece  =
  if(piece.posy <nbp-1)
  then
    (plateau.(piece.posy +1).(piece.posx)).top
  else
    (plateau.(0).(piece.posx)).top 
;;

let voisingauche piece =
  if(piece.posx>0)
  then
    (plateau.(piece.posy).(piece.posx-1)).rigth
  else
    (plateau.(piece.posy).(nbp-1)).rigth 
;; 

let voisin piece n =
  try 
    if n==0
    then
      voisinhaut piece 
    else
    if n==1
    then
      voisindroit piece 
    else
    if n==2
    then
      voisinbas piece 
    else 
      voisingauche piece 
  with
  |Invalid_argument "index out of bounds" -> (-1)
;;

let valeur x y n =
  let taille=Array.length lesCouleur in
  Random.self_init(); 
  let v=voisin {posx=x;posy=y;top=0;rigth=0; bottom=0;left=0} n in
  if v<>(-1)
  then
    v
  else
    Random.int taille 
;;
let piece x y  = 
  {posx=x;posy=(nbp-1)-y;top=(valeur x y 0);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(valeur x y 3)} 
;;

let initLine m= 
  let n=nbp in 
  let rec initLine_aux n acc= 
    if n==1 
    then 
      let res=piece (n-1) m in 
      plateau.(m).(n-1)<- res ;
      Array.append  [|res|] acc ;
    else 
      let res=piece (n-1) m in
      plateau.(m).(n-1)<-res ;
      initLine_aux (n-1) (Array.append  [|res|] acc) 
  in
  initLine_aux n [||] 
;;

let plateau2=Array.init nbp  initLine ;; 

plateau;;

let rec turnR piece n= 
  if n<2
  then
    {posx=piece.posx;posy=piece.posy;top=piece.left;rigth=piece.top; bottom=piece.rigth;left=piece.bottom}
  else
    turnR{posx=piece.posx;posy=piece.posy;top=piece.left;rigth=piece.top; bottom=piece.rigth;left=piece.bottom} (n-1)
       
;;
  
let rec turnL piece n=
  
  if n<2
  then
    {posx=piece.posx;posy=piece.posy;top=piece.rigth;rigth=piece.bottom; bottom=piece.left;left=piece.top}
  else
    turnL{posx=piece.posx;posy=piece.posy;top=piece.rigth;rigth=piece.bottom; bottom=piece.left;left=piece.top} (n-1)
;;
            
let deplacer piece x y= 
  {posx=x;posy= (nbp-1)-y;top=piece.top;rigth=piece.rigth; bottom=piece.bottom;left=piece.left}
;;


let echanger x y x1 y1=
  
  let temp=plateau.(y).(x)in
  
  plateau.(y).(x)<- (deplacer plateau.(y1).(x1) x y);
  
  plateau.(y1).(x1)<- (deplacer temp x1 y1) 
;;

let melanger taille=
  
  let nbpiece=taille*taille in
  let rec melanger_aux conteur=
    
    if conteur <>0
    then
      (
        Random.self_init();
        let x=Random.int nbp in
        let y=Random.int nbp in
        let operation=Random.int 3 in
        if operation==0
        then
          (
            let lr=Random.int 2 in
            if lr==0
            then
              plateau.(y).(x)<- turnR  plateau.(y).(x) (Random.int 4) 
            else
              plateau.(y).(x)<- turnL  plateau.(y).(x) ( Random.int 4)
          )
        else
        if operation==1
        then
          (
            let x2=Random.int nbp in
            let y2=Random.int nbp in
            if (x2 <> x)||(y2<>y)
            then
              echanger x y x2 y2
          ) 
        else 
          (
            let x2=Random.int nbp in
            let y2=Random.int nbp in
            let lr=Random.int 2 in 
            if lr==0
  
            then
              plateau.(y).(x)<-turnR  plateau.(y).(x) (Random.int 4) 
            else 
              plateau.(y).(x)<-turnL  plateau.(y).(x) (Random.int 4);
            
            if (x2 <> x)||(y2<>y)
            then
              echanger x y x2 y2 
                
          ); 
        melanger_aux (conteur -1)
      )
    
  in
  melanger_aux nbpiece 
;;






           


 
           




 
           






 
           







let dessiner piece =

let taille=80 in

  set_color (rgb lesCouleur.( piece.bottom).r lesCouleur.( piece.bottom).g lesCouleur.( piece.bottom).b );

  fill_poly [|(( piece.posx * taille) ,(piece.posy * taille));
              (((piece.posx +1) * taille) ,(piece.posy * taille));
              (((piece.posx * taille )+(taille/2)) ,((piece.posy * taille) + (taille/2)));
              (( piece.posx * taille) ,(piece.posy * taille))|];

  
  set_color (rgb lesCouleur.( piece.top).r lesCouleur.( piece.top).g lesCouleur.( piece.top).b );

  fill_poly [|(( piece.posx  * taille) ,((piece.posy +1) * taille));
              (((piece.posx +1) * taille) ,((piece.posy +1) * taille));
            (((piece.posx * taille )+(taille/2)) ,((piece.posy * taille) + (taille/2)));
              (( piece.posx  * taille) ,((piece.posy +1) * taille));|];

  
  set_color (rgb lesCouleur.(piece.rigth).r lesCouleur.( piece.rigth).g lesCouleur.( piece.rigth).b );

  fill_poly [|(((piece.posx +1) * taille) ,(piece.posy * taille));
              (((piece.posx +1) * taille) ,((piece.posy+1) * taille));
                (((piece.posx * taille )+(taille/2)) ,((piece.posy * taille) + (taille/2)));
              (((piece.posx +1) * taille) ,(piece.posy * taille))|];

 set_color (rgb lesCouleur.(piece.left).r lesCouleur.( piece.left).g lesCouleur.( piece.left).b );

  fill_poly [|(((piece.posx  ) * (taille  )) ,((piece.posy  ) * (taille  )));
              (((piece.posx  ) * (taille  )) ,((piece.posy+1) * (taille  )));
              (((piece.posx * taille )+(taille/2)) ,((piece.posy * taille) + (taille/2)));
              (((piece.posx  ) * (taille  )) ,((piece.posy  ) * (taille  )))|];
   
;;

let dessinerligne ligne=

Array.iter dessiner ligne

;;


let dessinerPlateau ()=
clear_graph();
Array.iter dessinerligne plateau
;;



dessinerPlateau ();;

 melanger nbp;;

