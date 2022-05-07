#load "graphics.cma"
open Graphics;;
open_graph " 960x960";;
type color ={r:int; g:int;b:int};; 
type piece ={posx:int;posy:int;top:int; rigth :int; bottom:int;left:int;num:int };; 
type noeud={piece:piece; list: piece list};; 

let nbp=10;; 
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
let noir={r=0;g=0;b=0};; 
let lesCouleur =[|rouge;vert;bleu;cyan;magenta;jaune;gris;fauve;rose;haki|];;
let lesCouleuraf =[|rouge;vert;bleu;cyan;magenta;jaune;gris;fauve;rose;haki;noir|];;
let pnull={posx=(-1);posy=(-1);top=(-1);rigth=(-1); bottom=(-1);left=(-1);num=(-1)};;

let plateau=Array.init nbp (function _-> Array.init nbp (function _-> pnull));;

let voisinhaut x y = 
  if(y>0)
  then
    (plateau.(y-1).(x)).bottom
  else
    (plateau.(nbp-1).(x)).bottom 
;;

let voisindroit x y =
  if(x<nbp-1)
  then
    (plateau.(y).(x+1)).left
  else
    (plateau.(y).(0)).left 
;;

let voisinbas x y  =
  if(y<nbp-1)
  then
    (plateau.(y+1).(x)).top
  else
    (plateau.(0).(x)).top 
;;

let voisingauche x y =
  if(x>0)
  then
    (plateau.(y).(x-1)).rigth
  else
    (plateau.(y).(nbp-1)).rigth 
;; 


let voisin x y n =
  try 
    if n==0
    then
      voisinhaut x y 
    else
    if n==1
    then
      voisindroit x y 
    else
    if n==2
    then
      voisinbas x y 
    else 
      voisingauche x y 
  with
  |Invalid_argument "index out of bounds" -> (-1)
;;

let valeur x y n =
  let taille=Array.length lesCouleur in
  Random.self_init(); 
  let v=voisin x y n in
  if v<>(-1)
  then
    v
  else
    Random.int taille 
;;

let piece2 x y num  =
  
  {posx=x;posy=(y);top=(valeur x y 0);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(valeur x y 3);num=num}
  
;;

let piece x y  num = 
  if(x==0)
  then
    (
      if(y>0&&y<(nbp-1))
      then
        {posx=x;posy=y;top=(valeur x y 0);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(10);num=num} 
      else
      if(y==0)
      then
        {posx=x;posy=y;top=(10);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(10);num=num}
      else 
        {posx=x;posy=y;top=(valeur x y 0);rigth=(valeur x y 1);bottom=(10);left=(10);num=num}
      
    )
  else  
    
  if(x==nbp-1)
  then
    (   if(y>0&&y<(nbp-1))
        then
          {posx=x;posy=y;top=(valeur x y 0);rigth=(10);bottom=(valeur x y 2);left=(valeur x y 3);num=num}
        else
        if(y==0)
        then
          {posx=x;posy=y;top=(10);rigth=(10);bottom=(valeur x y 2);left=(valeur x y 3);num=num}
        else 
          {posx=x;posy=y;top=(valeur x y 0);rigth=(10);bottom=(10);left=(valeur x y 3);num=num}
          
        
    )
  else 
    
  if(y==0)
  then 
    {posx=x;posy=y;top=(10);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(valeur x y 3);num=num}
  else 
  if(y==nbp-1)
  then
    {posx=x;posy=y;top=(valeur x y 0);rigth=(valeur x y 1);bottom=(10);left=(valeur x y 3);num=num}
  else
    {posx=x;posy=y;top=(valeur x y 0);rigth=(valeur x y 1);bottom=(valeur x y 2);left=(valeur x y 3);num=num}
    

;;



let initLine m= 
  let n=nbp in 
  let rec initLine_aux n acc= 
    if n==1 
    then 
      let res=piece (n-1) m 0 in 
      plateau.(m).(n-1)<- res ;
      Array.append  [|res|] acc ;
    else 
      let res=piece (n-1) m 0 in
      plateau.(m).(n-1)<-res ;
      initLine_aux (n-1) (Array.append  [|res|] acc) 
  in
  initLine_aux n [||] 
;;

Array.init nbp  initLine ;; 



















let dessiner piece =

let taille=80 in
if(piece.top>(-1))
then
(
  set_color (rgb lesCouleuraf.( piece.bottom).r lesCouleuraf.( piece.bottom).g lesCouleuraf.( piece.bottom).b );

  fill_poly [|(( piece.posx * taille) ,(((nbp-1)-piece.posy) * taille));
              (((piece.posx +1) * taille) ,(((nbp-1)-piece.posy) * taille));
              (((piece.posx * taille )+(taille/2)) ,((((nbp-1)-piece.posy) * taille) + (taille/2)));
              (( piece.posx * taille) ,(((nbp-1)-piece.posy) * taille))|];

  
  set_color (rgb lesCouleuraf.( piece.top).r lesCouleuraf.( piece.top).g lesCouleuraf.( piece.top).b );

  fill_poly [|(( piece.posx  * taille) ,((((nbp-1)-piece.posy) +1) * taille));
              (((piece.posx +1) * taille) ,((((nbp-1)-piece.posy) +1) * taille));
            (((piece.posx * taille )+(taille/2)) ,((((nbp-1)-piece.posy) * taille) + (taille/2)));
              (( piece.posx  * taille) ,((((nbp-1)-piece.posy) +1) * taille));|];

  
  set_color (rgb lesCouleuraf.(piece.rigth).r lesCouleuraf.( piece.rigth).g lesCouleuraf.( piece.rigth).b );

  fill_poly [|(((piece.posx +1) * taille) ,(((nbp-1)-piece.posy) * taille));
              (((piece.posx +1) * taille) ,((((nbp-1)-piece.posy)+1) * taille));
                (((piece.posx * taille )+(taille/2)) ,((((nbp-1)-piece.posy) * taille) + (taille/2)));
              (((piece.posx +1) * taille) ,(((nbp-1)-piece.posy) * taille))|];

 set_color (rgb lesCouleuraf.(piece.left).r lesCouleuraf.( piece.left).g lesCouleuraf.( piece.left).b );

  fill_poly [|(((piece.posx  ) * (taille  )) ,((((nbp-1)-piece.posy)  ) * (taille  )));
              (((piece.posx  ) * (taille  )) ,((((nbp-1)-piece.posy)+1) * (taille  )));
              (((piece.posx * taille )+(taille/2)) ,((((nbp-1)-piece.posy) * taille) + (taille/2)));
              (((piece.posx  ) * (taille  )) ,((((nbp-1)-piece.posy)  ) * (taille  )))|];
)  
;;

let dessinerligne ligne=

Array.iter dessiner ligne

;;


let dessinerPlateau ()=
clear_graph();
Array.iter dessinerligne plateau
;;

dessinerPlateau ();;
let _=read_line();;













let rec turnR piece n= 
  if n==0
  then
    piece
  else
  if n==1
  then
    {posx=piece.posx;posy=piece.posy;top=piece.left;rigth=piece.top; bottom=piece.rigth;left=piece.bottom;num=piece.num}
  else
    turnR{posx=piece.posx;posy=piece.posy;top=piece.left;rigth=piece.top; bottom=piece.rigth;left=piece.bottom;num=piece.num} ((n-1) mod 4) 
;;
  
let rec turnL piece n=
  if n==0
  then
    piece
  else
  if n==1
  then
    {posx=piece.posx;posy=piece.posy;top=piece.rigth;rigth=piece.bottom; bottom=piece.left;left=piece.top;num=piece.num}
  else
    turnL{posx=piece.posx;posy=piece.posy;top=piece.rigth;rigth=piece.bottom; bottom=piece.left;left=piece.top;num=piece.num} ((n-1) mod 4)
;;
            
let deplacer piece x y= 
  {posx=x;posy= y;top=piece.top;rigth=piece.rigth; bottom=piece.bottom;left=piece.left;num=piece.num}
;;


let estdans x piece=
  if(x==piece.top||x==piece.rigth||x==piece.bottom||x==piece.left)
  then true
  else
    false 
;;
  
let rec egales piece1 piece2=
  let rec egales_aux piece1 piece2 n= 
    if (n==5)
    then 
      false
    else 
    if(piece1.top==piece2.top)
    then
      (
        if(piece1.rigth==piece2.rigth)
        then
          (
            if(piece1.bottom==piece2.bottom) 
            then 
              true 
            else
              false
          )
        else
          false
      )
    else
      egales_aux (turnR piece1 1) piece2 (n+1)
  in 
  if((estdans piece1.top piece2)&&(estdans piece1.rigth piece2)&&(estdans piece1.bottom piece2)&&(estdans piece1.left piece2)) 
  then egales_aux piece1 piece2 1
  else
    false
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

plateau;;
melanger nbp;;
dessinerPlateau ();;
let _=read_line();;



let setNumero piece num=
  {posx=piece.posx;posy=piece.posy;top=piece.top;rigth=piece.rigth; bottom=piece.bottom;left=piece.left;num=num}
;;

let setNumeros liste=
  let rec setNumeros_aux liste num acc=
    match liste with
    |[]->acc
    |t::q-> setNumeros_aux q (num+1) (acc@[(setNumero t num)])
  in
  setNumeros_aux liste 0 []
;;

let toliste tableau= 
  let lines= Array.to_list tableau in
  let rec linetoliste lignes acc= 
    match lignes with
    |[]->acc
    |t::q->
        linetoliste q (acc@ Array.to_list t) 
  in 
  linetoliste lines []
;;

let lesPieces=setNumeros (toliste plateau);;

let rec init_plateau x y = 
  if(x<nbp)
  then
    ( 
      plateau.(y).(x)<-pnull;
      init_plateau (x+1) y
    )
  else
  if(x==(nbp)&&y==(nbp-1))
  then true
  else 
    (
      init_plateau (0) (y+1)
    ) 
;; 
init_plateau 0 0;;


plateau;;



type arbre={ pieces:piece list; possible:piece list array}


let placer piece possibles arbre=
  plateau.((piece.posy)).(piece.posx)<-piece;
  dessinerPlateau ();
   
  {pieces= [piece]@arbre.pieces;possible=Array.append [|possibles|] arbre.possible} 
  
;;

let compatible x y =
  if(x==y||y==(-1))
  then 
    true
  else
    false
;;


let convient piece x y = 

  let rec convient_aux piece n vt vr vb vl= 
    if(n==5)
    then 
      false
    else 
    if((compatible piece.top vt)&&(compatible piece.rigth vr)&&(compatible piece.bottom vb)&&(compatible piece.left vl))
    then 
      (
        plateau.(y).(x)<-piece;
        true
      )
    else 
      convient_aux (turnR piece 1) (n+1) vt vr vb vl
  in 
 

let v=deplacer piece x y in
if(x==0)
  then
    (
      if(y>0&&y<(nbp-1))
      then
      (
          let vt=voisin x y 0 in
          let vr=(-1) in 
          let vb=(-1) in 
          let vl=10 in 
          convient_aux v 1 vt vr vb vl
      )
      else
      if(y==0)
      then
      (
          let vt=10 in
          let vr=(-1) in 
          let vb=(-1) in 
          let vl=10 in 
           convient_aux v 1 vt vr vb vl
      )
        
      else 
      (
         let vt=voisin x y 0 in
          let vr=(-1) in 
          let vb=10 in 
          let vl=10 in 
           convient_aux v 1 vt vr vb vl
      )
      
    )
  else  
    
  if(x==nbp-1)
  then
    (   if(y>0&&y<(nbp-1))
        then
        (
          let vt=voisin x y 0 in
          let vr=10 in 
          let vb=(-1) in 
          let vl=voisin x y 3 in 
           convient_aux v 1 vt vr vb vl
        )
        else
        if(y==0)
        then
        (
          let vt=10 in
          let vr=10 in 
          let vb=(-1) in 
          let vl= voisin x y 3 in 
           convient_aux v 1 vt vr vb vl
        )
        else 
        (
          let vt=voisin x y 0 in
          let vr=10 in 
          let vb=10 in 
          let vl= voisin x y 3 in 
           convient_aux v 1 vt vr vb vl
        )
          
        
    )
  else 
    
  if(y==0)
  then 
     (
          let vt=10 in
          let vr=(-1) in 
          let vb=(-1) in 
          let vl= voisin x y 3 in 
           convient_aux v 1 vt vr vb vl
    )
  else 
  if(y==nbp-1)
  then
     (
          let vt=voisin x y 0 in
          let vr=(-1) in 
          let vb=10 in 
          let vl= voisin x y 3 in 
           convient_aux v 1 vt vr vb vl
    )
  else
  (
      
  let vt=voisin x y 0 in
  let vr=(-1) in 
  let vb=(-1) in 
  let vl=voisin x y 3 in 
   convient_aux v 1 vt vr vb vl
  )


;;
let rec possibles liste x y acc= 
  match liste with
  |[]->acc
  |t::q-> if(convient t x y)
      then 
        (  
          possibles q x y (acc@[plateau.(y).(x)]) 
        )
      else
        possibles q x y acc 
;;

let mettrePiecesAzero liste=
  let rec pDebut_aux liste acc = 
    match liste with
    |[]->acc
    |t::q-> 
        pDebut_aux q (acc@[deplacer t 0 0])
          
  in
  
  pDebut_aux liste []
;;

let piecesDebut liste=
  let rec piecesDebut_aux liste acc = 
    match liste with
    |[]->acc
    |t::q-> 
        let t1=(turnR t  1)in
        let t2=(turnR t1 1)in
        let t3=(turnR t2 1)in
        piecesDebut_aux q (acc@[t1;t2;t3])
          
  in
  
  piecesDebut_aux liste []
  
;;



let black2 piece=
  
  if((piece.top==10&&piece.rigth==10)||(piece.rigth==10&&piece.bottom==10)||(piece.bottom==10&&piece.left==10)||(piece.left==10&&piece.top==10))
  then true
  else
    false
  
;;


let trier liste=
    
  let rec trier_aux acc1 acc2 acc3 liste=
    
    match liste with
    |[]->acc1,acc2,acc3
    |t::q-> if(estdans 10 t)
        then
          if(black2 t)
          then trier_aux (acc1@[t]) acc2 acc3 q
          else
            trier_aux acc1 (acc2@[t]) acc3 q
        else
          trier_aux acc1 acc2 (acc3@[t]) q
  in
    
  trier_aux [] [] [] liste
;;
(*
let lesPieces=mettrePiecesAzero lesPieces;;

let debut =lesPieces@(piecesDebut lesPieces);;


let arbre=placer (List.hd debut) (List.tl debut)  {pieces=[];possible=[||]};;



*)

let noir2,noir1,normale= trier lesPieces;;

let lesPieces=noir1@(List.tl noir2)@normale;;

let noir2=mettrePiecesAzero noir2;;

let debut =possibles (noir2) 0 0 [];;


dessinerPlateau ();;

let _=read_line();;
let arbre=placer (List.hd debut) (List.tl debut)  {pieces=[];possible=[||]};;

dessinerPlateau ();;
let _=read_line();;





let rec enleverPieceN liste n acc=
  match liste with
  |[]->acc
  |t::q-> if(t.num ==n)
      then 
        enleverPieceN q n acc 
      else
        enleverPieceN q n (acc@[t])
;;

let rec backt liste arbre x y = 
 

  if(List.length (arbre.pieces)>1)
  then
    (
      let lesPossibilite= arbre.possible.(0) in
  
      if(List.length lesPossibilite>0)
      then 
        ( 
        
          let temp=List.hd lesPossibilite in
          plateau.(temp.posy).(temp.posx)<-temp;
         
         
         (enleverPieceN liste temp.num [])@[(List.hd arbre.pieces)], 
          {pieces=([temp]@List.tl(arbre.pieces));possible= Array.append [|(List.tl lesPossibilite)|]  (Array.sub arbre.possible 1 ((Array.length arbre.possible)-1))  }
          ,x ,y
        )
      else
        (
          
      
          let temp=(List.hd arbre.pieces)in
          plateau.(temp.posy).(temp.posx)<-pnull;
         
          
          backt (liste@[temp]) {pieces=List.tl arbre.pieces;possible=Array.sub arbre.possible 1 ((Array.length arbre.possible)-1)} temp.posx temp.posy
          )
          
    )
  else
    (

      let lesPossibilite= arbre.possible.(0) in
  
      if(List.length lesPossibilite>1)
      then 
        (
       
          print_endline "il ya d'autres possibilité2";
          let temp=List.hd lesPossibilite in
          plateau.(0).(0)<-temp;
        
          (enleverPieceN liste temp.num [])@[(List.hd arbre.pieces)], 
          {pieces=([temp]);possible=[|(List.tl lesPossibilite)|] }
          ,1,0
        )
      else
      if(List.length lesPossibilite==1)
      then
        (
          print_endline "derniere chance";
          let temp=List.hd lesPossibilite in
          plateau.(0).(0)<-temp;
          dessinerPlateau ();
          let _=read_line()in
         (enleverPieceN liste temp.num [])@[(List.hd arbre.pieces)], 
          {pieces=([temp]);possible= [|[]|] }
          ,1,0
        )
      else
        (
          print_endline "Pas de chance , il fallait tester les rotations aussi";
          [],arbre,x,y
        
        )
        
    )
;;
  




let rec resoudre lesPieces x y compteur arbre= 
  
  
  match lesPieces with

  |[]-> arbre 
  |t::q -> 
      if(compteur<(nbp*nbp)) 
      then
        (
                
          if((convient t x y)) 
          then 
            ( 
              
              let amettre=plateau.(y).(x)in
              let pos= possibles q x y []in 
              if(x<(nbp-1))
              then 
                ( 
                 
                  resoudre q (x+1) y (List.length arbre.pieces+1) (placer amettre pos  arbre) 
                )
              else 
                (
                 
                  resoudre q 0 (y+1) (List.length arbre.pieces+1) (placer amettre pos arbre)
                )
            )                                                    
          else
            (
             
              resoudre (q@[t]) x y (compteur+1) arbre
            )
        )
      else
        (
         
          let newliste,newarbre,nx,ny= backt lesPieces arbre x y in
          resoudre newliste nx ny (List.length arbre.pieces) newarbre
  
        ) 
;;
resoudre lesPieces  1 0 1 arbre ;;
dessinerPlateau();;
let _=read_line();;
plateau;;




