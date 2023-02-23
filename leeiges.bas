' NOTA: solo trata entidades 314(color), 128(SURF bspline), 126(CURVA bspline), 142+144(CONS trimmed) y 116(puntos)

Declare Function leelinea(b As Integer) As String
Declare Function sacavalores(ss As String) As Integer

dim shared info as integer=1 ' 0=nada, 1=solo info entidades, 2=todo, completo mas lento
' nota: el peso de cada coordenada no sirve para nada, segun veo, siempre son 1's
Dim Shared peso As Integer=1 ' 0= no extraer el peso de cada coordenada, 1= extraer y guardar

' variables genericas
Dim Shared As Integer d,e,f,g,h,I,j,l 
Dim shared As String sa,sb,sc,sd,se,sf
Dim Shared As Single da,db,dc,dd,de,df


' valores de las entidades
' K1 y K2 son KNOTS o NUDOS en superficies (K en caso de curvas)
' M1 y M2 son grados en superficies (M1xM2) (M en caso de curvas)
' N1 y N2 son valores intermedios para calculos (N en caso de curvas)
' A,B,C son valores sumatorios para llegar a cada campo de datos a partir de N1 y N2 (o N)
dim shared As Integer K1,K2,M1,M2,N1,n2 ' PARA SUPERFICIES
Dim Shared As Integer K,M,N ' PARA CURVAS
Dim Shared As Integer A,B,C ' comunes ambos elementos
Dim Shared As Integer PROP1,PROP2,PROP3,PROP4,PROP5 ' propiedades de las entidades, por ejemplo PROP1=1, es curva plana
Dim Shared As Double  X,Y,Z ' coordenadas

Dim Shared As String NCOLOR ' indice de color segun DATOSENTIDAD(13) de la cabecera "D", segunda fila

Dim Shared linealong As Integer ' longitud total de la linea del IGES
Dim Shared ficlong As Integer ' longitud total del IGES

Dim Shared final As String ' almacena la linea final del IGES, la que contiene los datos totales 
Dim Shared ncabecera As Integer ' numero de lineas de la cabecera

Dim Shared nglobal As Integer ' numero de lineas globales
Dim Shared iniglobal As Integer ' inicio de datos

Dim Shared nparametros As Integer ' numero de lineas de parametros
Dim Shared iniparametros As Integer ' inicio de datos

Dim Shared ndatos As Integer ' numero de lineas de datos
Dim Shared inidatos As Integer ' inicio de datos

Dim Shared datosglobal(26) As string ' parametros globales, 26 campos 
Dim Shared datosentidad(18) As string ' datos de cada entidad, 20 campos

' almacen de datos, cuanto mayor sea la surfa, mayor debe ser este valor
' como ejemplo TERRIBLE, me encontre una surfa de 410x212!!!! 
' cuyo volumen de datos ascencia a la friolera de 4.000.000 caracteres, o sea, un 4megas
' de momento, lo ponemos en 2 megas, que admitiria una surfa de unos 500x500, casi na, una entre un millon
dim shared as integer maxmegas=8000000
Dim Shared temporal(maxmegas) As String*32 ' temporal de almacenamiento GIGANTESCO (256 megas del ala)
Dim Shared valores(maxmegas) As Single ' para guardar datos temporales (8megas por 4 bytes, 32 megas)

Dim Shared entidades(520) As String
Dim Shared entidad As Integer ' numero de entidad a estudiar (por ejemplo, la 128 es surfa, la 126 curva, etc)
Dim Shared nentidad As Integer ' numero P asociado a la entidad a leer

' leemos tipos de entidades, solo para referencia en pantalla
Restore
For f=1 To 1000
	Read a,sa
	If a=0 Then Exit for
	entidades(a)=sa
Next

a=1

dim as string nomfic
nomfic=command
if nomfic="" then nomfic="1.igs"

nomfic=left(nomfic,instr(nomfic,".")-1)

Open nomfic+".igs" For Input As 1
Open nomfic+".txt" For Output As 2

' primero miramos si el IGS lleva CR, LF, ambos o ninguno
sa=" "
b=0
For f=1 To 90
	get #1,f,sa
	If sa=Chr(13) Then b+=1
	If sa=Chr(10) Then b+=1
Next
If b=0 Then linealong=80 ' ninguno
If b=1 Then linealong=81 ' solo CR o solo LF
If b=2 Then linealong=82 ' ambos
ficlong=Lof(1)\linealong

final=leelinea(ficlong-1) ' lee la linea ultima (la "T")
' descompone
ncabecera  =Val(Mid(final, 2,7))
nglobal    =Val(Mid(final,10,7))
ndatos     =Val(Mid(final,18,7))
nparametros=Val(Mid(final,26,7))

iniglobal=ncabecera
inidatos=iniglobal+nglobal
iniparametros=inidatos+ndatos

' mostramos las primeras lineas de cada campo
'If info then print left(leelinea(iniglobal),80-8)
'If info then print left(leelinea(inidatos),80-8)
'If info then print left(leelinea(iniparametros),80-8)

' mostramos la cabecera
For f=0 To ncabecera-1
	print left(leelinea(f),80)
	if linealong=80 then print 'sino lleva CR+LF le obligo a saltar linea
next
print
print "LINEA FINAL DEL IGES:"
' mostramos el final
print left(leelinea(ficlong-1),80)
print
' mostramos longitudes de campos (el global suele ser 2 en la inmensa mayoria de casos)
print "Globales:";nglobal
print "datos:";ndatos
print "Parametros:";nparametros
print


'If info then print left(leelinea(a),80);

' ---------------------------------------
' recogemos la zona global y la interpretamos
sb=""
For f=0 To nglobal-1 ' son 26 campos "G" que pueden ocupar entre 1 y "nglobal" lineas
	sa=RTrim(Left(leelinea(f+iniglobal),80-8))
	sb=sb+sa
Next
' se descompone ahora
sa=""
a=0
a=sacavalores(sb)

print "Datos globales:"
For f=1 To a
	datosglobal(f)=temporal(f)
	print f;"= ";datosglobal(f)
Next
print "----------------"
' --------------------------------------


sigueleyendo:
' ---------------------------------------
' ahora, cogemos entidades, mas complejo y largo
' cada entidad comienza con dos lineas de datos que dicen como van a ser sus parametros
' esas dos lineas de datos contienen 20 campos de 8 caracteres, de los cuales dos son numeros de orden
' y dos estan reservados, por lo que quedan 16 campos a leer
sa=Left(leelinea(inidatos+0),80)
sb=Left(leelinea(inidatos+1),80)
sa=sa+sb
a=0
inidatos+=2 ' pasamos a la siguiente linea "D" para cuando volvamos a leer

' si no es una entidad "D" de datos, hemos acabado con todas
If Mid(sa,153,1)<>"D" Then GoTo fin ' hemos leido todas ya

' la num. 13 es el color, como indice, no como RGB, o sea, un numero de color
'print "Datos Entidad:"
For f=1 To Len(sa) Step 8
	a+=1
	datosentidad(a)=Mid(sa,f,8)
	'Print datosentidad(a)
	'If a=13 Then Print "IND. COLOR:";datosentidad(a)
Next
' entidad de color. creo que solo existen 8 colores en IGES, si pasa de 8 seria de usuario
' y se indica con valores negativos???
NCOLOR=Trim(datosentidad(13))
' los colores del tebis que salen en negativo :
'   creo que es un indicativo para usar colores de usuario, la entidad 314
'   como entiendo que es asi, lo paso a positivo, y lo indico al guardar el valor
If Left(NCOLOR,1)="-" Then NCOLOR=Mid(NCOLOR,2)+" USUARIO"
' asi, NCOLOR apunta al color de usuario entidad 314 del mismo numero (antes de la "P" justo)

' ------------------------------------


' ------------------------------------
' ya podemos coger una entidad completa, por que sabemos su numero y longitud
a=Val(Mid(datosentidad(10),2)) ' numero de entidad a buscar en los parametros
l=Val(datosentidad(14)) ' numero de lineas que conforman los datos a leer

nentidad=a ' guardo el numero de P que se asocia a la entidad a leer

sa=""
For f=1 To l
	sb=RTrim(Left(leelinea(iniparametros),80-16)) ' todo menos el "P" y el numero de orden (16 bytes)
   iniparametros+=1 ' pasamos a la siguiente linea
   sa=sa+sb
Next
If info=2 then print sa

' miramos si nos hemos pasado de longitud tratable
if len(sa) > maxmegas then 
	print "MeCagonLaFruta: superficie megagigante ";len(sa);" de datos...."
	print "Estudia el tema en la entidad:";a
	sleep:end
endif

' ----------------------------------------------------------------------------
' tratamos cada entidad segun lo que sea (curva, surfa, face, color, eje, etc)
' ----------------------------------------------------------------------------
                           entidad=Val(Mid(sa,1,3))

If info then ' con INFO=1 solo mostramos esto, con INFO=2, se muestra esto mas trogollon de info extra
	' para acelerar el proceso, podemos NO mostrar esto.
	' como son impares, lo pasamos a pares
	Color 7 ' por defecto
	
	' CACHONDEO con el texto de la entidad 314 (COLOR) ... jeje
	' imprimo en tres colores RGB, jaja
	If entidad=314 Then 
		Color 11
		Print "Tratando entidad:";(a-1)/2+1;" Tipo:";entidad;" --> ";
		'sa=entidades(entidad) ' el texto es "Color definition entity", pero no lo empleo
		Color 4:Print "Color "; ' ROJO
		Color 2:Print "Definition "; ' VERDE
		Color 1:Print "Entity" ' AZUL
	Else
		If entidad=142 Then Color 2 ' cons
		If entidad=128 Then Color 3 ' surfas
		If entidad=116 Then Color 4 ' puntos
		If entidad=144 Then Color 5 ' face
		If entidad=126 Then Color 6 ' bspline curve
		Print "Tratando entidad:";(a-1)/2+1;" Tipo:";entidad;" --> ";entidades(entidad)
		Color 7
	EndIf
	
EndIf





/' ################################################################################################################# '/
' ----------------------------------------------------------------------------

' entidades 314, COLORS
' NOTA: no tiene interes por ahora, por que no van asociados a las SURPERFICIES
'       son mas bien, colores de usuario, para ejes y alguna otra cosa que desconzoco
'       el color como tal lo indica la cabecera tipo "D" en la pos. 13, como un INDICE
If entidad=314 Then
	e=sacavalores(sa) 
	Print #2,"; COLOR:";nentidad;
   ' los colores en IGES son estandar solo del 0 al 8
   ' el resto se consideran de usuario y se indican en negativo
   ' los niveles van de 0 a 100 solo, no llegan a 255
   Print #2," R";Int(valores(1));" G";Int(valores(2));" B";Int(valores(3))
EndIf







/' ################################################################################################################# '/
' entidades 116, POINTS
If entidad=116 Then
	Print #2,"; ################################################################"
	e=sacavalores(sa)

	' el punto es el mas simple de todos, solo lleva las tres XYZ, nada mas
   Print #2,"; PUNTO:";nentidad;" COLOR:";NCOLOR
   ' guardamos la cosa mas simple del mundo
   Print #2, Using " X######.###";valores(1);
   Print #2, Using " Y######.###";valores(2);
   Print #2, Using " Z######.###";valores(3)
 	
EndIf





/' ################################################################################################################# '/
' ----------------------------------------------------------------------------

' entidades 142, CONS, lo que pienso que es asociacion de curvas sobre superficies para obtener FACES tipo 144
If entidad=142 Then
	Print #2,"; ################################################################"	
	e=sacavalores(sa)

	' solo lleva 5 parametros: tipo de curva usada,  de superf.,  de curva, otro puntero de curva y desconocido
   Print #2,"; CONSTANTE PARAMETRICA:";nentidad;" COLOR:";NCOLOR
   ' guardamos la cosa mas simple del mundo
   Print #2, Using "         TIPO:######";valores(1)
   Print #2, Using "   SUPERFICIE:######";valores(2)
   Print #2, Using "      CURVA 1:######";valores(3)
   Print #2, Using "      CURVA 2:######";valores(4)
   Print #2, Using "   NIPUTAIDEA:######";valores(5)	
EndIf

' ----------------------------------------------------------------------------

' entidades 144, TRIMED SURFACE, entidad que asocia una curva y una surfa mediante entidad CONS 142, para sacar una FACE
If entidad=144 Then
	Print #2,"; ################################################################"	
	e=sacavalores(sa)

	' el unico parametro a mirar bien, es el  de curvas que integran la face (curvas de recortes interiores)
	' por que el borde exterior es siempre uno, solo varian los interiores, que pueden ser desde 0 hasta el mas alla
	PROP1=valores(1) ' SURFA A RECORTAR
	PROP2=valores(2) ' TIPO DE BORDE, 0=UNICO, SU PROPIO BORDE, 1=BORDE EXT+INTERIORES
	PROP3=valores(3) '  de curvas de recorte interiores
	PROP4=valores(4) ' esta es SIEMPRE el borde exterior, si o si
	
   Print #2,"; SUPERFICIE RECORTADA:";nentidad;" COLOR:";NCOLOR
   ' guardamos la cosa mas simple del mundo
   Print #2, Using "       SUPERFICIE:######";PROP1
   Print #2, Using "  TIPO DE RECORTE:######";PROP2; 
   If PROP2=1 Then Print #2," LLEVA RECORTES INTERIORES" Else Print #2," SOLO BORDE EXTERIOR"
   Print #2, Using "   NUM CURVAS INT:######";PROP3
   Print #2, Using "        BORDE EXT:######";PROP4
   For f=0 To PROP3-1
	   Print #2, Using "     CURVA INT ##:######";f+1;valores(5+F)
   Next	
EndIf







/' ################################################################################################################# '/
' ----------------------------------------------------------------------------

' entidades 126, BSPLINE CURVE
If entidad=126 Then
	Print #2,"; ################################################################"	
	e=sacavalores(sa)

	K=valores(1) ' nudos o KNOTS
	M=valores(2) ' grado (empieza en 0, por lo tanto, es siempre +1, osea, M=1 es grado 2
	' de 3 a 6 son PROP1 a PROP4 que indican tipo varios:curva plana=1, cerrada=1, polinomica=1 o periodica=1
	PROP1=valores(3) ' 1=PLANA      0=3D, influye en el  de datos a leer en total (plana tiene un valor menos)
	PROP2=valores(4) ' 1=CERRADA    0=ABIERTA
	PROP3=valores(5) ' 1=POLINOMICA 0=RACIONAL
	PROP4=valores(6) ' 1=PERIODICA  0=NO PERIODICA
	
	' calculamos medidas de curva
	N = 1+K-M 
   A = N+2*M  
   
	' escribimos el formato que se nos salga de los .... pataplines...
   Print #2,"; CURVA";IIf(PROP1," PLANA"," 3D");IIf(PROP2," CERRADA "," ABIERTA ");nentidad;":";K+1;" NUDOS DE GRADO";M;" COLOR:";NCOLOR

   ' secuencia de nudos
   I = 7   ' primer valor
   J = 7+A ' ultimo valor
   Print #2," ; NUDOS CURVA ->";A+1;" (DE 0 A";A;")"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next

	' solo si activo el parametro de peso arriba del todo.
	' he visto que el peso no sirve para nada, y siempre son 1's
   If peso Then
	   ' peso de cada nudo
	   I = 8+A     ' primer valor
	   J = 8+(A+K) ' ultimo valor
	   Print #2," ; PESO DE COORDENADAS (";K+1;" VALORES )"
	   For f=I To J
	   	Print #2, valores(f+0);
	   	If f<>J Then Print #2,","; Else Print #2,""
	   Next
   End If
   
   ' COORDENADAS
   I = 9+(A+K)     ' primer XYZ
   J = 9+(A+(4*K)) ' ultimo XYZ
   Print #2," ; COORDENADAS XYZ (";K+1;" DATOS )"
   For f=I To j Step 3
   	Print #2, Using " X######.###";valores(f+0);
   	Print #2, Using " Y######.###";valores(f+1);
   	Print #2, Using " Z######.###";valores(f+2)
   Next

   ' valores parametricos en V (al ser curva, no lleva en U, solo V)
   I = 12+(A+(4*K)) ' primer valor
   J = 13+(A+(4*K)) ' ultimo valor
   Print #2," ; VALOR U"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next	     

   ' valor de NORMAL a curva, SOLO SI ES PLANA (si es 3D este dato NO EXISTE)
	If PROP1 Then 
	   I = 14+(A+(4*K)) ' primer XYZ
	   J = 16+(A+(4*K)) ' ultimo XYZ
	   Print #2," ; NORMAL AL PLANO"
	   For f=I To j Step 3
	   	Print #2, Using " X######.###";valores(f+0);
	   	Print #2, Using " Y######.###";valores(f+1);
	   	Print #2, Using " Z######.###";valores(f+2)
	   Next
	End If
   
EndIf





/' ################################################################################################################# '/
' ----------------------------------------------------------------------------

' entidades 128, BSPLINE SURFACE
If entidad=128 Then
	Print #2,"; ################################################################"	
	e=sacavalores(sa)

	' nota: valores(0) es SIEMPRE el tipo de entidad, que como ya lo sabemos, no se usa
	' segun IGES, estos son los valores a tratar
	K1=valores(1) ' nudos (KNOTS)
	K2=valores(2)
	M1=valores(3) ' grados (empieza en 0, o sea, grado 1 x grado 1=2x2)
	M2=valores(4)
	' de 5 a 9 son PROP1 a 5 que indican tipo de surfa
	' NOTA: en general, solo he visto 0,0,1,0,0 en cada surfa que he tratado
	PROP1=valores(5) ' 1=cerrado en primera direccion parametrica,   0=no cerrado
	PROP2=valores(6) ' 1=cerrado en segunda direccion parametrica,   0=no cerrado
	PROP3=valores(7) ' 1=polinomico,                                 0=racional
	PROP4=valores(8) ' 1=periodico en primera direccion parametrica, 0=no periodico
	PROP5=valores(9) ' 1=periodico en segunda direccion parametrica, 0=no periodico
		
	' calculamos medidas de surfa
	N1 = 1+K1-M1
    N2 = 1+K2-M2
    A  = N1+2*M1
    B  = N2+2*M2
    C  = (1+K1)*(1+K2)
	
	' escribimos el formato que se nos salga de los .... pataplines...
   Print #2,"; SUPERFICIE";nentidad;": GRADO ";trim(Str(M1));"x";trim(Str(M2));"(";trim(Str(K1+1));"x";trim(Str(K2+1));" NUDOS=";trim(Str((K1+1)*(K2+1)));" VALORES)";" COLOR:";NCOLOR 

   ' primera secuencia de nudos 
   I = 10   ' primer valor
   J = 10+A ' ultimo valor
   Print #2," ; PRIMEROS NUDOS EN 'U' ->";A+1;" (DE 0 A";A;")"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next

   ' segunda secuencia de nudos
   I = 11+A     ' primer valor
   J = 11+(A+B) ' ultimo valor
   Print #2," ; SEGUNDOS NUDOS EN 'V' ->";B+1;" (DE 0 A";B;")"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next

	' solo si activo el parametro de peso arriba del todo.
	' he visto que el peso no sirve para nada, y siempre son 1's
   If peso Then  
	   ' peso de cada nudo
	   I = 12+(A+B)   ' primer valor
	   J = 11+(A+B+C) ' ultimo valor
	   Print #2," ; PESO DE COORDENADAS (";(K1+1)*(K2+1);" VALORES )"
	   For f=I To J
	   	Print #2, valores(f+0);
	   	If f<>J Then Print #2,","; Else Print #2,""
	   Next
   End If
   
   ' coordenadas XYZ
   I = 12+(A+B+C)     ' inicio coordenadas XYZ (documentacion IGES) 
   J = 11+(A+B+(4*C)) ' final, ultimo valor Z
   Print #2," ; COORDENADAS XYZ (";(K1+1)*(K2+1);" VALORES )"
   For f=I To J Step 3 
   	Print #2, Using " X######.###";valores(f+0);
   	Print #2, Using " Y######.###";valores(f+1);
   	Print #2, Using " Z######.###";valores(f+2)
   Next

   ' valores parametricos en U
   I = 12+(A+B+(4*C)) ' primer valor
   J = 13+(A+B+(4*C)) ' ultimo valor
   Print #2," ; VALORES U"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next	

   ' valores parametricos en V
   I = 14+(A+B+(4*C)) ' primer valor
   J = 15+(A+B+(4*C)) ' ultimo valor
   Print #2," ; VALORES V"
   For f=I To J
   	Print #2, valores(f+0);
   	If f<>J Then Print #2,","; Else Print #2,""
   Next	  
   
EndIf
/' ################################################################################################################# '/







' ----------------------------------------------------------------------------

' seguimos con mas entidades hasta acabar
'sleep
GoTo sigueleyendo ' un simple bucle, a mi gusto, con GOTO, a tomar viento.


' ------------------------------------
fin:
Close
Color 7
print "Mos acabao...."
Sleep

End


Function leelinea(n As Integer) As string
	Dim b As String
	Dim nn As Integer
		b=space(80) ' mida lo que mida, solo leemos el estandar de 80
		nn=(n*linealong)+1 ' cada linea del igs es de 80 caracteres fijos+CR o LF
		Seek #1,nn
		Get #1,,b
	Return b
End Function

' descompone una cadena de texto separada con comas en campos sueltos
' y devuelve el total encontrado
Function sacavalores(ss As String) As Integer
   Dim a As Integer
   Dim oldf As Integer
   Dim f As Integer

   a=-1 ' empiezo en -1 para que la primera entidad se guarde en la "0", que es el tipo de entidad (128 sup., 126 curva, etc)
   oldf=1

	For f=1 To Len(ss)
	If Mid(ss,f,1)="," Or Mid(ss,f,1)=";" Then 
		a+=1
		If f-oldf<>0 Then
		  temporal(a)=Mid(ss,oldf,f-oldf) 
		Else
		  temporal(a)=""
	   End If
		oldf=f+1
	EndIf
	Next

	' guardamos definitivo
	For f=0 To A
			valores(f)=Val(temporal(f)) 
		   if info=2 Then print A,valores(f) ' ojo, que puede ser tremendo lo que salga, imagina 2000 elementos.......
	Next
	
	Return a

End Function

entidades:
data 514 , "Shell Entity"
data 510 , "Face Entity"
data 508 , "Loop Entity"
data 504 , "Edge Entity"
data 502 , "Vertex Entity"
data 416 , "External reference entity"
data 408 , "Singular subfigure instance entity"
data 406 , "Form 12 External reference file"
data 402 , "Form 7 Group entity"
data 314 , "Color definition entity"
data 308 , "Subfigure definition entity"
data 198 , "Toroidal Surface"
data 196 , "Spherical Surface"
data 194 , "Right Circular Conical Surface"
data 192 , "Right Circular Cylindrical Surface"
data 190 , "Plane Surface"
data 186 , "Manifold Solid B-Rep Object Entity"
data 144 , "Trimmed (parametric) surface"
data 143 , "Import only Bounded Surface"
data 142 , "Curve on a parametric surface"
data 140 , "Import only Offset surface"
data 128 , "Rational B-spline surface"
data 126 , "Rational B-spline curve"
data 124 , "Transformation matrix entity"
data 123 , "Direction entity"
data 122 , "Tabulated cylinder"
data 120 , "Surface of revolution"
data 118 , "Import only Ruled surface"
data 116 , "Point"
data 114 , "Import only Parametric spline surface"
data 112 , "Parametric spline curve"
data 110 , "Line"
data 108 , "Plane"
data 106 , "Copious Data—3D Piecewise linear curve"
Data 104 , "Import only Conic arc"
data 102 , "Composite curve"
data 100 , "Circular arc"
Data 000 , ""

