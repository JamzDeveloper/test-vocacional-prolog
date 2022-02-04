
 :- use_module(library(pce)).
 :- pce_image_directory('./imagenes').
 :- use_module(library(pce_style_item)).
 :- dynamic color/2.

 resource(img_principal, image, image('principal.jpg')).
 resource(portada, image, image('portada.jpg')).
resource(informatica,image,image('informatica.jpg')).
resource(sistemas,image,image('sistemas.jpg')).
resource(mecanica,image,image('mecanica.jpg')).
resource(industrial,image,image('industrial.jpg')).
resource(agroindustrial,image,image('agroindustrial.jpg')).
resource(agronomia,image,image('agronomia.jpg')).
resource(enfermeria,image,image('enfermeria.jpg')).
resource(trabajo_social,image,image('trabajo_social.jpg')).
resource(administracion,image,image('administracion.jpg')).


 resource(carrera_desconocida, image, image('desconocido.jpg')).


resource(programacion,image,image('Programacion.jpg')).
resource(robotica,image,image('Robotica.jpg')).
resource(lectura,image,image('Lectura.jpg')).
resource(estadistica,image,image('Estadistica.jpg')).
resource(compromiso,image,image('Compromiso.jpg')).
resource(idiomas,image,image('Idiomas.jpg')).
resource(numeros,image,image('Numeros.jpg')).
resource(disenio,image,image('Disenio.jpg')).
resource(calculo,image,image('Calculo.jpg')).
resource(servicios,image,image('Servicios.jpg')).
resource(estrategia,image,image('Estrategia.jpg')).
resource(finanzas,image,image('Finanzas.jpg')).
resource(innovacion,image,image('Innovacion.jpg')).
resource(electricidad,image,image('Electricidad.jpg')).
resource(ambiente,image,image('Ambiente.jpg')).
resource(socializar,image,image('Socializar.jpg')).
resource(biodiversidad,image,image('Biodiversidad.jpg')).
resource(ecologia,image,image('Ecologia.jpg')).
resource(economia,image,image('Economia.jpg')).
resource(alimentaria,image,image('Alimentaria.jpg')).
resource(salud,image,image('Salud.jpg')).
resource(humanidades,image,image('Humanidades.jpg')).
resource(etica,image,image('Etica.jpg')).
resource(disciplina,image,image('Disciplina.jpg')).
resource(bienestar,image,image('Bienestar.jpg')).
resource(analisis,image,image('Analisis.jpg')).
resource(hardware,image,image('Hardware.jpg')).
resource(ciencia,image,image('Ciencia.jpg')).
resource(politica_social,image,image('Politica_social.jpg')).


 mostrar_imagen(Pantalla, Imagen) :- new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(100,80)).
  mostrar_imagen_estudio(Pantalla, Imagen) :-new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(20,100)).
 nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(0,0)).
  imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
  botones:-borrado,
                send(@boton, free),
                send(@btnestudio,free),
                mostrar_test(Carrera),
                send(@texto, selection('Su posible eleccion podria ser:')),
                send(@resp1, selection(Carrera)),
                new(@boton, button('Iniciar consulta',
                message(@prolog, botones)
                )),

                new(@btnestudio,button('Detalles del resultado',
                message(@prolog, mostrar_estudio,Carrera)
                )),
                send(@main, display,@boton,point(20,450)),
                send(@main, display,@btnestudio,point(138,450)).



  mostrar_estudio(X):-new(@tratam, dialog('test')),
                         /* send(@tratam, append, label(nombre, 'Explicacion: ')),*/
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          estudio(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

estudio(X):- send(@lblExp1,selection('Informacion general de su eleccion')),
                 mostrar_imagen_estudio(@tratam,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


   preguntar(Preg,Resp):-new(Di,dialog('Test de preguntas')),
                        new(L2,label(texto,'Responde la siguiente pregunta')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  interfaz_principal:-new(@main,dialog('Sistema Experto para test de seleccion vocacional',
        size(1000,1000))),
        new(@texto, label(nombre,'Realiza el test para ver su resultado',font('times','roman',18))),
        new(@resp1, label(nombre,'',font('times','roman',22))),
        new(@lblExp1, label(nombre,'',font('times','roman',14))),
        new(@lblExp2, label(nombre,'',font('times','roman',14))),
        new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
        new(@boton, button('Iniciar consulta',message(@prolog, botones))),

        new(@btnestudio,button('¿estudio?')),

        nueva_imagen(@main, img_principal),
        send(@main, display,@boton,point(138,450)),
        send(@main, display,@texto,point(20,130)),
        send(@main, display,@salir,point(300,450)),
        send(@main, display,@resp1,point(20,180)),
        send(@main,open_centered).

       borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  crea_interfaz_inicio:- new(@interfaz,dialog('Bienvenido al Sistema Experto ',
  size(1000,1000))),

  mostrar_imagen(@interfaz, portada),

  new(BotonComenzar,button('COMENZAR',and(message(@prolog,interfaz_principal) ,
  and(message(@interfaz,destroy),message(@interfaz,free)) ))),
  new(BotonSalir,button('SALIDA',and(message(@interfaz,destroy),message(@interfaz,free)))),
  send(@interfaz,append(BotonComenzar)),
  send(@interfaz,append(BotonSalir)),
  send(@interfaz,open_centered).

  :-crea_interfaz_inicio.

/* BASE DE CONOCIMIENTOS: Sintomas y Carreraes del Pez Goldfish, contiente ademas
el identificador de imagenes de acuerdo al  sintoma
*/

conocimiento('informatica',
['te gusta la programacion', 'te gusta la lectura','te gusta la robotica'
 ,'te gusta los idiomas','tienes compromiso']).

conocimiento('sistemas',['te gusta la programacion','te gusta el hardware','te gusta el diseño','te gusta la ciencia','tienes compromiso']).

conocimiento('mecanica',['te gusta innovar','te gusta los numeros',
'te gusta la electricidad','tienes compromiso']).

conocimiento('industrial',['te gusta ofrecer servicios',
'te gusta los numeros','te gusta plantear estrategias','te gusta las finanzas','tienes compromiso']).

conocimiento('agroindustrial',['te gusta los numeros',
'te gusta el ambiente', 'te gusta la estadistica'
,'te gusta el area alimentaria','tienes compromiso']).

conocimiento('agronomia',['te gusta el ambiente','te gusta la ecologia',
'te gusta la biodiversidad','te gusta socializar','tienes compromiso']).

conocimiento('enfermeria',['te gusta la salud','te gusta el habito de humanidades',
'te gusta la etica','tienes compromiso']).


conocimiento('administracion',
['te gusta la disciplina', 'te gusta la economia',
 'te gusta la estadistica','te gusta el calculo', 'tienes compromiso']).

conocimiento('trabajo_social',
['te gusta la disciplina','te gusta la potilica social','te gusta el bienestar', 'tienes compromiso']).


 conocimiento('contabilidad',['te gusta la economia','te gusta la estadistica'
 ,'te gusta el calculo','te gusta la etica','te gusta el analisis','tienes compromiso']).



id_imagen_preg('te gusta la programacion','programacion').
id_imagen_preg('te gusta la robotica','robotica').
id_imagen_preg('te gusta la lectura','lectura').
id_imagen_preg('te gusta la estadistica','estadistica').
id_imagen_preg('tienes compromiso','compromiso').
id_imagen_preg('te gusta los idiomas','idiomas').
id_imagen_preg('te gusta los numeros','numeros').
id_imagen_preg('te gusta el diseño','disenio').
id_imagen_preg('te gusta la ciencia','ciencia').
id_imagen_preg('te gusta el hardware','hardware').
id_imagen_preg('te gusta el calculo','calculo').
id_imagen_preg('te gusta ofrecer servicios','servicios').
id_imagen_preg('te gusta plantear estrategias','estrategia').
id_imagen_preg('te gusta las finanzas','finanzas').
id_imagen_preg('te gusta innovar','innovacion').
id_imagen_preg('te gusta la electricidad','electricidad').
id_imagen_preg('te gusta el ambiente','ambiente').
id_imagen_preg('te gusta socializar','socializar').
id_imagen_preg('te gusta la biodiversidad','biodiversidad').
id_imagen_preg('te gusta la ecologia','ecologia').
id_imagen_preg('te gusta la economia','economia').
id_imagen_preg('te gusta el area alimentaria','alimentaria').
id_imagen_preg('te gusta la salud','salud').
id_imagen_preg('te gusta el habito de humanidades','humanidades').
id_imagen_preg('te gusta la etica','etica').
id_imagen_preg('te gusta la disciplina','disciplina').
id_imagen_preg('te gusta la potilica social','politica_social').
id_imagen_preg('te gusta el bienestar','bienestar').
id_imagen_preg('te gusta el analisis','analisis').



 /* MOTOR DE INFERENCIA: Esta parte del sistema experto se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */
:- dynamic conocido/1.

  mostrar_test(X):-realizar_test(X),clean_scratchpad.
  mostrar_test(carrera_desconocida):-clean_scratchpad .

  realizar_test(Study_test):-
                            obten_hipotesis_y_sintomas(Study_test, ListaDeSintomas),
                            prueba_presencia_de(Study_test, ListaDeSintomas).


obten_hipotesis_y_sintomas(Study_test, ListaDeSintomas):-
                            conocimiento(Study_test, ListaDeSintomas).


prueba_presencia_de(Study_test, []).
prueba_presencia_de(Study_test, [Head | Tail]):- prueba_verdad_de(Study_test, Head),
                                              prueba_presencia_de(Study_test, Tail).


prueba_verdad_de(Study_test, Sintoma):- conocido(Sintoma).
prueba_verdad_de(Study_test, Sintoma):- not(conocido(is_false(Sintoma))),
pregunta_sobre(Study_test, Sintoma, Reply), Reply = 'si'.


pregunta_sobre(Study_test, Sintoma, Reply):- preguntar(Sintoma,Respuesta),
                          process(Study_test, Sintoma, Respuesta, Reply).


process(Study_test, Sintoma, si, si):- asserta(conocido(Sintoma)).
process(Study_test, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


clean_scratchpad:- retract(conocido(X)), fail.
clean_scratchpad.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).
