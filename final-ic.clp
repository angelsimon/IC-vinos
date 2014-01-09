;;----------------------------------------------------------------------------;;
;; Sistema experto de selección de vinos v2.0																	;;
;;																																						;;
;;																																						;;
;; Descripcion:																															  ;;
;; ------------																															  ;;
;; Leer documento adjunto ic-proyecto-final.pdf																;;
;;																																						;;
;; Instrucciones:																														  ;;
;;	--------------																														;;
;; 1 - Copiar los archivos al disco. ej " C:\ " o " \home\user\ic\ "  				;;
;; 2 - CLIPS> (load ruta-archivo) ej: (load 'C:\final-ic.clp') 							  ;;
;; 3 - CLIPS> (reset)																												  ;;
;; 4 - CLIPS> (run)																													  ;;
;;																																						;;
;; Licencia: 																																	;;
;; ---------																																	;;
;; WTFPL 2.0 - http://es.wikipedia.org/wiki/WTFPL															;;
;;----------------------------------------------------------------------------;;

;;********************************************
;; PLANTILLAS
;;********************************************
(deftemplate vino 
	(slot id)
  (slot nombre (default ?NONE)) 
  (slot tipo) 
	(slot precio (default 0))
)

;;********************************************
;; LISTA DE HECHOS - VINOS
;;********************************************
(deffacts lista-de-vinos
  (vino (id 1) (nombre "Beaujolais") (tipo tinto) (precio 40))
	(vino (id 2) (nombre "Bordeaux tinto") (tipo tinto) (precio 60))
	(vino (id 3) (nombre "Cabernet franc") (tipo tinto) (precio 55))
	(vino (id 4) (nombre "Cabernet sauvignon") (tipo tinto) (precio 45))
	(vino (id 5) (nombre "Carmenere") (tipo tinto) (precio 70))
	(vino (id 6) (nombre "Malbec") (tipo tinto) (precio 60))
	(vino (id 7) (nombre "Merlot") (tipo tinto) (precio 50))
	(vino (id 8) (nombre "Pinot noir") (tipo tinto) (precio 40))
	(vino (id 9) (nombre "Pinotage") (tipo tinto) (precio 50))
	(vino (id 10) (nombre "Sangiovanese") (tipo tinto) (precio 60))
	(vino (id 11) (nombre "Syrah") (tipo tinto) (precio 70))
	(vino (id 12) (nombre "Tempranillo") (tipo tinto) (precio 80))
	(vino (id 13) (nombre "Zinfandel") (tipo tinto) (precio 65))
	(vino (id 14) (nombre "Albariño") (tipo blanco) (precio 40))
	(vino (id 15) (nombre "Bordeaux blanco") (tipo blanco) (precio 60))
	(vino (id 16) (nombre "Chablis") (tipo blanco) (precio 50))
	(vino (id 17) (nombre "Chardonnay") (tipo blanco) (precio 70))
	(vino (id 18) (nombre "Pinot grigio") (tipo blanco) (precio 60))
	(vino (id 19) (nombre "Riesling") (tipo blanco) (precio 50))
	(vino (id 20) (nombre "Sauvignon blanc") (tipo blanco) (precio 60))
	(vino (id 21) (nombre "Rosado") (tipo rosado) (precio 90))
	(vino (id 22) (nombre "Espumante brut") (tipo espumante) (precio 120))
	(vino (id 23) (nombre "Espumante demi-sec") (tipo espumante) (precio 100))
	(vino (id 24) (nombre "Espumante rose") (tipo espumante) (precio 90))
	(vino (id 25) (nombre "Espumante dulce") (tipo espumante) (precio 80))
	(vino (id 26) (nombre "Espumante seco") (tipo espumante) (precio 70))	
	(vino (id 27) (nombre "Jerez") (tipo otro) (precio 70))
	(vino (id 28) (nombre "Oporto") (tipo otro) (precio 90))
)

;;********************************************
;; FUNCIONES
;;********************************************
(deffunction preguntar (?pregunta $?respuestas-permitidas) 
	(printout t ?pregunta) 
	(bind ?respuesta (read)) 
	(if (lexemep ?respuesta) then (bind ?respuesta(lowcase ?respuesta))) 
	(while (not (member ?respuesta ?respuestas-permitidas)) do 
		(printout t ?pregunta) 
		(bind ?respuesta (read)) 
		(if (lexemep ?respuesta) then (bind ?respuesta(lowcase ?respuesta)))) ?respuesta
)
		
(deffunction preguntar-si-o-no (?pregunta)
	(bind ?respuesta (preguntar ?pregunta yes y si s 1 no n 0))
	(if (or (eq ?respuesta si) (eq ?respuesta s) (eq ?respuesta 1) (eq ?respuesta yes) (eq ?respuesta y)) then 
		TRUE
  else 
		FALSE
	)
)

;;********************************************
;; REGLAS DE INTRO
;;********************************************
(defrule inicio
	(declare (salience 10))
	=>
	(printout t "#########################################################" crlf)
	(printout t "### SISTEMA SELECCIONADOR DE VINOS v2.0               ###" crlf)
	(printout t "#########################################################" crlf crlf)
	(printout t "PREGUNTAS INICIALES:" crlf)
	(printout t "--------------------" crlf)	
	(assert (accion-actual p-toma-alcohol))
)

(defrule hacer-pregunta-toma-alcohol
	(accion-actual p-toma-alcohol)
	=>
	(assert (respuesta-toma-alcohol (preguntar-si-o-no "### ¿Tomas alcohol [si/no] ? ")))
)

(defrule respuesta-toma-alcohol-si
	(respuesta-toma-alcohol TRUE)
	?prox <- (accion-actual p-toma-alcohol)
	=>
	(assert (usuario-toma-alcohol si))
	(retract ?prox)
	(assert (accion-actual p-tipo-vino))
)

(defrule respuesta-toma-alcohol-no
	(respuesta-toma-alcohol FALSE)
	?prox <- (accion-actual p-toma-alcohol)
	=>
	(assert (usuario-toma-alcohol no))
	(retract ?prox)
	(assert (accion-actual correr-nuevamente))
	(printout t crlf "### Este sistema sugiere vinos. No funciona si no vas a tomar una bebida con alcohol. :P" crlf crlf)
)

(defrule hacer-pregunta-tipo-vino
	?prox <- (accion-actual p-tipo-vino)
	(usuario-toma-alcohol si)
	=>
	(assert (usuario-tipo-vino (preguntar "### ¿Que tipo de vino preferis [tinto/blanco/rosado/espumante/otro/indiferente]: ? " tinto blanco rosado espumante otro indiferente)))
	(retract ?prox)
	(assert (accion-actual p-tipo-comida))
)

(defrule hacer-pregunta-tipo-comida
	?prox <- (accion-actual p-tipo-comida)
	(usuario-toma-alcohol si)
	=>
	(assert (usuario-tipo-comida (preguntar "### ¿Que tipo de comida preferis [entrada/plato-fuerte/postre]: ? " entrada plato-fuerte postre)))
	(retract ?prox)
	(assert (accion-actual p-cantidad-dinero))
)
 
(defrule hacer-pregunta-cantidad-dinero
	?prox <- (accion-actual p-cantidad-dinero)
	(usuario-toma-alcohol si)
	=>
	(printout t "### ¿Cuanto dinero esta dispuesto a pagar por el vino ? $ ")
	(assert (usuario-total-dinero (read)))
	(retract ?prox)
	(assert (accion-actual p-confirmar-datos))
)

(defrule hacer-pregunta-datos-correctos
	(accion-actual p-confirmar-datos)
	=>
	(assert (respuesta-confirmar-datos (preguntar-si-o-no "### ¿La informacion ingresada previamente, es correcta [si/no]: ?")))
)

(defrule respuesta-datos-correctos-si
	?prox <- (accion-actual p-confirmar-datos)
	(respuesta-confirmar-datos TRUE)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-info))
)

(defrule respuesta-datos-correctos-no
	?prox <- (accion-actual p-confirmar-datos)
	(respuesta-confirmar-datos FALSE)
	=>
	(retract ?prox)
	(assert (accion-actual resetear))
)

(defrule mostrar-informacion-ingresada
	(usuario-toma-alcohol si)
	(accion-actual mostrar-info)
	=>
	(printout t crlf crlf "DATOS DEL USUARIO:" crlf)
	(printout t "--------------------" crlf)
)

(defrule mostrar-usuario-toma-alcohol
	(accion-actual mostrar-info)
	(usuario-toma-alcohol ?user-toma-alcohol)
	=>
	(printout t "### TOMA ALCOHOL: " ?user-toma-alcohol crlf)
)

(defrule mostrar-usuario-tipo-vino
	(accion-actual mostrar-info)
	(usuario-tipo-vino ?user-tipo-vino)
	=>
	(printout t "### TIPO VINO: " ?user-tipo-vino crlf)
)

(defrule mostrar-usuario-tipo-comida
	(accion-actual mostrar-info)
	(usuario-tipo-comida ?user-tipo-comida)
	=>
	(printout t "### TIPO COMIDA: " ?user-tipo-comida crlf)
)

(defrule mostrar-usuario-cantidad-dinero
	(accion-actual mostrar-info)
	(usuario-total-dinero ?user-total-dinero)
	=>
	(printout t "### TOTAL DINERO: $" ?user-total-dinero crlf)
)

(defrule determinar-estado-inicial
	?prox <- (accion-actual mostrar-info)
	=>
	(retract ?prox)
	(assert (accion-actual estado-inicial))
)

(defrule resetear
	(accion-actual resetear)
	=>
	(reset)
	(run)
)

(defrule hacer-pregunta-correr-nuevamente
	(accion-actual correr-nuevamente)
	=>
	(assert (respuesta-correr-nuevamente (preguntar-si-o-no "### ¿Queres ejecutar nuevamente la aplicacion [si/no] ? ")))
)

(defrule respuesta-correr-nuevamente-si
	?prox <- (accion-actual correr-nuevamente)
	(respuesta-correr-nuevamente TRUE)
	=>
	(retract ?prox)
	(assert (accion-actual resetear))
)

(defrule respuesta-correr-nuevamente-no
	?prox <- (accion-actual correr-nuevamente)
	(respuesta-correr-nuevamente FALSE)
	=>
	(retract ?prox)
	(assert (accion-actual byebye))
)

(defrule finalizar-aplicacion
	(accion-actual byebye)
	=>
	(printout t "### Presiona una tecla para finalizar..." crlf crlf)
	(get-char)
)

;;********************************************
;; ESTADO INICIAL
;;********************************************

(defrule mostrar-inicio-preguntas-comida
	(accion-actual estado-inicial)
	=>
	(printout t crlf crlf "PREGUNTAS DE COMIDA:" crlf)
	(printout t "--------------------" crlf)
)

(defrule eligio-entrada
	?prox <- (accion-actual estado-inicial)
	(usuario-tipo-comida entrada)
	=>
	(retract ?prox)
	(assert (accion-actual determinar-comida))
	(assert (entrada-1))
)

(defrule eligio-plato-fuerte
	?prox <- (accion-actual estado-inicial)
	(usuario-tipo-comida plato-fuerte)
	=>
	(retract ?prox)
	(assert (accion-actual determinar-comida))
	(assert (plato-fuerte-1))
)

(defrule eligio-postre
	?prox <- (accion-actual estado-inicial)
	(usuario-tipo-comida postre)
	=>
	(retract ?prox)
	(assert (accion-actual determinar-comida))
	(assert (postre-1))
)
	
;;********************************************
;; PREGUNTAS
;;********************************************

;;ENTRADA
;;========

(defrule entrada-dips
	(entrada-1)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-dips (preguntar-si-o-no "### ¿La entrada consiste en dips o nachos [si/no] ? ")))
)

(defrule entrada-dips-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-dips TRUE)
	=>
	(assert (comida dips))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule entrada-dips-no
	(accion-actual determinar-comida)
	(respuesta-entrada-dips FALSE)
	=>
	(assert (entrada-2))
)

(defrule entrada-queso
	(entrada-2)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-queso (preguntar-si-o-no "### ¿La entrada consiste en algun tipo de queso [si/no] ? ")))
)

(defrule entrada-queso-si
	(respuesta-entrada-queso TRUE)
	=>
	(assert (entrada-3))
)

(defrule entrada-queso-no
	(respuesta-entrada-queso FALSE)
	=>
	(assert (entrada-5))
)

(defrule entrada-queso-suave
	(entrada-3)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-queso-suave (preguntar-si-o-no "### ¿La entrada consiste en algun tipo de queso suave [si/no] ? ")))
)

(defrule entrada-queso-suave-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-queso-suave TRUE)
	=>
	(assert (comida queso-suave))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule entrada-queso-suave-no
	(respuesta-entrada-queso-suave FALSE)
	=>
	(assert (entrada-4))
)

(defrule entrada-queso-fuerte
	(entrada-4)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-queso-fuerte (preguntar-si-o-no "### ¿La entrada consiste en algun tipo de queso fuerte [si/no] ? ")))
)

(defrule entrada-queso-fuerte-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-queso-fuerte TRUE)
	=>
	(assert (comida queso-fuerte))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule entrada-queso-fuerte-no
	(respuesta-entrada-queso-fuerte FALSE)
	=>
	(assert (entrada-5))
)

(defrule entrada-tabla-fiambre
	(entrada-5)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-tabla-fiambre (preguntar-si-o-no "### ¿La entrada consiste en una tabla de fiambres o picada [si/no] ? ")))
)

(defrule entrada-tabla-fiambre-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-tabla-fiambre TRUE)
	=>
	(assert (comida tabla-fiambre))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule entrada-tabla-fiambre-no
	(respuesta-entrada-tabla-fiambre FALSE)
	=>
	(assert (entrada-6))
)

(defrule entrada-sandwich
	(entrada-6)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-entrada-sandwich (preguntar-si-o-no "### ¿La entrada consiste en un sandwich liviano [si/no] ? ")))
)

(defrule entrada-sandwich-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-sandwich TRUE)
	=>
	(assert (comida sandwich))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule entrada-sandwich-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-entrada-sandwich FALSE)
	=>
	(assert (no-entrada))
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
)

;;PLATO FUERTE
;;============

(defrule comida-internacional
	(plato-fuerte-1)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-comida-internacional (preguntar-si-o-no "### ¿El plato principal consiste en alguna variedad de comida internacional [si/no]? ")))
)

(defrule comida-internacional-si
	(respuesta-comida-internacional TRUE)
	=>
	(assert (plato-fuerte-2))
)

(defrule comida-internacional-no
	(respuesta-comida-internacional FALSE)
	=>
	(assert (plato-fuerte-4))
)

(defrule comida-asiatica
	(plato-fuerte-2)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-comida-asiatica (preguntar-si-o-no "### ¿El plato principal consiste en alguna variedad de comida asiatica [si/no] ? ")))
)

(defrule comida-asiatica-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-comida-asiatica TRUE)
	=>
	(assert (comida comida-asiatica))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-asiatica-no
	(respuesta-comida-asiatica FALSE)
	=>
	(assert (plato-fuerte-3))
)

(defrule comida-mexicana-cubana
	(plato-fuerte-3)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-comida-mexicana-cubana (preguntar-si-o-no "### ¿El plato principal consiste en alguna variedad de comida mexicana o cubana [si/no] ? ")))
)

(defrule comida-mexicana-cubana-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-comida-mexicana-cubana TRUE)
	=>
	(assert (comida comida-mexicana-cubana))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-mexicana-cubana-no
	(respuesta-comida-mexicana-cubana FALSE)
	=>
	(assert (plato-fuerte-4))
)

(defrule comida-carne-roja
	(plato-fuerte-4)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-carne-roja (preguntar-si-o-no "### ¿El plato principal consiste en alguna variedad de carne roja -ternera, cerdo, cordero- [si/no] ? ")))
)

(defrule comida-carne-roja-si
	(respuesta-carne-roja TRUE)
	=>
	(assert (plato-fuerte-5))
)

(defrule comida-carne-roja-no
	(respuesta-carne-roja FALSE)
	=>
	(assert (plato-fuerte-10))
)

(defrule comida-asado
	(plato-fuerte-5)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-asado (preguntar-si-o-no "### ¿El plato principal consiste en asado o carne de res [si/no] ? ")))
)

(defrule comida-asado-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-asado TRUE)
	=>
	(assert (comida asado))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-asado-no
	(respuesta-asado FALSE)
	=>
	(assert (plato-fuerte-6))
)

(defrule comida-cordero
	(plato-fuerte-6)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-cordero (preguntar-si-o-no "### ¿El plato principal consiste en cordero [si/no] ? ")))
)

(defrule comida-cordero-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-cordero TRUE)
	=>
	(assert (comida cordero))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-cordero-no
	(respuesta-cordero FALSE)
	=>
	(assert (plato-fuerte-7))
)

(defrule comida-ternera
	(plato-fuerte-7)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-ternera (preguntar-si-o-no "### ¿El plato principal consiste en ternera [si/no] ? ")))
)

(defrule comida-ternera-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-ternera TRUE)
	=>
	(assert (comida ternera))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-ternera-no
	(respuesta-ternera FALSE)
	=>
	(assert (plato-fuerte-8))
)

(defrule comida-cerdo
	(plato-fuerte-8)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-cerdo (preguntar-si-o-no "### ¿El plato principal consiste en cerdo [si/no] ? ")))
)

(defrule comida-cerdo-si
	(respuesta-cerdo TRUE)
	=>
	(assert (plato-fuerte-9))
)

(defrule comida-cerdo-no
	(respuesta-cerdo FALSE)
	=>
	(assert (plato-fuerte-10))
)

(defrule comida-cerdo-arroz
	(plato-fuerte-9)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-cerdo-arroz (preguntar-si-o-no "### ¿El plato principal consiste en cerdo acompañado de arroz [si/no] ? ")))
)

(defrule comida-cerdo-arroz-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-cerdo-arroz TRUE)
	=>
	(assert (comida cerdo-arroz))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-cerdo-arroz-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-cerdo-arroz FALSE)
	=>
	(assert (comida cerdo))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pollo
	(plato-fuerte-10)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pollo (preguntar-si-o-no "### ¿El plato principal consiste en pollo [si/no] ? ")))
)

(defrule comida-pollo-si
	(respuesta-pollo TRUE)
	=>
	(assert (plato-fuerte-11))
)

(defrule comida-pollo-no
	(respuesta-pollo FALSE)
	=>
	(assert (plato-fuerte-12))
)

(defrule comida-pollo-arroz
	(plato-fuerte-11)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pollo-arroz (preguntar-si-o-no "### ¿El plato principal consiste en pollo acompañado de arroz [si/no] ? ")))
)

(defrule comida-pollo-arroz-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-pollo-arroz TRUE)
	=>
	(assert (comida pollo-arroz))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pollo-arroz-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-pollo-arroz FALSE)
	=>
	(assert (comida pollo))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pescados-o-frutos-mar
	(plato-fuerte-12)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pescado-o-fruto-mar (preguntar-si-o-no "### ¿El plato principal consiste en pescados o frutos de mar [si/no] ? ")))
)

(defrule comida-pescado-o-fruto-mar-si
	(respuesta-pescado-o-fruto-mar TRUE)
	=>
	(assert (plato-fuerte-13))
)

(defrule comida-pescado-o-fruto-mar-no
	(respuesta-pescado-o-fruto-mar FALSE)
	=>
	(assert (plato-fuerte-17))
)

(defrule comida-pescado-blanco
	(plato-fuerte-13)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pescado-blanco (preguntar-si-o-no "### ¿El plato principal consiste en pescado blanco [si/no] ? ")))
)

(defrule comida-pescado-blanco-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-pescado-blanco TRUE)
	=>
	(assert (comida pescado-blanco))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pescado-blanco-no
	(respuesta-pescado-blanco FALSE)
	=>
	(assert (plato-fuerte-14))
)

(defrule comida-salmon-atun
	(plato-fuerte-14)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-salmon-atun (preguntar-si-o-no "### ¿El plato principal consiste en salmon o atun [si/no] ? ")))
)

(defrule comida-salmon-atun-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-salmon-atun TRUE)
	=>
	(assert (comida salmon-atun))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-salmon-atun-no
	(respuesta-salmon-atun FALSE)
	=>
	(assert (plato-fuerte-15))
)

(defrule comida-mariscos
	(plato-fuerte-15)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-mariscos (preguntar-si-o-no "### ¿El plato principal consiste en mariscos [si/no] ? ")))
)

(defrule comida-mariscos-si
	(respuesta-mariscos TRUE)
	=>
	(assert (plato-fuerte-16))
)

(defrule comida-mariscos-no
	(respuesta-mariscos FALSE)
	=>
	(assert (plato-fuerte-17))
)

(defrule comida-mariscos-arroz
	(plato-fuerte-16)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-mariscos-arroz (preguntar-si-o-no "### ¿El plato principal consiste en mariscos con arroz -paella- [si/no] ? ")))
)

(defrule comida-mariscos-arroz-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-mariscos-arroz TRUE)
	=>
	(assert (comida mariscos-arroz))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-mariscos-arroz-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-mariscos-arroz FALSE)
	=>
	(assert (comida mariscos))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pastas
	(plato-fuerte-17)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pasta (preguntar-si-o-no "### ¿El plato principal consiste en algun tipo de pastas [si/no] ? ")))
)

(defrule comida-pastas-si
	(respuesta-pasta TRUE)
	=>
	(assert (plato-fuerte-18))
)

(defrule comida-pastas-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-pasta FALSE)
	=>
	(assert (no-plato-fuerte))
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
)

(defrule comida-pastas-salsa-liviana
	(plato-fuerte-18)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pasta-salsa-liviana (preguntar-si-o-no "### ¿El plato principal consiste en pastas con salsa liviana [si/no] ? ")))
)

(defrule comida-pastas-salsa-liviana-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-pasta-salsa-liviana TRUE)
	=>
	(assert (comida pastas-salsa-liviana))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)
	
(defrule comida-pastas-salsa-liviana-no
	(respuesta-pasta-salsa-liviana FALSE)
	=>
	(assert (plato-fuerte-19))
)

(defrule comida-pastas-salsa-fuerte
	(plato-fuerte-19)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-pasta-salsa-fuerte (preguntar-si-o-no "### ¿El plato principal consiste en pastas con salsa fuerte [si/no] ? ")))
)

(defrule comida-pastas-salsa-fuerte-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-pasta-salsa-fuerte TRUE)
	=>
	(assert (comida pastas-salsa-fuerte))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule comida-pastas-salsa-fuerte-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-pasta-salsa-fuerte FALSE)
	=>
	(assert (no-plato-fuerte))
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
)

;;POSTRE
;;=======

(defrule postre-helado
	(postre-1)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-helado (preguntar-si-o-no "### ¿El postre consiste en helado [si/no] ? ")))
)

(defrule postre-helado-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-helado TRUE)
	=>
	(assert (comida helado))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-helado-no
	(respuesta-helado FALSE)
	=>
	(assert (postre-2))
)

(defrule postre-chocolate
	(postre-2)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-chocolate (preguntar-si-o-no "### ¿El postre contiene chocolate [si/no] ? ")))
)

(defrule postre-chocolate-si
	(respuesta-chocolate TRUE)
	=>
	(assert (postre-3))
)

(defrule postre-chocolate-no
	(respuesta-chocolate FALSE)
	=>
	(assert (postre-5))
)

(defrule postre-exclusivo-chocolate
	(postre-3)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-exclusivo-chocolate (preguntar-si-o-no "### ¿El postre esta hecho exclusivamente de chocolate [si/no] ? ")))
)

(defrule postre-exclusivo-chocolate-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-exclusivo-chocolate TRUE)
	=>
	(assert (comida todo-chocolate))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-exclusivo-chocolate-no
	(respuesta-exclusivo-chocolate FALSE)
	=>
	(assert (postre-4))
)

(defrule postre-contiene-chocolate
	(postre-4)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-contiene-chocolate (preguntar-si-o-no "### ¿El postre contiene en parte chocolate [si/no] ? ")))
)

(defrule postre-contiene-chocolate-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-contiene-chocolate TRUE)
	=>
	(assert (comida poco-chocolate))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-contiene-chocolate-no
	(respuesta-contiene-chocolate FALSE)
	=>
	(assert (postre-5))
)

(defrule postre-sorbetto
	(postre-5)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-sorbetto (preguntar-si-o-no "### ¿El postre es alguna clase de sorbetto [si/no] ? ")))
)

(defrule postre-sorbetto-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-sorbetto TRUE)
	=>
	(assert (comida sorbetto))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-sorbetto-no
	(respuesta-sorbetto FALSE)
	=>
	(assert (postre-6))
)

(defrule postre-frutas
	(postre-6)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-frutas (preguntar-si-o-no "### ¿El postre contiene frutas [si/no] ? ")))
)

(defrule postre-frutas-si
	(respuesta-frutas TRUE)
	=>
	(assert (postre-7))
)

(defrule postre-frutas-no
	(respuesta-frutas FALSE)
	=>
	(assert (postre-9))
)

(defrule postre-tarta-frutas
	(postre-7)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-tarta-frutas (preguntar-si-o-no "### ¿El postre es algun tipo de tarta de fruta [si/no] ? ")))
)

(defrule postre-tarta-frutas-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-tarta-frutas TRUE)
	=>
	(assert (comida tarta-fruta))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-tarta-frutas-no
	(respuesta-tarta-frutas FALSE)
	=>
	(assert (postre-8))
)

(defrule postre-fruta-tropical
	(postre-8)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-fruta-tropical (preguntar-si-o-no "### ¿El postre contiene frutas tropicales [si/no] ? ")))
)

(defrule postre-fruta-tropical-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-fruta-tropical TRUE)
	=>
	(assert (comida frutas-tropicales))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-fruta-tropical-no
	(respuesta-fruta-tropical FALSE)
	=>
	(assert (postre-9))
)

(defrule postre-flan
	(postre-9)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-flan (preguntar-si-o-no "### ¿El postre consiste en flan [si/no] ? ")))
)

(defrule postre-flan-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-flan TRUE)
	=>
	(assert (comida flan))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-flan-no
	(respuesta-flan FALSE)
	=>
	(assert (postre-10))
)

(defrule postre-de-queso
	(postre-10)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-postre-queso (preguntar-si-o-no "### ¿El postre contiene queso [si/no] ? ")))
)

(defrule postre-de-queso-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-postre-queso TRUE)
	=>
	(assert (comida postre-queso))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-de-queso-no
	(respuesta-postre-queso FALSE)
	=>
	(assert (postre-11))
)

(defrule postre-pasteleria
	(postre-11)
	(accion-actual determinar-comida)
	=>
	(assert (respuesta-postre-pasteleria (preguntar-si-o-no "### ¿El postre es algun tipo de pieza de pasteleria [si/no] ? ")))
)

(defrule postre-pasteleria-si
	?prox <- (accion-actual determinar-comida)
	(respuesta-postre-pasteleria TRUE)
	=>
	(assert (comida postre-pasteleria))
	(retract ?prox)
	(assert (accion-actual determinar-maridaje))
)

(defrule postre-pasteleria-no
	?prox <- (accion-actual determinar-comida)
	(respuesta-postre-pasteleria FALSE)
	=>
	(assert (no-postre))
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
)

;;********************************************
;; REGLAS DE MARIDAJE
;;********************************************

(defrule sugerir-dips
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida entrada)
	(comida dips)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 14 16 19 20 21 22 24 27))
)

(defrule sugerir-queso-suave
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida entrada)
	(comida queso-suave)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 7 8 10 14 16 17 18 20 21 22 24 27))
)

(defrule sugerir-queso-fuerte
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida entrada)
	(comida queso-fuerte)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 2 3 4 5 6 7 9 10 12 13 17 28))
)

(defrule sugerir-tabla-fiambre
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida entrada)
	(comida tabla-fiambre)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 4 10 14 16 19 20 21 22 24 27))
)

(defrule sugerir-sandwich
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida entrada)
	(comida sandwich)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 8 10 12 13 17 18 19 21 22 23 24))
)

;;PLATO FUERTE
;;============

(defrule sugerir-comida-asiatica
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida comida-asiatica)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 11 13 14 18 19 20 21 22 23 24))
)

(defrule sugerir-comida-mexicana-cubana
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida comida-mexicana-cubana)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 7 12 13 15 17 19 20 21 22 23 24 27))
)

(defrule sugerir-asado
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida asado)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 3 4 5 6 7 8 10 11 12 13))
)

(defrule sugerir-cordero
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida cordero)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 2 3 4 5 6 7 8 13 21 24))
)

(defrule sugerir-ternera
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida ternera)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 3 4 5 6 7 8 10 11 12 13 16 17 18 19 20 21 22 24))
)

(defrule sugerir-cerdo
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida cerdo)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 3 4 5 6 7 8 10 11 12 13 16 17 18 19 20 21 22 24))
)

(defrule sugerir-cerdo-arroz
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida cerdo-arroz)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 4 5 6 7 8 10 11 12 13 15 17 19 21 24 27))
)

(defrule sugerir-pollo
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida pollo)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 3 4 5 6 7 8 10 12 13 16 17 18 19 20 21))
)

(defrule sugerir-pollo-arroz
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida pollo-arroz)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 2 4 5 6 7 8 10 11 12 13 15 17 19 21 24 27))
)

(defrule sugerir-pescado-blanco
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida pescado-blanco)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 8 10 14 16 17 18 19 20 22))
)

(defrule sugerir-salmon-atun
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida salmon-atun)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 8 10 13 16 17 18 19 20 21))
)

(defrule sugerir-mariscos
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida mariscos)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 14 16 17 18 19 20 22))
)

(defrule sugerir-mariscos-arroz
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida mariscos-arroz)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 8 12 14 15 17 18 19 21 22 24 27))
)

(defrule sugerir-pastas-salsa-liviana
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida pastas-salsa-liviana)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 1 10 14 16 17 18 19 20 22))
)

(defrule sugerir-pastas-salsa-fuerte
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida plato-fuerte)
	(comida pastas-salsa-fuerte)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 2 3 4 5 6 7 8 10 12 13))
)

;;POSTRE
;;=======

(defrule sugerir-postre-helado
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida helado)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 23))
)

(defrule sugerir-todo-chocolate
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida todo-chocolate)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 4 6))
)

(defrule sugerir-poco-chocolate
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida poco-chocolate)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 27 28))
)

(defrule sugerir-sorbetto
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida sorbetto)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 19 28 22))
)

(defrule sugerir-tarta-frutas
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida tarta-fruta)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 22 23 26))
)

(defrule sugerir-fruta-tropical
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida frutas-tropicales)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 17))
)

(defrule sugerir-flan
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida flan)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 19 23))
)

(defrule sugerir-postre-queso
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida postre-queso)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 27 28))
)

(defrule sugerir-pasteleria
	?prox <- (accion-actual determinar-maridaje)
	(usuario-tipo-comida postre)
	(comida postre-pasteleria)
	=>
	(retract ?prox)
	(assert (accion-actual mostrar-sugerencias))
	(assert (id-vinos-sugeridos 26 27 28))
)

;;********************************************
;; REGLAS DE SUGERENCIA
;;********************************************
(defrule mostrar-datos-sugerencia
	(declare (salience 9))
	(accion-actual mostrar-sugerencias)
	(comida ?c)
	(usuario-tipo-comida ?user-tipo-comida)
	(usuario-total-dinero ?user-total-dinero)
	(usuario-tipo-vino ?user-tipo-vino)
	=>
	(printout t crlf crlf "SUGERENCIAS:" crlf)
	(printout t "--------------------" crlf crlf)
	(printout t "#######################################################################################" crlf)
	(printout t "TIPO DE COMIDA: " ?user-tipo-comida "  COMIDA: " ?c "   TIPO DE VINO: " ?user-tipo-vino "  DINERO: $ " ?user-total-dinero crlf)
	(printout t "#######################################################################################" crlf)
)

(defrule determinar-indeferencia-vino
	(declare (salience 9))
	(usuario-tipo-vino indiferente)
	=>
	(assert (cualquier-vino))
)


(defrule mostrar-vino-sugerencia
	(declare (salience 8))
	(accion-actual mostrar-sugerencias)
	(id-vinos-sugeridos $? ?idvino $?)
	(comida ?c)
	(usuario-tipo-vino ?user-tipo-vino)
	(vino (id ?idvino) (nombre ?vino) (precio ?p) (tipo ?user-tipo-vino))
	(usuario-total-dinero ?user-total-dinero)
	=>
	(assert (se-sugirio-vino))
	(printout t "### VINO SUGERIDO: " ?vino crlf)
	(if (< ?user-total-dinero ?p) then
		(printout t "### PRECIO: $ " ?p " -> DINERO INSUFICIENTE" crlf crlf)
	else
		(printout t "### PRECIO: $ " ?p crlf crlf)
	)
)

(defrule mostrar-vino-sugerencia-indiferente
	(declare (salience 8))
	(accion-actual mostrar-sugerencias)
	(cualquier-vino)
	(id-vinos-sugeridos $? ?idvino $?)
	(comida ?c)
	(vino (id ?idvino) (nombre ?vino) (precio ?p) (tipo ?tipo))
	(usuario-total-dinero ?user-total-dinero)
	=>
	(assert (se-sugirio-vino))
	(printout t "### VINO SUGERIDO: " ?vino crlf)
	(printout t "### TIPO DE VINO : " ?tipo crlf)
	(if (< ?user-total-dinero ?p) then
		(printout t "### PRECIO: $ " ?p " -> DINERO INSUFICIENTE" crlf crlf)
	else
		(printout t "### PRECIO: $ " ?p crlf crlf)
	)
)

(defrule post-sugerir-vino-correr-nuevamente
	(declare (salience 7))
	(se-sugirio-vino)
	=>
	(assert (accion-actual correr-nuevamente))
)

(defrule mostrar-no-hubo-vino-sugerido
	?prox <- (accion-actual mostrar-sugerencias)
	(not (se-sugirio-vino))
	(usuario-tipo-vino ?user-tipo-vino)
	(comida ?c)
	=>
	(printout t crlf "### No hubo ningun vino " ?user-tipo-vino " que tenga maridaje con " ?c ". Intenta elegir tipo de vino 'indiferente'." crlf crlf)
	(retract ?prox)
	(assert (accion-actual correr-nuevamente))
)

(defrule mostrar-no-selecciono-comida
	?prox <- (accion-actual mostrar-sugerencias)
	(or (no-entrada) (no-plato-fuerte) (no-postre))
	=>
	(retract ?prox)
	(assert (accion-actual correr-nuevamente))
	(printout t crlf "### No seleccionaste ninguna comida del menu." crlf crlf)
)
